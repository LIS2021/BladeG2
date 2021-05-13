module IntMap = Map.Make(Int)

open Expr;;
open Commands;;
open Option;;
open Format;;
open Utils;;

let mem_op_cost = (20 : int)

let can_fetch (cs : cmd list) : bool = cs != [];;

let can_retire (is : instruction list) : bool =
  match is with
    | Nop :: _
    | Fail(_) :: _
    | Fence :: _
    | Assign(_, Cst(_)) :: _
    | Store(Cst(CstI(_)), _, Cst(CstI(_))) :: _ -> true
    | _ -> false;;

let can_exec (n : int) (is : instruction list) (rho : environment) : bool =
  match split is n with
    | None -> false
    | Some (is1, is, _) ->
      (* The LFENCE instruction does not execute until all prior instructions have completed locally, and no later instruction begins execution until LFENCE completes.
      https://software.intel.com/content/www/us/en/develop/articles/using-intel-compilers-to-mitigate-speculative-execution-side-channel-issues.html?wapkw=lfence%20x86 *)
      if List.exists (fun i -> i = Fence) is1
        then false
      else try (let rho' = Eval.phi is1 rho in
        match is with
          | Assign(_, Cst(_)) -> false
          | Assign(_, e) -> ignore (Eval.eval e rho'); true
          | Guard(e, _, _, _, _) ->
              ignore (Eval.eval e rho'); true
          | Load(_, _, e) ->
              if List.exists (fun i -> match i with | Store(_,_,_) -> true | _ -> false) is1 then false else (ignore (Eval.eval e rho'); true)
              (* Type errors are handled by the vm *)
          | Store(e1, _, e2) ->
              ignore (Eval.eval e1 rho'); ignore (Eval.eval e2 rho'); true
          | IProtect(_, Cst(_)) ->
              not (List.exists (fun i -> match i with | Guard(_,_,_,_, _) -> true | _ -> false) is1)
          | IProtect(_, expr) -> ignore (Eval.eval expr rho'); true
          | Nop
          | Fail(_)
          | Fence -> false)
        with | Not_found
             | Invalid_argument(_) -> false;;

type step_t =
    | SFetch of cmd
    | SPFetch of prediction * cmd
    | SExec of int * instruction
    | SRetire of instruction
    | SLoadEnd of identifier * int
    | SRollback of guard_id;;
    (* | SCost of int;; *)

let string_of_step (s : step_t) : string =
  match s with | SFetch(_) -> "Fetch"
               | SPFetch(_, _) -> "PFetch"
               | SExec(n, istr) -> "Exec " ^ string_of_int n ^ ": " ^ string_of_instruction istr
               | SRetire(istr) -> "Retire: " ^ string_of_instruction istr
               | SLoadEnd(ide, v) -> "Load ended: " ^ ide ^ " (left " ^ string_of_int v ^ ")"
               | SRollback(id) -> "Rollback (" ^ string_of_int id ^ ")";;
               (* | SCost(n) -> "Cost " ^ string_of_int n;; *)

Random.self_init ();;

class speculator =
  object(self)
    (* We don't rollback statistics because we assume it's too much overhead for the processor to do so *)
    val mutable statistics = IntMap.empty
    val mutable while_flag = false

    method get_prediction (id : if_id) : bool =
      (*
       * while_flag means that we compiled a while, so this if is the if of the unrolling of a while and hence we speculate it true
       * id = 0 is the bound check of an array access, so we speculate it true because otherwise the program crashes (so in this case speculation is useless)
       *)
      if while_flag || (id = 0) then (
        while_flag <- false;
        true)
      else (
        match IntMap.find_opt id statistics with | None -> statistics <- IntMap.add id (0, 0) statistics; true
                                                 | Some (ntrue, nfalse) -> if ntrue < nfalse then false else true)

    method update_stats (id : if_id) (outcome : bool) : unit =
      let updater old =
        match old with | None -> None
                       | Some(ntrue, nfalse) -> if outcome then some (ntrue + 1, nfalse) else some (ntrue, nfalse + 1)
      in
      if id != 0 then
        statistics <- IntMap.update id updater statistics
      else
        ()
    
    method fetch_while : unit =
      while_flag <- true
  end;;

type cacheline =
  | CLArray of identifier
  | CLMem of int;;

class cache =
  object(self)
    val cache_size = 5
    val mutable lines = ([] : cacheline list)

    method is_hit (l : cacheline) : bool =
      List.mem l lines

    method refresh (l : cacheline) : unit =
      let lines' = delete l lines in
      lines <- take cache_size (l :: lines')
  end;;

module Processor = struct
  class type processor_t =
    object
      method get_next_directive : Commands.configuration -> Commands.directive
      method print_state : bool -> unit
    end

  class simple_processor : processor_t = 
    object (self)
      val mem_op_cost = mem_op_cost
      val mutable tot_cost = (0 : int)
      val mutable exec0 = (0 : int)
      val mutable loadc = (0 : int)

      method get_next_directive (conf : configuration) : directive =
        match conf.is with
            | [] -> (match conf.cs with | If(_, _, _, _) :: _ -> tot_cost <- tot_cost + 1; PFetch true
                                        | _ :: _ -> tot_cost <- tot_cost + 1; Fetch
                                        | [] -> tot_cost <- tot_cost + 1; exec0 <- exec0 + 1; Exec 0 (* The JIT should never ask a directive when both is and cs are empty *)
                    )
            (* Fa Retire se può altrimenti Exec 0 *)
            | Nop :: _
            | Assign(_, Cst(_)) :: _
            | Fail(_) :: _
            | Fence :: _ -> tot_cost <- tot_cost + 1; Retire
            | Store(Cst(CstI(_)), _, Cst(CstI(_))) :: _ -> tot_cost <- tot_cost + mem_op_cost; Retire
            | Load(_, _, _) :: _ -> tot_cost <- tot_cost + 1 + mem_op_cost; exec0 <- exec0 + 1; loadc <- loadc + mem_op_cost; Exec 0
            | _ -> tot_cost <- tot_cost + 1; exec0 <- exec0 + 1; Exec 0

      method print_state (_ : bool) =
        printf "--------- Simple processor output ---------\n";
        printf "Total cost: %d\n" tot_cost;
        printf "Number of Exec: %d\n" exec0;
        printf "Number of loads: %d\n" (loadc / mem_op_cost);
        printf "Total time spent waiting loads: %d\n" loadc;
    end;;

  class cycle_processor : processor_t = 
    object (self)
        val mutable state = (0 : int)

      method private get_fetch (c : cmd) : directive =
        match c with | If(_, _, _, _) -> PFetch true
                     | _ -> Fetch

        method get_next_directive (conf : configuration) : directive =
          match state with | 0 -> state <- 1; if can_fetch conf.cs then self#get_fetch (List.hd conf.cs) else self#get_next_directive conf
                           | 1 -> state <- 2; if can_exec 0 conf.is conf.rho then Exec 0 else self#get_next_directive conf
                           | 2 -> state <- 0; if can_retire conf.is then Retire else self#get_next_directive conf
                           | _ -> failwith "unexpected processor internal state"
          
        method print_state (_ : bool) = ()
      end;;

  (* Doesn't work *)
  class random_processor : processor_t =
    object(self)
      val mutable old_conf = ({is = []; cs = []; mu = [||]; rho = Expr.StringMap.empty} : configuration)
      val mutable possibilities = ([] : directive list)
      val mutable can_retire = true

      method get_next_directive (conf : configuration) : directive =
        if conf != old_conf then (
          possibilities <- List.mapi (fun i  _ -> Exec i) (take 20 conf.is);
          can_retire <- true;
          old_conf <- conf)
        else ();
        let can_fetch = (conf.cs != []) in
        let r = (
          if can_fetch then
            if can_retire then
              Random.int 3
            else
              Random.int 2
          else
            if can_retire then
              1 + Random.int 2
            else
              1
          ) in
        match r with
          | 0 -> (match conf.cs with | If(_, _, _, _) :: _ -> PFetch true
                                     | [] -> Retire
                                     | _ -> Fetch)
          | 1 -> if possibilities = [] then Retire else (let i = Random.int (List.length possibilities) in
                    match split possibilities i with
                      | Some(p1, d, p2) -> possibilities <- p1 @ p2; d
                      | None -> failwith "List index out of bounds")
          | 2 -> can_retire <- false; Retire
          | _ -> failwith "Unexpected random number"
      
      method print_state (_ : bool) = ()
    end;;

  class complex_processor (static_len : bool) : processor_t =
    object(self)
      val onthefly_buffer_target_len = (20 : int)
      val length_load_cost = mem_op_cost
      val mem_op_cost = mem_op_cost
      (* val static_len = true *)

      (* For each load we keep: identifier = variable assigned, int = time left, guard_id list = list of guard ids of
         guards that, when rollbacked, removes this load, int = timestamp of the load, cacheline = mem address della load *)
      (* TODO: forse il guard_id list non serve con i timestamp, ma vediamo dopo *)
      val mutable pending_loads_buffer = ([] : (identifier * (int * guard_id list * int * cacheline)) list)
      val mutable exec_idx = 0

      val mutable exec_trace = ([] : step_t list)

      val mutable tot_cost = (0 : int)
      
      val mutable retnum = (0 : int) (* Retnum is used to compute ordering of instructions *)

      val mutable exec0 = (0 : int)
      val mutable execOOO = (0 : int)
      val mutable loadc = (0 : int)
      val mutable loadn = (0 : int)

      val specul = new speculator
      val proc_cache = new cache

      method private can_retire (is : instruction list) : bool =
        match is with
          | Store(Cst(CstI(_)), _, Cst(CstI(_))) :: _ -> pending_loads_buffer = []
          (* Checking that this Assign isn't the result of a load that hasn't completed yet *)
          | Assign(ide, Cst(_)) :: _ -> not (List.exists (fun (idel, (_, _, ts, _)) -> ide = idel && ts = retnum) pending_loads_buffer)
          | _ -> can_retire is

      (* Pay t time and possibly reduce cost of pending loads *)
      method private pay_time (t : int) : unit =
        let rec helper (n : int) : unit =
          match pending_loads_buffer with | [] -> ()
                                          | (ide, (v, l, ts, addr)) :: ls when v > n -> pending_loads_buffer <- (ide, (v - n, l, ts, addr)) :: ls
                                          | (ide, (v, _, _, addr)) :: ls -> pending_loads_buffer <- ls;
                                                                            exec_trace <- SLoadEnd(ide, 0) :: exec_trace;
                                                                            exec_idx <- 0;
                                                                            if static_len then proc_cache#refresh addr;
                                                                            helper (n - v)
        in
        helper t;
        (* exec_trace <- SCost(t) :: exec_trace; *)
        tot_cost <- tot_cost + t

      method private add_fetch_cost : unit =
        self#pay_time 1 (* TODO *)

      method private add_retire_cost (istr : instruction) : unit =
        match istr with | Store(_, _, _) -> self#pay_time mem_op_cost
                        | _ -> self#pay_time 1

      method private add_exec_cost (istr : instruction) : unit =
        self#pay_time 1 (* All instructions cost 1 at Exec time. Memory ops cost more, but we pay this either with the pending loads buffer (Loads) or at Retire time (Stores) *)

      method private can_fetch (conf : configuration) : bool =
        let can_fetch_specul = not (List.exists (fun i -> match i with | Guard(_, _, _, _, _) -> true | _ -> false) conf.is && (pending_loads_buffer = [])) in
        List.length conf.is < onthefly_buffer_target_len && conf.cs != [] && can_fetch_specul

      method private get_fetch (c : cmd) : directive =
        self#add_fetch_cost;
        match c with | If(_, _, _, id) -> let pred = specul#get_prediction id in
                                          exec_trace <- SPFetch(pred, c) :: exec_trace;
                                          PFetch pred
                     | While(_, _, _) -> specul#fetch_while;
                                         exec_trace <- SFetch(c) :: exec_trace;
                                         Fetch
                     | _ -> exec_trace <- SFetch(c) :: exec_trace;
                            Fetch

      (* Checks whether expr e depends on a pending load. Since our Exec
         executes whole expressions, we have to stop the entire execution *)
      method private depends_pending_load (e : expr) (ts : int) : bool =
        let rec helper (e : expr) : bool =
          match e with | Cst(_) -> false
                       | Var(ide) -> List.mem_assoc ide (List.filter (fun (_, (_, _, tsl, _)) -> tsl <= ts) pending_loads_buffer)
                       | BinOp(e1, e2, _) -> helper e1 || helper e2
                       | InlineIf(e, e1, e2) -> helper e || helper e1 || helper e2
                       | Base(e) -> helper e
                       | Length(ide) -> static_len &&
                            (* this cases implicitly handles loads of length(a): it checks whether length(a) is already in cache, and in this case
                               it returns false because we can exec an expression that depends on length(a). Otherwise it requires the load of length(a)
                               (only if it isn't already in the pending loads buffer) and returns true because we can't exec the expression. *)
                          if proc_cache#is_hit (CLArray ide) then (
                            proc_cache#refresh (CLArray ide);
                            false
                          )
                          else (
                            if not (List.exists (fun pl -> match pl with | ("", (_, _, _, CLArray ide)) -> true | _ -> false) pending_loads_buffer) then
                              pending_loads_buffer <- pending_loads_buffer @ [("", (length_load_cost, [], ts, CLArray ide))];
                            true
                          )
          in helper e

      method private i_depends_pending_load (i : instruction) (ts : int) : bool =
        match i with | IProtect(_, e)
                     | Guard(e, _, _, _, _)
                     | Assign(_, e)
                     | Load(_, _, e) -> self#depends_pending_load e ts
                     | Store(e1, _, e2) -> self#depends_pending_load e1 ts || self#depends_pending_load e2 ts
                     | Nop
                     | Fence
                     | Fail(_) -> false

      method private cant_exec (n : int) (istr : instruction) (is : instruction list) (rho : environment) : bool =
        (* let is_double_load = static_len && match istr with
            | Load(_, _, e) -> (match Eval.eval e (Eval.phi (take n is) rho) with
                                    | CstI(pos) -> List.exists (fun (_, (_, _, _, addr)) -> addr = CLMem pos) pending_loads_buffer
                                    | _ -> failwith "Shouldn't reach this program point (see processor#cant_exec)!" )
            | _ -> false
        in *)
        (not (can_exec n is rho)) || self#i_depends_pending_load istr (retnum + n)
        || (static_len && match istr with
                | Load(_, _, e) -> (match Eval.eval e (Eval.phi (take n is) rho) with
                                        | CstI(pos) -> List.exists (fun (_, (_, _, _, addr)) -> addr = CLMem pos) pending_loads_buffer
                                        | _ -> failwith "Shouldn't reach this program point (see processor#cant_exec)!" )
                | _ -> false)

      method private get_exec (conf : configuration) : directive =
        let istr = List.nth_opt conf.is exec_idx in
        if istr = None then (
          (* Se nessuna delle n istruzioni successive è eseguibile si attende la terminazione della prima load pendente,
             poi si riparte perché questa load potrebbe aver sbloccato alcune istruzioni *)
          (match pending_loads_buffer with | [] -> failwith "Exec out of instruction list with empty pending loads buffer"
                                           | (ide, (v, _, _, addr)) :: ls -> (* exec_trace <- SCost(v) :: exec_trace; *)
                                                                             tot_cost <- tot_cost + v;
                                                                             loadc <- loadc + v;
                                                                             exec_trace <- SLoadEnd(ide, v) :: exec_trace;
                                                                             if static_len then proc_cache#refresh addr;
                                                                             pending_loads_buffer <- ls);
          exec_idx <- 0;
          self#get_next_directive conf)
        else (
          let current_exec_idx = exec_idx in
          if self#cant_exec current_exec_idx (Option.get istr) conf.is conf.rho then (
            (* Can't exec the instruction due to semantics or to pending loads *)
            exec_idx <- exec_idx + 1;
            self#get_exec conf)
          else (
            (* Can exec the instruction *)
            if current_exec_idx = 0 then exec0 <- exec0 + 1 else execOOO <- execOOO + 1;
            if current_exec_idx > 0 && pending_loads_buffer = [] then failwith "Can't Exec out of order without pending loads";
            (* match to actually execute instructions *)
            match Option.get istr with | IProtect(_, _)
                                       | Assign(_, _)
                                       | Store(_, _, _) -> (
                                          exec_idx <- exec_idx + 1;
                                          self#add_exec_cost (Option.get istr); (* OCCHIO va dopo exec_idx++ perché potrebbe modificarlo anche la pay_time *)
                                          exec_trace <- SExec (current_exec_idx, Option.get istr) :: exec_trace;
                                          Exec current_exec_idx)
                                       | Guard(e, p, _, idg, idi) -> (
                                          exec_trace <- SExec (current_exec_idx, Option.get istr) :: exec_trace;
                                          if not (Eval.eval e (Eval.phi (take current_exec_idx conf.is) conf.rho) = CstB(p)) then (
                                            (* Rollback, we drop pending loads that were rollbacked *)
                                            specul#update_stats idi (not p);
                                            exec_trace <- SRollback(idg) :: exec_trace;
                                            (* pending_loads_buffer <- List.filter (fun (_, (_, ids, _, _)) -> not (List.mem idg ids)) pending_loads_buffer; *)
                                            pending_loads_buffer <- List.filter (fun (_, (_, _, tsl, _)) -> tsl <= retnum + current_exec_idx) pending_loads_buffer;
                                            exec_idx <- 0)
                                          else (
                                            specul#update_stats idi p);
                                          exec_idx <- exec_idx + 1;
                                          self#add_exec_cost (Option.get istr);
                                          Exec current_exec_idx)
                                       | Load(ide, _, e) -> (
                                          exec_idx <- exec_idx + 1;
                                          loadn <- loadn + 1;
                                          self#add_exec_cost (Option.get istr);
                                          exec_trace <- SExec (current_exec_idx, Option.get istr) :: exec_trace;
                                          (match Eval.eval e (Eval.phi (take current_exec_idx conf.is) conf.rho) with
                                            | CstI(addr) -> if static_len && proc_cache#is_hit (CLMem addr) then
                                                proc_cache#refresh (CLMem addr)
                                              else (
                                                let guard_ids = List.filter_map (fun i -> match i with | Guard(_, _, _, id, _) -> some id | _ -> none) (take current_exec_idx conf.is) in
                                                pending_loads_buffer <- pending_loads_buffer @ [(ide, (mem_op_cost, guard_ids, retnum + current_exec_idx, CLMem addr))];
                                              );
                                            | _ -> failwith "Shouldn't reach this program point (see processor#get_exec)!");
                                          Exec current_exec_idx)
                                       | _ -> failwith "Shouldn't reach this program point (see processor#get_exec)!"
          )
        )

      method get_next_directive (conf : configuration) : directive =
        if self#can_fetch conf then
          self#get_fetch (List.hd conf.cs)
        else if self#can_retire conf.is then (
          exec_idx <- 0;
          self#add_retire_cost (List.hd conf.is);
          exec_trace <- SRetire (List.hd conf.is) :: exec_trace;
          retnum <- retnum + 1;
          if static_len then (
            match List.hd conf.is with | Store(Cst(CstI(addr)), _, _) -> proc_cache#refresh (CLMem addr)
                                       | _ -> ());
          Retire
        )
        else
          self#get_exec conf

      (* OUTPUT FUNCTIONS *)
      method private print_exec_trace () : unit =
        printf "\n------- Exec trace\n";
        printf "%s" (String.concat "\n" (List.rev_map string_of_step exec_trace));
        printf "\n"
      
      method private print_plb () : unit =
        printf "\nPending loads buffer: ";
        if pending_loads_buffer = [] then
          printf "empty"
        else
          List.iter (fun (ide, (v, _, ts, _)) -> printf "(%s - v: %d, ts: %d), " ide v ts) pending_loads_buffer;
        printf "\n"

      method print_state (verbose : bool) =
        printf "--------- Complex processor output ---------\n";
        printf "Total cost: %d\n" tot_cost;
        printf "Number of in order Exec: %d\n" exec0;
        printf "Number of out-of-order Exec: %d\n" execOOO;
        printf "Number of loads: %d\n" loadn;
        printf "Total time spent waiting loads: %d\n" loadc;
        if verbose then (
          self#print_exec_trace ();
          (* self#print_plb (); *) (* Not printed because at the end is always empty *)
        )
        else
          ()
    end;;
end;;

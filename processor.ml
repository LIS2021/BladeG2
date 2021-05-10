open Expr;;
open Commands;;
open Option;;
open Format;;
open Utils;;

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
          | Guard(e, _, _, _) ->
              ignore (Eval.eval e rho'); true
          | Load(_, _, e) ->
              if List.exists (fun i -> match i with | Store(_,_,_) -> true | _ -> false) is1 then false else (ignore (Eval.eval e rho'); true)
              (* Type errors are handled by the vm *)
          | Store(e1, _, e2) ->
              ignore (Eval.eval e1 rho'); ignore (Eval.eval e2 rho'); true
          | IProtect(_, Cst(_)) ->
              not (List.exists (fun i -> match i with | Guard(_,_,_,_) -> true | _ -> false) is1)
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

let string_of_step (s : step_t) : string =
  match s with | SFetch(_) -> "Fetch"
               | SPFetch(_, _) -> "PFetch"
               | SExec(n, istr) -> "Exec " ^ string_of_int n ^ ": " ^ string_of_instruction istr
               | SRetire(istr) -> "Retire: " ^ string_of_instruction istr
               | SLoadEnd(ide, v) -> "Load ended: " ^ ide ^ " (left " ^ string_of_int v ^ ")"
               | SRollback(id) -> "Rollback (" ^ string_of_int id ^ ")";;

Random.self_init ();;

module Processor = struct
  class type processor_t =
    object
      method get_next_directive : Commands.configuration -> Commands.directive
      method print_state : unit -> unit
    end

  class simple_processor : processor_t = 
    object (self)
      method get_next_directive (conf : configuration) : directive =
        match conf.is with
            | [] -> (match conf.cs with | If(_, _, _) :: _ -> PFetch true
                                        | _ :: _ -> Fetch
                                        | [] -> Exec 0 (* The JIT should never ask a directive when both is and cs are empty *)
                    )
            (* Fa Retire se può altrimenti Exec 0 *)
            | Nop :: _
            | Assign(_, Cst(_)) :: _
            | Store(Cst(CstI(_)), _, Cst(CstI(_))) :: _
            | Store(Cst(_), _, Cst(_)) :: _
            | Fail(_) :: _
            | Fence :: _ -> Retire
            | _ -> Exec 0
      
      method print_state () = ()
    end;;

  class cycle_processor : processor_t = 
    object (self)
        val mutable state = (0 : int)

      method private get_fetch (c : cmd) : directive =
        match c with | If(_, _, _) -> PFetch true
                     | _ -> Fetch

        method get_next_directive (conf : configuration) : directive =
          match state with | 0 -> state <- 1; if can_fetch conf.cs then self#get_fetch (List.hd conf.cs) else self#get_next_directive conf
                           | 1 -> state <- 2; if can_exec 0 conf.is conf.rho then Exec 0 else self#get_next_directive conf
                           | 2 -> state <- 0; if can_retire conf.is then Retire else self#get_next_directive conf
                           | _ -> failwith "unexpected processor internal state"
          
        method print_state () = ()
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
          | 0 -> (match conf.cs with | If(_, _, _) :: _ -> PFetch true
                                     | [] -> Retire
                                     | _ -> Fetch)
          | 1 -> if possibilities = [] then Retire else (let i = Random.int (List.length possibilities) in
                    match split possibilities i with
                      | Some(p1, d, p2) -> possibilities <- p1 @ p2; d
                      | None -> failwith "List index out of bounds")
          | 2 -> can_retire <- false; Retire
          | _ -> failwith "Unexpected random number"
      
      method print_state () = ()
    end;;

  class complex_processor : processor_t =
    object(self)
      val fetched_block_len = (20 : int)
      val mem_op_cost = (20 : int)

      val mutable pending_loads_buffer = ([] : (identifier * (int * guard_id list)) list)
      val mutable exec_idx = 0

      val mutable exec_trace = ([] : step_t list)

      val mutable tot_cost = (0 : int)


      method private can_retire (is : instruction list) : bool =
        match is with
          | Store(Cst(CstI(_)), _, Cst(CstI(_))) :: _ -> pending_loads_buffer = []
          | Assign(ide, Cst(_)) :: _ -> not (List.mem_assoc ide pending_loads_buffer)
          | _ -> can_retire is

      (* Pay t time and possibly reduce cost of pending loads *)
      method private pay_time (t : int) : unit =
        let rec helper (n : int) : unit =
          match pending_loads_buffer with | [] -> ()
                                          | (ide, (v, l)) :: ls when v > n -> pending_loads_buffer <- (ide, (v - n, l)) :: ls
                                          | (ide, (v, l)) :: ls -> pending_loads_buffer <- ls;
                                                                   exec_trace <- SLoadEnd(ide, 0) :: exec_trace;
                                                                   exec_idx <- 0;
                                                                   helper (n - v)
        in
        helper t;
        tot_cost <- tot_cost + t

      method private add_fetch_cost : unit =
        self#pay_time 1

      method private add_retire_cost (istr : instruction) : unit =
        match istr with | Store(_, _, _) -> self#pay_time mem_op_cost
                        | _ -> self#pay_time 1

      method private add_exec_cost (istr : instruction) : unit =
        self#pay_time 1 (* All instructions cost 1 at Exec time. Memory ops cost more, but we pay this either with the pending loads buffer or at Retire time *)

      method private get_fetch (c : cmd) : directive =
        self#add_fetch_cost;
        match c with | If(_, _, _) -> exec_trace <- SPFetch(true, c) :: exec_trace;
                                      PFetch true (* TODO speculatore *)
                     | _ -> exec_trace <- SFetch(c) :: exec_trace;
                            Fetch

      (* Checks whether expr e depends on a pending load. Since our Exec
         executes whole expressions, we have to stop the entire execution *)
      method private depends_pending_load (e : expr) : bool =
        let rec helper (e : expr) : bool =
          match e with | Cst(_) -> false
                       | Var(ide) -> List.mem_assoc ide pending_loads_buffer
                       | BinOp(e1, e2, _) -> helper e1 || helper e2
                       | InlineIf(e, e1, e2) -> helper e || helper e1 || helper e2
                       | Length(e)
                       | Base(e) -> helper e
          in helper e

      method private i_depends_pending_load (i : instruction) : bool =
        match i with | IProtect(_, e)
                     | Guard(e, _, _, _)
                     | Assign(_, e)
                     | Load(_, _, e) -> self#depends_pending_load e
                     | Store(e1, _, e2) -> self#depends_pending_load e1 || self#depends_pending_load e2
                     | Nop
                     | Fence
                     | Fail(_) -> false

      method private get_exec (conf : configuration) : directive =
        let istr = List.nth_opt conf.is exec_idx in
        if istr = None then (
          (* Se nessuna delle n istruzioni successive è eseguibile si attende la terminazione della prima load pendente,
             poi si riparte perché questa load potrebbe aver sbloccato alcune istruzioni *)
          (match pending_loads_buffer with | [] -> ()
                                           | (ide, (v, _)) :: ls -> tot_cost <- tot_cost + v;
                                                                    exec_trace <- SLoadEnd(ide, v) :: exec_trace;
                                                                    pending_loads_buffer <- ls);
          exec_idx <- 0;
          self#get_next_directive conf)
        else (
          let current_exec_idx = exec_idx in
          if (not (can_exec current_exec_idx conf.is conf.rho)) || self#i_depends_pending_load (Option.get istr) then (
            (* Can't exec the instruction due to semantics or to pending loads *)
            exec_idx <- exec_idx + 1;
            self#get_exec conf)
          else
            (* Can exec the instruction *)
            match Option.get istr with | IProtect(_, _)
                                       | Assign(_, _)
                                       | Store(_, _, _) -> (
                                          exec_idx <- exec_idx + 1;
                                          exec_trace <- SExec (current_exec_idx, Option.get istr) :: exec_trace;
                                          self#add_exec_cost (Option.get istr); (* OCCHIO va prima di exec_idx++ perché potrebbe modificarlo anche la pay_time *)
                                          Exec current_exec_idx)
                                       | Guard(e, p, _, id) -> (
                                          exec_trace <- SExec (current_exec_idx, Option.get istr) :: exec_trace;
                                          if not (Eval.eval e (Eval.phi (take current_exec_idx conf.is) conf.rho) = CstB(p)) then (
                                            (* Rollback, we drop pending loads that were rollbacked *)
                                            exec_trace <- SRollback(id) :: exec_trace;
                                            pending_loads_buffer <- List.filter (fun (_, (_, ids)) -> not (List.mem id ids)) pending_loads_buffer)
                                          else
                                            ();
                                          exec_idx <- exec_idx + 1;
                                          self#add_exec_cost (Option.get istr);
                                          Exec current_exec_idx)
                                       | Load(ide, _, _) -> (
                                          exec_idx <- exec_idx + 1;
                                          exec_trace <- SExec (current_exec_idx, Option.get istr) :: exec_trace;
                                          self#add_exec_cost (Option.get istr);
                                          let guard_ids = List.filter_map (fun i -> match i with | Guard(_, _, _, id) -> some id | _ -> none) (take current_exec_idx conf.is) in
                                          pending_loads_buffer <- pending_loads_buffer @ [(ide, (mem_op_cost, guard_ids))];
                                          Exec current_exec_idx)
                                       | _ -> failwith "shouldn't reach this program point (see processor#get_exec)!"
        )

      method get_next_directive (conf : configuration) : directive =
        if List.length conf.is < fetched_block_len && can_fetch conf.cs then
          self#get_fetch (List.hd conf.cs)
        else if self#can_retire conf.is then (
            exec_idx <- 0;
            self#add_retire_cost (List.hd conf.is);
            exec_trace <- SRetire (List.hd conf.is) :: exec_trace;
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
        printf "---------- Pending loads buffer\n";
        if pending_loads_buffer = [] then
          printf "PLB is empty"
        else
          List.iter (fun (ide, (v, _)) -> printf "%s - %d" ide v) pending_loads_buffer;
        printf "\n"

      method print_state () =
        (* printf "\n------- Fetch trace\n";
        printf "%s" (String.concat "\n#####\n" (List.rev_map string_of_cmd fetch_trace)); *)
        (* self#print_exec_trace (); *)
        (* self#print_plb (); *)
        printf "Exec cost: %d\n" tot_cost
    end;;
end;;

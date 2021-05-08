open Expr;;
open Commands;;
open Option;;
open Format;;

let split (l : 'a list) (n : int) =
  if List.length l <= n then none else
    let rec _split (l1, x, l2) (n : int) =
      match n with
      | 0 -> some (l1, x, l2)
      | n -> _split (l1 @ [x], List.hd l2, List.tl l2) (n - 1)
    in _split ([], List.hd l, List.tl l) n;;

let rec firstk k xs = match xs with
  | [] -> []
  | x::xs -> if k=1 then [x] else x::firstk (k-1) xs;;

let taile l = match l with | [] -> []
                         | _ :: t -> t;;

Random.self_init ()

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

        method get_next_directive (conf : configuration) : directive =
          match state with | 0 -> state <- 1; Fetch
                           | 1 -> state <- 2; PFetch true
                           | 2 -> state <- 3; Exec 0
                           | 3 -> state <- 0; Retire
                           | _ -> failwith "unexpected processor internal state"
          
        method print_state () = ()
      end;;

  class random_processor : processor_t =
    object(self)
      val mutable old_conf = ({is = []; cs = []; mu = [||]; rho = Expr.StringMap.empty} : configuration)
      val mutable possibilities = ([] : directive list)
      val mutable can_retire = true

      method get_next_directive (conf : configuration) : directive =
        if conf != old_conf then (
          possibilities <- List.mapi (fun i  _ -> Exec i) (firstk 20 conf.is);
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
      val mem_op_cost = (10 : int)
      val mutable old_conf = ({is = []; cs = []; mu = [||]; rho = Expr.StringMap.empty} : configuration)
      val mutable pending_loads_buffer = ([] : (identifier * int) list)
      val mutable exec_idx = 0
      val mutable sent_exec = false

      val mutable exec_trace = []
      val mutable fetch_trace = []


      method private can_retire (is : instruction list) : bool =
        match is with
          | Nop :: _
          | Fail(_) :: _
          | Fence :: _
          | Assign(_, Cst(_)) :: _ -> true
          | Store(Cst(CstI(_)), _, Cst(CstI(_))) :: _ -> pending_loads_buffer = []
          | _ -> false

      method private check_step (new_conf : configuration) : unit =
        if new_conf != old_conf then
          (old_conf <- new_conf;
          if sent_exec then (
            (* Last Exec was succesfully executed *)
            sent_exec <- false;
            match pending_loads_buffer with | [] -> ()
                                            | (ide, 0) :: ls -> pending_loads_buffer <- ls; exec_idx <- 0
                                            | (ide, v) :: ls -> pending_loads_buffer <- (ide, v - 1) :: ls
          )
          else
            ()
        )
        else
          if sent_exec then (
            sent_exec <- false;
            exec_trace <- List.tl exec_trace
          )
          else
            ()

      method private get_fetch (c : cmd) : directive =
        fetch_trace <- c :: fetch_trace;
        sent_exec <- false;
        match c with | If(_, _, _) -> PFetch true (* TODO speculatore *)
                     | _ -> Fetch

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

      method private get_exec (conf : configuration) : directive =
        let istr = List.nth_opt conf.is exec_idx in
        if istr = None then (
          pending_loads_buffer <- taile pending_loads_buffer; (* Finisco di aspettare la prima load bufferata. Occhio al costo! *)
          exec_idx <- 0;
          self#get_next_directive conf)
        else (
          exec_idx <- exec_idx + 1;
          match Option.get istr with | Nop
                                     | Fence
                                     | Fail(_) -> self#get_exec conf
                                     | IProtect(_, e)
                                     | Guard(e, _, _, _)
                                     | Assign(_, e) -> if self#depends_pending_load e then
                                          self#get_exec conf
                                        else (
                                          sent_exec <- true;
                                          exec_trace <- (exec_idx - 1, Option.get istr) :: exec_trace;
                                          Exec (exec_idx - 1))
                                     | Store(e1, _, e2) -> if self#depends_pending_load e1 || self#depends_pending_load e2 then
                                          self#get_exec conf
                                        else (
                                          sent_exec <- true;
                                          exec_trace <- (exec_idx - 1, Option.get istr) :: exec_trace;
                                          Exec (exec_idx - 1))
                                     | Load(ide, _, e) -> if self#depends_pending_load e then
                                          self#get_exec conf
                                        else (
                                          pending_loads_buffer <- pending_loads_buffer @ [(ide, mem_op_cost)];
                                          sent_exec <- true;
                                          exec_trace <- (exec_idx - 1, Option.get istr) :: exec_trace;
                                          Exec (exec_idx - 1))
        )

      method get_next_directive (conf : configuration) : directive =
        self#check_step conf;
        if List.length conf.is < fetched_block_len && conf.cs != [] then
          self#get_fetch (List.hd conf.cs)
        else if self#can_retire conf.is then (
            exec_idx <- 0;
            sent_exec <- false;
            Retire
          )
          else
            self#get_exec conf

      method print_state () =
        (* printf "\n------- Fetch trace\n";
        printf "%s" (String.concat "\n#####\n" (List.rev_map string_of_cmd fetch_trace)); *)
        printf "\n------- Exec trace\n";
        printf "%s" (String.concat "\n" (List.rev_map (fun (n, istr) -> string_of_int n ^ ": " ^ string_of_instruction istr) exec_trace));
        printf "\n";
    end;;
end;;

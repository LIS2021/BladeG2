open Commands;;
open Option;;

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

Random.self_init ()

module Processor = struct
  class type processor_t =
    object
      method get_next_directive : Commands.configuration -> Commands.directive
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
      end;;
end;;
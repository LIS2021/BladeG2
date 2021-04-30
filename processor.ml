open Commands;;

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

end;;
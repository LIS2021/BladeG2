open Option;;
open Expr;;
open Commands;;

let split (l : 'a list) (n : int) =
  if List.length l <= n then none else
    let rec _split (l1, x, l2) (n : int) =
      match n with
      | 0 -> some (l1, x, l2)
      | n -> _split (l1 @ [x], List.hd l2, List.tl l2) (n - 1)
    in _split ([], List.hd l, List.tl l) n

let rec take k xs = match xs with
    | [] -> []
    | x::xs -> if k <= 0 then [] else x::take (k-1) xs;;

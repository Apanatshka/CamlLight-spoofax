
(* Define constant constructor a *)
type A = a | c;;

let f c = if c = a then 1 else 2;;

f a;;

(* This is an example of undecidability of polymorphic recursion
(which isn't even in CamlLight, but it's still a nice example) *)
let rec diverge x = diverge diverge x;;
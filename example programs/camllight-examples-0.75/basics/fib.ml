(* The Fibonacci function, once more. *)

# open "sys";;

let rec Fib n =
  if n < 2 then 1 else Fib(n - 1) + Fib(n - 2)
;;

let z = z + 1;;

if sys__interactive then () else
if vect_length sys__command_line <> 2 then begin
  print_string "Usage: fib <number>";
  print_newline()
end else begin
  try
    print_int(Fib(int_of_string sys__command_line.(1)));
    print_newline()
  with Failure "int_of_string" ->
    print_string "Bad integer constant";
    print_newline()
end
;;

#close "sys";;

let rec Fib n =
  if n < 2 then 1 else Fib(n - 1) + Fib(n - 2)
;;
(* $Id: run.ml,v 1.1.1.1 2002/05/28 15:59:17 weis Exp $ *)

#open "main";;

input_stream := stdin;;

if sys__interactive then () else begin go(); exit 0 end;;

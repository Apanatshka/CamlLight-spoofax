(* $Id: load.ml,v 1.1.1.1 2002/05/28 15:59:17 weis Exp $ *)

#open "token";;
#open "parser";;
#open "semant";;
#open "typing";;
#open "main";;

load_object "hash";
load_object "asl";
load_object "token";
load_object "parser";
load_object "semant";
load_object "typing";
load_object "main";;


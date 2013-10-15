(* $Id: asl.mli,v 1.1.1.1 2002/05/28 15:59:17 weis Exp $ *)

exception Error of string;;
type 'a option = None | Some of 'a;;
value init_env : string list;;

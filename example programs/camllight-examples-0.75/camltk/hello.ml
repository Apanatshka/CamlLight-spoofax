#open "tk";;

let action () = print_string "Bonjour!"; print_newline ();;

let hello () =
  let fen�tre_principale = openTk () in
  let bouton_press =
    button__create fen�tre_principale
      [Text "Pressez-moi"; Command action] in
  pack [bouton_press] [];
  mainLoop ();;

if sys__interactive then () else hello ();;

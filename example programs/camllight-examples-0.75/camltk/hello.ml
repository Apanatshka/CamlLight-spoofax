#open "tk";;

let action () = print_string "Bonjour!"; print_newline ();;

let hello () =
  let fenêtre_principale = openTk () in
  let bouton_press =
    button__create fenêtre_principale
      [Text "Pressez-moi"; Command action] in
  pack [bouton_press] [];
  mainLoop ();;

if sys__interactive then () else hello ();;

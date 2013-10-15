#open "tk";;

let synchronise_zones source dest taux_source taux_dest =
  function infos ->
    try
      let montant_source = float_of_string (entry__get source) in
      let montant_dest =
        montant_source *. !taux_source /. !taux_dest in
      entry__delete_range dest (At 0) End;
      entry__insert dest (At 0)
                         (printf__sprintf "%.2f" montant_dest)
    with Failure _ ->
      entry__delete_range dest (At 0) End;
      entry__insert dest (At 0) "erreur";;

let convertit_en_francs () =
  let fp = openTk () in

  let ligne1 = frame__create fp []
  and ligne2 = frame__create fp [] in

  let �tiq1 = label__create ligne1 [Text "Francs:"]
  and entr�e1 = entry__create ligne1 [TextWidth 10; Relief Sunken]

  and �tiq2 = label__create ligne2 [Text "Euros:"]
  and entr�e2 = entry__create ligne2 [TextWidth 10; Relief Sunken] in

  let quit = button__create fp [Text "Quit"; Command closeTk] in

  let taux1 = ref 1.0     (* francs pour 1 franc *)
  and taux2 = ref 6.55957 (* francs pour 1 euro *) in

  bind entr�e1 [[], KeyRelease]
       (BindSet([], synchronise_zones entr�e1 entr�e2 taux1 taux2));
  bind entr�e2 [[], KeyRelease]
       (BindSet([], synchronise_zones entr�e2 entr�e1 taux2 taux1));
 
  pack [�tiq1] [Side Side_Left]; pack [entr�e1] [Side Side_Right];
  pack [�tiq2] [Side Side_Left]; pack [entr�e2] [Side Side_Right];
  pack [ligne1; ligne2] [Side Side_Top; Fill Fill_X];
  pack [quit] [Side Side_Bottom; Fill Fill_X]; 
  mainLoop ();;

if sys__interactive then () else begin convertit_en_francs (); exit 0 end;;

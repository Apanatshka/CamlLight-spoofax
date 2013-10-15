#open "tk";;

let rgb () =
  let fen�tre_principale = openTk () in

  let cr�er_glissi�re nom =
    scale__create fen�tre_principale
                  [Label nom; From 0.0; To 255.0;
                   Length(Centimeters 10.0); Orient Horizontal] in

  let rouge = cr�er_glissi�re "Rouge"
  and vert  = cr�er_glissi�re "Vert"
  and bleu  = cr�er_glissi�re "Bleu"
  and �chantillon =
    frame__create fen�tre_principale
                  [Height(Centimeters 1.5); Width(Centimeters 6.0)]
  and quitter =
    button__create fen�tre_principale
                   [Text "Quitter"; Command closeTk] in

  let rafra�chir_couleur x =
    let r = int_of_float(scale__get rouge)
    and v = int_of_float(scale__get vert)
    and b = int_of_float(scale__get bleu) in
    let couleur = printf__sprintf "#%02x%02x%02x" r v b in
    frame__configure �chantillon [Background (NamedColor couleur)] in

  scale__configure rouge [ScaleCommand rafra�chir_couleur];
  scale__configure vert [ScaleCommand rafra�chir_couleur];
  scale__configure bleu [ScaleCommand rafra�chir_couleur];
  pack [rouge; vert; bleu] [Side Side_Top];
  pack [quitter] [Side Side_Bottom];
  pack [�chantillon] [Side Side_Bottom; PadY(Millimeters 2.0)];
  mainLoop ();;

if sys__interactive then () else begin rgb (); exit 0 end;;

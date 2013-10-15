#open "tk";;

let rgb () =
  let fenêtre_principale = openTk () in

  let créer_glissière nom =
    scale__create fenêtre_principale
                  [Label nom; From 0.0; To 255.0;
                   Length(Centimeters 10.0); Orient Horizontal] in

  let rouge = créer_glissière "Rouge"
  and vert  = créer_glissière "Vert"
  and bleu  = créer_glissière "Bleu"
  and échantillon =
    frame__create fenêtre_principale
                  [Height(Centimeters 1.5); Width(Centimeters 6.0)]
  and quitter =
    button__create fenêtre_principale
                   [Text "Quitter"; Command closeTk] in

  let rafraîchir_couleur x =
    let r = int_of_float(scale__get rouge)
    and v = int_of_float(scale__get vert)
    and b = int_of_float(scale__get bleu) in
    let couleur = printf__sprintf "#%02x%02x%02x" r v b in
    frame__configure échantillon [Background (NamedColor couleur)] in

  scale__configure rouge [ScaleCommand rafraîchir_couleur];
  scale__configure vert [ScaleCommand rafraîchir_couleur];
  scale__configure bleu [ScaleCommand rafraîchir_couleur];
  pack [rouge; vert; bleu] [Side Side_Top];
  pack [quitter] [Side Side_Bottom];
  pack [échantillon] [Side Side_Bottom; PadY(Millimeters 2.0)];
  mainLoop ();;

if sys__interactive then () else begin rgb (); exit 0 end;;

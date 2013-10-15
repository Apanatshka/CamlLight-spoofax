#open "tk";;

let d�coupe_image img nx ny =
  let l = imagephoto__width img
  and h = imagephoto__height img in
  let tx = l / nx and ty = h / ny in
  let pi�ces = ref [] in
  for x = 0 to nx - 1 do
    for y = 0 to ny - 1 do
      let pi�ce = imagephoto__create
                    [Width (Pixels tx); Height (Pixels ty)] in
      imagephoto__copy pi�ce img
        [ImgFrom(x * tx, y * ty, (x + 1) * tx, (y + 1) * ty)];
      pi�ces := pi�ce :: !pi�ces
    done
  done;
  (tx, ty, tl !pi�ces);;

let remplir_taquin c nx ny tx ty pi�ces =
  let trou_x = ref (nx - 1)
  and trou_y = ref (ny - 1) in
  let trou =
    canvas__create_rectangle c
      (Pixels (!trou_x * tx)) (Pixels (!trou_y * ty))
      (Pixels tx) (Pixels ty) [] in
  let taquin = make_matrix nx ny trou in
  let p = ref pi�ces in
  for x = 0 to nx - 1 do
    for y = 0 to ny - 1 do
      match !p with
      | [] -> ()
      | pi�ce :: reste ->
          taquin.(x).(y) <-
            canvas__create_image c
                (Pixels (x * tx)) (Pixels (y * ty))
                [ImagePhoto pi�ce; Anchor NW; Tags [Tag "pi�ce"]];
          p := reste
    done
  done;
  let d�placer x y =
    let pi�ce = taquin.(x).(y) in
    canvas__coords_set c pi�ce
      [Pixels (!trou_x * tx); Pixels(!trou_y * ty)];
    canvas__coords_set c trou
      [Pixels (x * tx); Pixels(y * ty); Pixels tx; Pixels ty];
    taquin.(!trou_x).(!trou_y) <- pi�ce;
    taquin.(x).(y) <- trou;
    trou_x := x; trou_y := y in
  let jouer ei =
    let x = ei.ev_MouseX / tx and y = ei.ev_MouseY / ty in
    if x = !trou_x && (y = !trou_y - 1 || y = !trou_y + 1)
    || y = !trou_y && (x = !trou_x - 1 || x = !trou_x + 1)
    then d�placer x y in
  canvas__bind c (Tag "pi�ce") [[], ButtonPress]
                 (BindSet ([Ev_MouseX; Ev_MouseY], jouer));;

let rec permutation = function
  | [] -> []
  | l  -> let n = random__int (list_length l) in
          let (�l�ment, reste) = partage l n in
          �l�ment :: permutation reste

and partage l n =
  match l with
  | [] -> failwith "partage"
  | t�te :: reste ->
      if n = 0 then (t�te, reste) else
        let (�l�ment, reste') = partage reste (n - 1) in
        (�l�ment, t�te :: reste');;

let taquin nom_fichier nx ny =
  let fp = openTk () in
  let img = imagephoto__create [File nom_fichier] in
  let c = canvas__create fp
          [Width(Pixels(imagephoto__width img));
           Height(Pixels(imagephoto__height img))] in
  let (tx, ty, pi�ces) = d�coupe_image img nx ny in
  remplir_taquin c nx ny tx ty (permutation pi�ces);
  pack [c] [];
  let quit = button__create fp [Text "Quit"; Command closeTk] in
  pack [quit] [Side Side_Bottom; Fill Fill_X]; 
  mainLoop ();;

if sys__interactive then () else begin taquin "joconde.gif" 3 5; exit 0 end;;


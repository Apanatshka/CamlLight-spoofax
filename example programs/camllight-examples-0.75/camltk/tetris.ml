(*************************************************************************)
(*                                                                       *)
(*                Objective Caml CamlTk library                          *)
(*                                                                       *)
(*         Jun Furuse, projet Cristal, INRIA Rocquencourt                *)
(*        Pierre Weis, projet Cristal, INRIA Rocquencourt                *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.                               *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* A Tetris game for CamlTk *)
(* Written by Jun P. Furuse *)
(* Adapted to the oc examples repository  by P. Weis *)

#open "tk";;
#open "protocol";;

(* The directory where images will be found. *)
let baseurl = "images/";;

exception Done;;

type cell = {
 mutable color : int; 
 tag : tagOrId * tagOrId * tagOrId
};;

type falling_block = {
  mutable pattern : int vect list;
  mutable bcolor : int;
  mutable x : int;
  mutable y : int;
  mutable d : int;
  mutable alive: bool
};;

let stop_a_bit = 300;;

let colors = [|
  NamedColor "red"; NamedColor "yellow"; NamedColor "blue";
  NamedColor "orange"; NamedColor "magenta"; NamedColor "green";
  NamedColor "cyan"
|];;

let backgrounds = 
  map (fun s -> baseurl ^ s)
    [ "dojoji.back.gif"; "Lambda2.back.gif"; "CamlBook.gif"; ];;

(* blocks *)
let block_size = 16
and cell_border = 2;;

let blocks = [
  [ [|"0000"; "0000"; "1111"; "0000" |];
    [|"0010"; "0010"; "0010"; "0010" |];
    [|"0000"; "0000"; "1111"; "0000" |];
    [|"0010"; "0010"; "0010"; "0010" |] ];

  [ [|"0000"; "0110"; "0110"; "0000" |];
    [|"0000"; "0110"; "0110"; "0000" |];
    [|"0000"; "0110"; "0110"; "0000" |];
    [|"0000"; "0110"; "0110"; "0000" |] ];

  [ [|"0000"; "0111"; "0100"; "0000" |];
    [|"0000"; "0110"; "0010"; "0010" |];
    [|"0000"; "0010"; "1110"; "0000" |];
    [|"0100"; "0100"; "0110"; "0000" |] ];

  [ [|"0000"; "0100"; "0111"; "0000" |];
    [|"0000"; "0110"; "0100"; "0100" |];
    [|"0000"; "1110"; "0010"; "0000" |];
    [|"0010"; "0010"; "0110"; "0000" |] ];

  [ [|"0000"; "1100"; "0110"; "0000" |];
    [|"0010"; "0110"; "0100"; "0000" |];
    [|"0000"; "1100"; "0110"; "0000" |];
    [|"0010"; "0110"; "0100"; "0000" |] ];

  [ [|"0000"; "0011"; "0110"; "0000" |];
    [|"0100"; "0110"; "0010"; "0000" |];
    [|"0000"; "0011"; "0110"; "0000" |];
    [|"0000"; "0100"; "0110"; "0010" |] ];

  [ [|"0000"; "0000"; "1110"; "0100" |];
    [|"0000"; "0100"; "1100"; "0100" |];
    [|"0000"; "0100"; "1110"; "0000" |];
    [|"0000"; "0100"; "0110"; "0100" |] ];
];;

let line_empty = int_of_string "0b1110000000000111"
and line_full = int_of_string  "0b1111111111111111";;

let decode_block dvec =
  let btoi d = int_of_string ("0b" ^ d) in
  map_vect btoi dvec;;

let init fw =
  let scorev = textvariable__create ()
  and linev = textvariable__create ()
  and levv = textvariable__create ()
  and namev = textvariable__create () in
  let f = frame__create fw [BorderWidth (Pixels 2)] in
  let c =
    canvas__create f
     [Width (Pixels (block_size * 10));
      Height (Pixels (block_size * 20));
      BorderWidth (Pixels cell_border);
      Relief Sunken;
      Background Black]
  and r = frame__create f []
  and r' = frame__create f [] in

  let nl = label__create r [Text "Next"; Font "variable"] in
  let nc =
    canvas__create r
     [Width (Pixels (block_size * 4));
      Height (Pixels (block_size * 4));
      BorderWidth (Pixels cell_border);
      Relief Sunken;
      Background Black] in                           
  let scl = label__create r [Text "Score"; Font "variable"] in
  let sc = label__create r [TextVariable scorev; Font "variable"] in
  let lnl = label__create r [Text "Lines"; Font "variable"] in
  let ln = label__create r [TextVariable linev; Font "variable"] in
  let levl = label__create r [Text "Level"; Font "variable"] in
  let lev = label__create r [TextVariable levv; Font "Variable"] in 
  let newg = button__create r [Text "New Game"; Font "variable"] in
  let exitg = button__create r [Text "Quit"; Font "variable"] in

  pack [f] [];
  pack [c; r; r'] [Side Side_Left; Fill Fill_Y];
  pack [nl; nc] [Side Side_Top];
  pack [scl; sc; lnl; ln; levl; lev; newg; exitg] [Side Side_Top];

  let cells_src = make_matrix 20 10 () in
  let cells = map_vect (map_vect (fun () ->
    {tag =
      (let t1, t2, t3 =
         canvas__create_rectangle c 
           (Pixels (-block_size - 8)) (Pixels (-block_size - 8))
           (Pixels (-9)) (Pixels (-9)) [],
          canvas__create_rectangle c 
           (Pixels (-block_size - 10)) (Pixels (-block_size - 10))
           (Pixels (-11)) (Pixels (-11)) [],
          canvas__create_rectangle c 
           (Pixels (-block_size - 12)) (Pixels (-block_size - 12))
           (Pixels (-13)) (Pixels (-13)) [] in
       canvas__raise_top c t1;
       canvas__raise_top c t2;
       canvas__lower_bot c t3;
       t1, t2, t3);
     color = 0})) cells_src in
  let nexts_src = make_matrix 4 4 () in
  let nexts = 
   map_vect (map_vect (fun () ->
    {tag = 
      (let t1, t2, t3 =
         canvas__create_rectangle nc 
           (Pixels (-block_size - 8)) (Pixels (-block_size - 8))
           (Pixels (-9)) (Pixels (-9)) [],
         canvas__create_rectangle nc 
           (Pixels (-block_size - 10)) (Pixels (-block_size - 10))
           (Pixels (-11)) (Pixels (-11)) [],
         canvas__create_rectangle nc 
           (Pixels (-block_size - 12)) (Pixels (-block_size - 12))
           (Pixels (-13)) (Pixels (-13)) [] in
       canvas__raise_top nc t1;
       canvas__raise_top nc t2;
       canvas__lower_bot nc t3;
       t1, t2, t3);
     color = 0})) nexts_src in
  let game_over () = ()
  in
    [f; c; r; nl; nc; scl; sc; levl; lev; lnl; ln], newg, exitg,
      (c, cells), (nc, nexts), scorev, linev, levv, game_over;;
  
let cell_get (c, cf) x y = cf.(y).(x).color;;

let cell_set (c, cf) x y col =
  let cur = cf.(y).(x) in
  let t1, t2, t3 = cur.tag in 
  if cur.color = col then () else
  if cur.color <> 0 && col = 0 then begin
    canvas__move c t1
      (Pixels (- block_size * (x + 1) -10 - cell_border * 2))
      (Pixels (- block_size * (y + 1) -10 - cell_border * 2));
    canvas__move c t2
      (Pixels (- block_size * (x + 1) -10 - cell_border * 2))
      (Pixels (- block_size * (y + 1) -10 - cell_border * 2));
    canvas__move c t3
      (Pixels (- block_size * (x + 1) -10 - cell_border * 2))
      (Pixels (- block_size * (y + 1) -10 - cell_border * 2))
      
  end else begin
    canvas__configure_rectangle c t2
      [FillColor (colors.(col - 1)); 
       Outline (colors.(col - 1))];
    canvas__configure_rectangle c t1
      [FillColor Black;
       Outline Black];
    canvas__configure_rectangle c t3
      [FillColor (NamedColor "light gray");
       Outline (NamedColor "light gray")];
    if cur.color = 0 && col <> 0 then begin
      canvas__move c t1
        (Pixels (block_size * (x+1)+10+ cell_border*2))
        (Pixels (block_size * (y+1)+10+ cell_border*2));
      canvas__move c t2
        (Pixels (block_size * (x+1)+10+ cell_border*2))
        (Pixels (block_size * (y+1)+10+ cell_border*2));
      canvas__move c t3
        (Pixels (block_size * (x+1)+10+ cell_border*2))
        (Pixels (block_size * (y+1)+10+ cell_border*2))
    end     
  end;
  cur.color <- col;;

let draw_block field col d x y =
  for iy = 0 to 3 do
    let base = ref 1 in
    let xd = d.(iy) in
    for ix = 0 to 3 do
      if xd land !base <> 0 then begin
        try cell_set field (ix + x) (iy + y) col with _ -> ()  
      end;
      base := !base lsl 1
    done
  done;;

let timer_ref = (ref None : timer option ref);;

let remove_timer () =
  match !timer_ref with
  | None -> ()
  | Some t -> remove_timer t;;

let do_after milli f = timer_ref := Some (add_timer milli f);;

let copy_block c = 
  { pattern = !c.pattern;
    bcolor = !c.bcolor;
    x = !c.x;
    y = !c.y;
    d = !c.d;
    alive = !c.alive };;

let start_game () =
  let top = openTk () in
  wm__title_set top "";
  let lb = label__create top []
  and fw = frame__create top [] in
  let set_message s = label__configure lb [Text s] in
  pack [lb; fw] [Side Side_Top]; 
  let score = ref 0 in
  let line = ref 0 in
  let level = ref 0 in
  let time = ref 1000 in
  let blocks = map (map decode_block) blocks in
  let field = make_vect 26 0 in
  let widgets, newg, exitg, cell_field, next_field,
      scorev, linev, levv, game_over = init fw in
  let canvas = fst cell_field in

  let init_field () =
    for i = 0 to 25 do
      field.(i) <- line_empty
    done;
    field.(23) <- line_full;
    for i = 0 to 19 do
      for j = 0 to 9 do
        cell_set cell_field j i 0
      done
    done;
    for i = 0 to 3 do
      for j = 0 to 3 do
        cell_set next_field j i 0
      done
    done in

  let rec nth l n =
    match l with
      [] -> failwith "nth"
    |  a::l ->
        if n = 0 then a else
        if n > 0 then nth l (n-1) else
        invalid_arg "nth" in

  let ignore _ = () in

  let draw_falling_block fb =
    draw_block cell_field fb.bcolor 
      (nth fb.pattern fb.d) (fb.x - 3) (fb.y - 3)
  and erase_falling_block fb =
    draw_block cell_field 0 (nth fb.pattern fb.d) (fb.x - 3) (fb.y - 3) in

  let stone fb =
    for i = 0 to 3 do
      let cur = field.(i + fb.y) in
      field.(i + fb.y) <-
         cur lor ((nth fb.pattern fb.d).(i) lsl fb.x)
    done;
    for i = 0 to 2 do field.(i) <- line_empty done

  and clear fb =
    let l = ref 0 in
    for i = 0 to 3 do
      if i + fb.y >= 3 && i + fb.y <= 22 &&
         field.(i + fb.y) = line_full then begin
        incr l;
        field.(i + fb.y) <- line_empty;
        for j = 0 to 9 do cell_set cell_field j (i + fb.y - 3) 0 done
      end
    done;
    !l

  and fall_lines () =
    let eye = ref 22 (* bottom *)
    and cur = ref 22 (* bottom *) in
    try
      while !eye >= 3 do
        while field.(!eye) = line_empty do
          decr eye;
          if !eye = 2 then raise Done
        done;
        field.(!cur) <- field.(!eye);
        for j = 0 to 9 do
          cell_set cell_field j (!cur-3) (cell_get cell_field j (!eye-3))
        done;
        decr eye;
        decr cur
      done
    with Done -> ();
      for i = 3 to !cur do
        field.(i) <- line_empty;
        for j = 0 to 9 do cell_set cell_field j (i - 3) 0 done
      done in

  let next = ref 42 (* THE ANSWER *)
  and current =
    ref { pattern= [[|0; 0; 0; 0|]];
          bcolor = 0; x = 0; y = 0; d = 0; alive = false} in

  let draw_next () =
    draw_block next_field (!next + 1) (hd (nth blocks !next)) 0 0 

  and erase_next () =
    draw_block next_field 0 (hd (nth blocks !next)) 0 0 in

  let set_nextblock () =
    current :=
       { pattern = (nth blocks !next);
         bcolor = !next + 1;
         x = 6; y = 1; d = 0; alive = true};
    erase_next ();
    next := random__int 7;
    draw_next () in

  let death_check fb =
    try
      for i=0 to 3 do
        let cur = field.(i + fb.y) in
        if cur land ((nth fb.pattern fb.d).(i) lsl fb.x) <> 0 
        then raise Done
      done;
      false
    with 
      Done -> true in

  let try_to_move m =
    if !current.alive then
      let sub m =
        if death_check m then false
        else
          begin
            erase_falling_block !current;
            draw_falling_block m;
            current := m;
            true
          end in
      if sub m then () else begin
        m.x <- m.x + 1;
        if sub m then () else begin 
          m.x <- m.x - 2;
          ignore (sub m)
        end  
      end
    else () in

  let image_load =
    let i =
      canvas__create_image canvas 
        (Pixels (block_size * 5 + block_size / 2)) 
        (Pixels (block_size * 10 + block_size / 2))
        [Anchor Center] in
    canvas__lower_bot canvas i;
    let img = imagephoto__create [] in
    fun file ->
      try 
        imagephoto__configure img [File file];
        canvas__configure_image canvas i [ImagePhoto img]
      with _ -> printf__eprintf "%s : No such image...\n" file; flush stderr in

  let add_score l =
    let pline = !line in
    if l <> 0 then
      begin
        line := !line + l; 
        score := !score + l * l;
        set_message (printf__sprintf "%d pts" (1 lsl ((l - 1) * 2)))
      end; 
    textvariable__set linev (string_of_int !line);
    textvariable__set scorev (string_of_int !score); 

    if !line / 10 <> pline / 10 then 
      (* update the background every 10 lines. *)
      begin
        let num_image = list_length backgrounds - 1 in
        let n = !line / 10 in
        let n = if n > num_image then num_image else n in
        let file = nth backgrounds n in
        image_load file;
        (* Future work: We should gain level after an image is put... *)
        incr level; 
        textvariable__set levv (string_of_int !level) 
      end in

  let rec newblock () = 
    set_message "TETRIS";
    set_nextblock ();
    draw_falling_block !current;
    if death_check !current then begin
        !current.alive <- false;
        set_message "GAME OVER";
        game_over ()
    end else begin
      time := 1100 - (!level / 4 * 300) - ((!level mod 4) * 200);
      if !time < 60 - !level * 3 then time := 60 - !level * 3;
      do_after stop_a_bit loop
    end

  and loop () =
    let m = copy_block current in
    m.y <- m.y + 1;
    if death_check m then begin
      !current.alive <- false;
      stone !current;
      do_after stop_a_bit (fun () ->
        let l = clear !current in
        if l > 0 then
          do_after stop_a_bit (fun () ->
            fall_lines ();
            add_score l;
            do_after stop_a_bit newblock)
        else newblock ())
    end else begin
      erase_falling_block !current;
      draw_falling_block m;
      current := m;
      do_after !time loop
    end in

  let bind_game w =
    bind w [([], KeyPress)] (BindSet ([Ev_KeySymString],
      fun e -> 
        match e.ev_KeySymString with
        | "h" ->
            let m = copy_block current in
            m.x <- m.x - 1;
            try_to_move m
        | "j" ->
            let m = copy_block current in
            m.d <- m.d + 1;
            if m.d = list_length m.pattern then m.d <- 0;
            try_to_move m
        | "k" ->
            let m = copy_block current in
            m.d <- m.d - 1;
            if m.d < 0 then m.d <- list_length m.pattern - 1;
            try_to_move m
        | "l" ->
            let m = copy_block current in
            m.x <- m.x + 1;
            try_to_move m
        | "m" ->
            remove_timer ();
            loop ()
        | "space" ->
            if !current.alive then
              begin
                let m = copy_block current
                and n = copy_block current in
                while 
                  m.y <- m.y + 1;
                  if death_check m then false
                  else begin n.y <- m.y; true end
                do () done;
                erase_falling_block !current;
                draw_falling_block n;
                current := n;
                remove_timer ();
                loop ()
              end  
        | _ -> ()
      )) in

  let game_init () =
    (* Game Initialization *)
    set_message "Initializing ...";
    remove_timer ();
    image_load (hd backgrounds);
    time := 1000;
    score := 0;
    line := 0;
    level := 1;
    add_score 0; 
    init_field ();
    next := random__int 7;
    set_message "Welcome to TETRIS";
    set_nextblock ();
    draw_falling_block !current;
    do_after !time loop in

  bind_game top;
  button__configure newg [Command game_init];
  button__configure exitg [Command (fun () -> exit 0)];
  game_init ();;

let tetris () =
 start_game ();
 printexc__f mainLoop ();;

if sys__interactive then () else begin tetris (); exit 0 end;;

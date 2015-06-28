module Tama = 
		struct
		(* (status, health, energy, hygiene, happyness) *)
		type gochi = string * int * int * int * int
		let zero =  ("newborn", 100, 100, 100, 100)
		(***)
		let getone (a,_,_,_,_) = a
		let gettwo (_,a,_,_,_) = a
		let getthree (_,_,a,_,_) = a
		let getfour (_,_,_,a,_) = a
		let getfive (_,_,_,_,a) = a

		let is_dead (x:gochi) =
		getone x = "l-dead" || getone x = "e-dead" || getone x = "b-dead" || getone x = "t-dead" || getone x = "k-dead"

		(***)
		let live (x:gochi) t= 
			match x with 
			| (s, he, e, hy, ha) when he - t > 0 -> (s, he - t, e, hy, ha)
			| (s, he, e, hy, ha) -> ("l-dead", 0, 0, 0, 0)

		let eat (x:gochi) = 
			match x with 
			| (s, he, e, hy, ha) when (he + 25) <= 100  &&  (e - 10) > 0 && (hy - 20) > 0 && (ha + 5) <= 100 ->
					("eat", he + 25, e - 10, hy - 20, ha + 5)
			| (s, he, e, hy, ha) when (he + 25) > 100  &&  (e - 10) > 0 && (hy - 20) > 0 && (ha + 5) <= 100 ->
					("eat", 100, e - 10, hy - 20, ha + 5)
			| (s, he, e, hy, ha) when (he + 25) <= 100  &&  (e - 10) > 0 && (hy - 20) > 0 && (ha + 5) > 100 ->
					("eat", he + 25, e - 10, hy - 20, 100)
			| (s, he, e, hy, ha) when (he + 25) > 100  &&  (e - 10) > 0 && (hy - 20) > 0 && (ha + 5) > 100 ->
					("eat", 100, e - 10, hy - 20, 100)
			| (s, he, e, hy, ha) -> ("e-dead", 0, 0, 0, 0)
		
		let bath (x:gochi) =
			match x with 
			| (s, he, e, hy, ha) when (he - 20) > 0  &&  (e - 10) > 0 && (hy + 25) <= 100 && (ha + 5) <= 100 ->
					("bath", he - 20, e - 10, hy + 25, ha + 5)
			| (s, he, e, hy, ha) when (he - 20) > 0  &&  (e - 10) > 0 && (hy + 25) > 100 && (ha + 5) <= 100 ->
					("bath", he - 20, e - 10, 100, ha + 5)
			| (s, he, e, hy, ha) when (he - 20) > 0  && (e - 10) > 0 && (hy + 25) <= 100 && (ha + 5) >100 ->
					("bath", he - 20, e - 10, hy + 25, 100)
			| (s, he, e, hy, ha) when (he - 20) > 0  &&  (e - 10) > 0 && (hy + 25) > 100 && (ha + 5) >100 ->
					("bath", he - 20, e - 10, 100, 100)
			| (s, he, e, hy, ha) -> ("b-dead", 0, 0, 0, 0)

		let thunder (x:gochi) = 
			match x with 
			| (s, he, e, hy, ha) when (he - 20) > 0  &&  (e + 25) > 100  && (ha - 20) > 0 ->
				("thunder", he - 20, 100, hy, ha - 20)
			| (s, he, e, hy, ha) when (he - 20) > 0  &&  (e + 25) <= 100  && (ha - 20) > 0 ->
				("thunder", he - 20, e + 25 , hy, ha - 20)
			| (s, he, e, hy, ha) -> ("t-dead", he - 10, e + 25, hy, ha - 20)

		let kill (x:gochi) = 
			match x with 
			| (s, he, e, hy, ha) when (he - 20) > 0  && (e - 10) > 0 && (ha + 20) <= 100 ->
					("kill", he - 20, e - 10, hy, ha + 20)
			| (s, he, e, hy, ha) when (he - 20) > 0  &&  (e - 10) > 0 && (ha + 20) > 100 ->
					("kill", he - 20, e - 10, hy, 100)
			| (s, he, e, hy, ha) -> ("k-dead", he - 20, e - 10, hy , ha + 20)

		let save (x:gochi) = 
			match x with 
			| (s, he, e, hy, ha) ->
				let file = "save.itama" in
					let str = (s ^ ":" ^ string_of_int he ^ ":" ^ string_of_int e ^ ":" ^ string_of_int he ^ ":" ^ string_of_int ha) in
						let oc  = open_out file in
							Printf.fprintf oc "%s\n" str;
							close_out oc;
							(s, he, e, hy, ha)

		let load () = 
			try
			let file = "save.itama" in
				let  ic = open_in file in

						let line = input_line ic in
							let tab = (Str.split (Str.regexp ":") line) in
								(List.nth tab 0, int_of_string (List.nth tab 1), int_of_string (List.nth tab 2), int_of_string (List.nth tab 3), int_of_string (List.nth tab 4))
					with  e -> print_endline "Save not found or save file corrupted.\nStarting new game."; ("newborn", 100, 100, 100, 100)

	end

let draw_button () =
	Graphics.draw_rect 55 10 40 30;
	Graphics.draw_rect 105 10 40 30;
	Graphics.draw_rect 155 10 40 30;
	Graphics.draw_rect 205 10 40 30;
	Graphics.set_font "-*-fixed-medium-r-semicondensed--10-*-*-*-*-*-iso8859-1";
	Graphics.moveto 158 21;
	Graphics.draw_string "THUNDER";
	Graphics.set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
	Graphics.moveto 66 18;
	Graphics.draw_string "EAT";
	Graphics.moveto 111 18;
	Graphics.draw_string "BATH";
	Graphics.moveto 212 18;
	Graphics.draw_string "KILL"


let suffixe_tmp = ".tmp "
and rm = 
  if Sys.os_type="Unix" then
    "rm -f "
  else
    "del " 
and mv = 
  if Sys.os_type="Unix" then
    "mv "
  else
    "move " 
and dev_null =
  if Sys.os_type="Unix" then
    " 2> /dev/null"
  else
    "" 

let lire_image_ppm nom = 
  let entree = open_in_bin nom 
  in
    let format = input_line entree 
    and largeur,hauteur = 
      let ligne = ref (input_line entree)
      in
	while !ligne.[0] = '#' do
	  ligne := input_line entree
	done ;
	Scanf.sscanf 
	  !ligne 
          "%d %d" 
	  (fun x y -> x,y)
    and _ = input_line entree (* lecture de la ligne contenant 255 *)
    in
      let img = Array.make_matrix hauteur largeur (Graphics.rgb 0 0 0)
      and en_couleur = (format = "P6")
      in
	for i = 0 to hauteur - 1 do
	  for j = 0 to largeur - 1 do
	    img.(i).(j) <- 
	      if en_couleur then
		let x = input_byte entree
		and y = input_byte entree
		and z = input_byte entree
		in 
		  Graphics.rgb x y z
	      else
		let x = input_byte entree 
		in 
		  Graphics.rgb x x x
	  done
	done ;
	close_in entree ;
	img 

let lire_image nom =
  let r = Sys.command ("convert -depth 8 "^nom^" "^nom^".ppm "^dev_null)
  in
    if r <> 0 then
      failwith ("lire_image : fichier "^nom^" manquant ou pas dans un format image")
    else
      let res = lire_image_ppm (nom^".ppm")
      in
	ignore(Sys.command (rm^nom^".ppm"));
	res

let draw_tama str =
	try 
	let img = lire_image str in
	Graphics.draw_image (Graphics.make_image img) 75 75
	with e
	-> Graphics.moveto 75 75;
		Graphics.draw_string "Image not found"

let draw_fill truc x y = match truc with
	| n when n < 33 -> Graphics.set_color Graphics.red;
					Graphics.fill_rect x y (n / 2) 10;
					Graphics.set_color Graphics.white;
					Graphics.fill_rect (x + n / 2) y (100 - n / 2) 10
	| n when n < 66 ->Graphics.set_color Graphics.yellow;
					Graphics.fill_rect x y (n / 2) 10;
					Graphics.set_color Graphics.white;
					Graphics.fill_rect (x + n / 2) y (100 - n / 2) 10
	| n ->Graphics.set_color Graphics.green;
			Graphics.fill_rect x y (n / 2) 10;
			Graphics.set_color Graphics.white;
			Graphics.fill_rect (x + n / 2) y (100 - n / 2) 10

let draw_status he e hy ha =
	draw_fill he 35 250;
	draw_fill e 95 250;
	draw_fill hy 155 250;
	draw_fill ha 215 250;
	Graphics.set_color Graphics.black;
	Graphics.draw_rect 35 250 50 10;
	Graphics.draw_rect 95 250 50 10;
	Graphics.draw_rect 155 250 50 10;
	Graphics.draw_rect 215 250 50 10;
	Graphics.set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
	Graphics.moveto 40 260;
	Graphics.draw_string "HEALTH";
	Graphics.moveto 101 260;
	Graphics.draw_string "ENERGY";
	Graphics.moveto 157 260;
	Graphics.draw_string "HYGYENE";
	Graphics.moveto 223 260;
	Graphics.draw_string "HAPPY"

let draw_endgame str =
	Graphics.clear_graph ();
	draw_tama "kill_pika.png";
	Graphics.moveto 1 75;
	Graphics.set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
	Graphics.draw_string str;
	Graphics.draw_rect 50 200 200 50;
	Graphics.moveto 98 215;
	Graphics.set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
	Graphics.draw_string "REINCARNATE"

let draw_game (x:Tama.gochi) =
	ignore (Tama.save x);
	match x with
	|("l-dead", _, _, _, _) -> draw_endgame "You lived and then you died."
	|("e-dead", _, _, _, _) -> draw_endgame "You died will eating, you glutton !"
	|("b-dead", _, _, _, _) -> draw_endgame "You took a bath Claude Francois style."
	|("t-dead", _, _, _, _) -> draw_endgame "You were not thunder proof..."
	|("k-dead", _, _, _, _) -> draw_endgame "Your opponent was stronger and killed you."
	|(s, he, e, hy, ha) ->begin
						draw_button ();
						draw_tama "base_pika.png";
						draw_status he e hy ha
					end


let print_stat (x:Tama.gochi) =
	match x with 
	| (s, he, e, hy, ha) ->	print_endline ("Tama: ( " ^ s ^ ", " ^ string_of_int he ^ ", " ^ string_of_int e ^ ", " ^ string_of_int he ^ ", " ^ string_of_int ha ^ " )")

let rec loop (t:Tama.gochi) (time:int) =

	let bla = (Graphics.wait_next_event [Graphics.Button_down; Graphics.Poll]) in
	if bla.Graphics.button = false && (int_of_float (Unix.time ()) - time >= 1) && Tama.is_dead t = false then 
				begin
					draw_game (Tama.live t (int_of_float (Unix.time ()) - time)); loop (Tama.live t (int_of_float (Unix.time ()) - time)) (int_of_float (Unix.time ()))
				end
	else if bla.Graphics.button = false then
				loop t time
	else
		let status = (Graphics.wait_next_event [Graphics.Button_up]) in 
		match (status.Graphics.mouse_x, status.Graphics.mouse_y) with
		|(x, y) when x >= 55 && x <= 95 && y >= 10 && y <= 40 && Tama.is_dead t = false -> draw_game (Tama.eat t);Unix.sleep 1;loop (Tama.eat t) (int_of_float (Unix.time ()))
		|(x, y) when x >= 105 && x <= 145 && y >= 10 && y <= 40 && Tama.is_dead t = false -> draw_game (Tama.bath t); Unix.sleep 1;loop (Tama.bath t) (int_of_float (Unix.time ()))
		|(x, y) when x >= 155 && x <= 195 && y >= 10 && y <= 40 && Tama.is_dead t = false -> draw_game (Tama.thunder t); Unix.sleep 1;loop (Tama.thunder t) (int_of_float (Unix.time ()))
		|(x, y) when x >= 205 && x <= 245 && y >= 10 && y <= 40 && Tama.is_dead t = false -> draw_game (Tama.kill t); Unix.sleep 1;loop (Tama.kill t) (int_of_float (Unix.time ()))
		|(x, y) when x >= 50 && x <=250 && y >= 200 && y <=250 && Tama.is_dead t -> Graphics.clear_graph (); draw_game ("newborn", 100, 100, 100, 100); loop ("newborn", 100, 100, 100, 100) (int_of_float (Unix.time ()))
		|_ -> 	if ((int_of_float (Unix.time ()) - time >= 1)) && Tama.is_dead t = false then 
				begin
					draw_game (Tama.live t (int_of_float (Unix.time ()) - time)); loop (Tama.live t (int_of_float (Unix.time ()) - time)) (int_of_float (Unix.time ()))
				end
			else
				loop t time

let () =
try
	Graphics.open_graph " 300x300";
	draw_game (Tama.load ());
	loop (Tama.load ()) (int_of_float (Unix.time ()))
with
| _ -> print_endline "Thanks for using our Tamagochi."
	


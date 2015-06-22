type tcontent = U | D | T | Q

type player = {name:string; symbol:tcontent}

type tsmallgrid = {content:tcontent list; status: tcontent}

type tbigGrid = tsmallgrid list


let toString symbol = match symbol with
	|U -> Graphics.set_color (Graphics.rgb 25 25 25); Graphics.draw_string "-"
	|D -> Graphics.set_color (Graphics.rgb 0 120 215);Graphics.set_text_size 5; Graphics.draw_string "O"
	|T -> Graphics.set_color (Graphics.rgb 255 0 0);Graphics.set_text_size 5; Graphics.draw_string "X"
	|Q -> Graphics.set_color (Graphics.rgb 255 0 255); Graphics.draw_string "draw"

let toStringB symbol = match symbol with
	|U -> "-"
	|D -> "\x1B[34mO\x1B[0m"
	|T -> "\x1B[31mX\x1B[0m"
	|Q -> "Draw"

let rec draw_content content x y = match content with
	|[] -> ()
	|t1 :: t2:: t3 :: q -> Graphics.moveto x y;
			toString t1;
			Graphics.draw_string "       ";
			toString t2;
			Graphics.draw_string "       ";
			toString t3;
			draw_content q x (y - 50)
	|_ -> ()

let draw_player status x y = match status with
	| T -> 
		Graphics.set_color (Graphics.rgb 255 0 0);
		Graphics.moveto (x + 10) (y + 10);
		Graphics.lineto (x + 190) (y + 190);
		Graphics.moveto (x + 10) (y + 190);
		Graphics.lineto (x + 190) (y + 10);
	|D -> 
		Graphics.set_color (Graphics.rgb 0 120 215);
		Graphics.fill_circle (x + 100) (y + 100) 80;
	|_ -> ()

let drawSmallGrid grid x y = match grid.status with
	| U -> 	Graphics.set_color (Graphics.rgb 0 0 255);
		 	Graphics.set_line_width 5;
		 	draw_content grid.content (x + 50) (y + 150);
	| _ -> draw_player grid.status x y; Graphics.set_color (Graphics.rgb 0 0 0)

let draw_sep () =
	(*Graphics.background;*)
	Graphics.set_color (Graphics.rgb 155 167 255);
	Graphics.set_line_width 5;
	Graphics.moveto 200 0;
	Graphics.lineto 200 600;
	Graphics.moveto 400 0;
	Graphics.lineto 400 600;
	Graphics.moveto 0 200;
	Graphics.lineto 600 200;
	Graphics.moveto 0 400;
	Graphics.lineto 600 400;
	Graphics.set_color (Graphics.rgb 0 0 0)

let drawBigGrid (grid:tbigGrid) = 
	let rec drawCase g x y=
		match g with
		|[] -> ()
		|t::q -> drawSmallGrid t x y ;
			match x with
			|400 -> drawCase q 0 (y - 200)
			|_ -> drawCase q (x + 200) y
	in
	Graphics.open_graph " 600x600"; 
	drawCase grid 0 400;
	draw_sep ()


let rec cellIsFull grid = match grid.content with
	|[] -> true
	|t::q -> (t <> U) && (cellIsFull {content = q; status = U})

let rec getCase grid n = match (n, grid) with
	|(_, {content = []; _}) ->invalid_arg "Probleme 5"
	|(1, {content = t::q; _}) -> t
	|(m, {content = t::q; _}) when m > 1 -> getCase {content = q; status = U} (m -1)
	|(_, _) ->invalid_arg "Probleme 5"

let rec getSmallGrid (grid:tbigGrid) n = match (n, grid) with
	|(_, []) -> invalid_arg "Probleme 1"
	|(1, t::q) -> t
	|(m, t::q) when m > 1 -> getSmallGrid q (m -1)
	|(_,_) -> invalid_arg "Probleme 1"


let winSmallGrid grid = 
	match grid with
	|{content = []; _} -> invalid_arg "Probleme 6 liste vide"
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t1 = t2 && t1 = t3 && t1 <> U -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); status = t1}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t4 = t5 && t4 = t6 && t4 <> U -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); status = t4}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t7 = t8 && t7 = t9 && t7 <> U -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); status = t7}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t1 = t4 && t1 = t7 && t1 <> U -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); status = t1}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t2 = t5 && t2 = t8 && t2 <> U -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); status = t2}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t3 = t6 && t3 = t9 && t3 <> U -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); status = t3}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t1 = t5 && t1 = t9 && t1 <> U -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); status = t1}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t3 = t5 && t3 = t7 && t3 <> U -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); status = t3}
	|_ -> grid

let winBigGrid grid = match grid with
	|[] -> invalid_arg "Probleme 2"
	|_ when (getSmallGrid grid 1).status = (getSmallGrid grid 2).status && (getSmallGrid grid 1).status = (getSmallGrid grid 3).status && U <> (getSmallGrid grid 1).status -> (getSmallGrid grid 1).status
	|_ when (getSmallGrid grid 1).status = (getSmallGrid grid 4).status && (getSmallGrid grid 1).status = (getSmallGrid grid 7).status && U <> (getSmallGrid grid 1).status -> (getSmallGrid grid 1).status
	|_ when (getSmallGrid grid 1).status = (getSmallGrid grid 5).status && (getSmallGrid grid 1).status = (getSmallGrid grid 9).status && U <> (getSmallGrid grid 1).status -> (getSmallGrid grid 1).status
	|_ when (getSmallGrid grid 4).status = (getSmallGrid grid 5).status && (getSmallGrid grid 4).status = (getSmallGrid grid 6).status && U <> (getSmallGrid grid 4).status -> (getSmallGrid grid 4).status
	|_ when (getSmallGrid grid 7).status = (getSmallGrid grid 8).status && (getSmallGrid grid 7).status = (getSmallGrid grid 9).status && U <> (getSmallGrid grid 7).status -> (getSmallGrid grid 7).status
	|_ when (getSmallGrid grid 3).status = (getSmallGrid grid 6).status && (getSmallGrid grid 3).status = (getSmallGrid grid 9).status && U <> (getSmallGrid grid 9).status -> (getSmallGrid grid 3).status
	|_ when (getSmallGrid grid 2).status = (getSmallGrid grid 5).status && (getSmallGrid grid 2).status = (getSmallGrid grid 8).status && U <> (getSmallGrid grid 8).status -> (getSmallGrid grid 2).status
	|_ when (getSmallGrid grid 3).status = (getSmallGrid grid 5).status && (getSmallGrid grid 3).status = (getSmallGrid grid 7).status && U <> (getSmallGrid grid 7).status -> (getSmallGrid grid 3).status
	|_ when (getSmallGrid grid 1).status != U && (getSmallGrid grid 2).status != U && (getSmallGrid grid 3).status != U && (getSmallGrid grid 4).status != U && (getSmallGrid grid 5).status != U && (getSmallGrid grid 6).status != U && (getSmallGrid grid 7).status != U && (getSmallGrid grid 8).status != U && (getSmallGrid grid 9).status != U -> Q
	|_ -> U

let rec modifSmallGrid grid n (v:tcontent) bol =
	let add x grid = {content = (x::grid.content); status =(grid.status)}
	in
	match (n, grid.content) with
		|(_, []) -> invalid_arg "Probleme 3"
		|(1, t::q) when (cellIsFull {content = v::q; status = grid.status}) && (bol) -> {content = v::q; status = v}
		|(1, t::q) -> {content = v::q; status = grid.status}
		|(m, t::q) when m > 1 -> add t (modifSmallGrid {content = q; status = grid.status} (m - 1) v (t <> U))
		|(_, _) -> invalid_arg "Probleme 3"

let rec modifBigGrid grid n m (v:tcontent) = match (grid, n) with
	|([], _) -> invalid_arg "Probleme 4"
	|(t::q, 1) -> (winSmallGrid (modifSmallGrid t m v true)) :: q 
	|(t::q, l) when l > 1 -> t::(modifBigGrid q (l - 1) m v)
	| _ -> invalid_arg "Probleme 4"

let is_digit c = c >= '1' && c <= '9'

let translate2 c a b = match (a, b) with
	|(e, d) when e = (int_of_char '3') -> (c, char_of_int (6 + d))
	|(e, d) when d = (int_of_char '3') && e = (int_of_char '2') -> (c, '6')
	|(e, d) when d = (int_of_char '3') && e = (int_of_char '1') -> (c, '3')
	|(e, d) -> (c, char_of_int ((e - 1) mod 3 * 3 + d))

let translate a b = match (a, b) with
	|(c, d) when (is_digit c) && (is_digit d) && c <= '3' && d <= '3'-> translate2 '1' (int_of_char c) (int_of_char d)
	|(c, d) when (is_digit c) && (is_digit d) && c <= '3' && d <= '6'-> translate2 '2' (int_of_char c) (int_of_char d - 3)
	|(c, d) when (is_digit c) && (is_digit d) && c <= '6' && d <= '3'-> translate2 '4' (int_of_char c - 3) (int_of_char d)
	|(c, d) when (is_digit c) && (is_digit d) && c <= '6' && d <= '6'-> translate2 '5' (int_of_char c - 3) (int_of_char d - 3)
	|(c, d) when (is_digit c) && (is_digit d) && c <= '3' && d <= '9'-> translate2 '3' (int_of_char c) (int_of_char d - 6)
	|(c, d) when (is_digit c) && (is_digit d) && c <= '9' && d <= '3'-> translate2 '7' (int_of_char c - 6) (int_of_char d)
	|(c, d) when (is_digit c) && (is_digit d) && c <= '9' && d <= '6'-> translate2 '8' (int_of_char c - 6) (int_of_char d - 3)
	|(c, d) when (is_digit c) && (is_digit d) && c <= '6' && d <= '9'-> translate2 '6' (int_of_char c - 3) (int_of_char d - 6)
	|(c, d) when (is_digit c) && (is_digit d) && c <= '9' && d <= '9'-> translate2 '9' (int_of_char c - 6) (int_of_char d - 6)
	|_ -> (a, b)

let rec is_space str = match String.length str with
	|0 -> true
	|n when str.[0] = ' ' -> is_space (String.sub str 1 (n - 1))
	|_ -> false

let check str =
	(is_digit str.[0]) && (is_digit str.[(String.length str) - 1]) && (is_space (String.sub str 1 ((String.length str) - 2)))

let rec getMove player grid =
	drawBigGrid grid;
	print_endline ("It's " ^ player.name ^ "'s turn.");
	print_endline "Where do you want to play ?";
	let buf = read_line () in 
	let buff = match buf with
		|"" -> "Bordel de merde t'es oblige de mettre une ligne vide du con ?"
		|c when String.length c <= 2 -> "Bordel de merde t'es oblige de mettre une ligne vide du con ?"
		|_-> String.trim (buf)
	in
	if check buff then
	begin
	let a = buff.[0] and b = buff.[String.length buff - 1] 
	in 
	let tupple = translate a b in 
	match tupple with
	|(d, c) when is_digit d && is_digit c && (getCase (getSmallGrid grid (int_of_char d - 48)) (int_of_char c - 48)) = U && (getSmallGrid grid (int_of_char d - 48)).status = U -> 	((int_of_char d) - 48, (int_of_char c) - 48)
	|(d, c) when is_digit d && is_digit c -> print_endline "A player already played here !";
							getMove player grid
	|(d, c)->print_endline "Wrong format, please enter two correct digit";
		getMove player grid
end
else
	begin
		print_endline "Wrong format, please enter two correct digit";
		getMove player grid
	end
	

let getInfoPlayer sym=
	print_endline ("Please enter the name for player " ^ toStringB sym);
	match sym with
	| D ->	{name = String.trim ("\x1B[34m"^read_line ()^"\x1B[0m"); symbol = D}
	| _ -> {name = String.trim ("\x1B[31m"^read_line ()^"\x1B[0m"); symbol = T}

let rec launchGame playerA playerB grid=
	let (a, b) = getMove playerA grid in
	let cur_grid = modifBigGrid grid a b playerA.symbol in
	if winBigGrid cur_grid <> U then
	begin
		drawBigGrid cur_grid;
		winBigGrid cur_grid
	end
	else
		launchGame playerB playerA cur_grid
		


let initGrid = [{content = [U;U;U;U;U;U;U;U;U]; status =U}; { content = [U;U;U;U;U;U;U;U;U];status = U}; { content = [U;U;U;U;U;U;U;U;U];status = U}; { content = [U;U;U;U;U;U;U;U;U];status = U}; { content = [U;U;U;U;U;U;U;U;U];status = U}; { content = [U;U;U;U;U;U;U;U;U];status = U}; { content = [U;U;U;U;U;U;U;U;U];status = U}; { content = [U;U;U;U;U;U;U;U;U];status = U}; { content = [U;U;U;U;U;U;U;U;U];status = U}]

let rec main ?(r=false) () =
	Random.self_init ();
	let player1 = (getInfoPlayer D) and player2 = (getInfoPlayer T) and rand = (Random.int 2) in
	let oneGame rand = match (rand, r) with
		|(1, true) -> launchGame player2 player1 initGrid
		|(_, _) -> launchGame player1 player2 initGrid 
	in
	let winner = (oneGame rand)
	in
	if winner = Q then
		print_endline "It's a draw !"
	else if winner = D then
	 	print_endline (player1.name ^ " is the winner ! Congratulations.")
	else
		print_endline (player2.name ^ " is the winner ! Congratulations.");
	print_endline "Do you wish to play again ? Enter 'y' for yes, anything else for no";
	let buff = String.trim (read_line ()) in match buff with
	| "y" -> Graphics.clear_graph ();
			main () 
	| _ -> ()


let () = match Sys.argv with
	|y when Array.length y = 1 -> main ()
	|y when (y.(1)) = "-r" -> main ~r:true ()
	|_-> print_endline "Bad argument, launch in normal mode";
		main ()

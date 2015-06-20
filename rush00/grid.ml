type tcontent = U | D | T | Q

type player = {name:string; symbol:tcontent}

type tsmallgrid = {content:tcontent list; status: tcontent}

type tbigGrid = tsmallgrid list


let toString symbol = match symbol with
	|U -> "-"
	|D -> "O"
	|T -> "X"
	|Q -> "Draw"

let rec draw_content content = match content with
	|[] -> ()
	|t :: q -> print_string (toString t ^ " ");
				draw_content q

let draw_player status =
	print_endline (toString status)

let drawSmallGrid grid = match grid.status with
	| U -> draw_content grid.content
	| _ -> draw_player grid.status

let rec drawBigGrid (grid:tbigGrid) = match grid with
	|[] -> ()
	|t::q -> drawSmallGrid t;
			print_endline "|";
			drawBigGrid q

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

let winSmallGrid grid = match grid with
	|{content = []; _} -> invalid_arg "Probleme 6"
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t1 = t2 && t1 = t3 -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::t9::[]); status = t1}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t4 = t5 && t4 = t6 -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::t9::[]); status = t4}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t7 = t8 && t7 = t9 -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::t9::[]); status = t7}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t1 = t4 && t1 = t7 -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::t9::[]); status = t1}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t2 = t5 && t2 = t8 -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::t9::[]); status = t2}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t3 = t6 && t3 = t9 -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::t9::[]); status = t3}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t1 = t5 && t1 = t9 -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::t9::[]); status = t1}
	|{content = (t1::t2::t3::t4::t5::t6::t7::t8::[t9]); _} when t3 = t5 && t3 = t7 -> {content = (t1::t2::t3::t4::t5::t6::t7::t8::t9::[]); status = t3}
	|_ -> grid

let winBigGrid grid = match grid with
	|[] -> invalid_arg "Probleme 2"
	|_ when (getSmallGrid grid 1).status = (getSmallGrid grid 2).status && (getSmallGrid grid 1).status = (getSmallGrid grid 3).status -> (getSmallGrid grid 1).status
	|_ when (getSmallGrid grid 1).status = (getSmallGrid grid 4).status && (getSmallGrid grid 1).status = (getSmallGrid grid 7).status -> (getSmallGrid grid 1).status
	|_ when (getSmallGrid grid 1).status = (getSmallGrid grid 5).status && (getSmallGrid grid 1).status = (getSmallGrid grid 9).status -> (getSmallGrid grid 1).status
	|_ when (getSmallGrid grid 4).status = (getSmallGrid grid 5).status && (getSmallGrid grid 4).status = (getSmallGrid grid 6).status -> (getSmallGrid grid 4).status
	|_ when (getSmallGrid grid 7).status = (getSmallGrid grid 8).status && (getSmallGrid grid 7).status = (getSmallGrid grid 9).status -> (getSmallGrid grid 7).status
	|_ when (getSmallGrid grid 3).status = (getSmallGrid grid 6).status && (getSmallGrid grid 3).status = (getSmallGrid grid 9).status -> (getSmallGrid grid 3).status
	|_ when (getSmallGrid grid 2).status = (getSmallGrid grid 5).status && (getSmallGrid grid 2).status = (getSmallGrid grid 8).status -> (getSmallGrid grid 2).status
	|_ when (getSmallGrid grid 3).status = (getSmallGrid grid 5).status && (getSmallGrid grid 3).status = (getSmallGrid grid 7).status -> (getSmallGrid grid 3).status
	|_ when (getSmallGrid grid 1).status != U && (getSmallGrid grid 2).status != U && (getSmallGrid grid 3).status != U && (getSmallGrid grid 4).status != U && (getSmallGrid grid 5).status != U && (getSmallGrid grid 6).status != U && (getSmallGrid grid 7).status != U && (getSmallGrid grid 8).status != U && (getSmallGrid grid 9).status != U -> Q
	|_ -> U

let rec modifSmallGrid grid n (v:tcontent) bol =
	let add x grid = {content = (x::grid.content); status =(grid.status)}
	in
	match (n, grid.content) with
		|(_, []) -> invalid_arg "Probleme 3"
		|(1, t::q) when (cellIsFull {content = v::q; status = grid.status}) && (bol) -> {content = v::q; status = v}
		|(1, t::q) -> {content = v::q; status = grid.status}
		|(m, t::q) when m > 1 -> add t (modifSmallGrid {content = q; status = grid.status} (m -1) v (t <> U))
		|(_, _) -> invalid_arg "Probleme 3"

let rec modifBigGrid grid n m (v:tcontent) = match (grid, n) with
	|([], _) -> invalid_arg "Probleme 4"
	|(t::q, 1) -> let save = winSmallGrid (modifSmallGrid t m v true) in
			save :: q 
	|(t::q, l) when l > 1 -> t::(modifBigGrid q (l -1) m v)
	| _ -> invalid_arg "Probleme 4"

let is_digit c = c >= '1' && c <= '9'

let rec getMove player grid =
	drawBigGrid grid;
	print_endline ("It's " ^ player.name ^ "'s turn.");
	print_endline "Where do you want to play ?";
	let buf = read_line () in 
	let buff = match buf with
		|"" ->"Bordel de merde t'es oblige de mettre une ligne vide du con ?"
		|_-> String.trim (buf)
	in
	let a = buff.[0] and b = buff.[String.length buff - 1] 
	in match (a, b) with
	|(d, c) when is_digit d && is_digit c && (getCase (getSmallGrid grid (int_of_char d - 48)) (int_of_char c - 48)) = U && (getSmallGrid grid (int_of_char d - 48)).status = U -> ((int_of_char d) - 48, (int_of_char c) - 48)
	|(d, c) when is_digit d && is_digit c -> print_endline "A player already played here !";
							getMove player grid
	|_-> print_endline "Wrong format, please enter two correct digit";
		getMove player grid


let getInfoPlayer symbol=
	print_endline ("Please enter the name for player" ^ toString symbol);
	{name = String.trim (read_line ()); symbol =symbol}

let rec launchGame playerA playerB grid=
	let (a, b) = getMove playerA grid in
	let cur_grid = modifBigGrid grid a b playerA.symbol in
	if winBigGrid cur_grid <> U then
		winBigGrid cur_grid
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
	let winner = oneGame rand in match winner with
		| Q -> print_endline "It's a draw !"
		| D -> print_endline ( player1.name ^ "is the winner ! Congratulations.");
		| _ -> print_endline ( player2.name ^ "is the winner ! Congratulations.");
	print_endline "Do you wish to play again ? Enter 'y' for yes, anything else for no";
	let buff = String.trim (read_line ()) in match buff with
	| "y" -> main () 
	| _ -> ()


let () = main ()

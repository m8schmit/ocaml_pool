let rot_one c =
	if (c < 'a' && c < 'A') || (c > 'Z' && c > 'z') then
		c
	else if c = 'z' || c = 'Z' then
		char_of_int ((int_of_char c) - 25)
	else
		char_of_int ((int_of_char c) + 1)

let rec ft_rot_n n s =
	if n > 0 then
		let s2 = String.map rot_one s in ft_rot_n (n - 1) s2
	else
		s

let main () =
	let s = ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz" in print_endline s;
	let s2 = ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz" in print_endline s2;
	let s3 = ft_rot_n 42 "0123456789" in print_endline s3;
	let s4 = ft_rot_n 2 "OI2EAS6789" in print_endline s4;
	let s5 = ft_rot_n 0 "Damned !" in print_endline s5;
	let s6 = ft_rot_n 42 "" in print_endline s6;
	let s7 = ft_rot_n 1 "NBzlk qnbjr !" in print_endline s7

let () = main ()
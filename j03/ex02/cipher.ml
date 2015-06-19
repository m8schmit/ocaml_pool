let rot_one c =
	if (c < 'a' && c < 'A') || (c > 'Z' && c > 'z') then
		c
	else if c = 'z' || c = 'Z' then
		char_of_int ((int_of_char c) - 25)
	else
		char_of_int ((int_of_char c) + 1)

let  rot42 s =
	let rec loop n s =
		if n > 0 then
			let s2 = String.map rot_one s in loop (n - 1) s2
		else
			s
	in
		loop 42 s

(***)		

let unrot_one c =
	if (c < 'a' && c < 'A') || (c > 'Z' && c > 'z') then
		c
	else if c = 'a' || c = 'A' then
		char_of_int ((int_of_char c) + 25)
	else
		char_of_int ((int_of_char c) - 1)

let unrot42 s =
	let rec loop n s =
		if n > 0 then
			let s2 = String.map unrot_one s in loop (n - 1) s2
		else
			s
	in
		loop 42 s
(***)
let rec caesar n s =
	if n > 0 then
		let s2 = String.map rot_one s in caesar (n - 1) s2
	else
		s
(***)
let rec uncaesar n s =
	if n > 0 then
		let s2 = String.map unrot_one s in uncaesar (n - 1) s2
	else
		s
(***)

	let ft_string_all func str key =
	let len = ((String.length str) - 1) in
		let rec loop r current_len =
			if current_len >= 0 then
			begin
                loop (r ^ func (String.get str current_len) key) (current_len - 1)
			end
			else
				r
		in
		loop "" len

let xor_ing c key =
    Char.escaped (char_of_int((int_of_char c) lxor key))

let xor s key =
	if key > 0 then
		ft_string_all xor_ing s key
	else
		s


let main () =
	let s = rot42 "abcdefghijklmnopqrstuvwxyz" in print_endline s;
	let s2 = rot42 "abcdefghijklmnopqrstuvwxyz" in print_endline s2;
	let s3 = rot42 "0123456789" in print_endline s3;
	let s4 = rot42 "OI2EAS6789" in print_endline s4;
	let s5 = rot42 "Damned !" in print_endline s5;
	let s6 = rot42 "" in print_endline s6;
	let s7 = rot42 "NBzlk qnbjr !" in print_endline s7;

	let s8 = unrot42 s in print_endline s8;
	let s9 = unrot42 s2 in print_endline s9;
	let sa = unrot42 s3 in print_endline sa;
	let sb = unrot42 s4 in print_endline sb;
	let sc = unrot42 s5 in print_endline sc;
	let sd = unrot42 s6 in print_endline sd;
    let se = unrot42 s7 in print_endline se;

    let x = xor "alex" 2 in print_endline x


let () = main ()

(* UNROT42 *)

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

(* UNCAESAR *)

let rec uncaesar n s =
	if n > 0 then
		let s2 = String.map unrot_one s in uncaesar (n - 1) s2
	else
		s

(* FT_UNCRYPT *)

let ft_uncrypt str f_lst = 
	Cipher.ft_crypt str f_lst

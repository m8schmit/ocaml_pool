(* ROT42 *)

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

(* CAESAR *)

let rec caesar n s =
	if n > 0 then
		let s2 = String.map rot_one s in caesar (n - 1) s2
	else
		s

(* XOR *)

let xor_ing key c =
    char_of_int((int_of_char c) lxor key)

let xor key s =
    if key > 0 then
        String.map (xor_ing (key mod 255)) s
    else
        s

(* FT_CRYPT *)

let rec ft_crypt str f_lst =  match f_lst with
	| head::tail -> ft_crypt (head str) tail
	| [] -> str
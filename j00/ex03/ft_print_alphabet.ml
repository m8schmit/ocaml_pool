

let ft_print_alphabet () =
	let ascii_of_a = 97 in
	let ascii_of_z = 122 in
	let rec loop current_digit = 
		if current_digit <= ascii_of_z	then
				let current_char = char_of_int current_digit in
				print_char current_char;
				loop (current_digit + 1)
	in
	loop ascii_of_a;
	print_char '\n'

let main () =
	ft_print_alphabet ()

let () = main ()
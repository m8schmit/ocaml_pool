let ft_is_palindrome str =
	let len = ((String.length str)) in
		let rec loop str current_len len i =
			if current_len > 0 && i < len then
				begin
					if String.get str current_len == String.get str i then
						loop str (current_len - 1) len (i + 1)
					else
						false
				end
			else
				true
		in
			loop str (len - 1) len 0

let main () =
	print_string "Test with [abba]: ";
	print_endline (string_of_bool(ft_is_palindrome "abba"));
		print_string "Test with [madam]: ";
	print_endline (string_of_bool(ft_is_palindrome "madam"));
		print_string "Test with [car]: ";
	print_endline (string_of_bool(ft_is_palindrome "car"));
		print_string "Test with [radar]: ";
	print_endline (string_of_bool(ft_is_palindrome "pouette"));
		print_string "Test with []: ";
	print_endline (string_of_bool(ft_is_palindrome ""))

let () = main ()
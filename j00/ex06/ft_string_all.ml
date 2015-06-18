let is_digit c = c >= '0' && c <= '9'

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let ft_string_all func str =
	let len = ((String.length str) - 1) in
		let rec loop func current_len =
			if current_len > 0 then
			begin
				func (String.get str current_len);
				loop func (current_len - 1)
			end
			else
				func (String.get str current_len)
		in
		loop func len

let main () =
	print_string "Test with [is_digit 0123456789]: ";
	print_endline (string_of_bool(ft_string_all is_digit "0123456789"));
	print_string "Test with [is_digit abcdefghi]: ";
	print_endline (string_of_bool(ft_string_all is_digit "abcdefghi"));
	print_string "Test with [is_alpha 0123456789]: ";
	print_endline (string_of_bool(ft_string_all is_alpha "0123456789"));
	print_string "Test with [is_alpha abcdefghi]: ";
	print_endline (string_of_bool(ft_string_all is_alpha "abcdefghi"))

let () = main ()
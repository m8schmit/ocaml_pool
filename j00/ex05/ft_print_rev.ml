
let ft_print_rev str =
	let len = ((String.length str) - 1) in
		let rec loop current_len =
			if current_len >= 0 then
				begin
					print_char(String.get str current_len);
					loop (current_len - 1);
				end
		in
			loop len;
			print_char '\n'

let main () =
	ft_print_rev "Hello world !";
	ft_print_rev "abcdefghijklmnopqrstuvwxyz";
	ft_print_rev "123456789"


let () = main ()
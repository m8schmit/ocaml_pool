let ft_print_comb () =
	let rec loop a b c =
		print_int a;
		print_int b;
		print_int c;
		if a < 7 then
		begin
			print_string ", ";
			if c < 9 then
				loop a b (c + 1)
			else if b < 8 then
				loop a (b + 1) (b + 2)
			else if a < 7 then
					loop (a + 1) (a + 2) (a + 3)
		end
		else
			print_string "\n"		
	in
	loop 0 1 2


let main () =
	ft_print_comb ()

let () = main ()
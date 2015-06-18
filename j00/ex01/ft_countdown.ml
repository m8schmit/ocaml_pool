let rec ft_countdown a = 
	if a > 0
		then
			begin
				print_int (a);
				print_char '\n';
				ft_countdown (a - 1)
			end
	else
		begin
			print_char '0';
			print_char '\n'
		end



let main () =
	ft_countdown (10);
	print_char '\n';
	ft_countdown (-10)


let () = main ()
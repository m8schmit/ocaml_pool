let rec ft_power a b =
	if b > 0
		then 
			a * ft_power(a) (b - 1)
	else
		1

let main () =
	print_int (ft_power 2 4);
	print_char '\n';
	print_int (ft_power 3 0);
	print_char '\n';
	print_int (ft_power 0 5);
	print_char '\n'

let () = main ()
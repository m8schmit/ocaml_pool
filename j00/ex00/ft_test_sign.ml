
let ft_test_sign a =
	if a < 0
		then print_endline "negative"	
	else print_endline "positive"

let main () = 
	ft_test_sign (10);
	ft_test_sign (0);
	ft_test_sign (42);
	ft_test_sign (-42);
	ft_test_sign (1000000);
	ft_test_sign (-1000000)

let () = main ()
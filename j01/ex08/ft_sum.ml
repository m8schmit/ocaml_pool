let ft_sum f x n =
	let rec addSum f x acc =
		if x > n then
			acc
		else
			addSum f (x + 1) (acc +. (f x))
	in
		if x > n then
			nan
		else
			addSum f x 0.

let () = 
	print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
	print_char '\n'
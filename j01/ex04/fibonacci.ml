let fibonacci n =
	let rec f n acc acc2 =
		if n < 0 then
			-1
		else if n <= 0 then
			acc
		else if n = 1 then
			acc2
		else
			f (n - 1) acc2 (acc + acc2)
	in
		f n 0 1

let () =
	print_int (fibonacci (-42));
	print_char '\n';
	print_int (fibonacci 1);
	print_char '\n';
	print_int (fibonacci 3);
	print_char '\n';
	print_int (fibonacci 6);
	print_char '\n'



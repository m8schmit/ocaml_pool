let rec to_lst str = match (String.length str) with
	  | 0 -> []
	  | x -> ((int_of_char (str.[0])) - 48) :: to_lst (String.sub str 1 (x - 1))

let rec to_string lst = match lst with
	| [] -> ""
	| (x, y)::q -> (string_of_int x) ^ (string_of_int y) ^ (to_string q)

let rec check_nb lst nb acc = match lst with
	| [] -> acc
	|head::second::tail when head = second -> check_nb (second::tail) (nb + 1) acc
	|head::second::tail -> check_nb (second::tail) 0 (acc @ [((nb + 1), head)])
	|head::tail -> check_nb [] 0 (acc @ [((nb + 1), head)])
;;

let encode lst  =

	check_nb lst 0 []

let rec sequence n = match n with
	| y when y <= 0 -> ""
	| 1 -> "1"
	| _ -> to_string (encode (to_lst(sequence (n - 1))))

let () =
	print_endline (sequence 1);
	print_endline (sequence 2);
	print_endline (sequence 3);
	print_endline (sequence 4);
	print_endline (sequence 5);
	print_endline (sequence 6);
	print_endline (sequence 7)
type 'a ft_ref = {mutable content:'a}

let return arg = {content = arg} 

let incr x = return (x + 1)

let get (x:'a ft_ref) = x.content

let set (x:'a ft_ref ) arg = x.content <- arg

let bind (x:'a ft_ref ) (f:'a -> 'b ft_ref) = f x.content

let () =
	let ret = return 10 in
		print_string "Get : ";
		print_int (get ret);
		print_char '\n';
		print_string "Set (then get): ";
		set ret 20;
		print_int (get ret);
		print_char '\n';
		print_string "Bind (and useless fct): ";
		let  ret2 = bind ret incr in
			print_int (get ret2)


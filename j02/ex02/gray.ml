let rec print_lst lst = match lst with
	| [] -> ()
	| head::tail ->
		print_string head;
		print_string " ";
		print_lst tail
	
let prepend c s =
	let s' = Bytes.create (String.length s + 1) in
		s'.[0] <- c;
		String.blit s 0 s' 1 (String.length s);
		s'

let rec concat x y = match (x, y) with
	|([], []) -> []
	|([], _) -> y
	|(_,[]) -> x
	|(t::q, _) -> t :: (concat q y)

let rec list_map f = function
	[] -> []
	| h::t -> let r = f h in r :: list_map f t

let rec gray n =
	if n <= 1 then 
		["0"; "1"]
	else
		let g = gray (n - 1) in
			concat (list_map (prepend '0') g) (list_map (prepend '1') g)

let () =
	print_lst (gray 1);
	print_char '\n';
	print_lst (gray 2);	
	print_char '\n';
	print_lst (gray 3)
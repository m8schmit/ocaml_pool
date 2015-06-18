let rec encode_loop lst nb acc  = match lst with
	| [] -> acc
	| head::second::tail -> begin
								if head = second then
									encode_loop (second::tail) (nb + 1) acc
								else
									encode_loop (second::tail) 0 (acc @ [((nb + 1), head)])
							end
	| head::tail -> encode_loop [] 0 (acc @ [((nb + 1), head)])
;;

let rec encode lst =
	encode_loop lst 0 []
let main () =
	let s = Value.all in
		let rec loop s = match s with
			| h::t -> 	begin
							print_string "toInt (with print_int): ";
							print_int (Value.toInt h);
							print_char '\n';
								(**)
							print_string "toString: ";
							print_endline (Value.toString h);
								(**)
							print_string "toStringVerbose: ";
							print_endline (Value.toStringVerbose h);
								(**)
							print_string "next then toStringVerbose: ";
							print_string (Value.toStringVerbose (Value.next h));
							print_string "\n\n";
							
							loop t
						end
			| [] -> ()
		in loop s

let () = main ()
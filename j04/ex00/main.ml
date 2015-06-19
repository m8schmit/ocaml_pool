let main () =
	let s = Color.all in 
		let rec loop s = match s with
			| h::t -> 	begin
							print_endline (Color.toString h);
							print_endline (Color.toStringVerbose h);
							loop t
						end
			| [] -> ()
		in loop s

let () = main ()
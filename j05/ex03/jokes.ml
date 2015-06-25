let fill_tab file =
	let lst = ref [] in
		begin try
			let ic = open_in file in
				while true do
					let s = input_line ic in
						lst := !lst @ [s]
				done
		with
		| Sys_error err -> Printf.printf "Something went wrong: %s\n" err
		| End_of_file -> Printf.printf ""
		end;
		!lst

let () = 
			Random.self_init ();
			if Array.length Sys.argv <> 2 then
				print_endline "Something went wrong...\n"
			else
				let tab = Array.of_list (fill_tab Sys.argv.(1)) in
					let rand = Random.int  (Array.length tab) in
						print_endline  (tab.(rand))
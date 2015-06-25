let my_sleep () = Unix.sleep 1

let micronap sec =
	for i = 0 to sec do
		my_sleep ();
	done

let () = 
	if Array.length Sys.argv <> 1 then
		begin
			let time =
				try int_of_string Sys.argv.(1)
					with _ -> 0
				in
				micronap time
		end

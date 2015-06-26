module Watchtower =
	struct
		type hour = int
		let zero = ((12):hour)
		let sub (x:hour) (y:hour) = ((if (x - y) mod 12 = 0 then
														12 
													else if (x - y) mod 12 < 0 then
														((x - y) mod 12) * -1
													else
														(x - y) mod 12):hour)
		let add (x:hour) (y:hour) = ((if (x + y) mod 12 = 0 then 12 else (x + y) mod 12):hour)
	end

let () = 
	let h1:Watchtower.hour = 12 in 
	let h2:Watchtower.hour = 12 in
		print_endline "12h - 12h, 12h + 12h";
		print_endline (string_of_int (Watchtower.sub h1 h2));
		print_endline (string_of_int (Watchtower.add h1 h2));
	let h1:Watchtower.hour = 12 in 
	let h2:Watchtower.hour = 40 in
		print_endline "12h - 40h, 12h + 40h";
		print_endline (string_of_int (Watchtower.sub h1 h2));
		print_endline (string_of_int (Watchtower.add h1 h2));
	let h1:Watchtower.hour = 12 in 
	let h2:Watchtower.hour = 8 in
		print_endline "12h - 8h, 12h + 8h";
		print_endline (string_of_int (Watchtower.sub h1 h2));
		print_endline (string_of_int (Watchtower.add h1 h2));
	let h1:Watchtower.hour = 12 in 
	let h2:Watchtower.hour = 6 in
		print_endline "12h - 6h, 12h + 6h";
		print_endline (string_of_int (Watchtower.sub h1 h2));
		print_endline (string_of_int (Watchtower.add h1 h2))

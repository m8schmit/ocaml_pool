let sum (a:float) (b:float) = 
	a +. b


let () = 

	Printf.printf "20.0 + 21.89 = %f\n" (sum  20.2 21.8);
	Printf.printf "-456.789 + 456.789 = %f\n" (sum  (-456.789) 456.789)
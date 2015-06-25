let eu_dist a b =
	let len = Array.length a in
		let rec loop a b len acc n = 
			if len > 0 then
				loop a b (len - 1) (acc +. ((a.(n) -. b.(n)))**2.) (n + 1)
			else
				sqrt(acc)
	in
		loop a b len 0. 0


let () =
	Printf.printf "> %f\n" (eu_dist [|1.; 2.; 3.; 4.12|] [|7.; 6.;65.; (-7.)|])
module App =
	struct
		type project = string * string * int
		let zero = ("", "", 0)
		let combine (x:project) (y:project) = 	let getfst (a,_,_) = a in
												let getlst (_,_,c) = c in
													if ((((getlst x) + (getlst y)) / 2) > 80) then
														((getfst x) ^ (getfst y), "succeed", ((getlst x) + (getlst y)) / 2)
													else
														((getfst x) ^ (getfst y), "fail", ((getlst x) + (getlst y)) / 2)
		let fail (x:project) = let getfst (a,_,_) = a in
										(getfst x, "fail", 0)

		let success (x:project) = let getfst (a,_,_) = a in
										(getfst x, "succeed", 80)

	end

let print_proj (x:App.project) = let getfst (a,_,_) = a in
								let getsnd (_,b,_) = b in
								let getlst (_,_,c) = c in
									print_endline ("( " ^ (getfst x) ^ ", " ^ (getsnd x) ^ ", " ^ (string_of_int (getlst x)) ^ " )")

	let () =
		 let p1:App.project = (App.zero) in 
		let p2:App.project = (("Rush 01", "succes", 80):App.project) in

		print_endline "Test App.fail avec App.zero : ";
		print_proj (App.fail p1);
		print_endline "Test App.success avec App.zero : ";
		print_proj (App.success p1);
		print_endline "Test App.combine avec App.zero et ((\"Rush 01\", \"succeed\", 80):App.project) : ";
		print_proj (App.combine p1 p2);
		print_endline "Test App.fail avec ((\"Rush 01\", \"succeed\", 80):App.project) : ";
		print_proj (App.fail p2);
		print_endline "Test App.success avec ((\"Rush 01\", \"succeed\", 80):App.project) : ";
		print_proj (App.success p2)
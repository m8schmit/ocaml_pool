class ['a] army =  
	object
		val mutable _y:'a list =  []

		method get_list = _y
		method add (x:'a) = _y <- [x] @ _y
		method delete =	let rec remove lst = 
							match lst with
								| h::t ->  t
   								| []   -> []
							in
							 _y <- remove _y;
							 print_endline "> remove one"
	end
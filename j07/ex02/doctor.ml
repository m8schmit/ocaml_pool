class doctor (name:string) (age:int) (sidekick:People.people) =
	object (self)
		initializer print_endline "Create New instance of \"doctor\""; self#regenerate

		val _name:string = name
		val _age:int = age
		val _sidekick =  sidekick
		val mutable _hp = 0

		method to_string = _name ^ ": " ^ (string_of_int _hp) ^ "hp. " ^ (string_of_int _age) ^ " years old, is sidekick is " ^ _sidekick#to_string
		method talk = print_endline ("Hi! I'm the Doctor!")
		method travel_in_time (start:int) (arrival:int) =
			print_endline
 "						  		                 __/\\__                
								                |-+--+-|                
								                |-+--+-|                
								                |_|__|_|                
								       =========================        
								       =========================        
								    ===============================     
								+-------------------------------------+ 
								| +---------------------------------+ | 
								| | POLICE     PUBLIC CALL      BOX | | 
								| +---------------------------------+ | 
								+-------------------------------------+ 
								  | | +-----------+ +-----------+ | |   
								  | | | +-------+ | | +-------+ | | |   
								  | | | |  | |  | | | |  | |  | | | |   
								  | | | |--+-+--| | | |--+-+--| | | |   
								  | | | |--+-+--| | | |--+-+--| | | |   
								  | | | |  | |  | | | |  | |  | | | |   
								  | | | +-------+ | | +-------+ | | |   
								  | | |           | |           | | |   
								  | | | +-------+ | | +-------+ | | |   
								  | | | | ~~~~~ | | | |       | | | |   
								  | | | | ~~~~~ | | | |       | | | |   
								  | | | | ~~~~~ | | | |       | | | |   
								  | | | | ~~~~~ | | | |       | | | |   
								  | | | +-------+ | | +-------+ | | |   
								  | | |           | |           | | |   
								  | | | +-------+ | | +-------+ | | |   
								  | | | |       | | | |       | | | |   
								  | | | |       | | | |       | | | |   
								  | | | |       | | | |       | | | |   
								  | | | |       | | | |       | | | |   
								  | | | +-------+ | | +-------+ | | |   
								  | | |           | |           | | |   
								  | | | +-------+ | | +-------+ | | |   
								  | | | |       | | | |       | | | |   
								  | | | |       | | | |       | | | |   
								  | | | |       | | | |       | | | |   
								  | | | |       | | | |       | | | |   
								  | | | +-------+ | | +-------+ | | |   
								  | | +-----------+ +-----------+ | |   
								======================================= 
								======================================= ";
			if start > arrival then					
				print_endline ("Doctor travel " ^ (string_of_int (start - arrival)) ^ " years in past.")
			else
				print_endline ("Doctor travel " ^ (string_of_int (arrival - start)) ^ " years in future.");
			print_endline "... But the travel was instant, his age haven't change !"
		method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii !"
		method private regenerate = _hp <- 100

	end
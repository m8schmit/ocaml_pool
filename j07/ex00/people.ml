class people name =
	object
		initializer print_endline "Create New instance of \"people\" !"

		val _name:string = name
		val mutable _hp = 100

		method to_string = _name ^ ": " ^ (string_of_int _hp) ^ "hp. " 
		method talk = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")
		method die = print_endline "Aaaarghh!"
	end
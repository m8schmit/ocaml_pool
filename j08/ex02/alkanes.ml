class virtual atom =
	object (this)
		method virtual name : string
		method virtual symbol : string
		method virtual atomic_number : int

		method equals (that: atom) = ((this#name = that#name)
											&& (this#symbol = that#symbol)
											&& (this#atomic_number = that#atomic_number))

				method to_string = "atom :" ^ this#name ^ " [" ^ this#symbol ^ "] [" ^ string_of_int(this#atomic_number) ^ "]"
	end

class virtual molecule (name:string) (lst:atom list) =
object (this)

	method genFormula lst =
		let rec loop lst acc = match lst with
			| [] -> acc
			| h::t -> loop t (acc @ [h#symbol])
			in let rec check_nb lst nb acc = match lst with
			| [] -> acc
			| head::second::tail -> 
				begin
					if head = second then
						check_nb (second::tail) (nb + 1) acc
					else
						check_nb (second::tail) 0 (acc @ [((nb + 1), head)])
				end
			| head::tail ->	check_nb [] 0 (acc @ [((nb + 1), head)])
			in let tub_lst = (check_nb (loop lst []) 0 [])
				in let rec getString x acc = match x with
					| [] -> acc
					| (a,b)::t -> (getString t  (acc ^ b ^ (string_of_int a)))
					in getString tub_lst ""

	method name = name
	method formula = (this#genFormula lst )


	method equals (that:molecule) = ((this#name = this#name)
										&& (this#formula = this#formula))

	method to_string = "Molecule " ^ this#name ^ " is composed of [" ^ this#formula ^ "]" 
end

class hydrogen =
object (this)
	inherit atom
	method name = "Hydrogen"
	method symbol = "H"
	method atomic_number = 1

end

class carbon =
object (this)
	inherit atom
	method name = "Carbon"
	method symbol = "C"
	method atomic_number = 6

end

let get_atom_list (n:int) (a:atom) =
    let rec loop n a list =
        if (n = 0) then
            list
        else
            loop (n - 1) (a) (a::list)
    in
    loop n a []

let get_atoms_list n =
    ((get_atom_list n (new carbon))@(get_atom_list (2 * n + 2) (new
    hydrogen)))

class virtual alkanes (n) =
	object (this)
		inherit molecule (
							let rec ret_name n = match n with
								| 2 -> "Ethane" 
								| 3 -> "Propane" 
								| 4 -> "Butane" 
								| 5 -> "Pentane" 
								| 6 -> "Hexane" 
								| 7 -> "Heptane" 
								| 8 -> "Octane" 
								| 9 -> "Nonane"
								| 10 -> "Decane" 
								| 11 -> "Undecane" 
								| 12 -> "Dodecane" 
								| 16 -> "Hexadecane" 
								| 20 -> "Icosane" 
								| 30 -> "Triacontane" 
								| 40 -> "Tetracontane" 
								| 50 -> "Pentacontane" 
								| 60 -> "Hexacontane"
								| _ -> "Methane"
							in ret_name n
						)(get_atoms_list n)
end



class methane =
	object (this)
		inherit alkanes 1
	end

class ethane =
	object (this)
		inherit alkanes 2
	end

class octane =
	object (this)
		inherit alkanes 8
	end

class pentane =
	object (this)
		inherit alkanes 5
	end

class hexacontane =
	object (this)
		inherit alkanes 60
	end

class icosane =
	object (this)
		inherit alkanes 20
	end

class undecane =
	object (this)
		inherit alkanes 11
	end
let () =
	let m = new methane in
		print_endline m#to_string;
	let e = new ethane in
		print_endline e#to_string;
	let o = new octane in
		print_endline o#to_string;
	let p = new pentane in
		print_endline p#to_string;
	let h = new hexacontane in
		print_endline h#to_string;
	let i = new icosane in
		print_endline i#to_string;
	let u = new undecane in
		print_endline u#to_string
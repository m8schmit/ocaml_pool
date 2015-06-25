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

(****)

class nitrogen =
object (this)
	inherit atom
	method name = "Nitrogen"
	method symbol = "N"
	method atomic_number = 7
end

class carbon =
object (this)
	inherit atom
	method name = "Carbon"
	method symbol = "C"
	method atomic_number = 6

end

class hydrogen =
object (this)
	inherit atom
	method name = "Hydrogen"
	method symbol = "H"
	method atomic_number = 1

end

class oxygen = 
object (this)
	inherit atom
	method name = "Oxygen"
	method symbol = "O"
	method atomic_number = 8

end

class nitric_oxide =
object (this)
	inherit atom
	method name = "Nitric oxide"
	method symbol =  "NO"
	method atomic_number = 102

end
class water =
	object (this)
		inherit molecule "Water" [new hydrogen;new hydrogen;new oxygen]
	end

class carbon_dioxyde =
	object (this)
		inherit molecule "Carbon Dioxyde" [new carbon;new oxygen; new oxygen]
	end

class tnt =
	object (this)
		inherit molecule "Trinitrotoluene" [new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new nitrogen;new nitrogen;new nitrogen;new oxygen;new oxygen;new oxygen;new oxygen;new oxygen;new oxygen;]
	end

class butan =
	object (this)
		inherit molecule "Butan" [new carbon;new carbon;new carbon;new carbon;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;]
	end

class acetaminophen =
	object (this)
		inherit molecule "Acetaminophen" [new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new nitric_oxide;new nitric_oxide]
	end
class benzylparaben =
	object (this)
		inherit molecule "Benzylparaben" [new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new oxygen;new oxygen;new oxygen;]
	end
class trans_cinnamic_acid =
	object (this)
		inherit molecule "Trans-Cinnamic Acid" [new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new carbon;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new hydrogen;new oxygen;new oxygen]
	end

let () =
	let w = new water in
		print_endline w#to_string;
	let co = new carbon_dioxyde in
		print_endline co#to_string;
	let tnt = new tnt in
		print_endline tnt#to_string;
	let but = new butan in
		print_endline but#to_string;
	let no = new acetaminophen in
		print_endline no#to_string;
	let ben = new benzylparaben in
		print_endline ben#to_string;
	let tca = new trans_cinnamic_acid in
		print_endline tca#to_string;
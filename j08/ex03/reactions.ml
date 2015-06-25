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


class virtual reaction (x: molecule list) (y: molecule list) =
	object (this)
		method  virtual get_start: (molecule * int) list
		method  virtual get_result: (molecule * int) list
		method  virtual balance: reaction
		method virtual is_balanced: bool

	end
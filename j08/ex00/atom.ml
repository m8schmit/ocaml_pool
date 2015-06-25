class virtual atom =
	object (this)
		method virtual name : string
		method virtual symbol : string
		method virtual atomic_number : int

		method equals (that: atom) = ((this#name = that#name)
											&& (this#symbol = that#symbol)
											&& (this#atomic_number = that#atomic_number))

		method to_string = "atom :" ^ this#name ^" ["^ this#symbol ^"] ["^ string_of_int(this#atomic_number) ^ "]\n"
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

class oxygen = 
object (this)
	inherit atom
	method name = "Oxygen"
	method symbol = "O"
	method atomic_number = 8

end

class helium = 
object (this)
	inherit atom
	method name = "Helium"
	method symbol = "He"
	method atomic_number = 2

end

class silicium = 
object (this)
	inherit atom
	method name = "Silicium"
	method symbol = "Si"
	method atomic_number = 14

end

class phosphore = 
object (this)
	inherit atom
	method name = "Phosphore"
	method symbol = "P"
	method atomic_number =  15

end

class calcium = 
object (this)
	inherit atom
	method name = "Calcium"
	method symbol = "Ca"
	method atomic_number = 20

end

let () =
	let h = new hydrogen in
	let c = new carbon in
	let o = new oxygen in
	let he = new helium in
	let s = new silicium in
	let p = new phosphore in
	let ca = new calcium in
	print_endline h#to_string;
	print_endline c#to_string;
	print_endline o#to_string;
	print_endline he#to_string;
	print_endline s#to_string;
	print_endline p#to_string;
	print_endline ca#to_string;
	Printf.printf "\nIs %s equal to %s ?\n> " h#name ca#name;
	print_endline (string_of_bool(h#equals ca));
	Printf.printf "Whouuuu...\nAnd %s, equal to himself ?\n> " o#name;
	print_endline (string_of_bool(o#equals o))

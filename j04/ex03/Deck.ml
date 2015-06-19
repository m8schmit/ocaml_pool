module Color =
struct
	type t = Spade | Heart | Diamond | Club

	let all = [Spade; Heart; Diamond; Club]

	let toString t = match t with
		| Spade -> "S"
		| Heart -> "H"
		| Diamond -> "D"
		| Club -> "C"

	let toStringVerbose t = match t with
		| Spade -> "Spade"
		| Heart -> "Heart"
		| Diamond -> "Diamond"
		| Club -> "Club"
end

module Value =
struct
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

	let toInt t = match t with
		| T2 -> 1
		| T3 -> 2
		| T4 -> 3
		| T5 -> 4
		| T6 -> 5
		| T7 -> 6
		| T8 -> 7
		| T9 -> 8
		| T10 -> 9
		| Jack -> 10
		| Queen -> 11
		| King -> 12
		| As -> 13

	let toString t = match t with
		| T2 -> "1"
		| T3 -> "2"
		| T4 -> "3"
		| T5 -> "4"
		| T6 -> "5"
		| T7 -> "6"
		| T8 -> "7"
		| T9 -> "8"
		| T10 -> "9"
		| Jack -> "J"
		| Queen -> "Q"
		| King -> "K"
		| As -> "A"

	let toStringVerbose t = match t with
		| T2 -> "1"
		| T3 -> "2"
		| T4 -> "3"
		| T5 -> "4"
		| T6 -> "5"
		| T7 -> "6"
		| T8 -> "7"
		| T9 -> "8"
		| T10 -> "9"
		| Jack -> "Jack"
		| Queen -> "Queen"
		| King -> "King"
		| As -> "As"

	let next t = match t with
		| T2 -> T3
		| T3 -> T4
		| T4 -> T5
		| T5 -> T6
		| T6 -> T7
		| T7 -> T8
		| T8 -> T9
		| T9 -> T10
		| T10 -> Jack
		| Jack -> Queen
		| Queen -> King
		| King -> As
		| As -> invalid_arg "As is the most powerful card."

	let previous t = match t with
		| T2 -> invalid_arg "2 is the less powerful card."
		| T3 -> T2
		| T4 -> T3
		| T5 -> T4
		| T6 -> T5
		| T7 -> T6
		| T8 -> T7
		| T9 -> T8
		| T10 -> T9
		| Jack -> T10
		| Queen -> Jack
		| King -> Queen
		| As -> King
end

module Card =
struct
	type t = {
				color : Color.t;
				value : Value.t
	}

	let newCard v c = {	
		value = v;
		color = c
	}

	let allSpades = 
		let v = Value.all in 
			let rec gen_rec va = match va with
			| [] -> []
			| (head:Value.t)::tail -> ({ color = Color.Spade; value = head}) :: (gen_rec tail)
		in gen_rec v

	let allHearts = 
		let v = Value.all in 
			let rec gen_rec va = match va with
			| [] -> []
			| (head:Value.t)::tail -> ({ color = Color.Heart; value = head}) :: (gen_rec tail)
		in gen_rec v

	let allDiamonds = 
		let v = Value.all in 
			let rec gen_rec va = match va with
			| [] -> []
			| (head:Value.t)::tail -> ({ color = Color.Diamond; value = head}) :: (gen_rec tail)
		in gen_rec v

	let allClubs = 
		let v = Value.all in 
			let rec gen_rec va = match va with
			| [] -> []
			| (head:Value.t)::tail -> ({ color = Color.Club; value = head}) :: (gen_rec tail)
		in gen_rec v

	let all =
			allSpades@allHearts@allDiamonds@allDiamonds

	let getValue self =
		self.value

	let getColor self =
		self.color

	let toString self =
		((Value.toString self.value)^(Color.toString self.color))

	let toStringVerbose self =
		((Value.toStringVerbose self.value)^(Color.toStringVerbose self.color))

	let compare t1 t2 =
		((Value.toInt t1.value) - (Value.toInt t2.value))

	let max t1 t2 = match t1 with
		| t when (compare t t2) < 0 -> t2
		| _ -> t1

	let min t1 t2 = match t1 with
		| t when (compare t t2) > 0 -> t2
		| _ -> t1

	let best lst:t = match lst with
		| [] -> invalid_arg "List is empty" 
		| h::t ->	List.fold_left max h t

	let isOf (t:t) (color:Color.t) =
			getColor t = color

	let isSpade (t:t) =
		getColor t = Color.Spade

	let isHeart (t:t) =
		getColor t = Color.Heart

	let isDiamond (t:t) =
		getColor t = Color.Diamond

	let isClub (t:t) =
		getColor t = Color.Club
end

type t = Card.t list

let randomize (res:t) (card:Card.t) = 
	let rand = Random.int 2 in
		match res with
			| [] -> [card]
			| h::t when rand = 0 -> h::card::t
			| h::t when rand = 1 -> card::h::t
			| h::t -> (card::t@[h]) 

let newDeck () = 
	let base = Card.all in
		List.fold_left randomize [] base 

let drawCard self = match self with
	| [] -> invalid_arg "Failure"
	|  h::t -> (h, t)
	
let rec toStringList self = match self with
	| []  -> []
	| h::t -> (Card.toString h)::toStringList t

let rec toStringListVerbose self = match self with
	| []  -> []
	| h::t -> (Card.toStringVerbose h)::toStringListVerbose t


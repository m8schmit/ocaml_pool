let rec is_in_list x lst = match lst with
	| [] -> false
	| head::tail -> (x = head) || (is_in_list x tail)

let rec crossover lst1 lst2 = match lst1 with
	| [] -> []
	| head::tail when (is_in_list head lst2) -> head :: crossover tail lst2
	| head::tail ->	crossover tail lst2


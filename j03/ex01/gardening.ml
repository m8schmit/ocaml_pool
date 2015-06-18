type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

(* Node (42, Nil, Nil) *)

let rec size = function
    | Nil                       ->  0
    | Node (_, left, right)     ->  1 + (size left) + (size right)

let rec height = function
    | Nil                       ->  0
    | Node (_, left, right)     ->  1 + (max (size left) (size right))

let ft_draw_square x y size =
	Graphics.moveto x y;
	Graphics.lineto x (y + size);
	Graphics.lineto (x + size) (y + size);
	Graphics.lineto (x + size) y;
	Graphics.lineto x y
	
let rec draw_tree  node x y = match node with
	| Node (v, l, r) -> begin
							ft_draw_square x y 100;
							Graphics.moveto (x+40) (y +40);
							Graphics.draw_string v;
							Graphics.moveto  (x + 100) y;
							Graphics.lineto (x + 170) (y - 50);
							Graphics.moveto  (x + 100) (y + 100);
							Graphics.lineto (x + 170) (y + 150);
							draw_tree l (x + 170) (y + 100);
							draw_tree r (x + 170) (y - 100)

						end
	| Nil ->	begin
					ft_draw_square x y 100;
					Graphics.moveto (x + 40) (y + 40);
					Graphics.draw_string "Nil"
		
				end 

let main () =
	Graphics.open_graph " 800x600";
	Graphics.moveto 400 300;
	print_int (size (Node ("420", Node("00", Nil, Node("01", Nil, Nil)), Nil)));
	print_endline " ";
	print_int (height (Node ("420", Node("00", Nil, Node("01", Nil, Nil)), Nil)));
	print_endline " ";
	draw_tree (Node ("420", Node("00", Nil, Node("01", Nil, Nil)), Nil)) 50 200;
	Graphics.read_key ()


let _ =
		main ()
		
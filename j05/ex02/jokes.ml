let jokes () = 
	Random.self_init ();
	let tab = [|"Un petit nuage se promène avec sa maman dans le ciel.\nTout à coup, il s'arrête en se tortillant :\n-J'ai envie de faire pluie-pluie, maman !";
				"Que dit un chien quand il cherche quelque chose et qu'il ne trouve pas ?\nJe suis tombé sur un os.";
				"Quel sont les lettres qui se voit le moin ?\nFAC (effacer)";
				"Quelle sont les lettres qui bougent tout le temps ?\nAJT (agiter)";
				"pull my finger"|] in
		let rand = Random.int 5 in
			print_endline (tab.(rand))

let () = jokes ()
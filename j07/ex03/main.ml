			let rec show_list lst = 
				match lst with
				| h::t -> print_endline h#to_string ; show_list t
				| [] -> ()

let () = 
let faceOfBoe = new People.people "Face of Boe" in
	let cyberman = new People.people "Cyberman" in
		let dannyPink = new People.people "Danny Pink" in
			print_endline "";
			let crew = new Army.army in
				crew#add (new People.people "Rose");
				crew#add (new People.people "Amy");
				crew#add (new People.people "Donna");
				show_list crew#get_list;
				crew#delete;
				show_list crew#get_list;
				print_endline "";
			let crew2 = new Army.army in
				crew2#add (new Dalek.dalek);
				crew2#add (new Dalek.dalek);
				show_list crew2#get_list;
				crew2#delete;
				show_list crew2#get_list;
				print_endline "";
			let crew3 = new Army.army in
				crew3#add (new Doctor.doctor "who" 900 faceOfBoe);
				crew3#add (new Doctor.doctor "The Master" 800 cyberman);
				crew3#add (new Doctor.doctor "Missy" 850 dannyPink);
				show_list crew3#get_list;
				crew3#delete;
				show_list crew3#get_list
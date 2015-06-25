let () = 
	let faceOfBoe = new People.people ("Face of Boe") in
		print_endline faceOfBoe#to_string;
		faceOfBoe#talk;
		faceOfBoe#die
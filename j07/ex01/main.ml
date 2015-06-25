let () = 
	let faceOfBoe = new People.people "Face of Boe" in
		print_endline faceOfBoe#to_string;
		faceOfBoe#talk;
		faceOfBoe#die;

	let doctor = new Doctor.doctor "Who" 900 faceOfBoe in
		print_endline doctor#to_string;
		doctor#talk;
		doctor#travel_in_time 2015 1515;
		doctor#use_sonic_screwdriver

let  () =

	print_endline "\nNROT42: ";
	let s = Cipher.rot42 "abcdefghijklmnopqrstuvwxyz" in print_endline s;
	let s3 = Cipher.rot42 "0123456789" in print_endline s3;
	let s4 = Cipher.rot42 "OI2EAS6789" in print_endline s4;
	let s5 = Cipher.rot42 "Damned !" in print_endline s5;
	print_endline "\nUNROT42: ";
	let s8 = Uncipher.unrot42 s in print_endline s8;
	let sa = Uncipher.unrot42 s3 in print_endline sa;
	let sb = Uncipher.unrot42 s4 in print_endline sb;
	let sc = Uncipher.unrot42 s5 in print_endline sc;
	print_endline "\nCAESAR AND UNCAESAR: ";
	let sf = Cipher.caesar 10 "Michel" in print_endline sf;
	let sf = Uncipher.uncaesar 10 "Wsmrov" in print_endline sf;
	print_endline "\nXOR: ";
	let x = Cipher.xor 2 "Ceci est un test" in print_endline x;
	print_endline "\nCRYPT AND UNCRYPT: ";
	let ret = Cipher.ft_crypt "blablabla" [Cipher.rot42; (Cipher.caesar 15)] in print_endline ret;
	let ret2  = Uncipher.ft_uncrypt ret [(Uncipher.uncaesar 15); Uncipher.unrot42] in print_endline ret2
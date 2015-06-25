module StringHash =
	struct
		type t = string
		let equal i j = i=j
		let hash s =
            let len = String.length s in
            let rec loop st i n =
                if i >= len then
                    n
                else
                    begin
                    	print_string "> ";
                    	print_int ((n lsl 6) + n + (int_of_char (st.[i])));
                        print_string "\n";
                        loop st (i + 1) ((n lsl 6) + n + (int_of_char (st.[i])))

                    end
            in
            loop s 0 5381
	end

module StringHashtbl =  Hashtbl.Make(StringHash)

let () =
let ht = StringHashtbl.create 5 in
let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
let pairs = List.map (fun s -> (s, String.length s)) values in
List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
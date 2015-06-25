class galifrey (dalek_army:Dalek.dalek list) (doctor_army:Doctor.doctor list) (people_army:People.people list) =
    object
        val _dalek_army:Dalek.dalek list =  dalek_army
        val mutable _doctor_army:Doctor.doctor list = doctor_army
        val _people_army:People.people list = people_army

        method do_time_war =
        let rec loop da p d0 = 

            if (List.length d0) > 0 then
            (
                (List.hd d0)#runaway;
                _doctor_army <- (List.tl d0)
            )
            else
                _doctor_army <- [];

            match p with
            | [] -> print_endline "genocide is over"
            | _ ->
                        match da with
                    | [] -> ()
                    | h::t -> h#exterminate (List.hd p); loop da (List.tl p) _doctor_army


        in
            loop _dalek_army _people_army _doctor_army


    end

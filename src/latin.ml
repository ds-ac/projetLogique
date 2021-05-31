let domain n =
	(* For each column i, row j and symbol k,
	 * we have one variable x_i,j,k indicating that symbol k is there. *)
	Dimacs.(make (n**n**n**o))

let problem n =
	let x = domain n in
	(* For each location, there is at least one correct colour, and for any
	colour, there is at least a lcoation in each row and column *)
	for i=0 to n-1 do
		for j=0 to n-1 do
			(* At least a colour *)
			Dimacs.(add_clause (bigor n (fun k -> x.(i).(j).(k))));
			(* At least a column / row *)
			Dimacs.(add_clause (bigor n (fun k -> x.(i).(k).(j))));
			Dimacs.(add_clause (bigor n (fun k -> x.(k).(i).(j))))
		done
	done;
	(* For each location, there is at most one correct colour. *)
	for i=0 to n-1 do
		for j=0 to n-1 do
			for k=0 to n-1 do
				for k'=k+1 to n-1 do
					(* At most one colour per location. *)
					Dimacs.(add_clause [ not x.(i).(j).(k); not x.(i).(j).(k') ]);
					(* At most one row per colour. *)
					Dimacs.(add_clause [ not x.(k).(j).(i); not x.(k').(j).(i) ]);
					(* At most one column per colour. *)
					Dimacs.(add_clause [ not x.(i).(k).(j); not x.(i).(k').(j) ])
				done;
			done
		done
	done

let solution n =
	let x = domain n in
	let m = Dimacs.read_model (open_in "output.sat") in
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			for k = 0 to n-1 do
				if Dimacs.sat m x.(i).(j).(k) then
					Format.printf "%c"
						(char_of_int
							 (k + int_of_char 'A'))
			done
		done ;
		Format.printf "\n"
	done

let () = Dimacs.run_int ~problem ~solution

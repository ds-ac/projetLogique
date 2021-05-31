let domain n =
	(* For each column i, row j and symbol k,
	 * we have one variable x_i,j,k indicating that symbol k is there. *)
	Dimacs.(make (n**n**2**n**o))

let problem n =
	let x = domain n in
	(* Each dimension of the greek square problem is a latin square *)
	(* For each location, there is at least one correct colour, and for any
	colour, there is at least a lcoation in each row and column *)
	for i=0 to n-1 do
		for j=0 to n-1 do
			(* At least a colour *)
			Dimacs.(add_clause (bigor n (fun k -> x.(i).(j).(0).(k))));
			Dimacs.(add_clause (bigor n (fun k -> x.(i).(j).(1).(k))));
			(* At least a column / row *)
			Dimacs.(add_clause (bigor n (fun k -> x.(i).(k).(0).(j))));
			Dimacs.(add_clause (bigor n (fun k -> x.(i).(k).(1).(j))));
			Dimacs.(add_clause (bigor n (fun k -> x.(k).(i).(0).(j))));
			Dimacs.(add_clause (bigor n (fun k -> x.(k).(i).(1).(j))))
		done
	done;
	(* For each location, there is at most one correct colour. *)
	for i=0 to n-1 do
		for j=0 to n-1 do
			for k=0 to n-1 do
				for k'=k+1 to n-1 do
					(* At most one colour per location. *)
					Dimacs.(add_clause [ not x.(i).(j).(0).(k); not x.(i).(j).(0).(k') ]);
					Dimacs.(add_clause [ not x.(i).(j).(1).(k); not x.(i).(j).(1).(k') ]);
					(* At most one row per colour. *)
					Dimacs.(add_clause [ not x.(k).(j).(0).(i); not x.(k').(j).(0).(i) ]);
					Dimacs.(add_clause [ not x.(k).(j).(1).(i); not x.(k').(j).(1).(i) ]);
					(* At most one column per colour. *)
					Dimacs.(add_clause [ not x.(i).(k).(0).(j); not x.(i).(k').(0).(j) ]);
					Dimacs.(add_clause [ not x.(i).(k).(1).(j); not x.(i).(k').(1).(j) ])
				done;
			done
		done
	done;
	(* A configuration cannot be found twice in the grid *)
	for i=0 to n-1 do
		for j=0 to n-1 do
			for i'=0 to n-1 do
				for j'=0 to n-1 do
					if (i, j) <> (i', j') then
						(
						for k=0 to n-1 do
							Dimacs.(add_clause [
								not x.(i).(j).(0).(k) ; not x.(i').(j').(0).(k);
								not x.(i).(j).(1).(k) ; not x.(i').(j').(1).(k)
								])
						done
						)
				done
			done
		done
	done

let solution n =
	let x = domain n in
	let m = Dimacs.read_model (open_in "output.sat") in
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			for k = 0 to n-1 do
				for k' = 0 to n-1 do
					if Dimacs.sat m x.(i).(j).(0).(k) && Dimacs.sat m x.(i).(j).(1).(k') then
					(
						(
							if j = 0 then Format.printf "| "
						);
						Format.printf "%c%c | "
							(char_of_int
								 (k + int_of_char 'A'))
							(char_of_int
								 (k' + int_of_char 'A'))
					)
				done
			done
		done ;
		Format.printf "\n"
	done

let () = Dimacs.run_int ~problem ~solution

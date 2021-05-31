(** Variables (i.e. propositional constants) are represented by positive
	* integers. Literals are arbitrary integers: for any variable X coded
	* as a positive integer [n], the positive literal X is represented by
	* [n] and not(X) is represented by [-n].
	*
	* A partial assignment [m] is an association list from variables
	* (i.e. positive literals) to booleans. *)

exception Conflict
exception SAT of bool array * bool array * int list
exception Found of int
exception Break

(** [sat m l] indicates whether a literal is satisfied by a partial
	* assignment. Satisfaction is always false for unassigned literals. *)
let sat (assignments, values, _) l =
	let idx = if l > 0 then l else -l in
	assignments.(idx) && values.(idx)

(** Return new partial assignment where a previously unassigned literal
	* is set to true. *)
let add (assignments, values, stack) l =
	Format.printf "Adding %d\n" l;
	let value, idx = if l > 0 then (true, l) else (false, -l) in
	assignments.(idx) <- true; values.(idx) <- value;
	(assignments, values, idx :: stack)

(** Indicates whether a litteral is assigned in a partial assignment. *)
let assigned (assignments, _, _) l =
	assignments.(if l > 0 then l else -l)


(** One-step propagation. *)
let propagate_step m clauses =
	List.fold_left
		(fun m c ->
			if List.exists (fun l -> sat m l) c then m else
				match List.filter (fun l -> not (sat m (-l))) c with
					| [] -> raise Conflict
					| [l] -> add m l
					| l::_ -> m)
		m
		clauses

(** Pretty-print a model. *)
let pp_model chan (assignments, values, stack) =
	Format.fprintf chan "[#%d:" (List.length stack) ;
	List.iter
		(fun i -> Format.fprintf chan " %d" (if values.(i) then i else -i))
		stack ;
	Format.fprintf chan "]"

(** Run DPLL for current Dimacs problem. *)
let dpll out =

	(* Function for producing the trace file, if TRACE is enabled.
	 * Don't mess with the traces as you will need them to be stable
	 * over successive refinements of your implementation. *)
	let print_trace =
		try
			ignore (Sys.getenv "TRACE") ;
			let chan = Format.formatter_of_out_channel (open_out "trace.txt") in
			fun m -> Format.fprintf chan "%a@." pp_model m
		with _ -> fun _ -> ()
	in

	(* Flag to show some debugging information.
	 * You may add your own debugging information, but don't
	 * mess with the traces as you will need them to be stable
	 * over successive refinements of your implementation. *)
	let debug =
		try ignore (Sys.getenv "DEBUG") ; true with Not_found -> false in

	(* Get clauses as lists of integers. *)
	let clauses =
		List.rev_map (List.map Dimacs.to_int) (Dimacs.get_clauses ()) in

	let rec propagate m =
		let _, _, stack_m = m in
		let m' = propagate_step m clauses in
		let _, _, stack_m' = m' in
			if stack_m = stack_m' then m else propagate m'
	in

	let rec find_unassigned m =
		try
			List.iter
				(fun c ->
					let unassigned = ref 0 in
					try
						List.iter
							(fun l ->
									Format.printf "%d " l;
									if sat m l then raise Break ;
									if not (assigned m l) then unassigned := l)
							c ;
						Format.printf "Hey %d\n" !unassigned;
						assert (!unassigned <> 0) ;
						raise (Found !unassigned)
					with Break -> ())
				clauses ;
			(* If we exit [List.iter] then all clauses are SAT. *)
			let a, v, s = m in
			raise (SAT (a, v, s))
		with Found i -> i
	in


	(* DPLL algorithm. *)
	let rec dpll m =
		print_trace m ;
		if debug then Format.printf "> %a@." pp_model m ;
		match propagate m with
			| exception Conflict -> ()
			| m ->
					let l = find_unassigned m in
						dpll (add m l) ;
						if debug then Format.printf "backtrack@." ;
						dpll (add m (-l))
	in
		try
			let model = Array.make_matrix 2 (Dimacs.nb_variables () + 2) false in
			dpll (model.(0), model.(1), []) ;
			Format.printf "UNSAT\n" ;
			Printf.fprintf out "UNSAT\n"
		with SAT (a, v, s) ->
			let m = (a, v, s) in
			Format.printf "SAT\n" ;
			Printf.fprintf out "SAT\n" ;
			for i = 1 to Dimacs.nb_variables () do
				Printf.fprintf out "%d "
					(if sat m i then i else -i)
			done ;
			Printf.fprintf out "0\n"

let () =
	if Array.length Sys.argv <> 3 then begin
		Format.printf "Usage: %s <input.cnf> <output.model>\n" Sys.argv.(0) ;
		exit 1
	end ;
	let input = open_in Sys.argv.(1) in
	let output = open_out Sys.argv.(2) in
	Dimacs.input input ;
	dpll output

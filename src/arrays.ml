(** Variables (i.e. propositional constants) are represented by positive
	* integers. Literals are arbitrary integers: for any variable X coded
	* as a positive integer [n], the positive literal X is represented by
	* [n] and not(X) is represented by [-n].
	*
	* A partial assignment [m] is a matrix of size [2 x (n+1)] of booleans where n
	* is the number of variables to be assgned in the SAT problem. The first line
	* represents whether a litteral has already been assigned or not, and the
	* second line represents the value of assigned litterals.
	*
	* e.g. if [(0, 5)] and [(1, 5)] are both set to [true], it means that at this
	* point of the execution, the litteral represented by the integer 5 is
	* assigned to [true], but if [(0, 5)] is [false], then it is not assigned,
	* whatever the value of [(1, 5)] is.
	*
	*
	*
	* The stack represents changes made by each successive calls of dpll in the
	* current branch of the assignment tree.
	* Before the execution of the main call, the stack is empty.
	* Before each recursive call, 0 is added on the stack. As this value does not
	* represent any litteral, it does not mean anything in the stack.
	* If the dpll function does not raise SAT, and stops, every alternate value is
	* set back to unassigned and the stack returns to its origilan form. In such a
	* case, there is no need to add 0 to the stack again as there is no more
	* possible values to try for the current litteral.
	*)

exception Conflict
exception SAT
exception Found of int
exception Break

(** Pretty-print a model. *)
let pp_model chan m =
	let nb_assigned = (* number of assigned variables *)
		Array.fold_left
			(fun nb elt -> if elt then nb + 1 else nb)
			0
			m.(0)
	in
	Format.fprintf chan "[#%d:" (nb_assigned) ;
	Array.iteri
		(fun idx elt ->
			if elt && idx <> 0 then
				Format.fprintf chan " %d" (if m.(1).(idx) then idx else -idx))
		m.(0);
	Format.fprintf chan "]"

(* Function for producing the trace file, if TRACE is enabled.
 * Don't mess with the traces as you will need them to be stable
 * over successive refinements of your implementation. *)
let print_trace =
	try
		ignore (Sys.getenv "TRACE") ;
		let chan = Format.formatter_of_out_channel (open_out "trace.txt")
		in
		fun m -> Format.fprintf chan "%a@." pp_model m
	with _ -> fun _ -> ()

(* Flag to show some debugging information.
 * You may add your own debugging information, but don't
 * mess with the traces as you will need them to be stable
 * over successive refinements of your implementation. *)
let debug =
	try ignore (Sys.getenv "DEBUG") ; true with Not_found -> false 



(** Run DPLL for current Dimacs problem. *)
let dpll out =
	let m = Array.make_matrix 2 (Dimacs.nb_variables()+1) false
	(* Get clauses as lists of integers. *)
	and clauses =
		List.rev_map (List.map Dimacs.to_int) (Dimacs.get_clauses ())
	and stack = Stack.create () in

	(** [sat m l] indicates whether a literal is satisfied by a partial
	 * assignment. Satisfaction is always false for unassigned literals. *)
	let sat l =
		if l > 0 then
			m.(0).(l) && m.(1).(l)
		else
			m.(0).(-l) && (not m.(1).(-l))
	in
	 
	(** Return new partial assignment where a previously unassigned literal
	 * is set to true. *)
	let add l =
		let value, idx = if l > 0 then (true, l) else (false, -l) in
		Stack.push idx stack;
		m.(0).(idx) <- true;
		m.(1).(idx) <- value
	in
	
	(** Indicates whether a litteral is assigned in a partial assignment. *)
	let assigned l =
		m.(0).(if l > 0 then l else -l)
	in

	(** One-step propagation. *)
	let propagate_step clauses =
		List.fold_left
		(fun b c ->
			if List.exists (fun l -> sat l) c then b else
				match List.filter (fun l -> not (sat (-l))) c with
				| [] -> raise Conflict
				| [l] -> add l; true
				| l::_ -> b)
		false
		clauses
	in

	let rec propagate () =
		if propagate_step clauses then propagate (); 
	in

	let rec find_unassigned () =
		try
			List.iter
				(fun c ->
					let unassigned = ref 0 in
					try
						List.iter
							(fun l ->
									if sat l then raise Break ;
									if not (assigned l) then unassigned := l)
						 c ;
						assert (!unassigned <> 0) ;
						raise (Found !unassigned)
					with Break -> ())
				clauses ;
			(* If we exit [List.iter] then all clauses are SAT. *)
			raise (SAT)
		with Found i -> i
	in

	(* DPLL algorithm. *)
	let rec dpll () =
		print_trace m ;
		if debug then Format.printf "> %a@." pp_model m ;
		match propagate () with
			| exception Conflict -> ()
			| _ -> 
					let l = find_unassigned () in
						Stack.push 0 stack;
						add l;
						dpll () ;
						if debug then Format.printf "backtrack@." ;
						let rec backtrack_stack () =
							match Stack.pop stack with
								| 0 -> ()
								| i ->
									(
										m.(0).(i) <- false;
										backtrack_stack ()
									)
						in
						backtrack_stack ();
						(add (-l));
						dpll ()
	in
	try
		dpll ();
		Format.printf "UNSAT\n" ;
		Printf.fprintf out "UNSAT\n"
	with SAT ->
		Format.printf "SAT\n" ;
		Printf.fprintf out "SAT\n" ;
		for i = 1 to Dimacs.nb_variables () do
			Printf.fprintf out "%d "
			(if sat i then i else -i)
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

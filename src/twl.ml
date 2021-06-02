module type SIG = sig
	type 'a mlist
	val create: 'a mlist
	val prec_opt: 'a mlist -> 'a mlist option
	val prec: 'a mlist -> 'a mlist
	val next_opt: 'a mlist -> 'a mlist option
	val next: 'a mlist -> 'a mlist
	val set: 'a mlist -> 'a -> unit
	val get: 'a mlist -> 'a
	val add: 'a mlist ->'a -> 'a mlist
	val remove: 'a mlist -> 'a mlist
	val iter_l: 'a mlist -> ('a -> unit) -> unit
	val iter_r: 'a mlist -> ('a -> unit) -> unit
	val iter: 'a mlist -> ('a -> unit) -> unit
	val last: 'a mlist -> 'a mlist option
	val first: 'a mlist -> 'a mlist option
	val fold_l: ('a -> 'b -> 'a) -> 'a -> 'b mlist -> 'a
	val fold_r: ('a -> 'b -> 'a) -> 'a -> 'b mlist -> 'a
end


(* Doubled chained lists are used in this algorithm.
 * Putting the double linked list code in module makes it easier to manipulate
 * in the code to come, as the internal representation of the lists do not
 * matter.
 *)

module MList: SIG = struct
	type 'a mlist =
		Element of {
			mutable value: 'a;
			mutable next: 'a mlist;
			mutable prec: 'a mlist}
		| Empty

	let create : 'a mlist =
		Empty

	let prec_opt (lst: 'a mlist): 'a mlist option =
		match lst with
		| Element c -> Some c.prec
		| Empty -> None

	let prec (lst: 'a mlist): 'a mlist =
		match lst with
		| Element c -> c.prec
		| Empty -> raise Not_found

	let next_opt (lst: 'a mlist): 'a mlist option =
		match lst with
		| Element c -> Some c.next
		| Empty -> None

	let next (lst: 'a mlist): 'a mlist =
		match lst with
		| Element c -> c.next
		| Empty -> raise Not_found

	let set (lst: 'a mlist) (v: 'a): unit =
		match lst with
		| Element c -> c.value <- v
		| Empty -> failwith("set applied to Empty.")

	let get (lst: 'a mlist) : 'a =
		match lst with
		| Element c -> c.value
		| Empty -> failwith("get applied to Empty.")

	let rec add (lst: 'a mlist) (v: 'a) : 'a mlist =
		match lst with
		| Element c ->
			(
				(match c.next with
				| Element c' -> let _ = add c.next v in ()
				| Empty -> c.next <- Element {value = v ; prec = c.next ; next = Empty});
				lst
			)
		| Empty -> Element {value = v; next = Empty; prec = Empty}

	(* When remove is called, the element removed is removed and the nearest
	 * element is send back
	 *)
	let remove (lst: 'a mlist) : 'a mlist =
		match lst with
		| Element c ->
			(
				match c.prec, c.next with
				| Element pc, Empty      -> ( pc.next <- Empty; c.prec )
				| Element pc, Element nc -> ( pc.next <- c.next; nc.prec <- c.prec;
				c.prec )
				| Empty,      Element nc -> ( nc.prec <- Empty; c.next )
				| Empty,      Empty      -> Empty
			)
		| Empty -> Empty

	let rec iter_l (lst: 'a mlist) (f: 'a -> unit) : unit =
		match lst with
		| Element c -> ( f c.value;  iter_l c.prec f )
		| Empty -> ()

	let rec iter_r (lst: 'a mlist) (f: 'a -> unit) : unit =
		match lst with
		| Element c -> ( f c.value;  iter_r c.next f )
		| Empty -> ()

	let iter (lst: 'a mlist) (f: 'a -> unit) : unit =
		match lst with
		| Element c ->
			( f c.value; iter_l c.prec f; iter_r c.next f)
		| Empty -> ()

	let rec last (lst: 'a mlist) : 'a mlist option =
		match lst with
		| Element c ->
			(
				match last c.next with
				| Some l -> Some l
				| None -> Some lst
			)
		| Empty -> None

	let rec first (lst: 'a mlist) : 'a mlist option =
		match lst with
		| Element c ->
			(
				match first c.prec with
				| Some l -> Some l
				| None -> Some lst
			)
		| Empty -> None

	let rec fold_l (f: 'a -> 'b -> 'a) (start: 'a) (lst: 'b mlist) : 'a =
		match lst with
		| Element c -> f (fold_l f start c.prec) c.value
		| Empty -> start

	let rec fold_r (f: 'a -> 'b -> 'a) (start: 'a) (lst: 'b mlist) : 'a =
		match lst with
		| Element c -> f (fold_r f start c.next) c.value
		| Empty -> start
end





(*       -------      TWL Algorithm related code below      -------          *)



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
	(* Creation of usefull data structures:
	 * + [m] is the matrix representing the partial assigning ;
	 * + [clauses_array] is the array of doubled chaines lists of clauses. Each
	 *     | cell i contains the list of clauses indexed by the litteral i ;
	 * + [stack] is the stack that stores changes applied during sucessive [dpll]
	 *     | calls.
	 *)
	let m = Array.make_matrix 2 (Dimacs.nb_variables()+1) false
	and clauses_array = Array.make (Dimacs.nb_variables () + 1) MList.create
	and stack = Stack.create ()
	in

	(* Get clauses as lists of int * int * int list
	 * and add them to the global array.
	 *)
	let clauses =
		let add_clause (i, i', c) =
			let idx = if i > 0 then i else -i
			and idx' = if i' > 0 then i' else -i'
			in
			clauses_array.(idx) <- MList.add clauses_array.(idx) (i, i', c);
			clauses_array.(idx') <- MList.add clauses_array.(idx') (i, i', c);
			(idx, idx', c)
		in
		List.map
			(fun c ->
				match c with
				| [] -> failwith "Null clause."
				| [h] -> add_clause (h, h, c)
				| h :: h' :: t -> add_clause (h, h', c)
				)
			( (* List of list of integers representing a list of clauses *)
			List.rev_map (List.map Dimacs.to_int) (Dimacs.get_clauses ()))
	in

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
		(fun b (_, _, c) ->
			if List.exists (fun l -> sat l) c then b else
				match List.filter (fun l -> not (sat (-l))) c with
				| [] -> raise Conflict
				| [l] -> add l; true
				| l::_ -> b)
		false
		clauses
	in

	let rec propagate n =
		if propagate_step clauses then propagate (n+1); 
	in

	let rec find_unassigned () =
		try
			List.iter
				(fun (_, _, c) ->
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
		match propagate 0 with
			| exception Conflict -> ()
			| _ -> 
					let l = find_unassigned () in
						Stack.push 0 stack;
						add l;
						dpll () ;
						if debug then Format.printf "backtrack@." ;
						let elem = ref (1) in
						while !elem > 0 do
							elem := Stack.pop stack;
							m.(0).(!elem)<-false
						done;
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

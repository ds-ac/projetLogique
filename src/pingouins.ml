exception Found

(** Get the penalty value from the corresponding environment variable. *)
let penalty = Sys.getenv "PENALTY" |> int_of_string

(** Count the cardinal of the ice set*)
let get_ice_size bool_ice =
	Array.fold_left
		(fun nb arr ->
			Array.fold_left
			(fun nb elt -> if elt then 1 + nb else nb)
			nb
			arr)
		0
		bool_ice

(** Make the domain for the problem from the ice state.
 * There will be one for the pingouin positions through time and one for the ice
 * evolution throught time:
 * → [ice_size] is the (initial) cardinal of the set of the ice poritions.
 * → [moves.(i).(j).(t)] should be set to [true] iff the pingouin is at (i, j)
 *    at the time t (i.e. after t moves).
 * → [ice.(i).(j).(t)] should be set to [true] iff there is ice at (i, j) at
 *    the time t.
 * NB: the time is less or equal to the initial number of frozen locations minus
 * the chosen penalty for the resolution.
 *)
let domain ice_state =
	let ice_size = get_ice_size ice_state
	and height = Array.length ice_state
	and width = Array.length ice_state.(0) in
	let moves = Dimacs.(make (height**width**(ice_size - penalty)**o))
	and ice_locs = Dimacs.(make (height**width**(ice_size - penalty)**o)) in
	ice_size, height, width, moves, ice_locs


(** Write the problem under the SAT form. *)
let problem file =
	let pos_init, ice_state = Hex.from_channel (open_in file) in
	let ice_size, height, width, moves, ice_locs = domain ice_state in
	let ice = (* get an array of the ice locations *)
		let result = Array.make ice_size (-1,-1)
		and idx = ref 0 in
		Array.iteri
			(fun i arr ->
				Array.iteri
					(fun j b ->
						if b then (result.(!idx) <- (i,j); incr idx))
					arr)
			ice_state;
		result
	in

	(** Print the ice_state to solve. *)
	Hex.pp_bool_grid Format.std_formatter ice_state;

	(**   ---   Initial conditions   ---   *)

	(** Set up the litterals ice_locs.(_).(_).(0) (state of the ice at time 0). *)
	Array.iteri
		(fun i (a, b) ->
			Dimacs.(add_clause [ice_locs.(a).(b).(0)]))
		ice;

	(** Set up the litterals moves.(_).(_).(0) (state of the pingouin location at
	 * time 0).
	 *)
	Dimacs.(add_clause [moves.(fst pos_init).(snd pos_init).(0)]);

	(**   ---   moves constraints   ---   *)

	(* The pingouin cannot stand on water *)
	Array.iter
		(fun(i, j) ->
			Array.iter2
				(fun i m ->
				Dimacs.(
					add_clause
						[i; not m]))
				ice_locs.(i).(j)
				moves.(i).(j)
		)
		ice;

	(** The pingouin cannot go to a location containing water in the initial game,
	 * and there will never be ice on a position that was initially frozen.
	 *)
	Array.iteri
		(fun i arr ->
			Array.iteri
				(fun j state ->
					if not state then
						(
						Array.iter2
							(fun m i -> Dimacs.(add_clause [not m; not i]))
							moves.(i).(j)
							ice_locs.(i).(j)
						)
				)
				arr
		)
		ice_state;

	(** The pingouin cannot use flooded locations to move. *)
	for t = 1 to ice_size - penalty - 1 do
		List.iter
			(fun dir ->
				Array.iter
					(fun (i, j) ->
						let new_pos = ref (Hex.move (i, j) dir)
						and available_moves = ref []
						in
						while ice_state.(fst !new_pos).(snd !new_pos) do
							List.iter
								(fun (i', j') ->
									(Dimacs.
										(add_clause
											[not moves.(i).(j).(t-1);
											ice_locs.(i').(j').(t-1);
											not moves.(fst !new_pos).(snd !new_pos).(t)])))
							!available_moves;
							available_moves := !new_pos::!available_moves;
							new_pos := (Hex.move !new_pos dir)
						done)
					ice)
			Hex.all_directions
	done;

	(** The pingouin stands on exacly one position at all time [t]. *)
	for t = 0 to ice_size - penalty - 1 do
		(** At any time [t], there is at least a position where the pingouin
		 * stands.
		 *)
		Dimacs.(
			add_clause
				(bigor
					ice_size
					(fun n -> moves.(fst ice.(n)).(snd ice.(n)).(t))));
		(** At any time [t], there is only location s.t. [x.(i).(j).(t)] *)
		Array.iteri
			(fun i arr ->
				Array.iteri
				(fun j m ->
					(** Consider another location m at time [t] *)
					Array.iteri
						(fun i' arr' ->
							Array.iteri
								(fun j' m' ->
									if (i, j) <> (i', j') then
										Dimacs.(add_clause [not m.(t); not m'.(t)])
								)
								arr'
						)
						moves
				)
				arr
			)
			moves
	done;

	(** For any move, there is a direction in which it is achieved *)
	for t = 1 to ice_size - penalty - 1 do
		Array.iter
			(fun (i, j) ->
				let available_moves = ref [] in
				List.iter
					(fun dir ->
						let new_pos = ref (Hex.move (i, j) dir) in
							while ice_state.(fst !new_pos).(snd !new_pos) do
								available_moves := !new_pos::!available_moves;
								new_pos := (Hex.move !new_pos dir)
							done)
					Hex.all_directions;
				let available_moves_array = Array.of_list !available_moves in
				Dimacs.(
					add_clause
						((not moves.(i).(j).(t-1))
							::
							(bigor
								(Array.length available_moves_array)
								(fun n ->
									let i, j = (fst available_moves_array.(n), snd available_moves_array.(n))
									in
									moves.(i).(j).(t))))))
			ice
	done;

	(** The pingouin cannot get twice at the same position, at the position would
	 * not be frozen the second time.
	 *)
	Array.iter(* iterate on every initially frozen location. *)
		(fun (i, j) ->
			for t = 0 to ice_size - penalty - 1 do
				for t' = t + 1 to ice_size - penalty - 1 do
					Dimacs.(
						add_clause
							[not moves.(i).(j).(t); not moves.(i).(j).(t')])
				done
			done
		)
		ice;

	(**   ----   Ice constraints   ---   *)

	(** A flood location cannot freeze again. *)
	Array.iter
		(fun (i, j) ->
			for t = 0 to ice_size - penalty - 1 do
				for t' = t + 1 to ice_size - penalty - 1 do
					Dimacs.(
						add_clause
							[ice_locs.(i).(j).(t);
							not ice_locs.(i).(j).(t')])
				done
			done
		)
		ice;

	(** Frozen locations turn into flooded locations *)
	Array.iter
		(fun (i, j) ->
			for t = 0 to ice_size - penalty - 2 do
				Dimacs.(
					add_clause
						[not moves.(i).(j).(t);
						not ice_locs.(i).(j).(t + 1)])
			done)
		ice

let solution file =
	let pos_init, ice_state = Hex.from_channel (open_in file) in
	let ice_size, height, width, moves, ice_locs = domain ice_state in
	let m = Dimacs.read_model (open_in "output.sat") in

	(* Finding the path *)
	let path = Array.make (ice_size - penalty) (0, 0) in
	for t = 0 to ice_size - penalty - 1 do
		try(** There is only one interesting location at time [t]. *)
			for i = 0 to height - 1 do
				for j = 0 to width - 1 do
					if Dimacs.sat m moves.(i).(j).(t) then
						(path.(t) <- (i,j); raise Found)
				done
			done
		with Found -> ()
	done;
	Hex.pp_solution Format.std_formatter ice_state path

let () = Dimacs.run ~problem ~solution

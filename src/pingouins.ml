let problemFile = "PROBLEM" |> Sys.getenv
let penalty = "PENALTY" |> Sys.getenv |> int_of_string

let start, grid = Hex.from_channel (open_in problemFile);;
let width = Array.length grid and height = Array.length grid.(0);;
let maxWidthHeight = max width height;;

let cases = ref 0;;
for x' = 0 to width - 1 do
    for y' = 0 to height - 1 do
        if grid.(x').(y') then (incr cases)
    done;
done;;

let steps = !cases - penalty;;

(* banquise, pingouin *)
let domain () =
    let banquise, pingouin = Dimacs.(make (width**height**(!cases)**o)), Dimacs.(make (width**height**(!cases)**o)) in (* la troisième dimension est le temps *)
	for x' = 0 to width - 1 do
		for y' = 0 to height - 1 do
			if grid.(x').(y') then Dimacs.(add_clause [banquise.(x').(y').(0)])
		done;
	done;
	banquise, pingouin

(* doit empêcher d'être à deux endroits en même temps (au lieu de tout énumérer pourrait juste traiter les cases que l'on considère *)
(* doit autoriser que les mouvements valides *)

let dirAndDistance x' y' x'' y'' =
	(*let rec aux x y l =
		if x = x'' 
	in aux [];;*)
	(*print_string "hey0\n";*)
	let dir = ref Hex.N and distance = ref 0 and i = ref 1 in
	while !i <= maxWidthHeight && !distance = 0 do
		let rec aux l = match l with
		| dir'::q -> if (Hex.move_n (x', y') (dir', !i)) = (x'', y'') then (dir := dir'; distance := maxWidthHeight); aux q
		| [] -> ()
		in aux Hex.all_directions;
		incr i;
	done;
	(*print_string "hey1\n";*)
	!dir, !distance;;
	
let positionsTo x' y' x'' y'' =
	let dir, distance = dirAndDistance x' y' x'' y'' in
	if distance <> 0 then
	(
		(*print_string "hey\n";*)
		let rec aux x y i l =
			if x = x'' && y = y''
			then l
			else (let (x''', y''') = (Hex.move_n (x', y') (dir, i)) in aux x''' y''' (i+1) ((x''', y''')::l))
		in aux x' y' 1 []
	)
	else
		[]

let allWithout x' y' =
    let l = ref [] in
    for x'' = 0 to width-1 do
        for y'' = 0 to height-1 do
            if grid.(x'').(y'') && x'' <> x' && y'' <> y' then
                l := (x'', y'')::(!l)
        done
    done;
    !l;;

let aled x k =
    let l = ref [] in
    for x' = 0 to width-1 do
        for y' = 0 to height-1 do
            (*print_string "h0: "; print_int x'; print_char ' '; print_int y'; print_char ' '; print_int k; print_char ' '; print_int (!cases); print_string " (";  print_string ")"; print_string "\n";*)
            if grid.(x').(y') then
                ((*print_int k; print_char ' ';*)
                l := (x.(x').(y').(k))::(!l))
        done
    done;
    !l;;

let problem n =
    let banquise, pingouin = domain () in
	(* forcer *)
	for t = 0 to steps - 1 do
		for x' = 0 to width-1 do
			for y' = 0 to height-1 do
				Dimacs.(add_clause [banquise.(x').(y').(t); not pingouin.(x').(y').(t)])
			done
		done
	done;
	(* valide *)
	for t = 0 to steps - 2 do
		for x' = 0 to width-1 do
			for y' = 0 to height-1 do
				Dimacs.(add_clause [not pingouin.(x').(y').(t); not banquise.(x').(y').(t + 1)])
			done
		done
	done;
	
	(* plus de banquise = plus de banquise ! *)
	for t = 0 to steps - 1 do
		for x' = 0 to width-1 do
			for y' = 0 to height-1 do
				for t' = t + 1 to steps - 2 do
					Dimacs.(add_clause [banquise.(x').(y').(t); not banquise.(x').(y').(t')])
				done
			done
		done
	done;
	
	(* si pas de banquise, pas de glisse *)
	(*print_string "yes\n";*)
	(*for t = 0 to steps - 2 do
		(*print_string "ho\n";*)
		for x' = 0 to width-1 do
			for y' = 0 to height-1 do
				for x'' = 0 to width-1 do
					for y'' = 0 to height-1 do
						if x' <> x'' || y' <> y'' then
						(
							if grid.(x').(y') && grid.(x'').(y'') then
							(
								let l' = positionsTo x' y' x'' y'' in
								(*print_int (List.length l');*)
								let rec aux l = match l with
								| (x''', y''')::q -> Dimacs.(add_clause [not pingouin.(x').(y').(t); banquise.(x''').(y''').(t); not pingouin.(x'').(y'').(t + 1)]); aux q
								| [] -> ()
								in aux l'
							)
						)
					done
				done
			done
		done
	done;*)
	
	(* si pas de banquise, pas de glisse *)
	(*for t = 0 to steps - 1 do
		for x' = 0 to width-1 do
			for y' = 0 to height-1 do
				if grid.(x').(y') then
					Dimacs.(add_clause [banquise.(x').(y').(t); not banquise.(x').(y').(t')])
			done
		done
	done;*)
	
    (* not at two places at once, max à un endroit *)
	for k = 0 to !cases-1 do
        for x' = 0 to width-1 do
            for y' = 0 to height-1 do
                for x'' = 0 to width-1 do
                    for y'' = 0 to height-1 do
                        if x'' <> x' && y'' <> y' then
                            Dimacs.(add_clause [not pingouin.(x').(y').(k); not pingouin.(x'').(y'').(k)])
                    done
                done
            done
        done
    done;

    (* faire que doit bouger sur une nouvelle case à chaque fois, la factorielle pour décrire chaque chemin c'est mal, dire à chaque time step de changer de case peut être mieux *)
    (* complexité *)
    (* factorielle: cases! *)
    (* pas à pas: (cases * cases) * cases  *)
    (* could use hash table not to list all and check if on grid each time *)
    (*for k = 0 to !cases-2 do
        for x' = 0 to width-1 do
            for y' = 0 to height-1 do
                for x'' = 0 to width-1 do
                    for y'' = 0 to height - 1 do
                        if grid.(x'').(y'') && x'' <> x' && y'' <> y' then
                            Dimacs.(add_clause [])
                    done
                done
            done
        done
    done;*)
    (* this makes it move each time but we need the melting part ! *)
    (*310521
	for k = 0 to !cases-2 do
        for x' = 0 to width-1 do
            for y' = 0 to height-1 do
                Dimacs.(add_clause [not pingouin.(x').(y').(k); not pingouin.(x').(y').(k+1)])
            done
        done
    done;*) (*qd tu c qu'un pinguin est à un endroit, tu sais qu'il n'y sera plus*)

    (* x', y', k *)
    (* not x.(x').(y').(k); not x.(x').(y').(k+1) *)

    (* faire que chaque mouvement doit être valide, implémenter fonction prog2 liste des mouvements dispo ? *)

	(*print_int (fst start); print_char ','; print_int (snd start); print_newline();*)
    Dimacs.(add_clause [pingouin.(1).(1).(1)]);
    (*Dimacs.(add_clause [pingouin.(fst start).(snd start).(1)]);*)

    (*let alled = all x in*)
    (* au moins un endroit à chaque time step *)
    for k = 0 to steps-1 do
        Dimacs.(add_clause (aled pingouin k))
    done;;

(* take into consideration penalty for modelization *)

(* lister en ou toutes les positions au temps 1 et ainsi de suite tout en vérifiant qu'il ne peut pas être à 2 endroits en même temps *)
(* moi je vais lister les chemins en OU de ET et utiliser un truc pour passer à CNF - on a le droit ? DIMACS ne gère pas DNF faisong le à la main ?*)

(* attention il y a une position de départ *)
(* returns list of list when each sub list is a path for a given origin *)
(*let paths grid origin =
    let all_dirs = Hex.all_directions 
    let rec aux l =

    in aux []*) (*well all_directions ne liste pas les différentes possibilités à un moment donné, utilisé prog2 semble trop complexe *)

let solution n =
    let banquise, pingouin = domain () and m = Dimacs.read_model (open_in "output.sat") in
    Hex.pp_bool_grid Format.std_formatter grid;
    (*let bGrid = Array.make_matrix width height false in*)
    let positions = ref [] in
    for k = 0 to !cases-1 do
        for x' = 0 to width-1 do
            for y' = 0 to height-1 do
                (*bGrid.(y).(x') <- Dimacs.sat m x.(y).(x').(k)*)
                if (Dimacs.sat m pingouin.(x').(y').(k)) then
                (
                    (*print_int x'; print_char ' '; print_int y'; print_string "\n";*)
                    positions := (x', y')::(!positions)
                )
            done
        done
    done;
	positions := List.rev !positions; (* likewise chronological *)
    (*print_int width; print_char ' '; print_int height; print_string "\n";*)
    Hex.pp_solution Format.std_formatter grid(*bGrid*) (Array.of_list (!positions))

let () =
    Dimacs.run ~problem ~solution

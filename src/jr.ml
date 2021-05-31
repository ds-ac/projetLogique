(* 0         1         2         3         4         5         6         7         8         9         10

   1         2         1         0         2         0         1         2         2         3         1
3     1   3     1   3     3   2     2   2     2   0     0   0     3   1     0   1     1   1     3   3     0
   1         2         3         1         0         1         2         2         0         2         1    *)

let domain n =
    (* for each position in the tiling we have 11 possibilities of tiles *)
    Dimacs.(make (n**n**11**o))

let problem n =
    let v = domain n in

    (* the k-th tile isn't at y x or we fix the potential neighbors *)
    let u0 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y-1).(x).(4); v.(y-1).(x).(8)]) in
    let u1 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y-1).(x).(0); v.(y-1).(x).(3); v.(y-1).(x).(5); v.(y-1).(x).(10)]) in
    let u2 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y-1).(x).(1); v.(y-1).(x).(6); v.(y-1).(x).(7); v.(y-1).(x).(9)]) in
    let u3 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y-1).(x).(2)]) in

    let l0 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y).(x-1).(5); v.(y).(x-1).(7); v.(y).(x-1).(10)]) in
    let l1 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y).(x-1).(0); v.(y).(x-1).(1); v.(y).(x-1).(8)]) in
    let l2 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y).(x-1).(3); v.(y).(x-1).(4)]) in
    let l3 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y).(x-1).(2); v.(y).(x-1).(6); v.(y).(x-1).(9)]) in

    let r0 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y).(x+1).(5); v.(y).(x+1).(6)]) in
    let r1 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y).(x+1).(7); v.(y).(x+1).(8); v.(y).(x+1).(9)]) in
    let r2 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y).(x+1).(3); v.(y).(x+1).(4)]) in
    let r3 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y).(x+1).(0); v.(y).(x+1).(1); v.(y).(x+1).(2); v.(y).(x+1).(10)]) in

    let d0 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y+1).(x).(3); v.(y+1).(x).(5)]) in
    let d1 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y+1).(x).(0); v.(y+1).(x).(2); v.(y+1).(x).(6); v.(y+1).(x).(10)]) in
    let d2 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y+1).(x).(1); v.(y+1).(x).(4); v.(y+1).(x).(7); v.(y+1).(x).(8)]) in
    let d3 y x k = Dimacs.(add_clause [not v.(y).(x).(k); v.(y+1).(x).(9)]) in

    (* this function checks the colors match the tiles around the y x one *)
    (* up left right down *)
	let next_to y x k =
		let isNotDown = y > 0 and isNotLeft = x > 0 and isNotRight = x < n-1 and isNotUp = y < n-1 in
		match k with
		| 0 -> if isNotDown then u1 y x k;
			   if isNotLeft then l3 y x k;
			   if isNotRight then r1 y x k;
			   if isNotUp then d1 y x k

		| 1 -> if isNotDown then u2 y x k;
			   if isNotLeft then l3 y x k;
			   if isNotRight then r1 y x k;
			   if isNotUp then d2 y x k

		| 2 -> if isNotDown then u1 y x k;
			   if isNotLeft then l3 y x k;
			   if isNotRight then r3 y x k;
			   if isNotUp then d3 y x k

		| 3 -> if isNotDown then u0 y x k;
			   if isNotLeft then l2 y x k;
			   if isNotRight then r2 y x k;
			   if isNotUp then d1 y x k

		| 4 -> if isNotDown then u2 y x k;
			   if isNotLeft then l2 y x k;
			   if isNotRight then r2 y x k;
			   if isNotUp then d0 y x k

		| 5 -> if isNotDown then u0 y x k;
			   if isNotLeft then l0 y x k;
			   if isNotRight then r0 y x k;
			   if isNotUp then d1 y x k

		| 6 -> if isNotDown then u1 y x k;
			   if isNotLeft then l0 y x k;
			   if isNotRight then r3 y x k;
			   if isNotUp then d2 y x k

		| 7 -> if isNotDown then u2 y x k;
			   if isNotLeft then l1 y x k;
			   if isNotRight then r0 y x k;
			   if isNotUp then d2 y x k

		| 8 -> if isNotDown then u2 y x k;
			   if isNotLeft then l1 y x k;
			   if isNotRight then r1 y x k;
			   if isNotUp then d0 y x k

		| 9 -> if isNotDown then u3 y x k;
			   if isNotLeft then l1 y x k;
			   if isNotRight then r3 y x k;
			   if isNotUp then d2 y x k

		| _ -> if isNotDown then u1 y x k;
			   if isNotLeft then l3 y x k;
			   if isNotRight then r0 y x k;
			   if isNotUp then d1 y x k
	in

    (* check that there is at most one tile per position *)
    for y = 0 to n-1 do
        for x = 0 to n-1 do
            for k = 0 to 10 do
                for l = k+1 to 10 do
                    Dimacs.(add_clause [not v.(y).(x).(k); not v.(y).(x).(l)])
                done
            done
        done
    done;

    (* check that for every position there is a tile *)
    for y = 0 to n-1 do
        for x = 0 to n-1 do
            Dimacs.(add_clause (bigor 11 (fun k -> v.(y).(x).(k))))
        done
    done;

	let forbidden_pattern = [|5; 0; 2; 9; 4|] in
	for y = 0 to 4 do
		Dimacs.(add_clause [v.(n/2-2+y).(n/2).(forbidden_pattern.(y))]);
	done;

	(* fix neighbors *)
    for y = 0 to n-1 do
        for x = 0 to n-1 do
            for k = 0 to 10 do
                next_to y x k
            done
        done
    done

let solution n =
    let v = domain n and m = Dimacs.read_model (open_in "output.sat") in
    for y = 0 to n-1 do
        for x = 0 to n-1 do
            for k = 0 to 10 do
                if Dimacs.sat m v.(y).(x).(k) then
					Format.printf (if k < 10 then "0%i " else "%i ") k
            done
        done;
        Format.printf "\n"
    done

let () = Dimacs.run_int ~problem ~solution

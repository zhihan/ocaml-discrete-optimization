open Dist 
open Util

(* Create the distance data structure  *)
let create_dist nV (coords: (float*float) list) = 
  let x = Array.create nV 0. in
  let y = Array.create nV 0. in
  let rec loop i remain  = 
    match remain with
      | [] -> ()
      | h::tl -> 
        begin
          x.(i) <- fst h;
          y.(i) <- snd h;
          loop (i+1) tl
        end 
  in
  begin
    loop 0 coords;
    ArrayDist.compute_edges x y
  end

let check_tour (tour:int array) = 
  let n = Array.length tour in
  let unique = tour in
  for i = 0 to (n-1) do
    let v = tour.(i) in
    let f = find_second unique (fun x -> x = v) in
    match f with
      | Some k -> print_endline ("Invalid tour at " ^ (string_of_int i)
      ^ " found at " ^ (string_of_int k))
      | None -> ()
  done

let compute_cost (tour:int array) (dist:ArrayDist.t) (nV:int) =
  let cost = ref 0.0 in
  let n = (Array.length tour) in
  begin
    for i=0 to n -2 do
      cost := !cost +. (ArrayDist.get dist tour.(i) tour.(i+1) nV)
    done;
    !cost +. (ArrayDist.get dist tour.(0) tour.(n-1) nV)
  end
    

=======
(* A simple greedy strategy that travels to the nearest neighbor
 at every step. *)
let nearest_neighbor (n:int) (dist:ArrayDist.t) (start:int) = 
  let visited = BoolArray.empty n in
  let tour = Array.create n start in
  let cost = ref 0.0 in

  (*Embedded function visit *)
  let visit (i:int) =
    BoolArray.set visited i
  in
  (* Query whether the vertex is visited *)
  let not_visited (i:int) = 
    not (BoolArray.mem visited i)
  in
  
  let indices = array_range 0 (n-1) in
  let current = ref start in
  let _ = visit start in
  let offset = ref 1 in
  begin
    while !offset < n  do
      let nextOpt = find_min_filter indices 
        (fun i-> ArrayDist.get dist (!current) i n) not_visited in
      match nextOpt with
        | Some m -> 
          begin
            tour.(!offset) <- m;
            cost := !cost +. (ArrayDist.get dist (!current) m n );
            visit m;
            current := m;
            offset := !offset + 1
          end
        | None ->  (* No more point to find *)
          ()
    done;
    cost := !cost +. (ArrayDist.get dist start tour.(n-1) n); 
    (!cost, tour)
  end
    
(* Print the result in the format required by the course *)    let print_result cost tour = 
  begin
    Printf.printf "%d %d\n" (int_of_float cost) 0;
    Array.iter (fun x-> Printf.printf "%d " x) tour
  end

let get_edge (tour:int array) (i:int) = 
  if i < (Array.length tour)-1 then
    (tour.(i), tour.(i+1))
  else
    if i = (Array.length tour) -1 then
      (tour.(i), tour.(0))
    else
      (
        print_endline ((string_of_int i) ^ " " 
        ^ (string_of_int (Array.length tour )));
          invalid_arg "Index out of bound"
        )

let swap_targets (tour:int array) i j =
 (* let print_tour s = 
    begin
      Printf.printf s; 
      Array.iter (fun x-> Printf.printf "%d " x) tour ;
      Printf.printf "\n";
    end
  in *)
  (* Get the second vertex in the edge *)
  let to_v i = 
    if i < (Array.length tour) - 1 then i+1 else 0 
  in
  let t1 = to_v i in
  let t2 = j in
  (* swap the vertices at t1 and t2 *)
  begin
    if t1 < t2 then
      reverse tour (t1) t2 
    else
      reverse tour (t2) t1 ;
  end




(* 2-opt local search *)
let two_opt (nV:int) (dist:ArrayDist.t) (cost:float) (tour:int array) =
  (* Test if the swapping of 2-opt is improving *)
  let test (first: int*int) (second: int*int) =
    let v1 = fst first in
    let v2 = snd first in
    let u1 = fst second in
    let u2 = snd second in
    if (v1 = u1) || (v1 = u2) || (v2 = u1) || (v2 = u2) then
        1.0 
    else
      let old_val = (ArrayDist.get dist v1 v2 nV) +. 
        (ArrayDist.get dist u1 u2 nV) in
      let new_val = (ArrayDist.get dist v1 u1 nV) +. 
        (ArrayDist.get dist u2 v2 nV) in
      ( (*Printf.printf "%d - %d, %d - %d\n" v1 v2 u1 u2;
       Printf.printf "against %d - %d, %d - %d\n" v1 u2 v2 u1;
       Printf.printf "new_val %2.4f old_val %2.4f\n" new_val old_val; *)
       (new_val -. old_val))
  in

  let current_cost = ref cost in
  let current_tour = Array.copy tour in
  let stop = ref false in
  begin
    while not(!stop) do
      let improved = ref false in
      for i= 0 to (Array.length tour)-1 do
        let e1 = get_edge current_tour i in
        let j = ref (i+2) in
        while (not(!improved)) && (!j < (Array.length tour) -1) do
          if not((i=0) && ((!j)=(Array.length tour)-2)) then
            let e2 = get_edge current_tour !j in
            let delta = test e1 e2 in
            if delta < 0.0 then
              begin
                current_cost := (!current_cost) +. delta;
                swap_targets current_tour i !j;
                (* 
                let c = compute_cost current_tour dist nV in
                if (c -. !current_cost ) > 1.0 then
                  (
                    Printf.printf "Real cost is %2.4f\n" c;
                    invalid_arg "Cost computation is wrong"
                  )
                else (); *)
                improved := true
              end
            else()
          else();
          j := !j + 1
        done;
      done;
      if not(!improved) then
        stop := true
      else ()
    done;
    (!current_cost, current_tour)
  end
  
  

      

      

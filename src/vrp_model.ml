(* VRP Model *)

open Unix (*time function *)

exception InvalidSol of string
exception InvalidVal of string

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
    Dist.ArrayDist.compute_edges x y
  end



(** Solution *)
module Sol = struct

  open Dist
  open Util
  open Tsp_graph
    
  (** Every tour contains a list of positive integers and is
  interpreted as the tour 0->x0->x1->... ->xn->0 *)
  type t = {tours: int array array; cost: float; loads: int array}

  let tour_dist (tour:int array) (dist:ArrayDist.t) (n:int) = 
    let rec loop (curr:int) (remain:int list) (acc:float) = 
      match remain with
        | [] -> acc +. (ArrayDist.get dist curr 0 n) (* Return to depot*)
        | h::tl -> 
            (* Go to next position *)
          let next = acc +. (ArrayDist.get dist curr h n) in
          loop h tl next
    in
    loop 0 (Array.to_list tour) 0.0 


  let compute_dist (x:int array array) (dist:ArrayDist.t) (n:int) = 
    (** Compute total distance of a tour *)
    Array.fold_left (fun s e -> s +. (tour_dist e dist n)) 0.0 x

  let compute_loads (x:int array array) (demands:int array) = 
    let truck_load (tour:int list) = 
      List.fold_left (fun s e -> s + demands.(e)) 0 tour
    in
    Array.map (fun e -> truck_load (Array.to_list e)) x


  let create (x: int array array) (dist:ArrayDist.t) (n:int) (demands:int array) = 
    {tours = x;
     cost = compute_dist x dist n;
     loads = compute_loads x demands}

  let from_data x c l = {tours = x; cost = c; loads = l}

  let verify x dist n demands = 
    (
      let cost = compute_dist x.tours dist n in
      if ((cost -. x.cost) > 0.001) || ((cost -. x.cost) < -0.001) then
        raise (InvalidSol ("Cost value is wrong! expected " ^
        (string_of_float cost) ^ " actual "^ (string_of_float x.cost)) )
      else ()
      ;
      let loads = compute_loads x.tours demands in
      if loads <> x.loads then
        raise (InvalidSol "Loads vector is wrong!")
      else ()
    )

  let to_string (x:t) =
    let s = ref "" in
    begin
      for i = 0 to ((Array.length x.tours) -1) do
	s := !s ^ "\n" ^ (string_of_int_array x.tours.(i)) ^
	  ": " ^ (string_of_int x.loads.(i))
      done;
      "Sol {" ^ !s ^ "\n}\n" ^ (string_of_float x.cost) ^ "\n"
     end
      
  let overload_penalty (x:t) (cap:int) = 
    Array.fold_left (fun s load -> s + 
      if load > cap then (load - cap) else 0 ) 0 x.loads 
      
      
  (** Compute the delta for adding a new stop i *)
  let new_stop (tour:int array) (stop:int) (dist:ArrayDist.t) (n:int) : float*int = 
    let delta start finish mid = 
      (ArrayDist.get dist start mid n) +. (ArrayDist.get dist mid finish n) -.
        (ArrayDist.get dist start finish n)
    in
    let tn = Array.length tour in
    let ext = Array.make (tn+2) 0 in
    begin
      Array.blit tour 0 ext 1 tn;  (*copy x.tour to ext.(1)...ext(tn) *)
      let r = array_range 0 tn in
      let bestIdx = find_min_idx r (fun idx -> delta ext.(idx) ext.(idx+1) stop) in
      ((delta ext.(bestIdx) ext.(bestIdx+1) stop), bestIdx)
    end

  (** Compute the minimal cost way to add a new stop to the solution *)
  let best_new_stop (x:t) (stop:int) 
      (demand:int) (cap:int) (dist:ArrayDist.t) (n:int) = 
    let cap_constraint i = 
      x.loads.(i) + demand <= cap
    in
    let delta_idx i = 
      let (delta, _) = new_stop x.tours.(i) stop dist n in
      delta
    in
    let maxIdx = (Array.length x.tours) -1  in
    let someIdx = find_min_filter_idx maxIdx delta_idx cap_constraint in
    match someIdx with
      | Some i -> 
	let (delta, bestIdx) = new_stop x.tours.(i) stop dist n in
	let new_tour = array_insert x.tours.(i) stop bestIdx in
	begin
	  let new_tours = Array.copy x.tours in
	  new_tours.(i) <- new_tour;
	  let new_loads = Array.copy x.loads in
	  new_loads.(i) <- x.loads.(i) + demand;
	  Some {tours = new_tours;
	        cost = x.cost +. delta;
	        loads = new_loads}
	end
      | None ->  None

  
  let two_opt (sol:t) (dist:ArrayDist.t) (n:int) = 
    let new_tours = Array.copy sol.tours in
    let delta_cost = ref 0.0 in
    begin
      Array.iteri (fun i x ->
        let old_cost = (tour_dist x dist n) in
        let (new_cost, new_tour) = two_opt n dist old_cost x in
        begin
          new_tours.(i) <- new_tour;
          delta_cost := !delta_cost +. new_cost -. old_cost;
        end 
      ) sol.tours ;
      { tours = new_tours; cost = !delta_cost +. sol.cost; loads = sol.loads }
    end
   
  let three_opt (sol:t) (dist:ArrayDist.t) (n:int) = 
    let new_tours = Array.copy sol.tours in
    let delta_cost = ref 0.0 in
    begin
      Array.iteri (fun i x ->
        let old_cost = (tour_dist x dist n) in
        let (new_cost1, x1) = Tsp_graph.two_opt n dist old_cost x in
        let (new_cost, new_tour) = 
          Tsp_graph.three_opt n dist new_cost1 x1 in
        begin
          new_tours.(i) <- new_tour;
          delta_cost := !delta_cost +. new_cost -. old_cost;
        end 
      ) sol.tours ;
      { tours = new_tours; cost = !delta_cost +. sol.cost; loads = sol.loads }
    end

  (** Move customer local search *)
  let mc_delta (x:t) v1 i1 v2 i2 (dist:ArrayDist.t) (n:int) = 
    (* Take i1 from v1 and insert it before i2 on v2 *)
    let u = x.tours.(v1).(i1) in 
    let u1 = if i1 > 0 then x.tours.(v1).(i1-1) else 0
    in
    let u2 = 
      if i1 < (Array.length x.tours.(v1)) -1 then 
        x.tours.(v1).(i1+1) 
      else 0
    in
    let vv1 =  
      if i2 = 0 then
        0
      else x.tours.(v2).(i2-1)
    in
    try 
      let vv2 = 
        if (Array.length x.tours.(v2))=0 then
          0 
        else x.tours.(v2).(i2) in
      (ArrayDist.get dist vv1 u n) +. (ArrayDist.get dist u vv2 n) -.
        (ArrayDist.get dist vv1 vv2 n) -.
        (ArrayDist.get dist u1 u n) -. (ArrayDist.get dist u u2 n) +.
        (ArrayDist.get dist u1 u2 n)
    with
        _ -> 
          (
            print_endline (to_string x);
          raise (InvalidVal ("catch it "^ 
                                (string_of_int v2) ^ " " ^
                                (string_of_int i2)))
          )
  let mc_test (x:t) v1 i1 v2 i2 demands cap = 
    let u = x.tours.(v1).(i1) in
    let d = demands.(u) in 
    (x.loads.(v2) + d <= cap)

  (* Soft constraint version *) 
  let mc_penalty(x:t) v1 i1 v2 i2 demands cap = 
    let penalty (load:int) = 
      if  load > cap then load - cap else 0 
    in
    let op = overload_penalty x cap in 
    let u = x.tours.(v1).(i1) in
    let p1 = penalty x.loads.(v1) in
    let d = demands.(u) in 
    let p1' = penalty (x.loads.(v1) - d) in
    let p2 = penalty x.loads.(v2) in
    let p2' = penalty (x.loads.(v2) + d) in
    op + p1' + p2' - p1 - p2 

  let mc_move (x:t)  v1 i1 v2 i2 dist n demands cap = 
    let delta = mc_delta x v1 i1 v2 i2 dist n in
    let n1 = Array.length x.tours.(v1) in
    let n2 = Array.length x.tours.(v2) in
    let customer = x.tours.(v1).(i1) in
    begin
      let new_loads = Array.copy x.loads in
      new_loads.(v1) <- x.loads.(v1) - demands.(customer);
      new_loads.(v2) <- x.loads.(v2) + demands.(customer);
      let new_tours = Array.copy x.tours in 
      let t1 = Array.create (n1-1) 0 in
      Array.blit x.tours.(v1) 0 t1 0 (i1);
      Array.blit x.tours.(v1) (i1+1) t1 (i1) (n1-1-i1);
      new_tours.(v1) <- t1;
      let t2 = Array.create (n2+1) 0 in
      Array.blit x.tours.(v2) 0 t2 0 (i2);
      t2.(i2) <- customer;
      Array.blit x.tours.(v2) (i2) t2 (i2+1) (n2 -i2);
      new_tours.(v2) <- t2;
      {tours = new_tours; cost = x.cost +. delta; loads = new_loads }
    end


  (** 
      Crossover local search
      
     Cross over is taking one vehicle [0 ..i .. n] and another
     vehicle [0 .. j .. m] and switch customers, it is either 
     forward, i.e., [0 .. i (j+1) ... m] and [0 .. j (i+1) .. n] or
     backward, i.e., [ 0 .. i j .. 0] [m .. (j+1) (i+1) .. n] 
  *)
  let crossover_delta_fwd (x:t) v1 i1 v2 i2 (dist:ArrayDist.t) (n:int) =
    (* Assume i1 < n-1  and i2 < m-1 *)
    let u1 = x.tours.(v1).(i1) in
    let u2 = x.tours.(v1).(i1+1) in
    let v1 = x.tours.(v2).(i2) in
    let v2 = x.tours.(v2).(i2+1) in
    (ArrayDist.get dist u1 v2 n) +. (ArrayDist.get dist u2 v1 n) -.
      (ArrayDist.get dist u1 u2 n) -. (ArrayDist.get dist v1 v2 n) 
      

  let crossover_loads_fwd (x:t) v1 i1 v2 i2 demands = 
    let sum_demands tour lo hi = 
      sub_array_fold_left (fun s e -> s + demands.(e)) 0 tour lo hi
    in
    let n1 = Array.length x.tours.(v1) in
    let dv11 = sum_demands x.tours.(v1) 0 (i1+1) in
    let dv12 = sum_demands x.tours.(v1) (i1+1) n1 in
    let n2 = Array.length x.tours.(v2) in
    let dv21 = sum_demands x.tours.(v2) 0 (i2+1) in
    let dv22 = sum_demands x.tours.(v2) (i2+1) n2 in
    ((dv11+dv22), (dv12+dv21))
 
  let crossover_constraint_fwd (x:t) v1 i1 v2 i2 demands cap =
    let (d1,d2) = crossover_loads_fwd x v1 i1 v2 i2 demands in
    (d1 <= cap && d2 <= cap)

  (* Soft constraint version *) 
  let crossover_penalty_fwd (x:t) v1 i1 v2 i2 demands cap = 
    let penalty (load:int) = 
      if  load > cap then load - cap else 0 
    in
    let op = overload_penalty x cap in
    let p1 = penalty x.loads.(v1) in
    let p2 = penalty x.loads.(v2) in 
    let (l1',l2') = crossover_loads_fwd x v1 i1 v2 i2 demands in
    let p1' = penalty l1' in
    let p2' = penalty l2' in
    op - p1 - p2 + p1' + p2'

  let crossover_move_fwd (x:t) v1 i1 v2 i2 dist n demands cap = 
    let delta = crossover_delta_fwd x v1 i1 v2 i2 dist n in
    let (d1, d2) = crossover_loads_fwd x v1 i1 v2 i2 demands in
    (* Upate the tours *)
    let n1 = Array.length x.tours.(v1) in
    let n2 = Array.length x.tours.(v2) in
    begin
      (* Printf.printf "Fwd swapping %d %d with %d %d\n" v1 i1 v2 i2; 
      print_endline ""; *)
      let new_loads = Array.copy x.loads in
      new_loads.(v1) <- d1;
      new_loads.(v2) <- d2;
      let new_tours = Array.copy x.tours in 
      let t1 = Array.create (i1 + n2 - i2) 0 in
      Array.blit x.tours.(v1) 0 t1 0 (i1 + 1); 
      Array.blit x.tours.(v2) (i2+1) t1 (i1+1) (n2-i2-1);
      new_tours.(v1) <- t1;
      let t2 = Array.create (i2 + n1 - i1) 0 in
      Array.blit x.tours.(v2) 0 t2 0 (i2 + 1); 
      Array.blit x.tours.(v1) (i1+1) t2 (i2+1) (n1-i1-1);
      new_tours.(v2) <- t2;
      {tours = new_tours; cost = x.cost +. delta; loads = new_loads }
    end

  (** Backward crossover operations  *)
  let crossover_delta_bwd (x:t) v1 i1 v2 i2 (dist:ArrayDist.t) (n:int) =
    (* Assume i1 < n-1  and i2 < m-1 *)
    try
      let u1 = x.tours.(v1).(i1) in
      let u2 = x.tours.(v1).(i1+1) in
      let w1 = x.tours.(v2).(i2) in
      let w2 = x.tours.(v2).(i2+1) in
      
      (ArrayDist.get dist u1 w1 n) +. (ArrayDist.get dist u2 w2 n) -.
        (ArrayDist.get dist u1 u2 n) -. (ArrayDist.get dist w1 w2 n)
    with
        _ -> raise (InvalidVal (" v1=" ^ (string_of_int v1) ^ 
                                   " v2=" ^(string_of_int v2) ^ 
                                   " i1="^ (string_of_int i1) ^ 
                                   " i2="^ (string_of_int i2) ^
                                   " n1=" ^ (string_of_int (Array.length x.tours.(v1))) ^
                                   " n2=" ^ (string_of_int (Array.length x.tours.(v2)))  ))
      
  let crossover_loads_bwd (x:t) v1 i1 v2 i2 demands = 
    let sum_demands tour lo hi = 
      sub_array_fold_left (fun s e -> s + demands.(e)) 0 tour lo hi
    in
    let n1 = Array.length x.tours.(v1) in
    let dv11 = sum_demands x.tours.(v1) 0 (i1+1) in
    let dv12 = sum_demands x.tours.(v1) (i1+1) n1 in
    let n2 = Array.length x.tours.(v2) in
    let dv21 = sum_demands x.tours.(v2) 0 (i2+1) in
    let dv22 = sum_demands x.tours.(v2) (i2+1) n2 in
    ((dv11+dv21), (dv12+dv22))
      
  let crossover_constraint_bwd (x:t) v1 i1 v2 i2 demands cap =
    let (d1,d2) = crossover_loads_bwd x v1 i1 v2 i2 demands in
    (d1 <= cap && d2 <= cap)

  (* Soft constraint version *) 
  let crossover_penalty_bwd (x:t) v1 i1 v2 i2 demands cap = 
    let penalty (load:int) = 
      if  load > cap then load - cap else 0 
    in
    let op = overload_penalty x cap in
    let p1 = penalty x.loads.(v1) in
    let p2 = penalty x.loads.(v2) in 
    let (l1',l2') = crossover_loads_bwd x v1 i1 v2 i2 demands in
    let p1' = penalty l1' in
    let p2' = penalty l2' in
    op - p1 - p2 + p1' + p2'


  let crossover_move_bwd (x:t) v1 i1 v2 i2 dist n demands cap = 
    let delta = crossover_delta_bwd x v1 i1 v2 i2 dist n in
    let (d1, d2) = crossover_loads_bwd x v1 i1 v2 i2 demands in
    (* Upate the tours *)
    let n1 = Array.length x.tours.(v1) in
    let n2 = Array.length x.tours.(v2) in
    begin
      (* Printf.printf "Bwd swapping %d %d with %d %d\n" v1 i1 v2 i2; *)
      let new_loads = Array.copy x.loads in
      new_loads.(v1) <- d1;
      new_loads.(v2) <- d2;
      let new_tours = Array.copy x.tours in 
      let t1 = Array.create (i1 + i2 +2) 0 in
      Array.blit x.tours.(v1) 0 t1 0 (i1 + 1); 
      Array.blit x.tours.(v2) (0) t1 (i1+1) (i2+1);
      reverse t1 (i1+1) (i1+i2+1);
      new_tours.(v1) <- t1;
      let t2 = Array.create (n1 + n2 - i1 -i2 -2) 0 in
      Array.blit x.tours.(v1) (i1+1) t2 0 (n1-i1-1);
      reverse t2 0 (n1-i1-2);
      Array.blit x.tours.(v2) (i2+1) t2 (n1-i1-1) (n2-i2-1);
      new_tours.(v2) <- t2;
      {tours = new_tours; cost = x.cost +. delta; loads = new_loads }
    end
      
end

let acceptable delta temp = (* SA accepting criteria *)
  if delta < -0.00001 then 
    true
  else 
    if  (temp > 0.00001) then
      let prob = exp ((-.delta) /. temp) in
      ((Random.float 1.0) < prob)
    else
      false
        
(* Improve the solution using local search *)
let improve ?timeout:(tout=0.5) sol dist n demands cap nV =
  let starttime = time () in
  let best_so_far = ref sol in
  let current = ref sol in
  let temp  = ref 2. in  (* Simulated annealing *)
  
  let find_moving (x:Sol.t) v i =
    let to_consider = Util.random_range 0 ((Array.length x.Sol.tours)-1) in
    let vIdx = ref 0 in
    let j = ref 0 in
    let stop = ref false in
    let result = ref None in
    begin
      while not(!stop) do
        let vI = to_consider.(!vIdx) in
        begin
          if (vI <> v) then
            if (Sol.mc_test x v i vI !j demands cap) &&
              (acceptable (Sol.mc_delta x v i vI !j dist n) !temp) then
              begin
                result := Some (vI, !j);
                stop := true
              end
            else  ()
          else ();
          (* Move indices forward *)
          if not(!stop) then
            begin
              (* print_endline ("j=" ^ (string_of_int !j)
                             ^ " vI=" ^ (string_of_int !vI) ) ; *)
              j := !j + 1;
              if !j > (Array.length x.Sol.tours.(vI) - 1) then
                (
                  j := 0;
                  vIdx := !vIdx + 1;
                )
              else () ;
              
              if !vIdx = (Array.length x.Sol.tours ) then 
                stop := true
              else ()
            end          
        end
      done;
      !result
    end    
  in
  let find_swapping (x:Sol.t) v i =
    let stop = ref false in
    let accepted = ref false in
    let to_consider = Util.random_range 0 ((Array.length x.Sol.tours)-1) in
    let vIdx = ref 0 in
    let j = ref 0 in
    let best_so_far = ref (0,0,false) in
    let best_delta = ref 0. in
    begin
      while not(!stop) do
        let vI = to_consider.(!vIdx) in
        begin
          if (vI <> v) && ((Array.length x.Sol.tours.(vI)) > 1) then
            (let delta = Sol.crossover_delta_fwd x v i vI !j dist n in
            if (acceptable delta !temp) &&
              (Sol.crossover_constraint_fwd x v i vI !j demands cap) then
              (* Found improving solution *)
              (
                (*Printf.printf "Fwd swap %d %d with %d %d " v i !vI !j ;
                Printf.printf "Improve by %2.2f" delta; 
                print_endline ""; *)
                stop := true;
                accepted := true;
                best_delta := delta;
                best_so_far := (vI, !j, true)
              )
            else 
              let delta = Sol.crossover_delta_bwd x v i vI !j dist n in
              if (acceptable delta !temp) &&
                 (Sol.crossover_constraint_bwd x v i vI !j demands cap) then
                 (* Found improving solution *)
                 (
                   (*Printf.printf "Bwd swap %d %d with %d %d " v i !vI !j ;
                   Printf.printf "Improve by %2.2f\n" delta; 
                   print_endline ""; *)
                   stop := true;
                   accepted := true;
                   best_delta := delta;
                   best_so_far := (vI, !j, false)
                 )
            )
          else ();

          (* Move indices forward *)
          if not(!stop) then
            begin
              j := !j + 1;
              if !j >= (Array.length x.Sol.tours.(vI) - 1) then
                (
                  j := 0;
                  vIdx := !vIdx + 1
                )
              else () ;
              
              if !vIdx = (Array.length x.Sol.tours ) then 
                stop := true
              else ()
            end
          else ()
        end
      done;
      if !accepted then
        Some !best_so_far
      else
        None
    end
  in
  begin
    let stop = ref false in 
    while not(!stop) do
      let to_consider = Util.random_range 0 
        (Array.length !current.Sol.tours -1) in
      let improved = ref false in
      for ci = 0 to (Array.length to_consider)-1 do
        let v = to_consider.(ci) in
        if (Array.length !current.Sol.tours.(v) > 1) then
          let i_to_consider = Util.random_range 0 ((Array.length !current.Sol.tours.(v)) -1) in
          let iIdx = ref 0 in
          let i = i_to_consider.(!iIdx) in
          while (not !improved) && (i <> v) && 
            (!iIdx < (Array.length !current.Sol.tours.(v))-1 ) do
            begin
            (
              let mOpt = find_moving !current v i in
              match mOpt with
                | Some (vI, j) -> 
                  begin
                    current := Sol.mc_move 
                      !current v i vI j dist n demands cap;
                        (* print_endline ("Move customer " ^ 
                              (string_of_float !current.Sol.cost) ^ 
                           " @ " ^ (string_of_float !temp)) ; *)
                    improved := true
                  end
                | None ->
                  let xOpt = find_swapping !current v i in
                  match xOpt with
                    | Some (vI, j, fwd) -> 
                      begin
                        current := 
                          if fwd then Sol.crossover_move_fwd 
                            !current v i vI j dist n demands cap 
                          else Sol.crossover_move_bwd 
                            !current v i vI j dist n demands cap
                        ;
                        improved := true
                  (* print_endline ("Swapping " ^ 
                     (string_of_int v) ^
                     ":" ^ (string_of_int !i) ^ " and " ^
                     (string_of_int vI) ^
                     ":" ^ (string_of_int !i) ^ "  " ^ 
                     (string_of_bool fwd) ^ " " ^
                     (string_of_float !current.Sol.cost) ^ 
                     " @ " ^ (string_of_float !temp) ) *)
                      end
                | None ->
                      begin
                        iIdx := !iIdx + 1
                      end
            );
              if !current.Sol.cost < !best_so_far.Sol.cost then
                (
                  best_so_far := !current
                )
              else (
              ) 
              ;
              
              if (!temp > 0.00001) && (!improved) then
              temp := 0.95 *. !temp
            else ();
            let now = time () in
            if now -. starttime > tout then
              ( print_endline "time out!";
              stop := true)
            else ()
          end
          done
        else ()
      done;
      if not(!improved) then
        stop := true (* Finished the for loop *)
      else ()
    done;
    !best_so_far
  end


(* Improve the solution using local search and soft constraints*)
let improve_soft ?timeout:(tout=0.5) sol dist n demands cap nV =
  let starttime = time () in
  let best_so_far = ref sol in
  let current = ref sol in
  let temp  = ref 2. in  (* Simulated annealing *)
  let multiplier = ref 1. in 
  let inc = 1. in 
  
  let find_moving (x:Sol.t) v i =
    let to_consider = Util.random_range 0 ((Array.length x.Sol.tours)-1) in
    let vIdx = ref 0 in
    let j = ref 0 in
    let stop = ref false in
    let result = ref None in
    begin
      while not(!stop) do
        let vI = to_consider.(!vIdx) in
        begin
          if (vI <> v) then
            let delta_obj = Sol.mc_delta x v i vI !j dist n in
            let current_penalty = (float_of_int (Sol.overload_penalty x cap)) *. 
              !multiplier in
            let new_penalty = (float_of_int (Sol.mc_penalty x v i vI !j demands cap)) *. 
              (!multiplier +. inc) in
            let delta_penalty = new_penalty -. current_penalty in
            if (acceptable (delta_obj +. delta_penalty) !temp) then
              begin
                result := Some (vI, !j);
                stop := true
              end
            else  ()
          else ();
          (* Move indices forward *)
          if not(!stop) then
            begin
              (* print_endline ("j=" ^ (string_of_int !j)
                             ^ " vI=" ^ (string_of_int !vI) ) ;*)
              j := !j + 1;
              if !j > (Array.length x.Sol.tours.(vI) - 1) then
                (
                  j := 0;
                  vIdx := !vIdx + 1;
                )
              else () ;
              
              if !vIdx = (Array.length x.Sol.tours ) then 
                stop := true
              else ()
            end          
        end
      done;
      !result
    end    
  in
  let find_swapping (x:Sol.t) v i =
    let stop = ref false in
    let accepted = ref false in
    let to_consider = Util.random_range 0 ((Array.length x.Sol.tours)-1) in
    let vIdx = ref 0 in
    let j = ref 0 in
    let best_so_far = ref (0,0,false) in
    begin
      while not(!stop) do
        let vI = to_consider.(!vIdx) in
        begin
          if (vI <> v) && ((Array.length x.Sol.tours.(vI)) > 1) then
            (let delta_obj = Sol.crossover_delta_fwd x v i vI !j dist n in
             let current_penalty = (float_of_int (Sol.overload_penalty x cap)) *. 
               !multiplier in
            let new_penalty = (float_of_int 
                                 (Sol.crossover_penalty_fwd x v i vI !j demands cap)) *. 
              (!multiplier +. inc) in
            let delta_penalty = new_penalty -. current_penalty in
            if (acceptable (delta_obj +. delta_penalty) !temp) then
              (* Found improving solution *)
              (*
                Printf.printf "Fwd swap %d %d with %d %d " v i vI !j ;
                Printf.printf "Improve by %2.2f : Penalty %2.2f" delta_obj new_penalty; 
                print_endline ""; *)
              (
                stop := true;
                accepted := true;
                best_so_far := (vI, !j, true)
              )
            else 
              let delta_obj = Sol.crossover_delta_bwd x v i vI !j dist n in
              let current_penalty = (float_of_int (Sol.overload_penalty x cap)) *. 
                !multiplier in
              let new_penalty = (float_of_int 
                                   (Sol.crossover_penalty_bwd x v i vI !j demands cap)) *. 
                (!multiplier +. inc) in
              let delta_penalty = new_penalty -. current_penalty in
              if (acceptable (delta_obj +. delta_penalty) !temp) then
                (* Found improving solution *)
                (
                  (* Printf.printf "Bwd swap %d %d with %d %d " v i vI !j ;
                     Printf.printf "Improve by %2.2f : Penalty %2.2f\n" delta_obj new_penalty; 
                     print_endline ""; *)
                  stop := true;
                  accepted := true;
                  best_so_far := (vI, !j, false)
                )
            )
          else ()
              ;

          (* Move indices forward *)
          if not(!stop) then
            begin
              j := !j + 1;
              if !j >= (Array.length x.Sol.tours.(vI) - 1) then
                (
                  j := 0;
                  vIdx := !vIdx + 1
                )
              else () ;
              
              if !vIdx = (Array.length x.Sol.tours ) then 
                stop := true
              else ()
            end
          else ()
        end
      done;
      if !accepted then
        Some !best_so_far
      else
        None
    end
  in
  begin
    let stop = ref false in 
    while not(!stop) do
      let to_consider = Util.random_range 0 
        (Array.length !current.Sol.tours -1) in
      let improved = ref false in
      for ci = 0 to (Array.length to_consider)-1 do
        let v = to_consider.(ci) in
        if (Array.length !current.Sol.tours.(v) > 1) then
          let i_to_consider = Util.random_range 0 
            ((Array.length !current.Sol.tours.(v)) -1) in
          let iIdx = ref 0 in
          let i = i_to_consider.(!iIdx) in
          while (not !improved) && (!iIdx < (Array.length i_to_consider)-1 ) do
            (
              (* Printf.printf "Consider %d of %d :%d of %d \n" ci (Array.length to_consider) !iIdx (Array.length i_to_consider); *)
              (let mOpt = find_moving !current v i in
              match mOpt with
                | Some (vI, j) -> 
                  begin
                    current := Sol.mc_move 
                      !current v i vI j dist n demands cap;
                    (* print_endline ("Move customer " ^ 
                                      (string_of_float !current.Sol.cost) ^ 
                                      " :" ^ (string_of_int (Sol.overload_penalty !current cap)) ^
                                      " @ " ^ (string_of_float !temp)) ; *)
                
                    improved := true
                  end
                | None -> 
                  let xOpt = find_swapping !current v i in
                  match xOpt with
                    | Some (vI, j, fwd) -> 
                      begin
                        current := 
                          if fwd then Sol.crossover_move_fwd 
                            !current v i vI j dist n demands cap 
                          else Sol.crossover_move_bwd 
                            !current v i vI j dist n demands cap
                        ;
                        (* print_endline ("Swapping " ^ 
                                          (string_of_int v) ^
                                          ":" ^ (string_of_int i) ^ " and " ^
                                          (string_of_int vI) ^
                                          ":" ^ (string_of_int j) ^ "  " ^ 
                                          (string_of_bool fwd) ^ " " ^
                                          (string_of_float !current.Sol.cost) ^ 
                                          " :" ^ (string_of_int (Sol.overload_penalty !current cap)) ^
                                          " @ " ^ (string_of_float !temp) ); *)
                        improved := true
                          
                      end
                    | None ->
                      iIdx := !iIdx + 1
              );
              if (!improved) then 
                (
                  if !current.Sol.cost < !best_so_far.Sol.cost then
                    (
                      (* Printf.printf "Real improvement %2.2f : %d !!!\n" 
                        !current.Sol.cost (Sol.overload_penalty !current cap);
                      print_string "" ;  *)
                      best_so_far := !current
                    )
                  else () 
                )
              else ()
              ;
              
              if !temp > 0.00001 then
                (
                  multiplier := !multiplier +. inc;
                  temp := 0.99 *. !temp
                )
              else ();
              
              
              let now = time () in
              if now -. starttime > tout then
                ( print_endline "time out!";
                  stop := true)
              else ()
            )
          done
        else ()
      done;
      if not(!improved) then
        (
          (* print_endline "No improvement possible"; *)
          stop := true (* Finished the for loop *)
        )
      else ()
    done;
    !best_so_far
  end
module ClarkeWrightHeuristic = struct
  (** Initialize a list of singleton arrays*)
  open Dist 
  open Util

  let init n =
    let r = ref [] in
    begin
      for i=1 to (n-1) do
        r := [|i|] :: !r
      done;
      !r
    end

  let check_capacity (tour:int array) (demands:int array) (cap:int) = 
    let total = Array.fold_left (fun s x ->
      s + demands.(x) ) 0 tour in
    total <= cap

  (** Merge two tours that end in p1 and begin with p2 *)
  let try_merge l p1 p2 s demands cap = 
    let t1o = find_opt (fun tour -> tour.(0) = p1) l in
    let t2o = find_opt (fun tour -> tour.((Array.length tour)-1) = p2) l in
    match (t1o, t2o) with
      | (Some t1, Some t2) -> 
        (* Check capacity constraints *)
        if t1 <> t2 then
          let combined = Array.append t2 t1 in
          if (check_capacity combined demands cap) then
            let remain = List.filter (fun x -> x <> t1 && x <> t2) l in
              Some (combined :: remain)
          else
            None
        else
          None
      | _ -> None
      
  let parallel lambda n nV cap demands xy = 
    let dist = create_dist n xy in
    let savings = ArrayDistSaving.compute_savings lambda dist n in
    let config = ref (init n) in
    let i = ref 0 in
    begin
      while (!i < Array.length savings) do
        let (p1,p2,s) = savings.(!i) in
        begin
          (let m1o = try_merge !config p1 p2 s demands cap in
           match m1o with
             | Some new_config -> 
               config := new_config
             | None -> 
               let m2o = try_merge !config p2 p1 s demands cap in
               match m2o with
                 | Some new_config ->
                   config := new_config
                 | None -> ()
          ); 
          i := !i +1
        end
      done;
      Sol.create (Array.of_list !config) dist n demands 
    end
end

let first_fit ?timeout:(tout=60.) n nV cap demands xy = 
  let dist = create_dist n xy in
  let d = Array.mapi (fun i x -> (i,x) ) demands in 
  let _ = Array.sort (
    fun x y -> Pervasives.compare (snd y) (snd x) ) d in
  let tours = Array.create nV [||] in
  let sol = ref (Sol.create tours dist n demands) in
  begin
    for i= 0 to n-2 do
      (* First fit *)
      let (stop, demand) = d.(i) in
      let sOpt = Sol.best_new_stop !sol stop demand cap dist n in
      match sOpt with
        | None -> raise (InvalidSol "Can't find a fit")
        | Some s -> sol := s
    done;
    let x = Sol.three_opt !sol dist n in
    let x = improve_soft ~timeout:tout x dist n demands cap nV in
    let x = Sol.three_opt x dist n in
    x
  end

let random_sol n nV demands dist = 
  let sets = Array.create nV [] in
  begin
    for i=1 to n-1 do
      let vi = Random.int nV in
      sets.(vi) <- i :: sets.(vi)
    done;
    let tours = Array.create nV [||] in
    for i=0 to nV-1 do
      tours.(i) <- Array.of_list sets.(i) ;
      Util.permute tours.(i)
    done;
    Sol.create tours dist n demands
  end

let random_fit ?timeout:(tout=6.0) n nV cap demands xy threshold = 
  let dist = create_dist n xy in
  let d = Array.mapi (fun i x -> (i,x) ) demands in 
  let _ = Util.permute d in
  let tours = Array.create nV [||] in
  let sol = ref (Sol.create tours dist n demands) in
  begin
    for i= 0 to n-1 do
      (* First fit *)
      begin
        let (stop, demand) = d.(i) in
        if stop > 0 then
          let sOpt = Sol.best_new_stop !sol stop demand cap dist n in
          match sOpt with
            | None -> raise (InvalidSol "Can't find a fit")
            | Some s -> 
              if s.Sol.cost > threshold then 
                raise (InvalidSol "Can't find a fit")
              else
                sol := s
        else () ;
      end
    done;
    
    let x = Sol.three_opt !sol dist n in
    let x = improve_soft ~timeout:tout x dist n demands cap nV in
    let x = Sol.three_opt x dist n in
    x
  end




(* VRP Model *)

exception InvalidSol of string

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

  let to_string (x:t) =
    let s = ref "" in
    begin
      for i = 0 to ((Array.length x.tours) -1) do
	s := !s ^ "\n" ^ (string_of_int_array x.tours.(i)) ^
	  ": " ^ (string_of_int x.loads.(i))
      done;
      "Sol {" ^ !s ^ "\n}\n" ^ (string_of_float x.cost) ^ "\n"
     end
      
      
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
    (ArrayDist.get dist u1 u2 n) +. (ArrayDist.get dist v1 v2 n) -.
      (ArrayDist.get dist u1 v2 n) -. (ArrayDist.get dist u2 v1 n)

  let crossover_loads_fwd (x:t) v1 i1 v2 i2 demands = 
    let sum_demands tour lo hi = 
      sub_array_fold_left (fun s e -> s + demands.(e)) 0 tour lo hi
    in
    let n1 = Array.length x.tours.(v1) in
    let dv11 = sum_demands x.tours.(v1) 0 (v1+1) in
    let dv12 = sum_demands x.tours.(v1) (v1+1) n1 in
    let n2 = Array.length x.tours.(v2) in
    let dv21 = sum_demands x.tours.(v2) 0 (v1+1) in
    let dv22 = sum_demands x.tours.(v2) (v2+1) n2 in
    ((dv11+dv22), (dv12+dv21))
 
  let crossover_constraint_fwd (x:t) v1 i1 v2 i2 demands cap =
    let (d1,d2) = crossover_loads_fwd x v1 i1 v2 i2 demands in
    (d1 <= cap && d2 <= cap)

  let crossover_move_fwd (x:t) v1 i1 v2 i2 dist n demands cap = 
    let delta = crossover_delta_fwd x v1 i1 v2 i2 dist n in
    let (d1, d2) = crossover_loads_fwd x v1 i1 v2 i2 demands in
    (* Upate the tours *)
    let n1 = Array.length x.tours.(v1) in
    let n2 = Array.length x.tours.(v2) in
    begin
      let new_loads = Array.copy x.loads in
      x.loads.(v1) <- d1;
      x.loads.(v2) <- d2;
      let new_tours = Array.copy x.tours in 
      let t1 = Array.create (v1 + n2 - v2) 0 in
      Array.blit x.tours.(v1) 0 t1 0 (v1 + 1); 
      Array.blit x.tours.(v2) (v2+1) t1 (v1+1) (n2-v2-1);
      x.tours.(v1) <- t1;
      let t2 = Array.create (v2 + n1 - v1) 0 in
      Array.blit x.tours.(v2) 0 t2 0 (v2 + 1); 
      Array.blit x.tours.(v2) (v1+1) t2 (v2+1) (n1-v1-1);
      {tours = new_tours; cost = x.cost +. delta; loads = new_loads }
    end

  (** Backward crossover operations  *)
  let crossover_delta_bwd (x:t) v1 i1 v2 i2 (dist:ArrayDist.t) (n:int) =
    (* Assume i1 < n-1  and i2 < m-1 *)
    let u1 = x.tours.(v1).(i1) in
    let u2 = x.tours.(v1).(i1+1) in
    let v1 = x.tours.(v2).(i2) in
    let v2 = x.tours.(v2).(i2+1) in
    (ArrayDist.get dist u1 u2 n) +. (ArrayDist.get dist v1 v2 n) -.
      (ArrayDist.get dist u1 v1 n) -. (ArrayDist.get dist u2 v2 n)
      
      

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
  let try_merge l p1 p2 demands cap = 
    let t1o = find_opt (fun tour -> tour.(0) = p1) l in
    let t2o = find_opt (fun tour -> tour.((Array.length tour)-1) = p2) l in
    match (t1o, t2o) with
      | (Some t1, Some t2) -> 
        (* Check capacity constraints *)
        if t1 <> t2 then
          let combined = Array.append t1 t2 in
          if (check_capacity combined demands cap) then
            let remain = List.filter (fun x -> x <> t1 && x <> t2) l in
              Some (combined :: remain)
          else
            None
        else
          None
      | _ -> None
      
  let parallel n nV cap demands xy = 
    let dist = create_dist n xy in
    let savings = ArrayDistSaving.compute_savings dist n in
    let config = ref (init n) in
    let stop = ref false in
    let i = ref 0 in
    begin
      while not(!stop) && (!i < Array.length savings) do
        let (p1,p2,saving) = savings.(!i) in
        if (saving > 0. && (List.length !config) <= nV) then
          stop := true
        else
          begin
            (let m1o = try_merge !config p1 p2 demands cap in
              match m1o with
                | Some new_config -> 
                  config := new_config
                | None -> 
                  let m2o = try_merge !config p2 p1 demands cap in
                  match m2o with
                    | Some new_config ->
                      config := new_config
                    | None -> ()
            ); 
            i := !i +1
          end
      done;
      let sol = Sol.create (Array.of_list !config) dist n demands in
      Sol.three_opt sol dist n
    end
end

let first_fit n nV cap demands xy = 
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
    Sol.three_opt !sol dist n
  end

let permute a = 
  begin
    Random.self_init () ;
    let n = Array.length a in
    for i = 0 to n -1 do
      let idx = Random.int (n-i) in
      if (idx > 0) then
        begin
          let tmp = a.(idx) in
          a.(idx) <- a.(i+idx) ;
          a.(i+idx) <- tmp;
        end 
    done;
  end

let random_fit n nV cap demands xy = 
  let dist = create_dist n xy in
  let d = Array.mapi (fun i x -> (i,x) ) demands in 
  let _ = permute d in
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
            | Some s -> sol := s
        else () ;
      end
    done;
    Sol.three_opt !sol dist n
  end

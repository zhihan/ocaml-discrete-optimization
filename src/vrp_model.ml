(* VRP Model *)

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

module Sol = struct

  open Dist
  open Util
    
  (** Every tour contains a list of positive integers and is
  interpreted as the tour 0->x0->x1->... ->xn->0 *)
  type t = {tours: int array array; cost: float; loads: int array}

  let compute_dist (x:int array array) (dist:ArrayDist.t) (n:int) = 
    (** Compute total distance of a tour *)
    let tour_dist (tour:int list) = 
      let rec loop (curr:int) (remain:int list) (acc:float) = 
        match remain with
          | [] -> acc +. (ArrayDist.get dist curr 0 n) (* Return to depot*)
          | h::tl -> 
            (* Go to next position *)
            let next = acc +. (ArrayDist.get dist curr h n) in
            loop h tl next
      in
      loop 0 tour 0.0 
    in
    Array.fold_left (fun s e -> s +. (tour_dist (Array.to_list e))) 0.0 x

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
      
  (** Compute the delta for adding a new stop i *)
  let new_stop (tour:int array) (i:int) (dist:ArrayDist.t) (n:int) : float*int = 
    let delta start finish mid = 
      (ArrayDist.get dist start mid n) +. (ArrayDist.get dist mid finish n) -.
        (ArrayDist.get dist start finish n)
    in
    let tn = Array.length tour in
    let ext = Array.make (tn+2) 0 in
    begin
      Array.blit tour 0 ext 1 tn;  (*copy x.tour to ext.(1)...ext(tn) *)
      let r = array_range 0 tn in
      let bestIdx = find_min_idx r (fun idx -> delta ext.(idx) ext.(idx+1) i) in
      ((delta ext.(bestIdx) ext.(bestIdx+1) i), bestIdx)
    end

end

open Dist 
open Util

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

let nearest_neighbor (n:int) (dist:ArrayDist.t) (start:int) = 
  let visited = BoolArray.empty n in
  let tour = Array.create n start in
  let cost = ref 0.0 in
  (*Embedded function visit *)
  let visit (i:int) =
    BoolArray.set visited i
  in

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
    
    
let print_result cost tour = 
  begin
    Printf.printf "%d %d\n" (int_of_float cost) 0;
    Array.iter (fun x-> 
      Printf.printf "%d " x) tour
  end

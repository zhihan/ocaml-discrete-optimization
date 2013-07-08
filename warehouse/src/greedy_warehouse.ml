(* A greedy heuristic *)

(* A special case is that any single warehouse is 
   sufficient to serve all customers, this is the case
   for the given problem set. *)
open Matrix
open Parse_input_warehouse
open Unix (* time function *)

exception Inconsistent of string

let total a = 
  Array.fold_left (fun s x -> s +.x) 0.0 a

module KWarehouse = struct
  type t = (int list) * (int array) * float

  let singleton i n v = 
    ([i], Array.create n i, v)

  let empty n = ([], Array.create n 0, 0.)

  let to_string x =  match x with 
    | (l,_,c) -> (string_of_float c) ^ 
      "{" ^ (List.fold_left (fun s x -> s ^ " " ^ (string_of_int x)) "" l) ^ "}"

  (* Suppose i is not open *)
  let new_open r i setupi trans = 
    match r with
      | (l, w, c) -> 
        begin
          let diff = ref 0. in
          let m = Array.length w in
          let neww = Array.copy w in
          for j= 0 to m-1 do
            let d = (Dense.get trans j i) -.
              (Dense.get trans j w.(j)) in
            if d < 0. then
              begin
                neww.(j) <- i; (* Reroute j to i *)
                diff := (!diff) +. d;
              end
            else ()
          done;
          ( i::l, neww, c +. !diff +. setupi)
        end

  let cost x = match x with
    | (_,_,c) -> c
    
  let which x = match x with
    | (_, w, _) -> w

  let opened x i = match x with
    | (a, _,_) -> List.exists (fun e -> e = i) a

  let setup_cost x (setup:float array) = match x with
    | (l, _, _) -> List.fold_left (fun s e -> s +. setup.(e)) 0.0 l

  let validate x setup trans = match x with
    | (l, w, c) ->
      let cost = setup_cost x setup in
      let trans_cost = 
        let tcost = ref 0.0 in
        begin 
          Array.iteri (fun i j -> 
            tcost := !tcost +. (Dense.get trans i j)
          ) w;
	  Printf.printf "tcost = %2.2f\n" !tcost;
          !tcost
        end
      in
      if (abs_float (cost +. trans_cost -. c)) > 1.0 then
        (
          List.iter (fun x -> Printf.printf "%d " x) l; 
          Array.iteri (fun i j -> 
            Printf.printf "%d ->> %d %2.2f\n " j i (Dense.get trans i j)
          ) w;
          Printf.printf "%2.2f + %2.2f - %2.2f\n" cost trans_cost c  ;
          raise (Inconsistent "Cost value is inconsistent" )
        )
      else ()

end

(* Trivially open only one warehouse *)
let one_open m n (cap:float array) setup demand trans = 
  let total_demand = total demand in 
  let first = ref true in
  let best = ref (KWarehouse.empty n) in
  begin
    for i = 0 to m-1 do
      let cost = setup.(i) in
      if cap.(i) >= total_demand then
        let total_cost = cost +. (Dense.sum_column trans i) in
        if (!first) then
          begin
            best := KWarehouse.singleton i n total_cost;
            first := false
          end
        else
          if (KWarehouse.cost !best ) > total_cost then
            best := KWarehouse.singleton i n total_cost
          else ()
    done;
    !best
  end


let greedy ?timeout:(tout=30) m n cap setup demand trans = 
  let best_one = one_open m n cap setup demand trans in
  let best_so_far = ref best_one in
  let starttime = time () in 

  let find_neighbor r = 
    let candidates = ref [] in
    begin
    for i = 0 to m-1 do
      if not(KWarehouse.opened r i) then 
          (* If it is less than the total bound *)
        if ((KWarehouse.setup_cost r setup) +. setup.(i) < 
          (KWarehouse.cost !best_so_far)) then
          let newr = KWarehouse.new_open r i setup.(i) trans in
            (* Local improving neighborhood *)
          if (KWarehouse.cost newr) < (KWarehouse.cost r) then
            (
              (* KWarehouse.validate newr setup trans; *)
              candidates := newr :: !candidates
            )
          else ()
        else() 
      else ()
    done;
      !candidates 
    end
  in

  let sort_neighbors n = 
    let result = Array.of_list n in
    let cmp x y = Pervasives.compare 
      (KWarehouse.cost x) (KWarehouse.cost y) in
    (
      Array.sort cmp result;
      result 
    )
  in

  let rec explore r = 
    begin
      if (KWarehouse.cost r) < (KWarehouse.cost !best_so_far) then
        begin
          Printf.printf "Best cost: %2.2f\n" (KWarehouse.cost r);
          Printf.printf "%!";
          best_so_far := r
        end
      else () ;
      let neighbors = find_neighbor r in
      let sorted = sort_neighbors neighbors in
      Array.iter (fun e-> 	
	let now = time () in 
	if now -. starttime > (float_of_int tout) then 
	  () 
	else
	  explore e) sorted
    end
  in
  begin
    explore !best_so_far;
    !best_so_far
  end

let write_out m n w cost = 
  let filename = "result.dat" in
  let oc = open_out filename in
   begin
    Printf.fprintf oc "%2.4f %d\n" cost 0; 
    Array.iter (fun x -> Printf.fprintf oc "%d " x) w; 
    close_out oc
  end
    
let _ = 
  if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (m,n, cap, setup, demand, trans) = process_input filename in
    let cap = Array.of_list cap in
    let setup = Array.of_list setup in
    let demand = Array.of_list demand in
    let trans = Dense.of_list n m trans in
    begin
      let best = greedy ~timeout:60 m n cap setup demand trans in
      KWarehouse.validate best setup trans;
      write_out m n (KWarehouse.which best) (KWarehouse.cost best);
      Printf.printf "Best so far %2.4f\n" (KWarehouse.cost best)
    end    



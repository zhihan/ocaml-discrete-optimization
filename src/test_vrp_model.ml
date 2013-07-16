(** A simple test for VRP model *)
open Vrp_model
open Dist
open Util

let test1 () = 
  let n = 5 in
  let x = [|0.;0.; 1.; 1.; 1.|] in
  let y = [|0.;1.;1.; -1.; 0.|] in
  let dist = ArrayDist.compute_edges x y in
  let demands = [|0;1;2;3;4|] in
  let sol = Sol.create [|[|1;2|];[|3;4|]|] dist n demands in
  begin
    Printf.printf "%s" (Sol.to_string sol);
    Printf.printf "Total distance is %2.2f\n" Sol.(sol.cost);
    let l = Sol.( sol.loads ) in
    Array.iter (fun x -> 
      Printf.printf "Total loads %d\n" x) l
  end

let test2 () = 
  let n = 5 in
  let x = [|0.;0.; 1.; 1.; 1.|] in
  let y = [|0.;1.;1.; -1.; 0.|] in
  let dist = ArrayDist.compute_edges x y in
  let tour = [|1;2|] in
  let (delta, bestIdx) = Sol.new_stop tour 3 dist n in
  begin
    Printf.printf "Best way to add 3 is %2.2f %d \n" delta bestIdx;
    let new_tour = array_insert tour 3 bestIdx in
    begin
      Array.iter (fun x -> Printf.printf "%d " x) new_tour ;
      Printf.printf "\n"
    end
  end

let test3 () = 
  let n = 5 in
  let xy = [(0., 0.); (0., 10.); (-10., 10.); (0., -10.); (10., -10.)] in
  let d = create_dist n xy in
  let _ = ArrayDistSaving.compute_savings d n in
  Printf.printf "Savings computed!\n" 
    
let _ = 
  begin
    test1 ();
    test2 ();
    test3 ()
  end
    

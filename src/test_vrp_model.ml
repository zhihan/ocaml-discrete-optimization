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
  let _ = ArrayDistSaving.compute_savings 1.0 d n in
  Printf.printf "Savings computed!\n" 

let test4 () = 
  let n = 5 in
  let xy = [(0., 0.); (0., 10.); (-10., 10.); (0., -10.); (10., -10.)] in
  let d = create_dist n xy in
  let demands = Array.of_list [0;1;1;1;1] in
  let cap = 1 in
  let tours = [| [|1;4|]; [|3;2|] |] in 
  let sol = Sol.create tours d n demands in
  let sol2 = Sol.crossover_move_fwd sol 0 0 1 0 d n demands cap in
  begin
    Printf.printf "Before %s\n" (Sol.to_string sol);
    Printf.printf "After %s\n" (Sol.to_string sol2);
    Printf.printf "New penalty %d\n" 
      (Sol.crossover_penalty_fwd sol 0 0 1 0 demands cap);
    Printf.printf "Penalty after move %d\n" (Sol.overload_penalty sol2 cap)
  end
let test5 () = 
  let n = 5 in
  let xy = [(0., 0.); (0., 10.); (-10., 10.); (0., -10.); (10., -10.)] in
  let d = create_dist n xy in
  let demands = Array.of_list [0;1;1;1;1] in
  let cap = 5 in
  let tours = [| [|1;4|]; [|3;2|] |] in 
  let sol = Sol.create tours d n demands in
  let sol2 = Sol.crossover_move_bwd sol 0 0 1 0 d n demands cap in
  begin
    Printf.printf "Before %s\n" (Sol.to_string sol);
    Printf.printf "After %s\n" (Sol.to_string sol2);
   
  end
     
let test6 () = 
  let n = 5 in
  let xy = [(0., 0.); (0., 10.); (-10., 10.); (0., -10.); (10., -10.)] in
  let d = create_dist n xy in
  let demands = Array.of_list [0;1;1;1;1] in
  let cap = 1 in
  let tours = [| [|1;4|]; [|3;2|] |] in 
  let sol = Sol.create tours d n demands in
  let sol2 = Sol.mc_move sol 0 0 1 0 d n demands cap in
  begin
    Printf.printf "Move a customer:\n";
    Printf.printf "Before %s\n" (Sol.to_string sol);
    Printf.printf "After %s\n" (Sol.to_string sol2);
    Printf.printf "New penalty %d\n" 
      (Sol.crossover_penalty_bwd sol 0 0 1 0 demands cap);
    Printf.printf "Penalty after move %d\n" (Sol.overload_penalty sol2 cap)
  end


 let test7 () = 
  let n = 5 in
  let xy = [(0., 0.); (0., 10.); (-10., 10.); (0., -10.); (10., -10.)] in
  let d = create_dist n xy in
  let demands = Array.of_list [0;1;2;1;2] in
  let cap = 1 in
  let tours = [| [|1;4|]; [|3;2|] |] in 
  let sol = Sol.create tours d n demands in
  let sol2 = Sol.mc_move sol 0 0 1 0 d n demands cap in
  begin
    print_endline (Sol.to_string sol);
    Printf.printf "Penalty %d\n" (Sol.overload_penalty sol cap);
    Printf.printf "New penalty %d\n" (Sol.mc_penalty sol 0 0 1 0 demands cap);
    Printf.printf "Penalty after move %d\n" (Sol.overload_penalty sol2 cap)
  end

 let test8 () =
   let _ = Random.self_init () in
   let n = 5 in
   let xy = [(0., 0.); (0., 10.); (-10., 10.); (0., -10.); (10., -10.)] in
   let dist = create_dist n xy in
   let demands = Array.of_list [0;1;2;1;2] in
   let cap = 1 in
   let nV = 2 in
   let sol = random_sol n nV demands dist in 
   begin
     Printf.printf "Random solution:";
     print_endline (Sol.to_string sol);
     Printf.printf "Penalty %d\n" (Sol.overload_penalty sol cap)
   end
    
let _ = 
  begin
    test1 ();
    test2 ();
    test3 ();
    test4 ();
    test5 ();
    test6 ();
    test7 ();
    test8 ()
  end
    

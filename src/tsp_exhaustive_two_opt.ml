open Tsp_parse_input
open Dist
open Tsp_graph


(* Simply parse a file *)
let _ = 
  if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (nV, coords) = process_input filename in
    let array_dist = create_dist nV coords in

    let bound = 20800.0 in
    let i = ref 0 in
    let stop = ref false in
    while (!i < nV) && not(!stop) do
      let start = !i in
      let (c, tour) = nearest_neighbor nV array_dist start in
      let (c, tour) = two_opt nV array_dist c tour in
      if c <= bound then
        begin
          print_result c tour;
          stop := true
        end
      else 
        (
          Printf.printf "Tried %d %2.4f \n " !i c;
          i := !i + 1;
        )
    done;
    

open Tsp_parse_input
open Dist
open Tsp_graph


(* Simply parse a file *)
let _ = 
  if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (nV, coords) = process_input filename in
    let array_dist = create_dist nV coords in
    let _ = Random.self_init () in
    let start = Random.int nV in
    let (c, tour) = nearest_neighbor nV array_dist start in
    print_result c tour



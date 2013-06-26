open Parse_input
open Dist
open Graph


(* Simply parse a file *)
let _ = 
  if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (nV, coords) = process_input filename in
    let t = Sys.time() in
    let array_dist = create_dist nV coords in
    let dt = Sys.time() -. t in
    Printf.printf "%d nodes, %2.4f seconds\n" nV dt

(** Solve the VRP problem using clarke and wright merge heuristic *)

open Vrp_model
open Vrp_parse_input
open Tsp_graph

let print_result sol nV = 
  let oc = open_out "result.dat" in
  begin
    Printf.fprintf oc "%2.2f %d\n" sol.Sol.cost 0 ; 
    Array.iter (fun x -> 
      begin
        Printf.fprintf oc "0 ";
        Array.iter (fun e ->
          Printf.fprintf oc "%d " e ) x;
        Printf.fprintf oc "0\n"
      end
    ) sol.Sol.tours ;
    for i = 1 to (nV - (Array.length sol.Sol.tours)) do
      Printf.fprintf oc "0 0\n"
    done;
    close_out oc
  end


let _ = 
if Array.length Sys.argv > 1 then
    let filename = Sys.argv.(1) in  
    let n,nV,cap, d, xy = process_input filename in
    let demands = Array.of_list d in
    let sol = first_fit ~timeout:20 n nV cap demands xy in
    begin
      Printf.printf "%s\n" (Sol.to_string sol);

      print_result sol nV;
    end


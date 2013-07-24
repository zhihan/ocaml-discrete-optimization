(** Solve the VRP problem using clarke and wright merge heuristic *)

open Vrp_model
open Vrp_parse_input
open Unix

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
    Random.self_init () ;
    let dist = create_dist n xy in
    let stop = ref false in
    let best_so_far = ref (ClarkeWrightHeuristic.parallel 1. n nV cap demands xy) in
    let current = ref (!best_so_far) in
    let starttime = time () in
    begin
      print_endline (" Best so far : " ^ (string_of_float !best_so_far.Sol.cost));
      while not(!stop) do 
        current := improve ~timeout:600. !current dist n demands cap nV;
        (* let x = Sol.three_opt x dist n in *)
        print_endline ("New result" ^ (string_of_float !current.Sol.cost));
        if (!current.Sol.cost < !best_so_far.Sol.cost) then
          (
            print_endline (" Best so far : " ^ (string_of_float !current.Sol.cost));
            best_so_far := !current
          )
            else 
          (
            print_endline (" Discard : " ^ (string_of_float !current.Sol.cost) ^ 
                              " Best :" ^  (string_of_float !best_so_far.Sol.cost));
          )
        ; 
        let now = time () in
        if now -. starttime > 36000. then
          stop := true
        else (); 
      done;
      Printf.printf "%s\n" (Sol.to_string !best_so_far);
      print_result !best_so_far nV
    end
      

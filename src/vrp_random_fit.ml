(** Solve the VRP problem using clarke and wright merge heuristic *)

open Vrp_model
open Vrp_parse_input
open Tsp_graph
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
    let stop = ref false in
    let best_so_far = ref (first_fit ~timeout:0.2 n nV cap demands xy) in
    let starttime = time () in
    begin
      while not(!stop) do
        let sol = 
          try Some (random_fit ~timeout:0.2 n nV cap demands xy)
          with _ -> None 
        in
        match sol with
          | Some x -> 
            if (x.Sol.cost < !best_so_far.Sol.cost) then
              (
                print_endline (" Best so far : " ^ (string_of_float x.Sol.cost));
                best_so_far := x
              )
            else () 
          |_ -> ()
            ;
        let now = time () in
        if now -. starttime > 1200. then
          stop := true
        else ()
      done;
      begin      
        Printf.printf "%s\n" (Sol.to_string !best_so_far);
        print_result !best_so_far nV;
      end
    end


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
    let best_so_far = ref (ClarkeWrightHeuristic.parallel 0.6 n nV cap demands xy) in
    let starttime = time () in
    begin
      print_endline (" Best so far : " ^ (string_of_float !best_so_far.Sol.cost));
        while not(!stop) do 
        Random.self_init ();
        
        let lambda = 0.5 +. (Random.float 2.) in
        let sol = ClarkeWrightHeuristic.parallel lambda n nV cap demands xy in
        if ((Array.length sol.Sol.tours) <= nV) then
          let x = sol in
          begin
          (* print_endline ("Initial " ^ (string_of_float x.Sol.cost)); *)
            let x2 = improve_soft ~timeout:10. x dist n demands cap nV in
            let x3 = Sol.three_opt x2 dist n in
            if (x3.Sol.cost < !best_so_far.Sol.cost) then
              (
                if (Sol.overload_penalty x3 cap) = 0 then
                  (
                    print_endline (" Best so far : " ^ (string_of_float x3.Sol.cost));
                    if (x3.Sol.cost < 540.) then
                      stop := true
                    else ()
                    ;
                    
                    best_so_far := x3
                  )
                else (
                  print_endline "Infeasible"
                )
              )
            else 
              (
            (* print_endline (" Discard : " ^ (string_of_float x3.Sol.cost) ^ 
               " Best :" ^  (string_of_float !best_so_far.Sol.cost)); *)
              );
          end;
          let now = time () in
          if now -. starttime > 3600. then
            stop := true
          else (); 
        else ();
      done; 
      Printf.printf "%s\n" (Sol.to_string !best_so_far);
      print_result !best_so_far nV
    end

      

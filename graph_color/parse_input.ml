open Graph

let _ = 
  if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    (print_endline "Start parsing...";
    let (n, e) = process_input filename in
    let g = create_adjacency n e in
    begin
      Printf.printf "Finished parsing %d\n" (Array.length g);
      Printf.printf "Start sorting\n";
      let a = sort_graph g in
      Printf.printf "Sort vertices %d.\n" a.(0) ;
 
      let x = initial_clique g a.(0) in
      Printf.printf "Initial clique %d\n" (List.length x);
      List.iter (fun e -> Printf.printf "%d " e) x;
      Printf.printf "\n";

      let x = max_clique ~debug:false g a.(0) in
      Printf.printf "Max clique %d\n" (List.length x);
      List.iter (fun e -> Printf.printf "%d " e) x;
      Printf.printf "\n"
    end
    )






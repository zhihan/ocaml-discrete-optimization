open Gc_graph

let _ = 
  if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (n, e) = process_input filename in
    let g = create_adjacency n e in
(*    let sorted = sort_graph g in
    let cliq = initial_clique g sorted.(0) in
    let k = List.length cliq in *)
    begin
      let (colors,nC) = Gc_color.greedy_color ~debug:false g in
      let stop =ref false in
      let i = ref 0 in
      begin
        while not(!stop) do
          let (colors, succeed) = Gc_color.refine ~debug:false g colors !i nC in
          if succeed then
            stop := true
          else 
            i := !i +1 
        done;
        Printf.printf "%d %d\n" nC 0;
        Array.iter (fun x -> Printf.printf "%d " x) colors
      end
    end






open Graph

let _ = 
  if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (n, e) = process_input filename in
    let g = create_adjacency n e in
    begin
      let (colors,nC) = Color.color ~debug:true ~start:16 g in
      let _ = Printf.printf "%d %d\n" n nC in 
      Array.iter (fun x -> Printf.printf "%d " x) colors 
    end






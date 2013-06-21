open Graph

let test1 () = 
  let filename = "./test.data" in
  let (nv,edges) = process_input filename in
  let g = create_adjacency nv edges in
  print_adjacency nv g

let _ = 
  begin
    test1 () 
  end

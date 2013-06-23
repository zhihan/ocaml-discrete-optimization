open Graph

let test1 () = 
  let filename = "./test.data" in
  let (nv,edges) = process_input filename in
  let g = create_adjacency nv edges in
  print_adjacency nv g 

let test2 () = 
  let a = [|1;2;3|] in
  let b = IntSet.from_array a in
  let c = IntSet.to_array b in
  begin
    Printf.printf "test 2\n";
    Array.iter (fun x -> Printf.printf "%d " x) c;
    Printf.printf "\n"
  end

let _ = 
  begin
    test1 ();
    test2 ()
  end

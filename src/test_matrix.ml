open Matrix

let simple_matrix () = 
  let a = Triplet.create 2 3 6 in
  let idx = ref 0 in
  begin
    for i = 1 to 2 do
      for j = 1 to 3 do
        Triplet.set a !idx i j 1.0;
        idx := !idx + 1
      done
    done;
    a
  end  

let test1 () = 
  let a = simple_matrix () in 
  print_endline (Triplet.to_string a)

let test_zip () = 
  let a = [|1;2;3|] in
  let b = [|1;2;3|] in
  let z = zip_array a b in
  Array.iter (fun (x,y) -> Printf.printf "%d %d\n" x y) z

let test_unzip () = 
  let a = [|1;2;3|] in
  let b = [|1;2;3|] in
  let z = zip_array a b in
  let c,d = unzip_array z in
  begin
    Array.iter (fun x -> Printf.printf "%d " x) c;
    Printf.printf "\n";
    Array.iter (fun x -> Printf.printf "%d " x) c;
    Printf.printf "\n"
  end

let test_normalize () = 
  let ai = [|1;1;2;2|] in
  let aj = [|2;1;1;2|] in
  let ar = [|1.;2.;3.;4.|] in
  let mat = Triplet.of_arrays 2 2 ai aj ar in
  begin
    Triplet.normalize mat;
    print_endline (Triplet.to_string mat)
  end
  
let test_horzcat () = 
  let a = simple_matrix () in
  let b = Triplet.horzcat a a in
  print_endline (Triplet.to_string b)

let test_vertcat () = 
  let a = simple_matrix () in
  let b = Triplet.vertcat a a in
  print_endline (Triplet.to_string b)


let test_dense () =
  let a = [[1. ;2.];[3.; 4.]] in
  let mat = Dense.of_list 2 2 a in
  let (x,y) = Dense.size mat in
  begin
    Printf.printf "%d by %d\n" x y ;
    print_endline (Dense.to_string mat)
  end

let test_dense_sum () =
  let a = [[1. ;2.];[3.; 4.]] in
  let mat = Dense.of_list 2 2 a in
  begin
    let a = Dense.sum_row mat 0 in
    Printf.printf "Sum of row 0 is %2.4f\n" a;
    let a = Dense.sum_column mat 0 in
    Printf.printf "Sum of column 0 is %2.4f\n" a;
   
  end


let _ = 
  begin
    test1 ();
    test_zip ();
    test_unzip ();
    test_normalize ();
    test_horzcat ();
    test_vertcat ();
    test_dense ();
    test_dense_sum ()
  end
    

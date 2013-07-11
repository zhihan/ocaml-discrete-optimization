open Dist

let test1 () = 
  let x = [| 0. ; 0. ; 1. ; 1. |] in
  let y = [| 0. ; 1. ; 1. ; 0. |] in
  let dist = ArrayDist.compute_edges x y in
  begin
    Printf.printf "%d to %d is %2.4f\n" 0 1 (ArrayDist.get dist 0 1 4);
    Printf.printf "%d to %d is %2.4f\n" 1 3 (ArrayDist.get dist 1 3 4);
    Printf.printf "%d to %d is %2.4f\n" 3 1 (ArrayDist.get dist 3 1 4);
    Printf.printf "%d to %d is %2.4f\n" 2 0 (ArrayDist.get dist 2 0 4);
  end

let test2 () = 
  let m = MappedDist.PairMap.create 2 in
  begin
    MappedDist.PairMap.add m (1,2) 1;
    let x = MappedDist.PairMap.find m (2,1) in
    Printf.printf "%d should be 1\n" x
  end

let main () = 
  begin
    test1 ();
    test2 ()
  end

let _ = main ()

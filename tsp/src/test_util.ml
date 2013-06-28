(* Unit test for the util file *)
open Util

let test_find_first () =
  let x = [| 1;2;3;4;5|] in
  let f = fun e -> e > 2 in
  let result = find_first x f in
  match result with
    | Some i -> 
      begin
        print_int i;
        print_endline (" should be " ^ (string_of_int 2))
      end
    | None -> print_endline "Wrong result"
    
let test_find_second () = 
  let x = [| 1;2;3;4;5|] in
  for i = 0 to (Array.length x) - 1 do
    let m = x.(i) in
    let f = find_second x (fun v -> v = m) in
    match f with
      | Some k -> Printf.printf "Find second %d at %d\n" m k
      | None -> ()
  done
  

let test_find_max () = 
  let x = [| 1;2;3;4;5|] in
  let f = fun (x:int) -> x in
  let i = find_max x f in
  Printf.printf "%d equals %d\n" i 4

let test_find_min () = 
  let x = [| 1;2;3;4;5|] in
  let f = fun (x:int) -> 8-x in
  let i = find_min x f in
  Printf.printf "%d equals %d\n" i 4

let test_find_min_filter () =
  let x = [| 1;2;3;4;5|] in
  let f = fun e -> (e mod 2) = 1 in
  let v = fun e -> float_of_int (8 - e) in
  let result = find_min_filter x v f in
  match result with
    | Some i -> 
      begin
        print_int i;
        print_endline (" should be " ^ (string_of_int 4))
      end
    | None -> print_endline "Wrong result"

let test_reverse () =
  let a = [|1;2;3;4;5|] in
  let b = [|1;2;5;4;3|] in
  begin
    reverse a 2 4;
    if  a <>  b then
      Printf.printf "wrong\n"
    else ()
  end

let main () = 
  begin
    test_find_first ();
    test_find_second ();
    test_find_max ();
    test_find_min ();
    test_find_min_filter ();
    test_reverse () 
  end
let _ = main ()

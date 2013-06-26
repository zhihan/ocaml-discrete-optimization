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

let main () = 
  begin
    test_find_first ();
    test_find_max ();
    test_find_min ();
    test_find_min_filter ()
  end
let _ = main ()

open My
open Item
open Num

let test1 () =
  let print_result w u r = 
    if (r < 0) then
      Printf.printf "Upper bound of %d is %s\n" w (string_of_num u)
    else
      Printf.printf "Solution of %d is %s\n" w (string_of_num u)
  in

  let a = {index=0; value=3; weight=1} in
  let b = {index=1; value=2; weight=1} in
  let c = {index=2; value=1; weight=2} in
  let all = [|b;c;a|] in

  begin
    Array.sort decUnitValue all;
    let u,r = upper all 0 3 in
    print_result 3 u r ;
    let (u,r) = upper all 0 4 in
    print_result 4 u r ;
  end

let test2 () = 
  let a = Selection.empty in
  let b = Selection.add_range 0 4 a in
  let c = Selection.to_list b in
  begin
    Printf.printf "Elements \n";
    List.iter (fun e -> Printf.printf "%d " e ) c ;
    Printf.printf "\n";
  end

let test3 () = 
  let a = {index=0; value=3; weight=1} in
  let b = {index=1; value=2; weight=1} in
  let c = {index=2; value=1; weight=2} in
  let all = [|b;c;a|] in
  begin
    Array.sort decUnitValue all;
    let res = Result.init all 4 in
    Printf.printf "Weight %d, Initial result: %s\n" 4 (Result.to_string res);
    let res = Result.init all 3 in
    Printf.printf "Weight %d, Initial result: %s\n" 3 (Result.to_string res);
    let res2 = Result.add_item all 0 res in 
    Printf.printf "Added. Result: %s\n" (Result.to_string res2);
    let res2 = Result.leave_item all 0 res in 
    Printf.printf "Left. Result: %s\n" (Result.to_string res2);
  end

let test4 () = 
  let a = {index=0; value=3; weight=1} in
  let b = {index=1; value=2; weight=1} in
  let c = {index=2; value=1; weight=2} in
  let all = [|b;c;a|] in
  begin
    Array.sort decUnitValue all;
    let res = bb_solve all 3 in
    begin
      Printf.printf "Solving for %d\n" 3;
      Printf.printf "Result %s\n" (Result.to_string res)
    end
  end

let test_input () = 
  let (a,b,c) = process_input 
    (Filename.concat (Sys.getcwd()) "ks_4_0") in
  begin
    Printf.printf "items %d, capacity %d\n" a b; 
    Printf.printf "Item: ";
    Array.iter (fun x -> 
      begin
        Printf.printf "%s\n" (Item.to_string x)
      end ) c ;
  end
  
 

let runtests () = 
  begin
    test1 ();
    test2 ();
    test3 ();
    test4 (); 
    test_input ();
  end

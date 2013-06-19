open My
open Item
open Num

let test1 () =
  let a = {index=1; value=3; weight=1} in
  let b = {index=2; value=2; weight=1} in
  let c = {index=3; value=1; weight=2} in
  let all = [|b;c;a|] in
  begin
    Array.sort decUnitValue all;
    let u = upper all 0 3 in
    Printf.printf "Upper bound of %d is %s\n"
      3 (string_of_num u);
    let u = upper all 0 4 in
    Printf.printf "Upper bound of %d is %s\n"
      4 (string_of_num u)
  end


let _ = 
  test1 () 

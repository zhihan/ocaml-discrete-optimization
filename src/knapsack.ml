open Knapsack_model
open Item

(* Main function for solving knapsack problem*)
let _ = 
  if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (n, cap, all) = process_input filename in
    begin
      Array.sort decUnitValue all;
      let res = bb_solve all cap in
      
      begin
        Printf.printf "%d 1\n" (Result.get_value res);
        let sel = Result.get_set all res in
        for i= 0 to n-1 do
          if Selection.mem i sel then  
            Printf.printf "1"
          else
            Printf.printf "0"
          ;
          if i < n-1 then Printf.printf " "
        done ;
      end
    end
      

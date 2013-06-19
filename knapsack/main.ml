open My
open Item
open Num

let _ = 
  if Array.length Sys.argv > 1 then
    let filename = Filename.concat (Sys.getcwd()) Sys.argv.(1) in
    let (n, cap, all) = process_input filename in
    begin
      Array.sort decUnitValue all;
      let res = bb_solve all cap in
      begin
        Printf.printf "Result:\n";
        Printf.printf "%s\n" (Result.to_string res)
      end
    end
      

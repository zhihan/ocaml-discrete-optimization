(* Process input file *)
exception WrongInput of string

let read_lines ic = 
  let lines = ref [] in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done ;
    !lines
  with End_of_file -> (List.rev !lines)
 
let process_input filename =  
  let process_first_line first_line = 
    let reg = Str.regexp "[ ]+" in
    let parts = Str.split reg first_line in
    match parts with 
      | [h] -> int_of_string h
      | _ -> raise (WrongInput "Fail to parse first line") 
  in
  let process_line line = 
     let reg = Str.regexp "[ ]+" in
     let parts = Str.split reg line in
     match parts with 
      | [h;t] -> (float_of_string h, float_of_string t)
      | _ -> raise (WrongInput "Fail to parse the line") 
  in  
  let ic = open_in filename in
  let lines = read_lines ic in
  let nV = process_first_line (List.hd lines) in
  let rec loop i remain acc = 
    match remain with
      | [] -> List.rev acc
      | line::tl ->
        let e = process_line line in
        let new_acc = e::acc in
        if i < (nV-1) then
          loop (i+1) tl new_acc
        else 
          List.rev new_acc
  in
  let coords = loop 0 (List.tl lines) [] in
  (nV, coords)

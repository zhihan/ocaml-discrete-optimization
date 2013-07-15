(* Process coursera input file *)
exception WrongInput of string

(* Read all the lines in a file *)
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
  (* Parse first line and return (n, nV, cap). *)
  let process_first_line first_line = 
    let reg = Str.regexp "[ ]+" in
    let parts = Str.split reg first_line in
    match parts with 
      | [m;n;k] -> (int_of_string m, int_of_string n, int_of_string k)
      | _ -> raise (WrongInput "Fail to parse the first line")
  in
  (* Process a line and return (demand, (x,y)) *)
  let process_line line =
     let reg = Str.regexp "[ ]+" in
     let parts = Str.split reg line in
     match parts with 
       | [d;x;y] -> (int_of_string d, 
                     (float_of_string x, float_of_string y))
      | _ -> raise (WrongInput "Fail to parse the line") 
  in
  
  let ic = open_in filename in
  let lines = read_lines ic in
  let (n,nV,cap) = process_first_line (List.hd lines) in
  let rec loop i remain acc_d acc_xy = 
    match remain with
      | [] -> (List.rev acc_d, List.rev acc_xy)
      | line::tl ->
        let (d, e) = process_line line in
        let new_d = d :: acc_d in
        let new_xy = e :: acc_xy in
        if i < (n-1) then
          loop (i+1) tl new_d new_xy
        else
          (List.rev new_d, List.rev new_xy)
            
  in
  let (d, xy) = loop 0 (List.tl lines) [] [] in
  (n, nV, cap, d, xy)

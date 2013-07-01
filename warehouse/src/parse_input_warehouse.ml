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
  (* |M|, |N|*)
  let process_first_line line = 
     let reg = Str.regexp "[ ]+" in
     let parts = Str.split reg line in
     match parts with 
      | [h;t] -> (int_of_string h, int_of_string t)
      | _ -> raise (WrongInput "Fail to parse the line") 
  in
  let process_lines n lines = 
      (* Cap, Setup *)
      let process_a_line line = 
      let reg = Str.regexp "[ ]+" in
      let parts = Str.split reg line in
      match parts with 
        | [h;t] -> (float_of_string h, float_of_string t)
        | _ -> raise (WrongInput "Fail to parse the line") 
      in
      let rec loop i remain c_acc s_acc = 
        if i < n then
          let (c,s) = process_a_line (List.hd remain) in
          loop (i+1) (List.tl remain) (c::c_acc) (s::s_acc)
        else
          ((List.rev c_acc), (List.rev s_acc), remain)
      in
      loop 0 lines [] []
  in
  (* Demand *)
  let process_consumer m n lines = 
    let process_demand first_line: float = 
      let reg = Str.regexp "[ ]+" in
      let parts = Str.split reg first_line in
      match parts with 
        | [h] -> float_of_string h
        | _ -> raise (WrongInput ("Fail to parse first line:" ^ first_line)) 
    in
    (* Process a long line of prices *)
    let process_long_line line: float list = 
      let reg = Str.regexp "[ ]+" in
      let parts = Str.split reg line in
      let rec loop i remain acc = 
        match remain with
          | [] -> List.rev acc
          | h::tl -> 
            let x = float_of_string h in
            let new_acc = x :: acc in
            if i < m then
              loop (i+1) tl new_acc
            else
              List.rev acc
      in
      loop 0 parts []
    in
    let rec loop j remain acc_d acc_t  = 
      if j < n then
        let d = process_demand (List.hd remain) in
        let remain = List.tl remain in
        let trans = process_long_line (List.hd remain) in
        let remain = List.tl remain in
        loop (j+1) remain (d::acc_d) (trans::acc_t) 
      else
        ((List.rev acc_d),(List.rev acc_t))
    in
    loop 0 lines [] []
  in
  (* Main function *)
  let ic = open_in filename in
  let lines = read_lines ic in
  let m,n = process_first_line (List.hd lines) in
  let lines = List.tl lines in
  let (cap, setup, lines) = process_lines m lines in
  let (demand, trans) = process_consumer m n lines in
  (m,n, cap, setup, demand, trans)

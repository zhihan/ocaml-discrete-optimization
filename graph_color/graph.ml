(* Simple graph with adjacency list *)
module Vertex = struct
  (* A vertex consists of an array of neighbors*)
  type t = int array
  let create (x:int array) = x
end

type graph = Vertex.t array
let adjacent (g:graph) (i:int) = g.(i)

let print_adjacency nv g = 
  begin
    Printf.printf "%d vertices:\n" nv ;
    for i = 0 to nv-1 do
      Printf.printf "%d:" i;
      Array.iter (fun x-> Printf.printf "%d " x) (adjacent g i);
      Printf.printf "\n"
    done
  end
      

(* Converting from edge list to adjacency list *)
let create_adjacency (nV:int) (edges: (int*int) list ) = 
  let adj = Array.create nV [| |] in
  begin
    for i=0 to (nV-1) do
      let outE = List.filter (fun (x,y) -> x=i) edges in
      let inE = List.filter (fun (x,y) -> y=i) edges in
      let adjacent = Array.create ((List.length outE) + (List.length inE)) 0 in
      let rec loop_out i remain = 
        match remain with 
          | [] -> ()
          | hd::tl -> 
            begin
              adjacent.(i) <- (snd hd);
              loop_out (i+1) tl
            end
      in
      let rec loop_in i remain = 
        match remain with 
          | [] -> ()
          | hd::tl -> 
            begin
              adjacent.(i) <- (fst hd);
              loop_out (i+1) tl
            end
      in
      begin
        loop_out 0 outE;
        loop_in (List.length outE) inE;
        adj.(i) <- adjacent
      end
    done;
    adj
  end

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
 
(* Create adjacency list from input *)
let process_input filename =  
  let process_first_line first_line = 
    let reg = Str.regexp "[ ]+" in
    let parts = Str.split reg first_line in
    match parts with 
      | [h;t] -> (int_of_string h, int_of_string t)
      | _ -> raise (WrongInput "Fail to parse first line") 
  in
  let process_line line = 
     let reg = Str.regexp "[ ]+" in
     let parts = Str.split reg line in
     match parts with 
      | [h;t] -> (int_of_string h, int_of_string t)
      | _ -> raise (WrongInput "Fail to parse the line") 
  in  
  let ic = open_in filename in
  let lines = read_lines ic in
  let (nV, nE) = process_first_line (List.hd lines) in
  let rec loop i remain acc = 
    match remain with
      | [] -> acc
      | line::tl ->
        let e = process_line line in
        let new_acc = e::acc in
        if i < (nE-1) then
          loop (i+1) tl new_acc
        else 
          new_acc
  in
  let edges = loop 0 (List.tl lines) [] in
  (nV, edges)


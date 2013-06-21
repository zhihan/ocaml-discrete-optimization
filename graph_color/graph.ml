(* Simple graph with adjacency list *)
module Vertex = struct
  (* A vertex consists of an array of neighbors*)
  type t = int array
  let create (x:int array) = x
  (* Vertex are compared by their degrees*)
  let compare x y = compare (Array.length x) (Array.length y)
end

type graph = Vertex.t array
let adjacent (g:graph) (i:int) = g.(i)
(* Get a range of integer and return in array *)
let array_range i j = 
  if (j < i) then 
    invalid_arg "Invalid argument" 
  else
    let r = Array.create (j-i+1) 0 in 
    begin
      for x = i to j do
        r.(x-i) <- x
      done;
      r
    end

(* Sort vertices by decremental order of degrees *)
let sort_graph (g:graph) : int array = 
  let idx = array_range 0 (Array.length g-1) in
  begin
    Array.sort (fun x y -> - (Vertex.compare g.(x) g.(y))) idx;
    idx
  end

let print_adjacency nv g = 
  begin
    Printf.printf "%d vertices:\n" nv ;
    for i = 0 to nv-1 do
      Printf.printf "%d:" i;
      Array.iter (fun x-> Printf.printf "%d " x) (adjacent g i);
      Printf.printf "\n"
    done
  end


(* Clique algorithm *)
module Int = struct
  type t = int
  let compare = Pervasives.compare 
end

module IntSet = struct
  include Set.Make(Int)

  (* Convert array to set *)
  let from_array (a: int array) = 
    Array.fold_left (fun s e -> add e s) empty a
end

let find_max_adj (g:graph) (s:IntSet.t) = 
  let idx = ref (IntSet.min_elt s) in
  begin
    IntSet.iter ( fun x ->
      if (Array.length g.(x)) > (Array.length g.(!idx)) then
        idx := x
      else
        ()
    ) s;
    !idx
  end

let initial_clique (g:graph) (start:int) =
  (* Greedy algorithm: always add first *)
  let rec loop (acc:int list) (candidates: IntSet.t) = 
    let _ = IntSet.iter (fun e -> Printf.printf " %d;" e) candidates in
    if (IntSet.is_empty candidates) then
      acc 
    else
      (Printf.printf "size %d\n" (IntSet.cardinal candidates);
      let next = find_max_adj g candidates in
      let _ = Printf.printf "pick %d" next in
      let next_adj = adjacent g next in
      let s = IntSet.from_array next_adj in
      (* Remove the existing ones from candidates *)
      let remain = List.fold_left 
        (fun ss e -> IntSet.remove e ss) s acc in  
      loop (next::acc) (IntSet.inter candidates remain)
      )
  in
  let init = [start] in
  let toprint = adjacent g start in
  let _ = Printf.printf "adjacent\n" in
  let _ = Array.iter (fun e -> Printf.printf "%d " e) toprint in
  let adj = IntSet.from_array (adjacent g start) in
  loop init adj

      
(* Converting from edge list to adjacency list *)
let create_adjacency (nV:int) (edges: (int*int) list ) = 
  let adj = Array.create nV [| |] in
  begin
    for i=0 to (nV-1) do
      let outE = List.filter (fun (x,y) -> x=i) edges in
      let inE = List.filter (fun (x,y) -> y=i) edges in
      let adjacent = Array.create ((List.length outE) + (List.length inE)) 0 in
      let rec loop_out j remain = 
        match remain with 
          | [] -> ()
          | hd::tl -> 
            begin
              Printf.printf "Out:Add (%d %d) to adj.%d\n" (fst hd) (snd hd) i;
              adjacent.(j) <- (snd hd);
              loop_out (j+1) tl
            end
      in
      let rec loop_in j remain = 
        match remain with 
          | [] -> ()
          | hd::tl -> 
            begin
              Printf.printf "In:Add %d to adj.%d\n" (fst hd) i;
              adjacent.(j) <- (fst hd);
              loop_in (j+1) tl
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


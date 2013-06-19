(* Solve the knapsack problem using OCaml *)

module Item = struct
  (* Uses Num module in nums library *)
  type t = {index: int; value: int; weight: int}

  let make (i:int) (v:int) (w:int) = 
    {index=i; value = v; weight = w}
  let to_string (v:t) = "{I:" ^ (string_of_int v.index) ^ "; " ^
    "V:" ^ (string_of_int v.value) ^"; " ^ 
    "W:" ^ (string_of_int v.weight) ^"}"

  let get_weight (v:t) = v.weight
  let get_value  (v:t) = v.value
  let get_index (v:t) = v.index

  (* Comparison functions *)
  let byValue (a:t) (b:t) = (a.value - b.value) 
  let byWeight (a:t) (b:t) = (a.weight - b.weight)

  (* Unit value *)
  let unitValue (x:t) = (float_of_int x.value) /. (float_of_int x.weight)
  let byUnitValue (a:t) (b:t) = 
    compare (unitValue a) (unitValue b)
  let decUnitValue (a:t) (b:t) = 
    - (compare (unitValue a) (unitValue b))

  (* Compute upper bound using relaxation *)
  let upper (x:t array) (start: int) (total_weight:int)  =
  (* Assume x is sorted by descreasing order of unit value *)
    let rec loop (current: int) (remain:int) (sum:int) = 
      if current < (Array.length x) && 
        (x.(current).weight <= remain) then
        (* Include the item and loop*)
        let next_remain = remain - x.(current).weight in
        let next_sum = sum + x.(current).value in
        loop (current+1) next_remain next_sum
      else 
        (* Return *)
        let s = float_of_int sum in
        if current < (Array.length x) then
          if remain == 0 then
            (* attanable solution *)
            (s, current)
          else
            (* bound *)
            let a = (float_of_int x.(current).value) *. 
              (float_of_int remain) /. (float_of_int x.(current).weight) in
            (s +. a, -1)
        else
          (s, current)
    in
    loop start total_weight 0
end

module Selection = struct
  module Index = struct
    type t = int
    let compare = Pervasives.compare
  end

  module IndexSet = Set.Make(Index)
  type t = IndexSet.t

  let empty = IndexSet.empty

  let add_range (l:int) (h:int) v = 
    let res = ref v in
    begin
      for i = l to h-1 do
        res := (IndexSet.add i !res)
      done ;
      !res
    end

  let add = IndexSet.add 
  let mem = IndexSet.mem
  let elements = IndexSet.elements

  let from_list (l:int list) = 
    List.fold_left (fun s e -> IndexSet.add e s) (IndexSet.empty) l

  let to_list (v:t) = 
    IndexSet.fold (fun e acc ->
      e :: acc
    ) v []

  let to_string (v:t) = 
    let a = IndexSet.fold (fun e acc -> 
      acc ^ (string_of_int e) ^ " " ) v "" in
    "{" ^ a ^ "}" 
end

module Result = struct
  type t = {sel:Selection.t;
            isFull: bool;
            remain: int;  (* Remaining room *)
            lowerBound: int;
            upperBound: float}
  
  let get_set (x: Item.t array) (v:t) = 
    let res = Selection.elements v.sel in
    let mapped = List.map (fun e -> Item.get_index x.(e)) res in
    Selection.from_list mapped

  let get_value (v:t) = v.lowerBound

  let init (x:Item.t array) (weight:int) = 
    let (u, r) = Item.upper x 0 weight in
    if (r<0) then
      {sel = Selection.empty; 
       isFull=false; 
       remain=weight; 
       lowerBound = 0;
       upperBound = u}
    else
      {sel = (Selection.add_range 0 r Selection.empty);
       isFull = true;
       remain = 0;
       lowerBound = int_of_float u;
       upperBound = u}

  let empty (weight:int) = 
     {sel = Selection.empty; isFull=true; remain=weight; 
      lowerBound = 0;
      upperBound = float_of_int 0}

  let is_full (v:t) = v.isFull
  let better_than (a:t) (b:t) = a.lowerBound > b.lowerBound
  let up_better_than (a:t) (b:t) = a.upperBound > b.upperBound
    

  let can_add (x:Item.t array) (i:int) (v:t) = 
    v.remain >= (Item.get_weight x.(i))

  (* Add the item to the partial result *)
  let add_item (x:Item.t array) (i:int) (v:t) :t = 
    let s = Selection.add i v.sel in
    let re = v.remain - (Item.get_weight x.(i)) in
    let value = float_of_int (v.lowerBound + Item.get_value x.(i)) in
    let (u, r) = Item.upper x (i+1) re in
    if (r < 0) then
      {sel=s; 
       isFull=false; 
       remain=re; 
       lowerBound = (int_of_float value);
       upperBound= (u +. value)}
    else
      (*Solution is found *)
      let s = Selection.add_range (i+1) r s in
      let newval = u +. value in
      (* (xxx) remain is not 0 *)
      {sel=s; 
       isFull=true; 
       remain=0; 
       lowerBound = int_of_float newval;
       upperBound = newval}

  (* Do not add the item for the partial result *)
  let leave_item (x:Item.t array) (i:int) (v:t) :t = 
    let s = v.sel in
    let re = v.remain in
    let (u, r) = Item.upper x (i+1) re in
    if (r < 0) then
      {sel=s; 
       isFull=false; 
       remain=re; 
       lowerBound = v.lowerBound;
       upperBound= u +. (float_of_int v.lowerBound)}
    else
      (*Solution is found *)
      let s = Selection.add_range (i+1) r s in
      (* (xxx) remain is not 0 *)
      {sel=s; 
       isFull=true; 
       remain=0; 
       lowerBound = (int_of_float u) + v.lowerBound ; 
       upperBound = u +. (float_of_int v.lowerBound)}

  let to_string v = 
    "{ sel: " ^ (Selection.to_string v.sel) ^ "; " ^
      "isFull:" ^ (string_of_bool v.isFull) ^ "; " ^
      "remain: " ^ (string_of_int v.remain) ^ "; " ^
      "lowerBound: " ^ (string_of_int v.lowerBound) ^ "; " ^
      "upperBound: " ^ (string_of_float v.upperBound) ^ " }"
end

type subproblem = Result.t * int * bool  
  
(* Main algorithm *)
let bb_solve (x:Item.t array) (weight:int) = 
 let best_so_far = ref (Result.empty weight) in
  let update_best (r:Result.t) = 
    if Result.better_than r !best_so_far then 
      best_so_far := r 
    else 
      ()
  in

  let stack = Stack.create () in
  let process_result (new_result:Result.t) (i:int) = 
    if (Result.is_full new_result ) then 
      update_best new_result
    else
      if not(Result.up_better_than !best_so_far new_result) then
        begin
          Stack.push (new_result, i, false) stack;
          Stack.push (new_result, i, true) stack
        end
      else 
        ()
  in
  begin
    let result = Result.init x weight in
    process_result result 0 ;
    while not(Stack.is_empty stack) do
      let (current, i, branch) = Stack.pop stack in
      if branch then 
        if Result.can_add x i current then
          let new_result = Result.add_item x i current in
          process_result new_result (i+1)
        else
          ()
      else
         let new_result = Result.leave_item x i current in
         process_result new_result (i+1)
    done ;
    !best_so_far
  end

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
    let items = int_of_string (List.hd parts) in
    let capacity = int_of_string (List.nth parts 1) in
    (items, capacity)
  in
  let ic = open_in filename in
  let lines = read_lines ic in
  let (n_items, capacity) = process_first_line (List.hd lines) in
  let default_item = Item.make 0 0 0 in
  let items = Array.make n_items default_item in
  let rec loop acc i = 
    match acc with
      | [] -> ()
      | line :: tl -> 
        let reg = Str.regexp "[ ]+" in
        let parts = Str.split reg (String.trim line) in
        let value = int_of_string (List.hd parts) in
        let weight = int_of_string (List.nth parts 1) in
        begin
          let item = Item.make i value weight in
          items.(i) <- item ;
          if i < (n_items -1) then
            loop tl (i+1)
          else
            ()
        end
  in
  begin
    loop (List.tl lines) 0;
    (n_items, capacity, items)
  end
  

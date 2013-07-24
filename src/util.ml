(* Some simple utilities used in local search *)

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

let permute a = 
  begin
    let n = Array.length a in
    for i = 0 to n -1 do
      let idx = Random.int (n-i) in
      if (idx > 0) then
        begin
          let tmp = a.(idx) in
          a.(idx) <- a.(i+idx) ;
          a.(i+idx) <- tmp;
        end 
    done;
  end

let random_range i j =
  let r = array_range i j in
  let _ = permute r in
  r

(* Search in an array *)
let find_first x pred : int option = 
  let idx = ref 0 in
  let stop = ref false in
  begin
    while not(!stop) && ((!idx) < (Array.length x)) do 
      if (pred x.(!idx) ) then
        stop := true
      else 
        idx := !idx + 1
    done;
    if (!stop) then
      Some !idx
    else
      None
  end

let find_second x pred: int option = 
  let idx = ref 0 in
  let first = ref false in
  let stop = ref false in
  begin
    while not(!stop) && ((!idx) < (Array.length x)) do 
      if (pred x.(!idx) ) then
        begin
          if not(!first) then
            first := true
          else
            stop := true ;
          idx := !idx + 1
        end
      else 
        idx := !idx + 1
    done;
    if (!stop) then
      Some !idx
    else
      None
  end
  

(* Find the max value in the array *)
let find_max x f : int = 
  (* imperative implementation *)
  let idx = ref 0 in
  let maxf = ref (f x.(0)) in
  begin
    for i = 1 to (Array.length x)-1 do
      if (f x.(i)) > !maxf then
        begin
          idx := i;
          maxf := f x.(i)
        end
      else ()
    done;
    !idx
  end

(* Find the min value in the array *)
let find_min x f : int = 
  (* imperative implementation *)
  let idx = ref 0 in
  let minf = ref (f x.(0)) in
  begin
    for i = 1 to (Array.length x)-1 do
      if (f x.(i)) < !minf then
        begin
          idx := i;
          minf := f x.(i)
        end
      else ()
    done;
    !idx
  end

(* Find the min value in the array *)
let find_min_idx x f : int = 
  (* imperative implementation *)
  let idx = ref 0 in
  let minf = ref (f 0) in
  begin
    for i = 1 to (Array.length x)-1 do
      if (f i) < !minf then
        begin
          idx := i;
          minf := f i
        end
      else ()
    done;
    !idx
  end

(* Find min v if f is true *)
let find_min_filter_idx (hi:int) (v:int->float) (f:int->bool) : int option = 
  let minf = ref 0.0 in
  let minI = ref 0 in
  let first = ref true in
  begin
    for i = 0 to hi do
      if (f i) then
        if (!first) then 
          (* First time, no need to check minf *)
          begin
            minf := v i;
            minI := i;
            first := false
          end
        else
          if (!minf) > (v i) then
            begin
              minf := v i;
              minI := i
            end
          else 
           ()
      else ()
    done;
    if not(!first) then
      Some !minI
    else
      None
  end

(* Find min v if f is true *)
let find_min_filter (type a) (x:a array) (v:a->float) (f:a->bool) : int option = 
  let minf = ref 0.0 in
  let minI = ref 0 in
  let first = ref true in
  begin
    for i = 0 to (Array.length x)-1 do
      if (f x.(i)) then
        if (!first) then 
          (* First time, no need to check minf *)
          begin
            minf := v x.(i);
            minI := i;
            first := false
          end
        else
          if (!minf) > (v x.(i)) then
            begin
              minf := v x.(i);
              minI := i
            end
          else 
           ()
      else ()
    done;
    if not(!first) then
      Some !minI
    else
      None
  end
 
(** Reverse the elements in an array *) 
let reverse a startIdx endIdx =
  let sub = Array.sub a startIdx (endIdx-startIdx+1) in
  let l = Array.to_list sub in
  let lv = List.rev l in
  let rec loop i remain = 
    match remain with
      | [] -> ()
      | h :: tl -> 
        begin
          a.(i) <- h;
          loop (i+1) tl
        end
  in
  loop (startIdx) lv 

let array_insert (tour:int array) (i:int) (offset:int) = 
  let tn = Array.length tour in
  let new_tour = Array.make (tn+1) 0 in
  begin
    Array.blit tour 0 new_tour 0 (offset);
    new_tour.(offset) <- i;
    Array.blit tour offset new_tour (offset+1) (tn-offset);
    new_tour 
  end


let string_of_int_array (x:int array) = 
  Array.fold_left (fun s x -> s ^ " " ^ (string_of_int x )) "" x 

(** An alternate implementation of List.find, avoid using exception *)
let find_opt pred l  = 
  let rec loop remain pred = 
    match remain with
      | [] -> None
      | h::tl -> if (pred h) then Some h else loop tl pred
  in
  loop l pred


let sub_array_fold_left f s0 x (s:int) (e:int) = 
  let rec loop i acc = 
    if (i < e) then
      let next = f acc x.(i) in
      loop (i+1) next
    else acc
  in
  loop s s0 

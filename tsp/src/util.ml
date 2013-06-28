(* Some simple utilities *)

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

(* Find min v if f is true *)
let find_min_filter x (v:int->float) (f:int->bool) : int option = 
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

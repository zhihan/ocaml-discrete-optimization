(* Basic matrix computations *) 

let zip_list a b = 
  let rec loop x y acc = 
    match (x,y) with
      | ([], []) -> List.rev acc
      | (hx::tx, hy::ty) -> loop tx ty ((hx,hy)::acc)
      | ([], _ ) | (_, []) -> invalid_arg "Input size mismatch"
  in
  loop a b []

let zip_array a b =
  let al = Array.to_list a in
  let bl = Array.to_list b in
  let zipped = zip_list al bl in
  Array.of_list zipped

let unzip_array a = 
  let e = a.(0) in
  let n = Array.length a in
  let a1 = fst e in
  let a2 = snd e in
  let r1 = Array.create n a1 in
  let r2 = Array.create n a2 in
  begin
    for i = 1 to n - 1 do
      r1.(i) <- fst a.(i) ;
      r2.(i) <- snd a.(i)
    done;
    (r1, r2)
  end

let array_to_string (d: float array) = 
  Array.fold_left (fun s e -> s ^ " " ^ (string_of_float e)) "" d

(* Shift indices by k *)
let shifted (idx:int array) (k:int) = 
  let result = Array.copy idx in
  begin
    for i = 0 to (Array.length idx) -1 do
      result.(i) <- result.(i) + k
    done;
    result
  end

    
module Triplet = struct
  (* 1-based matrix for use with glpk *)
  type t = int * int * int array* int array * float array

  let size mat = match mat with 
    | (m,n,_,_,_) -> (m,n)
      
  let nnz mat = match mat with
    | (_,_,ai,_,_) -> Array.length ai

  let row_index mat = match mat with
    | (_,_,ai,_,_) -> ai

  let col_index mat = match mat with
    | (_,_,_,aj,_) -> aj

  let value mat = match mat with
    | (_,_,_,_,ar) -> ar


  let set mat idx i j v = match mat with
    | (_,_,ai,aj,ar) -> 
      begin
        ai.(idx) <- i;
        aj.(idx) <- j;
        ar.(idx) <- v
      end

  let of_arrays m n ai aj ar = 
    (m,n,ai, aj,ar)
  
  let create m n numel = 
    let ai = Array.create numel 0 in
    let aj = Array.create numel 0 in
    let ar = Array.create numel 0.0 in
    (m, n, ai, aj, ar)

  let to_string (mat:t) = 
    let a_row i j v = 
      "(" ^ (string_of_int i) ^"," ^ 
        (string_of_int j) ^ ") " ^ (string_of_float v) ^"\n"
    in
    match mat with 
      | (m,n,ai,aj,ar) -> 
        let s = ref (
          (string_of_int m)^ 
            "x"^ (string_of_int n) ^" matrix:\n") in
        begin
          for i = 0 to (nnz mat)-1 do
            s := !s ^ (a_row ai.(i) aj.(i) ar.(i)) 
          done;
          !s
        end

  (* Index normalization to row major storage *)
  let normalize (mat:t) = 
    let comp x y = 
      match (x,y) with
        | (((ix,jx),_), ((iy,jy),_)) -> 
          if (ix < iy) || (ix = iy && jx < jy) then
            -1 
          else if (ix = iy) && (jx = jy) then
            0
          else
            1
    in
    match mat with
      | (_,_, ai, aj, ar) ->
        let coord = zip_array ai aj in
        let l = zip_array coord ar in
        begin
          Array.sort comp l;
          let sorted_coord, sorted_ar = unzip_array l in
          let sorted_ai, sorted_aj = unzip_array sorted_coord in
          (* Modify ai aj r in place *)
          for i = 0 to (nnz mat)-1 do
            ai.(i) <- sorted_ai.(i);
            aj.(i) <- sorted_aj.(i);
            ar.(i) <- sorted_ar.(i)
          done
        end

  (* Simple matrix operations *)
  let horzcat (a:t) (b:t) = 
    match (a,b) with
      | ((am,an,ai,aj,ar),(bm,bn,bi,bj,br)) ->
        if (am <> bm) then
          invalid_arg "Row size not match"
        else
          begin
            let mat = (am, an+bn, 
                       Array.append ai bi,
                       Array.append aj (shifted bj an),
                       Array.append ar br) in
            normalize mat;
            mat
          end

  let vertcat (a:t) (b:t) = 
    match (a,b) with
      | ((am,an,ai,aj,ar),(bm,bn,bi,bj,br)) ->
        if (an <> bn) then
          invalid_arg "Column size not match"
        else
          begin
            let mat = (am + bm, an, 
                       Array.append ai (shifted bi am),
                       Array.append aj bj,
                       Array.append ar br) in
            normalize mat;
            mat
          end
            
end

(* A very simple dense matrix implementation *)
module Dense = struct
  type t = int * int * float array

  let  size mat = match mat with
    | (m,n,_) -> (m,n)
      
  let of_list m n l = 
    let data = Array.create (m*n) 0. in
    let offset = ref 0 in
    let rec add_entry remain =
      match remain with
        | h::tl -> 
          begin
            data.(!offset) <- h;
            offset := !offset + 1;
            add_entry tl
          end
        | [] -> ()
    in
    let rec add_row remain = 
      match remain with
        | h::tl -> 
          begin
            add_entry h;
            add_row tl
          end
        | [] -> ()
    in
    begin
      add_row l;
      (m, n, data)
    end

  let to_string mat = 
    let a_row m n i data = 
      let s = ref "[" in
      begin
        for j=0 to n-1 do
          s := !s ^ (string_of_float data.(i*n + j)) ^ " ";
        done;
        !s ^ "]\n"
      end
    in
    match mat with 
      | (m,n,data) ->
        let s = ref "" in
        begin
          for i=0 to m-1 do
            s := !s ^ (a_row m n i data)
          done;
          !s
        end
          
  let get mat i j = 
    match mat with 
      | (m,n,data) -> data.(i*n + j)

  let sum_column mat j = 
    let (m,_) = size mat in
    let sum = ref 0. in
    begin
      for i = 0 to m-1 do
        sum := !sum +. (get mat i j)
      done;
      !sum
    end

  let sum_row mat i = 
    let (_, n) = size mat in
    let sum = ref 0. in
    begin
      for j = 0 to n-1 do
        sum := !sum +. (get mat i j)
      done;
      !sum
    end

end

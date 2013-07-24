(** Dist.ml implements distance data structure. It computes distances between
 2-D points and store in a n*(n-1)/2 array. *)

(* A simple map that maps (i,j) and (j,i) to the distance *)
module MappedDist = struct
  
  (* Unordered pair (i,j) *)
  module UnorderedPair = struct
    type t = int*int

    (* Equal should not distinguish the items in the tuple *)
    let equal (x:t) (y:t) = match (x,y) with
      | ((xi,xj),(yi,yj)) -> (xi = yi && xj = yj) || (xi = yj && xj = yi)

    (* Compute hash value for (i<j) *)
    let hash (i,j) = 
      if i < j then Hashtbl.hash (i,j) else Hashtbl.hash (j,i)
  end

  module PairMap = Hashtbl.Make(UnorderedPair)

  type t = float PairMap.t 

  let compute_dist x y = 
    sqrt( ((fst x) -. (fst y)) *. ((fst x) -. (fst y))  +. 
           ((snd x) -. (snd y)) *. ((snd x) -. (snd y)))
      

  (* Delegate methods to PairMap*)
  let mem = PairMap.mem
  let add = PairMap.add 
end

(* Store distance in an array. To save space we avoid storing symmetric
   entries. All distances are stored in a single array. *)
module ArrayDist = struct
  type t = float array
  exception WrongIndex of string 
  (* Assume the dist matrix is [ a[00] a[01] a[02], ... a[20] a[21] a[22]],
     We only need to store the entries [ a[01] a[02] a[12] ] in an array. 
     
     Function edge_idx calculate the index into the storing array. 
  *) 
  let edge_idx i j n = 
    if i < j then
      (n-1 + n-i ) *i /2 + (j-i) -1
    else
      (n-1 + n-j) * j/ 2 + (i-j) -1

  (* Given array of x and y locations, compute the distances *)
  let compute_edges x y = 
    let n = Array.length x in 
    let nE = n * (n - 1) / 2 in
    let c = Array.create nE 0. in
    (
      for i = 0 to (Array.length x) -2 do
        for j = i+1 to (Array.length x) -1 do
          c.(edge_idx i j n) <- sqrt( 
            (x.(i) -. x.(j)) *. (x.(i) -. x.(j)) +. 
              (y.(i) -. y.(j)) *. (y.(i) -. y.(j))) 
        done
      done;
      c
    )

  let get d i j n = 
    if i <> j then 
      let eIdx = edge_idx i j n in
      if (eIdx < n*(n-1)/2) && (eIdx>=0) then
        try 
          d.(eIdx)
        with
            Invalid_argument e -> raise (WrongIndex (
          (string_of_int i) ^ " " ^
            (string_of_int j) ^ " of " ^
            (string_of_int n)))
      else
        raise (WrongIndex (
          (string_of_int i) ^ " " ^
            (string_of_int j) ^ " of " ^
            (string_of_int n)))
    else 0.0
  
  let getE d (e:int*int) n = 
    get d (fst e) (snd e) n

end

(* Create the distance data structure  *)
let create_dist nVert (coords: (float*float) list) = 
  let x = Array.create nVert 0. in
  let y = Array.create nVert 0. in
  let rec loop i remain  = 
    match remain with
      | [] -> ()
      | h::tl -> 
        begin
          x.(i) <- fst h;
          y.(i) <- snd h;
          loop (i+1) tl
        end 
  in
  begin
    loop 0 coords;
    ArrayDist.compute_edges x y
  end


module ArrayDistSaving = struct
  type t = (int*int* float) array
      
  let compute_savings (lambda:float) (dist:ArrayDist.t) (n:int) = 
    let m = (n-1)*(n-2)/2 in
    let savings = Array.create m (0,0, 0.0) in
    let offset = ref 0 in 
    begin
      for i=1 to (n-1) do
        for j = (i+1) to (n-1) do
          let saving =  lambda *. (ArrayDist.get dist i j n) -. 
            (ArrayDist.get dist i 0 n) -. 
            (ArrayDist.get dist j 0 n) in
          savings.(!offset) <- (i,j, saving);
          offset := !offset + 1
        done
      done;
      Array.sort (fun x y ->
        match (x,y) with ((_,_,sx),(_,_,sy)) -> Pervasives.compare sx sy ) savings;
      savings
    end

end

(* Array of Boolean flags, serves as bit vectors *)
module BoolArray = struct
  type t = bool array
      
  let empty n = Array.create n false
  let mem t i = t.(i)
  let set t i = t.(i) <- true
end

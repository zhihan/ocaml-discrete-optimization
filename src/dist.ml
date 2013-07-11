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
    if i!= j then 
      d.(edge_idx i j n)
    else 0.0
  
  let getE d (e:int*int) n = 
    get d (fst e) (snd e) n

end

(* Array of Boolean flags, serves as bit vectors *)
module BoolArray = struct
  type t = bool array
      
  let empty n = Array.create n false
  let mem t i = t.(i)
  let set t i = t.(i) <- true
end
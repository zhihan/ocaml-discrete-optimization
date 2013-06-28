(* A simple map that maps (i,j) to dist *)
module MappedDist = struct
  module IntPair = struct
    type t = int * int
    let compare (x:t) (y:t) = 
      if (fst x) < (fst y) then
        -1 
      else if (fst x) > (fst y) then
        1
      else
        Pervasives.compare (snd x) (snd y)
  end
 
  module PairMap = Map.Make(IntPair)

  type t = float PairMap.t 

  let pair i j = 
    if i<j then (i,j) else (j,i)

  let compute_dist x y = 
    sqrt( ((fst x) -. (fst y)) *. ((fst x) -. (fst y))  +. 
           ((snd x) -. (snd y)) *. ((snd x) -. (snd y)))
      

  (* Delegate methods to PairMap*)
  let mem = PairMap.mem
  let add = PairMap.add 
end


module ArrayDist = struct
  type t = float array

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

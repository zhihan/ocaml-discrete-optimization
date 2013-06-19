(* Solve the knapsack problem using OCaml *)

module Item = struct
  (* Uses Num module in nums library *)
  open Num

  type t = {index: int; value: int; weight: int}

  (* Comparison functions *)
  let byValue (a:t) (b:t) = (a.value - b.value) 
  let byWeight (a:t) (b:t) = (a.weight - b.weight)

  (* Unit value *)
  let unitValue (x:t) = (num_of_int x.value) // (num_of_int x.weight)
  let byUnitValue (a:t) (b:t) = 
    compare_num (unitValue a) (unitValue b)
  let decUnitValue (a:t) (b:t) = 
    - (compare_num (unitValue a) (unitValue b))

  (* Compute upper bound using relaxation *)
  let upper (x:t array) (start: int) (total_weight:int)  =
  (* Assume x is sorted by descreasing order of unit value *)
    let rec loop (current: int) (remain:int) (sum:int) : num = 
      if current < (Array.length x) && 
        (x.(current).weight <= remain) then
        (* Include the item and loop*)
        let next_remain = remain - x.(current).weight in
        let next_sum = sum + x.(current).value in
        loop (current+1) next_remain next_sum
      else 
        (* Return *)
        let s = num_of_int sum in
        if current < (Array.length x) then
          let a = (num_of_int x.(current).value) */ 
          (num_of_int remain) // (num_of_int x.(current).weight) in
          s +/ a
        else
          s
    in
    loop start total_weight 0

end


  
  
  

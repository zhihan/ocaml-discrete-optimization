open Graph

let debugging = ref false 

module Colors = struct
  type t = int array
  (* Uncolored items has number -1 *)
  let create (n:int) = Array.create n (-1) 

  let uncolor (colors:t) (k:int) = colors.(k) <- -1

  let is_colored (colors:t) (k:int) = colors.(k) > -1

  let get_color (colors:t) (k:int) = colors.(k)

  let get_colors (colors:t) (a: int array) = 
    let init = IntSet.empty in 
    Array.fold_left 
      (fun s e -> IntSet.add (get_color colors e) s) init a  
  let to_string colors = 
    "[" ^ 
      (Array.fold_left 
      (fun s e -> s ^ " " ^ (string_of_int e)) "" colors) ^ "]"
  let color (colors:t) (k:int) (c:int) = 
    begin
      colors.(k) <- c;
      if !debugging then
	Printf.printf "Color %d to %d: %s \n" k c  (to_string colors)

    end
end

module ColorDegree = struct
  type t = IntSet.t array

  let add (cd:t) (colors:Colors.t) (g:graph) (v:int) =
    let adj = adjacent g v in
    let c = Colors.get_color colors v in
    Array.iter (fun e ->
      cd.(e) <- (IntSet.add c cd.(e))) adj
  
  let update (cd:t) (colors:Colors.t) (g:graph) (v:int) = 
    let adj = adjacent g v in
    Array.iter (fun e ->
      let aa = adjacent g e in
      let newVal = Array.fold_left (fun s x -> 
	let c = Colors.get_color colors x in
	if (c >=0 ) then
	  IntSet.add c s 
	else s ) IntSet.empty aa in
      cd.(e) <- newVal ) adj

  let create (n:int) = Array.create n IntSet.empty   

  let degree (cd:t) (i:int) = IntSet.cardinal cd.(i)

  let print (cd:t) = 
    Array.iteri ( fun i e ->
      begin
	print_int i ;
	print_string ":[" ;
	IntSet.iter (fun i -> ( print_int i; print_string " ")) e;
	print_endline "]"
      end ) cd
      
end

let initialize_clique ?mc:(m=true) (g:graph) (x:int) (colors:Colors.t) 
    (cd:ColorDegree.t) = 
  let clique = if m then 
      max_clique g x
    else initial_clique g x
  in
  let rec loop i remain = 
    match remain with
      | [] -> ()
      | h::tl -> 
	begin
	  Colors.color colors h i;
	  ColorDegree.add cd colors g h;
	  loop (i+1) tl
	end
  in
  begin
    loop 0 clique ;
    List.length clique
  end 

let first_uncolored_max_cd (colors:Colors.t) (cd:ColorDegree.t) (n:int) : int option =
  let idx = ref 0 in
  let max_cd = ref (-1) in
  begin
    for i = 0 to n-1 do
      if ((ColorDegree.degree cd i) > !max_cd) && 
	not(Colors.is_colored colors i) then
	begin
	  max_cd := ColorDegree.degree cd i;
	  idx := i 
	end
      else ()
    done;
    (* ColorDegree.print cd ;
     Printf.printf " max cd %d\n" !idx;  *)
    if (!max_cd >0) then
      Some(!idx)
    else 
      None
  end
  

let first_uncolored colors sorted: int option  = 
  let i = ref 0 in
  begin
    while (!i < (Array.length sorted)) &&
      (Colors.is_colored colors sorted.(!i)) do
      i := !i+1
    done;
    if !i < (Array.length sorted) then
      Some sorted.(!i)
    else
      None
  end

let next_available_color g colors v start =
  let c = ref start in
  let adj_colors = Colors.get_colors colors (adjacent g v) in
  begin
    while (IntSet.mem !c adj_colors) do
      c := !c + 1
    done;
    !c
  end 
  
let greedy_color ?debug:(d=false) (g:graph) = 
  begin 
    debugging := d; 
    let n = Array.length g in
    let colors = Colors.create n in
    let cd = ColorDegree.create n in
    let sorted = sort_graph g in 
    (* Local functions *)
    let nClique = initialize_clique ~mc:false g sorted.(0) colors cd in

    let nColors = ref nClique in

    let nextOpt = first_uncolored_max_cd colors cd n in
    match nextOpt with
      | None -> (colors, !nColors)
      | Some next ->       
	let stop = ref false in 
	begin
	(* maintain a stack of (v, currentColor) *)
	  let nextc = next_available_color g colors next 0 in
	  Colors.color colors next nextc; 
	  ColorDegree.add cd colors g next;
	  
	  while not(!stop) do
	    let nextOpt = first_uncolored_max_cd colors cd n in
	    match nextOpt with
	      | None -> stop := true
	      | Some next -> 
              (* Need to color the next vertex *)
		let nextc = next_available_color g colors next 0 in
		begin
		  if (nextc > !nColors) then
		    nColors := nextc
		  else (); 
	   	  Colors.color colors next nextc;
		  ColorDegree.add cd colors g next; 
		end
	  done;
	  (colors, !nColors)
	end	  
  end

let color ?debug:(d=false) ?start:(s=0) (g:graph)  = 
  begin 
    debugging := d; 
    let n = Array.length g in
    let colors = Colors.create n in
    let cd = ColorDegree.create n in
    let sorted = sort_graph g in 
    (* Local functions *)
    let nClique = initialize_clique g sorted.(0) colors cd in

    let nColors = ref (max s nClique) in
    let stack = Stack.create () in
    let backtrack () = 
      let bt = ref true in
      begin
      (* Backtrack until another possibility, i.e.,
         whether currentColor < currentBound or until
         the stack is empty, where we need to add a new
         color. *)
	while (!bt && not(Stack.is_empty stack)) do 
          let (last, lastc) = Stack.pop stack in
          let _ = Colors.uncolor colors last in
	  let _ = ColorDegree.update cd colors g last in
          if (lastc < !nColors) then
	    let tryc = 
	      next_available_color g colors last (lastc+1) in
	    if (tryc <= !nColors) then
	      (* Stop backtracking *)
	      begin
		Colors.color colors last tryc;
		ColorDegree.add cd colors g last;
		Stack.push (last, tryc) stack;
		bt := false
	      end
	    else ()
          else ()
	done;
	if (Stack.is_empty stack) then
	  (print_endline "empty stack";
	   print_endline 
	     ( "Try " ^ (string_of_int (!nColors + 1)) ^ "colors"); 
           nColors := !nColors + 1
	  )
	else ()
      end
    in
    let nextOpt = first_uncolored_max_cd colors cd n in
    match nextOpt with
      | None -> (colors, !nColors)
      | Some next ->       
	let stop = ref false in 
	begin
	(* maintain a stack of (v, currentColor) *)
	  let nextc = next_available_color g colors next 0 in
	  Colors.color colors next nextc; 
	  ColorDegree.add cd colors g next;
	  Stack.push (next, nextc) stack; 
	  
	  while not(!stop) do
	    let nextOpt = first_uncolored_max_cd colors cd n in
	    match nextOpt with
	      | None -> stop := true
	      | Some next -> 
              (* Need to color the next vertex *)
		let nextc = next_available_color g colors next 0 in
		if (nextc > !nColors) then
		  backtrack () 
		else 
		  begin
		    Colors.color colors next nextc;
		    ColorDegree.add cd colors g next; 
		    Stack.push (next, nextc) stack
		  end
	  done;
	  (colors, !nColors)
	end	  
  end

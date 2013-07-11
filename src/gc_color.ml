open Gc_graph

let debugging = ref false 

module Colors = struct
  type t = int array
  (* Uncolored items has number -1 *)
  let create (n:int) = Array.create n (-1) 

  let uncolor (colors:t) (k:int) = 
    begin 
      colors.(k) <- -1;
      if !debugging then Printf.printf "Uncolor %d\n" k else ()
    end

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
      max_clique g
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
      else 
        ()
    done;
    if !debugging then
      ( Printf.printf "Colors %s\n" (Colors.to_string colors);
        ColorDegree.print cd ;
        Printf.printf "Uncolored max cd %d:%d\n" !idx !max_cd) else ();
    if (!max_cd > -1) then
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
		  if (nextc >= !nColors) then
		    nColors := nextc+1
		  else (); 
	   	  Colors.color colors next nextc;
		  ColorDegree.add cd colors g next; 
		end
	  done;
	  (colors, !nColors)
	end	  
  end

let propagate (g:graph) (colors:Colors.t) (cd:ColorDegree.t)
    stack (nC:int) = 
  let find_singleton () = 
    let idx = ref 0 in
    let stop = ref false in
    begin
      while not(!stop) && (!idx < Array.length colors) do
        if not(Colors.is_colored colors !idx) && 
            ((ColorDegree.degree cd !idx) >= nC-1 ) then
            stop := true
        else 
          idx := !idx + 1
      done; 
      if (!stop) then Some !idx else None 
    end
  in
  (* If a singleton can be found propagate *)
  let stop = ref false in
  let bt = ref false in 
  begin
    while not(!stop) do
      let nextOpt = find_singleton () in
      match nextOpt with 
        | Some next -> 
          begin
            let tryc = next_available_color g colors next 0 in
            if (tryc >= nC) then 
              begin
                stop := true;
                bt := true  (* backtrack *)
              end
            else
              begin 
                Colors.color colors next tryc;
                ColorDegree.add cd colors g next;
                Stack.push (next, tryc) stack
              end
          end
        | None -> stop := true
    done;
    !bt
  end

let color ?debug:(d=false) ?start:(s=1) (g:graph)  = 
  begin 
    debugging := d; 
    let n = Array.length g in
    let colors = Colors.create n in
    let cd = ColorDegree.create n in
    let sorted = sort_graph g in 
    (* Local functions *)
    let nClique = initialize_clique ~mc:true g sorted.(0) colors cd in

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
          if (lastc < !nColors - 1) then
	    let tryc = 
	      next_available_color g colors last (lastc+1) in
	    if (tryc < !nColors) then
	      (* Stop backtracking *)
	      begin
		Colors.color colors last tryc;
		ColorDegree.add cd colors g last;
		Stack.push (last, tryc) stack;
		bt := propagate g colors cd stack !nColors
	      end
	    else ()
          else ()
	done;
	if (Stack.is_empty stack) then
	  ( if !debugging then 
	      ( print_endline "empty stack";
		print_endline 
		  ( "Try " ^ (string_of_int (!nColors +1)) ^ "colors"); 
	      ) else () ;
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
		if (nextc >= !nColors) then
		  backtrack () 
		else 
		  begin
		    Colors.color colors next nextc;
		    ColorDegree.add cd colors g next; 
		    Stack.push (next, nextc) stack;
                    let bt = propagate g colors cd stack !nColors in
                    if bt then
                      backtrack () 
                    else ()
		  end
	  done;
	  (colors, !nColors)
	end	  
  end

let refine ?debug:(d=false) (g:graph) (colors:Colors.t) (elim:int)(nC: int) =
  begin
    debugging := d;
    for i=0 to ((Array.length g) -1) do
      (* Eliminate the last color *) 
      begin
        if (Colors.get_color colors i) == elim then
          Colors.uncolor colors i
        else () ;
        (* If eliminate one of the intermediate ones, recolor the last entry *)
        if (elim != nC -1) then
          if (Colors.get_color colors i) == nC -1 then
            Colors.color colors i (elim)
          else ()
        else()
      end
    done;
    let nColors = nC - 1 in
    let n = Array.length g in
    let cd = ColorDegree.create (Array.length g) in
    for i=0 to ((Array.length g) -1) do
      ColorDegree.update cd colors g i
    done;
    let stack = Stack.create () in
    let backtrack () = 
      let bt = ref true in
      begin
      (* Backtrack until another possibility *)
	while (!bt && not(Stack.is_empty stack)) do 
          let (last, lastc) = Stack.pop stack in
          let _ = Colors.uncolor colors last in
	  let _ = ColorDegree.update cd colors g last in
          if (lastc < nColors - 1) then
	    let tryc = 
	      next_available_color g colors last (lastc+1) in
	    if (tryc < nColors) then
	      (* Stop backtracking *)
	      begin
		Colors.color colors last tryc;
		ColorDegree.add cd colors g last;
		Stack.push (last, tryc) stack;
		bt := propagate g colors cd stack nColors
	      end
	    else ()
          else ()
	done;
	if (Stack.is_empty stack) then false else true
      end
    in
    let nextOpt = first_uncolored_max_cd colors cd n in
    match nextOpt with
      | None -> (colors, true)
      | Some next ->       
	let stop = ref false in 
        let failed = ref false in
        let nextc = next_available_color g colors next 0 in
        if (nextc >= nColors) then
          (colors, false)
        else
	  begin
	  (* maintain a stack of (v, currentColor) *)
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
		if (nextc >= nColors) then
		  let nonempty = backtrack () in
                  if not(nonempty) then 
                        (* Stack is empty, stop *)
                    (
                      stop := true;
                      failed := true
                    )
                  else ()                  
		else 
		  begin
		    Colors.color colors next nextc;
		    ColorDegree.add cd colors g next; 
		    Stack.push (next, nextc) stack;
                    let bt = propagate g colors cd stack nColors in
                    if bt then
                      let nonempty = backtrack () in
                      if not(nonempty) then 
                        (* Stack is empty, stop *)
                        (
                          stop := true;
                          failed := true
                        )
                      else ()
                    else ()
		  end
	  done;
	  (colors, not(!failed) )
	end	      
  end
    


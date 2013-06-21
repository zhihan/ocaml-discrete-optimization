open Graph

module ColorMap = struct
  include Map.Make(Int)
    
end

let color (g:graph) = 
  let color = ColorMap.empty in
  

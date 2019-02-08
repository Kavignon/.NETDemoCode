module IfExpressions
open CoreDS

let tryingIfExpressions euclidPosition = 
    if euclidPosition.Depth > 5.0 then 
        printfn "Depth is %f" euclidPosition.Depth
    elif euclidPosition.Depth < 5.0 && euclidPosition.Depth > 0.0 then 
        printfn "Nothing"
    else 
        printfn " (%f, %f) " euclidPosition.CartesianPosition.x euclidPosition.CartesianPosition.y 
        
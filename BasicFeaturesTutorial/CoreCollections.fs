module CoreCollections

let toSquare x = x*x

(**Arrays**)
let myEmptyArr = [| |] 

let filteredArray = [|1..100|] |> Array.filter(fun x -> x%2 = 0)

let mappedArray = filteredArray |> Array.map toSquare

let operationAsPipeline = 
    [|1..1000|]
    |> Array.filter (fun x -> x%2 = 0)
    |> Array.map toSquare 
    |> Array.iter (fun x -> printfn "%d" x)

let arrayInit = Array.init 10 (fun value -> toSquare value)


(**Lists**)
let myEmptyList = []

let filteredList = [1..10] |> List.filter(fun x -> x%2 <>0)

let mappedList = [1..10] |> List.map toSquare

let operationPipelineOnList = 
    [1..1000]
    |> List.filter (fun x -> x%2 =0)
    |> List.map toSquare 
    |> List.tryFind (fun x -> x = 100)
    |> Option.map(fun foundX -> toSquare foundX)
    |> Option.iter(fun squareFoundX -> printfn "%d" squareFoundX)
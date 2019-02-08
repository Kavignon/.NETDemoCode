module LetBinding
//Top level module


let someFunction() =
    let a = 10
    let a = 5
    printfn "%d" a
    () // Unit - absence of value

////Because of %s -> compiler knows str is a string
//string -> unit
let printingString str =
    printfn "%s" str

////string -> int
let gettingLengthFromString (str: string) =
    let lengthOfString = str.Length
    lengthOfString

// unit -> unit
let ``you can name functions this way but this usage is more for testing than anything else``() =
    ()

////Danger --
////Left like this, after single computation, becomes value
//// Need to add () next to name to become a function
let getTypeOfInferredInt =
    let valOf20 = 20
    printfn "%d" valOf20
    let typeOfAInt = valOf20.GetType()
    printfn "%O" typeOfAInt

////usingPreviouslyDefineFunction considered as undefined
//let cannotCallFunctionDefineAfterSelf str =
//    usingPreviouslyDefineFunction str

//// Based on parameter type, compiler infers type to str
let usingPreviouslyDefineFunction str =
    let length = gettingLengthFromString str
    printfn "%d" length

////Constant
[<Literal>]
let myConstantDefinition = 10

////Literal cannot be duplicated
//[<Literal>]
//let myConstantDefinition = 10

let add x y = x + y

let usingAddFunction x = add x

let a = usingAddFunction 10 20

// Local module need at least an expression, type definition, value, constant
module myLocalModule =
//Causes a problem of indentation
    let a = 1
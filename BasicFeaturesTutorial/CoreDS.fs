module CoreDS
open System
//
//// Discriminated unions 
type Condiment =
    | Mustard
    | Ketchup
    | Relish
    | Vinegar

type Favorite =
    | Bourbon of string
    | Number of int

type Shape = 
    | Rectangle of width: int * height: int 
    | Circle of radius : float 
    | Square of edge : int 

////Matching on Shape 
let getShapeSurface shape = 
    match shape with 
    | Square _ 
    | Circle _ -> 0 
    | _ -> 0

//
////Records 
type Point2D = { 
    x : float 
    y : float 
}

type Point3D = {
     CartesianPosition : Point2D
     Depth : float 
} 
with 
    member x.getCartesianPosition = 
        x.CartesianPosition
    member x.updatePosition abscisse ordonna depth = 
        { x with CartesianPosition = { x = abscisse; y = ordonna; }; Depth = depth }

//
let isMatchByCartesianPosition euclidPosition cartesianPosition =
    match euclidPosition with
    | { Point3D.CartesianPosition = foundPosition; Point3D.Depth = _; } when foundPosition = cartesianPosition -> true
    | _ -> false
//
//
//// Option 
(*null means a reference or pointer to an object that doesn't exist*)
(*In F#, to indicate missing data, you would use an option type and set it to None.*)
//(*
//    type Option<'a> = 
//    | Some of <'a>
//    | None 
//*)
////Makes us consider all the different possible outcome 
let tryParseTuple intStr = 
   try
      let i = System.Int32.Parse intStr
      (true,i)
   with _ -> (false,0)  // any exception
//
let x = Some 99
let result = match x with 
| Some i -> Some(i * 2)
| None -> None
module TypeProviderTest

open FSharp.Data
open System.IO

type Sample = JsonProvider<"./SampleModel.json">
let contentList = "./SampleModel.json" |> File.ReadAllText |> Sample.Parse

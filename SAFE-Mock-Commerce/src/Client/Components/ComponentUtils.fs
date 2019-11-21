module ComponentUtils

open Fable.React

let inline elmishView name render = FunctionComponent.Of(render, name, equalsButFunctions)
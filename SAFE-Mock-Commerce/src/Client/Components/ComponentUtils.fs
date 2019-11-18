module ComponentUtils

let inline elmishView name render = FunctionComponent.Of(render, name, equalsButFunctions)
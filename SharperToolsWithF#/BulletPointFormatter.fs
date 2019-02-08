module BulletPointFormatter
open System
open System.Linq 

type Indexes = {
    Primary: int 
    Secondary : int
    Third : int 
}

type BulletPointStyle =
    | NumberedStyle 
    | LetteredStyle 

type HeadingWeight =
    | HW1
    | HW2
    | HW3

type Heading = {
    Weight : HeadingWeight
    Text : string
}

type Node = {
    Line : Heading 
}

type Outline = {
    Text : string 
    HeadingIndexes : Indexes
}
with 
    member x.addContent str = { x with Text = x.Text + Environment.NewLine  + str }

let updateIndexes o h = 
    match h.Weight with 
    | HW1 -> 
        { o with HeadingIndexes = { o.HeadingIndexes with Primary = o.HeadingIndexes.Primary + 1; Secondary = 1 ;Third = 1 } }
    | HW2 -> 
        { o with HeadingIndexes = { o.HeadingIndexes with Secondary = o.HeadingIndexes.Secondary + 1; Third = 1 } }
    | HW3 ->
        { o with HeadingIndexes = { o.HeadingIndexes with Third = o.HeadingIndexes.Third + 1 } }       

let determineBulletStyle (hw: HeadingWeight) = 
    match hw with 
    | HW1 -> NumberedStyle
    | HW2 | HW3 -> LetteredStyle

let getLetter (index: int) = (("ABCDEFGHIJKLMNOPQRSTUVWXYZ".[index-1]).ToString())

let formatTextNode(n: Node) (o: Outline)= 
    let header = n.Line
    let mutable text = ""
    let style = determineBulletStyle header.Weight
    let indexer = o.HeadingIndexes
    match style with 
    | NumberedStyle -> 
        text <- (indexer.Primary.ToString()) + ". " + header.Text
    | LetteredStyle -> 
        let head = 
            match header.Weight with 
            | HW2 -> String.Join("", Enumerable.Repeat(" ",4)) + getLetter indexer.Secondary
            | HW3 -> String.Join("", Enumerable.Repeat(" ", 8)) + String.Join("", Enumerable.Repeat("i", indexer.Third))
            | _ -> ""
        text <- head + ". " + header.Text 
    text

// Folds an Outline and a list Nodes to an Outline
let formatTextOutline(nodeList) =
    ( { Text = ""; HeadingIndexes = { Primary = 1; Secondary = 1; Third = 1 } }, nodeList) 
    ||> Seq.fold (fun outline node -> 
        let text = formatTextNode node outline
        let outline = outline.addContent text
        updateIndexes outline node.Line
    )
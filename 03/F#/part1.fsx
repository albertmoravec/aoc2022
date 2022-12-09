open System.IO

let rec splitCompartments content =
    content
    |> Seq.toList
    |> List.splitInto 2
    |> List.map Set.ofList
    
let rec findDuplicate compartments =
    Set.intersectMany compartments
    |> Set.minElement
    
let itemToPriority item =
    match item with
    | item when item >= 'a' && item <= 'z' -> int item - int 'a' + 1
    | item when item >= 'A' && item <= 'Z' -> int item - int 'A' + 27    
    
File.ReadLines("./03/input.txt")
|> List.ofSeq
|> List.map splitCompartments
|> List.map findDuplicate
|> List.map itemToPriority
|> List.sum
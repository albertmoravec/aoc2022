open System.IO

let rec splitCompartments content =
    content
    |> Seq.toList
    |> List.splitInto 2
    |> fun list -> (list.[0], list.[1])
    
let rec findTriplicate rucksacks =
    rucksacks
    |> List.map Set.ofList
    |> Set.intersectMany
    |> Set.minElement
    
let itemToPriority item =
    match item with
    | item when item >= 'a' && item <= 'z' -> int item - int 'a' + 1
    | item when item >= 'A' && item <= 'Z' -> int item - int 'A' + 27
    
File.ReadLines("./03/input.txt")
|> List.ofSeq
|> List.map List.ofSeq
|> List.chunkBySize 3
|> List.map findTriplicate
|> List.map itemToPriority
|> List.sum
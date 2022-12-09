open System.IO

let rec pair list = (List.item 0 list, List.item 1 list)

let parseRange (range: string) =
    range.Split("-")
    |> List.ofArray
    |> List.map int

let rec parseRanges (ranges: string) =
    ranges.Split(",")
    |> List.ofArray
    |> List.map parseRange
    |> List.map pair
    
let numberRange (low, high) = [ low .. high ]

let intersects (rangeOne, rangeTwo) =
    Set.intersect rangeOne rangeTwo
    |> Set.isEmpty
    |> not


File.ReadLines("./04/input.txt")
|> List.ofSeq
|> List.map parseRanges
|> List.map (List.map numberRange)
|> List.map (List.map Set.ofList)
|> List.map pair
|> List.map intersects
|> List.where id
|> List.length
open System.IO

let rec collectItems acc item = match (acc, item) with
                                | (acc, "") -> [] :: acc
                                | (head :: tail, item) -> (int item :: head) :: tail
                                | ([], item) -> [[int item]]

File.ReadLines("./01/input.txt")
|> List.ofSeq
|> List.fold(collectItems) []
|> List.map(List.sum)
|> List.max
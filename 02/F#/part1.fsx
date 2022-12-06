open System.IO

type Choice =
    | Rock
    | Paper
    | Scissors
    
type Outcome =
    | Win
    | Draw
    | Loss

let mapChoice choice = match choice with
                       | "A" | "X" -> Rock
                       | "B" | "Y" -> Paper
                       | "C" | "Z" -> Scissors
                       | _ -> failwith "Unknown choice"
                       
let outcome opponentsChoice myChoice = match (myChoice, opponentsChoice) with                                       
                                       | Rock, Paper -> Loss
                                       | Rock, Scissors -> Win
                                       | Paper, Rock -> Win
                                       | Paper, Scissors -> Loss
                                       | Scissors, Rock -> Loss
                                       | Scissors, Paper -> Win
                                       | _, _ -> Draw

let outcomePoints outcome = match outcome with
                            | Win -> 6
                            | Draw -> 3
                            | Loss -> 0

let choicePoints choice = match choice with
                          | Rock -> 1
                          | Paper -> 2
                          | Scissors -> 3
                          
let mapTuple f (a, b) = (f a, f b)
    
File.ReadLines("./02/input.txt")
|> List.ofSeq
|> List.map(fun x -> x.Split(" "))
|> List.map(fun x -> (x.[0], x.[1]))
|> List.map(mapTuple mapChoice)
|> List.map(fun (x, y) -> (outcomePoints (outcome x y)) + (choicePoints y))
|> List.sum
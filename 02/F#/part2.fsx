open System.IO

type Choice =
    | Rock
    | Paper
    | Scissors
    
type Outcome =
    | Win
    | Draw
    | Loss

let mapChoice choice =
    match choice with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "Unknown choice"
                       
let mapOutcome outcome =
    match outcome with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "Unknown outcome"
                         
let myChoice opponentChoice outcome =
    match (opponentChoice, outcome) with
    | c, Draw -> c
    | Rock, Win -> Paper
    | Rock, Loss -> Scissors
    | Paper, Win -> Scissors
    | Paper, Loss -> Rock
    | Scissors, Win -> Rock
    | Scissors, Loss -> Paper
                       
let outcome opponentsChoice myChoice =
    match (myChoice, opponentsChoice) with
    | Rock, Paper -> Loss
    | Rock, Scissors -> Win
    | Paper, Rock -> Win
    | Paper, Scissors -> Loss
    | Scissors, Rock -> Loss
    | Scissors, Paper -> Win
    | _, _ -> Draw

let outcomePoints outcome =
    match outcome with
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

let choicePoints choice =
    match choice with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
                              
File.ReadLines("./02/input.txt")
|> List.ofSeq
|> List.map(fun x -> x.Split(" "))
|> List.map(fun x -> (mapChoice x.[0], mapOutcome x.[1]))
|> List.map(fun (opponentChoice, outcome) -> (opponentChoice, myChoice opponentChoice outcome))
|> List.map(fun (x, y) -> (outcomePoints (outcome x y)) + (choicePoints y))
|> List.sum
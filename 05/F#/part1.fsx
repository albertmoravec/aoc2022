#r "nuget: FParsec"

open System
open FParsec
open System.IO

type Stacks = char list list
type Movement = { Amount: int; From: int; To: int }

module InputParser =
    let ws = pstring " "
    
    module StackingParser =
        let pCrate = pstring "[" >>. asciiUpper .>> pstring "]" |>> Some
        let pEmptySpace = pstring "   " >>. preturn None
        let pCrateOrEmpty = pCrate <|> pEmptySpace
        let pStackLine = sepEndBy1 pCrateOrEmpty ws
        let pStacking = sepEndBy1 pStackLine newline .>> skipRestOfLine true .>> skipRestOfLine true
    
        
    module MovementParser =

        let pAmount = pstring "move" >>. ws >>. pint32
        let pFrom = ws >>. pstring "from" >>. ws >>. pint32
        let pTo = ws >>. pstring "to" >>. ws >>. pint32
        let pMovement =
            pipe3 pAmount pFrom pTo (fun item posFrom posTo ->
                { Amount = item
                  From = posFrom - 1
                  To = posTo - 1 })

        let pMovements = sepEndBy1 pMovement newline
    
    
    let pInput = StackingParser.pStacking .>>. MovementParser.pMovements
    
let input = File.ReadAllText("./Solutions/05/input.txt")

let stackLines, movements =
    match run InputParser.pInput input with
    | Success(config, _, _) -> config
    | Failure _ -> failwith "invalid input"
    
let stackLinesToStacks = List.transpose >> (List.map (List.choose id))
    
let move (stacks: Stacks) (posFrom: int) (posTo: int): Stacks =
    let item = List.item posFrom stacks |> List.head
        
    let mapper i stack =
        match i with
        | pos when pos = posFrom -> List.tail stack
        | pos when pos = posTo -> item::stack
        | _ -> stack
    
    List.mapi mapper stacks

let processMovement (stacks: Stacks) ({Amount = amount; From = posFrom; To = posTo}: Movement): Stacks =
    List.fold (fun s _ -> move s posFrom posTo ) stacks [ 1..amount ]

let processMovements (movements: Movement list) (stacks: Stacks) = List.fold processMovement stacks movements

let stacks: Stacks = stackLinesToStacks stackLines

let stacksTop (stacks: Stacks) = List.map List.head stacks

processMovements movements stacks
|> stacksTop 
|> Array.ofList
|> String
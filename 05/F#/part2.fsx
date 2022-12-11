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
    
let input = File.ReadAllText("./05/input.txt")

let stackLines, movements =
    match run InputParser.pInput input with
    | Success(config, _, _) -> config
    | Failure _ -> failwith "invalid input"
    
let stackLinesToStacks = List.transpose >> (List.map (List.choose id))
    
let move (stacks: Stacks) ({Amount = amount; From = posFrom; To = posTo}: Movement): Stacks =
    let items = List.item posFrom stacks |> List.take amount
        
    let mapper i (stack: char list) =
        match i with
        | pos when pos = posFrom -> List.rev (List.take ((List.length stack) - amount) (List.rev stack))
        | pos when pos = posTo -> items @ stack
        | _ -> stack
    
    List.mapi mapper stacks

let processMovements (movements: Movement list) (stacks: Stacks) = List.fold move stacks movements

let stacks: Stacks = stackLinesToStacks stackLines

let stacksTop (stacks: Stacks) = List.map List.head stacks

processMovements movements stacks
|> stacksTop 
|> Array.ofList
|> String
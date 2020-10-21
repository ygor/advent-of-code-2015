open System
open System.IO
open day3.ListExtensions

type Move =
    | North
    | South
    | East
    | West
type House = int * int

let input = File.ReadAllLines(@"input.txt") |> Seq.head

let parseMove =
    function
    | '^' -> North
    | 'v' -> South
    | '>' -> East
    | '<' -> West
    | _ -> failwith "Invalid move"

let moves =
    input
    |> Seq.toList
    |> List.map parseMove

let visit house visited =
    let count =
        if Map.containsKey house visited then visited.[house] + 1 else 1
    Map.add house count visited

let deliverPresents moves visited =
    let start = (0, 0)
    moves
    |> Seq.fold (fun (house, visited') move ->
        let house' =
            match move with
            | North -> (fst house + 1, snd house)
            | South -> (fst house - 1, snd house)
            | East -> (fst house, snd house + 1)
            | West -> (fst house, snd house - 1)
        (house', visit house' visited')) (start, visit start visited)
    |> snd

let housesVisitedBySanta = deliverPresents moves Map.empty<House, int> |> Map.count

let housesVisitedBySantaAndRobo =
    let (santaMoves, roboMoves) = List.splitPairwise moves
    
    Map.empty<House, int>
    |> deliverPresents santaMoves
    |> deliverPresents roboMoves
    |> Map.count

[<EntryPoint>]
let main argv =    
    housesVisitedBySanta |> printfn "Part 1: Visited houses by santa: %i"
    housesVisitedBySantaAndRobo |> printfn "Part 2: Visited houses by santa and robo: %i"
    0
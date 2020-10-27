open System.IO
open Extensions

let moves =
    File.ReadAllLines(@"input.txt")
    |> Seq.head
    |> seq

let deliver moves houses =
    moves |>
    Seq.fold (fun houses move ->
        match houses with
        | (x,y)::_ ->
            match move with
            | '^' -> (x, y - 1) :: houses
            | 'v' -> (x, y + 1) :: houses
            | '>' -> (x + 1, y) :: houses
            | '<' -> (x - 1, y) :: houses
            | x -> failwithf "Invalid move: %A" x
        | _ -> failwith "Missing start") houses
        
let housesVisitedBySanta =
    deliver moves [ (0, 0) ]
    |> Seq.distinct
    |> Seq.length


let housesVisitedBySantaAndRobo =
    let (santaMoves, roboMoves) = List.splitPairwise (moves |> List.ofSeq)
    Seq.append (deliver santaMoves [ (0, 0) ]) (deliver roboMoves [ (0, 0) ])
    |> Seq.distinct
    |> Seq.length

[<EntryPoint>]
let main argv =
    housesVisitedBySanta |> printfn "Part 1: Visited houses by santa: %i"
    housesVisitedBySantaAndRobo |> printfn "Part 2: Visited houses by santa and robo: %i"
    0

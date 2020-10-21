open System.IO

type Move =
    | North
    | South
    | East
    | West
type House = int * int
type Visits = Map<House, int>

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

let visit house visits =
    let count =
        if Map.containsKey house visits then visits.[house] + 1 else 1
    Map.add house count visits

let deliver moves visits =
    let start = (0, 0)
    moves
    |> Seq.fold (fun (house, visits) move ->
        let house' =
            match move with
            | North -> (fst house + 1, snd house)
            | South -> (fst house - 1, snd house)
            | East -> (fst house, snd house + 1)
            | West -> (fst house, snd house - 1)
        (house', visit house' visits)) (start, visit start visits)

let prepend2 (x, y) (xs, ys) = x::xs, y::ys

let rec splitList = function
    | [] | [_] as xs -> xs, []
    | x::y::xs -> prepend2 (x, y) (splitList xs)  

let splitMoves = splitList moves

[<EntryPoint>]
let main argv =
    let visits = deliver moves Map.empty<House, int> |> snd
    Map.count visits |> printfn "Part 1: Visited houses: %i"
    0
open System.IO

type Direction =
    | Up
    | Down

let parseDirection = function
    | '(' -> Up
    | ')' -> Down
    | _ -> failwith "Invalid direction"
        
let directions =
    File.ReadAllLines(@"input.txt")
    |> Seq.head
    |> Seq.toList
    |> Seq.map parseDirection

let walk directions =
    let count dir =
        directions
        |> Seq.where (fun d -> d = dir)
        |> Seq.length
    count Up - count Down

let numDirectionsToBasement directions =
    directions
    |> Seq.fold (fun (floor, count) dir ->
        match floor with
        | -1 -> (floor, count)
        | _ ->
            let floor' =
                match dir with
                | Up -> floor + 1
                | Down -> floor - 1
            (floor', count + 1)) (0, 0)

[<EntryPoint>]
let main argv =
    walk directions |> printfn "Part 1: end at floor: %i"
    
    numDirectionsToBasement directions
    |> snd
    |> printfn "Part 2: number of directions to basement: %i"
    0

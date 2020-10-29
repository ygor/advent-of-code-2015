open System.IO

let numbers =
    File.ReadAllLines("input.txt")
    |> Seq.head
    |> seq
    |> Seq.map (string >> int)

let read (numbers: int seq) =
    numbers
    |> Seq.fold (fun result next ->
        match result with
        | [] -> [ (next, 1) ]
        | (prev, count) :: xs as result ->
            if prev = next then (prev, count + 1) :: xs else (next, 1) :: result) []
    |> Seq.fold (fun acc (number, count) -> Seq.append (count :: [ number ]) acc) (seq [])

let rec repeat (numbers: int seq) =
    function
    | 1 -> read numbers
    | n -> repeat (read numbers) (n - 1)

[<EntryPoint>]
let main _ =
    let repeat40 = repeat numbers 40
    
    repeat40 |> Seq.length
    |> printfn "Part 1. Say-and-read: %i"

    repeat repeat40 10
    |> Seq.length
    |> printfn "Part 2. Say-and-read: %i"
    0
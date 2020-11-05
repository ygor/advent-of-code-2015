open System.IO
open FSharp.Data

let json =
    File.ReadAllLines("input.txt")
    |> Seq.head
    |> JsonValue.Parse

let rec count =
    function
    | JsonValue.Array elements -> elements |> Array.sumBy count
    | JsonValue.Record properties -> properties |> Array.sumBy (count << snd)
    | JsonValue.Number number -> int number
    | _ -> 0

let hasRed properties =
    properties
    |> Array.exists (function
         | _, JsonValue.String value -> value = "red"
         | _ -> false)

let rec countAndIgnoreRed =
    function
    | JsonValue.Array elements -> elements |> Array.sumBy countAndIgnoreRed
    | JsonValue.Record properties ->
        if hasRed properties
        then 0
        else Array.sumBy (countAndIgnoreRed << snd) properties
    | JsonValue.Number number -> int number
    | _ -> 0

[<EntryPoint>]
let main _ =
    count json |> printfn "Part 1. Count: %i"
    countAndIgnoreRed json |> printfn "Part 2. Count: %i"
    0
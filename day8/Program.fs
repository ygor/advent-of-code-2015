open System.IO
open System.Text.RegularExpressions
open Extensions

let input = File.ReadAllLines("input.txt")

let replace pattern (replacement:string) string =
    Regex.Replace(string, pattern, replacement)

let decode lines =
    lines
    |> Seq.sumBy (fun (line: string) ->
        let line' =
            line.Substring(1, line.Length - 2)
            |> replace @"\\x[0-9a-fA-F]{2}" "."
            |> replace @"\\\\" "."
            |> replace @"\\""" "\""                        
        line.Length - line'.Length)

let encode lines =
    lines
    |> Seq.sumBy (fun (line: string) ->
        let line' =
            line
            |> replace @"\\" @"\\"
            |> replace @"""" @"\"""                        
        line'.Length + 2 - line.Length)

[<EntryPoint>]
let main _ =
    decode input |> printfn "Part 1. Count: %i"
    encode input |> printfn "Part 2. Count: %i"
    0
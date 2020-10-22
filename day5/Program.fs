open System.IO

let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ]
let forbidden = [ "ab"; "cd"; "pq"; "xy" ]

let strings = File.ReadAllLines(@"input.txt")

let hasThreeVowels (string: string) =
    string
    |> Seq.filter (fun char -> List.contains char vowels)
    |> Seq.length
    |> (<) 2

let rec hasRepeatedLetter (chars: char list) =
    match chars with
    | x :: y :: xs -> x = y || hasRepeatedLetter (y :: xs)
    | []
    | [ _ ] -> false

let rec containsForbidden (forbidden: string list) (string: string) =
    match forbidden with
    | x :: xs -> string.Contains(x) || containsForbidden xs string
    | [] -> false

let isNice (string: string) =
    hasThreeVowels string && hasRepeatedLetter (Seq.toList string) && (containsForbidden forbidden string |> not)

let rec containsRepeatedPair (string: string) =
    string.Length >= 4
    && (string.Substring(2).Contains(string.Substring(0, 2)) || containsRepeatedPair (string.Substring(1)))

let rec containsRepeatedLetter (string: string) =
    string.Length >= 3 && (string.[0] = string.[2] || containsRepeatedLetter (string.Substring(1)))

let isNicer (string: string) =
    containsRepeatedPair string && containsRepeatedLetter string

[<EntryPoint>]
let main argv =
    strings
    |> Seq.filter isNice
    |> Seq.length
    |> printfn "Part 1: number of nice strings %i"

    strings
    |> Seq.filter isNicer
    |> Seq.length
    |> printfn "Part 2: number of nicer strings %i"

    0
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

[<EntryPoint>]
let main argv =
    strings
    |> Seq.filter isNice
    |> Seq.length
    |> printfn "Part 1: number of nice strings %i"
    0

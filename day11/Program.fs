open System.IO

let password =
    File.ReadAllLines("input.txt")
    |> Seq.head
    |> List.ofSeq

let containsTriplet password =
    password
    |> Seq.windowed 3
    |> Seq.filter (function
        | [| a; b; c |] -> int c - int b = 1 && int b - int a = 1
        | _ -> false)
    |> (not << Seq.isEmpty)

let containsValidChars (password: char seq) =
    password
    |> Seq.filter (fun x -> Seq.contains x [ 'i'; 'o'; 'l' ] |> not)
    |> Seq.length
    |> (=) (Seq.length password)

let containsTwoPairs (password: char seq) =
    password
    |> Seq.windowed 2
    |> Seq.filter (function
        | [| x; y |] -> x = y
        | _ -> false)
    |> Seq.distinct
    |> Seq.length
    |> (<) 1

let rec increment (string: char list) =
    match string with
    | [] -> []
    | x :: xs ->
        if x = 'z' then 'a' :: increment xs else (char (int x + 1)) :: xs

let isValid password =
    containsTwoPairs password && containsValidChars password && containsTriplet password

let rec nextPassword password =
    let next =
        password
        |> List.rev
        |> increment
        |> List.rev
    if isValid next then next else nextPassword next

let toString chars =
    chars
    |> List.map string
    |> List.fold (+) ""

[<EntryPoint>]
let main _ =
    let next = nextPassword password
    next
    |> toString
    |> printfn "Part 1. Next password: %s"

    nextPassword next
    |> toString
    |> printfn "Part 2. Next password: %s"

    0
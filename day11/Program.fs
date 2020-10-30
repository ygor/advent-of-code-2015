open System.IO

let password =
    File.ReadAllLines("input.txt")
    |> Seq.head
    |> List.ofSeq

let isIncreasing ((a: char), (b: char), (c: char)) =
    int c - int b = 1 && int b - int a = 1

let rec triplets password =
    match password with
    | [] -> []
    | [ _ ] -> []
    | _ :: _ :: [] -> []
    | x :: y :: z :: xs ->
        if isIncreasing (x, y, z) then [ (x, y, z) ] :: triplets xs else triplets (y :: z :: xs)

let containsOneTriplet password =
    triplets password
    |> Seq.length
    |> (<) 0

let containsValidChars (password: char seq) =
    password
    |> Seq.filter (fun x -> Seq.contains x [ 'i'; 'o'; 'l' ] |> not)
    |> Seq.length
    |> (=) (Seq.length password)

let rec nonOverlappingPairs password =
    match password with
    | [] -> []
    | [ _ ] -> []
    | x :: y :: xs ->
        if x = y then [ x ] :: nonOverlappingPairs xs else nonOverlappingPairs (y :: xs)

let containsTwoPairs (password: char list) =
    nonOverlappingPairs password
    |> Seq.distinct
    |> Seq.length
    |> (<) 1

let rec increment (string: char list) =
    match string with
    | [] -> []
    | x :: xs ->
        if x = 'z' then 'a' :: increment xs else (char (int x + 1)) :: xs

let isValid password =
    containsTwoPairs password && containsValidChars password && containsOneTriplet password

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

open System.IO
open Extensions

let sue =
    [ ("children", 3)
      ("cats", 7)
      ("samoyeds", 2)
      ("pomeranians", 3)
      ("akitas", 0)
      ("vizslas", 0)
      ("goldfish", 5)
      ("trees", 3)
      ("cars", 2)
      ("perfumes", 1) ]
    |> Map.ofSeq

let aunts =
    File.ReadAllLines("input.txt")
    |> Seq.fold (fun aunts line ->
        match line with
        | Regex "Sue (\d+): (.*)" [ number; compounds ] ->
            (int number,
             compounds.Split(", ")
             |> Seq.map
                 (String.split ": "
                  >> List.unpack2
                  >> (fun (a, b) -> a, int b)))
            :: aunts
        | _ -> failwithf "Invalid input %s" line) []

let find sue comparator aunts =
    aunts
    |> Seq.map (fun (id, compounds) ->
        id,
        compounds
        |> Seq.map (comparator sue)
        |> Seq.reduce (&&))
    |> Seq.filter snd
    |> Seq.head
    |> fst

let comparator1 sue (key, value) =
    Map.containsKey key sue && sue.[key] = value

let comparator2 sue (key, value) =
    match key with
    | "cats"
    | "trees" -> Map.containsKey key sue && sue.[key] < value
    | "pomeranians"
    | "goldfish" -> Map.containsKey key sue && sue.[key] > value
    | _ -> comparator1 sue (key, value)

[<EntryPoint>]
let main _ =
    printfn "Part 1: %i" (find sue comparator1 aunts)
    printfn "Part 2: %i" (find sue comparator2 aunts)
    0

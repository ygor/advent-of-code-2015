open System.IO
open Extensions

let input = File.ReadAllLines("input.txt")

type Guest = string

type Score = int

type Table =
    { Guests: Guest list
      Preferences: Map<Guest * Guest, Score> }

    static member Default =
        { Guests = []
          Preferences = Map.empty<Guest * Guest, Score> }

let seat name1 name2 happiness table =
    { table with
          Guests = List.append table.Guests [ name1; name2 ] |> List.distinct
          Preferences = table.Preferences.Add((name1, name2), happiness) }

let table input =
    input
    |> Seq.fold (fun table line ->
        match line with
        | Regex "(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)." [ name1; action; score; name2 ] ->
            let happiness =
                int score * (if action = "lose" then -1 else 1)
            table |> seat name1 name2 happiness
        | _ -> failwithf "Invalid input %s" line) Table.Default

let happiness (table: Table) =
    table.Guests
    |> List.pairwise
    |> List.append [ table.Guests.Head, List.last table.Guests ]
    |> List.sumBy (fun pair -> table.Preferences.[pair] + table.Preferences.[Tuple.rev pair])

let optimize (table: Table) =
    table.Guests
    |> List.permute
    |> List.map (fun guests -> happiness { table with Guests = guests })
    |> List.max

let seatMe (table: Table) =
    let me = "me"
    table.Guests
    |> Seq.fold (fun table' guest ->
        table'
        |> seat me guest 0
        |> seat guest me 0) table

[<EntryPoint>]
let main _ =
    let table' = table input
    table'
    |> optimize
    |> printfn "Part 1. Happiness: %i"

    table'
    |> seatMe
    |> optimize
    |> printfn "Part 2. Happiness: %i"

    0

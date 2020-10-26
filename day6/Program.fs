open System
open System.IO
open Extensions

type Coordinate = int * int

type Instruction =
    | On of Coordinate * Coordinate
    | Off of Coordinate * Coordinate
    | Toggle of Coordinate * Coordinate

type Lights<'a> = Map<Coordinate, 'a>

let input = File.ReadAllLines(@"input.txt")

let coordinate (string: string) =
    string.Split(",") |> (fun xs -> int xs.[0], int xs.[1])

let instruction (line: string) =
    match line with
    | Regex "turn on (\d+,\d+) through (\d+,\d+)" [ start; finish ] ->
        On(coordinate start, coordinate finish)
    | Regex "turn off (\d+,\d+) through (\d+,\d+)" [ start; finish ] ->
        Off(coordinate start, coordinate finish)
    | Regex "toggle (\d+,\d+) through (\d+,\d+)" [ start; finish ] ->
        Toggle(coordinate start, coordinate finish)
    | x -> failwithf "Invalid instruction: %A" x

let instructions = input |> Seq.map instruction

let lights<'a> initial =
    Seq.allPairs [ 0 .. 999 ] [ 0 .. 999 ]
    |> Seq.fold (fun (lights': Lights<'a>) coordinate -> lights'.Add(coordinate, initial)) Map.empty<Coordinate, 'a>

let apply fn start finish (lights: Lights<'a>) =
    [ fst start .. fst finish ]
    |> Seq.fold (fun (lights: Lights<'a>) x ->
        [ snd start .. snd finish ] |> Seq.fold (fun lights y -> lights.Add((x, y), fn lights.[(x, y)])) lights) lights

let applyBooleanInstruction lights instruction =
    match instruction with
    | On (start, finish) -> apply (fun _ -> true) start finish lights
    | Off (start, finish) -> apply (fun _ -> false) start finish lights
    | Toggle (start, finish) -> apply (fun state -> not state) start finish lights

let applyBrightnessInstruction lights instruction =
    match instruction with
    | On (start, finish) -> apply (fun state -> state + 1) start finish lights
    | Off (start, finish) -> apply (fun state -> Math.Max(0, (state - 1))) start finish lights
    | Toggle (start, finish) -> apply (fun state -> state + 2) start finish lights

let setup lights instructions instructionFn =
    instructions |> Seq.fold instructionFn lights

let totalLightsLit lights =
    lights
    |> Map.filter (fun _ state -> state)
    |> Map.count

let totalBrightness lights =
    lights
    |> Map.toList
    |> Seq.sumBy (fun (_, brightness) -> brightness)

[<EntryPoint>]
let main argv =
    setup (lights false) instructions applyBooleanInstruction
    |> totalLightsLit
    |> printfn "Part 1. Number of lights lit: %i"

    setup (lights 0) instructions applyBrightnessInstruction
    |> totalBrightness
    |> printfn "Part 2. Number of lights lit: %i"

    0

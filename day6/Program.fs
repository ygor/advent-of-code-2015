open System
open System.IO

type Coordinate = int * int

type Grid = Coordinate * Coordinate

type Instruction =
    | On of Grid
    | Off of Grid
    | Toggle of Grid

type Lights<'a> = Map<Coordinate, 'a>

let input = File.ReadAllLines(@"input.txt")

let parseCoordinate (string: string) =
    string.Split(",") |> (fun xs -> int xs.[0], int xs.[1])

let parseGrid offset (line: string) =
    line.Substring(offset).Split(" ") |> (fun xs -> (parseCoordinate xs.[0], parseCoordinate xs.[2]))

let parseInstruction (line: string) =
    if line.Contains("turn on") then On(parseGrid 8 line)
    elif line.Contains("turn off") then Off(parseGrid 9 line)
    elif line.Contains("toggle") then Toggle(parseGrid 7 line)
    else failwith "Invalid instruction"

let instructions = input |> Seq.map parseInstruction

let lights<'a> initial =
    Seq.allPairs [ 0 .. 999 ] [ 0 .. 999 ]
    |> Seq.fold (fun (lights': Lights<'a>) coordinate -> lights'.Add(coordinate, initial)) Map.empty<Coordinate, 'a>

let changeLights (lights: Lights<'a>) grid changeFn =
    let (a, b), (c, d) = grid
    [ a .. c ]
    |> Seq.fold (fun (lights': Lights<'a>) x ->
        [ b .. d ] |> Seq.fold (fun lights'' y ->
            lights''.Add((x, y), changeFn lights''.[(x, y)])) lights') lights

let applyInstruction lights instruction =
    match instruction with
    | On grid -> changeLights lights grid (fun _ -> true)
    | Off grid -> changeLights lights grid (fun _ -> false)
    | Toggle grid -> changeLights lights grid (fun state -> not state)

let applyBrightnessInstruction lights instruction =
    match instruction with
    | On grid -> changeLights lights grid (fun state -> state + 1)
    | Off grid -> changeLights lights grid (fun state -> Math.Max(0, (state - 1)))
    | Toggle grid -> changeLights lights grid (fun state -> state + 2)

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
    setup (lights false) instructions applyInstruction
    |> totalLightsLit
    |> printfn "Part 1. Number of lights lit: %i"

    setup (lights 0) instructions applyBrightnessInstruction
    |> totalBrightness
    |> printfn "Part 2. Number of lights lit: %i"

    0
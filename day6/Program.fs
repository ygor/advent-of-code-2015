open System.IO

type Coordinate = int * int

type Grid = Coordinate * Coordinate

type Instruction =
    | On of Grid
    | Off of Grid
    | Toggle of Grid

type Lights = Map<Coordinate, bool>

let input = File.ReadAllLines(@"input.txt")

let parseCoordinate (string: string) =
    string.Split(",") |> (fun xs -> int xs.[0], int xs.[1])

let parseGrid offset (line: string) =
    line.Substring(offset).Split(" ") |> (fun xs -> (parseCoordinate xs.[0], parseCoordinate xs.[2]))

let parseLine (line: string) =
    if line.Contains("turn on") then On(parseGrid 8 line)
    elif line.Contains("turn off") then Off(parseGrid 9 line)
    elif line.Contains("toggle") then Toggle(parseGrid 7 line)
    else failwith "Invalid instruction"

let instructions = input |> Seq.map parseLine

let lights =
    Seq.allPairs [ 0 .. 999 ] [ 0 .. 999 ]
    |> Seq.fold (fun (lights': Lights) coordinate -> lights'.Add(coordinate, false)) Map.empty<Coordinate, bool>

let switchLights (lights: Lights) grid fn =
    let (a, b), (c, d) = grid
    [ a .. c ]
    |> Seq.fold (fun (lights': Lights) x ->
        [ b .. d ] |> Seq.fold (fun lights'' y -> lights''.Add((x, y), fn lights''.[(x, y)])) lights') lights

let applyInstruction lights instruction =
    match instruction with
    | On grid -> switchLights lights grid (fun _ -> true)
    | Off grid -> switchLights lights grid (fun _ -> false)
    | Toggle grid -> switchLights lights grid (fun state -> not state)

let setup lights instructions =
    instructions |> Seq.fold applyInstruction lights

let numLightsLit lights =
    lights
    |> Map.filter (fun _ state -> state)
    |> Map.count

[<EntryPoint>]
let main argv =
    setup lights instructions
    |> numLightsLit
    |> printfn "Part 1. Number of lights lit: %i"
    0
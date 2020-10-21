open System
open System.Diagnostics
open System.IO

type Present =
    { length: int
      width: int
      height: int }

type Face = int * int

let parseLine (line: string) =
    let dimensions =
        line.Split("x")
        |> Seq.map int
        |> Seq.toList
    { length = dimensions.[0]
      width = dimensions.[1]
      height = dimensions.[2] }

let presents = File.ReadAllLines(@"input.txt") |> Seq.map parseLine

let faces (present: Present) =
    [ (present.length, present.width)
      (present.width, present.height)
      (present.height, present.length) ]

let wrap (present: Present) =
    let areas =
        faces present
        |> Seq.map (fun (x, y) -> x * y)
        |> Seq.sort
    Seq.head areas + Seq.sumBy (fun area -> 2 * area) areas

let ribbon (present: Present) =
    let perimeters =
        faces present
        |> Seq.map (fun (x, y) -> 2 * x + 2 * y)
        |> Seq.sort
    Seq.head perimeters + (present.length * present.height * present.width)

let wrappingPaper = presents |> Seq.sumBy wrap
let ribbons = presents |> Seq.sumBy ribbon

[<EntryPoint>]
let main argv =
    wrappingPaper |> printfn "Part 1: square feet of wrapping paper: %i"
    ribbons |> printfn "Part 2: feet of ribbon: %i"
    0

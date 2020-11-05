open System
open System.IO
open Extensions

let input = File.ReadAllLines("input.txt")

type Scores = Map<string, int>

type Reindeer =
    { Name: string
      Velocity: int
      FlyTime: int
      RestTime: int }

let reindeers input =
    input
    |> Seq.fold (fun reindeers line ->
        match line with
        | Regex "(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds."
          [ name; velocity; flyTime; restTime ] ->
            { Name = name
              Velocity = int velocity
              FlyTime = int flyTime
              RestTime = int restTime }
            :: reindeers
        | _ -> failwithf "Invalid input %s" line) []

let rec distance (time: int) (reindeer: Reindeer) =
    let flyTime = Math.Min(time, reindeer.FlyTime)
    let time' = Math.Max(0, time - reindeer.FlyTime - reindeer.RestTime)
    flyTime * reindeer.Velocity + (if time' > 0 then distance time' reindeer else 0)

let winnersByDistance time reindeers =
    reindeers
    |> Seq.map (fun reindeer -> reindeer, distance time reindeer)
    |> Seq.groupBy snd
    |> Seq.maxBy fst
    |> snd

let maxDistance time reindeers =
    reindeers
    |> winnersByDistance time
    |> Seq.head
    |> snd

let increaseScore reindeer scores =
    scores
    |> Map.add reindeer.Name (if Map.containsKey reindeer.Name scores then scores.[reindeer.Name] + 1 else 1)

let maxScore time reindeers =
    [ 1 .. time ]
    |> Seq.fold (fun scores i ->
        winnersByDistance i reindeers
        |> Seq.fold (fun scores' (reindeer, _) -> increaseScore reindeer scores') scores) Map.empty<string, int>
    |> Map.toSeq
    |> Seq.maxBy snd
    |> snd

[<EntryPoint>]
let main _ =
    let time = 2503
    let reindeers = reindeers input

    reindeers
    |> maxDistance time
    |> printfn "Part 1. Max distance: %i km."

    reindeers
    |> maxScore 2503
    |> printfn "Part 1. Max score: %i points."
    0

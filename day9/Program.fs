open System.IO
open Extensions

let input = File.ReadAllLines("input.txt")

type City = string

type Graph =
    { Cities: City list
      Distances: Map<City * City, int> }

    static member Default =
        { Cities = []
          Distances = Map.empty<City * City, int> }

    member graph.distance city1 city2 = graph.Distances.[Tuple.sort (city1, city2)]

    member graph.connect city1 city2 distance =
        let key = Tuple.sort (city1, city2)
        if graph.Distances.ContainsKey(key) then
            graph
        else
            let cities = List.append graph.Cities (city1 :: [ city2 ]) |> List.distinct
            { graph with
                  Cities = cities
                  Distances = graph.Distances.Add(key, distance) }

let parse input =
    input
    |> Seq.fold (fun (graph: Graph) (line: string) ->
        let parts = line.Split(' ')
        graph.connect parts.[0] parts.[2] (int parts.[4])) Graph.Default

let routes (graph: Graph) =
    graph.Cities
    |> List.permute
    |> List.map (function
        | [] -> 0
        | city :: cities ->
            cities
            |> List.fold (fun (distance, prev) next -> distance + graph.distance prev next, next) (0, city)
            |> fst)

[<EntryPoint>]
let main _ =
    let routes' = parse input |> routes
    printfn "Part 1. %i" (List.min routes')
    printfn "Part 2. %i" (List.max routes')
    0
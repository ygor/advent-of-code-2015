open System.IO
open Extensions

let input = File.ReadAllLines("input.txt")

type Ingredient =
    { Capacity: int
      Durability: int
      Flavor: int
      Texture: int
      Calories: int }

    static member (+)(lf, rt) =
        { Capacity = lf.Capacity + rt.Capacity
          Durability = lf.Durability + rt.Durability
          Flavor = lf.Flavor + rt.Flavor
          Texture = lf.Texture + rt.Texture
          Calories = lf.Calories + rt.Calories }

    static member (*)(x, n) =
        { Capacity = x.Capacity * n
          Durability = x.Durability * n
          Flavor = x.Flavor * n
          Texture = x.Texture * n
          Calories = x.Calories * n }

let parse input =
    input
    |> Seq.fold (fun ingredients line ->
        match line with
        | Regex "(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)"
          [ name; capacity; durability; flavor; texture; calories ] ->
            { Capacity = int capacity
              Durability = int durability
              Flavor = int flavor
              Texture = int texture
              Calories = int calories }
            :: ingredients
        | _ -> failwithf "Invalid input %s" line) []

type Recipe = (Ingredient * int) list

let bake recipe =
    recipe
    |> Seq.map (fun (ingredient, teaspoons) -> ingredient * teaspoons)
    |> Seq.reduce (+)

let score cake =
    max 0 cake.Capacity * max 0 cake.Durability * max 0 cake.Flavor * max 0 cake.Texture

let calories cake =
    max 0 cake.Calories

let combine ingredients =
    let rec loop ingredients (accN, cx, cxs) =
        match ingredients with
        | [] -> cx :: cxs
        | [ hd ] -> ((hd, 100 - accN) :: cx) :: cxs
        | hd :: tl ->
            [ 0 .. (100 - accN) ]
            |> Seq.map (fun n -> loop tl (accN + n, (hd, n) :: cx, cxs))
            |> Seq.reduce (@)

    loop ingredients (0, [], [])

let recipes input = parse input |> combine

let cakes input =
    input
    |> recipes
    |> Seq.map bake

[<EntryPoint>]
let main _ =
    let cakes = cakes input

    cakes
    |> Seq.map score
    |> Seq.max
    |> printfn "Part 1. Highest score: %i"

    cakes
    |> Seq.filter (fun cake -> cake.Calories = 500)
    |> Seq.map score
    |> Seq.max
    |> printfn "Part 2. Highest score: %i"
    0

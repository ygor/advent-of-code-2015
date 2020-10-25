open System.IO

type Wire = string

type Signal = int

type Input =
    | Wire of Wire
    | Signal of Signal

let parseInput (string: string) =
    match System.Int32.TryParse string with
    | true, value -> Signal value
    | _ -> Wire string

type Instruction =
    | Set of Input * Wire
    | LeftShift of Wire * Signal * Wire
    | RightShift of Wire * Signal * Wire
    | And of Input * Input * Wire
    | Or of Input * Input * Wire
    | Not of Wire * Wire

let parseInstruction (string: string) =
    let args = string.Split " "
    if args.[0] = "NOT" then Not(args.[1], args.[3])
    elif args.[1] = "->" then Set(parseInput args.[0], args.[2])
    elif args.[1] = "LSHIFT" then LeftShift(args.[0], int args.[2], args.[4])
    elif args.[1] = "RSHIFT" then RightShift(args.[0], int args.[2], args.[4])
    elif args.[1] = "AND" then And(parseInput args.[0], parseInput args.[2], args.[4])
    elif args.[1] = "OR" then Or(parseInput args.[0], parseInput args.[2], args.[4])
    else failwith "Invalid instruction"

let instructions =
    File.ReadAllLines(@"input.txt")
    |> Seq.map parseInstruction
    |> Seq.toList

let applyInstruction circuit (instruction: Instruction) =
    let getValue =
        function
        | Signal value -> Some value
        | Wire id -> Map.tryFind id circuit

    match instruction with
    | Set (input, wire) ->
        match getValue input with
        | Some value -> true, Map.add wire value circuit
        | None -> false, circuit
    | Not (w1, w2) ->
        match getValue (Wire w1) with
        | Some v1 -> true, Map.add w2 (~~~v1) circuit
        | None -> false, circuit
    | LeftShift (w1, value, w2) ->
        match getValue (Wire w1) with
        | Some v1 -> true, Map.add w2 (v1 <<< value) circuit
        | None -> false, circuit
    | RightShift (w1, value, w2) ->
        match getValue (Wire w1) with
        | Some v1 -> true, Map.add w2 (v1 >>> value) circuit
        | None -> false, circuit
    | And (i1, i2, wire) ->
        match (getValue i1, getValue i2) with
        | (Some v1, Some v2) -> true, Map.add wire (v1 &&& v2) circuit
        | (_, _) -> false, circuit
    | Or (i1, i2, wire) ->
        match (getValue i1, getValue i2) with
        | (Some v1, Some v2) -> true, Map.add wire (v1 ||| v2) circuit
        | (_, _) -> false, circuit

let rec run (circuit: Map<Wire, int>) instructions =
    let (changed'', circuit''') =
        instructions
        |> Seq.fold (fun (changed, circuit') instruction ->
            let (changed', circuit'') = applyInstruction circuit' instruction
            changed && changed', circuit'') (true, circuit)

    if changed'' then circuit''' else run circuit''' instructions

let part1 instructions =
    let circuit = run Map.empty instructions
    circuit.["a"]

let part2 valueB instructions =
    let circuit =
        instructions
        |> List.filter (fun instruction ->
            match instruction with
            | Set (_, "b") -> false
            | _ -> true)
        |> List.append [ Set(Signal valueB, "b") ]
        |> run Map.empty
    circuit.["a"]

[<EntryPoint>]
let main argv =
    let va = part1 instructions
    printfn "Part 1. Value on wire a: %i" va
    let va' = part2 va instructions
    printfn "Part 2. Value on wire a: %i" va'
    0

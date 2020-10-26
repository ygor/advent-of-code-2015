open System.IO
open Extensions

type Wire = string

type Signal = uint16 option

let signal n = Some <| uint16 n

type Source =
    | Signal of Signal
    | Wire of string
    | Gate of Gate

and Gate =
    | Unary of Source * (Signal -> Signal)
    | Binary of Source * Source * (Signal -> Signal -> Signal)

module Gates =
    let AND src1 src2 =
        Binary(src1, src2, (fun s1 s2 -> (s1, s2) |> Option.bind2 (fun v1 v2 -> Some(v1 &&& v2))))

    let OR src1 src2 =
        Binary(src1, src2, (fun s1 s2 -> (s1, s2) |> Option.bind2 (fun v1 v2 -> Some(v1 ||| v2))))

    let LSHIFT src value =
        Unary(src, (fun s -> s |> Option.bind (fun v -> Some(v <<< value))))

    let RSHIFT src value =
        Unary(src, (fun s -> s |> Option.bind (fun v -> Some(v >>> value))))

    let NOT src =
        Unary(src, (fun s -> s |> Option.bind (fun v -> Some(~~~v))))

type Circuit =
    { Wires: Map<Wire, Signal>
      Connections: Map<Wire, Source> }

    static member Default =
        { Wires = Map.empty<Wire, Signal>
          Connections = Map.empty<Wire, Source> }

    member circuit.addWireIfNotExists wire =
        match Map.tryFind wire circuit.Wires with
        | Some _ -> circuit
        | None -> { circuit with Wires = Map.add wire None circuit.Wires }

    member circuit.addWireOrUpdate wire signal =
        match Map.tryFind wire circuit.Wires with
        | Some s -> { circuit with Wires = circuit.Wires.Remove(wire).Add(wire, signal) }
        | None -> { circuit with Wires = circuit.Wires.Add(wire, signal) }

    member circuit.connect wire (source: Source) =
        { circuit.addWireIfNotExists wire with Connections = Map.add wire source circuit.Connections }

let circuit =
    File.ReadAllLines(@"input.txt")
    |> Seq.fold (fun (circuit: Circuit) (line: string) ->
        match line with
        | Regex "NOT ([a-z]+) -> ([a-z]+)" [ in'; out ] ->
            circuit.connect out (Gate <| Gates.NOT(Wire in'))
        | Regex "([a-z]+) -> ([a-z]+)" [ in'; out ] ->
            circuit.connect out (Wire in')
        | Regex "([0-9]+) -> ([a-z]+)" [ in'; out ] ->
            circuit.connect out (Signal <| Some(uint16 in'))
        | Regex "([a-z]+) AND ([a-z]+) -> ([a-z]+)" [ in1; in2; out ] ->
            circuit.connect out (Gate <| Gates.AND (Wire in1) (Wire in2))
        | Regex "([0-9]+) AND ([a-z]+) -> ([a-z]+)" [ in1; in2; out ] ->
            circuit.connect out (Gate <| Gates.AND (Signal <| signal in1) (Wire in2))
        | Regex "([a-z]+) OR ([a-z]+) -> ([a-z]+)" [ in1; in2; out ] ->
            circuit.connect out (Gate <| Gates.OR (Wire in1) (Wire in2))
        | Regex "([a-z]+) LSHIFT ([0-9]+) -> ([a-z]+)" [ in1; in2; out ] ->
            circuit.connect out (Gate <| Gates.LSHIFT (Wire in1) (int in2))
        | Regex "([a-z]+) RSHIFT ([0-9]+) -> ([a-z]+)" [ in1; in2; out ] ->
            circuit.connect out (Gate <| Gates.RSHIFT (Wire in1) (int in2))
        | x -> failwithf "Invalid instruction: %A" x) Circuit.Default

let rec evaluateWire (circuit: Circuit) wire =
    match circuit.Wires.[wire] with
    | Some _ -> circuit
    | None ->
        let source = circuit.Connections.[wire]
        let (signal: Signal), (circuit: Circuit) = evaluateSource source circuit
        circuit.addWireOrUpdate wire signal

and evaluateSource (source: Source) (circuit: Circuit) =
    match source with
    | Signal signal -> signal, circuit
    | Wire wire ->
        let circuit = evaluateWire circuit wire
        let signal = circuit.Wires.[wire]
        signal, circuit
    | Gate (Unary (source, f)) ->
        let signal, circuit = evaluateSource source circuit
        f signal, circuit
    | Gate (Binary (s1, s2, f)) ->
        let s1, circuit = evaluateSource s1 circuit
        let s2, circuit' = evaluateSource s2 circuit
        f s1 s2, circuit'

let evaluate (circuit: Circuit) =
    Map.keys circuit.Wires |> Seq.fold evaluateWire circuit

let part1 wire circuit =
    let circuit = evaluate circuit
    circuit.Wires.[wire]

[<EntryPoint>]
let main argv =
    let signalA = part1 "a" circuit
    match signalA with
    | Some v -> printfn "Part 1. Value on wire a: %i" v
    | None -> printfn "Part 1. No signal on wire a"
    0
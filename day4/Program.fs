open System.IO

let secret = File.ReadLines(@"input.txt") |> Seq.head

let md5Hex (message: string) =
    use md5 = System.Security.Cryptography.MD5.Create()
    message
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let startsWith (start:string) message =
    let hash = md5Hex message
    hash.Substring(0, start.Length) = start

let mine secret validator =
    1
    |> Seq.unfold (fun i ->
        if validator (secret + (string i)) then None else Some(i, i + 1))
    |> Seq.last
    |> (+) 1

[<EntryPoint>]
let main argv =
    mine secret (startsWith "00000") |> printfn "Part 1: hash is found for number: %i"
    mine secret (startsWith "000000") |> printfn "Part 2: hash is found for number: %i"
    0
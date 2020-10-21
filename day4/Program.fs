open System.IO

let secret = File.ReadLines(@"input.txt") |> Seq.head

let computeHash (message: string) =
    use md5 = System.Security.Cryptography.MD5.Create()
    message
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let startsWithZeros num message =
    let hash = computeHash message
    let start = [ 1 .. num ] |> Seq.fold (fun acc _ -> acc + "0") ""
    hash.Substring(0, num) = start

let mine secret validator =
    1
    |> Seq.unfold (fun i ->
        if validator (secret + (string i)) then None else Some(i, i + 1))
    |> Seq.last
    |> (+) 1

[<EntryPoint>]
let main argv =
    mine secret (startsWithZeros 5) |> printfn "Part 1: hash is found for number: %i"
    mine secret (startsWithZeros 6) |> printfn "Part 2: hash is found for number: %i"
    0
module Extensions
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
        
module Map =
    let keys (map: Map<'a, 'b>) =
        map |> Seq.map (fun keyValue -> keyValue.Key)

module Option =
    let bind2 binder =
        function
        | Some v1, Some v2 -> binder v1 v2
        | _ -> None

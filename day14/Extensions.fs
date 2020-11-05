module Extensions

open System.Text.RegularExpressions

module Tuple =
    let rev (a, b) = b, a

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module List =
    let rec distribute e =
        function
        | [] -> [ [ e ] ]
        | x :: xs' as xs ->
            (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

    let rec permute =
        function
        | [] -> [ [] ]
        | e :: xs -> List.collect (distribute e) (permute xs)

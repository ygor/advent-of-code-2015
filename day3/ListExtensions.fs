module day3.ListExtensions

module List =
    let prepend2 (x, y) (xs, ys) = x::xs, y::ys

    let rec splitPairwise = function
        | [] | [_] as xs -> xs, []
        | x::y::xs -> prepend2 (x, y) (splitPairwise xs)  

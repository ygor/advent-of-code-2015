module Extensions

open System.Text.RegularExpressions

module Tuple =
    let sort (x,y) = (min x y, max x y)

module List =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)

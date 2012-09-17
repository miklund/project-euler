module E020
let factorial n : bigint = List.fold (*) 1I [1I .. n]

let sum (n:bigint) =
    n |> string |> Seq.fold (fun acc x -> acc + System.Int32.Parse(x.ToString())) 0

let solution = lazy (sum (factorial 100I))

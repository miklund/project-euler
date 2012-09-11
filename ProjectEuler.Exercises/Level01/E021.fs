// http://projecteuler.net/index.php?section=problems&id=21

module E021

let d n =
    [1..(n - 1)] |> List.filter (fun x -> n % x = 0) |> List.sum

let amicable n =
    [1..n] |> List.filter (fun x -> x = (d (d x)) && x <> (d x))

let solution = lazy (amicable 10000 |> List.sum)
Utilities.measure_execution_time solution |> ignore
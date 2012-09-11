// http://projecteuler.net/index.php?section=problems&id=28
module E028

// 1
// 1 + 3 + 5 + 7 + 9
// 1 + 3 + 5 + 7 + 9 + 13 + 17 + 21 + 25

let dimension start d =
    let width = (d * 2) - 1
    let increment i = start + ((width - 1) * i)
    let next = [1..4] |> List.map (increment)
    next |> List.sum, next.[3]

let rec calc i =
    match i > 2 with
    | true -> 
        let previous = calc (i - 1)
        let current = dimension (snd previous) i
        (fst current) + (fst previous), (snd current)
    | false -> 25, 9

let solution = lazy (calc 501)
Utilities.measure_execution_time solution |> ignore
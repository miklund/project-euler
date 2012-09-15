module E043

let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let number = List.reduce (fun acc n -> acc * 10 + n)
let number64 = List.fold (fun acc n -> acc * 10L + int64(n)) 0L

let divisibleBy b a = a % b = 0

let criteria v =
    let [_; v2; v3; v4; v5; v6; v7; v8; v9; v10] = v
    number([v2; v3; v4]) |> divisibleBy 2 &&
    number([v3; v4; v5]) |> divisibleBy 3 &&
    number([v4; v5; v6]) |> divisibleBy 5 &&
    number([v5; v6; v7]) |> divisibleBy 7 &&
    number([v6; v7; v8]) |> divisibleBy 11 &&
    number([v7; v8; v9]) |> divisibleBy 13 &&
    number([v8; v9; v10]) |> divisibleBy 17

// very brute forceish
let solution = permutations [0..9] |> Seq.filter criteria |> Seq.fold (fun acc n -> acc + number64(n)) 0L
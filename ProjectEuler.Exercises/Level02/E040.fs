module E040

// expand 123
// -> [1, 2, 3]
let rec expand = function | 0 -> [] | n -> expand (n / 10) @ [n % 10]

// champernowne |> Seq.take(15) |> Seq.toList
// -> [1; 2; 3; 4; 5; 6; 7; 8; 9; 1; 0; 1; 1; 1; 2; 1; 3; 1; 4; 1]
let champernowne = 
    let rec _champernowne i = seq {
            yield! expand i
            yield! _champernowne (i + 1)
        }
    _champernowne(0) |> Seq.cache

// solution = d[1] * d[10] * d[100] * d[1000] * d[10000] * d[100000] * d[1000000]
let solution = lazy ( [0; 9; 99; 999; 9999; 99999; 999999] 
                    |> List.reduce (fun acc n -> acc * Seq.nth n champernowne) )
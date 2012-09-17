module E034

/// Factorial, example: fact 3 -> 6
let fact n = [1..n] |> List.fold (*) 1

// Precalculate factorials 0-9
let factorials = [0..9] |> List.map fact |> List.toArray

/// Get number as list of digits, example: 123 -> [3; 2; 1]
let rec digits = function
    | 0 -> []
    | n -> (n % 10) :: digits (n / 10)

/// Let you know if a number is curious, example: 145 -> true
let curious n = n = (digits n |> List.map (fun x -> factorials.[x]) |> List.fold (+) 0)

/// Find all curious numbers between 3 and 1 million
let findCurious() = 
    [3..50000] 
    |> List.filter curious
    |> List.sum

// Measure
let solution = lazy ( findCurious() )
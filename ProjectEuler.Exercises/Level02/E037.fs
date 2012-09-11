module E037

/// Remove digit from the left
/// Example: truncateLeft 3797 -> 797
let truncateLeft n = if n > 10 then Some(n % (int (10. ** (floor (log10 (float n)))))) else None

/// Remove digit from the right
/// Example: truncateRight 3797 -> 379
let truncateRight n = if n > 10 then Some(n / 10) else None

/// Int compatible square root
/// Example: sqrtn 16 -> 4
let sqrtn = float >> sqrt >> int

/// Determine if number is prime (not very effective)
let isPrime = function
    | 1 -> false
    | 2 | 3 | 5 | 7 -> true
    | n -> 2 :: [3..2..(sqrtn n)] |> List.exists (fun x -> n % x = 0) |> not

/// Returns true if n is prime when we truncate from left and right
let rec isPrimeTruncated n =
    let rec allPrimes fn n = 
        match fn n with 
        | None -> true 
        | Some(a) -> (isPrime a) && (allPrimes fn a)
    (isPrime n) && (allPrimes truncateLeft n) && (allPrimes truncateRight n)

/// Only check numbers that starts with 3, 7, 9 or ends with 3, 7, 9
let ofInterest n = 
    let startsWith n = n / ((float >> log10 >> floor >> (( ** ) 10.) >> int) n)

    // Ends with 3, 7 or 9
    ([3; 7; 9] |> List.exists ((=) (n % 10))) &&

    // Starts with 
    ([2; 3; 5; 7; 9] |> List.exists ((=) (startsWith n)))

/// Go find all eleven combinations
/// Example: findAllEleven(11, 0) -> [...]
let rec findAllEleven = function
    | _, 11 -> []
    | n, i when (n |> ofInterest) && (isPrimeTruncated n) -> n :: findAllEleven(n + 1, i + 1)
    | n, i -> findAllEleven(n + 1, i)

let solution = lazy ( findAllEleven(11, 0) |> List.sum )
Utilities.measure_execution_time solution |> ignore
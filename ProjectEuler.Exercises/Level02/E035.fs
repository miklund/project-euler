module E035

/// Get number as list of digits, example: 123 -> [3; 2; 1]
let rec digits = function
    | 0 -> []
    | n -> digits (n / 10) @ [(n % 10)]

/// Int compatible square root, example: sqrtn 16 -> 4
let sqrtn = float >> sqrt >> int

/// Determine if number is prime (not very effective)
let isPrime n = 2 :: [3..2..(sqrtn n)] |> List.exists (fun x -> n % x = 0) |> not

/// Number of digits in number, example: len 1234 -> 4
let len = float >> log10 >> ceil >> int

/// Take a list of digits and make it a number, example: join [1; 2; 3] -> 123
let join digitList = List.rev digitList |> List.mapi (fun i x -> x * (pown 10 i)) |> List.fold (+) 0

// Rotate number n, x steps
let rotate x n = n |> digits |> List.permute (fun i -> (i + x) % (len n)) |> join

// Get all rotated permutations for n
let permutations n = seq { for x in 0..(len n)-1 do yield rotate x n }

// Is n a circular prime?
let circularPrime n = (isPrime n) && (permutations n |> Seq.forall isPrime)

// Get the circular primes, n<10 are precalculated
let allCircularPrimes max = 
    let topSequence =
        seq { for n in 11..2..max do
                if circularPrime n then yield n
            }
    Seq.append [| 2; 3; 7; 9 |] topSequence

let solution = lazy ( allCircularPrimes 1000000 |> Seq.length )
Utilities.measure_execution_time solution |> ignore
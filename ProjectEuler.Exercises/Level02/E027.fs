// http://projecteuler.net/index.php?section=problems&id=27
module E027

// represents an a b pair in the quadratics formula
type Pair = { a : int; b : int }

// n^2 + an + b where |a| < 1000 && |b| < 1000
let quadratics pair n = (n * n) + (pair.a * n) + pair.b

// b is a prime
let primes = lazy (
    let rec primes_inner = function
        | head :: tail -> head :: primes_inner (tail |> List.filter (fun x -> x % head <> 0))
        | _ -> [] 
    primes_inner [2..1000]
    )
    
// Find consecituve primes from result calculated by the quadratics function
//consecutive_primes 0 (quadratics 1 41)
let rec consecutive_primes n fn_quadratics = 
    let sqrt_int x = x |> float |> sqrt |> int

    let isPrime x = 
        let root = sqrt_int x
        (x > 1) && not (primes.Value |> Seq.exists (fun y -> y <= root && x % y = 0))

    match isPrime (fn_quadratics n) with
    | true  -> 1 + consecutive_primes (n + 1) fn_quadratics
    | false -> 0;

// Return largest of the two
let (><) left right = 
    let calc pair = (consecutive_primes 0 (quadratics pair))
    if (calc left) > (calc right) then left else right
        
// Represent a range of numbers
type Range = { min : int; max : int }
    with member x.diff() = x.max - x.min

// Find the largest by divide & conquer
// divide_and_conquer {min = -1000; max = 1000} { min = -1000; max = 1000}
let rec divide_and_conquer (a : Range) (b : Range) =    
    let first_half range = {min = range.min; max = range.min + range.diff() / 2}
    let second_half (range : Range) = {min = min (1 + range.min + range.diff() / 2) range.max; max = range.max}

    match a.diff(), b.diff() with
    | 0, 0 -> {a = a.min; b = b.min}
    | 0, 1 -> (divide_and_conquer a {min = b.min; max = b.min}) >< (divide_and_conquer a {min = b.max; max = b.max})
    | 1, 0 -> (divide_and_conquer {min = a.min; max = a.min} b) >< (divide_and_conquer {min = a.max; max = a.max} b)
    | _, _ -> (divide_and_conquer (first_half a) (first_half b)) ><
              (divide_and_conquer (first_half a) (second_half b)) ><
              (divide_and_conquer (second_half a) (first_half b)) ><
              (divide_and_conquer (second_half a) (second_half b))

let most_consecutive_primes = lazy(
    Async.Parallel [ for i in -1..0 -> async { return divide_and_conquer {min = 1000 * i; max = 1000 * (i + 1)} {min = 2; max = 1000} } ]
    |> Async.RunSynchronously
    |> Seq.fold (><) {a = 0; b = 0})
    
let solution = lazy ( 
    let mcp = most_consecutive_primes.Value
    mcp.a * mcp.b 
    )

Utilities.measure_execution_time solution |> ignore
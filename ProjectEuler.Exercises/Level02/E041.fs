module E041

// expand 123
// -> [1, 2, 3]
let rec expand = function | 0 -> [] | n -> expand (n / 10) @ [n % 10]

// is_pandigital 2143
// -> true
let is_pandigital i = 
    let number = (expand i)
    [1..number.Length] |> List.forall (fun n -> number |> List.exists ((=) n))

// is_prime 2143
// -> true
let is_prime i =
    let sqrtn = float >> sqrt >> int 
    seq { 2..sqrtn(i) } |> Seq.exists (fun n -> i % n = 0) |> not

// starting at 7654321, counting down each pandigital that also is a prime
let pandigitals = 
    let rec _pandigitals i = seq {
        match i with
        | i when is_pandigital(i) && is_prime(i) -> yield i
        | _ -> yield! _pandigitals (i - 1)
    }
    _pandigitals(7654321)

let solution = lazy ( pandigitals |> Seq.nth(0) )
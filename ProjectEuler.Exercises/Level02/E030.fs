// http://projecteuler.net/index.php?section=problems&id=30
module E030

let rec powers start exponent = 
    let rec listify n =
        match n with
        | 0 -> []
        | _ -> listify (n / 10) @ [(n % 10)]

    let power_sum n exp =
        (listify n) |> List.map (fun x -> pown x exp) |> List.sum
        
    // Max limit is where n * 9^exp is larger than 10^n
    let rec limit n exp = function
        | true -> limit (n + 1) exp ((n * pown 9 exp) > (pown 10 n))
        | false -> (n - 1) * pown 9 exp
    
    [start..(limit 1 exponent true)]
    |> List.filter ( fun x -> (power_sum x exponent) = x )

let power n = 
    powers 10 n

let solution = lazy ( power 5 |> List.sum )
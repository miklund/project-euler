//http://projecteuler.net/index.php?section=problems&id=3
//The prime factors of 13195 are 5, 7, 13 and 29.
//What is the largest prime factor of the number 600851475143 ?
module E003
let sqrt_int (x : int64) = x |> float |> sqrt |> int64 
let rec factor (n : int64) = 
    let max = sqrt_int n
    [1L..max] |> 
        List.filter (fun x -> (n % x = 0L) && (x = 1L || (factor x) = 1)) 
        |> List.max 
        |> int

let solution = lazy (factor 600851475143L)
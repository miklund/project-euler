﻿//http://projecteuler.net/index.php?section=problems&id=2
//Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
//1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
//Find the sum of all the even-valued terms in the sequence which do not exceed four million.
module E002

let rec fibonacci n1 n2 limit =
    let next = n1 + n2
    if next > limit then
        []
    else
        next :: (fibonacci n2 next limit)

let even n = n % 2 = 0

let solution = [1; 1] @ fibonacci 1 1 4000000 |> List.filter even |> List.sum
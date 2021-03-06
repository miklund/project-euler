﻿//The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
//Find the sum of all the primes below two million.

module E010

let rec sum_primes top (primes:list<int>) : int64 =
    let root = top |> float |> sqrt |> int

    if (primes.Head < root) then
        match primes with
        | head :: tail -> (head |> int64) + sum_primes top (tail |> List.filter (fun x -> x % head <> 0))
        | [] -> 0L // Should never happen
    else
        primes |> List.map (fun x -> x |> int64) |> List.sum

let solution = lazy (sum_primes 2000000 [2..2000000])
//The following iterative sequence is defined for the set of positive integers:
//
//n  n/2 (n is even)
//n  3n + 1 (n is odd)
//
//Using the rule above and starting with 13, we generate the following sequence:
//
//13  40  20  10  5  16  8  4  2  1
//It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
//
//Which starting number, under one million, produces the longest chain?

module E014

// Calculate a collatz sequence
let rec collatz n result =
    let number, length = result // Unpack tuple
    let next = (number, length + 1)

    match (n, n % 2L = 0L) with
    | 1L, _     -> next
    |  _, true  -> collatz (n / 2L) next
    |  _, false -> collatz (3L * n + 1L) next

// Return largest of the two
let (><) left right =
    let _, l_length = left
    let _, r_length = right
    if l_length > r_length then left else right

// One core   
//
//let solution = lazy ( [1L..1000000L] 
//    |> List.map (fun i -> collatz i (i, 0)) 
//    |> List.fold (><) (0L, 0) )
//
//or
//
//let rec calc n result = 
//    match n with
//    | 1000000L -> result
//    | _   -> calc (n + 1L) (result >< (collatz n (n,0)))
//
//let solution = lazy ( calc 1L (0L, 0) )    
//Utilities.measure_execution_time solution |> ignore

// Multi core
let rec calc n max = 
    let diff = max - n
    match diff with
    | 0L -> collatz n (n,0)
    | 1L -> (collatz n (n,0)) >< (collatz max (max,0))
    | _  -> (calc n (n + (diff / 2L))) >< (calc (1L + n + (diff / 2L)) max)

let solution = lazy (
    Async.Parallel [ for i in 0L..99L -> async { return calc ((10000L * i) + 1L) (10000L * (i + 1L)) } ]
    |> Async.RunSynchronously
    |> Seq.fold (><) (0L, 0)
    )

Utilities.measure_execution_time solution |> ignore
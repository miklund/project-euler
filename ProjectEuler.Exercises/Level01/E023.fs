// http://projecteuler.net/index.php?section=problems&id=23
// This was a bitch

module E023

// Is n an abundant number?
let abundant n =
    [1..(n / 2)] 
    |> List.filter (fun x -> n % x = 0)
    |> List.sum
    |> (fun x -> x > n)

// Get all abundants up to 28123
let abundants = [1..28123] |> List.filter abundant

let rec mapWhile mapFunction whileFunction (list:int list) =
    if list.IsEmpty then
        []
    else
        match whileFunction list.Head with
        | true -> (mapFunction list.Head) :: mapWhile mapFunction whileFunction list.Tail
        | false -> []

let rec abundant_sums list abundants = 
    match list with
    | head :: tail -> (mapWhile (fun x -> x + head) (fun x -> x <= head) abundants) @ (abundant_sums tail abundants)
    | _ -> []
 
let sums = abundant_sums abundants abundants
           |> Set.ofList |> Set.toList
           |> List.filter (fun x -> x <= 28123)
    
let solution = lazy ([1..28123] 
                    |> List.filter (fun x -> false = List.exists (fun y -> x = y) sums)
                    |> List.sum)
Utilities.measure_execution_time solution |> ignore

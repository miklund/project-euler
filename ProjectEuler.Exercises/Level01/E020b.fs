module E020b

// Needs the list to be "reversed" read from left to right
let rec plan overflow l =
    match l with
    | head :: tail ->
        let column = head + overflow
        if column > 9 then
            let next_overflow = column / 10 |> float |> System.Math.Truncate |> int
            (column - next_overflow * 10) :: plan next_overflow tail
        else
            column :: plan 0 tail
    | _ -> if overflow > 0 then 
              (overflow % 10) :: plan ((overflow - (overflow % 10)) / 10) []
           else 
              []

let add (l1 : int list) (l2 : int list) =
    l1 |> List.map2 (fun i1 i2 -> i1 + i2) l2 

let prod (l1 : int list) (l2 : int list) = 
    let tenths i = 10.0 ** (i |> float) |> int
    l2 |> List.mapi (fun i x -> l1 |> List.map (fun y -> x * y * tenths (-1 + l2.Length - i)))
    |> List.fold (fun acc l -> add acc l) (List.init l1.Length (fun i -> 0))
    |> List.rev
    |> plan 0 // from outer space
    |> List.rev

let to_list n =
    let rec calc x = 
        if x > 0 then
            (x % 10) :: calc ((x - (x % 10)) / 10)
        else
            []
    calc n |> List.rev

let solution = lazy ([2..100] |> List.map (fun x -> to_list x) |> List.fold (fun acc y -> prod acc y) [1] |> List.sum)
Utilities.measure_execution_time solution |> ignore
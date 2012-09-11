// http://projecteuler.net/index.php?section=problems&id=16
module E016

let rec calc list exp = 
    let rec evenOut overhead l = 
        match (l, overhead) with
        | [], 0 -> []
        | [], _ -> [overhead]
        | head :: tail, _ -> ((overhead + head) % 10) :: (evenOut ((overhead + head) / 10) tail)
        
    match exp with
    | 1 -> list
    | _ -> calc (list |> List.map (fun x -> x * 2) |> evenOut 0) (exp - 1)

let solution = lazy ( calc [2] 1000 |> List.sum )
Utilities.measure_execution_time solution |> ignore
    
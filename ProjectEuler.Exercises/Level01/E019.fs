// http://projecteuler.net/index.php?section=problems&id=19
module E019

let months = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]

let solution = lazy ([0..99]
        |> List.map (fun x -> months) |> List.concat // all months consecutive
        |> List.mapi (fun i x -> if i = 1 && ((i - 1) / 12) % 4 = 0 then x + 1 else x) // leap year
        |> List.fold (fun (acc, result) x -> if (acc + x) % 7 = 6 then (acc + x, result + 1) else (acc + x, result)) (0,0) // Sunday bloody sunday
    )
Utilities.measure_execution_time solution |> ignore
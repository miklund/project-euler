// http://projecteuler.net/index.php?section=problems&id=15

module E015
let rec find_path square_size x y = 
    let complete = x = square_size || y = square_size
    match complete with
    | true -> 1L
    | false -> (find_path square_size (x + 1) y) + find_path square_size x (y + 1)

// This will take some time
let solution = lazy (find_path 20 0 0)
Utilities.measure_execution_time solution |> ignore
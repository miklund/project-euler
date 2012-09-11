namespace ProjectEuler.Excercises
module Utilities =

// Calculate mean execution time
let measure_execution_time (solution : Lazy<'a>) =
    let watch = new System.Diagnostics.Stopwatch()
    watch.Start()
    solution.Force() |> ignore
    watch.Stop()
    printfn "Mean execution time: %A ms" (watch.ElapsedMilliseconds |> int)
    solution.Value
    

// Combine two lists into every possible combination
// combine [0..2] [0..2] (fun x y -> (x, y)) -> [(0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (1, 2); (2, 0); (2, 1); (2, 2)]
let combine l1 l2 fn_create = l1 |> List.collect (fun a -> (l2 |> List.map (fun b -> fn_create a b)))    

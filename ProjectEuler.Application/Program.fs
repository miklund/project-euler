let exec (solution : Lazy<'a>) = solution.Force() |> ignore

let rec Loop input =
    printf "Please type the PE number, or q to exit: " |> ignore
    match input with
    | "24" -> exec E024.solution
    | "31" -> exec E031.solution_faster
    | "32" -> exec E032.solution
    | "q"  -> System.Environment.Exit(0)
    | _ -> ()

    Loop (System.Console.ReadLine())

Loop "start"
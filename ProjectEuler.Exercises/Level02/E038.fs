module E038

// digits 123 -> [1; 2; 3]
let rec digits = function
    | 0 -> []
    | n -> digits (n / 10) @ [(n % 10)]

/// number of digits in number
/// Example: len 123 -> 3
let len = float >> log10 >> ceil >> int

/// Example: isPandigital [9; 6; 2; 7; 8; 3; 5; 4; 1] -> true
let isPandigital l = 
    l |> List.fold (fun n x -> n + x * (pown 10 (x - 1))) 0
      |> ((=) 987654321)

/// Example: isConcatenatedProduct 192 [1..3] -> true
let concatenateProduct n p =
    p |> List.map ((*) n) 
      |> List.collect (digits)

/// Example: join [1; 2; 3] -> 123
let join l = 
    l |> List.mapi (fun i x -> x * (pown 10 (l.Length - i - 1))) 
      |> List.fold (+) 0

/// Example: hasConcatenatedProduct 7269 -> (true, Some 726914538)
let hasConcatenatedProduct n =
    let rec inner_HasConcatenatedProduct n p =
        match concatenateProduct n [1..p] with
        | l when l.Length < 9 -> inner_HasConcatenatedProduct n (p + 1)
        | l when l.Length = 9 -> isPandigital l, Some(join l)
        | _ -> false, None
    inner_HasConcatenatedProduct n 2

/// Iterate to find the all the pandigital concatenated products
let find = seq { for n in [9..99999] do
                   let query, product = hasConcatenatedProduct n
                   if  query then yield n, product.Value }

/// Get the largest product
let solution = lazy (find |> Seq.map snd |> Seq.max)
Utilities.measure_execution_time solution |> ignore
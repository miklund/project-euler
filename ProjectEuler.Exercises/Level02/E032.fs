// http://projecteuler.net/index.php?section=problems&id=32
module E032

// [3; 9; 1; 8; 6; 7; 2; 5; 4] = 7254
// 39 * 186 = 7254
let pandigital_product (l : int List) =
    let rec inner_pandigital_product multiplicand (rest : int List) =
        // Log10 for int
        let log10n n = (log10 (n |> float)) |> int

        // Division with ceiling
        let (-/) t n = (ceil ((t |> float) / (n |> float))) |> int

        // to_number [1; 2] -> 12
        let to_number l = l |> List.rev |> List.mapi (fun i x -> x * pown 10 i) |> List.fold (+) 0

        // Length of multiplicand
        let multiplicand_size = 1 + log10n multiplicand

        // Take the size of multiplier
        let multiplier_size = (rest.Length -/ 2) - (multiplicand_size - 1)

        match multiplier_size, rest with
        // Stop recursion when multiplier is 0
        | 0, _ -> 0

        | _, head :: tail -> 
        // Next multiplicand
        let multiplicand_next = multiplicand * 10 + rest.Head

        // Multiplier
        let multiplier = (head :: tail) |> Seq.take multiplier_size |> Seq.toList |> to_number

        // Product
        let product = (head :: tail) |> Seq.skip multiplier_size |> Seq.toList |> to_number

        if multiplicand * multiplier = product then 
            printf "%d * %d = %d\n" multiplicand multiplier product |> ignore
            product
        else inner_pandigital_product multiplicand_next tail

        (* Invalid: Should never end up here *)
        | _, _ -> raise (new System.Exception("Invalid operation")) // Satisfy match expression
    inner_pandigital_product l.Head l.Tail

// find [] [1; 2; 3; 4; 5; 6; 7; 8; 9]
let rec find result = function
    | [] -> 
        let product = pandigital_product result
        if product > 0 then [product]
        else []
    | alphabet -> alphabet |> List.collect (fun n -> find (result @ [n]) (alphabet |> List.filter (fun x -> x <> n)))

// find [1; 2; 3; 4; 5; 6; 7; 8; 9]
let find_async alphabet =
    // No more than alphabet.Length threads
    let first_batch = alphabet |> List.map (fun x -> [x], alphabet |> List.filter (fun y -> x <> y))
    (first_batch |> List.toSeq) |> Seq.map (fun value -> async { return find (fst value) (snd value) } )
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.fold (@) []
    |> List.fold (fun acc x -> if acc |> List.exists (fun y -> x = y) then acc else x :: acc) []
    |> List.sum

let solution = lazy ( find_async [1; 2; 3; 4; 5; 6; 7; 8; 9] )
Utilities.measure_execution_time solution |> ignore
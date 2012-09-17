// http://projecteuler.net/index.php?section=problems&id=29
module E029

type Range = { min : int; max : int }

type comparison = System.IComparable

let rec calc a b =
    // ** Nice but not needed, use pown instead
    // Only for natural numbers
    //let rec powi i exp = match exp > 1 with
    //| true  -> i * powi i (exp - 1)
    //| false -> i

    // list is sorted ascending
    let rec insert (item : comparison) list =
        match list with
        | head :: tail -> 
            match item.CompareTo(head) with
            | 0 -> list
            | 1 -> head :: insert item tail
            | -1 -> item :: list
            | _ -> []
        | [] -> [item]

    let rec insert_list item list =
        match item with
        | head :: tail -> insert_list tail (insert head list)
        | [] -> list

    let row = [(b.min)..(b.max)] |> List.map (fun b -> pown (System.Numerics.BigInteger a.min) b)

    match a.min > a.max with
    | false -> insert_list row (calc {min = a.min + 1; max = a.max} b)
    | true -> []

let solution = lazy ( calc {min = 2; max = 100} {min = 2; max = 100} |> List.length )
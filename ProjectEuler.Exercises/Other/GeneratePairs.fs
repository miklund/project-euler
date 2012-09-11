﻿module GeneratePairs

let rand i = (new System.Random()).Next(i)

let removeIndex i list = 
    let rec _removeIndex = function
        | _, [] -> []
        | 0, hd :: tl -> tl
        | n, hd :: tl -> hd :: _removeIndex (n - 1, tl)
    _removeIndex (i, list)

let rec pairs index_fn = function
    | [] -> []
    | hd :: tl ->
        let index = index_fn(tl) 
        (hd, List.nth tl index) :: pairs index_fn (removeIndex index tl)

let data = ["Mikael Lundin"; "Olle Janson"; "Nisse Karlsson"; "Sven Sten"]

let result = pairs (fun list -> rand(list.Length)) data

result |> List.mapi (fun i p -> sprintf "Par %d\n%s\n%s\n" i (fst p) (snd p)) |> List.reduce (+)
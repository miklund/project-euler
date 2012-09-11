// http://projecteuler.net/index.php?section=problems&id=18

module E018

type BinaryTree =
    | Node of int * BinaryTree * BinaryTree
    | Empty

//let tree = 
//    Node (3, 
//        Node (7, 
//            Node (2, Node (8, Empty, Empty), Node (5, Empty, Empty)), Node (4, Node (5, Empty, Empty), Node (9, Empty, Empty))),
//        Node (4,
//            Node (4, Node (5, Empty, Empty), Node (9, Empty, Empty)), Node (6, Node (9, Empty, Empty), Node (3, Empty, Empty)))
//        )   

let rec calculate tree =
    match tree with
    | Node (n, left, right) -> n + List.max [(calculate left); (calculate right)]
    | Empty -> 0

let s_data = 
    [
    "75";
    "95 64";
    "17 47 82";
    "18 35 87 10";
    "20 04 82 47 65";
    "19 01 23 75 03 34";
    "88 02 77 73 07 63 67";
    "99 65 04 28 06 16 70 92";
    "41 41 26 56 83 40 80 70 33";
    "41 48 72 33 47 32 37 16 94 29";
    "53 71 44 65 25 43 91 52 97 51 14";
    "70 11 33 28 77 73 17 78 39 68 17 57";
    "91 71 52 38 17 14 91 43 58 50 27 29 48";
    "63 66 04 68 89 53 67 30 73 16 69 87 40 31";
    "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23";
    ]

let rec data_tree (data : string list) row i =
    let data_row row_number = data.[row_number].Split(' ') |> Seq.map (fun s -> System.Int32.Parse(s)) |> Seq.toList
    match row with
    | 15 -> Empty
    | _ -> Node ((data_row row).[i], data_tree data (row + 1) i, data_tree data (row + 1) (i + 1))

let solution = lazy (data_tree s_data 0 0 |> calculate)
Utilities.measure_execution_time solution |> ignore
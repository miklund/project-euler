module E043b

// wide depth lft rgt
// depth: the length of result lists
// lft: the left list (default [])
// rgt: the right list (default items we want to permute)
//
// example: wide 2 [] [1; 2; 3]
// -> [[1; 2]; [1; 3]; [2; 1]; [2; 3]; [3; 2]; [3; 1]]
let rec wide depth lft = function
    | []       -> []
    | hd :: tl -> (deep hd (depth, lft @ tl)) @ (wide depth (hd :: lft) tl)

and deep x = function
    | 1, _ | _, [] -> [[x]]
    | d, l         -> List.map (fun i -> x :: i) (wide (d - 1) [] l)

// permute length items, utility method to wide/deep
//
// example: permute 2 [1..3]
// -> [[1; 2]; [1; 3]; [2; 1]; [2; 3]; [3; 2]; [3; 1]]
let permute length = wide length []

// divisibleBy 7 14
// -> true
let divisibleBy d n = n % d = 0

// toNumber [1; 2; 3]
// -> 123
let toNumber = List.reduce (fun acc n -> acc * 10 + n)

// toLog [1; 2; 3]
// -> 123L
let toLong = List.fold (fun acc n -> acc * 10L + int64(n)) 0L

// difference [1..3] [1; 3]
// [2]
let difference left right = left |> List.filter (fun n -> right |> List.exists ((=) n) |> not)

// appendPandigital [[1; 3; 5; 7; 9]]
// -> [[0; 1; 3; 5; 7; 9]; [2; 1; 3; 5; 7; 9]; [4; 1; 3; 5; 7; 9]; [6; 1; 3; 5; 7; 9]; [8; 1; 3; 5; 7; 9]]
let appendPandigital = List.map (fun pandigital -> List.map (fun i -> i :: pandigital) (difference [0..9] pandigital)) >> List.concat

// take 3 [1..9]
let rec take n l = match n, l with | 0, _ -> [] | n, x :: xs -> x :: take (n - 1) xs

// filterDivisibleBy 2 [[1]; [2]; [3]; [4]; [5]; [6]; [7]; [8]; [9]]
// -> [[2]; [4]; [6]; [8]]
let filterDivisibleBy n = List.filter (take 3 >> toNumber >> divisibleBy n)

// work from behind, all permutations of [0..9] with 3 length
// filter divisible by 17, 13, 11, 7, 5, 3, 2 and append a new pandigital for each iteration
let solution = lazy ( [17; 13; 11; 7; 5; 3; 2] 
               |> List.fold (fun acc n -> acc |> filterDivisibleBy n |> appendPandigital) (permute 3 [0..9])
               |> List.map (toLong)
               |> List.sum
               )
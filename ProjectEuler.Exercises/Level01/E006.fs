//The sum of the squares of the first ten natural numbers is,
//
//1^2 + 2^2 + ... + 10^2 = 385
//The square of the sum of the first ten natural numbers is,
//
//(1 + 2 + ... + 10)^2 = 55^2 = 3025
//Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.
//
//Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

module E006
let sum_of_squares list = list |> List.map (fun x -> (x |> float) ** 2.0) |> List.sum |> int
let square_of_sums list = (list |> List.sum |> float) ** 2.0 |> int

let solution = lazy ((square_of_sums [1..100]) - (sum_of_squares [1..100]))
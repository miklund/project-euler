//http://projecteuler.net/index.php?section=problems&id=5
//2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
//What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
module E005

(**
 * Gives true when n is evenly divisible for each number in list
 *
 * @method EvenlyDivisible
 * @param {int list} list
 * @param {int} n
 * @return {bool} True when number is evenly divisible for each number in list, else false
 *)
let EvenlyDivisible list n = list |> List.forall ((%) n >> (=) 0)

(**
 * Active pattern for Divisible/Undivisible
 * 
 * @method Divisible|Undivisible
 * @param {int} n
 * @return {Choice<unit,unit> Divisible when n is evenly divisible with every number up to 20, else Undivisible
 *)
let (|Divisible|Undivisible|) n = 
    if (EvenlyDivisible [11;12;13;14;15;16;17;18;19;20] n) then Divisible 
    else Undivisible

(**
 * Find the first number that is "Divisible"
 * 
 * @method find
 * @param {int} n
 * @return {int} First number that is considered "Divisible"
 *)
let rec find n =
    match n with
    | Divisible -> n
    | _ -> find (n + 1)

let solution = lazy (find 20)
    
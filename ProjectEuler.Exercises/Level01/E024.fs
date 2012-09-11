// http://projecteuler.net/index.php?section=problems&id=24
module E024

(*
LEXICOGRAPHIC PERMUTATION EXPANDING EXECUTION TREE
lexicographic_permutation [] [0; 1; 2]
-> lexicographic_permutation [0] [1; 2]
   -> lexicographic_permutation [0; 1] [2]
      -> lexicographic_permutation [0; 1; 2] [] // -- 012
   -> lexicographic_permutation [0; 2] [1]
      -> lexicographic_permutation [0; 2; 1] [] // -- 021
-> lexicographic_permutation [1] [0; 2]
   -> lexicographic_permutation [1; 0] [2]
      -> lexicographic_permutation [1; 0; 2] [] // -- 102
   -> lexicographic_permutation [1; 2] [0]
      -> lexicographic_permutation [1; 2; 0] [] // -- 120
-> lexicographic_permutation [2] [0; 1]
   -> lexicographic_permutation [2; 0] [1]
      -> lexicographic_permutation [2; 0; 1] [] // -- 201
   -> lexicographic_permutation [2; 1] [0]
      -> lexicographic_permutation [2; 1; 0] [] // -- 210
*)

// lexicographic_permutation [] [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] 1 1000000;;
// word is result
// alphabet is what should be permutated
// num is a counter starting with 1
// find is what we're looking for
let rec lexicographic_permutation word alphabet num find =
    // We need factorial to calculate the num correctly
    let factorial n = List.fold ( * ) 1 [1 .. n]

    let collect values =
        values |> List.collect (fun part -> 
            let newWord, letter, newNum = part
            lexicographic_permutation newWord (alphabet |> List.filter (fun x -> x <> letter)) newNum find) 

    // Calculate the next index in the map (i * number of permutations in recursion)
    let nextIndex i = (num + ((i * factorial alphabet.Length) / alphabet.Length))

    // We're only interested in finding the number we're looking for, don't go down paths we don't have to
    let withinFocus = (num <= find) && (num + (factorial alphabet.Length) > find)
        
    match alphabet, withinFocus with
    | [], _    -> word
    | _, true  -> (alphabet |> List.mapi (fun i x -> (word @ [x], x, nextIndex i))) |> collect
    | _, false -> []
  
let solution = lazy ( lexicographic_permutation [] [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] 1 1000000
                      |> List.fold ( fun acc x -> acc + x.ToString() ) "" )

Utilities.measure_execution_time solution |> ignore
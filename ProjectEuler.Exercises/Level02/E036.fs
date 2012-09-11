module E036

/// Split a string into two, omit middle character if string length is odd
/// Example: split "abcd" -> "ab", "cd"
/// Example: split "abc" -> "a", "c"
let split s = 
    let (|Odd|Even|) (s : string) = if s.Length % 2 = 0 then Even else Odd
    match s with
    | Odd  -> s.[..(-1 + (s.Length - 1) / 2)], s.[(s.Length + 1) / 2..]
    | Even -> s.[..(-1 + s.Length / 2)], s.[s.Length / 2..]
   
/// Reverse a string
/// Example: rev "abc" -> "cba"
let rev (s : string) = 
    let rec inner_rev ca =
        match ca with
        | [||] -> ""
        | s -> (inner_rev s.[1..]) + s.[0].ToString()
    inner_rev(s.ToCharArray())

/// Is string palindromic
/// Example: palindromic "lol" -> true
let palindromic s =
    let s1, s2 = split s
    s1 = rev s2
    
/// Is the number palindromic in both 10 base and 2 base
/// Example: isDualPalindromic 585 -> true
let isDualPalindromic (n : int) = (palindromic (n.ToString())) && (palindromic (System.Convert.ToString(n, 2)))

/// Get the sum of all dual palindromic number up to max
let sumOfAllDualPalindromic max = 
    [1..2..max] 
    |> List.filter isDualPalindromic
    |> List.sum

let solution = lazy ( sumOfAllDualPalindromic 1000000 )
Utilities.measure_execution_time solution |> ignore
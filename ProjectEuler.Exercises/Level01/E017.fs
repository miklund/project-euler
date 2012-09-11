// http://projecteuler.net/index.php?section=problems&id=17
module E017
let dictionary = 
    [
    (1000, "one thousand"); 
    (90, "ninety");
    (80, "eighty");
    (70, "seventy");
    (60, "sixty");
    (50, "fifty");
    (40, "forty");
    (30, "thirty");
    (20, "twenty");
    (19, "nineteen");
    (18, "eighteen");
    (17, "seventeen");
    (16, "sixteen");
    (15, "fifteen");
    (14, "fourteen");
    (13, "thirteen");
    (12, "twelve");
    (11, "eleven");
    (10, "ten");
    (9, "nine");
    (8, "eight");
    (7, "seven");
    (6, "six");
    (5, "five");
    (4, "four");
    (3, "three");
    (2, "two");
    (1, "one");
    ];


let rec translate (n:int) : string =
    if n = 0 then
        System.String.Empty
    elif dictionary |> List.exists (fun (x, s) -> x = n) then
        snd (dictionary |> List.find (fun (x, s) -> n = x)) + " "
    else
        let log10 x = x |> float |> System.Math.Log10 |> System.Math.Truncate
        let first_number x = System.Math.Truncate((x |> float) / (10.0 ** log10 x))
        let part x = (first_number x) * 10.0 ** log10 x |> int

        if 2.0 = log10 n then
            let rest = translate (n - part n)
            translate (first_number n |> int) + "hundred " + if System.String.IsNullOrWhiteSpace(rest) then "" else "and " + rest
        else
            (translate (part n)) + (translate (n - (part n)))
            
let solution = lazy ((List.fold (fun acc n -> acc + translate n) "" [1..1000]).Replace(" ", System.String.Empty).Length)
Utilities.measure_execution_time solution |> ignore
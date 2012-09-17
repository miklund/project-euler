module E033

/// Get the nth digit in a number, example: nth 2 456 -> 5
let nth n number = 
    let f_n = float n
    let f_number = float number
    let f_value = (f_number / (10. ** (floor ((log10 f_number) - f_n)))) % 10.
    f_value |> int

type Fraction = { Nominator : int; Denominator : int }

/// Get the fraction value nominator/denominator
let fractionValue(fraction) = (float fraction.Nominator) / (float fraction.Denominator)

/// Is this a curious fraction, Example: isCuriousFraction { Nominator = 49; Denominator = 98 } -> true
let isCuriousFraction(fraction) =
    let value = fractionValue fraction
    
    // Avoid division by zero
    (nth 1 fraction.Denominator) <> 0 &&

    // Second digit in Nominator == First digit in Denominator
    (nth 1 fraction.Nominator) = (nth 0 fraction.Denominator) &&

    // Value is the same after cancelling
    value = fractionValue { Nominator = (nth 0 fraction.Nominator); Denominator = (nth 1 fraction.Denominator) }

/// A simple fraction ends with zeroes, Example: isSimpleFraction { Nominator = 40; Denominator = 90 } -> true
let isSimpleFraction(fraction) =
    let value = fractionValue fraction
    value = fractionValue { Nominator = (nth 1 fraction.Nominator); Denominator = (nth 1 fraction.Denominator) }

/// Extend the type Fraction with Curious and Simple member methods
type Fraction with
    member x.Curious = isCuriousFraction x
    member x.Simple = isSimpleFraction x
    member x.Value = fractionValue x

/// Get possible fractions that are curious but not simple
let fractions = 
    seq { for a in [10..98] do
            for b in [a + 1..99] do
              yield { Nominator = a; Denominator = b }
    } |> Seq.filter (fun f -> f.Curious && not f.Simple)

// If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
let solution = lazy ( fractions |> Seq.fold (fun x fract -> x * fract.Value) 1. )
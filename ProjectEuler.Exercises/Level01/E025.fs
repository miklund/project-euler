// http://projecteuler.net/index.php?section=problems&id=25
module E025

let rec fibonacci n1 n2 digits =
    let next = n1 + n2
    if next > System.Numerics.BigInteger.Pow(10I, digits - 1) then
        3 // Because n1 is first term and n2 is second term
    else
        1 + fibonacci n2 next digits

let solution = lazy (fibonacci 1I 1I 1000)
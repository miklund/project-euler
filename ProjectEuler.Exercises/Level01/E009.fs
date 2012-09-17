//A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
//a^2 + b^2 = c^2
//For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
//There exists exactly one Pythagorean triplet for which a + b + c = 1000.
//Find the product abc.

module E009

type Triplet = { a : int; b : int; c : int }

let pythagorean_triplet triplet = 
    triplet.a * triplet.a + triplet.b * triplet.b = triplet.c * triplet.c 
    && triplet.a + triplet.b + triplet.c = 1000

let increase triplet =
    if (triplet.a + 1) < triplet.b then
        // a + 1 < b then increase a
        { a = triplet.a + 1; b = triplet.b; c = triplet.c }
    else
        if (triplet.b + 1) < triplet.c then
            // b + 1 < c then increase b
            { a = 0; b = triplet.b + 1; c = triplet.c }
        else
            // only possible to increase c
            { a = 0; b = 1; c = triplet.c + 1 }    
        
let rec find_pythagorean_triplet triplet =
    match pythagorean_triplet triplet with
    | true -> triplet
    | false -> find_pythagorean_triplet (increase triplet)

let solution = lazy ( find_pythagorean_triplet {a=0;b=1;c=2} )

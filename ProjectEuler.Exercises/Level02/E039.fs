module E039

// a^2 + b^2 = c^2
let pythagoras a b = sqrt(a ** 2.0 + b ** 2.0)

// get permutations of a, b
let permutations p = [1.0..p-2.0] |> List.collect (fun a -> [a..p-2.0] |> List.map (fun b -> a, b))

// is tuple t with pythagoras of t.1 t.2 equal to p?
let criteria p t =
    let a, b = t
    a + b + (pythagoras a b) = p

// filter out all permutations that matches the criteria
let solutions length = (permutations length) |> List.filter (criteria length)

// get the p with most triangles
let result = [1.0..999.0] |> List.maxBy (fun n -> (solutions n).Length)
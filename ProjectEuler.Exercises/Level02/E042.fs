module E042

// triangle
let t n = n * (n + 1) / 2

// sequence of all triangles growing n
let triangles =
    let rec _triangles n = seq {
            yield t n
            yield! _triangles (n + 1) 
        }
    _triangles(1) |> Seq.cache

// path to words
let path = "C:\Users\milu\Documents\Visual Studio 2010\Projects\projecteuler\ProjectEuler.Exercises\Level02\E042_words.txt"

// words as an array
let words =
    use reader = new System.IO.StreamReader(path)
    reader.ReadToEnd().Split(',') |> Array.map (fun s -> s.Trim().Replace("\"", ""))

// is triangle of word a member of sequence triangles?
let is_triangle triangles (word : string) =
    let word_value = word |> Seq.fold (fun acc c -> acc + int(c) - 64) 0 
    triangles |> Seq.skipWhile (fun n -> n < word_value) |> Seq.nth(0) = word_value

// filter word list on words that are triangles and count them
let solution = lazy ( words |> Seq.filter (is_triangle triangles) |> Seq.length )
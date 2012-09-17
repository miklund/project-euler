// http://projecteuler.net/index.php?section=problems&id=26
module E026

// The 1/d we're looking for is where d is a prime
let rec primes list = 
    match list with
    | head :: tail -> head :: primes (tail |> List.filter (fun x -> x % head <> 0))
    | _ -> list
    
let cycle denominator =
    let decimals (denominator : int) =
        Seq.initInfinite (fun index -> (10I ** (index + 1) / (bigint(denominator))) % 10I |> int)

    let rec inner_cycle (d : seq<int>) (past : list<int>) length =
        let isEqual seq1 seq2 = seq1 |> Seq.forall2 ( = ) seq2

        let (|Even|Odd|) i =
            if i % 2 = 0 then Even
            else Odd

        let find (list : seq<int>) max = 
            let rec find_inner (l : seq<int>) f_length =

                if f_length <= (denominator - 1) then 0, Seq.empty
                else match f_length with
                     | Odd -> find_inner (l |> Seq.skip 1) (f_length - 1)
                     | Even -> 
                         let half_length = f_length / 2
                         let first = l |> Seq.take half_length
                         let second = l |> Seq.skip half_length
                         if isEqual first second then half_length, first
                         else find_inner (l |> Seq.skip 1) (f_length - 1)
            find_inner list max

        let max (l1 : seq<'a>) (l2 : seq<'a>) =
            let rec max_inner s1 s2 =
                match Seq.isEmpty s1, Seq.isEmpty s2 with
                | true, false -> l1
                | false, true -> l2
                | true, true -> l1
                | false, false -> max_inner (s1 |> Seq.skip 1) (s2 |> Seq.skip 1)
            max_inner l1 l2

        let cycle_length, this_cycle = find past length
        let tail = d |> Seq.skip 1
        let head = d |> Seq.nth 0

        match cycle_length with
               // Continue
        | 0 -> inner_cycle tail (past @ [head]) (length + 1)

               // Found cycle beyond 9 iterations
        | _ -> if past.Length > 9 then this_cycle
            
               // Found cycle under 9 iterations, refine the match
               else max (inner_cycle tail (past @ [head]) (length + 1)) this_cycle

    let my_d = decimals denominator
    let start = (my_d |> Seq.take ((denominator - 1) * 2) |> Seq.toList)
    inner_cycle my_d start start.Length

let solution = 
    lazy (
    (primes [2..1000] |> List.toSeq) |> Seq.mapi (fun i value -> async { return (value, (cycle value |> Seq.length)) } )
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.maxBy (fun x -> (snd x))
    )

// After creating above solution you learn that you only need to calculate primes
// and a cycle for a prime is always the prime - 1.
let alternate_solution = lazy ( primes [2..1000] |> List.max )
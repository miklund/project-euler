// http://projecteuler.net/index.php?section=problems&id=31
module E031

[<CustomEquality;CustomComparison>]
type Coin = 
    | Pence of int
    | TwoPence of int
    | FivePence of int
    | TenPence of int
    | TwentyPence of int
    | FiftyPence of int
    | Pound of int
    | TwoPound of int

    member this.Add coin =
        match coin, this with
        | Pence x, Pence y -> [Pence (x + y)]
        | TwoPence x, TwoPence y -> [TwoPence (x + y)]
        | FivePence x, FivePence y -> [FivePence (x + y)]
        | TenPence x, TenPence y -> [TenPence (x + y)]
        | TwentyPence x, TwentyPence y -> [TwentyPence (x + y)]
        | FiftyPence x, FiftyPence y -> [FiftyPence (x + y)]
        | Pound x, Pound y -> [Pound (x + y)]
        | TwoPound x, TwoPound y -> [TwoPound (x + y)]
        | x, y -> [x; y]

    member this.InPence = 
        match this with
        | Pence amount -> amount
        | TwoPence x -> x * 2
        | FivePence x -> x * 5
        | TenPence x -> x * 10
        | TwentyPence x -> x * 20 
        | FiftyPence x -> x * 50
        | Pound x -> x * 100
        | TwoPound x -> x * 200

    override x.Equals obj = 
        match obj with
        | :? Coin as y -> (x.InPence = y.InPence)
        | _ -> false

    override x.GetHashCode() = hash (x)

    interface System.IComparable with
        member this.CompareTo obj =
            match obj with
            | :? Coin as otherCoin -> compare this.InPence otherCoin.InPence
            | _ -> invalidArg "obj" "Can't compare Coin with another object type"

let count (purse : Coin List) =
    purse |> List.fold (fun acc coin -> acc + coin.InPence) 0

// Not used, only interesting on print outs
let rec collapse (purse : Coin List) =
    match purse with
    | head :: tail -> 
        let same, rest = purse |> List.partition (fun x -> x = head)
        (same.Tail |> List.fold ( fun acc x -> (x.Add acc).Head ) same.Head ) :: collapse rest
    | [] -> []

let coins = [TwoPound 1; Pound 1; FiftyPence 1; TwentyPence 1; TenPence 1; FivePence 1; TwoPence 1; Pence 1]

let rec find purse previousCoin =
    let possibleCoins = coins |> List.filter (fun c -> c <= previousCoin)
    match count purse with
    | 200 ->  1
    | sum when sum > 200 -> 0
    | _ -> List.fold ( + ) 0 (possibleCoins |> List.map (fun c -> find (purse @ [c]) c))
        
let solution = lazy ( find [] (TwoPound 1) )
Utilities.measure_execution_time solution |> ignore

// Not using discriminated unions, this could be one like this
let rec find_faster n coins =
    match n with
    | 200 -> 1
    | purse_sum when purse_sum > 200 -> 0
    | _ -> List.fold ( + ) 0 (coins |> List.map (fun c -> find_faster (n + c)  (coins |> List.filter (fun coin -> coin <= c))))

let solution_faster = lazy ( find_faster 0 [200; 100; 50; 20; 10; 5; 2; 1] )
Utilities.measure_execution_time solution_faster |> ignore
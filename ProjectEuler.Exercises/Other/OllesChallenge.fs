// http://mint.litemedia.se/2010/08/13/the-co-workers-challange/

module OllesChallenge
let luhn pnr = (pnr |> List.fold2 (fun acc n1 n2 -> acc + (n1 * n2) % 10 + n1 * n2 / 10) 0 ([1..10] |> List.map (fun x -> (x % 2) + 1))) % 10 = 0

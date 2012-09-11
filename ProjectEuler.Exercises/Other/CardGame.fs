// This is an example of type extensions

module CardGame

type Suit = | Spades | Hearts | Diamonds | Clubs

type Card =
    | ValueCard of Suit * int
    | Knave of Suit
    | Queen of Suit
    | King of Suit
    | Ace of Suit

type Hand (cards : Card list) =
    member this.Cards = cards

let isRoyalStraightFlush (cards : Card list) = 
    match cards with
    | Ace suit_1 :: King suit_2 :: Queen suit_3 :: Knave suit_4 :: ValueCard (suit_5, 10) :: [] 
        when suit_1 = suit_2 && suit_2 = suit_3 && suit_3 = suit_4 && suit_4 = suit_5 
        -> true
    | _ -> false
    
type Hand with
    member x.IsRoyalStraightFlush = isRoyalStraightFlush x.Cards
    
let hand = new Hand ([Ace Hearts; King Hearts; Queen Hearts; Knave Hearts; ValueCard (Hearts, 10)])
hand.IsRoyalStraightFlush |> ignore
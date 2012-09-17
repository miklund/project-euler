// http://www.citerus.se/jfokus
module CiterusChallenge

// In the string
let s = "VOCDIITEIOCRUDOIANTOCSLOIOCVESTAIOCVOLIOCENTSU"

// Any occurrence of
let abbreviations = ["TDD"; "DDD"; "DI"; "DO"; "OO"; "UI"; "ANT"; "CV"; "IOC"; "LOC"; "SU"; "VO"]

// Should be removed
let rec remove (abbreviations : string List) (s : string) =
    
    // Collect any version where one abbr is removed from s
    // Filter out those with no effect on s
    let collect = 
        abbreviations 
        |> List.map (fun abbr -> s.Replace(abbr, ""))
        |> List.filter (fun short_s -> short_s.Length < s.Length)
        

    // Select longest string of s1 and s2, or s1 if they're equal
    let min (s1 : string) (s2 : string) =
        if s1.Length <= s2.Length then s1 else s2

    // Match result of abbreviations removal
    match collect with
    | [] -> s // Could not shorten any more, return string
    | _  -> List.fold ( min ) s (collect |> List.map ( remove abbreviations )) // Continue to take the smallest string of the shortened subtrees

// Execute
let solution = s |> remove abbreviations

// Tests
let tests = 
    "HELLO" = ("HETDDLLDIO" |> remove abbreviations )    &&  // Two consecutive abbr
    "HELLO" = ("HEDTDDILLO" |> remove abbreviations )    &&  // Two nested abbr
    "HELLO" = ("HEDTDDILLO" |> remove abbreviations )    &&  // Removing TDD before DI
    "HELIUM" = ("HELANTDDDIUM" |> remove abbreviations ) &&  // Don't remove DI or TDD, remove ANT then DDD
    "" = ("DTDDIIOC" |> remove abbreviations )           &&  // Take TDD then IOC last DI will leave only empty string
    "B" = ("BIDDDOCDDD" |> remove abbreviations)             // Some wierd example from the problem description
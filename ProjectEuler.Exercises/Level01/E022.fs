// http://projecteuler.net/index.php?section=problems&id=22

module E022

open System.Text.RegularExpressions

let path = "H:/litemedia/projekt/projecteuler/projecteuler.exercises/names.txt"

let data = 
    use file = new System.IO.StreamReader(path)
    let line = file.ReadToEnd()
    Regex.Matches(line, "\\\"(?<name>\\w+)\\\"") 
    |> Seq.cast 
    |> Seq.map (fun (name:Match) -> name.Groups.["name"].Value)
    |> Seq.toList
    
let weight (name:string) =
    name |> Seq.map (fun c -> (c |> int) - 64) |> Seq.sum

let solution = lazy (data 
                |> List.sort
                |> List.mapi (fun i name -> (i + 1) * weight name)
                |> List.sum)
Utilities.measure_execution_time solution |> ignore
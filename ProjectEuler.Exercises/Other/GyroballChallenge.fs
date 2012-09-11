// http://thycoticsolutionsblog.wordpress.com/2010/08/12/code-challenge-win-a-gyroball/

namespace LiteMedia.GyroballChallenge
open System

type Converter(alphabet:list<string>) =
    let alphabet = alphabet

    new () = Converter(["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "x"; "y"; "z"])

    member this.Convert (n:int) =
        let division = (n |> float) / (alphabet.Length |> float) |> System.Math.Truncate |> int
        match division with
        | 0 -> alphabet.Item(n)
        | _ -> this.Convert division + alphabet.Item(n % alphabet.Length)

type BinaryConverter() =
    inherit Converter(["0"; "1"])

type OctalConverter() =
    inherit Converter(["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"])

type HexConverter() =
    inherit Converter(["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "a"; "b"; "c"; "d"; "e"; "f"])

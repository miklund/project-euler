module HtmlLiteralEncrypt

let encode (s : string) = 
    s 
    |> Seq.map (fun c -> System.String.Format("&#{0:d};", c |> int)) 
    |> System.String.Concat
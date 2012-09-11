module NHibernateSql

let transform (s : string) = 
    let query, variables = (s.Split ';').[0], (s.Split ';').[1]
    let matches = System.Text.RegularExpressions.Regex.Matches(variables, "(?<arg>@p\d+) = (?<value>.*?)(?=,|$)")
    matches |> Seq.cast |> Seq.fold (fun (q : string) (arg : System.Text.RegularExpressions.Match) -> q.Replace(arg.Groups.["arg"].Value, arg.Groups.["value"].Value)) query

let solution = transform "SELECT count(*) as y0_ FROM tISUBALK_Stoppstallen_v3 this_ WHERE (((((this_.Postnummer = @p0 and this_.Postort = @p1) and this_.Gatuadress = @p2) and (this_.Adressplats = @p3 or ((this_.Gatuadress = @p4 and this_.AdressplatsFran <= @p5) and this_.AdressplatsTill >= @p6))) and this_.Littera1 = @p7) and this_.LGHnr = @p8);@p0 = '98765', @p1 = 'SMEDHULT', @p2 = 'BOX', @p3 = '20-31', @p4 = 'BOX', @p5 = NULL, @p6 = NULL, @p7 = '', @p8 = ''"
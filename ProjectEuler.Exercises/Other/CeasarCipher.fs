module CeasarCipher

let encrypt_ key (s : string) =
    let encrypt_char key (c : char) = ((int(c) - 65 + key) % 26) + 65 |> char;
    s |> Seq.map (encrypt_char key)

let cesar key (op : int -> int -> int) = 
    let encrypt_char key op (c : char) = (((op (int(c)) key) - 39) % 26) + 39 |> char;
    Seq.map (encrypt_char op key)

let encode key s = (cesar key (+)) s
let decode key s = (cesar key (-)) s


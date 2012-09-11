module GenerateCryptographicallyRandomKeys

let gen len =
    let provider = new System.Security.Cryptography.RNGCryptoServiceProvider()
    let out : byte array = Array.zeroCreate (len / 2)
    provider.GetBytes(out)
    out |> Seq.map (fun b -> System.String.Format("{0:X2}", b)) |> System.String.Concat

type MachineKey = { sha1 : string; aes : string; _3des : string }
let machineKey = { sha1 = (gen 128); aes = (gen 64); _3des = (gen 48) } 

printfn "<machineKey validationKey=\"%s\" decryptionKey=\"%s\" validation=\"SHA1\" decryption=\"AES\" />"
    machineKey.sha1
    machineKey.aes

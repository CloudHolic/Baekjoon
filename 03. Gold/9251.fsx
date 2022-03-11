open System
open System.IO

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())

    let str1 = stream.ReadLine().Trim()
    let str2 = stream.ReadLine().Trim()

    let len1 = String.length str1
    let len2 = String.length str2

    let cache : int[,] = Array2D.zeroCreate <|| (len1 + 1, len2 + 1)

    for i in 1 .. len1 do
        for j in 1 .. len2 do
            match (i, j) with
            | (a, b) when str1[a - 1] = str2[b - 1] -> cache[a, b] <- cache[a - 1, b - 1] + 1
            | _ -> cache[i, j] <- max <|| (cache[i - 1, j], cache[i, j - 1])

    printfn "%d" cache[len1, len2]
    0
open System
open System.IO

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let equal3 a b c = a = b && b = c
    let max3 a b c = max a <| max b c

    let str1 = stream.ReadLine().Trim()
    let str2 = stream.ReadLine().Trim()
    let str3 = stream.ReadLine().Trim()

    let len1 = String.length str1
    let len2 = String.length str2
    let len3 = String.length str3

    let cache : int[,,] = Array3D.zeroCreate <||| (len1 + 1, len2 + 1, len3 + 1)

    for i in 1 .. len1 do
        for j in 1 .. len2 do
            for k in 1 .. len3 do
                match (i, j, k) with
                | (a, b, c) when equal3 str1.[a - 1] str2.[b - 1] str3.[c - 1] -> cache.[a, b, c] <- cache.[a - 1, b - 1, c - 1] + 1
                | _ -> cache.[i, j, k] <- max3 <||| (cache.[i - 1, j, k], cache.[i, j - 1, k], cache.[i, j, k - 1])

    printfn "%d" cache.[len1, len2, len3]
    0
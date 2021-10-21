open System
open System.IO

[<EntryPoint>]
let main _ =
    let min3 a b c = min a <| min b c
    let boolToInt = 
        function
        | true -> 1
        | false -> 0

    use stream = new StreamReader(Console.OpenStandardInput())

    let str1 = stream.ReadLine().Trim()
    let str2 = stream.ReadLine().Trim()

    let len1 = String.length str1
    let len2 = String.length str2

    let cache : int[,] = Array2D.zeroCreate <|| (len1 + 1, len2 + 1)

    for i in 0 .. len1 do
        for j in 0 .. len2 do
            match (i, j) with
            | (0, 0) -> cache.[i, j] <- 0
            | (a, 0) -> cache.[i, j] <- a
            | (0, b) -> cache.[i, j] <- b
            | _ -> cache.[i, j] <- min3 <||| (cache.[i - 1, j] + 1, cache.[i, j - 1] + 1, cache.[i - 1, j - 1] + (boolToInt <| (str1.[i - 1] <> str2.[j - 1])))

    printfn "%d" cache.[len1, len2]
    0
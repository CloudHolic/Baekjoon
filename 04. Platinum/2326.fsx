open System
open System.IO

[<EntryPoint>]
let main _ =
    let min4 (a, b, c, d) = min a <| (min b <| min c d)

    use stream = new StreamReader(Console.OpenStandardInput())

    let str1 = stream.ReadLine().Trim()
    let str2 = stream.ReadLine().Trim()

    let len1 = String.length str1
    let len2 = String.length str2

    let cache : int[,] = Array2D.zeroCreate <|| (len1 + 2, len2 + 2)
    let da_cache: int[] = Array.init 1000 (fun _ -> 1)

    cache[0, 0] <- len1 + len2
    for i in 1 .. len1 + 1 do
        cache[i, 0] <- cache[0, 0]
        cache[i, 1] <- i - 1

    for j in 1 .. len2 + 1 do
        cache[0, j] <- cache[0, 0]
        cache[1, j] <- j - 1

    for i in 2 .. len1 + 1 do
        let mutable db = 1

        for j in 2 .. len2 + 1 do
            let k = da_cache[int str2[j - 2]]
            let l = db
            let mutable cost = 0

            match (i, j) with
            | _ when str1[i - 2] = str2[j - 2] -> db <- j
            | _ -> cost <- 1

            cache[i, j] <- min4 (cache[i, j - 1] + 1, cache[i - 1, j] + 1, cache[i - 1, j - 1] + cost, cache[k - 1, l - 1] + i + j - k - l - 1)

        da_cache[int str1[i - 2]] <- i

    printfn "%d" cache[len1 + 1, len2 + 1]
    0
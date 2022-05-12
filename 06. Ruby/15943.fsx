open System
open System.IO

[<EntryPoint>]
let main _ =
    let Mod = 1000000007L
    use stream = new StreamReader(Console.OpenStandardInput())
    let sStr = stream.ReadLine().Trim()
    let n = stream.ReadLine().Trim() |> int
    let size = sStr.Length
        
    // Pre-computations
    let sStr = ')' |> Array.create 3000 |> String |> fun x -> sStr + x
    let sumArr =
        let mutable temp = 0
        sStr
        |> Seq.map (fun x -> 
            let i = if x = '(' then 1 else -1
            temp <- temp + i
            temp)
        |> Seq.toArray
        |> fun x -> Array.append [|0|] x
    let nextIdx = 
        let nextArr = sumArr |> Array.mapi (fun i _ -> Array.FindIndex(sumArr, i + 1, fun x -> x < sumArr[i] - 1) - 1)
        Array.init (sStr.Length + 1) (fun x -> if x = 0 then -1 else Array.FindLastIndex(nextArr, x - 1, fun y -> y = x))

    let prevIdx =
        let prevArr = sumArr |> Array.mapi (fun i _ -> if i > 1 then Array.FindLastIndex(sumArr, i - 1, fun x -> x > sumArr[i]) else -1)
        Array.init (sStr.Length + 1) (fun x -> if x = 0 then -1 else Array.FindIndex(prevArr, x + 1, fun y -> y = x))

    // DP
    let cache : int64[,] = Array2D.zeroCreate 3001 6001
    sumArr |> Array.iteri (fun i x ->
        match abs x with
        | 0 -> cache[0, i] <- cache[0, i] + 1L
        | 1 -> cache[1, i] <- cache[1, i] + 1L
        | _ -> ())

    for i = 2 to n do
        for j = 1 to sStr.Length do
            if nextIdx[j] > -1 then
                cache[i, nextIdx[j]] <- cache[i, nextIdx[j]] + cache[i - 1, j]
                if cache[i, nextIdx[j]] > Mod then
                    cache[i, nextIdx[j]] <- cache[i, nextIdx[j]] - Mod

            if prevIdx[j] > -1 then
                cache[i, prevIdx[j]] <- cache[i, prevIdx[j]] + cache[i - 1, j]
                if cache[i, prevIdx[j]] > Mod then
                    cache[i, prevIdx[j]] <- cache[i, prevIdx[j]] - Mod

    cache[n, size]
    |> printfn "%d"
    0
open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ = 
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let t = stream.ReadLine().Trim() |> int
    let parseInts (str: string) = str.Trim().Split() |> Array.map int

    let shuffle arr =
        let random = Random()
        let swap x y (a: 'a []) =
            let temp = a[x]
            a[x] <- a[y]
            a[y] <- temp
        arr |> Array.iteri(fun i _ -> arr |> swap i (random.Next(i, Array.length arr)))
        arr

    let rec solve times =
        if times > 0 then
            let c = stream.ReadLine() |> parseInts |> fun x -> x[1]
            let nums = stream.ReadLine() |> parseInts |> Array.filter (fun x -> x <= c) |> shuffle
            let subsetA, subsetB = ResizeArray<int>(), ResizeArray<int>()

            let balancedDp t m =
                let inf, bias = subsetA.Count, m - t - 1
                let cache = 2 * m |> Array.zeroCreate
                let prv_cache = (2 * m, inf) ||> Array.create
                prv_cache[bias] <- -1

                subsetB
                |> Seq.iter (fun b ->
                    for i = t - m + 1 to t + m do
                        cache[i + bias] <- prv_cache[i + bias]
                    for i = t - m + 1 to t - 1 do
                        cache[i + b + bias] <- (cache[i + b + bias], prv_cache[i + bias]) ||> min
                    for i = t + m downto t + 1 do
                        if cache[i + bias] <> inf then
                            for j = cache[i + bias] + 1 to (prv_cache[i + bias], inf - 1) ||> min do
                                cache[i + subsetA[j] + bias] <- (cache[i + subsetA[j] + bias], j) ||> min
                    for i = t - m + 1 to t + m do
                        prv_cache[i + bias] <- cache[i + bias])

                let mutable flag, answer, i = true, -1, t
                while flag && i > t - m do
                    if cache[i + bias] <> inf then
                        answer <- i
                        flag <- false
                    else
                        i <- i - 1

                answer

            // Split Subsets
            let mutable m, sum = -1, 0
            nums
            |> Array.iter (fun s ->
                if sum + s <= c then
                    sum <- sum + s
                    subsetA.Add -s
                else subsetB.Add s
                m <- (m, s) ||> max)

            if Array.length nums = subsetA.Count then 
                result.Append(sum).Append('\n') |> ignore
            else result.Append(sum + (balancedDp <|| (c - sum, m))).Append('\n') |> ignore
            times - 1 |> solve

    solve t
    printfn "%A" result
    0
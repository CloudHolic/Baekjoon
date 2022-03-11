open System
open System.IO

[<EntryPoint>]
let main _ =
    let subSum (lst: int64 list) =
        let result = Array.zeroCreate (1 <<< lst.Length)
        let mutable idx = 0
        let rec powerset (xs: int64 list) =
            match xs with
            | [] ->
                result[idx] <- 0L
                idx <- idx + 1
            | h::t ->
                powerset t
                for i in 0 .. idx - 1 do
                    result[i + idx] <- h + result[i]
                idx <- idx * 2
    
        powerset lst
        Array.sort result

    let rec lowerBound target (arr:int64 array) beg en =
        match sign <| compare en beg with
        | -1 -> None
        | _ ->
            let mid = (beg + en) / 2
            match sign <| compare target arr[mid] with
            | 1 -> lowerBound target arr (mid + 1) en
            | _ ->
                if beg < mid then
                    lowerBound target arr beg mid
                else
                    Some(mid)

    use stream = new StreamReader(Console.OpenStandardInput())

    let size = stream.ReadLine() |> int
    let nums = stream.ReadLine().Split() |> Array.map int64 |> Array.toList
    
    match size with
    | 1 -> printfn "%d" nums[0]
    | _ ->
        let mid = (size + 1) / 2
        let leftSum = subSum nums[0 .. mid - 1]
        let rightSum = subSum nums[mid .. size - 1]

        let leftSize = leftSum.Length
        let rightSize = rightSum.Length
        let mutable answer = 0L

        for i in 34 .. -1 .. 0 do
            let cur = 1L <<< i
            let mutable a, b = 0, 0

            for j in 0 .. leftSize - 1 do
                a <- a + int (leftSum[j] / cur &&& 1L)
                leftSum[j] <- leftSum[j] % cur

            for j in 0 .. rightSize - 1 do
                b <- b + int (rightSum[j] / cur &&& 1L)
                rightSum[j] <- rightSum[j] % cur
            
            let calc = leftSize - a &&& b &&& 1 ^^^ rightSize - b &&& a &&& 1 |> int64
            answer <- answer <<< 1 ||| calc

            let temp = Array.sort leftSum
            rightSum
            |> Array.iter (fun x ->
                let idx = lowerBound (cur - x) temp 0 (leftSize - 1) |> function | None -> 0 | Some x -> x
                answer <- answer ^^^ int64 (leftSize - idx - 1 &&& 1))

        printfn "%d" answer
    0
open System
open System.Collections.Generic
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    let sumList: uint64[,] = Array2D.zeroCreate 50000 782

    let initBits index length = Array.Clear(sumList, 782 * index, length)

    let copyBits index length =
        let offset = 782 * sizeof<uint64>
        Buffer.BlockCopy(sumList, (index - 1) * offset, sumList, index * offset, length * sizeof<uint64>)

    let setBit index x = sumList[index, (x >>> 6)] <- sumList[index, (x >>> 6)] ||| (1UL <<< (63 - (x &&& 63)))

    let getBit index x = (sumList[index, (x >>> 6)] &&& (1UL <<< (63 - (x &&& 63)))) <> 0UL

    let leftShift p index shift =
        let bits, remainder, q, r = p + 63 >>> 6, p &&& 63, shift >>> 6, shift &&& 63
        let mutable i = 0

        match r > 0 with
        | true ->
            while i < bits - q - 1 do
                sumList[index, i] <- sumList[index, i] ||| (sumList[(index - 1), (i + q)] <<< r) ||| (sumList[(index - 1), (i + q + 1)] >>> 64 - r)
                i <- i + 1
            sumList[index, (bits - q - 1)] <- sumList[index, (bits - q - 1)] ||| (sumList[(index - 1), (bits - 1)] <<< r)
        | _ ->
            while i < bits - q do
                sumList[index, i] <- sumList[index, i] ||| sumList[(index - 1), (i + q)]
                i <- i + 1

        if remainder > 0 then
            let mask = (1UL <<< (64 - remainder)) - 1UL
            sumList[index, (bits - 1)] <- sumList[index, (bits - 1)] &&& (~~~ mask)

    let rightShift p index shift =
        let bits, remainder, q, r = p + 63 >>> 6, p &&& 63, shift >>> 6, shift &&& 63

        match r > 0 with
        | true ->
            let mutable i = 1
            sumList[index, q] <- sumList[index, q] ||| (sumList[(index - 1), 0] >>> r)
            while i < bits - q do
                sumList[index, (i + q)] <- sumList[index, (i + q)] ||| (sumList[(index - 1), (i - 1)] <<< 64 - r) ||| (sumList[(index - 1), i] >>> r)
                i <- i + 1
        | _ ->
            let mutable i = 0
            while i < bits - q do
                sumList[index, (i + q)] <- sumList[index, (i + q)] ||| sumList[(index - 1), i]
                i <- i + 1

        if remainder > 0 then
            let mask = (1UL <<< (64 - remainder)) - 1UL
            sumList[index, (bits - 1)] <- sumList[index, (bits - 1)] &&& (~~~ mask)

    let rec solve n nums : bool[] =
        let root = float n |> sqrt |> int
        let rec findFirstFactor n cur =
            if cur > root then 1
            elif n % cur = 0 then cur
            else findFirstFactor n (cur + 1)

        findFirstFactor n 2
        |> function
            | 1 -> solvePrime n nums
            | i -> solveComposite i (n / i) nums

    and solveComposite a b (nums: int[]) : bool[] =
        let result: bool[] = Array.zeroCreate (2 * a * b - 1)
        
        let curIndexes = Array.init (2 * a - 1) (fun x -> x)
        let indexQueue = Queue<int>([|2 * a - 1 .. 2 * a * b - 2|])

        let subSums = Array.zeroCreate (2 * b - 1)
        let subAnswers = Array2D.zeroCreate (2 * b - 1) a

        let mutable i = 0
        while i < 2 * b - 1 do
            let curAnswer = Array.init (2 * a - 1) (fun x -> nums[curIndexes[x]] % a) |> solve a
            let mutable idx, j = 0, 0
            while j < 2 * a - 1 do
                match curAnswer[j] with
                | true ->
                    subSums[i] <- subSums[i] + nums[curIndexes[j]]
                    subAnswers[i, idx] <- curIndexes[j]
                    idx <- idx + 1
                    if i < 2 * b - 2 then
                        curIndexes[j] <- indexQueue.Dequeue()
                | _ -> ()
                j <- j + 1

            subSums[i] <- subSums[i] / a % b
            i <- i + 1

        subSums
        |> solve b
        |> Array.iteri (fun i x -> 
            match x with
            | true -> subAnswers[i, *] |> Array.iter (fun y -> result[y] <- true)
            | _ -> ())

        result

    and solvePrime p nums : bool[] =
        let result: bool[] = Array.zeroCreate (2 * p - 1)
        let idx = Array.init (2 * p - 1) (fun i -> i) |> Array.sortBy (fun x -> nums[x])

        let checkDuplicate =
            let mutable dup, i = -1, 0
            while i < p do
                match dup, i with
                | d, _ when d > 0 -> ()
                | -1, x when nums[idx[x]] = nums[idx[x + p - 1]] -> dup <- x
                | _ -> ()
                i <- i + 1
            dup

        match checkDuplicate with
        | -1 ->
            let mutable initMod, i = 0, 0
            while i < p do
                initMod <- (initMod + nums[idx[i]]) % p
                result[idx[i]] <- true
                i <- i + 1

            if initMod <> 0 then
                let bits = (p + 63) >>> 6
                initBits 0 bits
                setBit 0 initMod

                let mutable step, flag = 1, true
                while flag do
                    let diff = nums[idx[step + p - 1]] - nums[idx[step]]
                    copyBits step bits
                    rightShift p step diff
                    leftShift p step (p - diff)

                    if getBit step 0 then flag <- false
                    else step <- step + 1
                    
                let mutable curSum, j = 0, step
                while j > 0 do
                    let diff = nums[idx[j + p - 1]] - nums[idx[j]]
                    let tempSum = (curSum - diff + p) % p
                    
                    if getBit (j - 1) tempSum then
                        result[idx[j]] <- false
                        result[idx[j + p - 1]] <- true
                        curSum <- tempSum
                    j <- j - 1
        | i ->
            for j in i .. i + p - 1 do
                result[idx[j]] <- true

        result
            
    use stream = new StreamReader(Console.OpenStandardInput())
    let size = stream.ReadLine() |> int
    let nums = stream.ReadLine().Trim().Split() |> Array.map int

    match size with
    | 1 -> printfn "%d" nums[0]
    | _ ->        
        let result = new StringBuilder()
        solve size nums
        |> Array.iteri (fun i x -> if x then result.AppendFormat("{0} ", nums[i]) |> ignore)
        printfn "%A" result
    0
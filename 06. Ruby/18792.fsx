open System
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Text

[<EntryPoint>]
let main _ =
    let sumList: uint64[][] = Array.init 50000 (fun _ -> Array.zeroCreate 782)

    let initBits index length = Array.Clear(sumList.[index], 0, length)

    let copyBits index length = Buffer.BlockCopy(sumList.[index - 1], 0, sumList.[index], 0, length * sizeof<uint64>)

    let setBit index x = sumList.[index].[x >>> 6] <- sumList.[index].[x >>> 6] ||| (1UL <<< (63 - (x &&& 63)))

    let getBit index x = (sumList.[index].[x >>> 6] &&& (1UL <<< (63 - (x &&& 63)))) <> 0UL

    let leftShift p index shift =
        let bits, remainder, q, r = p + 63 >>> 6, p &&& 63, shift >>> 6, shift &&& 63

        match r > 0 with
        | true ->
            [0 .. bits - q - 2]
            |> List.iter (fun x ->
                sumList.[index].[x] <- sumList.[index].[x] ||| (sumList.[index - 1].[x + q] <<< r) ||| (sumList.[index - 1].[x + q + 1] >>> 64 - r))
            sumList.[index].[bits - q - 1] <- sumList.[index].[bits - q - 1] ||| (sumList.[index - 1].[bits - 1] <<< r)
        | _ ->
            [0 .. bits - q - 1]
            |> List.iter (fun x ->
                sumList.[index].[x] <- sumList.[index].[x] ||| sumList.[index - 1].[x + q])

        if remainder > 0 then
            let mask = (1UL <<< (64 - remainder)) - 1UL
            sumList.[index].[bits - 1] <- sumList.[index].[bits - 1] &&& (~~~ mask)

    let rightShift p index shift =
        let bits, remainder, q, r = p + 63 >>> 6, p &&& 63, shift >>> 6, shift &&& 63

        match r > 0 with
        | true ->        
            sumList.[index].[q] <- sumList.[index].[q] ||| (sumList.[index - 1].[0] >>> r)
            [1 .. bits - q - 1]
            |> List.iter (fun x ->
                sumList.[index].[x + q] <- sumList.[index].[x + q] ||| (sumList.[index - 1].[x - 1] <<< 64 - r) ||| (sumList.[index - 1].[x] >>> r))
        | _ ->
            [0 .. bits - q - 1]
            |> List.iter (fun x ->
                sumList.[index].[x + q] <- sumList.[index].[x + q] ||| sumList.[index - 1].[x])

        if remainder > 0 then
            let mask = (1UL <<< (64 - remainder)) - 1UL
            sumList.[index].[bits - 1] <- sumList.[index].[bits - 1] &&& (~~~ mask)

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
        let subAnswers = Array.init (2 * b - 1) (fun _ -> Array.zeroCreate a)

        for i in 0 .. 2 * b - 2 do
            let curAnswer = Array.init (2 * a - 1) (fun x -> nums.[curIndexes.[x]] % a) |> solve a
            let mutable idx = 0
            [0 .. 2 * a - 2]
            |> List.iter (fun x ->
                match curAnswer.[x] with
                | true ->
                    subSums.[i] <- subSums.[i] + nums.[curIndexes.[x]]
                    subAnswers.[i].[idx] <- curIndexes.[x]
                    idx <- idx + 1
                    if i < 2 * b - 2 then
                        curIndexes.[x] <- indexQueue.Dequeue()
                | _ -> ())
            subSums.[i] <- subSums.[i] / a % b

        subSums
        |> solve b
        |> Array.iteri (fun i x -> 
            match x with
            | true -> subAnswers.[i] |> Array.iter (fun y -> result.[y] <- true)
            | _ -> ())

        result

    and solvePrime p nums : bool[] =
        let result: bool[] = Array.zeroCreate (2 * p - 1)
        let idx = Array.init (2 * p - 1) (fun i -> i) |> Array.sortBy (fun x -> nums.[x])

        let checkDuplicate =
            let mutable dup = -1
            [0 .. p - 1]
            |> List.iter (fun x ->
                match dup, x with
                | i, _ when i > 0 -> ()
                | -1, x when nums.[idx.[x]] = nums.[idx.[x + p - 1]] -> dup <- x
                | _ -> ())
            dup

        match checkDuplicate with
        | -1 ->
            let mutable initMod = 0
            [0 .. p - 1]
            |> List.iter (fun x ->
                initMod <- (initMod + nums.[idx.[x]]) % p
                result.[idx.[x]] <- true)

            if initMod <> 0 then
                let bits = (p + 63) >>> 6
                initBits 0 bits
                setBit 0 initMod

                let mutable step, flag = 1, true
                while flag do
                    let diff = nums.[idx.[step + p - 1]] - nums.[idx.[step]]
                    copyBits step bits
                    rightShift p step diff
                    leftShift p step (p - diff)

                    if getBit step 0 then flag <- false
                    else step <- step + 1
                    
                let mutable curSum = 0
                [step .. -1 .. 1]
                |> List.iter (fun x ->
                    let diff = nums.[idx.[x + p - 1]] - nums.[idx.[x]]
                    let tempSum = (curSum - diff + p) % p
                    
                    if getBit (x - 1) tempSum then
                        result.[idx.[x]] <- false
                        result.[idx.[x + p - 1]] <- true
                        curSum <- tempSum)
        | i ->
            for j in i .. i + p - 1 do
                result.[idx.[j]] <- true

        result
            
    use stream = new StreamReader(Console.OpenStandardInput())
    let size = stream.ReadLine() |> int
    let nums = stream.ReadLine().Trim().Split() |> Array.map int

    match size with
    | 1 -> printfn "%d" nums.[0]
    | _ ->        
        let result = new StringBuilder()
        solve size nums
        |> Array.iteri (fun i x -> if x then result.AppendFormat("{0} ", nums.[i]) |> ignore)
        printfn "%A" result
    0
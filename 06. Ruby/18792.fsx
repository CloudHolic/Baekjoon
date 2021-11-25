open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
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

        let indexUsed: bool[] = Array.zeroCreate (2 * a * b - 1)
        let subAnswers: Tuple<int, int list>[] = Array.init (2 * b - 1) (fun _ -> 0, [])

        for i in 0 .. 2 * b - 2 do
            let mutable curNums = []

            let mutable flag, j = true, 0
            while flag do
                match indexUsed.[j], List.length curNums with
                | true, _ -> ()
                | _, s when s < 2 * a - 1 ->
                    curNums <- (nums.[j] % a) :: curNums
                | _ -> flag <- false

                match j with
                | j when j = 2 * a * b - 2 -> flag <- false
                | _ -> j <- j + 1
            
            let curAnswer = curNums |> List.rev |> Array.ofList |> solve a
            let mutable flag, j, k = true, 0, 0
            while flag do
                match indexUsed.[j], curAnswer.[k] with
                | true, _ -> ()
                | _, true ->
                    let tup = subAnswers.[i]
                    subAnswers.[i] <- (nums.[j] + fst tup, j :: snd tup)
                    indexUsed.[j] <- true
                    k <- k + 1
                | _ -> k <- k + 1

                match j, k with
                | j, k when j = 2 * a * b - 1 || k = 2 * a - 1 -> flag <- false
                | _ -> j <- j + 1

        subAnswers
        |> Array.map (fun x -> fst x / a % b)
        |> solve b
        |> Array.iteri (fun i x -> 
            match x with
            | true -> snd subAnswers.[i] |> List.iter (fun y -> result.[y] <- true)
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
        | -1 -> ()
            // Modify this code to bit-wise

            // let sumList = Array2D.init p p (fun _ _ -> -1)
            // sumList.[0, nums.[idx.[0]]] <- idx.[0]
            // for i in 1 .. p - 1 do
            //     for j in 0 .. p - 1 do
            //         match sumList.[i - 1, j] with
            //         | -1 -> ()
            //         | _ ->
            //             let a_sum = (j + nums.[idx.[i]]) % p
            //             let b_sum = (j + nums.[idx.[i + p - 1]]) % p
                        
            //             if sumList.[i, a_sum] = -1 then
            //                 sumList.[i, a_sum] <- idx.[i]

            //             if b_sum <> a_sum && sumList.[i, b_sum] = -1 then
            //                 sumList.[i, b_sum] <- idx.[i + p - 1]
            
            // let mutable curSum = 0
            // for i in p - 1 .. -1 .. 0 do
            //     result.[sumList.[i, curSum]] <- true
            //     curSum <- (curSum - nums.[sumList.[i, curSum]] + p) % p
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
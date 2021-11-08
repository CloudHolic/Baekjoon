open System
open System.IO

[<EntryPoint>]
let main _ =
    let calcScore (str: string) =
        let first (a, _, _) = a
        let boolToInt b = if b then 1L else -1L

        // B = +1, W = -1
        str
        |> Seq.fold (fun (score, prev, multiplier) x ->
            let mult = if multiplier <> 40 then multiplier - 1 elif prev = x then multiplier else multiplier - 1
            if prev = ' ' then ((1L <<< multiplier) * (boolToInt <| (x = 'B')), x, multiplier)
            else (score + (1L <<< mult) * (boolToInt <| (x = 'B')), x, mult)) (0L, ' ', 40)
        |> first        
        |> function
            | score -> (score, str.Length)

    let subSum lst =
        let rec powerset (xs: (int64 * int) list) : (int64 * int) list =
            match xs with
            | [] -> [(0L, 0)]
            | h::t -> List.fold (fun ys s -> (fst h + fst s, snd h + snd s)::s::ys) [] (powerset t)
        
        powerset lst
        |> List.toArray
        |> Array.sort

    use stream = new StreamReader(Console.OpenStandardInput())

    let size = stream.ReadLine() |> int
    let mid = size / 2
    let piles = Array.init size (fun _ -> stream.ReadLine())

    let scoreArr = piles |> Array.map calcScore |> Array.toList
    let leftSum = subSum scoreArr.[0 .. mid - 1]
    let rightSum = subSum scoreArr.[mid .. size - 1]

    let leftSize = leftSum.Length - 1
    let rightSize = rightSum.Length - 1

    let mutable leftIdx = leftSize
    let mutable rightIdx = 0
    let mutable answer = 0

    while leftIdx >= 0 && rightIdx <= rightSize do
        let sum = fst leftSum.[leftIdx] + fst rightSum.[rightIdx]
        match sum with
        | s when s > 0L -> leftIdx <- leftIdx - 1
        | s when s < 0L -> rightIdx <- rightIdx + 1
        | _ -> 
            let mutable leftMax = snd leftSum.[leftIdx]
            let mutable rightMax = snd rightSum.[rightIdx]
            let mutable flag = leftIdx > 0 && fst leftSum.[leftIdx - 1] = fst leftSum.[leftIdx]
            while flag do
                leftIdx <- leftIdx - 1
                leftMax <- (leftMax, snd leftSum.[leftIdx]) ||> max
                flag <- leftIdx > 0 && fst leftSum.[leftIdx - 1] = fst leftSum.[leftIdx]

            flag <- rightIdx < rightSize && fst rightSum.[rightIdx + 1] = fst rightSum.[rightIdx]
            while flag do
                rightIdx <- rightIdx + 1
                rightMax <- (rightMax, snd rightSum.[rightIdx]) ||> max
                flag <- rightIdx < rightSize && fst rightSum.[rightIdx + 1] = fst rightSum.[rightIdx]

            answer <- (answer, leftMax + rightMax) ||> max
            leftIdx <- leftIdx - 1
            rightIdx <- rightIdx + 1

    printfn "%d" answer
    0
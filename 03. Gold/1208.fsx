open System
open System.IO

[<EntryPoint>]
let main _ =
    let subSum (lst: int list) =
        let result = Array.zeroCreate (1 <<< lst.Length)
        let mutable idx = 0
        let rec powerset (xs: int list) =
            match xs with
            | [] ->
                result[idx] <- 0
                idx <- idx + 1
            | h::t ->
                powerset t
                for i in 0 .. idx - 1 do
                    result[i + idx] <- h + result[i]
                idx <- idx * 2
        
        powerset lst
        Array.sort result

    use stream = new StreamReader(Console.OpenStandardInput())

    let size, sum = 
        stream.ReadLine().Split()
        |> Array.map int
        |> function
            | arr -> (arr[0], arr[1])

    let mid = size / 2
    let nums = stream.ReadLine().Split() |> Array.map int |> Array.toList

    let leftSum = subSum nums[0 .. mid - 1]
    let rightSum = subSum nums[mid .. size - 1]

    let leftSize = leftSum.Length - 1
    let rightSize = rightSum.Length - 1

    let mutable leftIdx = leftSize
    let mutable rightIdx = 0
    let mutable answer = 0L

    while leftIdx >= 0 && rightIdx <= rightSize do
        let cur = leftSum[leftIdx] + rightSum[rightIdx]
        match cur with
        | s when s > sum -> leftIdx <- leftIdx - 1
        | s when s < sum -> rightIdx <- rightIdx + 1
        | _ -> 
            let mutable leftCount = 1
            let mutable rightCount = 1
            let mutable flag = leftIdx > 0 && leftSum[leftIdx - 1] = leftSum[leftIdx]
            while flag do
                leftIdx <- leftIdx - 1
                leftCount <- leftCount + 1
                flag <- leftIdx > 0 && leftSum[leftIdx - 1] = leftSum[leftIdx]

            flag <- rightIdx < rightSize && rightSum[rightIdx + 1] = rightSum[rightIdx]
            while flag do
                rightIdx <- rightIdx + 1
                rightCount <- rightCount + 1
                flag <- rightIdx < rightSize && rightSum[rightIdx + 1] = rightSum[rightIdx]

            answer <- answer + int64 leftCount * int64 rightCount
            leftIdx <- leftIdx - 1
            rightIdx <- rightIdx + 1

    if sum = 0 then answer <- answer - 1L
    printfn "%d" answer
    0
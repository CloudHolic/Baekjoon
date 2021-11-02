open System
open System.IO

[<EntryPoint>]
let main _ =
    let makeArray = Array.map int

    let rec binSearch target arr idx =
        match Array.length arr with
        | 1 ->
            match sign <| compare target arr.[0] with
            | -1 -> idx
            | _ -> idx + 1
        | 2 ->
            match (target, arr.[0], arr.[1]) with
            | (a, b, _) when a < b -> idx
            | (a, b, c) when a > b && a < c -> idx + 1
            | _ -> idx + 2
        | i ->
            let middle = i / 2
            match sign <| compare target arr.[middle] with
            | -1 -> binSearch target arr.[.. middle - 1] idx
            | _ -> binSearch target arr.[middle + 1 ..] (idx + middle + 1)

    let lis lst =
        let size = Array.length lst
        let cache = Array.create size -1
        let mutable maxLength = 0

        for i in 0 .. (size - 1) do
            match maxLength with
            | k when k = 0 || cache.[k - 1] < lst.[i] ->
                cache.[k] <- lst.[i]
                maxLength <- maxLength + 1
            | _ ->                
                let idx = binSearch lst.[i] cache.[0 .. maxLength - 1] 0
                cache.[idx] <- lst.[i]
        maxLength

    let makeInverse arr =
        let res = Array.create (Array.length arr + 1) 0
        arr |> Array.iteri (fun i x -> res.[x] <- i + 1)
        res

    let applyInverse (arr: int[]) (ref: int[]) = Array.mapi (fun i _ -> ref.[arr.[i]]) arr

    use stream = new StreamReader(Console.OpenStandardInput())
    stream.ReadLine() |> ignore
    let arr1 = stream.ReadLine().Trim().Split() |> makeArray
    let arr2 = stream.ReadLine().Trim().Split() |> makeArray

    makeInverse arr2
    |> applyInverse arr1
    |> lis
    |> printfn "%d"
    0
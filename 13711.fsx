open System
open System.IO

[<EntryPoint>]
let main _ =
    let makeArray = Array.map int

    let rec binSearch target arr idx =
        match Array.length arr with
        | 0
        | 1 -> Some (idx + 1)
        | i ->
            let middle = i / 2
            match sign <| compare target arr.[middle] with
            | 0 -> Some target
            | -1 -> binSearch target arr.[.. middle - 1] 0
            | _ -> binSearch target arr.[middle + 1 ..] middle

    let lis l =
        let size = Array.length l
        let cache = Array.create size -1
        let mutable maxLength = 0

        for i in 0 .. (size - 1) do
            match maxLength with
            | k when k = 0 || cache.[k - 1] < l.[i] ->
                cache.[k] <- l.[i]
                maxLength <- maxLength + 1
            | _ ->                
                match binSearch l.[i] cache.[0 .. maxLength - 1] 0 with
                | None ->
                    cache.[maxLength] <- l.[i]
                    maxLength <- maxLength + 1
                | Some x ->
                    cache.[x] <- l.[i]
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
open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ = 
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let _ = stream.ReadLine().Trim() |> int
    let testArr = stream.ReadLine().Trim().Split() |> Array.map int |> Array.sort

    let _ = stream.ReadLine().Trim() |> int
    let targetArr = stream.ReadLine().Trim().Split() |> Array.map int

    let binSearch (arr: int array) target = 
        let rec inner beg en =
            match sign <| compare beg en with
            | -1 -> 
                let mid = (beg + en) >>> 1
                match sign <| compare arr[mid] target with
                | 0 -> 1
                | -1 -> inner (mid + 1) en
                | _ -> inner beg mid
            | _ -> 0

        inner 0 (Array.length arr)

    targetArr
    |> Array.iter (fun x -> result.Append(x |> binSearch testArr |> string).Append('\n') |> ignore)

    printfn "%A" result
    0
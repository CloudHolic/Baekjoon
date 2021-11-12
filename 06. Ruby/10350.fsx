open System
open System.IO

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())

    let size = stream.ReadLine() |> int
    let money = stream.ReadLine().Split() |> Array.map int

    let totalSum = Array.sum money
    let mutable answer = 0L

    let subSum idx =
        let mutable curSum = 0
        for i in idx .. size + idx - 1 do
            curSum <- curSum + money.[i % size]
            if curSum < 0 then
                let count = (abs curSum - 1) / totalSum + 1
                answer <- answer + int64 count

    money
    |> Array.iteri (fun i _ -> subSum i)

    printfn "%d" answer
    0
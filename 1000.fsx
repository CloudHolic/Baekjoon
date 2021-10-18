open System

[<EntryPoint>]
let main _ =
    let line = Console.ReadLine().Split()
    let result = Array.map int line |> Array.sum
    printfn "%d" result
    0
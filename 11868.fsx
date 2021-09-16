open System

[<EntryPoint>]
let main argv =
    do Console.ReadLine() |> ignore
    let line = Console.ReadLine().Split()
    let result = Array.map int line |> Array.fold (fun acc x -> acc ^^^ x) 0
    match result with
    | 0 -> printfn "cubelover"
    | _ -> printfn "koosaga"
    0
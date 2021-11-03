open System

[<EntryPoint>]
let main _ =
    let inverse player =
        match player with
        | "Whiteking" -> "Blackking"
        | _ -> "Whiteking"

    do Console.ReadLine() |> ignore
    let line = Console.ReadLine().Split()
    let first = Console.ReadLine()
    let result = Array.map int line |> Array.fold (fun acc x -> acc ^^^ (x - 2)) 0
    match result with
    | 0 -> printfn "%s" <| inverse first
    | _ -> printfn "%s" first
    0
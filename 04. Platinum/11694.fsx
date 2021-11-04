open System

[<EntryPoint>]
let main _ =
    do Console.ReadLine() |> ignore
    let line = Console.ReadLine().Split() |> Array.map int
    let oneCount = line |> Array.fold (fun acc x -> match x with 1 -> acc + 1 | _ -> acc) 0
    match oneCount with
    | n when n = line.Length ->
        match n with
        | _ when n &&& 1 = 0 -> printfn "koosaga"
        | _ -> printfn "cubelover"
    | _ ->
        match oneCount with
        | n when n &&& 1 = 0 && n > 0 ->
            let index = line |> Array.findIndex (fun x -> x <> 1)
            line.[index] <- 1
        | _ -> ()

        let result = line |> Array.fold (fun acc x -> acc ^^^ x) 0
        match result with
        | 0 -> printfn "cubelover"
        | _ -> printfn "koosaga"
    0
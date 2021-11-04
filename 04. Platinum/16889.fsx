open System
open System.IO

[<EntryPoint>]
let main _ =
    let precomp = Array.init 445 (fun i -> (i + 2) * (i + 3) / 2 - 1)

    use stream = new StreamReader(Console.OpenStandardInput())

    stream.ReadLine() |> ignore
    stream.ReadLine().Split()
    |> Array.map int
    |> Array.map (fun x -> 
        precomp
        |> Array.findIndex (fun p -> x <= p)
        |> function
            | x -> x + 1)
    |> Array.fold (fun acc x -> acc ^^^ x) 0
    |> function
        | 0 -> printfn "cubelover"
        | _ -> printfn "koosaga"
    0
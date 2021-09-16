open System

[<EntryPoint>]
let main argv =
    let solve =
        let divide6 x = x / 6
        let n = Console.ReadLine() |> int
        let total = [(n-2) .. n] |> List.fold (fun acc x -> acc * x) 1 |> divide6

        printfn "1"

    let t = Console.ReadLine() |> int
    [1 .. t] |> List.iter (fun _ -> solve)
    0
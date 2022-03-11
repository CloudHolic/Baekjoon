open System

[<EntryPoint>]
let main _ =
    let length = Console.ReadLine() |> int
    let line = Console.ReadLine().Split() |> Array.map int
    let mutable result = 0

    for i in 0 .. length - 1 do
        let origin = line[i]
        let temp = line |> Array.fold (fun acc x -> if (snd acc) = i then (fst acc, snd acc + 1) else (fst acc ^^^ x, snd acc + 1)) (0, 0) |> fst
        for _ in 1 .. line[i] do
            line[i] <- line[i] - 1
            line[i] ^^^ temp
            |> function
                | 0 -> result <- result + 1
                | _ -> ()
        line[i] <- origin

    printfn "%d" result
    0
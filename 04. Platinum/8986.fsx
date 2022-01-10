open System
open System.IO

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let mutable answer = int64 1e18

    stream.ReadLine() |> ignore
    let poles = stream.ReadLine().Trim().Split() |> Array.map int64

    let relation k = poles |> Array.fold (fun acc x -> (fst acc + abs (snd acc * k - x), snd acc + 1L)) (0L, 0L) |> fst

    let rec solve s e =
        if s + 3L <= e then
            let l, r = (2L * s + e) / 3L, (s + 2L * e) / 3L
            let calcL, calcR = relation l, relation r
            match calcL - calcR with
            | c when c < 0 -> solve s r
            | _ -> solve l e
        else
            for i in s .. e do
               answer <- relation i |> min answer

    solve 1L (int64 1e9)
    printfn "%d" answer
    0
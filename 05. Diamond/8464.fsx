open System
open System.IO

[<EntryPoint>]
let main _ =
    let int_sqrt (x:int64) = float x |> sqrt |> int64
    use stream = new StreamReader(Console.OpenStandardInput())
    let k = stream.ReadLine() |> int64

    let mobius = Array.zeroCreate 159705

    // Set mobius function value
    mobius |> Array.iteri (fun i _ ->
        if i > 0 then
            if i = 1 then mobius[i] <- i
            for j in (2 * i) .. i .. 159704 do
                mobius[j] <- mobius[j] - mobius[i])

    let count k = 
        [|1L .. (int_sqrt k)|]
        |> Array.fold (fun acc x -> acc + int64 mobius[int x] * k / (x * x)) 0L

    let rec solve s e =
        if s >= e - 1L then e
        else
            let mid = (s + e) / 2L
            if mid - count mid < k then solve mid e
            else solve s mid

    solve 0L 25505460948L
    |> printfn "%d"
    0
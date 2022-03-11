open System
open System.IO

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())

    let size = stream.ReadLine() |> int
    let matrix = Array.init (size + 1) (fun i -> if i = 0 then [|0; 0|] else stream.ReadLine().Split() |> Array.map int)
    let cache : int[,] = Array2D.zeroCreate <|| (size + 1, size + 1)

    for i in 1 .. size - 1 do
        for j in 1 .. size - i do
            cache[j, i + j] <- 1 <<< 30
            for k in j .. i + j - 1 do
                cache[j, i + j] <- (cache[j, i + j], cache[j, k] + cache[k + 1, i + j] + matrix[j][0] * matrix[k][1] * matrix[i + j][1]) ||> min

    printfn "%d" cache[1, size]
    0
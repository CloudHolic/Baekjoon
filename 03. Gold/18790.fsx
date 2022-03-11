open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let size = stream.ReadLine() |> int
    let nums = stream.ReadLine().Trim().Split() |> Array.map int

    let cache : bool[,,] = Array3D.init (2 * size) (size + 1) size (fun _ _ _ -> false)
    cache[0, 0, 0] <- true

    for i in 1 .. (2 * size - 1) do
        for j in 0 .. size do
            for k in 0 .. (size - 1) do
                cache[i, j, k] <- cache[i - 1, j, k]

        for j in 1 .. size do
            for k in 0 .. (size - 1) do
                let s = (k - nums[i - 1] + size) % size
                if cache[i - 1, j - 1, s] then
                    cache[i, j, k] <- true

    let mutable i, j, k = 2 * size - 1, size, 0
    while i > 0 && j > 0 do        
        let s = (k - nums[i - 1] + size) % size
        if cache[i - 1, j - 1, s] then
            j <- (j - 1)
            k <- s
            result.AppendFormat("{0} ", nums[i - 1]) |> ignore
        i <- i - 1

    printfn "%A" result
    0
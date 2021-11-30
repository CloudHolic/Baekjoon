open System
open System.IO

[<EntryPoint>]
let main _ =                    
    use stream = new StreamReader(Console.OpenStandardInput())

    let n, m = stream.ReadLine().Trim().Split() |> Array.map int |> function | nums -> nums.[0], nums.[1]
        
    let awkardness = Array2D.zeroCreate (n + 1) (n + 1)
    for i = 1 to n do
        awkardness.[i, 1 ..] <- stream.ReadLine().Trim().Split() |> Array.map int
    for i = 1 to n do
        for j = i + 1 to n do
            awkardness.[i, j] <- awkardness.[i, j] + awkardness.[i, j - 1]
    for j = 1 to n do
        for i = j - 1 downto 1 do
            awkardness.[i, j] <- awkardness.[i, j] + awkardness.[i + 1, j]

    let mutable cache = Array.init (n + 1) (fun x -> if x = 0 then 0L else int64 Int32.MaxValue)
    let mutable cache2 : int64[] = Array.zeroCreate (n + 1)

    let rec solve s e l r =
        if s <= e then
            let mid = (s + e) >>> 1

            let mutable k, best = l, (Int64.MaxValue, -1)
            while k <= min mid r do
                best <- min best (cache.[k - 1] + int64 awkardness.[k, mid], k)
                k <- k + 1

            cache2.[mid] <- fst best
            solve s (mid - 1) l (snd best)
            solve (mid + 1) e (snd best) r

    for _ = 0 to m - 1 do
        solve 1 n 1 n
        (cache, cache2)
        |> function
            | x, y ->
                cache2 <- x
                cache <- y

    printfn "%d" cache.[n]
    0
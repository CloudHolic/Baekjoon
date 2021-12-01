open System
open System.IO

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())

    let n, m = stream.ReadLine().Trim().Split() |> Array.map int |> function | nums -> nums.[0], nums.[1]
        
    let awkwardness = Array2D.zeroCreate (n + 1) (n + 1)
    for i = 1 to n do
        awkwardness.[i, 1 ..] <- stream.ReadLine().Trim().Split() |> Array.map int
    for i = 1 to n do
        for j = i + 1 to n do
            awkwardness.[i, j] <- awkwardness.[i, j] + awkwardness.[i, j - 1]
    for j = 1 to n do
        for i = j - 1 downto 1 do
            awkwardness.[i, j] <- awkwardness.[i, j] + awkwardness.[i + 1, j]

    let mutable cache = Array2D.init (m + 1) (n + 1) (fun x y -> if x = 0 && y <> 0 then int64 Int32.MaxValue else 0L)

    let rec solve step s e l r =
        if s <= e then
            let mid = (s + e) >>> 1

            let mutable k, best = l, (Int64.MaxValue, -1)
            while k <= min mid r do
                best <- min best (cache.[step - 1, k - 1] + int64 awkwardness.[k, mid], k)
                k <- k + 1

            cache.[step, mid] <- fst best
            solve step s (mid - 1) l (snd best)
            solve step (mid + 1) e (snd best) r

    for i = 1 to m do
        solve i 1 n 1 n
            
    printfn "%d" cache.[m, n]
    0
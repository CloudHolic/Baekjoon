open System
open System.IO

[<EntryPoint>]
let main _ =                    
    use stream = new StreamReader(Console.OpenStandardInput())

    let m, n = stream.ReadLine().Trim().Split() |> Array.map int |> function | nums -> nums.[0], nums.[1]
    let jailbreak = stream.ReadLine().Trim().Split() |> Array.map int64
    let sum = (0L, 0) |> Array.unfold (fun (curSum, idx) -> 
        match idx with
        | i when i > m -> None
        | i when i = m -> Some(curSum, (curSum, idx + 1))
        | _ -> Some(curSum, (curSum + jailbreak.[idx], idx + 1)))

    let relation i j = int64 (j - i + 1) * (sum.[j] - sum.[i - 1])

    let cache = Array2D.init 801 8001 (fun x y -> if x = 1 && y <= m then relation x y else 0L)
    let cache2 : int64[,] = Array2D.zeroCreate 801 8001

    let rec solve step s e l r =
        if s <= e then
            let mid = (s + e) >>> 1 |> int
            cache.[step, mid] <- -1L
            cache2.[step, mid] <- -1L

            let mutable k = l
            while k <= r do
                let temp = cache.[step - 1, int k] + relation (int k + 1) mid
                if cache.[step, int mid] = -1L || cache.[step, mid] > temp then
                    cache.[step, mid] <- temp
                    cache2.[step, mid] <- k
                k <- k + 1L

            solve step s (int64 mid - 1L) l cache2.[step, mid]
            solve step (int64 mid + 1L) e cache2.[step, mid] r

    for i = 2 to n do
        solve i 0L (int64 m) 0L (int64 m)

    printfn "%d" cache.[n, m]
    0
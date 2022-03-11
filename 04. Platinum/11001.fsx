open System
open System.IO

[<EntryPoint>]
let main _ =                    
    use stream = new StreamReader(Console.OpenStandardInput())
    let mutable result = 0L

    let n, d = stream.ReadLine().Trim().Split() |> Array.map int64 |> function | nums -> nums[0], nums[1]
    let t = stream.ReadLine().Trim().Split() |> Array.map int64
    let v = stream.ReadLine().Trim().Split() |> Array.map int64

    let temperature = Array.init (int n + 1) (fun x -> if x = 0 then 0L else t[x - 1])
    let value = Array.init (int n + 1) (fun x -> if x = 0 then 0L else v[x - 1])

    let relation i j = (j - i) * temperature[int j] + value[int i]

    let rec solve s e l r =
        if s <= e then
            let mid = s + e >>> 1
            let mutable k, i = max l (mid - d), max l (mid - d)

            while i <= min mid r do
                if relation k mid < relation i mid then
                    k <- i
                i <- i + 1L

            result <- relation k mid |> max result

            solve s (mid - 1L) l k
            solve (mid + 1L) e k r

    solve 1L n 1L n
    printfn "%d" result
    0
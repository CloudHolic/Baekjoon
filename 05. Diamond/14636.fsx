open System
open System.IO

[<EntryPoint>]
let main _ =
    let parseTwoInts f (str: string) = str.Trim().Split() |> Array.map f |> function | n -> n[0], n[1]
    use stream = new StreamReader(Console.OpenStandardInput())
    let mutable result = 0L

    let m, n = stream.ReadLine() |> parseTwoInts int
    let producers = Array.create m (0L, 0L)
    let consumers = Array.create n (0L, 0L)

    for i = 0 to m - 1 do
        producers[i] <- stream.ReadLine() |> parseTwoInts int64
    for i = 0 to n - 1 do
        consumers[i] <- stream.ReadLine() |> parseTwoInts int64

    let temp = ResizeArray<int64 * int64>()

    producers
    |> Array.sort
    |> Array.iter (fun x -> if temp.Count = 0 || snd temp[temp.Count - 1] > snd x then temp.Add x)
    let producers = temp.ToArray()

    temp.Clear()
    consumers
    |> Array.sort
    |> Array.rev
    |> Array.iter (fun x -> if temp.Count = 0 || snd temp[temp.Count - 1] < snd x then temp.Add x)
    let consumers = temp.ToArray() |> Array.rev

    let relation i j =
        let df = fst consumers[i] - fst producers[j]
        let ds = snd consumers[i] - snd producers[j]
        if df < 0L && ds < 0L then 0L else ds * df

    let rec solve s e l r =
        if s <= e then
            let mid = s + e >>> 1
            let mutable k, i, cur = s, l, Int64.MinValue

            while i <= r do
                let temp = relation i mid
                if temp > cur then
                    cur <- temp
                    k <- i
                i <- i + 1
                
            solve s (mid - 1) l k
            solve (mid + 1) e k r
            result <- max result cur

    solve 0 (producers.Length - 1) 0 (consumers.Length - 1)
    printfn "%d" result
    0
open System
open System.Collections.Generic
open System.IO

[<EntryPoint>]
let main _ = 
    use stream = new StreamReader(Console.OpenStandardInput())

    let n = stream.ReadLine().Trim() |> int
    let works = (0L, 0L) |> Array.create n

    for i = 0 to n - 1 do
        let l, d = stream.ReadLine().Trim().Split() |> Array.map int64 |> fun x -> x[0], x[1]
        works[i] <- (l + d, d)

    let mutable answer = 0L
    let pq = new PriorityQueue<int64, int64> ()

    works
    |> Array.sortBy (fun x -> fst x)
    |> Array.iter (fun x ->
        answer <- answer + snd x
        pq.Enqueue(snd x, -snd x)
        while answer > fst x do
            let top = pq.Dequeue()
            answer <- answer - top)

    printfn "%d" pq.Count
    0
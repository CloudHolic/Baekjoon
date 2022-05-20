open System
open System.Collections.Generic
open System.IO

[<EntryPoint>]
let main _ = 
    use stream = new StreamReader(Console.OpenStandardInput())

    let n = stream.ReadLine().Trim() |> int
    let works = (0, 0) |> Array.create n

    for i = 0 to n - 1 do
        let d, l = stream.ReadLine().Trim().Split() |> Array.map int |> fun x -> x[0], x[1]
        works[i] <- (d + l, d)

    let mutable answer = 0
    let pq = new PriorityQueue<int, int> ()

    works
    |> Array.sortBy (fun x -> fst x)
    |> Array.iter (fun x ->
        answer <- answer + snd x
        pq.Enqueue(snd x, -snd x)
        while answer >= fst x do
            let top = pq.Dequeue()
            answer <- answer - top)

    printfn "%d %d" pq.Count answer
    0
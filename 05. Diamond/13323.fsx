open System
open System.Collections.Generic
open System.IO

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let _ = stream.ReadLine().Trim() |> int
    let arr = stream.ReadLine().Trim().Split() |> Array.map int
    let pq = new PriorityQueue<int, int>()
    let mutable answer = 0L

    arr
    |> Array.iteri (fun i x -> 
        let cur = x - i
        pq.Enqueue(cur, -cur)
        
        if pq.Count > 0 && pq.Peek() > cur then
            answer <- pq.Dequeue() - cur |> int64 |> (+) answer
            pq.Enqueue(cur, -cur))

    printfn "%d" answer
    0
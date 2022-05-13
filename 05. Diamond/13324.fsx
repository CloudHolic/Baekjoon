open System
open System.Collections.Generic
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let n = stream.ReadLine().Trim() |> int
    let arr = stream.ReadLine().Trim().Split() |> Array.map int
    let pq = new PriorityQueue<int, int>()
    let answer = Array.zeroCreate n
    let result = new StringBuilder()

    arr
    |> Array.iteri (fun i x -> 
        let cur = x - i
        pq.Enqueue(cur, -cur)
        pq.Dequeue() |> ignore
        pq.Enqueue(cur, -cur)
        answer[i] <- pq.Peek())

    for i = n - 2 downto 0 do
        if answer[i] > answer[i + 1] then answer[i] <- answer[i + 1]

    answer
    |> Array.iteri (fun i x -> result.Append(i + x).Append("\n") |> ignore)

    Console.WriteLine(result)
    0
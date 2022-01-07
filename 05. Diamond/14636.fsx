open System
open System.Collections.Generic
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

    let temp = Stack<int64 * int64>()
    for i = 0 to m - 1 do
        let mutable flag = temp.Count > 0
        while flag do
            if temp.Count > 0 then
                let top = temp.Peek()
                if fst top > fst producers[i] && snd top > snd producers[i] then
                    temp.Pop() |> ignore
                else
                    flag <- false
            else
                flag <- false
        let top = if temp.Count = 0 then (int64 Int32.MaxValue, int64 Int32.MaxValue) else temp.Peek()
        if fst top < fst producers[i] && snd top < snd producers[i] then ()
        else producers[i] |> temp.Push

    let producers = Array.init temp.Count (fun _ -> temp.Pop()) |> Array.sort

    for i = 0 to n - 1 do
        let mutable flag = temp.Count > 0
        while flag do
            if temp.Count > 0 then
                let top = temp.Peek()
                if fst top < fst consumers.[i] && snd top < snd consumers[i] then
                    temp.Pop() |> ignore
                else
                    flag <- false
            else
                flag <- false
        let top = if temp.Count = 0 then (0L, 0L) else temp.Peek()
        if fst top > fst consumers[i] && snd top > snd consumers[i] then ()
        else consumers[i] |> temp.Push

    let consumers = Array.init temp.Count (fun _ -> temp.Pop()) |> Array.sort

    let relation con pro =
        let ds = snd con - snd pro
        let df = fst con - fst pro
        if ds > 0L && df > 0L then ds * df else 0

    let rec solve s e l r =
        if s <= e then
            let mid = s + e >>> 1
            let mutable k, i = l, l
                        
            while i <= r do
                let temp = relation consumers[i] producers[mid]
                let temp2 = relation consumers[k] producers[mid]
                if temp > 0 && temp > temp2 then
                    k <- i
                i <- i + 1

            result <- relation consumers[k] producers[mid] |> max result
            solve s (mid - 1) l k
            solve (mid + 1) e k r
        

    solve 0 (consumers.Length - 1) 0 (producers.Length - 1)
    printfn "%d" result
    0
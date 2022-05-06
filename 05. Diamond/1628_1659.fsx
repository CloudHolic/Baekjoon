open System
open System.IO

type Point = {
    Value: int
    Color: bool
}

[<EntryPoint>]
let main _ = 
    use stream = new StreamReader(Console.OpenStandardInput())

    let n, m = stream.ReadLine().Trim().Split() |> Array.map int |> fun x -> x[0], x[1]
    let sArr = stream.ReadLine().Trim().Split() |> Array.map (fun x -> { Value = int x; Color = false })
    let tArr = stream.ReadLine().Trim().Split() |> Array.map (fun x -> { Value = int x; Color = true })

    let solve (arr: Point array) =
        let size = n + m

        let near = Array.create (size + 1) Int32.MaxValue
        let sp = Array.create (size + 1) -1
        let last = Array.create (2 * (size + 1)) -1
        let matching = Array.create (size + 1) 0L
        let cache = Array.create (size + 1) 0L

        let calcNear direction =
            let boolToInt value = if value then 1 else 0

            let beg = if direction then 1 else size
            let en = if direction then size else 1
            let step = if direction then 1 else -1
            
            let color = Array.create 2 0
            for i in beg .. step .. en do
                if color[boolToInt <| not arr[i].Color] > 0 then 
                    near[i] <- (near[i], step * (arr[i].Value - arr[color[boolToInt <| not arr[i].Color]].Value)) ||> min
                else ()
                color[boolToInt arr[i].Color] <- i

        calcNear true
        calcNear false

        last[size] <- 0
        let mutable now = 0

        for i = 1 to size do
            match arr[i].Color with
            | true ->
                now <- now + 1
                matching[i] <- arr[i].Value
            | false ->
                now <- now - 1
                matching[i] <- -arr[i].Value

            matching[i] <- matching[i] + matching[i - 1]
            if last[now + size] < 0 then ()
            else
                sp[i] <- last[now + size]

            last[now + size] <- i

        for i = 1 to size do
            cache[i] <- cache[i - 1] + int64 near[i]
            if sp[i] < 0 then ()
            else
                cache[i] <- (cache[i], cache[sp[i]] + abs (matching[i] - matching[sp[i]])) ||> min
        
        cache[size]
    
    [sArr; tArr]
    |> Array.concat
    |> Array.sortBy (fun x -> x.Value)
    |> fun arr -> [[|{Value = 0; Color = false}|]; arr]
    |> Array.concat
    |> solve
    |> printfn "%d"
    0
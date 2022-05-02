open System
open System.IO

[<EntryPoint>]
let main _ =
    let MOD = 998244353
    let cache = Array2D.zeroCreate 200001 448

    let rec partition (n:int) (k:int) =
        match (n, k) with
        | (0, 0) -> 1
        | (a, b) when a <= 0 || b <= 0 -> 0
        | (a, b) when a < b -> 0
        | (a, b) -> 
            if cache[a, b] > 0 then ()
            else
                let v1 = (a - b, b) ||> partition
                let v2 = (a - 1, b - 1) ||> partition
                v1 + v2
                |> fun x -> if x >= MOD then x - MOD else x
                |> fun x -> cache[a, b] <- x
            cache[a, b]
        
    use stream = new StreamReader(Console.OpenStandardInput())    
    let t = stream.ReadLine() |> int

    for _ = 1 to t do
        let cur = stream.ReadLine() |> int
        cur % 2
        |> fun x -> if x = 0 then x + 2 else x
        |> List.unfold (fun state -> if cur >= state * state then Some((state * state, state), state + 2) else None)
        |> List.fold (fun acc elem ->             
            let temp, i = (cur - fst elem) >>> 1, snd elem
            (temp + i, i)
            ||> partition
            |> fun x -> x + acc
            |> fun x -> if x >= MOD then x - MOD else x) 0
        |> printfn "%d"

    0
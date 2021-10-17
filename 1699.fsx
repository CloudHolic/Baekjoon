#nowarn "40"

open System
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let memoize f =
        let cache = new Dictionary<_, _>()
        (fun x ->
            match cache.TryGetValue x with
            | true, v -> v
            | false, _ ->
                let v = f x
                cache.Add (x, v)
                v)

    let rec solve = memoize (fun n ->
        match n with
        | 0 -> 0
        | 1 -> 1
        | _ ->
            let sqrtSize = (sqrt <| float n) |> int
            let mutable value = n
            let mutable cur = 1
            while cur <= sqrtSize do
                value <- min value ((solve <| n - cur * cur) + 1)
                cur <- cur + 1
            value)

    Console.ReadLine().Trim() |> int
    |> solve
    |> printfn "%d"
    0
open System

[<EntryPoint>]
let main _ =
    let makePoint = Array.map int
    let makeVec = Array.map2 (fun x y -> x - y)

    let first = Console.ReadLine().Split() |> makePoint
    let second = Console.ReadLine().Split() |> makePoint
    let third = Console.ReadLine().Split() |> makePoint

    let vec1 = makeVec second first
    let vec2 = makeVec third second
    
    let ccw = vec1[0] * vec2[1] - vec1[1] * vec2[0]
    match ccw with
    | c when c < 0 -> printfn "-1"
    | c when c > 0 -> printfn "1"
    | _ -> printfn "0"
    0
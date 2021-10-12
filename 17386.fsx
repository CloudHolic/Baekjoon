open System

[<EntryPoint>]
let main argv =
    let makePoints = Array.map int
    let splitPoint (a: int []) = (a.[0 .. 1], a.[2 .. 3])
    let ccw a b c =        
        let makeVec = Array.map2 (fun x y -> x - y)
        let vec1 = makeVec b a
        let vec2 = makeVec c b
        vec1.[0] * vec2.[1] - vec1.[1] * vec2.[0]

    let first = Console.ReadLine().Split() |> makePoints |> splitPoint
    let second = Console.ReadLine().Split() |> makePoints |> splitPoint
    
    let ccw1 = (fst first, snd first, fst second) |||> ccw
    let ccw2 = (fst first, snd first, snd second) |||> ccw
    let ccw3 = (fst second, snd second, fst first) |||> ccw
    let ccw4 = (fst second, snd second, snd first) |||> ccw

    (ccw1 * ccw2, ccw3 * ccw4)
    |> function
        | (c1, c2) when c1 < 0 && c2 < 0 -> printfn "1"
        | _ -> printfn "0"
    0
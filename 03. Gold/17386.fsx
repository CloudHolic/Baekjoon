open System

[<EntryPoint>]
let main _ =
    let makePoints = Array.map int64
    let splitPoint (a: int64 []) = (a[0 .. 1], a[2 .. 3])
    let ccw a b c =
        let makeVec = Array.map2 (fun x y -> x - y)
        let vec1 = makeVec b a
        let vec2 = makeVec c b
        vec1[0] * vec2[1] - vec1[1] * vec2[0]
        |> function
            | c when c < 0L -> -1
            | c when c > 0L -> 1
            | _ -> 0

    let first = Console.ReadLine().Split() |> makePoints |> splitPoint
    let second = Console.ReadLine().Split() |> makePoints |> splitPoint
    
    let abc = (fst first, snd first, fst second) |||> ccw
    let abd = (fst first, snd first, snd second) |||> ccw
    let cda = (fst second, snd second, fst first) |||> ccw
    let cdb = (fst second, snd second, snd first) |||> ccw

    (abc * abd, cda * cdb)
    |> function
        | (c1, c2) when c1 < 0 && c2 < 0 -> printfn "1"
        | _ -> printfn "0"
    0
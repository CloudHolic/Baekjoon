open System

[<EntryPoint>]
let main _ =
    let unpack tup ind =
        let (a, b, c, d) = tup
        match ind with
        | 0 -> a
        | 1 -> b
        | 2 -> c
        | 3 -> d
        | _ -> failwith("Unpack failed - Invalid index.")       

    let makePoints = Array.map int
    let splitPoint (a: int []) = (a.[0 .. 1], a.[2 .. 3], a.[4 .. 5], a.[6 .. 7])
    let ccw a b c =
        let makeVec = Array.map2 (fun x y -> x - y)
        let vec1 = makeVec b a
        let vec2 = makeVec c b
        vec1.[0] * vec2.[1] - vec1.[1] * vec2.[0]
        |> function
            | c when c < 0 -> -1
            | c when c > 0 -> 1
            | _ -> 0

    let points = Console.ReadLine().Trim().Split() |> makePoints |> splitPoint
    
    let abc = (unpack points 0, unpack points 1, unpack points 2) |||> ccw
    let abd = (unpack points 0, unpack points 1, unpack points 3) |||> ccw
    let cda = (unpack points 2, unpack points 3, unpack points 0) |||> ccw
    let cdb = (unpack points 2, unpack points 3, unpack points 1) |||> ccw

    (abc * abd, cda * cdb)
    |> function
        | (c1, c2) when c1 < 0 && c2 < 0 -> printfn "1"
        | _ -> printfn "0"
    0
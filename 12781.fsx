open System

[<EntryPoint>]
let main _ =
    let first (a, _, _ , _) = a
    let second (_, b, _, _) = b
    let third (_, _, c, _) = c
    let fourth (_, _, _, d) = d

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

    let points = Console.ReadLine().Split() |> makePoints |> splitPoint
    
    let abc = (first points, second points, third points) |||> ccw
    let abd = (first points, second points, fourth points) |||> ccw

    abc * abd
    |> function
        | c when c < 0 -> printfn "1"
        | _ -> printfn "0"
    0
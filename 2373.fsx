open System

[<EntryPoint>]
let main _ =
    let fibSeq : int64 seq = Seq.unfold (fun (a, b) -> Some(a + b, (b, a + b))) (int64 0, int64 1)
    let fibList (n:int64) = fibSeq |> Seq.takeWhile (fun x -> x <= n) |> Seq.toList |> List.rev

    let size = Console.ReadLine() |> int64

    let rec solve (n:int64) (fiblist: int64 list) =
        match (n, List.head fiblist) with        
        | (n, h) when n = h -> n
        | _ -> let newSize = n - List.head fiblist in (newSize, List.filter (fun x -> x <= newSize) fiblist) ||> solve
    
    match (size, fibList size) with
    | (n, l) when n = List.head l -> printfn "-1"
    | (n, l) -> printfn "%d" <| solve n l
    0
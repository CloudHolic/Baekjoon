open System

[<EntryPoint>]
let main _ =
    let solve n =
        let divide2 x = x / 2
        let divide6 x = x / 6

        let triangle =
            [1 .. (n - 1)]
            |> List.map (fun _ -> 
                let line = Console.ReadLine().Split()
                Array.map int line)
            |> List.toArray

        let angles i =
            let group = 
                match i with 
                | 1 -> [||]
                | _ -> [0..(i - 2)] |> List.map (fun x -> triangle.[x].[i - x - 2]) |> List.toArray
                |> Array.append <| if i = n then [||] else triangle.[i - 1]
                |> Array.countBy (fun x -> x)
            match Array.length group with
            | 1 -> 0
            | _ -> Array.fold (fun acc x -> acc * snd x) 1 group
            
        let total = [(n-2) .. n] |> List.fold (fun acc x -> acc * x) 1 |> divide6
        printfn "%d" <| total - (divide2 <| List.fold (fun acc i -> acc + angles i) 0 [1..n])
        
    let t = Console.ReadLine() |> int
    [1 .. t] |> List.iter (fun _ -> Console.ReadLine() |> int |> solve)
    0
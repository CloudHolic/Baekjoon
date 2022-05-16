open System
open System.IO

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let n = stream.ReadLine().Trim() |> int
    let arr = stream.ReadLine().Trim().Split() |> Array.map int
    let freq = arr |> Array.countBy id |> Array.sortBy (fun x -> -snd x)

    let findIdx x = 
        match freq |> Array.tryFindIndex (fun (c, f) -> x <> c && f > 0) with
        | Some idx -> idx
        | None -> freq |> Array.findIndex (fun (_, f) -> f > 0)

    if snd freq[0] > n / 2 then printfn "-1"
    else
        let answer = 
            arr
            |> Array.map (fun x ->
                let idx = findIdx x
                freq[idx] <- (fst freq[idx], snd freq[idx] - 1)
                fst freq[idx])
        
        let maxIdx = arr |> Array.indexed |> Array.filter (fun (_, x) -> x = fst freq[0]) |> Array.map fst
        for i = 0 to n - 1 do
            if arr[i] = answer[i] then
                let idx = maxIdx |> Array.find (fun x -> arr[x] = fst freq[0] && answer[x] <> answer[i])
                let temp = answer[i]
                answer[i] <- answer[idx]
                answer[idx] <- temp

        answer |> Array.iter (fun x -> printf "%d " x)
    0
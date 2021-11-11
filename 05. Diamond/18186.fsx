open System
open System.IO

[<EntryPoint>]
let main _ =
    let min3 a b c = min <|| (a, min b c)

    use stream = new StreamReader(Console.OpenStandardInput())
    let size, b, c = stream.ReadLine().Split() |> Array.map int |> function | arr -> arr.[0], arr.[1], arr.[2]
    let arr = stream.ReadLine().Split() |> Array.map int |> function | a -> Array.append a [|0; 0|]

    if b <= c
    then
        printfn "%d" <| int64 b * (Array.sumBy (fun x -> int64 x) arr)
    else
        let mutable answer = 0L
        for i in 0 .. size - 1 do
            if arr.[i + 1] > arr.[i + 2] then
                let minimum = min <|| (arr.[i], arr.[i + 1] - arr.[i + 2])
                answer <- answer + int64 (b + c) * int64 minimum
                arr.[i] <- arr.[i] - minimum
                arr.[i + 1] <- arr.[i + 1] - minimum

                let minimum = min3 <||| (arr.[i], arr.[i + 1], arr.[i + 2])
                answer <- answer + int64 (b + c + c) * int64 minimum
                arr.[i] <- arr.[i] - minimum
                arr.[i + 1] <- arr.[i + 1] - minimum
                arr.[i + 2] <- arr.[i + 2] - minimum
            else
                let minimum = min3 <||| (arr.[i], arr.[i + 1], arr.[i + 2])
                answer <- answer + int64 (b + c + c) * int64 minimum
                arr.[i] <- arr.[i] - minimum
                arr.[i + 1] <- arr.[i + 1] - minimum
                arr.[i + 2] <- arr.[i + 2] - minimum

                let minimum = min <|| (arr.[i], arr.[i + 1])
                answer <- answer + int64 (b + c) * int64 minimum
                arr.[i] <- arr.[i] - minimum
                arr.[i + 1] <- arr.[i + 1] - minimum

            answer <- answer + int64 b * int64 arr.[i]

        printfn "%d" answer

    0
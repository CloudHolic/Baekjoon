open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    let square n = n * n

    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let t = stream.ReadLine() |> int

    let rec solve times =
        if times > 0 then
            let n = stream.ReadLine() |> int64 |> function | n -> (n + (n &&& 1L)) / 2L
            let mutable answer, multiplier, boundary = 0L, 1L, n
            
            while boundary > 0L do
                let next = (boundary + 1L) / 3L
                answer <- answer + multiplier * (square boundary - square next)
                boundary <- next
                multiplier <- multiplier * 2L

            result.AppendFormat("{0}\n", answer) |> ignore
            times - 1 |> solve

    solve t
    printfn "%A" result
    0
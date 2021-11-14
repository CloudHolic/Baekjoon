open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let mutable pprev, prev = 6, 28
    let precomputeArr = Array.init 100 (fun _ ->
        let temp = prev
        prev <- (6 * prev - 4 * pprev) % 1000
        if prev < 0 then prev <- prev + 1000
        pprev <- temp
        prev)

    let tests = stream.ReadLine() |> int

    for i in 1 .. tests do
        let n = stream.ReadLine() |> int64
        match n with
        | 2L -> result.AppendFormat("Case #{0}: {1:D3}\n", i, 27) |> ignore
        | _ -> result.AppendFormat("Case #{0}: {1:D3}\n", i, precomputeArr.[(n - 3L) % 100L |> int] - 1) |> ignore

    printfn "%A" result
    0
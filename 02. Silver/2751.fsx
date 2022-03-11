open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let size = stream.ReadLine() |> int
    let arr = Array.zeroCreate size

    for i in 1 .. size do
        arr[i - 1] <- stream.ReadLine() |> int

    arr
    |> Array.sort
    |> Array.iter (fun x -> result.AppendFormat("{0}\n", x) |> ignore)

    printfn "%A" result
    0
open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    let result = new StringBuilder()
    let formatString = "{0} to {1}\n"

    let rec solve n s e =
        match n with
        | 3 ->
            result.AppendFormat(formatString, s + 1, s - 2)
                .AppendFormat(formatString, e - 1, s + 1)
                .AppendFormat(formatString, s + 2, s - 4) |> ignore
        | 4 ->
            result.AppendFormat(formatString, e - 2, s - 2)
                .AppendFormat(formatString, s + 2, e - 2)
                .AppendFormat(formatString,s - 1 , s + 2)
                .AppendFormat(formatString, e - 1, s - 1) |> ignore
        | 5 -> 
            result.AppendFormat(formatString, e - 2, s - 2)
                .AppendFormat(formatString, s + 2, e - 2)
                .AppendFormat(formatString, e - 4, s + 2)
                .AppendFormat(formatString, s - 1, e - 4)
                .AppendFormat(formatString, e - 1, s - 1) |> ignore
        | 6 ->        
            result.AppendFormat(formatString, e - 2, s - 2)
                .AppendFormat(formatString, s + 2, e - 2)
                .AppendFormat(formatString, e - 4, s + 2)
                .AppendFormat(formatString, s - 1, e - 4)
                .AppendFormat(formatString, e - 1, s - 1) |> ignore
        | 7 ->
            result.AppendFormat(formatString, e - 2, s - 2)
                .AppendFormat(formatString, s + 2, e - 2)
                .AppendFormat(formatString, e - 4, s + 2)
                .AppendFormat(formatString, s - 1, e - 4)
                .AppendFormat(formatString, e - 1, s - 1) |> ignore
        | _ ->
            result.AppendFormat(formatString, e - 2, s - 2)
                .AppendFormat(formatString, s + 2, e - 2) |> ignore
            solve (n - 4) (s + 4) (e - 4)
            result.AppendFormat(formatString, s - 1, e - 5)
                .AppendFormat(formatString, e - 1, s - 1)|> ignore

    use stream = new StreamReader(Console.OpenStandardInput())
    let length = stream.ReadLine() |> int

    solve length 1 (2 * length)
    printfn "%A" result    
    0
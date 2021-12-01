open System
open System.IO

[<EntryPoint>]
let main _ =
    let bool n = if n <> 0 then true else false

    use stream = new StreamReader(Console.OpenStandardInput())
    let n, m = stream.ReadLine().Trim().Split() |> Array.map int |> function | nums -> nums.[0], nums.[1]

    let board = Array2D.zeroCreate (n + 2) (m + 2)
    for i = 1 to n do
        board.[i, 1 .. m] <- stream.ReadLine().Trim() |> Seq.toArray |> Array.map (fun x -> x = 'X')

    let cache = Array4D.create (n + 2) (n + 2) (m + 2) (m + 2) -1

    let rec solve n1 n2 m1 m2 =
        if n1 < 1 || n2 > n || m1 < 1 || m2 > m || n1 > n2 || m1 > m2 then 0
        else
            if cache.[n1, n2, m1, m2] <> -1 then cache.[n1, n2, m1, m2]
            else
                let mutable set = Set.empty<int>
                for i = n1 to n2 do
                    for j = m1 to m2 do
                        if not board.[i, j] then
                            let value = (solve n1 (i - 1) m1 (j - 1)) ^^^ (solve n1 (i - 1) (j + 1) m2) ^^^ (solve (i + 1) n2 m1 (j - 1)) ^^^ (solve (i + 1) n2 (j + 1) m2)
                            set <- set.Add(value)

                let mutable cur, flag, result = 0, true, 0
                while flag do
                    if set |> Set.filter (fun x -> x = cur) |> Set.count |> bool |> not then
                        flag <- false
                        result <- cur
                        cache.[n1, n2, m1, m2] <- cur
                    cur <- cur + 1

                result
            
    match solve 1 n 1 m |> bool with
    | true -> printfn "First"
    | _ -> printfn "Second"

    0
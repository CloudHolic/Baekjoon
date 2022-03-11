open System
open System.IO

[<EntryPoint>]
let main _ =    
    use stream = new StreamReader(Console.OpenStandardInput())
    let n = stream.ReadLine().Trim() |> int

    let starts = Array.zeroCreate n
    let ends = Array.zeroCreate n
    let mutable maxStation = 0

    for i = 0 to n - 1 do
        stream.ReadLine().Trim().Split()
        |> Array.map int
        |> function
            | nums ->
                maxStation <- max maxStation nums[1]
                starts[i] <- nums[0]
                ends[i] <- nums[1]

    let policy1 =
        let prefix = Array.zeroCreate (maxStation + 1)
        let suffix = Array.zeroCreate (maxStation + 1)

        for i = 0 to n - 1 do
            prefix[ends[i]] <- prefix[ends[i]] + 1
            suffix[maxStation - starts[i]] <- suffix[maxStation - starts[i]] + 1

        prefix
        |> Array.iteri (fun i _ ->
            let prev = if i = 0 then 0 else prefix[i - 1]
            prefix[i] <- prefix[i] + prev)

        suffix
        |> Array.iteri (fun i _ ->
            let prev = if i = 0 then 0 else suffix[i - 1]
            suffix[i] <- suffix[i] + prev)

        let mutable result = 0
        for i = 0 to n - 1 do
            result <- max result (n - prefix[starts[i]] - suffix[maxStation - ends[i]])

        result

    let policy2 =
        let imos = Array.zeroCreate (maxStation + 1)
        
        for i = 0 to n - 1 do
            imos[starts[i]] <- imos[starts[i]] + 1
            imos[ends[i]] <- imos[ends[i]] - 1

        imos
        |> Array.iteri (fun i _ ->
            let prev = if i = 0 then 0 else imos[i - 1]
            imos[i] <- imos[i] + prev)

        imos |> Array.max

    printfn "%d %d" policy1 policy2
    0
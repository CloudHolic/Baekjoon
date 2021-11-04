// Not finished
open System
open System.IO

type Edge = { Color: bool; Start: int; End: int; }

[<EntryPoint>]
let main _ = 
    let splitInts = Array.map int
    let clamped min max a = 
        (a >= min && a <= max)
        |> function
            | true -> 1
            | false -> 0

    let makeEdge (str: string array) = 
        { Color = str.[0] = "B"; Start = str.[1] |> int; End = str.[2] |> int}

    let mst (arr: Edge array) =
        let mutable added = []

        added.Length

    use stream = new StreamReader(Console.OpenStandardInput())
    let nmk = stream.ReadLine().Split() |> splitInts
    while (nmk.[0] <> 0) do
        let mutable edgeList = []
        for i in 1 .. nmk.[1] do
            edgeList <- (makeEdge <| stream.ReadLine().Split()) :: edgeList            
        let nmk = stream.ReadLine().Split() |> splitInts

        let edgeArr = Array.ofList edgeList
        let minK = edgeArr |> Array.sortBy (fun x -> x.Color) |> mst
        let maxK = edgeArr |> Array.sortBy (fun x -> not x.Color) |> mst
        clamped minK maxK nmk.[2]
        |> printfn "%d"
        
    0
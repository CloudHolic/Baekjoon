open System
open System.IO

type ConvexHullTrick =
    val mutable private pointer: int
    val private lines: ResizeArray<Line>
    member internal _.Inf = 10000000000L

    new () = { pointer = 0; lines = ResizeArray<Line>() }

    member private _.Intersect line1 line2 = (line2.b - line1.b |> float) / (line1.a - line2.a |> float)

    member this.AddLine a b =
        let newLine = { a = a; b = b; x = double -this.Inf }

        if this.lines.Count = 0 then this.lines.Add newLine
        else
            let mutable flag = true
            while this.lines.Count > 0 && flag do
                let top = this.lines.[this.lines.Count - 1]
                let x = this.Intersect top newLine
                if x <= top.x then this.lines.RemoveAt(this.lines.Count - 1)
                else flag <- false

            newLine.x <- this.Intersect this.lines.[this.lines.Count - 1] newLine
            this.lines.Add(newLine)

            if this.pointer >= this.lines.Count then
                this.pointer <- this.lines.Count - 1

    member this.Query x =
        while this.pointer < this.lines.Count - 1 && this.lines.[this.pointer + 1].x < float x do
            this.pointer <- this.pointer + 1
        this.lines.[this.pointer].eval x

and Line = {
    a: int64
    b: int64 
    mutable x: float } with

    member this.eval x = this.a * x + this.b    

[<EntryPoint>]
let main _ = 
    let parseInts (str: string) = str.Trim().Split() |> Array.map int64

    use stream = new StreamReader(Console.OpenStandardInput())
    let n = stream.ReadLine() |> int

    let lands = Array.create n (0L, 0L)
    let orderedLands = Array.create n (0L, 0L)

    for i = 0 to n - 1 do
        lands.[i] <- stream.ReadLine() |> parseInts |> function | nums -> nums.[0], nums.[1]
    let lands = lands |> Array.sortBy fst

    let mutable newSize = 0
    for i = 0 to n - 1 do
        while newSize > 0 && snd orderedLands.[newSize - 1] <= snd lands.[i] do
            newSize <- newSize - 1

        orderedLands.[newSize] <- lands.[i]
        newSize <- newSize + 1

    let cht = ConvexHullTrick()
    (snd orderedLands.[0], 0L) ||> cht.AddLine

    for i = 0 to newSize - 2 do
        let res = fst orderedLands.[i] |> cht.Query
        (snd orderedLands.[i + 1], res) ||> cht.AddLine

    fst orderedLands.[newSize - 1]
    |> cht.Query
    |> printfn "%d"

    0
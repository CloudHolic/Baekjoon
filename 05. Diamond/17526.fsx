open System
open System.IO

type ConvexHullTrick =
    val mutable private pointer: int
    val private lines: ResizeArray<Line>
    member internal _.Inf = 10000000000L

    new () = { pointer = 0; lines = ResizeArray<Line>() }

    member private _.Intersect line1 line2 = (line2.b - line1.b) / (line1.a - line2.a)

    member this.AddLine a b =
        let newLine = { a = a; b = b; x = this.Inf }

        if this.lines.Count = 0 then this.lines.Add newLine
        else
            let mutable flag = true
            while this.lines.Count > 0 && flag do
                let top = this.lines.[this.lines.Count - 1]
                let x = this.Intersect top newLine
                if x >= top.x then this.lines.RemoveAt(this.lines.Count - 1)
                else flag <- false

            this.lines.[this.lines.Count - 1].x <- this.Intersect this.lines.[this.lines.Count - 1] newLine
            this.lines.Add(newLine)

            if this.pointer >= this.lines.Count then
                this.pointer <- this.lines.Count - 1

    member this.Query x =
        while this.pointer < this.lines.Count - 1 && this.lines.[this.pointer + 1].x < x do
            this.pointer <- this.pointer + 1
        this.lines.[this.pointer].eval x

and Line = {
    a: int64
    b: int64 
    mutable x: int64 } with

    member this.eval x = this.a * x + this.b    

[<EntryPoint>]
let main _ = 
    let parseInts (str: string) = str.Trim().Split() |> Array.map int64

    use stream = new StreamReader(Console.OpenStandardInput())
    let n = stream.ReadLine() |> int

    let distance = Array.create (n + 1) 0L
    let spaceship = Array.create n (0L, 0L)

    stream.ReadLine() |> parseInts
    |> Array.iteri (fun i x -> distance.[i + 2] <- distance.[i + 1] + x)

    for i = 1 to n - 1 do
        spaceship.[i] <- stream.ReadLine() |> parseInts |> function | nums -> nums.[0], nums.[1]

    let cht = ConvexHullTrick()
    (-snd spaceship.[1], -fst spaceship.[1]) ||> cht.AddLine

    for i = 2 to n - 1 do
        let res = distance.[i] |> cht.Query
        let bias = distance.[i] * snd spaceship.[i] + res - fst spaceship.[i]
        (-snd spaceship.[i], bias) ||> cht.AddLine

    -distance.[n]
    |> cht.Query
    |> printfn "%d"

    0
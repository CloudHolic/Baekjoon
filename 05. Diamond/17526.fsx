open System
open System.IO

type LiChao =
    val private node: ResizeArray<Node>
    member internal _.Inf = Int64.MaxValue

    new (st: int64, en: int64) as this =
        { node = ResizeArray<Node>() }
        then this.node.Add { left = -1; right = -1; st = st; en = en; line = { a = 0L; b = this.Inf }}

    member this.Query x =
        let rec query k =
            if k = -1 then this.Inf
            else
                let cur = this.node.[k]
                let mid = (cur.st + cur.en) >>> 1

                if x <= mid then 
                    (this.node.[k].line.eval x, query this.node.[k].left) ||> min
                else (this.node.[k].line.eval x, query this.node.[k].right) ||> min

        query 0

    member this.AddLine (line: Line) =
        let rec add (l: Line) k =
            let cur = this.node.[k]
            let mid = (cur.st + cur.en) >>> 1

            let mutable lowLine = this.node.[k].line
            let mutable highLine = l

            if lowLine.eval cur.st > highLine.eval cur.st then
                let temp = lowLine
                lowLine <- highLine
                highLine <- temp

            if lowLine.eval cur.en <= highLine.eval cur.en then
                this.node.[k].line <- lowLine
            else if lowLine.eval mid > highLine.eval mid then
                this.node.[k].line <- highLine
                if this.node.[k].left = -1 then
                    this.node.[k].left <- this.node.Count
                    this.node.Add { left = -1; right = -1; st = cur.st; en = mid; line = { a = 0L; b = this.Inf }}

                (lowLine, this.node.[k].left) ||> add
            else 
                this.node.[k].line <- lowLine
                if this.node.[k].right = -1 then
                    this.node.[k].right <- this.node.Count
                    this.node.Add { left = -1; right = -1; st = mid + 1L; en = cur.en; line = { a = 0L; b = this.Inf }}

                (highLine, this.node.[k].right) ||> add

        add line 0

and Line = {
    a: int64
    b: int64 } with

    member this.eval x = this.a * x + this.b

and Node = {
    mutable left: int
    mutable right: int
    st: int64
    en: int64
    mutable line: Line }

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

    let cht = LiChao(1L, 1000000001L)
    {a = snd spaceship.[1]; b = fst spaceship.[1]} |> cht.AddLine

    for i = 2 to n - 1 do
        let res = distance.[i] |> cht.Query
        let bias = fst spaceship.[i] - distance.[i] * snd spaceship.[i] + res
        { a = snd spaceship.[i]; b = bias } |> cht.AddLine

    distance.[n]
    |> cht.Query
    |> printfn "%d"

    0
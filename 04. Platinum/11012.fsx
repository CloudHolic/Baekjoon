open System
open System.IO
open System.Text

type PersistentSegmentTree<'T> =
    val private op: 'T -> 'T -> 'T
    val private init: 'T
    val private rootCount: int
    val private maxSize: int
    val private nodes: ResizeArray<Node<'T>>    // Array of all nodes
    val private roots: int array                // Array of root indexes

    new (_rootCount, _maxSize, _op, _init) as this =
        {
            op = _op
            init = _init
            rootCount = _rootCount
            maxSize = _maxSize
            roots = Array.create (_rootCount + 2) 0
            nodes = new ResizeArray<Node<'T>>(2)
        } then
        this.Init

    member private this.Init =
        let rec init index st en =
            if st <> en then
                let mid = (st + en) >>> 1

                this.nodes.[index].Left <- this.nodes.Count
                Node<'T>.CreateNew this.init |> this.nodes.Add

                this.nodes.[index].Right <- this.nodes.Count
                Node<'T>.CreateNew this.init |> this.nodes.Add

                (this.nodes.[index].Left, st, mid) |||> init
                (this.nodes.[index].Right, mid + 1, en) |||> init

        this.roots.[0] <- 1
        init 1 1 (this.maxSize + 1)

    member this.Query root left right =
        0

    member this.Update root index value =
        ()

and Node<'T> = {
    mutable Value: 'T
    mutable Left: int
    mutable Right: int } with

    static member CreateNew value = { Value = value; Left = 0; Right = 0 }

[<CustomComparison; CustomEquality>]
type Point = {
    mutable X: int;
    mutable Y: int } with
    
    override this.GetHashCode() = hash this.Y
    
    interface IComparable<Point> with
        member this.CompareTo other =
            compare this.Y other.Y
    
    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | null -> 1
            | :? Point as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not a Score"
    
    interface IEquatable<Point> with
        member this.Equals other =
            this.Y = other.Y
    
    override this.Equals obj =
        match obj with
        | :? Point as other -> (this :> IEquatable<_>).Equals other
        |_ -> false

[<EntryPoint>]
let main _ = 
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let t = stream.ReadLine().Trim() |> int
    let mutable answer = 0

    let parseInts (str: string) = str.Trim().Split() |> Array.map int

    let rec solve times =
        if times > 0 then
            let n, m = stream.ReadLine().Trim().Split() |> Array.map int |> function | nums -> nums.[0], nums.[1]
            let pst = PersistentSegmentTree<int>(100000, 100000, (+), 0)
            
            Array.init n (fun _ -> stream.ReadLine() |> parseInts |> function | nums -> { X = nums.[0]; Y = nums.[1] })
            |> Array.sort
            |> Array.iter (fun x -> pst.Update x.Y x.X 1)

            for _ = 1 to m do
                let region = stream.ReadLine() |> parseInts
                answer <- answer + pst.Query region.[3] region.[0] region.[1] - pst.Query (region.[2] - 1) region.[0] region.[1]

            result.AppendFormat("{0}\n", answer) |> ignore
            times - 1 |> solve

    solve t
    printfn "%A" result
    0
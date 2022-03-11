open System
open System.IO
open System.Text

type SegmentTree<'T> =
    val private op1: 'T -> 'T -> 'T
    val private op2: 'T -> 'T -> 'T
    val private init: 'T
    val private node1: 'T array // Sum segtree
    val private node2: 'T array // GCD segtree
    val private size: int
    val private leafCount: int

    new (count: int, _op1, _op2, _init) =
        let lc = SegmentTree<'T>.LeastSquare count + 1 in
        {
            op1 = _op1
            op2 = _op2
            init = _init
            leafCount = lc
            size = count
            node1 = Array.create (2 * lc) _init
            node2 = Array.create (2 * lc) _init
        }

    static member private LeastSquare k =
        if k = 0 then 0
        else
            let rec loop i t =
                if i >= 64 then t
                else loop (i * 2) (t ||| (t >>> i))
            loop 1 (k - 1) + 1

    static member private LeftChild k = k * 2
    static member private RightChild k = k * 2 + 1

    member this.Query1 left right =
        let rec query k st en =
            if left > en || right < st then this.init
            else if left <= st && en <= right then this.node1[k]
            else
                let mid = (st + en) >>> 1
                ((SegmentTree<'T>.LeftChild k, st, mid) |||> query, (SegmentTree<'T>.RightChild k, mid + 1, en) |||> query) ||> this.op1

        query 1 1 this.size

    member this.Query2 left right =
        let rec query k st en =
            if left > en || right < st then this.init
            else if left <= st && en <= right then this.node2[k]
            else
                let mid = (st + en) >>> 1
                ((SegmentTree<'T>.LeftChild k, st, mid) |||> query, (SegmentTree<'T>.RightChild k, mid + 1, en) |||> query) ||> this.op2

        query 1 1 this.size

    member this.Update index value =
        let rec update k st en =
            if index < st || index > en then ()
            else if st = en then
                this.node1[k] <- (this.node1[k], value) ||> this.op1
                this.node2[k] <- (this.node2[k], value) ||> this.op1
            else
                let mid = (st + en) >>> 1
                (SegmentTree<'T>.LeftChild k, st, mid) |||> update
                (SegmentTree<'T>.RightChild k, mid + 1, en) |||> update

                this.node1[k] <- (this.node1[SegmentTree<'T>.LeftChild k], this.node1[SegmentTree<'T>.RightChild k]) ||> this.op1
                this.node2[k] <- (this.node1[SegmentTree<'T>.LeftChild k], this.node1[SegmentTree<'T>.RightChild k]) ||> this.op2

        update 1 1 this.size |> ignore

[<EntryPoint>]
let main _ = 
    let parseInts f (str: string) = str.Trim().Split() |> Array.map f
    let rec gcd (x: int64) (y: int64) =
        let rec inner x y =
            if y = 0L then x
            else inner y (x % y)

        if x = 0L || y = 0L then (x + y) |> abs
        else (abs x, abs y) ||> inner

    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let n = stream.ReadLine().Trim() |> int
    let nums = stream.ReadLine() |> parseInts int64  
    let segtree = SegmentTree<int64>(n, (+), gcd, 0L)

    for i = 1 to n do
        let prev = if i = 1 then 0L else nums[i - 2]
        nums[i - 1] - prev |> segtree.Update i
    
    let q = stream.ReadLine().Trim() |> int
    let rec solve times =
        if times > 0 then
            let query = stream.ReadLine() |> parseInts int
            match int64 query[0] with
            | 0L ->
                let a = (1, query[1]) ||> segtree.Query1
                let b = (1, query[2]) ||> segtree.Query1
                let g = (query[1] + 1, query[2]) ||> segtree.Query2
                result.AppendFormat("{0}\n", ((a, b) ||> gcd, g) ||> gcd) |> ignore
            | t ->
                (query[1], t) ||> segtree.Update
                (query[2] + 1, -t) ||> segtree.Update

            times - 1 |> solve

    solve q
    printfn "%A" result
    0
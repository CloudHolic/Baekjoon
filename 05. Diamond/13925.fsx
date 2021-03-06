open System
open System.IO
open System.Text

let modulo = 1000000007L

let Add x y = (x + y) % modulo
let Mul x y = (x * y) % modulo

type LazySegmentTree<'T when 'T: equality> =
    val private op: 'T -> 'T -> 'T
    val private updateOp: int -> 'T -> 'T * 'T -> 'T
    val private propOp: 'T * 'T -> 'T * 'T -> 'T * 'T
    val private init: 'T
    val private lazyInit: 'T * 'T
    val private node: 'T[]
    val private lazyNodes: LazyNode<'T>[]
    val private size: int
    val private leafCount: int

    new (count: int, _op, _init, _lazyInit, _updateOp, _propOp) =
        let lc = LazySegmentTree<'T>.LeastSquare count in
        {
            op = _op
            updateOp = _updateOp
            propOp = _propOp
            init = _init
            lazyInit = _lazyInit
            leafCount = lc
            size = count
            node = Array.create (2 * lc) _init
            lazyNodes = LazyNode<'T>.CreateNew _lazyInit |> Array.create (2 * lc)
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

    member this.Init (nums: 'T[]) =
        let rec init k st en = 
            if st = en then this.node[k] <- nums[st - 1]
            else
                let mid = (st + en) >>> 1
                (LazySegmentTree<'T>.LeftChild k, st, mid) |||> init
                (LazySegmentTree<'T>.RightChild k, mid + 1, en) |||> init
                this.node[k] <- (this.node[LazySegmentTree<'T>.LeftChild k], this.node[LazySegmentTree<'T>.RightChild k]) ||> this.op

        init 1 1 this.size

    member this.Query left right =
        let rec query k st en =
            this.Propagate k st en
            if left > en || right < st then this.init
            else if left <= st && en <= right then this.node[k]
            else
                let mid = (st + en) >>> 1
                ((LazySegmentTree<'T>.LeftChild k, st, mid) |||> query, (LazySegmentTree<'T>.RightChild k, mid + 1, en) |||> query) ||> this.op

        query 1 1 this.size

    member this.Update left right mul add =
        let rec update k st en =
            this.Propagate k st en
            if left > en || right < st then ()
            else if left <= st && en <= right then
                this.lazyNodes[k] <- { mul = mul; add = add }
                this.Propagate k st en
            else
                let mid = (st + en) >>> 1
                (LazySegmentTree<'T>.LeftChild k, st, mid) |||> update
                (LazySegmentTree<'T>.RightChild k, mid + 1, en) |||> update
                this.node[k] <- (this.node[LazySegmentTree<'T>.LeftChild k], this.node[LazySegmentTree<'T>.RightChild k]) ||> this.op

        update 1 1 this.size

    member private this.Propagate index st en =
        let cur = this.lazyNodes[index]
        if (cur.mul = fst this.lazyInit && cur.add = snd this.lazyInit) |> not then
            this.node[index] <- (this.node[index], (cur.mul, cur.add)) ||> this.updateOp (en - st + 1)

            if st <> en then
                let left = this.lazyNodes[LazySegmentTree<'T>.LeftChild index]
                let right = this.lazyNodes[LazySegmentTree<'T>.RightChild index]

                this.lazyNodes[LazySegmentTree<'T>.LeftChild index] <- ((cur.mul, cur.add), (left.mul, left.add)) ||> this.propOp |> function | (a, b) -> { mul = a; add = b }
                this.lazyNodes[LazySegmentTree<'T>.RightChild index] <- ((cur.mul, cur.add), (right.mul, right.add)) ||> this.propOp |> function | (a, b) -> { mul = a; add = b }

            this.lazyNodes[index] <- { mul = fst this.lazyInit; add = snd this.lazyInit }

and LazyNode<'T when 'T: equality> = {
    mutable mul: 'T
    mutable add: 'T } with

    static member CreateNew _init = { mul = fst _init; add = snd _init }
    

[<EntryPoint>]
let main _ = 
    let parseInts f (str: string) = str.Trim().Split() |> Array.map f
    let update k n (m, a) = (Mul n m, int64 k |> Mul a) ||> Add
    let prop (m1, a1) (m2, a2) = (Mul m1 m2, Add (Mul m1 a2) a1)

    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let n = stream.ReadLine().Trim() |> int
    let segtree = LazySegmentTree<int64>(n, Add, 0L, (1L, 0L), update, prop)
    stream.ReadLine() |> parseInts int64 |> segtree.Init

    let m = stream.ReadLine().Trim() |> int
    let rec solve times =
        if times > 0 then
            let query = stream.ReadLine() |> parseInts int
            match int query[0] with
            | 1 -> (1L, int64 query[3]) ||> segtree.Update query[1] query[2]
            | 2 -> (int64 query[3], 0L) ||> segtree.Update query[1] query[2]
            | 3 -> (0L, int64 query[3]) ||> segtree.Update query[1] query[2]
            | _ -> result.AppendFormat("{0}\n", segtree.Query query[1] query[2]) |> ignore

            times - 1 |> solve

    solve m
    printfn "%A" result
    0
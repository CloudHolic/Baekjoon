open System
open System.IO

// Implement priority queue
type IPriorityQueue<'T when 'T : comparison> =
    abstract member Peek : unit -> 'T
    abstract member TryPeek : unit -> 'T option

    abstract member Insert : 'T -> IPriorityQueue<'T>
    
    abstract member Pop : unit -> 'T * IPriorityQueue<'T>
    abstract member TryPop : unit -> ('T * IPriorityQueue<'T>) option
        
    abstract member IsEmpty : bool with get
        
    abstract member Length : int
    
type Heap<'T when 'T : comparison> (isDescending: bool, length: int, data: HeapData<'T>) =
    member internal _.heapData = data
    member internal _.heapLength = length

    member _.Head = 
        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, _) -> x

    member _.TryHead =
        match data with
        | E -> None
        | T(x, _) -> Some x

    member _.Insert x = Heap.merge isDescending (length + 1) (T(x, [])) data

    member _.Tail() = 
        let mergeData (h1: HeapData<'T>) (h2: HeapData<'T>) : HeapData<'T> =
            match h1, h2 with
            | E, h -> h
            | h, E -> h
            | T(x, xs), T(y, ys) ->
                if isDescending then
                    if x <= y then T(y, h1::ys) else T(x, h2::xs)
                else
                    if x <= y then T(x, h2::xs) else T(y, h1::ys)

        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(_, xs) ->
            let combine state item =
                match state with
                | Some p, l -> (None, (mergeData item p)::l)
                | None, l -> (Some item, l)

            let tail =
                xs
                |> List.fold combine (None, [])
                |> function
                    | Some i, l -> i::l
                    | None, l -> l
                |> List.fold mergeData E
            Heap(isDescending, (length - 1), tail)

    member this.Pop() =
        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, _) -> x, this.Tail()

    member this.TryPop() =
        match data with
        | E -> None
        | T(x, _) -> Some(x, this.Tail())

    member _.IsEmpty =
        match data with
        | E -> true
        | _ -> false
            
    member _.Length = length

    member _.IsDescending = isDescending

    static member private merge isDescending newLength (h1 : HeapData<'T>) (h2 : HeapData<'T>) : Heap<'T> =
        match h1, h2 with
        | E, h -> Heap(isDescending, newLength, h)
        | h, E -> Heap(isDescending, newLength, h)
        | T(x, xs), T(y, ys) ->
            if isDescending then
                if x <= y then Heap(isDescending, newLength, T(y, h1::ys)) else Heap(isDescending, newLength, T(x, h2::xs))
            else
                if x <= y then Heap(isDescending, newLength, T(x, h2::xs)) else Heap(isDescending, newLength, T(y, h1::ys))

    interface IPriorityQueue<'T> with        
        member this.Peek() = this.Head
        member this.TryPeek() = this.TryHead

        member this.Insert element = this.Insert element :> IPriorityQueue<'T>

        member this.Pop() = 
            let element, newHeap = this.Pop()
            element, (newHeap :> IPriorityQueue<'T>)
        member this.TryPop() =
            match this.TryPop() with
            | Some(element, newHeap) -> Some(element, newHeap :> IPriorityQueue<'T>)
            | None -> None

        member this.IsEmpty = this.IsEmpty
        member this.Length = this.Length

and HeapData<'T when 'T : comparison> =
    | E
    | T of 'T * list<HeapData<'T>>

module PriorityQueue =
    let empty<'T when 'T : comparison> isDescending = Heap<'T>(isDescending, 0, E) :> IPriorityQueue<'T>

    let inline peek (pq: IPriorityQueue<'T>) = pq.Peek()

    let inline tryPeek (pq: IPriorityQueue<'T>) = pq.TryPeek()

    let inline push (pq: IPriorityQueue<'T>) x = pq.Insert x

    let inline pop (pq: IPriorityQueue<'T>) = pq.Pop()

    let inline tryPop (pq: IPriorityQueue<'T>) = pq.TryPop()

    let inline isEmpty (pq: IPriorityQueue<'T>) = pq.IsEmpty

    let inline length (pq: IPriorityQueue<'T>) = pq.Length

// An HArc record.
[<CustomComparison; CustomEquality>]
type HArc = {
    Id: int
    U: int
    V: int
    Low: int
    Base: int64
    Mul: int64
    Num: int64
    Density: int64 } with

    member this.contains other = this.U <= other.U && this.V <= other.V
    member this.Support = this.Num / this.Density

    override this.GetHashCode () = hash this.Support

    interface IComparable<HArc> with
        member this.CompareTo other =
            compare this.Support other.Support

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | null -> 1
            | :? HArc as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not an HArc"

    interface IEquatable<HArc> with
        member this.Equals other =
            this.Support = other.Support

    override this.Equals obj =
        match obj with
        | :? HArc as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    
    let mutable last = 0L
    let size = stream.ReadLine() |> int |> function | s -> s + 1
    let matrix = Array.init (size + 2) (fun i ->
        if i = 0 || i > size then 0L
        else
            stream.ReadLine().Split()
            |> Array.map int64
            |> function
                | arr -> 
                    if i = size - 1 then last <- arr.[1]
                    arr.[0])
    matrix.[size] <- last

    let solve =
        let cp: int64 array = Array.zeroCreate (size + 2)
        let prepare =
            let minIdx = matrix |> Seq.indexed |> Seq.minBy (fun x -> if snd x = 0L then Int64.MaxValue else snd x) |> fst

            matrix.[size + 1] <- matrix.[1]
            for i in 1 .. size + 1 do
                cp.[i] <- matrix.[i] * matrix.[i - 1] + cp.[i - 1]

        let sweep =
            0

        let result =
            0

        0

    match size with
    | s when s < 2 -> printfn "0"
    | 2 -> matrix.[1] * matrix.[2] |> printfn "%d"
    | _ -> solve |> printfn "%d"

    0
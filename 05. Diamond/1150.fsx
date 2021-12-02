open System
open System.IO

// Implement a priority queue
type IPriorityQueue<'T when 'T : comparison> =
    abstract member Peek : unit -> 'T

    abstract member Insert : 'T -> IPriorityQueue<'T>
    
    abstract member Pop : unit -> 'T * IPriorityQueue<'T>
            
    abstract member IsEmpty : bool with get
        
    abstract member Length : int
    
type Heap<'T when 'T : comparison> (isDescending: bool, length: int, data: HeapData<'T>) =
    member internal _.heapData = data
    member internal _.heapLength = length

    member _.Head = 
        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, _) -> x

    member _.Insert x = Heap.merge isDescending (length + 1) (T(x, [])) data

    member _.Tail() = 
        let mergeData (h1: HeapData<'T>) (h2: HeapData<'T>) : HeapData<'T> =
            match h1, h2 with
            | E, h -> h
            | h, E -> h
            | T(x, xs), T(y, ys) ->
                if isDescending then
                    if x <= y then T(y, h1 :: ys) else T(x, h2 :: xs)
                else
                    if x <= y then T(x, h2 :: xs) else T(y, h1 :: ys)

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
                    | Some i, l -> i :: l
                    | None, l -> l
                |> List.fold mergeData E
            Heap(isDescending, (length - 1), tail)

    member this.Pop() =
        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, _) -> x, this.Tail()

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
                if x <= y then Heap(isDescending, newLength, T(y, h1 :: ys)) else Heap(isDescending, newLength, T(x, h2 :: xs))
            else
                if x <= y then Heap(isDescending, newLength, T(x, h2 :: xs)) else Heap(isDescending, newLength, T(y, h1 :: ys))

    interface IPriorityQueue<'T> with
        member this.Peek() = this.Head

        member this.Insert element = this.Insert element :> IPriorityQueue<'T>

        member this.Pop() = 
            let element, newHeap = this.Pop()
            element, (newHeap :> IPriorityQueue<'T>)

        member this.IsEmpty = this.IsEmpty
        member this.Length = this.Length

and HeapData<'T when 'T : comparison> =
    | E
    | T of 'T * list<HeapData<'T>>

module PriorityQueue =
    let empty<'T when 'T : comparison> isDescending = Heap<'T>(isDescending, 0, E) :> IPriorityQueue<'T>

    let inline peek (pq: IPriorityQueue<'T>) = pq.Peek()

    let inline push (pq: IPriorityQueue<'T>) x = pq.Insert x

    let inline pop (pq: IPriorityQueue<'T>) = pq.Pop()

    let inline isEmpty (pq: IPriorityQueue<'T>) = pq.IsEmpty

    let inline length (pq: IPriorityQueue<'T>) = pq.Length

[<CustomComparison; CustomEquality>]
type Point = {
    mutable Length: int;
    mutable Left: int;
    mutable Right: int } with

    override this.GetHashCode() = hash this.Length

    interface IComparable<Point> with
        member this.CompareTo other =
            compare this.Length other.Length

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | null -> 1
            | :? Point as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not a Score"

    interface IEquatable<Point> with
        member this.Equals other =
            this.Length = other.Length

    override this.Equals obj =
        match obj with
        | :? Point as other -> (this :> IEquatable<_>).Equals other
        |_ -> false

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let mutable result = 0

    let n, k = stream.ReadLine().Trim().Split() |> Array.map int |> function | nums -> nums.[0], nums.[1]
    let mutable pq = PriorityQueue.empty<Point> false
    let points = Array.create (n + 2) { Length = 0; Left = 0; Right = 0}
        
    let mutable prev = stream.ReadLine().Trim() |> int
    for i = 1 to n - 1 do
        let temp = stream.ReadLine().Trim() |> int
        points.[i] <- { Length = temp - prev; Left = i - 1; Right = i + 1 }
        pq <- PriorityQueue.push pq { Length = temp - prev; Left = i; Right = i + 1 }
        prev <- temp
    points.[n] <- { Length = 0; Left = n - 1; Right = n + 1 }

    let mutable i = 0
    while i < k do
        PriorityQueue.pop pq
        |> function
            | p, q ->
                pq <- q
                if p.Left >= 1 && p.Right <= n && p.Right = points.[p.Left].Right && p.Left = points.[p.Right].Left then
                    result <- result + p.Length
                    i <- i + 1
                    if i < k then
                        let newLeft = points.[p.Left].Left
                        let newRight = points.[p.Right].Right
                        let newLength = points.[newLeft].Length + points.[p.Right].Length - p.Length

                        pq <- PriorityQueue.push pq { Length = newLength; Left = newLeft; Right = newRight }

                        points.[newLeft].Length <- newLength
                        points.[newLeft].Right <- newRight
                        points.[newRight].Left <- newLeft
        
    printfn "%d" result
    0
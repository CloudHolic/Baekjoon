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
type Partial = {
    t: int
    mutable cur: int
    Photo: int
    mutable Deadline: int
    SumStarts: int64
    mutable AfterPhoto: bool } with

    override this.GetHashCode() = hash this.t

    interface IComparable<Partial> with
        member this.CompareTo other =
            if this.t <> other.t then compare other.t this.t
            else if this.Photo <> other.Photo then compare this.Photo other.Photo
            else compare this.SumStarts other.SumStarts

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | null -> 1
            | :? Partial as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not a Partial"

    interface IEquatable<Partial> with
        member this.Equals other =
            this.t = other.t && this.Photo = other.Photo && this.SumStarts = other.SumStarts

    override this.Equals obj =
        match obj with
        | :? Partial as other -> (this :> IEquatable<_>).Equals other
        |_ -> false

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())    
    let ParseTwoInts (str: string) = str.Trim().Split() |> Array.map int |> function | n -> n.[0], n.[1]

    let n, t = stream.ReadLine() |> ParseTwoInts

    let photos: (int * int) array = Array.zeroCreate n
    for i = 0 to n - 1 do
        photos.[i] <- stream.ReadLine() |> ParseTwoInts

    let photos = Array.sort photos
    let mutable result = false
    let mutable pq = PriorityQueue.empty<Partial> true
    let deadlines = ResizeArray<IPriorityQueue<int>>([PriorityQueue.empty<int> true])
    pq <- PriorityQueue.push pq { t = 0; cur = 0; Photo = 0; Deadline = 0; SumStarts = 0L; AfterPhoto = false }

    let mutable flag = true
    while flag && (not <| PriorityQueue.isEmpty pq) do
        let p = PriorityQueue.peek pq
        while not <| PriorityQueue.isEmpty pq && (PriorityQueue.peek pq).t = p.t do
            pq <- snd <| PriorityQueue.pop pq

        if p.Photo = n then
            result <- true
            flag <- false
        else
            if p.AfterPhoto then
                deadlines.[p.Deadline] <- PriorityQueue.pop deadlines.[p.Deadline] |> snd
            else
                deadlines.Add(deadlines.[p.Deadline])
                p.Deadline <- deadlines.Count - 1

            while p.cur < Array.length photos && fst photos.[p.cur] <= p.t do
                deadlines.[p.Deadline] <- PriorityQueue.push deadlines.[p.Deadline] (snd photos.[p.cur] * -1)
                p.cur <- p.cur + 1

            if PriorityQueue.length deadlines.[p.Deadline] > 0 && p.t + t + PriorityQueue.peek deadlines.[p.Deadline] > 0 then
                ()
            else
                if p.cur < n && (PriorityQueue.length deadlines.[p.Deadline] = 0 || fst photos.[p.cur] < p.t + t) then
                    pq <- PriorityQueue.push pq { t = fst photos.[p.cur]; cur = p.cur; Photo = p.Photo; Deadline = p.Deadline; SumStarts = p.SumStarts; AfterPhoto = false }
                if PriorityQueue.length deadlines.[p.Deadline] > 0 then
                    pq <- PriorityQueue.push pq { t = p.t + t; cur = p.cur; Photo = p.Photo + 1; Deadline = p.Deadline; SumStarts = p.SumStarts + int64 p.t; AfterPhoto = true }

    match result with
    | true -> "yes"
    | false -> "no"
    |> printfn "%s"
    0
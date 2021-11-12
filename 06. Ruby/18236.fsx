open System
open System.IO

// Implement a custom stack that can iterate & see the second element
type Stack<'T> (internalList: list<'T>)=
    member internal _.internalList = internalList

    member _.Head =
        match internalList with
        | head :: _ -> head
        | _ -> raise (new System.Exception("Stack is empty"))

    member _.Second =
        match internalList with
        | _ :: second :: _ -> second
        | _ -> raise (new System.Exception("Stack's size < 2"))

    member _.Push x = Stack(x :: internalList)

    member _.Pop =
        match internalList with
        | head :: tail -> head, Stack(tail)
        | _ -> raise (new System.Exception("Stack is empty"))
    
    member _.IsEmpty = internalList.IsEmpty

    member _.Length = internalList.Length

    interface System.Collections.Generic.IEnumerable<'T> with
        override _.GetEnumerator() : System.Collections.Generic.IEnumerator<'T> =
            (internalList :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator()

    interface System.Collections.IEnumerable with
        override this.GetEnumerator() =
            (this :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator()
            :> System.Collections.IEnumerator

module Stack =
    let empty<'T> : Stack<'T> = Stack<_>([])

    let inline head (s : Stack<'T>) = s.Head

    let inline second (s : Stack<'T>) = s.Second

    let inline push (s : Stack<'T>) (x : 'T) = s.Push x

    let inline pop (s : Stack<'T>) = s.Pop
    
    let inline isEmpty (s : Stack<'T>) = s.IsEmpty
    
    let inline length (s : Stack<'T>) = s.Length

// Implement a custom priority queue that can merge with other priority queue
type IPriorityQueue<'T when 'T : comparison> =
    abstract member Peek : unit -> 'T

    abstract member Insert : 'T -> IPriorityQueue<'T>
    
    abstract member Pop : unit -> 'T * IPriorityQueue<'T>

    abstract member Merge : IPriorityQueue<'T> -> IPriorityQueue<'T>
        
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

    member _.Merge (other: Heap<'T>) =
        if isDescending = other.IsDescending then Heap.merge isDescending (length + other.Length) data other.heapData
        else failwith "These two heaps have different sort orders"

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

        member this.Merge other =
            this.Merge (other :?> Heap<'T>) :> IPriorityQueue<'T>

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

    let inline Merge (pq1: IPriorityQueue<'T>) (pq2: IPriorityQueue<'T>) = pq1.Merge pq2

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
    mutable Num: int64
    mutable Density: int64 } with

    member this.contains other = this.U <= other.U && other.V <= this.V
    member this.Support = this.Num / this.Density

    static member Default = { Id = 0; U = 0; V = 0; Low = 0; Base = 0L; Mul = 0L; Num = 0L; Density = 0L }

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
        if i = 0 || i >= size then 0L
        else
            stream.ReadLine().Split()
            |> Array.map int64
            |> function
                | arr -> 
                    if i = size - 1 then last <- arr.[1]
                    arr.[0])
    matrix.[size] <- last

    let solve =
        let rotate (arr: 'T array) i startIdx endIdx =
            let diff = i - startIdx
            let tempArr = Array.copy arr.[startIdx .. i - 1]
            arr.[startIdx .. endIdx - diff] <- arr.[i .. endIdx]
            arr.[endIdx - diff + 1 .. endIdx] <- tempArr
        
        // Prepare
        let minIdx = matrix |> Seq.indexed |> Seq.minBy (fun x -> if snd x = 0L then Int64.MaxValue else snd x) |> fst
        rotate matrix minIdx 1 size
        matrix.[size + 1] <- matrix.[1]
        
        let cp: int64 array = Array.zeroCreate (size + 2)
        for i in 1 .. size + 1 do
            cp.[i] <- matrix.[i] * matrix.[i - 1] + cp.[i - 1]

        let mutable list = []
        
        // Sweep
        let mutable stack = Stack.empty<int>
        let mutable temp = []

        for i in 1 .. size do
            while Stack.length stack >= 2 && matrix.[Stack.head stack] > matrix.[i] do
                temp <- (Stack.second stack, i) :: temp
                stack <- snd <| Stack.pop stack
            stack <- Stack.push stack i

        while Stack.length stack >= 4 do
            temp <- (1, Stack.second stack) :: temp
            stack <- snd <| Stack.pop stack

        temp
        |> List.iter (fun x ->
            if fst x = 1 || snd x = 1 then ()
            else list <- x :: list)

        // Build HArc tree
        let listSize = List.length list
        let hArcs = Array.zeroCreate (listSize + 2)
        let mutable numHArc = 0
        let newHArc u v =
            // Assume that u <= v
            numHArc <- numHArc + 1
            let mul = matrix.[u] * matrix.[v]
            hArcs.[numHArc] <- { 
                Id = numHArc;
                U = u; V = v;
                Low = if matrix.[u] < matrix.[v] then u else v;
                Mul = mul;
                Base = cp.[v] - cp.[u] - mul;
                Num = 0L; Density = 0L }

        let childs = Array.init (List.length list + 2) (fun _ -> Stack.empty<int>)
        let mutable stack = Stack.empty<int>
        hArcs.[0] <- HArc.Default
        newHArc <|| (1, size + 1) // root
        list
        |> List.iter (fun x ->
            newHArc <|| (fst x, snd x)
            while not <| Stack.isEmpty stack && hArcs.[numHArc].contains hArcs.[Stack.head stack] do
                Stack.pop stack
                |> function
                    | v, s ->
                        childs.[numHArc] <- Stack.push childs.[numHArc] v
                        stack <- s
            stack <- Stack.push stack numHArc)

        while not <| Stack.isEmpty stack do
            Stack.pop stack
            |> function
                | v, s -> 
                    childs.[1] <- Stack.push childs.[1] v
                    stack <- s

        // Find an answer
        let mutable pqs = 0
        let sub = Array.zeroCreate (listSize + 2)
        let qid = Array.zeroCreate (listSize + 2)
        let pq = Array.init (listSize + 2) (fun _ -> PriorityQueue.empty<HArc> true)
        let con = Array.init (size + 2) (fun _ -> Stack.empty<HArc>)

        let Multiply node =
            if node = 1 then
                matrix.[1] * matrix.[2] + matrix.[1] * matrix.[size]
            elif hArcs.[node].U = hArcs.[node].Low then
                if Stack.isEmpty con.[hArcs.[node].U] || not <| hArcs.[node].contains (Stack.head con.[hArcs.[node].U])
                then matrix.[hArcs.[node].U] * matrix.[hArcs.[node].U + 1]
                else (Stack.head con.[hArcs.[node].U]).Mul
            else
                if Stack.isEmpty con.[hArcs.[node].V] || not <| hArcs.[node].contains (Stack.head con.[hArcs.[node].V])
                then matrix.[hArcs.[node].V] * matrix.[hArcs.[node].V - 1]
                else (Stack.head con.[hArcs.[node].V]).Mul

        let addArc node harc =
            pq.[qid.[node]] <- PriorityQueue.push pq.[qid.[node]] harc
            con.[harc.U] <- Stack.push con.[harc.U] harc
            con.[harc.V] <- Stack.push con.[harc.V] harc

        let removeArc node =
            PriorityQueue.pop pq.[qid.[node]]
            |> function
                | v, q ->
                    con.[v.U] <- snd <| Stack.pop con.[v.U]
                    con.[v.V] <- snd <| Stack.pop con.[v.V]
                    pq.[qid.[node]] <- q

        let mergePq node =
            let mutable maxChild = -1

            childs.[node]
            |> Seq.iter (fun x -> if maxChild = -1 || sub.[maxChild] < sub.[x] then maxChild <- x)

            qid.[node] <- qid.[maxChild]

            childs.[node]
            |> Seq.iter (fun x -> if x <> maxChild then pq.[qid.[node]] <- PriorityQueue.Merge pq.[qid.[node]] pq.[qid.[x]])

        let rec dfs node =
            sub.[node] <- 1
            if Stack.isEmpty childs.[node]
            then
                pqs <- pqs + 1
                qid.[node] <- pqs
                hArcs.[node].Density <- hArcs.[node].Base
                hArcs.[node].Num <- matrix.[hArcs.[node].Low] * (hArcs.[node].Density + hArcs.[node].Mul - Multiply node)
                addArc node hArcs.[node]
            else
                hArcs.[node].Density <- hArcs.[node].Base
                childs.[node]
                |> Seq.iter (fun x -> 
                    dfs x
                    sub.[node] <- sub.[node] + sub.[x]
                    hArcs.[node].Density <- hArcs.[node].Density - hArcs.[x].Base)

                hArcs.[node].Num <- matrix.[hArcs.[node].Low] * (hArcs.[node].Density + hArcs.[node].Mul - Multiply node)
                mergePq node

                while not <| PriorityQueue.isEmpty pq.[qid.[node]] && (PriorityQueue.peek pq.[qid.[node]]).Support >= matrix.[hArcs.[node].Low] do
                    let top = PriorityQueue.peek pq.[qid.[node]]
                    hArcs.[node].Density <- hArcs.[node].Density + top.Density
                    removeArc node
                    hArcs.[node].Num <- matrix.[hArcs.[node].Low] * (hArcs.[node].Density + hArcs.[node].Mul - Multiply node)

                while not <| PriorityQueue.isEmpty pq.[qid.[node]] && hArcs.[node] <= PriorityQueue.peek pq.[qid.[node]] do
                    let top = PriorityQueue.peek pq.[qid.[node]]
                    hArcs.[node].Density <- hArcs.[node].Density + top.Density
                    removeArc node
                    hArcs.[node].Num <- hArcs.[node].Num + top.Num

                addArc node hArcs.[node]

        let mutable answer = 0L
        dfs 1
        while not <| PriorityQueue.isEmpty pq.[qid.[1]] do
            PriorityQueue.pop pq.[qid.[1]]
            |> function
                | v, q ->
                    answer <- answer + v.Num
                    pq.[qid.[1]] <- q

        answer

    match size with
    | s when s < 2 -> printfn "0"
    | 2 -> matrix.[1] * matrix.[2] |> printfn "%d"
    | _ -> solve |> printfn "%d"

    0
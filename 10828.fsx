open System
open System.IO
open System.Text

type Stack<'T> (internalList: list<'T>)=
    let mutable hashCode = None
    member internal this.internalList = internalList

    member _.Head =
        match internalList with
        | head :: _ -> head
        | _ -> raise (new System.Exception("Stack is empty"))

    member _.TryHead =
        match internalList with
        | head :: _ -> Some(head)
        | _ -> None

    member _.Push x = Stack(x :: internalList)

    member _.Pop =
        match internalList with
        | head :: tail -> head, Stack(tail)
        | _ -> raise (new System.Exception("Stack is empty"))

    member _.TryPop =
        match internalList with
        | head :: tail -> Some(head, Stack(tail))
        | _ -> None
    
    member _.IsEmpty = internalList.IsEmpty

    member _.Length = internalList.Length

    member _.Reverse() = Stack(List.rev internalList)

    override this.GetHashCode() =
        match hashCode with
        | None ->
            let mutable hash = 1
            for x in this do
                hash <- 31 * hash + Unchecked.hash x
            hashCode <- Some hash
            hash
        | Some hash -> hash

    override this.Equals(other) =
        match other with
        | :? Stack<'T> as y ->
            (this :> System.IEquatable<Stack<'T>>).Equals y
        | _ -> false

    interface System.IEquatable<Stack<'T>> with
        member this.Equals (y: Stack<'T>) =
            if this.Length <> y.Length then false
            else
                if this.GetHashCode() <> y.GetHashCode() then false
                else Seq.forall2 Unchecked.equals this y

    interface System.Collections.Generic.IEnumerable<'T> with
        member _.GetEnumerator() : System.Collections.Generic.IEnumerator<'T> =
            let e = Seq.ofList internalList
            e.GetEnumerator()

    interface System.Collections.IEnumerable with
        override this.GetEnumerator() =
            (this :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator()
            :> System.Collections.IEnumerator

    interface System.Collections.Generic.IReadOnlyCollection<'T> with
        member this.Count = this.Length

[<RequireQualifiedAccess>]
module Stack =
    let empty<'T> : Stack<'T> = Stack<_>([])

    let inline head (s : Stack<'T>) = s.Head

    let inline tryHead (s : Stack<'T>) = s.TryHead

    let inline push (x : 'T) (s : Stack<'T>) = s.Push x

    let inline pop (s : Stack<'T>) = s.Pop

    let inline tryPop (s : Stack<'T>) = s.TryPop

    let (|Cons|Nil|) (s : Stack<'T>) = match s.TryPop with Some(a, b) -> Cons(a, b) | None -> Nil
    
    let inline isEmpty (s : Stack<'T>) = s.IsEmpty
    
    let inline length (s : Stack<'T>) = s.Length

    let inline rev (s : Stack<'T>) = s.Reverse()

    let ofList xs = Stack<'T>(xs)

    let ofSeq xs = Stack<'T>(Seq.toList xs)

    let inline toSeq (s : Stack<'T>) = s :> seq<'T>

    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (s : Stack<'T>) =
        List.fold f state s.internalList

    let foldBack (f : ('T -> 'State -> 'State)) (s : Stack<'T>) (state : 'State) =
        List.foldBack f s.internalList state

[<EntryPoint>]
let main _ =
    let mutable stack = Stack.empty<int>
    let result = new StringBuilder()

    let solve (str : string) =
        match str with
        | p when p.StartsWith "push" ->
            stack <- stack.Push <| (int <| p.Split().[1])
        | "pop" ->
            let (e, s) = stack.tryPop
            stack <- s
            match e with
            | Some(x) -> result.Append(x |> string) |> ignore
            | None -> result.Append("-1") |> ignore

        | "size" -> result.Append(stack.Length |> string) |> ignore
        | "empty" -> result.Append(stack.IsEmpty |> int |> string) |> ignore
        | "top"
        | _

    use stream = new StreamReader(Console.OpenStandardInput())
    let count = stream.ReadLine().Trim() |> int
    0
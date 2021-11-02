open System
open System.IO
open System.Text

type Queue<'T> (frontList: 'T list, rBackList: 'T list) =
    member internal _.frontList = frontList
    member internal _.rBackList = rBackList

    member _.Peek =
        match frontList with
        | head :: _ -> head
        | _ -> raise(new System.Exception("Queue is empty"))

    member _.TryPeek =
        match frontList with
        | head :: _ -> Some head
        | _ -> None

    member _.Enqueue x =
        match frontList, x :: rBackList with
        | [], r -> Queue((List.rev r), [])
        | f, r -> Queue(f, r)

    member _.Dequeue =
        match frontList with
        | head :: tail -> 
            match tail, rBackList with
            | [], r -> head, Queue((List.rev r), [])
            | f, r -> head, Queue(f, r)
        | _ -> raise (new System.Exception("Queue is empty"))

    member _.TryDequeue =
        match frontList with
        | head :: tail ->
            match tail, rBackList with
            | [], r -> Some (head, Queue((List.rev r), []))
            | f, r -> Some (head, Queue(f, r))
        | _ -> None

    member _.IsEmpty = frontList.IsEmpty

    member _.Length = frontList.Length + rBackList.Length

[<RequireQualifiedAccess>]
module Queue =
    let empty<'T> : 'T Queue = Queue<_>([], [])

    let inline peek (q: 'T Queue) = q.Peek

    let inline tryPeek (q: 'T Queue) = q.TryPeek

    let inline enqueue (q: 'T Queue) (x: 'T) = q.Enqueue x

    let inline dequeue (q: 'T Queue) = q.Dequeue

    let inline tryDequeue (q: 'T Queue) = q.TryDequeue

    let inline isEmpty (q: 'T Queue) = q.IsEmpty

    let inline length (q: 'T Queue) = q.Length

[<EntryPoint>]
let main _ =
    let mutable queue = Queue.empty<int>
    let mutable lastEnqueue = -1
    let result = new StringBuilder()
    use stream = new StreamReader(Console.OpenStandardInput())

    let solve (str : string) =
        match str with
        | p when p.StartsWith "push" ->
            let x = int <| p.Split().[1]
            lastEnqueue <- x
            queue <- Queue.enqueue queue x
        | "pop" ->
            Queue.tryDequeue queue
            |> function
                | Some(e, s) ->
                    queue <- s
                    result.Append(e |> string).Append("\n") |> ignore
                | None ->
                    result.Append("-1\n") |> ignore
        | "size" -> result.Append(Queue.length queue |> string).Append("\n") |> ignore
        | "empty" -> result.Append(Queue.isEmpty queue |> Convert.ToInt32 |> string).Append("\n") |> ignore
        | "front" ->
            Queue.tryPeek queue
            |> function
                | Some(x) -> result.Append(x |> string).Append("\n") |> ignore
                | None -> result.Append("-1\n") |> ignore
        | "back" ->
            Queue.length queue            
            |> function
                | 0 -> result.Append("-1\n") |> ignore
                | _ -> result.Append(lastEnqueue |> string).Append("\n") |> ignore
        | _ -> failwith("Invalid operation")

    let rec repeat n =
        match n with
        | 0 -> printfn "%s" <| result.ToString()
        | _ ->
            solve <| stream.ReadLine().Trim()
            repeat (n - 1)

    stream.ReadLine().Trim() |> int |> repeat
    0
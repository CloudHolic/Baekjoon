open System
open System.IO
open System.Text

type Stack<'T> (internalList: list<'T>)=
    member internal _.internalList = internalList

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

module Stack =
    let empty<'T> : Stack<'T> = Stack<_>([])

    let inline head (s : Stack<'T>) = s.Head

    let inline tryHead (s : Stack<'T>) = s.TryHead

    let inline push (s : Stack<'T>) (x : 'T) = s.Push x

    let inline pop (s : Stack<'T>) = s.Pop

    let inline tryPop (s : Stack<'T>) = s.TryPop
    
    let inline isEmpty (s : Stack<'T>) = s.IsEmpty
    
    let inline length (s : Stack<'T>) = s.Length

[<EntryPoint>]
let main _ =
    let mutable stack = Stack.empty<int>
    let result = new StringBuilder()
    use stream = new StreamReader(Console.OpenStandardInput())

    let solve (str : string) =
        match str with
        | p when p.StartsWith "push" ->
            stack <- Stack.push stack <| (int <| p.Split().[1])
        | "pop" ->
            Stack.tryPop stack
            |> function
                | Some(e, s) ->
                    stack <- s
                    result.Append(e |> string).Append("\n") |> ignore
                | None ->
                    result.Append("-1\n") |> ignore
        | "size" -> result.Append(Stack.length stack |> string).Append("\n") |> ignore
        | "empty" -> result.Append(Stack.isEmpty stack |> Convert.ToInt32 |> string).Append("\n") |> ignore
        | "top" ->
            Stack.tryHead stack
            |> function
                | Some(x) -> result.Append(x |> string).Append("\n") |> ignore
                | None -> result.Append("-1\n") |> ignore
        | _ -> failwith("Invalid operation")

    let rec repeat n =
        match n with
        | 0 -> printfn "%s" <| result.ToString()
        | _ ->
            solve <| stream.ReadLine().Trim()
            repeat (n - 1)

    stream.ReadLine().Trim() |> int |> repeat
    0
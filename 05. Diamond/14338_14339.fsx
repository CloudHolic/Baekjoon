open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ = 
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let t = stream.ReadLine().Trim() |> int

    let rec solve case =
        if case <= t then
            let a, b = stream.ReadLine().Trim().Split() |> Array.map int64 |> function | nums -> nums[0], nums[1]
            let regex = stream.ReadLine().Trim()

            let nfa = Array.create (regex.Length + 1) Set.empty<int>
            let mutable lookup = Set.empty<int>
            let rec buildNFA idx cur =
                if not <| Set.contains cur lookup then
                    lookup <- Set.add cur lookup

                    if cur = regex.Length then nfa[idx] <- Set.add cur nfa[idx]
                    else
                        match regex[cur] with
                        | c when Char.IsDigit c ->
                            nfa[idx] <- Set.add cur nfa[idx]

                        | ')' -> cur + 1 |> buildNFA idx

                        | '|' ->
                            let mutable count, next, i, flag = 0, 0, cur + 1, true
                            while flag do
                                match regex[i] with
                                | '(' -> count <- count + 1
                                | ')' -> count <- count - 1
                                | _ -> ()
                                if count = -1 then
                                    next <- i
                                    flag <- false
                                if i = regex.Length - 1 then
                                    flag <- false
                                i <- i + 1

                            next + 1 |> buildNFA idx

                        | '*' ->
                            let mutable count, next, i, flag = 0, 0, cur - 2, true
                            while flag do
                                match regex[i] with
                                | '(' -> count <- count + 1
                                | ')' -> count <- count - 1
                                | _ -> ()
                                if count = 1 then
                                    next <- i
                                    flag <- false
                                if i = 0 then
                                    flag <- false
                                i <- i - 1
                                                        
                            cur + 1 |> buildNFA idx
                            next + 1 |> buildNFA idx

                        | _ -> // c = '('
                            cur + 1 |> buildNFA idx

                            let mutable count, next, i, flag = 0, 0, cur, true
                            while flag do
                                match regex[i] with
                                | '(' -> count <- count + 1
                                | ')' -> count <- count - 1
                                | _ -> ()

                                if count = 1 && regex[i] = '|' then
                                    i + 1 |> buildNFA idx
                                if count = 0 then
                                    next <- i
                                    flag <- false
                                if i = regex.Length - 1 then
                                    flag <- false
                                i <- i + 1

                            if next <> regex.Length - 1 && regex[next + 1] = '*' then
                                next + 2 |> buildNFA idx

            let matchNFA value =
                let digits = value |> string |> Seq.toList |> List.map (fun x -> int x - int '0')
                let initState, lastState = 0, regex.Length
                let mutable curState = Map [((nfa[initState], true, true), 1L)]

                for i = 0 to digits.Length - 1 do
                    let mutable newState = Map [((nfa[initState], true, false), 1L)]
                    curState |> Map.iter (fun (state, empty, prefix) count -> 
                        for j = 0 to 9 do
                            if j = 0 && empty then ()
                            else if prefix && j > digits[i] then ()
                            else
                                let mutable possibleState = Set.empty<int>
                                for k in state do
                                    for l in nfa[k] do
                                        if l <> lastState && string regex[l] = string j then
                                            possibleState <- Set.union possibleState nfa[l + 1]
                                if not <| Set.isEmpty possibleState then                                  
                                    match newState.TryFind (possibleState, false, prefix && j = digits[i]) with
                                    | None -> newState <- newState |> Map.add (possibleState, false, prefix && j = digits[i]) count
                                    | Some k -> newState <- newState |> Map.add (possibleState, false, prefix && j = digits[i]) (count + k))

                    curState <- newState

                let mutable answer = 0L
                curState |> Map.iter (fun (state, _, _) count ->
                    if Set.contains lastState state then
                        answer <- answer + int64 count)

                answer

            for i = 0 to regex.Length do
                lookup <- Set.empty<int>
                buildNFA i i

            let answer = matchNFA b - matchNFA (a - 1L)
            result.AppendFormat("Case #{0}: {1}\n", case, answer) |> ignore
            case + 1 |> solve

    solve 1
    printfn "%A" result
    0
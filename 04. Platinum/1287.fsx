open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Text

exception Error of unit

[<EntryPoint>]
let main _ =
    try
        let toPostfix (eq: string) =
            let mutable postFix = []
            let number = new StringBuilder()
            let operators = new Stack<string>()

            let opPriority x =
                match x with
                | "(" -> 0 | ")" -> 0
                | "+" -> 1 | "-" -> 1
                | "*" -> 2 | "/" -> 2
                | _ -> raise <| Error()

            eq |> Seq.iteri (fun i x ->
                match x with
                | c when Char.IsDigit c -> number.Append(c) |> ignore
                | c when ['+'; '-'; '*'; '/'] |> List.contains c ->
                    if number.Length = 0 && eq[i - 1] <> ')' then raise <| Error()
                    if number.Length > 0 then postFix <- number.ToString() :: postFix
                    number.Clear() |> ignore

                    if operators.Count = 0 then string c |> operators.Push
                    else
                        let mutable flag = (opPriority <| string c) <= (opPriority <| operators.Peek())
                        while flag do
                            postFix <- operators.Pop() :: postFix
                            flag <- operators.Count > 0 && (opPriority <| string c) <= (opPriority <| operators.Peek())
                        operators.Push <| string c
                | '(' -> operators.Push "("
                | ')' ->
                    if number.Length = 0 && eq[i - 1] = ')' then raise <| Error()
                    if number.Length > 0 then postFix <- number.ToString() :: postFix
                    number.Clear() |> ignore

                    let mutable flag = true
                    while flag do
                        postFix <- operators.Pop() :: postFix
                        flag <- operators.Peek() <> "("
                    operators.Pop() |> ignore
                | _ -> raise <| Error())

            if number.Length = 0 && eq[eq.Length - 1] <> ')' then raise <| Error()
            if number.Length > 0 then postFix <- number.ToString() :: postFix

            let mutable flag = operators.Count > 0
            while flag do
                postFix <- operators.Pop() :: postFix
                flag <- operators.Count > 0

            List.rev postFix

        let eval eq =
            let isNumber s = s |> Seq.forall Char.IsDigit
            let numbers = new Stack<bigint>()

            eq |> List.iter (fun x ->
                match x with
                | num when isNumber num -> numbers.Push (BigInteger.Parse num)
                | op when ["+"; "-"; "*"; "/"] |> List.contains op ->
                    let num2 = numbers.Pop()
                    let num1 = numbers.Pop()
                    
                    match op with
                    | "+" -> numbers.Push (num1 + num2)
                    | "-" -> numbers.Push (num1 - num2)
                    | "*" -> numbers.Push (num1 * num2)
                    | "/" -> numbers.Push (num1 / num2)
                    | _ -> raise <| Error()
                | _ -> raise <| Error())

            if numbers.Count <> 1 then raise <| Error()
            numbers.Pop()

        use stream = new StreamReader(Console.OpenStandardInput())
        let eq = stream.ReadLine().Trim()
        eq |> toPostfix |> eval |> printfn "%A"
    with
    | _ -> printfn "ROCK"

    0
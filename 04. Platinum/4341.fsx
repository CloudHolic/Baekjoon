open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    let rec gcd (x: int64) (y: int64) =
        if y = 0L then x
        else gcd y (x % y)

    let boolToSign b = if b then 1 else -1

    // Change each equation to 'ax + by = c' format, and then return (a, b, c)
    let parse (eq: string array) =
        eq
        |> Array.fold (fun acc x ->
            match x, acc with
            | str, (a, b, c, left, plus) when str.EndsWith 'x' ->
                let coeff = if str = "x" then 1 elif str = "-x" then -1 else str.TrimEnd('x') |> int
                (a + coeff * boolToSign (left = plus), b, c, left, plus)
            | str, (a, b, c, left, plus) when str.EndsWith 'y' ->
                let coeff = if str = "y" then 1 elif str = "-y" then -1 else str.TrimEnd('y') |> int
                (a, b + coeff * boolToSign (left = plus), c, left, plus)
            | str, (a, b, c, left, plus) when str |> Seq.last |> Char.IsDigit ->
                let coeff = int str
                (a, b, c + coeff * boolToSign (left = plus), left, plus)
            | "=", (a, b, c, _, _) -> (a, b, c, false, true)
            | "+", (a, b, c, left, _) -> (a, b, c, left, true)
            | "-", (a, b, c, left, _) -> (a, b, c, left, false)
            | _, (a, b, c, left, plus) -> (a, b, c, left, plus)) (0, 0, 0, true, true)
        |> function
            | (a, b, c, _, _) -> (a, b, -c)

    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()
        
    let printSol x =
        match x with
        | (_, b) when b = 0 -> result.Append("don't know\n") |> ignore
        | _ -> 
            let sign = (int64 <| fst x) * (int64 <| snd x) > 0L // should not be 0
            let absX = (abs <| fst x, abs <| snd x)
            (int64 <| fst absX, int64 <| snd absX)
            ||> gcd |> int
            |> function
                | g when g = snd absX -> result.AppendFormat("{0}\n", boolToSign sign * fst absX / g) |> ignore
                | g -> result.AppendFormat("{0}/{1}\n", boolToSign sign * fst absX / g, snd absX / g) |> ignore

    let tests = stream.ReadLine() |> int
    for i in 1 .. tests do
        let eq1 = stream.ReadLine().Split() |> parse
        let eq2 = stream.ReadLine().Split() |> parse

        match eq1, eq2 with
        | (a1, b1, c1), (a2, b2, c2) when (a1 = 0 && b1 = 0 && c1 <> 0) || (a2 = 0 && b2 = 0 && c2 <> 0) ->
            (0, 0) |> printSol
            (0, 0) |> printSol
        | (a1, b1, c1), (a2, b2, c2) when (a1 = 0 && b1 = 0 && c1 = 0) || (a2 = 0 && b2 = 0 && c2 = 0) ->
            (c1 + c2, a1 + a2) |> printSol
            (c1 + c2, b1 + b2) |> printSol
        | (a1, b1, c1), (a2, b2, c2) when a1 = 0 && a2 = 0 ->
            (0, 0) |> printSol
            if b1 * c2 = b2 * c1 then (c1, b1) |> printSol else (0, 0) |> printSol
        | (a1, b1, c1), (a2, b2, c2) when b1 = 0 && b2 = 0 ->
            if a1 * c2 = a2 * c1 then (c1, a1) |> printSol else (0, 0) |> printSol
            (0, 0) |> printSol
        | (a1, b1, c1), (a2, b2, c2) ->
            (b2 * c1 - b1 * c2, a1 * b2 - a2 * b1) |> printSol
            (a2 * c1 - a1 * c2, a2 * b1 - a1 * b2) |> printSol

        result.Append("\n") |> ignore

        match i with
        | i when i = tests -> ()
        | _ -> stream.ReadLine().Split() |> ignore

    printf "%A" result
    0
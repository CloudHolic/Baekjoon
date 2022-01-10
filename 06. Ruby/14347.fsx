open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let tests = stream.ReadLine() |> int

    let solve1 a b =
        let trial py =
            let get_length posx posy len = len * (1.0 + 1.0 / (posx * posx + posy * posy))

            let eval_ppy x y py =
                let temp1 = 1.0 + py * py
                let temp2 = 2.0 * x * py - 2.0 * y
                let temp3 = x * x + y * y
                temp1 * temp2 / (temp3 * (temp3 + 1.0))
                
            let mutable x, px = -10.0, 0.0001
            let mutable y, py = a, py
            let mutable result = 0.0

            while x < 10.0 do
                let len = px * sqrt (1.0 + py * py)
                result <- result + get_length x y len
                x <- x + px
                let ppy = eval_ppy x y py
                y <- y + px * py
                py <- py + px * ppy

            y, result

        let rec inner low high =
            if low - high |> abs > 1e-5 then
                let mid = (low + high) / 2.0
                let endpos, _ = trial mid
                match endpos with
                | c when c < b -> inner mid high
                | _ -> inner low mid
            else
                trial low |> snd

        [(-100.0, -a / 10.0); (-a / 10.0, 100.0)]
        |> List.map (fun x -> x ||> inner)
        |> List.min

    let solve2 a b c =
        let trial py =
            let get_length posx posy len =
                let temp = posy - c
                len * (1.0 + 1.0 / (posx * posx + posy * posy) + 1.0 / (posx * posx + temp * temp))

            let eval_ppy x y py =
                let temp1 = 1.0 + py * py
                let temp2 = x * x + y * y
                let temp3 = x * x + (y - c) * (y - c)
                let temp4 = -2.0 * y / (temp2 * temp2)
                let temp5 = -2.0 * (y - c) / (temp3 * temp3)
                temp1 * (temp4 + temp5) / (1.0 + 1.0 / temp2 + 1.0 / temp3)
        
            let mutable x, px = -10.0, 0.0001
            let mutable y, py = a, py
            let mutable result = 0.0

            while x < 10.0 do
                let len = px * sqrt (1.0 + py * py)
                result <- result + get_length x y len
                x <- x + px
                let ppy = eval_ppy x y py
                y <- y + px * py
                py <- py + px * ppy

            y, result

        let rec inner low high =
            if low - high |> abs > 1e-5 then
                let mid = (low + high) / 2.0
                let endpos, _ = trial mid
                match endpos with
                | c when c < b -> inner mid high
                | _ -> inner low mid
            else
                trial low |> snd

        [(-100.0, -a / 10.0); (-a / 10.0, (c - a) / 10.0); ((a - c) / 10.0, 100.0)]
        |> List.map (fun x -> x ||> inner)
        |> List.min

    for i = 1 to tests do
        let n, a, b = stream.ReadLine().Trim().Split() |> Array.map float |> function | n -> int n[0], n[1], n[2]
        match n with
        | 1 ->
            let c = stream.ReadLine().Trim() |> float
            result.AppendFormat("Case #{0}: {1:N2}\n", i, (a - c, b - c) ||> solve1) |> ignore
        | _ ->
            let c1, c2 = stream.ReadLine().Trim().Split() |> Array.map float |> function | c -> Array.min c, Array.max c
            result.AppendFormat("Case #{0}: {1:N2}\n", i, (a - c1, b - c1, c2 - c1) |||> solve2) |> ignore

    printfn "%A" result
    0
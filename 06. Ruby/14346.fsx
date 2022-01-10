open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let tests = stream.ReadLine() |> int

    let solve a b =
        let trial y py =
            let get_length posx posy len = len * (1.0 + 1.0 / (posx * posx + posy * posy))
            let eval_ppy x y py =
                let temp1 = 2.0 * x * py - 2.0 * y
                let temp2 = 1.0 + py * py
                let temp3 = x * x + y * y
                temp1 / (temp3 * (temp3 + 1.0)) * temp2
                
            let mutable x, px = -10.0, 0.0001
            let mutable y, py = y, py
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
                let endpos, _ = trial a mid
                match endpos with
                | c when c < b -> inner mid high
                | _ -> inner low mid
            else
                trial a low |> snd

        [(-100.0, -a / 10.0); (-a / 10.0, 100.0)]
        |> List.map (fun x -> x ||> inner)
        |> List.min

    for i = 1 to tests do
        let a, b = stream.ReadLine().Trim().Split() |> Array.map float |> function | n -> n[1], n[2]
        let c = stream.ReadLine().Trim() |> float
        result.AppendFormat("Case #{0}: {1:N2}\n", i, (a - c, b - c) ||> solve) |> ignore

    printfn "%A" result
    0
open System
open System.IO

let pi = 3.14159265358979323846264338327950288419716939937510m

let sinDenominator = List.init 30 (fun x -> x |> fun t -> 2 * (t + 1) * (2 * (t + 1) + 1) |> decimal) |> List.rev
let cosDenominator = List.init 30 (fun x -> x |> fun t -> (2 * (t + 1) - 1) * 2 * (t + 1) |> decimal) |> List.rev

let rec sin x = 
    match x with
    | 0m -> 0m
    | c when c < 0m -> -c |> sin |> fun i -> i * -1m
    | c when c > pi -> 
        let temp = (c / pi) |> Decimal.Floor |> int
        if temp % 2 = 0 then (c % pi) |> sin else (c % pi) |> sin |> fun i -> i * -1m
    | c when c > pi / 4m -> (pi / 2m - c) |> cos
    | c ->
        let square = c * c
        sinDenominator
        |> List.fold (fun acc elem -> 
            let res = square / elem * fst acc
            (res + snd acc, snd acc * -1m)) (1m, -1m)
        |> fun t -> fst t * c
and cos x =
    match x with
    | 0m -> 1m
    | c when c < 0m -> -c |> cos
    | c when c > pi ->
        let temp = (c / pi) |> Decimal.Floor |> int
        if temp % 2 = 0 then (c % pi) |> cos else (c % pi) |> cos |> fun i -> i * -1m
    | c when c > pi / 4m -> (pi / 2m - c) |> sin
    | c ->
        let square = c * c
        cosDenominator
        |> List.fold (fun acc elem ->
            let res = square / elem * fst acc
            (res + snd acc, snd acc * -1m)) (1m, -1m)
        |> fst

[<EntryPoint>]
let main _ = 
    use stream = new StreamReader(Console.OpenStandardInput())
    let a, b, c = stream.ReadLine().Split() |> fun x -> decimal x[0], decimal x[1], decimal x[2]

    let calc x = a * x + b * sin x - c

    let rec solve low high =
        let mid = (low + high) / 2.0m
        if high - low >= 0.000000000000000000001m then
            let res = calc mid
            match res with
            | c when c < 0m -> solve mid high
            | c when c > 0m -> solve low mid
            | _ -> Decimal.Round(mid, 6, MidpointRounding.AwayFromZero)
        else
            Decimal.Round(mid, 6, MidpointRounding.AwayFromZero)

    ((c - b) / a - 1m, (c + b) / a + 1m)
    ||> solve
    |> printfn "%f"

    0
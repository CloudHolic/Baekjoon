open System
open System.IO

let modulo = 1000000007L

let Add x y = (x + y) % modulo
let Mul x y = (x * y) % modulo

let Inverse n =
  let rec fN n i g e l a =
    match e with
    | 0L -> g
    | _ -> let o = n / e
           fN e l a (n - o * e) (i - o * l) (g - o * a)
  (modulo + (fN modulo 1L 0L n 0L 1L)) % modulo

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())

    // Assume that size = 2^k - 1
    let solve k =
        let size = (1 <<< k) - 1
        let combArr =
            [1 .. k - 1]
            |> List.scan (+) 0
            |> List.map (fun x -> int64 <| size - x)

        let mutable answer = 1L
        for i in 1 .. k do
            let n = (1 <<< i) - 1
            let combinate = combArr |> List.take i |> List.fold (fun acc x -> Mul acc x) 1L
            let duplicate =
                [1 .. i - 1]
                |> List.scan (+) 0
                |> List.map (fun x -> int64 <| n - x)
                |> List.fold (fun acc x -> Mul acc x) 1L
                |> Inverse
            answer <- Add answer (Mul combinate duplicate)
        answer

    let max = stream.ReadLine() |> int
    solve max
    |> printfn "%d"
    0
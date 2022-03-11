open System
open System.IO
open System.Text

module Modulo =
    let AddMod m x y = (x + y) % m
    let MulMod m x y = (x * y) % m
    let SquareMod m n = (n * n) % m

    let InverseMod m n =
        let rec fN n i g e l a =
            match e with
            | 0L -> g
            | _ -> let o = n / e
                   fN e l a (n - o * e) (i - o * l) (g - o * a)
        (m + (fN m 1L 0L n 0L 1L)) % m

    let logPow mul sq (n: int64) (k: int64) =
        let infSeq = n |> Seq.unfold (fun state -> Some(state, sq state))
        let binaryList = k |> List.unfold (fun state ->
            match state with
            | 0L -> None
            | _ -> Some(state % 2L, state / 2L))
        let expArr = (List.length binaryList, infSeq) ||> Seq.take |> Array.ofSeq

        binaryList
        |> List.mapi (fun i x ->
            match x with
            | 1L -> expArr[i]
            | _ -> 1L)
        |> List.fold (fun acc x -> mul acc x) 1L
      
    let powMod m = logPow (MulMod m) (SquareMod m)

let modulo = 1000000007L

let Add = Modulo.AddMod modulo
let Sub x y = modulo - y |> Add x
let Mul = Modulo.MulMod modulo
let Inverse = Modulo.InverseMod modulo
let Pow = Modulo.powMod modulo

[<EntryPoint>]
let main _ =
    let parseInts (str: string) = str.Trim().Split() |> Array.map int64

    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let t = stream.ReadLine().Trim() |> int

    let rec solve times =
        if times > 0 then
            let r, g, b, k = stream.ReadLine() |> parseInts |> function | nums -> nums[0], nums[1], nums[2], nums[3]
            let mul1 = Mul g k  // G * k
            let mul2 = Mul b k  // B * k
            let mul3 = Mul r b  // R * B

            let temp1 = (b + 1L, k) ||> Pow // (1 + B)^k
            let temp2 = (b, k + 1L) ||> Pow // B^(k + 1)

            // (Gk+Bk+BR)(1+B)^k - BR^(k+1)
            let numerator = (((Add mul1 mul2, mul3) ||> Add, temp1) ||> Mul, Mul r temp2) ||> Sub
            let denominator = Mul b temp1   // B(1+B)^k

            result.AppendFormat("{0}\n", (numerator, Inverse denominator) ||> Mul) |> ignore
            times - 1 |> solve

    solve t
    printfn "%A" result
    0
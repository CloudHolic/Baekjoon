open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Text

module FastFourierTransform =
    let tau = 2. * Math.PI

    let root a inv =
        if not inv then Complex.FromPolarCoordinates(1., a)
        else Complex.FromPolarCoordinates(1., a) |> Complex.Conjugate

    let fft (vs : Complex []) invert : Complex [] =
        let n = vs.Length

        let mutable result = Array.copy vs

        let rec loop ln inv =
            if ln <= n then
                let roots = root (tau / float ln) inv

                for i in 0 .. ln .. n - 1 do
                    let mutable w = Complex(1., 0.)
                    for j = 0 to ln / 2 - 1 do
                        let u = result.[i + j]
                        let v = result.[i + j + ln / 2] * w

                        result.[i + j] <- u + v
                        result.[i + j + ln / 2] <- u - v

                        w <- w * roots

                loop (ln <<< 1) inv
        
        let mutable j = 0
        for i = 1 to n - 1 do
            let mutable bit = n >>> 1
            while j >= bit do
                j <- j - bit
                bit <- bit >>> 1
            j <- j + bit

            if i < j then
                let temp = result.[i]
                result.[i] <- result.[j]
                result.[j] <- temp

        loop 2 invert
        if invert then result |> Array.map (fun i -> i / Complex(float n, 0.)) else result

[<EntryPoint>]
let main _ =
    let toReverseArr str = str |> Seq.toArray |> Array.rev |> Array.map (fun x -> int x - int '0')

    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let nums = stream.ReadLine().Trim().Split() |> Array.map toReverseArr
    let size = max nums.[0].Length nums.[1].Length

    let x : Complex array = Array.zeroCreate 2097152
    let y : Complex array = Array.zeroCreate 2097152
        
    for i in 0 .. size - 1 do
        if i < nums.[0].Length then x.[i] <- Complex(float nums.[0].[i], 0.)
        if i < nums.[1].Length then y.[i] <- Complex(float nums.[1].[i], 0.)
            
    let stack = new Stack<int>()
    (FastFourierTransform.fft x false, FastFourierTransform.fft y false)
    ||> Array.map2 (fun x y -> x * y)
    |> function | list -> FastFourierTransform.fft list true |> Array.map (fun x -> Math.Floor (x.Real + 0.5) |> int)
    |> Array.iter (fun x -> 
        let mutable cur = 0
        if stack.Count > 0 then cur <- stack.Pop()
        let cur = ((cur + x) / 10, (cur + x) % 10)
        result.Append(snd cur) |> ignore
        if fst cur <> 0 then stack.Push(fst cur))    
      
    result.ToString().TrimEnd('0').ToCharArray()
    |> Array.rev
    |> function | [||] -> "0" | arr -> new String(arr)
    |> printfn "%s"
    0
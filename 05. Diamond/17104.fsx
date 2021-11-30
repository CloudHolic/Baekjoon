open System
open System.Collections
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
    let getPrimes nmax =
        let sieve = new BitArray((nmax/2) + 1, true)
        let result = new ResizeArray<int>(nmax / 10)
        let upper = int (sqrt (float nmax))   
    
        if nmax > 1 then result.Add(2) 

        let mutable m = 1
        while 2 * m + 1 <= nmax do
           if sieve.[m] then
               let n = 2 * m + 1
               if n <= upper then 
                   let mutable i = m
                   while 2 * i < nmax do sieve.[i] <- false; i <- i + n
               result.Add n
           m <- m + 1
    
        result

    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()
       
    let t = stream.ReadLine() |> int
    let x : Complex array = Array.zeroCreate 1048576
    
    for i in getPrimes 1000000 do
        if i = 2 then ()
        else x.[(i - 1) / 2] <- Complex(1., 0.)

    let res = 
        FastFourierTransform.fft x false
        |> Array.map (fun x -> x * x)
        |> function | list -> FastFourierTransform.fft list true |> Array.map (fun x -> Math.Floor (x.Real + 0.5) |> int)
        
    for _ = 1 to t do
        let cur = stream.ReadLine() |> int
        match cur with
        | 4 -> result.Append("1\n") |> ignore
        | _ -> result.AppendFormat("{0}\n", (res.[cur / 2 - 1] + 1) / 2) |> ignore

    printfn "%A" result
    0
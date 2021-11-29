open System
open System.Collections
open System.IO
open System.Numerics
open System.Text

module FastFourierTransform =
    let maxSize = 1048576
    let pi = Math.PI
    let tau = 2. * pi

    module internal Details =
        let isPowerOf2 n = (n &&& n - 1) = 0

        let ilog2 n =
            if n < 2 then failwith "n must be greater than 1"
            if not (isPowerOf2 n) then failwith "n must be a power of 2"
            let rec loop n c s =
                let t = 1 <<< c
                if t = n then c
                elif t > n then
                    loop n (c - s) (s >>> 1)
                else
                    loop n (c + s) (s >>> 1)
            loop n 16 8

        let root a inv =
            if not inv then Complex.FromPolarCoordinates(1., a)
            else Complex.FromPolarCoordinates(1., a) |> Complex.Conjugate

        let roots inv =
            let unfolder c =
                if c < 2 * maxSize then
                    let vs = Array.init (c / 2) (fun i -> root (tau * float i / float c) inv)
                    Some (vs, c*2)
                else
                    None

            Array.unfold unfolder 1

        let rec loop n2 ln s c f t inv =
            if c > 2 then
                let c2 = c >>> 1
                let struct (t, f) = loop n2 (ln - 1) (s <<< 1) c2 f t inv
                let roots = (roots inv).[ln]

                if s > 1 then
                    for j = 0 to c2 - 1 do
                        let w = roots.[j]
                        let off = s * j
                        let off2 = off <<< 1;
                        for i = 0 to s - 1 do
                            let e = Array.get f (i + off2 + 0)
                            let o = Array.get f (i + off2 + s)
                            let a = w * o
                            Array.set t (i + off + 0) (e + a)
                            Array.set t (i + off + n2) (e - a)
                else
                    for j = 0 to c2 - 1 do
                        let w = roots.[j]
                        let e = Array.get f (2 * j + 0)
                        let o = Array.get f (2 * j + s)
                        let a = w * o
                        Array.set t (j + 0)   (e + a)
                        Array.set t (j + n2)  (e - a)

                struct (f, t)

            elif c = 2 then
                for i = 0 to s - 1 do
                    let e = Array.get f (i + 0)
                    let o = Array.get f (i + s)
                    let a = o
                    Array.set t (i + 0)   (e + a)
                    Array.set t (i + n2)  (e - a)

                struct (f, t)

            else
                struct (t, f)

    open Details

    let fft (vs : Complex []) invert : Complex [] =
        let n = vs.Length
        let ln = ilog2 n

        //let vs0 = Array.copy vs
        let vs1 = Array.zeroCreate n

        let struct (_, t) = Details.loop (n >>> 1) ln 1 n vs vs1 invert
        if invert then t |> Array.map (fun i -> i / Complex(float n, 0.)) else t

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
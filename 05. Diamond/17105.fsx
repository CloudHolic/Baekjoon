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
                        let u = result[i + j]
                        let v = result[i + j + ln / 2] * w

                        result[i + j] <- u + v
                        result[i + j + ln / 2] <- u - v

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
                let temp = result[i]
                result[i] <- result[j]
                result[j] <- temp

        loop 2 invert
        if invert then result |> Array.map (fun i -> i / Complex(float n, 0.)) else result

[<EntryPoint>]
let main _ =
    let isPrime n =
        match n with
        | n when n = 2 || n = 3 -> true
        | n when n % 2 = 0 -> false
        | n ->
            let root = float n |> sqrt |> int
            let mutable result = true
            for i in 3 .. 2 .. root do
                if n % i = 0 then result <- false
            result

    let getPrimes max =
        let sieve = new BitArray((max / 2) + 1, true)
        let result = new ResizeArray<int>(max / 10)
        let upper = int (sqrt (float max))   
    
        if max > 1 then result.Add(2) 

        let mutable m = 1
        while 2 * m + 1 <= max do
           if sieve[m] then
               let n = 2 * m + 1
               if n <= upper then 
                   let mutable i = m
                   while 2 * i < max do sieve[i] <- false; i <- i + n
               result.Add n
           m <- m + 1
    
        result
                    
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let t = stream.ReadLine() |> int
    let x : Complex array = Array.zeroCreate 2097152
    let square : Complex array = Array.zeroCreate 2097152
    let primeList = getPrimes 1000000

    for i in primeList do
        if i = 2 then ()
        else 
            x[(i - 1) / 2] <- Complex(1., 0.)
            if i < 500000 then square[i - 1] <- Complex(1., 0.)

    let transform = FastFourierTransform.fft x false
    let res = 
        FastFourierTransform.fft x false
        |> Array.map (fun x -> x * x)
        |> function | list -> FastFourierTransform.fft list true

    let mul =
        (FastFourierTransform.fft res false, transform)
        ||> Array.map2 (fun x y -> x * y)
        |> function | list -> FastFourierTransform.fft list true |> Array.map (fun x -> Math.Floor (x.Real + 0.5) |> int)

    let dupMul =
        (FastFourierTransform.fft square false, transform)
        ||> Array.map2 (fun x y -> x * y)
        |> function | list -> FastFourierTransform.fft list true |> Array.map (fun x -> Math.Floor (x.Real + 0.5) |> int)
        
    for _ = 1 to t do
        let cur = stream.ReadLine() |> int
        let mutable evenDup = 0
        if cur - 4 |> isPrime then evenDup <- 1

        let mutable dup = 0
        if cur % 3 = 0 && cur / 3 |> isPrime then dup <- 1

        let semiDup = (dupMul[(cur - 1) / 2 - 1] - dup)
        let noDup = (mul[(cur - 1) / 2 - 1] - 3 * semiDup - dup) / 6

        let count = noDup + semiDup + dup + evenDup
        result.AppendFormat("{0}\n", count) |> ignore

    printfn "%A" result
    0
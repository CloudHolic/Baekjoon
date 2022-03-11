open System
open System.IO
open System.Numerics

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
    use stream = new StreamReader(Console.OpenStandardInput())

    let num = stream.ReadLine() |> int
    let x : Complex array = Array.zeroCreate 1048576
    let eq = Array.zeroCreate num

    for i = 1 to num - 1 do
        let idx = int64 i * int64 i % int64 num |> int
        let doubleIdx = 2L * int64 i * int64 i % int64 num |> int
        x[idx] <- x[idx] + Complex(1., 0.)
        eq[doubleIdx] <- eq[doubleIdx] + 1

    let res =
        FastFourierTransform.fft x false
        |> Array.map (fun x -> x * x)
        |> function | list -> FastFourierTransform.fft list true |> Array.map (fun x -> Math.Floor (x.Real + 0.5) |> int)

    let mutable answer = 0L
    for i = 1 to num - 1 do    
        let cur = int64 i * int64 i % int64 num |> int
        answer <- answer + int64 (eq[cur] + res[cur] + res[num + cur]) / 2L

    printfn "%d" answer
    0
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

    let multiply arr1 arr2 =
        let size = Array.length arr1 + Array.length arr2 - 1
        let size = Math.Pow(2., (Math.Log (float size) / Math.Log 2.) |> Math.Ceiling) |> int

        let coeff1 = Array.init size (fun i -> if i < Array.length arr1 then Complex(float arr1[i], 0.) else Complex(0., 0.))
        let coeff2 = Array.init size (fun i -> if i < Array.length arr2 then Complex(float arr2[i], 0.) else Complex(0., 0.))

        (fft coeff1 false, fft coeff2 false)
        ||> Array.map2 (fun x y -> x * y)
        |> function
            | res -> fft res true |> Array.map (fun x -> Math.Floor (x.Real + 0.5) |> int)

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let n = stream.ReadLine() |> int    
    let temp = stream.ReadLine().Trim().Split() |> Array.map int
    let y = stream.ReadLine().Trim().Split() |> Array.map int |> Array.rev

    let x = Array.zeroCreate (2 * n)
    Array.Copy(temp, x, n)
    Array.Copy(temp, 0, x, n, n)
       
    FastFourierTransform.multiply x y
    |> Array.max
    |> printfn "%d"

    0
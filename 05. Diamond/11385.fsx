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

        let roots = Array.init (n / 2) (fun x -> root (tau * float x / float n) invert)

        let rec loop ln inv =
            if ln <= n then
                let step = n / ln

                for i in 0 .. ln .. n - 1 do
                    for j = 0 to ln / 2 - 1 do
                        let u = result[i + j]
                        let v = result[i + j + ln / 2] * roots[step * j]

                        result[i + j] <- u + v
                        result[i + j + ln / 2] <- u - v

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
    let cut = 1000L
    let multiply arr1 arr2 =
        (arr1, arr2)
        ||> Array.map2 (fun x y -> x * y)
        |> fun res -> FastFourierTransform.fft res true
        |> Array.map (fun x -> x.Real + 0.5 |> Math.Floor |> int64)

    use stream = new StreamReader(Console.OpenStandardInput())
    let n, m = stream.ReadLine().Split() |> Array.map int |> fun x -> x[0], x[1]
    let fx = stream.ReadLine().Split() |> Array.map int64
    let gx = stream.ReadLine().Split() |> Array.map int64

    let lower_f : Complex array = Array.zeroCreate 2097152
    let upper_f : Complex array = Array.zeroCreate 2097152
    let lower_g : Complex array = Array.zeroCreate 2097152
    let upper_g : Complex array = Array.zeroCreate 2097152

    fx |> Array.iteri (fun i c -> 
        lower_f[i] <- Complex(c % cut |> float, 0.)
        upper_f[i] <- Complex(c / cut |> float, 0.))

    gx |> Array.iteri (fun i c -> 
        lower_g[i] <- Complex(c % cut |> float, 0.)
        upper_g[i] <- Complex(c / cut |> float, 0.))

    let trans = [lower_f; upper_f; lower_g; upper_g] |> List.map (fun x -> FastFourierTransform.fft x false)
    let res_ll, res_lu, res_ul, res_uu = multiply trans[0] trans[2], multiply trans[0] trans[3], multiply trans[1] trans[2], multiply trans[1] trans[3]

    let mutable answer = 0L
    for i = 0 to n + m do
        answer <- answer ^^^ ((res_uu[i] * cut * cut) + ((res_ul[i] + res_lu[i]) * cut) + res_ll[i])

    printfn "%d" answer
    0
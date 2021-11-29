open System
open System.IO
open System.Numerics

module FastFourierTransform =
    let tau = 2. * Math.PI

    let root a inv = 
        if not inv then Complex.FromPolarCoordinates(1., a)
        else Complex.FromPolarCoordinates(1., a) |> Complex.Conjugate

    let fft x invert =
        let rec inner x invert =
            match x, invert with
            | [||], _ -> [||]
            | [|x|], _ -> [|x|]
            | x, invert ->
                x
                |> Array.mapi (fun i c -> i % 2 = 0, c)
                |> Array.partition fst
                |> fun (even, odd) -> inner (Array.map snd even) invert, inner (Array.map snd odd) invert
                ||> Array.mapi2 (fun i even odd ->
                    let btf = odd * (root (tau * (float i / float x.Length)) invert)
                    even + btf, even - btf)
                |> Array.unzip
                ||> Array.append

        inner x invert
        |> function
            | list ->
                if invert then list |> Array.map (fun i -> i / Complex(float x.Length, 0.))
                else list

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let n = stream.ReadLine() |> int
    let nums : Complex array = Array.zeroCreate 524288

    nums.[0] <- Complex(1., 0.)
    for _ = 1 to n do
        let num = stream.ReadLine() |> int
        nums.[num] <- Complex(1., 0.)

    let res = 
        nums
        |> function | list -> FastFourierTransform.fft list false
        |> Array.map (fun x -> x * x)
        |> function | list -> FastFourierTransform.fft list true |> Array.map (fun x -> Math.Floor (x.Real + 0.5) |> int)
    
    let m = stream.ReadLine() |> int
    let mutable count = 0
    for _ = 1 to m do
        let cur = stream.ReadLine() |> int
        if res.[cur] > 0 then count <- count + 1
    
    printfn "%d" count
    0
open System
open System.IO
open System.Text

type Matrix (N: int, M: int) =
    member _.N = N
    member _.M = M
    member val data : int[,] = Array2D.zeroCreate N M with get, set

    member this.Item
        with get (r: int, s: int) = this.data[r, s]
        and set (r: int, s:int) value = this.data[r, s] <- value

    static member (*) (matrix: Matrix, other: Matrix) =
        let temp = new Matrix(matrix.N, other.M)
        for i in 0 .. temp.N - 1 do
            for j in 0 .. temp.M - 1 do
                temp[i, j] <- 0
                for k in 0 .. matrix.M - 1 do
                    temp[i, j] <- (temp[i, j] + matrix[i, k] * other[k, j]) % 10000
        temp

    static member (^^) (matrix: Matrix, k: int) =
        match k with
        | 1 -> matrix
        | k when k &&& 1 = 1 -> matrix * (matrix ^^ (k - 1))
        | _ ->
            let result = matrix ^^ (k / 2)
            result * result

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()
    
    let Mod x = (x % 10000 + 10000) % 10000    
    
    let mutable flag = true
    let mutable inputs = []
    while flag do
        let cur = stream.ReadLine()
        inputs <- cur :: inputs
        flag <- cur <> "STOP"
    let codes = List.rev inputs

    let mutable line = 1
    let rec solve (state: Matrix) =
        let mutable trans = Matrix(27, 27)
        for i in 0 .. 26 do
            trans[i, i] <- 1

        let mutable flag, print = true, false
        while flag do
            let code = codes[line].Trim()
            line <- line + 1

            match code with
            | "STOP" -> flag <- false           // STOP
            | c when c.StartsWith "REPEAT" ->   // REPEAT n
                let times = c.Split() |> function | s -> int s[1]
                let cur = line

                // Execute under loop
                let result = solve (trans * state)
                print <- print || snd result

                match snd result with
                | true ->
                    trans <- fst result * trans
                    for _ in 1 .. times - 1 do
                        line <- cur
                        trans <- (fst <| solve (trans * state)) * trans
                | false ->
                    trans <- (fst result ^^ times) * trans

            | c when c.StartsWith "PRINT" ->    // PRINT var
                let var = c.Split() |> function | s -> char s[1]
                print <- true
                result.AppendFormat("{0} = {1}\n", var, Mod (trans * state)[int var - int 'a', 0]) |> ignore
            | c ->                              // Assignment & Expressions
                let expr = c.Split()
                let temp = Matrix(27, 27)
                for i in 0 .. 26 do
                    temp[i, i] <- 1
                temp[(int <| char expr[0]) - int 'a', (int <| char expr[0]) - int 'a'] <- 0

                for i in 1 .. 2 .. Array.length expr - 1 do
                    let op, var = expr[i], expr[i + 1]
                    let mutable k, v = 1, 26

                    if Char.IsLetter var[var.Length - 1] then v <- int var[var.Length - 1] - int 'a'
                    if var[0] = '-' || Char.IsDigit var[0] then
                        let removeVar = var |> Seq.filter (fun x -> x = '-' || Char.IsDigit x) |> String.Concat
                        k <- int removeVar
                    if op = "-" then k <- k * -1

                    temp[(int <| char expr[0]) - int 'a', v] <- temp[(int <| char expr[0]) - int 'a', v] + k

                trans <- temp * trans

        (trans, print)

    let mutable state = Matrix(27, 1)
    state[26, 0] <- 1
    solve state |> ignore

    printfn "%A" result
    0
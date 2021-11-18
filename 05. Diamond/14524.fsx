open System
open System.IO

let modulo = 1000000007L

let Add x y = (x + y) % modulo
let Mul x y = (x * y) % modulo

type Matrix (N: int, M: int) =
    member _.N = N
    member _.M = M
    member val data : int64[,] = Array2D.zeroCreate N M with get, set

    member this.Item
        with get (r: int, s: int) = this.data.[r, s]
        and set (r: int, s:int) value = this.data.[r, s] <- value

    static member (*) (matrix: Matrix, other: Matrix) =
        let temp = new Matrix(matrix.N, other.M)
        for i in 0 .. temp.N - 1 do
            for j in 0 .. temp.M - 1 do
                temp.[i, j] <- 0L
                for k in 0 .. matrix.M - 1 do
                    temp.[i, j] <- Add temp.[i, j] (Mul matrix.[i, k] other.[k, j])
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
    
    let rec binSearch arr pos target =
        match Array.length arr with
        | 0 -> -1
        | i -> 
            let middle = i / 2
            match sign <| compare target arr.[middle] with
            | 0  -> middle + pos
            | -1 -> binSearch arr.[.. middle - 1] pos target
            | _  -> binSearch arr.[middle + 1 ..] (pos + middle + 1) target
    
    let mutable flag = true
    let mutable inputs = []
    while flag do
        let cur = stream.ReadLine()
        inputs <- cur :: inputs
        flag <- not <| cur.StartsWith "RETURN"
    let codes = List.rev inputs

    let mutable varList = []
    codes
    |> List.iter (fun x -> if x.Contains "=" then varList <- x.Trim().Split().[0] :: varList)
    let varTable = varList |> List.distinct |> List.toArray |> Array.sort
    let varCount = Array.length varTable + 1
    let varIdx = binSearch varTable 0

    let mutable line = 0
    let rec solve (state: Matrix) =
        let mutable trans = Matrix(varCount, varCount)
        for i in 0 .. varCount - 1 do
            trans.[i, i] <- 1L

        let mutable flag = true
        while flag do
            let code = codes.[line].Trim()
            line <- line + 1

            match code with
            | "}" -> flag <- false              // }
            | c when c.StartsWith "RETURN" ->   // RETURN var
                flag <- false
                let idx = varIdx <| c.Split().[1]
                (trans * state).[idx, 0] |> printfn "%d"
            | c when c.Contains "MOO" ->        // n MOO {
                let times = int <| c.Split().[0]
                let result = solve (trans * state)
                trans <- (result ^^ times) * trans
            | c ->                              // Assignment & Expressions
                let expr = c.Split()
                let temp = Matrix(varCount, varCount)
                for i in 0 .. varCount - 1 do
                    temp.[i, i] <- 1L

                let idx = varIdx expr.[0]
                temp.[idx, idx] <- 0L

                for i in 2 .. Array.length expr - 1 do
                    match expr.[i] with
                    | e when ["("; ")"; "+"] |> List.contains e -> ()
                    | e when Char.IsDigit e.[0] ->
                        temp.[idx, varCount - 1] <- Add temp.[idx, varCount - 1] (int64 e)
                    | e ->
                        let curIdx = varIdx e
                        temp.[idx, curIdx] <- Add temp.[idx, curIdx] 1L

                trans <- temp * trans

        trans

    let state = Matrix(varCount, 1)
    state.[varCount - 1, 0] <- 1L
    solve state |> ignore
    0
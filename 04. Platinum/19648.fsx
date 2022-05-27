open System
open System.IO

let Mod = 1000000007L

type Matrix (N: int, M: int) =
    member _.N = N
    member _.M = M
    member val data : int64[,] = Array2D.zeroCreate N M with get, set

    member this.Item
        with get (r: int, s: int) = this.data[r, s]
        and set (r: int, s:int) value = this.data[r, s] <- value

    static member (*) (matrix: Matrix, other: Matrix) =
        let temp = new Matrix(matrix.N, other.M)
        for i in 0 .. temp.N - 1 do
            for j in 0 .. temp.M - 1 do
                temp[i, j] <- 0
                for k in 0 .. matrix.M - 1 do
                    temp[i, j] <- (temp[i, j] + matrix[i, k] * other[k, j]) % Mod
        temp

    static member (^^) (matrix: Matrix, k: int) =
        match k with
        | 1 -> matrix
        | k when k &&& 1 = 1 -> matrix * (matrix ^^ (k - 1))
        | _ ->
            let result = matrix ^^ (k / 2)
            result * result

type Node = {
    A: int;
    B: int
}

[<EntryPoint>]
let main _ =
    use stream = new StreamReader(Console.OpenStandardInput())

    let map = array2D [
        [0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0];
        [0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
        [0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0];
        [1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 0; 0; 0];
        [0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
        [1; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0];
        [0; 0; 0; 1; 1; 0; 0; 1; 0; 0; 0; 1; 1; 0];
        [0; 0; 0; 0; 1; 0; 0; 0; 1; 0; 0; 0; 0; 0];
        [0; 0; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0];
        [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0];
        [0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 0; 0];
        [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0];
        [0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1];
        [0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0]
    ]

    let mapList = [[5]; [3]; [4]; [0; 5; 10]; [1]; [0; 9]; [3; 4; 7; 11; 12]; [4; 8]; [2; 4]; [10]; [6; 11]; [12]; [7; 13]; [8]]
    
    let size = Array2D.length1 map

    let floyd (graph: int[,]) =
        for k = 0 to size - 1 do
            for i = 0 to size - 1 do
                for j = 0 to size - 1 do
                    graph[i, j] <- (graph[i, j], graph[i, k] + graph[k, j]) ||> min
        graph

    let graph = 
        map
        |> Array2D.mapi (fun x y v -> if x <> y && v = 0 then Int32.MaxValue / 2 else v)
        |> floyd

    let mutable nodeList = []
    for i = 0 to size - 1 do
        for j = 0 to size - 1 do
            if i > j && graph[i, j] > 2 && graph[j, i] > 2 then
                nodeList <- nodeList @ [{A = i; B = j}; {A = j; B = i}]
    let nodeList = List.sort nodeList
    let nodeSize = List.length nodeList

    let matrix = Matrix(nodeSize, nodeSize)
    for i = 0 to nodeSize - 1 do
        let startA, startB = nodeList[i] |> fun x -> x.A, x.B
        let nextA, nextB = mapList[startA], mapList[startB]
        for a in nextA do
            for b in nextB do
                let idx = nodeList |> List.tryFindIndex (fun x -> x.A = a && x.B = b)
                match idx with
                | Some c -> matrix[i, c] <- 1L
                | None -> ()

    let n = stream.ReadLine().Trim() |> int
    let resultMatrix = matrix ^^ n    
    let startIndex = nodeList |> List.findIndex (fun x -> x.A = 3 && x.B = 7)

    let mutable result = 0L
    for i = 0 to nodeSize - 1 do
        result <- result + resultMatrix[startIndex, i]
        if result > Mod then result <- result - Mod

    printfn "%d" result
    0
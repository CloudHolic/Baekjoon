open System
open System.Numerics
open System.IO
open System.Text

let pollardRho (n: int64) =
    let find2s n =
        let rec inner s d =
            let isOdd = d &&& 1I |> int
            match isOdd with
            | 1 -> (s, d)
            | _ -> inner <|| (s + 1, d >>> 1)

        inner 0 n

    // Returns true if n is prime number.
    let millerRabin (n: int64) =
        let mulMod (m: bigint) (a: bigint) (b: bigint) = (a * b) % m
        let squareMod (m: bigint) (n: bigint) = (n * n) % m
        let logPow mul sq (n: bigint) (k: int64) =
            let infSeq = n |> Seq.unfold (fun state -> Some(state, sq state))
            let binaryList = k |> List.unfold (fun state ->
                match state with
                | 0L -> None
                | _ -> Some(state % 2L, state / 2L))
            let expArr = (List.length binaryList, infSeq) ||> Seq.take |> Array.ofSeq

            binaryList
            |> List.mapi (fun i x ->
                match x with
                | 1L -> expArr[i]
                | _ -> 1I)
            |> List.fold (fun acc x -> mul acc x) 1I
    
        let powMod m = logPow (mulMod m) (squareMod m)

        // Execute Miller-Rabin Test with n and a.
        let millerRabinTest (n: bigint) (a: int) =
            let (s, d) = (n - 1I) |> find2s
            let b = powMod n (bigint a) (int64 d)

            match (a, n) with
            | _ when b = 1I || b = (n - 1I) -> true
            | _ -> b
                    |> Seq.unfold (fun x -> let fx = (squareMod n x) in Some(x, fx))
                    |> Seq.take s
                    |> Seq.skip 1
                    |> Seq.tryPick (fun x -> if x = 1I then Some(false) elif x = n - 1I then Some(true) else None)
                    |> Option.exists id

        match n with
        | _ when n < 2L -> false
        | _ when n = 2L -> true
        | _ when n = 3L -> true
        | _ when n % 2L = 0L -> false
        | _ -> 
            [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37]
            |> List.filter (fun x -> (int64 x) < n)
            |> List.forall (millerRabinTest <| bigint n)
            
    let random = Random()

    let rec gcd (x: bigint) (y: bigint) =
        if y = 0I then int64 x
        else gcd y (x % y)

    let rec inner (n: int64) x y c factors =
        let squareIncMod (m: bigint) (c: bigint) = (m * m + c) % (bigint n)
        let calc x y c = (squareIncMod x c, squareIncMod (squareIncMod y c) c)

        match n with
        | 1L -> factors
        | _ ->
            millerRabin n
            |> function
                | true -> n :: factors
                | false -> 
                    let (x, y) = calc x y c
                    let g = gcd (bigint n) (abs (x - y))
                    match g with
                    | 1L -> inner n x y c factors
                    | _ when g = n -> 
                        let x = random.Next() |> bigint
                        let c = random.Next() |> bigint
                        inner n x x c factors
                    | _ ->
                        let x = random.Next() |> bigint
                        let c = random.Next() |> bigint
                        inner g x x c factors @ inner (n / g) x x c factors

    let (s, d) = bigint n |> find2s
    let startX = random.Next() |> bigint
    let startC = random.Next() |> bigint
    List.init s (fun _ -> 2L) @ inner (int64 d) startX startX startC []

[<EntryPoint>]
let main _ = 
    use stream = new StreamReader(Console.OpenStandardInput())
    let result = new StringBuilder()

    let lowerBound (arr:'T array) target =
        let rec inner beg en = 
            match sign <| compare beg en with
            | -1 -> 
                let mid = (beg + en) >>> 1
                match sign <| compare arr[mid] target with
                | -1  -> inner (mid + 1) en
                | _ -> inner beg mid
            | _ -> beg

        inner 0 (Array.length arr)

    let findDivisors list =
        let mutable result = []
        let rec inner idx x =
            if idx = List.length list then result <- x :: result
            else
                let mutable cur = 1L
                for _ = 0 to snd list[idx] do
                    inner (idx + 1) (x * cur)
                    cur <- cur * fst list[idx]
        
        inner 0 1L
        result

    let rec solve () =
        let n = stream.ReadLine() |> int64
        if n > 0 then
            let divisors = pollardRho n |> List.countBy id |> findDivisors |> List.sort |> List.toArray
            let mutable answer = Int64.MaxValue
            divisors
            |> Array.iter (fun x ->
                let div = n / x
                let sqrt = Math.Sqrt(float x)
                if answer < div + (Math.Ceiling(2.0 * sqrt) |> int64) then ()
                else
                    let idx = Math.Round(sqrt) |> int64 |> lowerBound divisors                   
                    let mutable diff, flag = 0, true
                    while flag && (idx - diff > 0 || idx + diff < Array.length divisors) do
                        match idx, diff with
                        | i, d when i - d >= 0 && x % divisors[i - d] = 0 ->
                            let y = divisors[i - d]
                            let z = x / y
                            answer <- (answer, y + z + div) ||> min
                            flag <- false
                        | i, d when i + d <= Array.length divisors && x % divisors[i + d] = 0 ->
                            let y = divisors[i + d]
                            let z = x / y
                            answer <- (answer, y + z + div) ||> min
                            flag <- false
                        | _ -> diff <- diff + 1)

            answer |> string |> result.AppendLine |> ignore
            solve ()
        else
            printfn "%A" result

    solve ()
    0
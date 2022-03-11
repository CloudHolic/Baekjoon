open System
open System.IO

[<EntryPoint>]
let main _ =
    // Returns true if n is prime number.
    let millerRabin (n: uint64) = 
        let logPow mul sq (n: uint64) (k: uint64) =
            let infSeq = n |> Seq.unfold (fun state -> Some(state, sq state))
            let binaryList = k |> List.unfold (fun state ->
                match state with
                | 0UL -> None
                | _ -> Some(state % 2UL, state / 2UL))
            let expArr = (List.length binaryList, infSeq) ||> Seq.take |> Array.ofSeq

            binaryList
            |> List.mapi (fun i x ->
                match x with
                | 1UL -> expArr[i]
                | _ -> 1UL)
            |> List.fold (fun acc x -> mul acc x) 1UL
            
        let mulMod (m: uint64) (a: uint64) (b: uint64) = (a * b) % m
        let squareMod (m: uint64) (n: uint64) = (n * n) % m
        let powMod m = logPow (mulMod m) (squareMod m)

        // Execute Miller-Rabin Test with n and a.
        let millerRabinTest (n: uint64) (a: int) =
            let find2s n =
                let rec inner s d =
                    let isOdd = d &&& 1UL |> int
                    match isOdd with
                    | 1 -> (s, d)
                    | _ -> inner <|| (s + 1, d >>> 1)

                inner 0 n

            let (s, d) = (n - 1UL) |> find2s
            let b = powMod n (uint64 a) d

            match (a, n) with
            | _ when b = 1UL || b = (n - 1UL) -> true
            | _ -> b
                   |> Seq.unfold (fun x -> let fx = (squareMod n x) in Some(x, fx))
                   |> Seq.take s
                   |> Seq.skip 1
                   |> Seq.tryPick (fun x -> if x = 1UL then Some(false) elif x = n - 1UL then Some(true) else None)
                   |> Option.exists id
                   
        match n with
        | _ when n < 2UL -> false
        | _ when n = 2UL -> true
        | _ when n = 3UL -> true
        | _ when n % 2UL = 0UL -> false
        | _ -> 
            [2; 7; 61]
            |> List.filter (fun x -> (uint64 x) < n)
            |> List.forall (millerRabinTest <| n)

    use stream = new StreamReader(Console.OpenStandardInput())

    let size = stream.ReadLine().Trim() |> int
    let mutable count = 0

    for i in 1..size do
        let n = stream.ReadLine().Trim() |> uint64
        2UL * n + 1UL
        |> millerRabin
        |> function
            | true -> count <- count + 1
            | _ -> ()

    printfn "%A" count
    0
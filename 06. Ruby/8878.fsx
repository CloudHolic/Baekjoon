open System
open System.IO

[<EntryPoint>]
let main _ = 
    let parseFloats (str: string) = str.Trim().Split() |> Array.map float

    use stream = new StreamReader(Console.OpenStandardInput())
    let refund, probability = stream.ReadLine() |> parseFloats |> function | nums -> nums.[0] / 100., nums.[1] / 100.

    let solve win lose =
        let factor = (1. - probability) / probability
        let x = Math.Pow(factor, float lose)
        let y = Math.Pow(factor, float <| win + lose)
        
        let pLose = (x - y) / (1. - y)
        let pWin = (1. - x) / (1. - y)

        -pLose * float lose * (1. - refund) + pWin * float win

    let mutable win, lose, bestWin, best, flag = 0, 1, 1, 0., true

    while flag do
        flag <- false
        if probability <> 0. then
            let mutable prev, innerFlag = 0., true
            win <- bestWin

            while innerFlag do
                let cur = solve win lose
                if cur > best then
                    best <- cur
                    bestWin <- win
                    flag <- true

                if cur < prev then
                    innerFlag <- false
                else
                    prev <- cur
                    win <- win + 1

            lose <- lose + 1
            if not flag then flag <- false

    printfn "%f" best
    0
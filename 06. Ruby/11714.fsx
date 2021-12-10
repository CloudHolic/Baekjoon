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
                        let u = result.[i + j]
                        let v = result.[i + j + ln / 2] * w

                        result.[i + j] <- u + v
                        result.[i + j + ln / 2] <- u - v

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
                let temp = result.[i]
                result.[i] <- result.[j]
                result.[j] <- temp

        loop 2 invert
        if invert then result |> Array.map (fun i -> i / Complex(float n, 0.)) else result

    let multiply arr1 arr2 =
        let LeastSquare k =
            if k = 0 then 0
            else
                let rec loop i t =
                    if i >= 64 then t
                    else loop (i * 2) (t ||| (t >>> i))
                loop 1 (k - 1) + 1

        let size = Array.length arr1 + Array.length arr2 - 1 |> LeastSquare

        let coeff1 = Array.init size (fun i -> if i < Array.length arr1 then Complex(float arr1.[i], 0.) else Complex(0., 0.))
        let coeff2 = Array.init size (fun i -> if i < Array.length arr2 then Complex(float arr2.[i], 0.) else Complex(0., 0.))

        (fft coeff1 false, fft coeff2 false)
        ||> Array.map2 (fun x y -> x * y)
        |> function
            | res -> fft res true |> Array.map (fun x -> Math.Floor (x.Real + 0.5) |> int)

type Fraction (numerator: int, denominator: int) =
    member internal _.numerator = numerator
    member internal _.denominator = denominator

    member this.isInt =
        let gcd = (max this.denominator this.numerator, min this.denominator this.numerator) ||> Fraction.gcd
        this.numerator = gcd
        
    member this.toInt = this.numerator / this.denominator

    member this.toDecimal = decimal this.numerator / decimal this.denominator

    static member private gcd n d =
        if d = 0 then n
        else Fraction.gcd d (n % d)

    static member (+) (frac1: Fraction, frac2: Fraction) =
        let newN = frac1.numerator * frac2.denominator + frac1.denominator + frac2.numerator
        let newD = frac1.denominator * frac2.denominator
        let gcd = (max newN newD, min newN newD) ||> Fraction.gcd
        Fraction(newN / gcd, newD / gcd)
    
    static member (-) (frac1: Fraction, frac2: Fraction) =
        let newN = frac1.numerator * frac2.denominator - frac1.denominator + frac2.numerator
        let newD = frac1.denominator * frac2.denominator
        let gcd = (max newN newD, min newN newD) ||> Fraction.gcd
        Fraction(newN / gcd, newD / gcd)

    static member (*) (frac1: Fraction, frac2: Fraction) =
        let newN = frac1.numerator * frac2.numerator
        let newD = frac1.denominator * frac2.denominator
        let gcd = (max newN newD, min newN newD) ||> Fraction.gcd
        Fraction(newN / gcd, newD / gcd)

    static member (/) (frac1: Fraction, frac2: Fraction) =
        let newN = frac1.numerator * frac2.denominator
        let newD = frac1.denominator * frac2.numerator
        let gcd = (max newN newD, min newN newD) ||> Fraction.gcd
        Fraction(newN / gcd, newD / gcd)

type Line = {
    Slope: Fraction
    Bias: Fraction } with
    
    static member InfSlope = Fraction(Int32.MaxValue, 1)

    static member CreateNew point1 point2 =
        let diffX = point2.X - point1.X
        let slope = if diffX <> 0 then Fraction(point2.Y - point1.Y, diffX) else Line.InfSlope
        let bias = if diffX <> 0 then Fraction(point1.Y, 1) - Fraction(point1.X, 1) * slope else Fraction(point1.X, 1)
        { Slope = slope; Bias = bias }
    
    member this.eval x = x * this.Slope + this.Bias

and [<CustomComparison; CustomEquality>]
    Point = {
    X: int
    Y: int } with

    override this.GetHashCode() = hash this.X

    interface IComparable<Point> with
        member this.CompareTo other =
            if this.X = other.X then compare this.Y other.Y
            else compare this.X other.X

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | null -> 1
            | :? Point as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not a Point"

    interface IEquatable<Point> with
        member this.Equals other =
            this.X = other.X && this.Y = other.Y

    override this.Equals obj =
        match obj with
        | :? Point as other -> (this :> IEquatable<_>).Equals other
        |_ -> false    


[<EntryPoint>]
let main _ = 
    let parseInts (str: string) = str.Trim().Split() |> Array.map int
    let isInt (x: decimal) = x % 1m = 0m

    let binSearch arr target =        
        let rec inner arr target pos =
            match Array.length arr with
            | 0 -> -1
            | i -> 
                let middle = i >>> 1
                match sign <| compare target arr.[middle] with
                | 0  -> middle + pos
                | -1 -> pos |> inner arr.[.. middle - 1] target
                | _  -> pos + middle + 1 |> inner arr.[middle + 1 ..] target

        inner arr target 0

    let lowerBound arr target =
        let rec inner (arr: 'T array) target beg en = 
            match sign <| compare en beg with
            | -1 -> -1
            | _ -> 
                let mid = (beg + en) >>> 1
                match sign <| compare target arr.[mid] with
                | 1  -> inner arr target (mid + 1) en
                | _ -> 
                    if beg < mid then inner arr target beg mid
                    else mid

        inner arr target 0 (Array.length arr)

    let upperBound arr target =
        let rec inner (arr: 'T array) target beg en =
            match sign <| compare en beg with
            | -1 -> -1
            | 0 -> beg
            | _ ->
                let mid = (beg + en) >>> 1
                match sign <| compare target arr.[mid] with
                | -1 -> inner arr target beg mid
                | _ ->
                    if beg < mid then inner arr target (mid + 1) en
                    else mid + 1

        inner arr target 0 (Array.length arr)

    use stream = new StreamReader(Console.OpenStandardInput())
    let l, m, n = stream.ReadLine() |> parseInts |> function | nums -> nums.[0], nums.[1], nums.[2]

    let mutable Apoints: Point[] = Array.zeroCreate l
    let mutable Bpoints: Point[] = Array.zeroCreate m
    let mutable Cpoints: Point[] = Array.zeroCreate n

    let mutable Aline = { Slope = Fraction(0, 1); Bias = Fraction(0, 1) }
    let mutable Bline = { Slope = Fraction(0, 1); Bias = Fraction(0, 1) }

    let mutable onePointA = true
    for i = 0 to l - 1 do
        stream.ReadLine() 
        |> parseInts 
        |> function
            | nums -> 
                if i > 0 && onePointA && (nums.[0] <> Apoints.[i - 1].X || nums.[1] <> Apoints.[i - 1].Y) then
                    onePointA <- false
                    Aline <- Line.CreateNew Apoints.[i - 1] { X = nums.[0]; Y = nums.[1] }
                Apoints.[i] <- { X = nums.[0]; Y = nums.[1] }

    let mutable onePointB = true
    for i = 0 to m - 1 do        
        stream.ReadLine() 
        |> parseInts 
        |> function 
            | nums -> 
                if i > 0 && onePointB && (nums.[0] <> Bpoints.[i - 1].X || nums.[1] <> Bpoints.[i - 1].Y) then 
                    onePointB <- false
                    Bline <- Line.CreateNew Bpoints.[i - 1] { X = nums.[0]; Y = nums.[1] }
                Bpoints.[i] <- { X = nums.[0]; Y = nums.[1] }

    for i = 0 to n - 1 do
        stream.ReadLine() |> parseInts |> function | nums -> Cpoints.[i] <- { X = nums.[0]; Y = nums.[1] }

    let mutable answer = 0

    // Case 1: l = 1 || m = 1
    if onePointA || onePointB then
        for i = 0 to l - 1 do
            for j = 0 to m - 1 do
                let sumX = Apoints.[i].X + Bpoints.[j].X
                let sumY = Apoints.[i].Y + Bpoints.[j].Y

                if sumX &&& 1 = 0 && sumY &&& 1 = 0 then
                    let idx = binSearch Cpoints { X = sumX / 2; Y = sumY / 2 }
                    if idx > -1 then answer <- answer + 1
    else
        // Before Case 2 & 3, remove Inf slope.
        if Aline.Slope = Line.InfSlope || Bline.Slope = Line.InfSlope then
            if Aline.Slope.toDecimal = 0m || Bline.Slope.toDecimal = 0m then
                // 45 degree transform
                Apoints <- Apoints |> Array.map (fun x -> { X = x.X - x.Y; Y = x.X + x.Y }) |> Array.sort
                Bpoints <- Bpoints |> Array.map (fun x -> { X = x.X - x.Y; Y = x.X + x.Y }) |> Array.sort
                Cpoints <- Cpoints |> Array.map (fun x -> { X = x.X - x.Y; Y = x.X + x.Y }) |> Array.sort
            else
                // Symmetric transform
                Apoints <- Apoints |> Array.map (fun x -> { X = x.Y; Y = x.X }) |> Array.sort
                Bpoints <- Bpoints |> Array.map (fun x -> { X = x.Y; Y = x.X }) |> Array.sort
                Cpoints <- Cpoints |> Array.map (fun x -> { X = x.Y; Y = x.X }) |> Array.sort

        let Aline = Line.CreateNew Apoints.[0] Apoints.[l - 1]
        let Bline = Line.CreateNew Bpoints.[0] Bpoints.[m - 1]
        
        // Case 2: A.Slope <> B.Slope
        if Aline.Slope <> Bline.Slope then
            let diffSlope = Bline.Slope - Aline.Slope
            for i = 0 to n - 1 do
                let temp1 = Fraction(2 * Cpoints.[i].X, 1)
                let temp2 = Fraction(2 * Cpoints.[i].Y, 1) - Aline.Bias - Bline.Bias

                let newX1 = (temp1 * Bline.Slope - temp2) / diffSlope
                let newX2 = (temp2 - temp1 * Aline.Slope) / diffSlope
                let newY1 = Aline.eval newX1
                let newY2 = Bline.eval newX2

                if newX1.isInt && newX2.isInt && newY1.isInt && newY2.isInt then
                    let numA = upperBound Apoints { X = newX1.toInt; Y = newY1.toInt } - lowerBound Apoints { X = newX1.toInt; Y = newY1.toInt }
                    let numB = upperBound Bpoints { X = newX2.toInt; Y = newY2.toInt } - lowerBound Bpoints { X = newX2.toInt; Y = newY2.toInt }
                    answer <- answer + numA * numB

        // Case 3: A.Slope = B.Slope
        else
            ()

    printfn "%d" answer
    0
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

[<CustomComparison; CustomEquality>]
type Fraction = { 
    Numerator: bigint
    Denominator: bigint } with

    member this.isInt() =
        let gcd = (max this.Denominator this.Numerator, min this.Denominator this.Numerator) ||> Fraction.gcd
        abs gcd = abs this.Denominator
        
    member this.toInt() = this.Numerator / this.Denominator

    static member private gcd n d =
        if d = 0I then n
        else Fraction.gcd d (n % d)

    static member CreateNew n d =
        let gcd = Fraction.gcd n d
        { Numerator = n / gcd; Denominator = d / gcd }

    static member (+) (frac1: Fraction, frac2: Fraction) =
        let newN = frac1.Numerator * frac2.Denominator + frac1.Denominator * frac2.Numerator
        let newD = frac1.Denominator * frac2.Denominator
        let gcd = (max newN newD, min newN newD) ||> Fraction.gcd
        (newN / gcd, newD / gcd) ||> Fraction.CreateNew
    
    static member (-) (frac1: Fraction, frac2: Fraction) =
        let newN = frac1.Numerator * frac2.Denominator - frac1.Denominator * frac2.Numerator
        let newD = frac1.Denominator * frac2.Denominator
        let gcd = (max newN newD, min newN newD) ||> Fraction.gcd
        (newN / gcd, newD / gcd) ||> Fraction.CreateNew

    static member (*) (frac1: Fraction, frac2: Fraction) =
        let newN = frac1.Numerator * frac2.Numerator
        let newD = frac1.Denominator * frac2.Denominator
        let gcd = (max newN newD, min newN newD) ||> Fraction.gcd
        (newN / gcd, newD / gcd) ||> Fraction.CreateNew

    static member (/) (frac1: Fraction, frac2: Fraction) =
        let newN = frac1.Numerator * frac2.Denominator
        let newD = frac1.Denominator * frac2.Numerator
        let gcd = (max newN newD, min newN newD) ||> Fraction.gcd
        (newN / gcd, newD / gcd) ||> Fraction.CreateNew

    override this.GetHashCode() = hash <| this.toInt()

    interface IComparable<Fraction> with
        member this.CompareTo other =
            (this.Numerator * other.Denominator, this.Denominator * other.Numerator) ||> compare

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | null -> 1
            | :? Fraction as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not a Point"

    interface IEquatable<Fraction> with
        member this.Equals other =            
            this.Numerator * other.Denominator = this.Denominator * other.Numerator

    override this.Equals obj =
        match obj with
        | :? Fraction as other -> (this :> IEquatable<_>).Equals other
        |_ -> false

type Line = {
    Slope: Fraction
    Bias: Fraction } with
    
    static member InfSlope = (bigint Int64.MaxValue, 1I) ||> Fraction.CreateNew

    static member CreateNew point1 point2 =
        let diffX = point2.X - point1.X
        let slope = if diffX <> 0I then (point2.Y - point1.Y, diffX) ||> Fraction.CreateNew else Line.InfSlope
        let bias = if diffX <> 0I then (Fraction.CreateNew point1.Y 1I) - (Fraction.CreateNew point1.X 1I) * slope else (point1.X, 1I) ||> Fraction.CreateNew
        { Slope = slope; Bias = bias }
    
    member this.eval x = x * this.Slope + this.Bias

and [<CustomComparison; CustomEquality>]
    Point = {
    X: bigint
    Y: bigint } with

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
    let parseInts f (str: string) = str.Trim().Split() |> Array.map f
    let spinTransform x = { X = x.X - x.Y; Y = x.X + x.Y }
    let symmetricTransform x = { X = x.Y; Y = x.X }

    let lowerBound arr target =
        let rec inner (arr: 'T array) target beg en = 
            match sign <| compare beg en with            
            | -1 -> 
                let mid = (beg + en) >>> 1
                match sign <| compare arr[mid] target with
                | -1  -> inner arr target (mid + 1) en
                | _ -> inner arr target beg mid
            | _ -> beg

        inner arr target 0 (Array.length arr)

    let upperBound arr target =
        let rec inner (arr: 'T array) target beg en =
            match sign <| compare beg en with
            | -1 ->
                let mid = (beg + en) >>> 1
                match sign <| compare arr[mid] target with
                | 1 -> inner arr target beg mid
                | _ -> inner arr target (mid + 1) en
            | _ -> beg

        inner arr target 0 (Array.length arr)

    use stream = new StreamReader(Console.OpenStandardInput())
    let l, m, n = stream.ReadLine() |> parseInts int |> function | nums -> nums[0], nums[1], nums[2]

    let mutable Apoints: Point[] = Array.zeroCreate l
    let mutable Bpoints: Point[] = Array.zeroCreate m
    let mutable Cpoints: Point[] = Array.zeroCreate n

    let mutable Aline = { Slope = (0I, 1I) ||> Fraction.CreateNew; Bias = (0I, 1I) ||> Fraction.CreateNew }
    let mutable Bline = { Slope = (0I, 1I) ||> Fraction.CreateNew; Bias = (0I, 1I) ||> Fraction.CreateNew }

    let mutable onePointA = true
    for i = 0 to l - 1 do
        stream.ReadLine() 
        |> parseInts bigint.Parse
        |> function
            | nums -> 
                if i > 0 && onePointA && (nums[0] <> Apoints[i - 1].X || nums[1] <> Apoints[i - 1].Y) then
                    onePointA <- false
                    Aline <- Line.CreateNew Apoints[i - 1] { X = nums[0]; Y = nums[1] }
                Apoints[i] <- { X = nums[0]; Y = nums[1] }

    let mutable onePointB = true
    for i = 0 to m - 1 do        
        stream.ReadLine() 
        |> parseInts bigint.Parse
        |> function 
            | nums -> 
                if i > 0 && onePointB && (nums[0] <> Bpoints[i - 1].X || nums[1] <> Bpoints[i - 1].Y) then 
                    onePointB <- false
                    Bline <- Line.CreateNew Bpoints[i - 1] { X = nums[0]; Y = nums[1] }
                Bpoints[i] <- { X = nums[0]; Y = nums[1] }

    for i = 0 to n - 1 do
        stream.ReadLine() |> parseInts bigint.Parse |> function | nums -> Cpoints[i] <- { X = nums[0]; Y = nums[1] }

    let mutable answer = 0L

    // Case 1: l = 1 || m = 1
    if onePointA || onePointB then
        Apoints <- Apoints |> Array.sort
        Bpoints <- Bpoints |> Array.sort
        Cpoints <- Cpoints |> Array.sort

        if onePointA then
            for i = 0 to m - 1 do
                let sumX = Apoints[0].X + Bpoints[i].X
                let sumY = Apoints[0].Y + Bpoints[i].Y

                if sumX &&& 1I = 0I && sumY &&& 1I = 0I then                    
                    let idx = upperBound Cpoints { X = sumX / 2I; Y = sumY / 2I } - lowerBound Cpoints { X = sumX / 2I; Y = sumY / 2I }
                    answer <- answer + int64 idx * int64 l
        else if onePointB then
            for i = 0 to l - 1 do
                let sumX = Apoints[i].X + Bpoints[0].X
                let sumY = Apoints[i].Y + Bpoints[0].Y

                if sumX &&& 1I = 0I && sumY &&& 1I = 0I then                    
                    let idx = upperBound Cpoints { X = sumX / 2I; Y = sumY / 2I } - lowerBound Cpoints { X = sumX / 2I; Y = sumY / 2I }
                    answer <- answer + int64 idx * int64 m

    else
        // Before Case 2 & 3, remove Inf slope.
        if Aline.Slope.Numerator = bigint Int64.MaxValue || Bline.Slope.Numerator = bigint Int64.MaxValue then
            if Aline.Slope.Numerator = 0I || Bline.Slope.Numerator = 0I then
                // 45 degree transform
                Apoints <- Apoints |> Array.map spinTransform
                Bpoints <- Bpoints |> Array.map spinTransform
                Cpoints <- Cpoints |> Array.map spinTransform
            else
                // Symmetric transform
                Apoints <- Apoints |> Array.map symmetricTransform
                Bpoints <- Bpoints |> Array.map symmetricTransform
                Cpoints <- Cpoints |> Array.map symmetricTransform

        Apoints <- Apoints |> Array.sort
        Bpoints <- Bpoints |> Array.sort
        Cpoints <- Cpoints |> Array.sort

        let Aline = Line.CreateNew Apoints[0] Apoints[l - 1]
        let Bline = Line.CreateNew Bpoints[0] Bpoints[m - 1]
        
        // Case 2: A.Slope <> B.Slope
        if Aline.Slope <> Bline.Slope then
            let diffSlope = Bline.Slope - Aline.Slope
            for i = 0 to n - 1 do
                let temp1 = (2I * Cpoints[i].X, 1I) ||> Fraction.CreateNew
                let temp2 = (Fraction.CreateNew <|| (2I * Cpoints[i].Y, 1I)) - Aline.Bias - Bline.Bias

                let newX1 = (temp1 * Bline.Slope - temp2) / diffSlope
                let newX2 = (temp2 - temp1 * Aline.Slope) / diffSlope
                let newY1 = Aline.eval newX1
                let newY2 = Bline.eval newX2

                if newX1.isInt() && newX2.isInt() && newY1.isInt() && newY2.isInt() then
                    let numA = upperBound Apoints { X = newX1.toInt(); Y = newY1.toInt() } - lowerBound Apoints { X = newX1.toInt(); Y = newY1.toInt() }
                    let numB = upperBound Bpoints { X = newX2.toInt(); Y = newY2.toInt() } - lowerBound Bpoints { X = newX2.toInt(); Y = newY2.toInt() }
                    answer <- answer + int64 numA * int64 numB

        // Case 3: A.Slope = B.Slope
        else
            let x: Complex array = Array.zeroCreate 1048576
            let y: Complex array = Array.zeroCreate 1048576

            for i = 0 to l - 1 do
                let idx = int Apoints[i].X + 200000
                x[idx] <- x[idx] + Complex(1., 0.)

            for i = 0 to m - 1 do
                let idx = int Bpoints[i].X + 200000
                y[idx] <- y[idx] + Complex(1., 0.)

            let res =
                (FastFourierTransform.fft x false, FastFourierTransform.fft y false)
                ||> Array.map2 (fun x y -> x * y)
                |> function | list -> FastFourierTransform.fft list true |> Array.map (fun x -> Math.Floor (x.Real + 0.5) |> int)
                            
            let desiredBias = (Aline.Bias + Bline.Bias) / (Fraction.CreateNew 2I 1I)
            for i = 0 to n - 1 do
                let eval = (Fraction.CreateNew Cpoints[i].X 1I) * Aline.Slope + desiredBias
                if eval.isInt() && Cpoints[i].Y = eval.toInt() then
                    let idx = int Cpoints[i].X * 2 + 400000
                    answer <- answer + int64 res[idx]

    printfn "%d" answer
    0
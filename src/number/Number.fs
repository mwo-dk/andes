namespace Andes 

open System.Numerics
open System

[<AutoOpen>]
module NumberHelpers =
    open System.Globalization

    let isInf = System.Double.IsInfinity
    let isNaN = System.Double.IsNaN

    let private english = CultureInfo("en-US")
    let private englishNumber = english.NumberFormat

    [<CompiledName("IsInteger")>]
    let isInteger (s: string) = System.Int32.TryParse(s, englishNumber) |> fst
    [<CompiledName("IsReal")>]
    let isReal (s: string) = System.Double.TryParse(s, englishNumber) |> fst
    let isImaginary (s: string) =
        if System.String.IsNullOrWhiteSpace(s) then false
        else
            let s = s.Trim()
            if s = "i" || s = "-i" then true
            else
                if s.StartsWith("i") then
                    isReal (s.Substring(1, s.Length-1))
                else
                    if s.StartsWith("-i") then
                        isReal (s.Substring(2, s.Length-2))
                    else
                        if s.EndsWith("i") then
                            isReal (s.Substring(0, s.Length-1))
                        else
                            false
    [<CompiledName("IsComplex")>]
    let isComplex (s: string) = 
        if System.String.IsNullOrWhiteSpace(s) then false
        else
            let s = s.Trim().TrimStart('+').TrimStart('-')
            if isReal s || isImaginary s then true
            else
                match s.Split('+', System.StringSplitOptions.RemoveEmptyEntries) with
                | [|a; b|] when isReal a && isImaginary b || isImaginary a && isReal b -> true
                | _ -> 
                    match s.Split('-', System.StringSplitOptions.RemoveEmptyEntries) with
                    | [|a; b|] when isReal a && isImaginary b || isImaginary a && isReal b -> true
                    | _ -> false
    [<CompiledName("IsNumber")>]
    let isNumber (s: string) = isInteger s || isReal s || isComplex s
    [<CompiledName("IsFraction")>]
    let isFraction (s: string) =
        if System.String.IsNullOrWhiteSpace(s) then false
        else 
            match s.Split('/', System.StringSplitOptions.RemoveEmptyEntries) with
            | [|a; b|] when isNumber a && isNumber b -> true
            | _ -> false

    [<CompiledName("TryParseInteger")>]
    let tryParseInteger (s: string) =
        match System.Int32.TryParse(s, englishNumber) with
        | true, n -> Some n
        | _ -> None
    [<CompiledName("TryParseReal")>]
    let tryParseReal (s: string) =
        match System.Double.TryParse(s, englishNumber) with
        | true, n -> Some n
        | _ -> None
    [<CompiledName("TryParseImaginary")>]
    let tryParseImaginary (s: string) =
        if System.String.IsNullOrWhiteSpace(s) then None
        else
            let s = s.Trim()
            if s = "i" then Some 1.0
            else
                if s = "-i" then Some -1.0
                else
                if s.StartsWith("i") then
                    match System.Double.TryParse(s.Substring(1, s.Length-1), englishNumber) with
                    | true, n -> Some n
                    | _ -> None
                else 
                    if s.StartsWith("-i") then
                        match System.Double.TryParse(s.Substring(2, s.Length-2), englishNumber) with
                        | true, n -> Some -n
                        | _ -> None
                    else
                        if s.EndsWith("i") then
                            match System.Double.TryParse(s.Substring(0, s.Length-1), englishNumber) with
                            | true, n -> Some n
                            | _ -> None
                        else
                            None
    [<CompiledName("TryParseComplex")>]
    let tryParseComplex (s: string) =
        let s = s.Trim()
        if System.String.IsNullOrWhiteSpace(s) then None
        else
            match tryParseReal s with
            | Some x -> Some (Complex(x, 0.0))
            | _ ->
                match tryParseImaginary s with
                | Some x -> Some (Complex(0.0, x))
                | _ ->
                    match s.Split('+', System.StringSplitOptions.RemoveEmptyEntries) with // Leading '+' are automatically removed
                        | [|a; b|] when isReal a && isImaginary b -> 
                            match tryParseReal a, tryParseImaginary b with
                            | Some a, Some b -> Some (Complex(a, b))
                            | _, _ -> None
                        | [|a; b|] when isImaginary a && isReal b -> 
                            match tryParseImaginary a, tryParseReal b with
                            | Some b, Some a -> Some (Complex(a, b))
                            | _, _ -> None
                        | _ -> 
                            match s.TrimStart('+').Split('-', System.StringSplitOptions.RemoveEmptyEntries) with
                            | [|a; b|] when isReal a && isImaginary b -> 
                                match tryParseReal a, tryParseImaginary b with
                                | Some a, Some b -> Some (Complex(a, -b))
                                | _, _ -> None
                            | [|a; b|] when isImaginary a && isReal b -> 
                                match tryParseImaginary a, tryParseReal b with
                                | Some b, Some a -> Some (Complex(a, b))
                                | _, _ -> None
                            | _ -> None

type Number =
| NotANumber
| Z
| N of int
| NQ of int*int
| Q of float*float
| R of float
| C of Complex
| QC of Complex*Complex
with
    interface IComparable<Number> with
        member this.CompareTo (other: Number) =
            match this, other with
            | Z, Z -> 0
            | Z, _ -> -1
            | _, Z -> 1
            | N a, N b -> a.CompareTo b
            | N a, NQ (c, d) -> (a*d).CompareTo c
            | NQ (c, d), N a -> c.CompareTo (a*d)
            | NQ (a, b), NQ (c, d) -> (a*d).CompareTo (c*b)
            | Q (a, b), Q (c, d) -> (a*d).CompareTo (c*b)
            | R a, R b -> a.CompareTo b
            | _, _ -> failwith "Invalid operation"
    member this.IsInfinite =
        match this with
        | NQ (a, b) -> b = 0
        | Q (a, b) -> isInf a || isInf b || b = 0.0
        | R r -> isInf r
        | C c -> isInf c.Real || isInf c.Imaginary
        | QC (a, b) ->
            isInf a.Real || isInf a.Imaginary || isInf b.Real || isInf b.Imaginary || b = Complex.Zero
        | _ -> false
    member this.IsNaN =
        match this with
        | NotANumber -> true
        | Q (a, b) -> isNaN a || isNaN b || b = 0.0
        | R r -> isNaN r
        | C c -> isNaN c.Real || isNaN c.Imaginary
        | QC (a, b) -> 
            isNaN a.Real || isNaN a.Imaginary || isNaN b.Real || isNaN b.Imaginary
        | _ -> false
    member private this.IsZero' =
        match this with
        | Z -> true
        | N 0 -> true
        | NQ (0, b) when b <> 0 -> true
        | R 0.0 -> true
        | Q (0.0, b) when b <> 0.0 -> true
        | C c when c = Complex.Zero -> true
        | QC (a, b) when a = Complex.Zero && b <> Complex.Zero -> true
        | _ -> false
    member private this.IsOne' =
        match this with
        | N 1 -> true
        | NQ (a, b) when a = b && b <> 0 -> true
        | Q (a, b) when a = b && b <> 0.0 -> true
        | R 1.0 -> true
        | C c when c = Complex.One -> true
        | QC (a, b) when a = b && b <> Complex.Zero -> true
        | _ -> false
    member this.IsZero = 
        let simplified: Number = this.Simplify()
        simplified.IsZero'
    member this.IsOne = 
        let simplified: Number = this.Simplify()
        simplified.IsOne'

    member this.Simplify() =
        match this with
        | NotANumber -> NotANumber
        | x when x.IsZero' -> Z
        | x when x.IsOne' -> N 1
        | NQ (a, 1) -> N a
        | NQ (_, 0) -> NotANumber
        | NQ (a, b) when a = b -> N 1
        | NQ (a, b) when a = -b -> N (-1)
        | Q (a, 1.0) -> (R a).Simplify()
        | Q (_, 0.0) -> NotANumber
        | Q (a, b) when a = b -> (R 1.0).Simplify()
        | Q (a, b) when a = -b -> (R -1.0).Simplify()
        | Q (a, b) when float (int a) = a && float (int b) = b -> (NQ (int a, int b)).Simplify()
        | R x when float (int x) = x -> (N (int x)).Simplify()
        | C c when c.Imaginary = 0.0 -> (R c.Real).Simplify()
        | QC (a, b) when b = Complex.One -> (C a).Simplify()
        | QC (a, b) when b = Complex.Zero -> NotANumber
        | QC (a, b) when a = b -> R 1.0
        | QC (a, b) when a = -b -> R -1.0
        | QC (a, b) -> (C (a/b)).Simplify()
        | _ -> this

    static member Lift (x: Number) (y: Number) =
        match x.Simplify(), y.Simplify() with
        | N x, NQ (a, b) -> NQ (x, 1), NQ(a, b)
        | NQ (a, b), N x -> NQ(a, b), NQ (x, 1)
        | N x, Q (a, b) -> Q (float x, 1.0), Q (a, b)
        | Q (a, b), N x -> Q (a, b), Q (float x, 1.0)
        | N x, R r -> R (float x), R r
        | R r, N x -> R r, R (float x)
        | N x, C c -> C (Complex(float x, 1.0)), C c
        | C c, N x -> C c, C (Complex(float x, 1.0))
        | N x, QC (a, b) | QC (a, b), N x -> QC (Complex(float x, 0.0), Complex.One), QC (a, b)

        | NQ (a, b), Q (c, d) -> Q (float a, float b), Q (c, d)
        | Q (c, d), NQ (a, b) -> Q (c, d), Q (float a, float b)
        | NQ (a, b), R r -> R (float a/float b), R r
        | R r, NQ (a, b) -> R r, R (float a/float b)
        | NQ (a, b), C c -> C (Complex(float a/float b, 0.0)), C c
        | C c, NQ (a, b) -> C c, C (Complex(float a/float b, 0.0))
        | NQ (a, b), QC (c, d) -> QC (Complex(float a/float b, 0.0), Complex.One), QC (c, d) 
        | QC (c, d), NQ (a, b) -> QC (c, d), QC (Complex(float a/float b, 0.0), Complex.One)

        | Q (a, b), R r -> R (a/b), R r
        | R r, Q (a, b) -> R r, R (a/b)
        | Q (a, b), C c -> C (Complex(a/b, 0.0)), C c
        | C c, Q (a, b) -> C c, C (Complex(a/b, 0.0))
        | Q (a, b), QC (c, d) -> QC (Complex(a/b, 0.0), Complex.One), QC (c, d)
        | QC (c, d), Q (a, b) -> QC (c, d), QC (Complex(a/b, 0.0), Complex.One)

        | R a, C c -> C (Complex(a, 0.0)), C c
        | C c, R a -> C c, C (Complex(a, 0.0))
        | R a, QC (c, d) -> QC (Complex(a, 0.0), Complex.One), QC (c, d)
        | QC (c, d), R a -> QC (c, d), QC (Complex(a, 0.0), Complex.One)

        | C a, QC (c, d) -> QC (a, Complex.One), QC (c, d)
        | QC (c, d), C a -> QC (c, d), QC (a, Complex.One)

        | _ -> x, y

    static member (~+) (x: Number) = x
    static member (~-) (x: Number) =
        match x.Simplify() with
        | NotANumber -> NotANumber
        | x when x.IsInfinite -> x
        | Z -> Z
        | N n -> N (-n)
        | NQ (a, b) -> NQ (-a, b)
        | Q (a, b) -> Q (-a, b)
        | R r -> R (-r)
        | C c -> C (-c)
        | QC (a, b) -> QC (-a, b)

    static member (+) (x: Number, y: Number) =
        let result = 
            match Number.Lift x y with
            | NotANumber, _ | _, NotANumber -> NotANumber
            | Z, x | x, Z -> x
            | N x, N y -> N (x+y)
            | NQ (a, b), NQ (c, d) -> NQ (a*d+b*c, b*d)
            | Q (a, b), Q (c, d) -> Q (a*d+b*c, b*d)
            | R a, R b -> R (a+b)
            | C a, C b -> C (a+b)
            | QC (a, b), QC (c, d) -> QC (a*d+b*c, b*d)
            | _, _ -> failwith "Invalid operation"
        result.Simplify()
    static member (-) (x: Number, y: Number) =
        let result = 
            match Number.Lift x y with
            | NotANumber, _ | _, NotANumber -> NotANumber
            | Z, x -> -x
            | x, Z -> x
            | N x, N y -> N (x-y)
            | NQ (a, b), NQ (c, d) -> NQ (a*d-b*c, b*d)
            | Q (a, b), Q (c, d) -> Q (a*d-b*c, b*d)
            | R a, R b -> R (a-b)
            | C a, C b -> C (a-b)
            | QC (a, b), QC (c, d) -> QC (a*d-b*c, b*d)
            | _, _ -> failwith "Invalid operation"
        result.Simplify()
    static member (*) (x: Number, y: Number) =
        let result = 
            match Number.Lift x y with
            | NotANumber, _ | _, NotANumber -> NotANumber
            | Z, _ | _, Z -> Z
            | N x, N y -> N (x*y)
            | NQ (a, b), NQ (c, d) -> NQ (a*c, b*d)
            | Q (a, b), Q (c, d) -> Q (a*c, b*d)
            | R a, R b -> R (a*b)
            | C a, C b -> C (a*b)
            | QC (a, b), QC (c, d) -> QC (a*b, c*d)
            | _, _ -> failwith "Invalid operation"
        result.Simplify()
    static member (/) (x: Number, y: Number) =
        let result = 
            match Number.Lift x y with
            | NotANumber, _ | _, NotANumber | _, Z -> NotANumber
            | Z, _ -> Z
            | N x, N y -> NQ (x, y)
            | NQ (a, b), NQ (c, d) -> NQ (a*d, b*c)
            | Q (a, b), Q (c, d) -> Q (a*d, b*c)
            | R a, R b -> R (a/b)
            | C a, C b -> C (a/b)
            | QC (a, b), QC (c, d) -> QC (a*d, b*c)
            | _, _ -> failwith "Invalid operation"
        result.Simplify()

    static member TryParse (s: string) =
        if System.String.IsNullOrWhiteSpace(s) then NotANumber
        else 
            let result =
                if isFraction s then
                    match s.Split('/', System.StringSplitOptions.RemoveEmptyEntries) with
                    | [|a; b|] -> 
                        match tryParseInteger a, tryParseInteger b with
                        | Some a, Some b -> NQ (a, b)
                        | _, _ -> 
                            match tryParseReal a, tryParseReal b with
                            | Some a, Some b -> Q (a, b)
                            | _, _ -> 
                                match tryParseComplex a, tryParseComplex b with
                                | Some a, Some b -> QC (a, b)
                                | _, _ -> NotANumber
                    | _ -> NotANumber
                else
                    match tryParseInteger s with
                    | Some n -> N n
                    | _ ->
                        match tryParseReal s with
                        | Some n -> R n
                        | _ ->
                            match tryParseImaginary s with
                            | Some n -> C (Complex(0.0, n))
                            | _ ->
                                match tryParseComplex s with
                                | Some n -> C n
                                | _ -> NotANumber
            result.Simplify()

    override this.ToString() =
        let formatComplex (c: Complex) =
            if c.Imaginary = 0.0 then c.Real.ToString()
            else
                if c.Real = 0.0 then sprintf "%fi" c.Imaginary
                else 
                    if c.Imaginary > 0.0 then
                        sprintf "%f+%fi" c.Real c.Imaginary
                    else
                        sprintf "%f%fi" c.Real c.Imaginary
        match this with
        | NotANumber -> "NaN"
        | Z -> "0"
        | N n -> n.ToString()
        | NQ (a, b) -> sprintf "%d/%d" a b
        | Q (a, b) -> sprintf "%f/%f" a b
        | R r -> r.ToString()
        | C c -> formatComplex c
        | QC (a, b) -> sprintf "%s/%s" (formatComplex a) (formatComplex b)

    member this.ToLaTexString() =
        let formatComplex (c: Complex) =
            if c.Imaginary = 0.0 then c.Real.ToString()
            else
                if c.Real = 0.0 then sprintf "%fi" c.Imaginary
                else 
                    if c.Imaginary > 0.0 then
                        sprintf "%f+%fi" c.Real c.Imaginary
                    else
                        sprintf "%f%fi" c.Real c.Imaginary
        match this with
        | NotANumber -> "\\text{NaN}"
        | Z -> "0"
        | N n -> n.ToString()
        | NQ (a, b) -> sprintf "$$\\frac{%d}{%d}$$" a b
        | Q (a, b) -> sprintf "$$\\frac{%f}{%f}$$" a b
        | R r -> r.ToString()
        | C c -> formatComplex c
        | QC (a, b) -> sprintf "$$\\frac{%s}{%s}$$" (formatComplex a) (formatComplex b)
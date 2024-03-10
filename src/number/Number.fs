namespace DSE 

open System.Numerics

[<AutoOpen>]
module NumberHelpers =
    let isInf = System.Double.IsInfinity
    let isNaN = System.Double.IsNaN
type Number =
| Z
| N of int
| NQ of int*int
| Q of float*float
| R of float
| C of Complex
| QC of Complex*Complex
with
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
            | Q (a, b) -> isNaN a || isNaN b || b = 0.0
            | R r -> isNaN r
            | C c -> isNaN c.Real || isNaN c.Imaginary
            | QC (a, b) -> 
                isNaN a.Real || isNaN a.Imaginary || isNaN b.Real || isNaN b.Imaginary
            | _ -> false
    member this.IsZero =
        match this.Simplify() with
        | Z -> true
        | N 0 -> true
        | NQ (0, b) when b <> 0 -> true
        | R 0.0 -> true
        | Q (0.0, b) when b <> 0.0 -> true
        | C c when c = Complex.Zero -> true
        | QC (a, b) when a = Complex.Zero && b <> Complex.Zero -> true
        | _ -> false
    member this.IsOne =
        match this.Simplify() with
        | N 1 -> true
        | NQ (a, b) when a = b && b <> 0 -> true
        | Q (a, b) when a = b && b <> 0.0 -> true
        | R 1.0 -> true
        | C c when c = Complex.One -> true
        | QC (a, b) when a = b && b <> Complex.Zero -> true
        | _ -> false
    member this.Simplify() =
        match this with
        | x when x.IsZero -> Z
        | x when x.IsOne -> N 1
        | NQ (a, b) when b = 1 -> N a
        | Q (a, b) when b = 1.0 -> R a
        | C c when c.Imaginary = 0.0 -> R c.Real
        | C c when c.Real = 0.0 && c.Imaginary = 1.0 -> C Complex.ImaginaryOne
        | QC (a, b) when b = Complex.One -> C a
        | _ -> this

    static member (~+) (x: Number) = x
    static member (~-) (x: Number) =
        match x.Simplify() with
        | Z -> Z
        | N n -> N (-n)
        | NQ (a, b) -> NQ (-a, b)
        | Q (a, b) -> Q (-a, b)
        | R r -> R (-r)
        | C c -> C (-c)
        | QC (a, b) -> QC (-a, b)
    static member (+) (x: Number, y: Number) =
        let result = 
            match x.Simplify(), y.Simplify() with
            | Z, x -> x
            | x,  Z -> x

            | N x, N y -> N (x+y)

            | N n, NQ (a, b) -> NQ (n*b+a, b)
            | NQ (a, b), N n -> NQ (a+n*b, b)
            | NQ (a, b), NQ (c, d) -> NQ (a*d+c*b, c*d)

            | N n, Q (a, b) -> Q ((float n)*b+a, b)
            | Q (a, b), N n -> Q (a+(float n)*b, b)
            | NQ (a, b), Q (c, d) -> 
                let a = float a
                let b = float b
                Q (a*d+c*b, c*d)
            | Q (a, b), NQ (c, d) -> 
                let c = float c
                let d = float d
                Q (a*d+c*b, c*d) 
            | Q (a, b), Q (c, d) -> Q (a*d+c*b, c*d)

            // | N x, R y -> R (float x + y)
            // | R x, N y -> R (x+float y)
            // | NQ (a, b), R x | R x, NQ (a, b) ->
            //     let a = float a
            //     let b = float b 
            //     Q (a*x, b*x)
            // | Q (a, b), R x | R x, Q (a, b) -> Q(a*x, b*x)

            // | N x, C c | C c, N x-> C (float x*c)
            // | NQ (a, b), C c | C c, NQ (a, b) ->
            //     let a = float a
            //     let b = float b
            //     QC (a*c, b*c)
            // | Q (a, b), C c | C c, Q (a, b) -> QC (a*c, b*c)
            // | R x, C c | C c, R x -> C (x*c)
            | _ -> Z

        result.Simplify()

namespace DSE 

open System.Numerics

type Number =
| Z
| N of int
| IQ of int*int
| Q of float*float
| R of float
| C of Complex
with
    member this.IsInfinite =
        match this with
        | IQ (a, b) -> b = 0
        | Q (a, b) -> System.Double.IsInfinity a || System.Double.IsInfinity b || b = 0.0
        | R r -> System.Double.IsInfinity r
        | C c -> System.Double.IsInfinity c.Real || System.Double.IsInfinity c.Imaginary
        | _ -> false
    member this.IsNaN =
        match this with
            | Q (a, b) -> System.Double.IsNaN a || System.Double.IsNaN b || b = 0.0
            | R r -> System.Double.IsNaN r
            | C c -> System.Double.IsNaN c.Real || System.Double.IsNaN c.Imaginary
            | _ -> false
    member this.IsZero =
        match this.Simplify() with
        | Z -> true
        | N 0 -> true
        | IQ (0, b) when b <> 0 -> true
        | R 0.0 -> true
        | Q (0.0, b) when b <> 0.0 -> true
        | C c when c = Complex.Zero -> true
        | _ -> false
    member this.IsOne =
        match this.Simplify() with
        | N 1 -> true
        | IQ (a, b) when a = b && b <> 0 -> true
        | Q (a, b) when a = b && b <> 0.0 -> true
        | R 1.0 -> true
        | C c when c = Complex.One -> true
        | _ -> false
    member this.Simplify() =
        match this with
        | x when x.IsZero -> Z
        | x when x.IsOne -> N 1
        | IQ (a, b) when b = 1 -> N a
        | Q (a, b) when b = 1.0 -> N (int a)
        | C c when c.Imaginary = 0.0 -> R c.Real
        | C c when c.Real = 0.0 && c.Imaginary = 1.0 -> C Complex.ImaginaryOne
        | _ -> this
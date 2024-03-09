namespace DSE.Ode.SingleStep.RungeKutta

open System.Numerics
open System.Linq.Expressions

module SchemaHelpers =
    let internal getArrayDimensions (x: 'T[]) =
        if x = null then 0
        else x.Length
    let internal getMatrixDimensions (x: 'T[][]) =
        if x = null then 0, 0
        else
            let rows = x.Length
            let cols = x |> Array.map (fun row -> row |> getArrayDimensions)
            if cols |> Array.length = 0 then
                0, 0
            else
                let maxCols = cols |> Array.max
                if maxCols = 0 then
                    0, 0
                else rows, maxCols

    let getArrayItem i maxLength zeroValue (x: 'T[]) =
        if i < 0 then
            invalidArg "i" "Index must be greater than or equal to zero"
        if maxLength <= 0 then
            invalidArg "maxLength" "Maximum length must be greater than zero"
        if maxLength <= i then
            invalidArg "i" "Index must be less than the maximum length"
        else
            if x = null then zeroValue
            else
                if i >= x.Length then
                    zeroValue
                else
                    x[i]

    let getMatrixItem i j maxRows maxCols zeroValue (x: 'T[][]) =
        if i < 0 then
            invalidArg "i" "Row index must be greater than or equal to zero"
        if j < 0 then
            invalidArg "j" "Column index must be greater than or equal to zero"
        if maxRows <= 0 then
            invalidArg "maxRows" "Maximum number of rows must be greater than zero"
        if maxCols <= 0 then
            invalidArg "maxCols" "Maximum number of columns must be greater than zero"
        if maxRows <= i then
            invalidArg "i" "Row index must be less than the maximum number of rows"
        if maxCols <= j then
            invalidArg "j" "Column index must be less than the maximum number of columns"
        else
            if x = null then zeroValue
            else
                if i >= x.Length then
                    zeroValue
                else
                    let row = x[i]
                    if j >= row.Length then
                        zeroValue
                    else
                        row[j]

type ButcherTableau<'T> =
    { A: 'T[][] // Coefficients for the stages
      B: 'T[]   // Coefficients for the solution
      C: 'T[]   // Coefficients for the time steps
      Steps: int // Number of stages
      Name: string // Name of the Butcher tableau
    }
    with
        member this.Validate() =
            
            let aRows = this.A.Length
            if aRows = 0 then
                invalidArg "A" "A must have at least one row"
            let bs = this.B.Length
            if bs = 0 then
                invalidArg "B" "B must have at least one element"
            let cs = this.C.Length
            if cs = 0 then
                invalidArg "C" "C must have at least one element"
            if aRows <> bs || bs <> cs then
                invalidArg "A" "Number of rows in A must be equal to the length of B which in turn must be equal to the length of c"
            let maxACols = this.A |> Array.maxBy Array.length |> Array.length
            if cs < maxACols then
                invalidArg "C" "Length of C must be greater than or equal to the maximum number of columns in A"
        member this.IsExplicit isZero =
            let isAijZero i j =
                let aRow = this.A[i]
                let aRowLength = aRow.Length
                if j < aRowLength then
                    isZero aRow.[j]
                else
                    true
            { 0 .. this.Steps - 1 } |> Seq.forall (fun i -> { 0 .. i - 1 } |> Seq.forall (fun j -> isAijZero i j))

type EmbeddedButcherTableau<'T> =
    { A: 'T[][] // Coefficients for the stages
      B1: 'T[]   // Coefficients for the solution
      B2: 'T[]   // Coefficients for the solution
      C: 'T[]   // Coefficients for the time steps
      Steps: int // Number of stages
      Name: string // Name of the Butcher tableau
    }

type Coefficient =
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
        | N 0 -> Z
        | IQ (a, b) when b = 1 -> (N a).Simplify()
        | Q (a, b) when b = 1.0 -> (N (int a)).Simplify()
        | R 0.0 -> Z
        | C c when c = Complex.Zero -> Z
        | C c when c = Complex.One -> N 1
        | C c when c.Imaginary = 0.0 -> (R c.Real).Simplify()
        | C c when c.Real = 0.0 && c.Imaginary = 1.0 -> C Complex.ImaginaryOne
        | _ -> this

type CoefficientButcherTableau = ButcherTableau<Coefficient>
type EmbeddedCoefficientButcherTableau = EmbeddedButcherTableau<Coefficient>

module ButcherTableauRegistry =
    type private BT =
    | T of ButcherTableau<Coefficient>
    | ET of EmbeddedButcherTableau<Coefficient>

    let mutable private _builtInButcherTableaus : BT list = List.empty
    
    let registerBuiltInButcherTableau (x: ButcherTableau<Coefficient>) =
        _builtInButcherTableaus <- T x :: _builtInButcherTableaus
    let unregisterBuiltInButcherTableau (x: ButcherTableau<Coefficient>) =
        _builtInButcherTableaus <- _builtInButcherTableaus |> List.filter (fun y -> y <> T x)

    let mutable private _customInButcherTableaus : BT list = List.empty
    [<CompiledName("RegisterCustomButcherTableau")>]
    let register (x: ButcherTableau<Coefficient>) =
        _customInButcherTableaus <- T x :: _customInButcherTableaus
    [<CompiledName("UnRegisterCustomButcherTableau")>]
    let unregister (x: ButcherTableau<Coefficient>) =
        _customInButcherTableaus <- _customInButcherTableaus |> List.filter (fun y -> y <> T x)

module CoefficientHelpers =

    open System.Globalization

    let private english = CultureInfo("en-US")
    let private englishNumber = english.NumberFormat

    let inline private tryParseInt (s: string) = System.Int32.TryParse(s)
    let inline private tryParseReal (s: string) = System.Double.TryParse(s, englishNumber)
    let inline private tryParseImaginary (s: string) = 
        if System.String.IsNullOrWhiteSpace(s) then (false, 0.0)
        else
            let s = s.Trim()
            if s.EndsWith("i") then
                let s = s.Substring(0, s.Length - 1)
                match System.Double.TryParse(s, englishNumber) with
                | true, r -> (true, r)
                | _ -> (false, 0.0)
            else (false, 0.0)
    let inline private tryParseComplex (s: string) = System.Numerics.Complex.TryParse(s, englishNumber)
    let parse (s: string) =
        if System.String.IsNullOrWhiteSpace(s) then
            Z
        else
            let items = s.Trim().Split([|'/'|], 
                System.StringSplitOptions.RemoveEmptyEntries)
            match items.Length with
            | 1 -> 
                match System.Int32.TryParse(items.[0]) with
                | true, 0 -> Z
                | true, n -> if n = 0 then Z else  N n
                | _ -> 
                    match System.Double.TryParse(items.[0], englishNumber) with
                    | true, r -> if r = 0.0 then Z else R r
                    | _ -> Z
            | 2 ->
                match System.Int32.TryParse(items.[0]), System.Int32.TryParse(items.[1]) with
                | (true, a), (true, b) -> 
                    if b = 0 then invalidArg "s" "Denominator cannot be zero"
                    else
                        if a = 0 then Z else IQ (a, b)
                | _ -> 
                    match System.Double.TryParse(items.[0], englishNumber), 
                        System.Double.TryParse(items.[1], englishNumber) with
                    | (true, a), (true, b) ->
                        if b = 0.0 then invalidArg "s" "Denominator cannot be zero" 
                        else 
                            if a = 0.0 then Z else Q (a, b)
                    | _ -> Z
            | _ -> invalidArg "s" "Invalid format"

    let inline toFloat (x: Coefficient) =
        match x with
        | N n -> float n
        | R d -> d
        | Q (a, b) -> a / b
        | IQ (a, b) -> float a / float b
        | _ -> 0.0

    let inline toFloat32 (x: Coefficient) =
        match x with
        | N n -> float32 n
        | R d -> float32 d
        | Q (a, b) -> float32 a / float32 b
        | IQ (a, b) -> float32 a / float32 b
        | _ -> 0.0f

    let toFloatArray (x: Coefficient[]) =
        Array.map toFloat x

    let toFloat32Array (x: Coefficient[]) =
        Array.map toFloat32 x

    let toFloatMatrix (x: Coefficient[][]) =
        Array.map (fun row -> Array.map toFloat row) x

    let toFloat32Matrix (x: Coefficient[][]) =
        Array.map (fun row -> Array.map toFloat32 row) x

module ButcherTableauHelpers =

    open CoefficientHelpers

    let create a b c steps name =
        { A = a
          B = b
          C = c
          Steps = steps
          Name = name }

    let toFloatButcherTableau (x: CoefficientButcherTableau) =
        { A = toFloatMatrix x.A
          B = toFloatArray x.B
          C = toFloatArray x.C
          Steps = x.Steps
          Name = x.Name }

    let toFloat32ButcherTableau (x: CoefficientButcherTableau) =
        { A = toFloat32Matrix x.A
          B = toFloat32Array x.B
          C = toFloat32Array x.C
          Steps = x.Steps
          Name = x.Name }

module EmbeddedButcherTableauHelpers =

    open CoefficientHelpers 

    let create  a b1 b2 c steps name =
        { A = a
          B1 = b1
          B2 = b2
          C = c
          Steps = steps
          Name = name }

    let toFloatEmbeddedButcherTableau (x: EmbeddedCoefficientButcherTableau) =
        { A = toFloatMatrix x.A
          B1 = toFloatArray x.B1
          B2 = toFloatArray x.B2
          C = toFloatArray x.C
          Steps = x.Steps
          Name = x.Name }

    let toFloat32EmbeddedButcherTableau (x: EmbeddedCoefficientButcherTableau) =
        { A = toFloat32Matrix x.A
          B1 = toFloat32Array x.B1
          B2 = toFloat32Array x.B2
          C = toFloat32Array x.C
          Steps = x.Steps
          Name = x.Name }
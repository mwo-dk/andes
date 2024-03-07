namespace DSE.Ode.SingleStep.RungeKutta

open System.Linq.Expressions

type ButcherTableau<'T> =
    { A: 'T[][] // Coefficients for the stages
      B: 'T[]   // Coefficients for the solution
      C: 'T[]   // Coefficients for the time steps
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
        member this.Steps = this.C.Length
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
      Name: string // Name of the Butcher tableau
    }
    with
        member this.Steps = this.C.Length

type Coefficient =
| Z
| N of int
| R of float
| Q of float*float
| IQ of int*int
with
    member this.IsInfinite =
        match this with
        | R r -> System.Double.IsInfinity r
        | Q (a, b) -> System.Double.IsInfinity a || System.Double.IsInfinity b || b = 0.0
        | IQ (a, b) -> b = 0
        | _ -> false
    member this.IsZero =
        match this with
        | Z -> true
        | N n when n = 0 -> true
        | R r when r = 0.0 -> true
        | Q (a, b) when a = 0.0 && b <> 0.0 -> true
        | IQ (a, b) when a = 0 && b <> 0 -> true
        | _ -> false
    member this.Simplify() =
        match this with
        | N 0 -> Z
        | R 0.0 -> Z
        | Q (a, b) when b = 1.0 -> (N (int a)).Simplify()
        | IQ (a, b) when b = 1 -> (N a).Simplify()
        | _ -> this

//type CoefficientButcherTableau = ButcherTableau<Coefficient>
//with
//    member this.IsInfinite =
//        this.A |> Array.exists (fun row -> row |> Array.exists (fun x -> x.IsInfinite)) ||
//        this.B |> Array.exists (fun x -> x.IsInfinite) ||
//        this.C |> Array.exists (fun x -> x.IsInfinite)
//    member this.IsZero =
//        this.A |> Array.exists (fun row -> row |> Array.exists (fun x -> x.IsZero)) ||
//        this.B |> Array.exists (fun x -> x.IsZero) ||
//        this.C |> Array.exists (fun x -> x.IsZero)

module ButcherTableaus =
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

    let parse (s: string) =
        if System.String.IsNullOrWhiteSpace(s) then
            nullArg "s"
        else
            let items = s.Trim().Split([|'/'|], 
                System.StringSplitOptions.RemoveEmptyEntries)
            match items.Length with
            | 1 -> 
                match System.Int32.TryParse(items.[0]) with
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

    let create (a: Coefficient[][]) (b: Coefficient[]) (c: Coefficient[]) (name: string) =
        { A = a
          B = b
          C = c
          Name = name }

    let toFloatButcherTableau (x: ButcherTableau<Coefficient>) =
        { A = toFloatMatrix x.A
          B = toFloatArray x.B
          C = toFloatArray x.C
          Name = x.Name }

    let toFloat32ButcherTableau (x: ButcherTableau<Coefficient>) =
        { A = toFloat32Matrix x.A
          B = toFloat32Array x.B
          C = toFloat32Array x.C
          Name = x.Name }

module EmbeddedButcherTableauHelpers =

    open CoefficientHelpers 

    let create (a: Coefficient[][]) (b1: Coefficient[]) (b2: Coefficient[]) (c: Coefficient[]) (name: string) =
        { A = a
          B1 = b1
          B2 = b2
          C = c
          Name = name }

    let toFloatEmbeddedButcherTableau (x: EmbeddedButcherTableau<Coefficient>) =
        { A = toFloatMatrix x.A
          B1 = toFloatArray x.B1
          B2 = toFloatArray x.B2
          C = toFloatArray x.C
          Name = x.Name }

    let toFloat32EmbeddedButcherTableau (x: EmbeddedButcherTableau<Coefficient>) =
        { A = toFloat32Matrix x.A
          B1 = toFloat32Array x.B1
          B2 = toFloat32Array x.B2
          C = toFloat32Array x.C
          Name = x.Name }
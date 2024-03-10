namespace DSE.Ode.SingleStep.RungeKutta

open System.Numerics
open DSE
open System.Linq.Expressions

[<AutoOpen>]
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

type ButcherTableau<'T when 'T: equality> =
    { A: 'T[][] // Coefficients for the stages
      B: 'T[]   // Coefficients for the solution
      C: 'T[]   // Coefficients for the time steps
      Steps: int // Number of stages
      Name: string // Name of the Butcher tableau
    }
    with
        member this.Validate() =
            let (aRows, aCols) = this.A |> getMatrixDimensions
            if this.Steps < aRows || this.Steps < aCols then
                invalidArg "A" "A must not be larger than number of steps"
            let bs = this.B |> getArrayDimensions
            if this.Steps < bs then
                invalidArg "B" "B must not be larger than number of steps"
            let cs = this.C |> getArrayDimensions
            if this.Steps < cs then
                invalidArg "C" "C must not be larger than number of steps"
            this
        member this.IsExplicit zeroValue =
            let n = this.Steps
            {0..n-1} |>
                Seq.fold (fun ok i -> 
                    {i..n-1} |> Seq.fold (fun ok j -> ok && zeroValue = (getMatrixItem  i j n n zeroValue this.A)) ok) true

type EmbeddedButcherTableau<'T when 'T: equality> =
    { A: 'T[][] // Coefficients for the stages
      B1: 'T[]   // Coefficients for the solution
      B2: 'T[]   // Coefficients for the solution
      C: 'T[]   // Coefficients for the time steps
      Steps: int // Number of stages
      Name: string // Name of the Butcher tableau
    }
with
    member this.Validate() =
        let (aRows, aCols) = this.A |> getMatrixDimensions
        if this.Steps < aRows || this.Steps < aCols then
            invalidArg "A" "A must not be larger than number of steps"
        let b1s = this.B1 |> getArrayDimensions
        if this.Steps < b1s then
            invalidArg "B1" "B1 must not be larger than number of steps"
        let b2s = this.B2 |> getArrayDimensions
        if this.Steps < b2s then
            invalidArg "B2" "B2 must not be larger than number of steps"
        let cs = this.C |> getArrayDimensions
        if this.Steps < cs then
           invalidArg "C" "C must not be larger than number of steps"
        this
    member this.IsExplicit zeroValue =
        let n = this.Steps
        {0..n-1} |>
            Seq.fold (fun ok i -> 
            {i..n-1} |> Seq.fold (fun ok j -> ok && zeroValue = (getMatrixItem  i j n n zeroValue this.A)) ok) true

type Coefficient = Number
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
                        if a = 0 then Z else NQ (a, b)
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
        | NQ (a, b) -> float a / float b
        | _ -> 0.0

    let inline toFloat32 (x: Coefficient) =
        match x with
        | N n -> float32 n
        | R d -> float32 d
        | Q (a, b) -> float32 a / float32 b
        | NQ (a, b) -> float32 a / float32 b
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
namespace Andes.Ode.SingleStep.RungeKutta

open Andes
open System.Runtime.CompilerServices

[<AutoOpen>]
module SchemaHelpers =
    let internal getArrayDimension (x: 'T[]) =
        if x = null then 0
        else x |> Array.length
    let internal getMatrixDimensions (x: 'T[][]) =
        if x |> getArrayDimension = 0 then 0, 0
        else
            let rows = x.Length
            let cols = x |> Array.map (fun row -> row |> getArrayDimension)
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

    let validateA steps A =
        let (aRows, aCols) = A |> getMatrixDimensions
        if steps < aRows || steps < aCols then
            invalidArg "A" "A must not be larger than number of steps"
    let validate steps B name =
        let bs = B |> getArrayDimension
        if steps < bs then
            invalidArg name $"{name} must not be larger than number of steps"

type ButcherTableau<'T when 'T: equality> =
    { A: 'T[][] // Coefficients for the stages
      B: 'T[]   // Coefficients for the solution
      C: 'T[]   // Coefficients for the time steps
      Steps: int // Number of stages
      Name: string // Name of the Butcher tableau
    }
    with
        member private this.ValidateLazy =
            lazy (
                validateA this.Steps this.A
                validate this.Steps this.B "B"
                validate this.Steps this.C "C"
                this
            )
        member this.Validate() = this.ValidateLazy.Force()
        member this.IsExplicit zeroValue =
            let N = this.Steps
            {0..N-1} |>
                Seq.fold (fun ok i -> 
                    {i..N-1} |> Seq.fold (fun ok j -> ok && zeroValue = (getMatrixItem  i j N N zeroValue this.A)) ok) true

type EmbeddedButcherTableau<'T when 'T: equality> =
    { A: 'T[][] // Coefficients for the stages
      B1: 'T[]   // Coefficients for the solution
      B2: 'T[]   // Coefficients for the solution
      C: 'T[]   // Coefficients for the time steps
      B1Order: 'T
      B2Order: 'T
      Steps: int // Number of stages
      Name: string // Name of the Butcher tableau
    }
with
    member private this.ValidateLazy =
            lazy (
                validateA this.Steps this.A
                validate this.Steps this.B1 "B1"
                validate this.Steps this.B2 "B2"
                validate this.Steps this.C "C"
                this
            )
    member this.Validate() = this.ValidateLazy.Force()
    member this.IsExplicit zeroValue =
        let n = this.Steps
        {0..n-1} |>
            Seq.fold (fun ok i -> 
            {i..n-1} |> Seq.fold (fun ok j -> ok && zeroValue = (getMatrixItem  i j n n zeroValue this.A)) ok) true

type FloatButcherTableau = ButcherTableau<float>
type FloatEmbeddedButcherTableau = EmbeddedButcherTableau<float>

type Float32ButcherTableau = ButcherTableau<float32>
type Float32EmbeddedButcherTableau = EmbeddedButcherTableau<float32>

type Coefficient = Number
type CoefficientButcherTableau = ButcherTableau<Coefficient>
type EmbeddedCoefficientButcherTableau = EmbeddedButcherTableau<Coefficient>

[<Extension>]
type TableauExtensions =
    [<Extension>]
    static member inline IsExplicit(tableau: FloatButcherTableau) = tableau.IsExplicit 0.0
    [<Extension>]
    static member inline IsExplicit(tableau: Float32ButcherTableau) = tableau.IsExplicit 0.0f
    [<Extension>]
    static member inline IsExplicit(tableau: CoefficientButcherTableau) = tableau.IsExplicit Z

    [<Extension>]
    static member inline IsExplicit(tableau: EmbeddedCoefficientButcherTableau) = tableau.IsExplicit Z
    [<Extension>]
    static member inline IsExplicit(tableau: FloatEmbeddedButcherTableau) = tableau.IsExplicit 0.0
    [<Extension>]
    static member inline IsExplicit(tableau: Float32EmbeddedButcherTableau) = tableau.IsExplicit 0.0f

module ButcherTableauRegistry =
    type Subscriber = unit -> unit
    let mutable private newId = 1
    let mutable private _subscribers : (Subscriber*int) list = List.empty
    [<CompiledName("SubscribeToTableauChanges")>]
    let subscribeToTableauChanges (listener: System.Action) =
        let id = newId
        newId <- newId + 1
        let subscriber : Subscriber = (fun () -> listener.Invoke())
        _subscribers <- (subscriber, id) :: _subscribers
        id
    [<CompiledName("UnsubscribeFromTableauChanges")>]
    let unsubscribeFromTableauChanges (id: int) =
        _subscribers <- _subscribers |> List.filter (fun (_, x) -> x <> id)
    let private notifyListeners () =
        _subscribers |> 
        List.iter (fun (subscriber, _) -> 
            try
                subscriber()
            with
            | _ -> ())

    type private BT =
    | T of CoefficientButcherTableau
    | ET of EmbeddedCoefficientButcherTableau

    let mutable private _builtInButcherTableaus : BT list = List.empty
    let registerBuiltInButcherTableau (x: CoefficientButcherTableau) =
        _builtInButcherTableaus <- T x :: _builtInButcherTableaus
        notifyListeners()
    let unregisterBuiltInButcherTableau (x: CoefficientButcherTableau) =
        _builtInButcherTableaus <- _builtInButcherTableaus |> List.filter (fun y -> y <> T x)
        notifyListeners()
    let registerBuiltInEmbeddedButcherTableau (x: EmbeddedCoefficientButcherTableau) =
        _builtInButcherTableaus <- ET x :: _builtInButcherTableaus
        notifyListeners()
    let unregisterBuiltInEmbeddedButcherTableau (x: EmbeddedCoefficientButcherTableau) =
        _builtInButcherTableaus <- _builtInButcherTableaus |> List.filter (fun y -> y <> ET x)
        notifyListeners()

    let mutable private _customButcherTableaus : BT list = List.empty
    [<CompiledName("RegisterCustomButcherTableau")>]
    let registerCustomButcherTableau (x: CoefficientButcherTableau) =
        _customButcherTableaus <- T x :: _customButcherTableaus
        notifyListeners()
    [<CompiledName("UnRegisterCustomButcherTableau")>]
    let unRegisterCustomButcherTableau (x: CoefficientButcherTableau) =
        _customButcherTableaus <- _customButcherTableaus |> List.filter (fun y -> y <> T x)
        notifyListeners()
    [<CompiledName("RegisterCustomEmbeddedButcherTableau")>]
    let registerCustomEmbeddedButcherTableau (x: EmbeddedCoefficientButcherTableau) =
        _customButcherTableaus <- ET x :: _customButcherTableaus
        notifyListeners()
    [<CompiledName("UnRegisterCustomEmbeddedButcherTableau")>]
    let unRegisterCustomEmbeddedButcherTableau (x: EmbeddedCoefficientButcherTableau) =
        _customButcherTableaus <- _customButcherTableaus |> List.filter (fun y -> y <> ET x)
        notifyListeners()

    type ButcherTableaus = {
        BuiltInButcherTableaus: CoefficientButcherTableau list
        EmbeddedBuiltInButcherTableaus: EmbeddedCoefficientButcherTableau list 
        CustomButcherTableaus: CoefficientButcherTableau list 
        EmbeddedCustomButcherTableaus: EmbeddedCoefficientButcherTableau list 
    }

    [<CompiledName("GetButcherTableaus")>]
    let getButcherTableaus () = {
        BuiltInButcherTableaus = _builtInButcherTableaus |> List.choose (function | T x -> Some x | _ -> None)
        EmbeddedBuiltInButcherTableaus = _builtInButcherTableaus |> List.choose (function | ET x -> Some x | _ -> None)
        CustomButcherTableaus = _customButcherTableaus |> List.choose (function | T x -> Some x | _ -> None)
        EmbeddedCustomButcherTableaus = _customButcherTableaus |> List.choose (function | ET x -> Some x | _ -> None)
    }

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

    let internal createAndRegisterBuiltIn a b c steps name =
        let x = create a b c steps name
        ButcherTableauRegistry.registerBuiltInButcherTableau x
        x

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

    let create  a b1 b2 c b1Order b2Order steps name =
        { A = a
          B1 = b1
          B2 = b2
          C = c
          B1Order = b1Order
          B2Order = b2Order
          Steps = steps
          Name = name }

    let internal createAndRegisterBuiltIn a b1 b2 c b1Order b2Order steps name =
        let x = create a b1 b2 c b1Order b2Order steps name
        ButcherTableauRegistry.registerBuiltInEmbeddedButcherTableau x
        x

    let toFloatEmbeddedButcherTableau (x: EmbeddedCoefficientButcherTableau) =
        { A = toFloatMatrix x.A
          B1 = toFloatArray x.B1
          B2 = toFloatArray x.B2
          C = toFloatArray x.C
          B1Order = toFloat x.B1Order
          B2Order = toFloat x.B2Order
          Steps = x.Steps
          Name = x.Name }

    let toFloat32EmbeddedButcherTableau (x: EmbeddedCoefficientButcherTableau) =
        { A = toFloat32Matrix x.A
          B1 = toFloat32Array x.B1
          B2 = toFloat32Array x.B2
          C = toFloat32Array x.C
          B1Order = toFloat32 x.B1Order
          B2Order = toFloat32 x.B2Order
          Steps = x.Steps
          Name = x.Name }

module Tableaus =

    let _1 = N 1
    let _M1 = N -1
    let _1_2 = NQ (1, 2)
    let _1_3 = NQ (1, 3)
    let _M1_3 = NQ (-1, 3)
    let _1_4 = NQ (1, 4)
    let _1_6 = NQ (1, 6)
    let _1_8 = NQ (1, 8)
    let _2_3 = NQ (2, 3)
    let _2_9 = NQ (2, 9)
    let _3_4 = NQ (3, 4)
    let _3_8 = NQ (3, 8)
    let _4_9 = NQ (4, 9)
    let _5_12 = NQ (5, 12)
    let _8_15 = NQ (8, 15)
    let _2 = N 2
    let forwardEuler =
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z|]|]
            [|_1|]
            [|Z|]
            1
            "Forward Euler"

    let explicitMidpoint =
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z|];[|_1_2;Z|]|]
            [|Z;_1|]
            [|Z;_1_2|]
            2
            "Explicit midpoint method"

    let heun =
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z|];[|_1;Z|]|]
            [|_1_2;_1_2|]
            [|Z;_1|]
            2
            "Heun's method"

    let ralston =
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z|];[|_2_3;Z|]|]
            [|_1_4;_3_4;|]
            [|Z;_2_3|]
            2
            "Ralston's method"

    let kutta3 =
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z;Z|];[|_1_2;Z;Z|];[|_M1;_2;Z|]|]
            [|_1_6;_2_3;_1_6|]
            [|Z;_1_2;_1|]
            3
            "Kutta's third-order method"

    let heun3 = 
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z;Z|];[|_1_3;Z;Z|];[|Z;_2_3;Z|]|]
            [|_1_4;Z;_3_4|]
            [|Z;_1_3;_2_3|]
            3
            "Heun's third-order method"

    let vanDerHouwenWray3 =
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z;Z|];[|_8_15;Z;Z|];[|_1_4;_5_12;Z|]|]
            [|_1_4;Z;_3_4|]
            [|Z;_8_15;_2_3|]
            3
            "van der Houwen-Wray third-order method"

    let ralston3 = 
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z;Z|];[|_1_2;Z;Z|];[|Z;_3_4;Z|]|]
            [|_2_9;_1_3;_4_9|]
            [|Z;_1_2;_3_4|]
            3
            "Ralston's third-order method"

    let strongStabilityPreservingRK3 =
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z;Z|];[|_1;Z;Z|];[|_1_4;_1_4;Z|]|]
            [|_1_6;_1_6;_2_3|]
            [|Z;_1;_1_2|]
            3
            "Strong stability preserving Runge-Kutta third-order method"

    let rk4 = 
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z;Z;Z|];[|_1_2;Z;Z;Z|];[|Z;_1_2;Z;Z|];[|Z;Z;_1;Z|]|]
            [|_1_6;_1_3;_1_3;_1_6|]
            [|Z;_1_2;_1_2;_1|]
            4
            "Classical Runge-Kutta method"

    let threeEighthsRule =
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z;Z;Z|];[|_1_3;Z;Z;Z|];[|_M1_3;_1;Z;Z|];[|_1;_M1;_1;Z|]|]
            [|_1_8;_3_8;_3_8;_1_8|]
            [|Z;_1_3;_2_3;_1|]
            4
            "Three-eighths rule"

    let ralston4 =
        ButcherTableauHelpers.createAndRegisterBuiltIn
            [|[|Z;Z;Z;Z|];[|R 0.4;Z;Z;Z|];[|R 0.29697761;R 0.15875964;Z;Z|];[|R 0.2181004;R -3.05096516;R 3.83286476;Z|]|]
            [|R 0.17476028;R -0.55148066;R 1.2055356;R 0.17118478|]
            [|Z;R 0.4;R 0.45573725;_1|]
            4
            "Ralston's fourth-order method"
    //let bogackiShampine =
    //    ButcherTableauHelpers.create
    //        [|[|Z;Z;Z;Z|];[|_1_2;Z;Z;Z|];[|Z;_3_4;Z;Z|];[|_2_9;_1_3;_4_9;Z|]|]
    //        [|_2_9;_1_3;_4_9;Z|]
    //        [|Z;_1_2;_3_4;_1|]
    //        4
    //        "Bogacki-Shampine method"
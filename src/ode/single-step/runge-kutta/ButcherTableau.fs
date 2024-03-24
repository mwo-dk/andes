namespace Andes.Ode.SingleStep.RungeKutta

open System
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
      Order: 'T
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

    type BT =
    | T of CoefficientButcherTableau
    | ET of EmbeddedCoefficientButcherTableau

    type ButcherTableauRegistration = {
        Id: Guid
        IsBuiltIn: bool
        Tableau: BT
    }
    with
        member x.IsEmbedded =
            match x.Tableau with
            | ET _ -> true
            | _ -> false
        member x.Name = 
            match x.Tableau with
            | T x -> x.Name
            | ET x -> x.Name
        member x.Steps = 
            match x.Tableau with
            | T x -> x.Steps
            | ET x -> x.Steps
        member x.A =
            match x.Tableau with
            | T x -> x.A
            | ET x -> x.A
        member x.B =
            match x.Tableau with
            | T x -> x.B
            | ET x -> Array.empty
        member x.B1 =
            match x.Tableau with
            | ET x -> x.B1
            | _ -> Array.empty
        member x.B2 =
            match x.Tableau with
            | ET x -> x.B2
            | _ -> Array.empty
        member x.C =
            match x.Tableau with
            | T x -> x.C
            | ET x -> x.C
        member x.Order =
            match x.Tableau with
            | T x -> x.Order
            | ET x -> Z
        member x.B1Order =
            match x.Tableau with
            | ET x -> x.B1Order
            | _ -> Z
        member x.B2Order =
            match x.Tableau with
            | ET x -> x.B2Order
            | _ -> Z
        member x.IsExplicit =
            match x.Tableau with
            | T x -> x.IsExplicit Z
            | ET x -> x.IsExplicit Z

    type ButcherTableauRegistrationList = ButcherTableauRegistration list

    let mutable private _butcherTables : ButcherTableauRegistrationList = List.empty

    let internal registerButcherTableau id isBuiltIn x =
        _butcherTables <- {Id = id; IsBuiltIn = isBuiltIn; Tableau = T x} :: _butcherTables
        notifyListeners()
    let internal registerEmbeddedButcherTableau id isBuiltIn x =
        _butcherTables <- {Id = id; IsBuiltIn = isBuiltIn; Tableau = ET x} :: _butcherTables
        notifyListeners()

    [<CompiledName("RegisterCustomButcherTableau")>]
    let registerCustomButcherTableau x =
        let id = Guid.NewGuid()
        match x with
        | T x -> registerButcherTableau id false x
        | ET x -> registerEmbeddedButcherTableau id false x
        id

    [<CompiledName("TryGetButcherTableau")>]
    let tryGetButcherTableau id =
        _butcherTables |> List.tryFind (fun x -> x.Id = id) |> Option.map (fun x -> x.Tableau)

    [<CompiledName("GetButcherTableaus")>]
    let getButcherTableaus () = _butcherTables

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

    let create a b c order steps name =
        { A = a
          B = b
          C = c
          Order = order
          Steps = steps
          Name = name }

    let internal createAndRegister id a b c order steps name =
        let x = create a b c order steps name
        ButcherTableauRegistry.registerButcherTableau id true x

    let toFloatButcherTableau (x: CoefficientButcherTableau) =
        { A = toFloatMatrix x.A
          B = toFloatArray x.B
          C = toFloatArray x.C
          Order = toFloat x.Order
          Steps = x.Steps
          Name = x.Name }

    let toFloat32ButcherTableau (x: CoefficientButcherTableau) =
        { A = toFloat32Matrix x.A
          B = toFloat32Array x.B
          C = toFloat32Array x.C
          Order = toFloat32 x.Order
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

    let internal createAndRegister id a b1 b2 c b1Order b2Order steps name =
        let x = create a b1 b2 c b1Order b2Order steps name
        ButcherTableauRegistry.registerEmbeddedButcherTableau id true x
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
    let _3 = N 3
    let _4 = N 4

    let private forwarEulerId = Guid("{EE2C38AB-F4D4-429D-8BA1-50DA5BB4A44B}")
    let forwardEuler =
        ButcherTableauHelpers.createAndRegister
            forwarEulerId
            [|[|Z|]|]
            [|_1|]
            [|Z|]
            _1
            1
            "Forward Euler"

    let private explicitMidpointId = Guid("{BEC2D821-4104-46FB-B772-A7128ED6326E}")
    let explicitMidpoint =
        ButcherTableauHelpers.createAndRegister
            explicitMidpointId
            [|[|Z;Z|];[|_1_2;Z|]|]
            [|Z;_1|]
            [|Z;_1_2|]
            _2
            2
            "Explicit midpoint method"

    let private heunId = Guid("{E312917E-E9B5-4190-85DE-AAE6A2AAFB98}")
    let heun =
        ButcherTableauHelpers.createAndRegister
            heunId
            [|[|Z;Z|];[|_1;Z|]|]
            [|_1_2;_1_2|]
            [|Z;_1|]
            _2
            2
            "Heun's method"

    let private ralstonId = Guid("{34D7E6BE-1826-44A8-9A2A-F4A9091F7617}")
    let ralston =
        ButcherTableauHelpers.createAndRegister
            ralstonId
            [|[|Z;Z|];[|_2_3;Z|]|]
            [|_1_4;_3_4;|]
            [|Z;_2_3|]
            _2
            2
            "Ralston's method"

    let private kutta3Id = Guid("{22845F16-80E5-4884-966A-C79849E118C4}")
    let kutta3 =
        ButcherTableauHelpers.createAndRegister
            kutta3Id
            [|[|Z;Z;Z|];[|_1_2;Z;Z|];[|_M1;_2;Z|]|]
            [|_1_6;_2_3;_1_6|]
            [|Z;_1_2;_1|]
            _3
            3
            "Kutta's third-order method"

    let private heun3Id = Guid("{A294241A-7EB4-41EF-8776-57EFAAB87A38}")
    let heun3 = 
        ButcherTableauHelpers.createAndRegister
            heun3Id
            [|[|Z;Z;Z|];[|_1_3;Z;Z|];[|Z;_2_3;Z|]|]
            [|_1_4;Z;_3_4|]
            [|Z;_1_3;_2_3|]
            _3
            3
            "Heun's third-order method"

    let private vanderHouwenWray3Id = Guid("{992BF048-367B-4482-9621-A25C46D3A94E}")
    let vanDerHouwenWray3 =
        ButcherTableauHelpers.createAndRegister
            vanderHouwenWray3Id
            [|[|Z;Z;Z|];[|_8_15;Z;Z|];[|_1_4;_5_12;Z|]|]
            [|_1_4;Z;_3_4|]
            [|Z;_8_15;_2_3|]
            _3
            3
            "van der Houwen-Wray third-order method"

    let private ralston3Id = Guid("{26376934-7EC5-4CCD-B453-E971E570EF4E}")
    let ralston3 = 
        ButcherTableauHelpers.createAndRegister
            ralston3Id
            [|[|Z;Z;Z|];[|_1_2;Z;Z|];[|Z;_3_4;Z|]|]
            [|_2_9;_1_3;_4_9|]
            [|Z;_1_2;_3_4|]
            _3
            3
            "Ralston's third-order method"

    let private strongStabilityPreservingRK3Id = Guid("{C1424E94-F0A4-4CE3-9EEE-FA63CF7F21AB}")
    let strongStabilityPreservingRK3 =
        ButcherTableauHelpers.createAndRegister
            strongStabilityPreservingRK3Id
            [|[|Z;Z;Z|];[|_1;Z;Z|];[|_1_4;_1_4;Z|]|]
            [|_1_6;_1_6;_2_3|]
            [|Z;_1;_1_2|]
            _3
            3
            "Strong stability preserving Runge-Kutta third-order method"

    let rk4Id = Guid("{55DEFACE-B198-477D-BB3E-2E3EC45DA520}")
    let rk4 = 
        ButcherTableauHelpers.createAndRegister
            rk4Id
            [|[|Z;Z;Z;Z|];[|_1_2;Z;Z;Z|];[|Z;_1_2;Z;Z|];[|Z;Z;_1;Z|]|]
            [|_1_6;_1_3;_1_3;_1_6|]
            [|Z;_1_2;_1_2;_1|]
            _4
            4
            "Classical Runge-Kutta method"

    let private threeEighthsRuleId = Guid("{9F7DA2A1-32FE-454D-9621-C2C2275AC7F9}")
    let threeEighthsRule =
        ButcherTableauHelpers.createAndRegister
            threeEighthsRuleId
            [|[|Z;Z;Z;Z|];[|_1_3;Z;Z;Z|];[|_M1_3;_1;Z;Z|];[|_1;_M1;_1;Z|]|]
            [|_1_8;_3_8;_3_8;_1_8|]
            [|Z;_1_3;_2_3;_1|]
            _4
            4
            "Three-eighths rule"

    let private ralston4Id = Guid("{40212268-8598-49F1-ABDB-DAFA059AE469}")
    let ralston4 =
        ButcherTableauHelpers.createAndRegister
            ralston4Id
            [|[|Z;Z;Z;Z|];[|R 0.4;Z;Z;Z|];[|R 0.29697761;R 0.15875964;Z;Z|];[|R 0.2181004;R -3.05096516;R 3.83286476;Z|]|]
            [|R 0.17476028;R -0.55148066;R 1.2055356;R 0.17118478|]
            [|Z;R 0.4;R 0.45573725;_1|]
            _4
            4
            "Ralston's fourth-order method"

    let private heunEulerId = Guid("31fda9be-9fc4-40cc-ae88-4620b3ad165c")
    let heunEuler =
        EmbeddedButcherTableauHelpers.createAndRegister
            heunEulerId
            [|[|Z;Z;|];[|_1;Z|]|]
            [|_1_2;_1_2|]
            [|Z;_1|]
            [|Z;_1|]
            _2
            _1
            2
            "Heun-Euler method"

    let private fehlbergRK1Id = Guid("14bd18e7-d44c-4a48-93ec-253f5384ee53")
    let fehlbergRK1 =
        EmbeddedButcherTableauHelpers.createAndRegister
            fehlbergRK1Id
            [|[|Z;Z;Z|];[|_1_2;Z;Z|];[|NQ (1,256);NQ (255,256);Z|]|]
            [|NQ(1,512);NQ(255,256);NQ(1,512)|]
            [|NQ(1,256);NQ(255,256);Z|]
            [|Z;_1_2;_1|]
            _2
            _1
            3
            "Fehlberg's Runge-Kutta first-order method"

    //let bogackiShampine =
    //    ButcherTableauHelpers.create
    //        [|[|Z;Z;Z;Z|];[|_1_2;Z;Z;Z|];[|Z;_3_4;Z;Z|];[|_2_9;_1_3;_4_9;Z|]|]
    //        [|_2_9;_1_3;_4_9;Z|]
    //        [|Z;_1_2;_3_4;_1|]
    //        4
    //        "Bogacki-Shampine method"
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
    let _1_9 = NQ (1, 9)
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
    let _5 = N 5
    let _6 = N 6

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
            [|
                [|Z;Z;Z|]
                [|_1_2;Z;Z|]
                [|_M1;_2;Z|]
            |]
            [|_1_6;_2_3;_1_6|]
            [|Z;_1_2;_1|]
            _3
            3
            "Kutta's third-order method"

    let private heun3Id = Guid("{A294241A-7EB4-41EF-8776-57EFAAB87A38}")
    let heun3 = 
        ButcherTableauHelpers.createAndRegister
            heun3Id
            [|
                [|Z;Z;Z|]
                [|_1_3;Z;Z|]
                [|Z;_2_3;Z|]
            |]
            [|_1_4;Z;_3_4|]
            [|Z;_1_3;_2_3|]
            _3
            3
            "Heun's third-order method"

    let private vanderHouwenWray3Id = Guid("{992BF048-367B-4482-9621-A25C46D3A94E}")
    let vanDerHouwenWray3 =
        ButcherTableauHelpers.createAndRegister
            vanderHouwenWray3Id
            [|
                [|Z;Z;Z|]
                [|_8_15;Z;Z|]
                [|_1_4;_5_12;Z|]
            |]
            [|_1_4;Z;_3_4|]
            [|Z;_8_15;_2_3|]
            _3
            3
            "van der Houwen-Wray third-order method"

    let private ralston3Id = Guid("{26376934-7EC5-4CCD-B453-E971E570EF4E}")
    let ralston3 = 
        ButcherTableauHelpers.createAndRegister
            ralston3Id
            [|
                [|Z;Z;Z|];
                [|_1_2;Z;Z|];
                [|Z;_3_4;Z|]
            |]
            [|_2_9;_1_3;_4_9|]
            [|Z;_1_2;_3_4|]
            _3
            3
            "Ralston's third-order method"

    let private strongStabilityPreservingRK3Id = Guid("{C1424E94-F0A4-4CE3-9EEE-FA63CF7F21AB}")
    let strongStabilityPreservingRK3 =
        ButcherTableauHelpers.createAndRegister
            strongStabilityPreservingRK3Id
            [|
                [|Z;Z;Z|]
                [|_1;Z;Z|]
                [|_1_4;_1_4;Z|]
            |]
            [|_1_6;_1_6;_2_3|]
            [|Z;_1;_1_2|]
            _3
            3
            "Strong stability preserving Runge-Kutta third-order method"

    let rk4Id = Guid("{55DEFACE-B198-477D-BB3E-2E3EC45DA520}")
    let rk4 = 
        ButcherTableauHelpers.createAndRegister
            rk4Id
            [|
                [|Z;Z;Z;Z|]
                [|_1_2;Z;Z;Z|]
                [|Z;_1_2;Z;Z|]
                [|Z;Z;_1;Z|]
            |]
            [|_1_6;_1_3;_1_3;_1_6|]
            [|Z;_1_2;_1_2;_1|]
            _4
            4
            "Classical Runge-Kutta method"

    let private threeEighthsRuleId = Guid("{9F7DA2A1-32FE-454D-9621-C2C2275AC7F9}")
    let threeEighthsRule =
        ButcherTableauHelpers.createAndRegister
            threeEighthsRuleId
            [|
                [|Z;Z;Z;Z|]
                [|_1_3;Z;Z;Z|]
                [|_M1_3;_1;Z;Z|]
                [|_1;_M1;_1;Z|]
            |]
            [|_1_8;_3_8;_3_8;_1_8|]
            [|Z;_1_3;_2_3;_1|]
            _4
            4
            "Three-eighths rule"

    let private ralston4Id = Guid("{40212268-8598-49F1-ABDB-DAFA059AE469}")
    let ralston4 =
        ButcherTableauHelpers.createAndRegister
            ralston4Id
            [|
                [|Z;Z;Z;Z|]
                [|R 0.4;Z;Z;Z|]
                [|R 0.29697761;R 0.15875964;Z;Z|]
                [|R 0.2181004;R -3.05096516;R 3.83286476;Z|]
            |]
            [|R 0.17476028;R -0.55148066;R 1.2055356;R 0.17118478|]
            [|Z;R 0.4;R 0.45573725;_1|]
            _4
            4
            "Ralston's fourth-order method"

    let private heunEulerId = Guid("31fda9be-9fc4-40cc-ae88-4620b3ad165c")
    let heunEuler =
        EmbeddedButcherTableauHelpers.createAndRegister
            heunEulerId
            [|
                [|Z;Z;|]
                [|_1;Z|]
            |]
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
            [|
                [|Z;Z;Z|]
                [|_1_2;Z;Z|]
                [|NQ (1,256);NQ (255,256);Z|]
            |]
            [|NQ(1,512);NQ(255,256);NQ(1,512)|]
            [|NQ(1,256);NQ(255,256);Z|]
            [|Z;_1_2;_1|]
            _2
            _1
            3
            "Fehlberg's Runge-Kutta first-order method"

    let bogackiShampineId = Guid("c41014c9-5abf-4d68-98f7-17560416eefe")
    let bogackiShampine =
        EmbeddedButcherTableauHelpers.createAndRegister
            bogackiShampineId
            [|
                [|Z;Z;Z;Z|]
                [|_1_2;Z;Z;Z|]
                [|Z;_3_4;Z;Z|]
                [|_2_9;_1_3;_4_9;Z|]
            |]
            [|_2_9;_1_3;_4_9;Z|]
            [|NQ(7,24);_1_4;_1_3;_1_8|]
            [|Z;_1_2;_3_4;_1|]
            _3
            _2
            4
            "Bogacki-Shampine method"

    let rkn34Id = Guid("4979806B-0404-426D-AD16-F0EB6448997F")
    let rkn34 =
        let _9_16 = NQ(9,16)
        EmbeddedButcherTableauHelpers.createAndRegister
            rkn34Id
            [|
                [|Z;Z;Z;Z;Z|]
                [|_3_8;Z;Z;Z;Z|]
                [|Z;_9_16;Z;Z;Z|]
                [|NQ(-125,672);NQ(325,336);Z;Z;Z|]
                [|NQ(371,891);NQ(-200,297);NQ(1120,891);Z;Z|]
            |]
            [|NQ(25,162);NQ(32,135);NQ(256,567);Z;NQ(11,70)|]
            [|NQ(37,225);NQ(44,117);Z;NQ(448,975);Z|]
            [|Z;_3_8;_9_16;NQ(25,32);_1|]
            _4
            _3
            5
            "Runge-Kutta-Nørsett 3/4-order method"

    let rkf45Id = Guid("158be900-43c2-4fca-b3a0-fec31a1eeaab")
    let rkf45 =
        EmbeddedButcherTableauHelpers.createAndRegister
            rkf45Id
            [|
                [|Z;Z;Z;Z;Z;Z|]
                [|_1_4;Z;Z;Z;Z;Z|]
                [|NQ(3,32);NQ(9,32);Z;Z;Z;Z|]
                [|NQ(1932,2197);NQ(-7200,2197);NQ(7296,2197);Z;Z;Z|]
                [|NQ(439,216);N -8;NQ(3680,513);NQ(-845,4104);Z;Z|]
                [|NQ(-8,27);N 2;NQ(-3544,2565);NQ(1859,4104);NQ(-11,40);Z|]
            |]
            [|NQ(16,135);Z;NQ(6656,12825);NQ(28561,56430);NQ(-9,50);NQ(2,55)|]
            [|NQ(25,216);Z;NQ(1408,2565);NQ(2197,4104);NQ(-1,5);Z|]
            [|Z;_1_4;_3_8;NQ(12,13);_1;_1_2|]
            _5
            _4
            6
            "Fehlberg's Runge-Kutta 4/5-order method"

    let rkf56Id = Guid("8A3F6EC7-C39A-4B99-A38C-DB0D791C6847")
    let rkf56 = 
        EmbeddedButcherTableauHelpers.createAndRegister
            rkf56Id
            [|
                [|Z;Z;Z;Z;Z;Z;Z;Z|]
                [|_1_6;Z;Z;Z;Z;Z;Z;Z|]
                [|NQ(4,75);NQ(16,75);Z;Z;Z;Z;Z;Z|]
                [|NQ(5,6);NQ(-8,3);NQ(5,2);Z;Z;Z;Z;Z|]
                [|NQ(-165,64);NQ(55,6);NQ(-425,64);NQ(85,96);Z;Z;Z;Z|]
                [|NQ(12,5);N -8;NQ(4015,612);NQ(-11,36);NQ(88,255);Z;Z;Z|]
                [|NQ(-8263,15000);NQ(124,75);NQ(-643,680);NQ(-81,250);NQ(2484,10625);Z;Z;Z|]
                [|NQ(3501,1720);NQ(-300,43);NQ(297275,52632);NQ(-319,2322);NQ(24068,84065);Z;NQ(3850,26703);Z|]
            |]
            [|NQ(3,40);Z;NQ(875,2244);NQ(23,72);NQ(264,1955);Z;NQ(125,11592);NQ(43,616)|]
            [|NQ(13,160);Z;NQ(2375,5984);NQ(5,16);NQ(12,85);NQ(3,44);Z;Z|]
            [|Z;_1_6;NQ(4,15);_2_3;NQ(5,6);_1;NQ(1,15);_1|]
            _6
            _5
            8
            "Fehlberg's Runge-Kutta 5/6-order method"

    let cashCarpId = Guid("277e577f-a236-4946-a30c-ae918346776c")
    let cashCarp = 
        EmbeddedButcherTableauHelpers.createAndRegister
            cashCarpId
            [|
                [|Z;Z;Z;Z;Z;Z|]
                [|NQ(1,5);Z;Z;Z;Z;Z|]
                [|NQ(3,40);NQ(9,40);Z;Z;Z;Z|]
                [|NQ(3,10);NQ(-9,10);NQ(6,5);Z;Z;Z|]
                [|NQ(-11,54);NQ(5,2);NQ(-70,27);NQ(35,27);Z;Z|]
                [|NQ(1631,55296);NQ(175,512);NQ(575,13824);NQ(44275,110592);NQ(253, 4096);Z|]
            |]
            [|NQ(37,378);Z;NQ(250,621);NQ(125,594);Z;NQ(512,177)|]
            [|NQ(2825,27648);Z;NQ(18575,48384);NQ(13525,55296);NQ(277,14336);_1_4;Z|]
            [|Z;NQ(1,5);NQ(3,10);NQ(3,5);_1;NQ(7,8);|]
            _5
            _4
            6
            "Cash-Karp Runge-Kutta 4/5-order method"

    let dormandPrinceId = Guid("d72c7269-4ee8-41df-90b4-304bd48cc2fe")
    let dormandPrince =
        EmbeddedButcherTableauHelpers.createAndRegister
            dormandPrinceId
            [|
                [|Z;Z;Z;Z;Z;Z;Z|]
                [|NQ(1,5);Z;Z;Z;Z;Z;Z|]
                [|NQ(3,40);NQ(9,40);Z;Z;Z;Z;Z|]
                [|NQ(44,45);NQ(-56,15);NQ(32,9);Z;Z;Z;Z|]
                [|NQ(19372,6561);NQ(-25360,2187);NQ(64448,6561);NQ(-212,729);Z;Z;Z|]
                [|NQ(9017,3168);NQ(-355,33);NQ(46732,5247);NQ(49,176);NQ(-5103,18656);Z;Z|]
                [|NQ(35,384);Z;NQ(500,1113);NQ(125,192);NQ(-2187,6784);NQ(11,84);Z|]
            |]
            [|NQ(35,384);Z;NQ(500,1113);NQ(125,192);NQ(-2187,6784);NQ(11,84);Z|]
            [|NQ(5179,57600);Z;NQ(7571,16695);NQ(393,640);NQ(-92097,339200);NQ(187,2100);NQ(1,40)|]
            [|Z;NQ(1,5);NQ(3,10);NQ(4,5);_1;NQ(8,9);_1;_1|]
            _6
            _5
            7
            "Dormand-Prince Runge-Kutta 4/5-order method"

    let backwardEulerId = Guid("042b419e-3524-4d96-a3fc-a8f8ee2a6f64")
    let backwardEuler =
        ButcherTableauHelpers.createAndRegister
            backwardEulerId
            [|
                [|_1|]
            |]
            [|_1|]
            [|_1|]
            _1
            1
            "Backward Euler"

    let implicitMidpointId = Guid("15872930-ca18-4147-a9fe-86dea5b1f46a")
    let implicitMidpoint =
        ButcherTableauHelpers.createAndRegister
            implicitMidpointId
            [|
                [|_1_2|]
            |]
            [|_1|]
            [|_1_2|]
            _2
            1
            "Implicit midpoint method"

    let crankNicolsonId = Guid("718591b7-b541-46e7-b3c0-7067be169a21")
    let crankNicolson =
        ButcherTableauHelpers.createAndRegister
            crankNicolsonId
            [|
                [|Z;Z|]
                [|_1_2;_1_2|]
            |]
            [|_1_2;_1_2|]
            [|Z;_1|]
            _2
            2
            "Crank-Nicolson method"

    let gaussLegendre4Id = Guid("a5fb7e1a-fb1f-4d4e-9520-016dd6b94965")
    let gaussLegendre4 =
        let sqrt3_6 = R (sqrt 3.0 / 6.0)
        let sqrt3_2 = R (sqrt 3.0 / 2.0)
        EmbeddedButcherTableauHelpers.createAndRegister
            gaussLegendre4Id
            [|
                [|_1_4;_1_4 - sqrt3_6|]
                [|_1_4 + sqrt3_6;_1_4|]
            |]
            [|_1_2;_1_2|]
            [|_1_2 + sqrt3_2;_1_2 - sqrt3_2|]
            [|_1_2 - sqrt3_6;_1_2 + sqrt3_6|]
            _4
            _2
            2
            "Gauss-Legendre fourth-order method"

    let gaussLegendre6Id = Guid("a86ab9d2-6489-45a2-a8bd-a808e75016ae")
    let gaussLegendre6 =
        let sqrt15_10 = R (sqrt 15.0 / 10.0)
        let sqrt15_15 = R (sqrt 15.0 / 15.0)
        let sqrt15_24 = R (sqrt 15.0 / 24.0)
        let sqrt15_30 = R (sqrt 15.0 / 30.0)
        EmbeddedButcherTableauHelpers.createAndRegister
            gaussLegendre6Id
            [|
                [|NQ(5,36);NQ(2,9) - sqrt15_15;NQ(5,36) - sqrt15_30|]
                [|NQ(5,36) + sqrt15_24;NQ(2,9);NQ(5,36) - sqrt15_24|]
                [|NQ(5,36) + sqrt15_30;NQ(2,9) + sqrt15_15;NQ(5,36)|]
            |]
            [|NQ(5,18);_4_9;NQ(5,18)|]
            [|NQ(-5,6);NQ(8,3);NQ(-5,6)|]
            [|_1_2 - sqrt15_10;_1_2;_1_2 + sqrt15_10|]
            _6
            _4
            3
            "Gauss-Legendre sixth-order method"

    let kraaljeVangerSpikerId = Guid("0c45467f-9b15-487e-8c6b-a531b84f0f67")
    let kraaljeVangerSpiker =
        ButcherTableauHelpers.createAndRegister
            kraaljeVangerSpikerId
            [|
                [|_1_2;Z|]
                [|-_1_2;_2|]
            |]
            [|-_1_2;NQ(3,2)|]
            [|_1_2;NQ(3,2)|]
            _2
            2
            "Kraalje-Vanger-Spiker method"

    let qinZhangId = Guid("ffdf135c-95aa-461c-8481-f02c78763df8")
    let qinZhang =
        ButcherTableauHelpers.createAndRegister
            qinZhangId
            [|
                [|_1_4;Z|]
                [|-_1_2;Z|]
            |]
            [|_1_2;_1_2|]
            [|_1_4;_3_4|]
            _2
            2
            "Qin-Zhang method"

    let crouze2Id = Guid("15bad55f-7375-41cf-aa4d-3581eaf83dee")
    let crouze2 =
        let sqrt3_6 = R (sqrt 3.0 / 6.0)
        let sqrt3_3 = R (sqrt 3.0 / 3.0)
        ButcherTableauHelpers.createAndRegister
            crouze2Id
            [|
                [|_1_2+sqrt3_6;Z|]
                [|-sqrt3_3;_1_2+sqrt3_6|]
            |]
            [|_1_2;_1_2|]
            [|_1_2+sqrt3_6;_1_2-sqrt3_6|]
            _3
            2
            "Crouzeix's second-order method"

    let rki43id = Guid("2d5d9cec-8db2-4df8-90d3-e3bcd42e0704")
    let rki43 =
        ButcherTableauHelpers.createAndRegister
            rki43id
            [|
                [|_1_2;Z;Z;Z|]
                [|_1_6;_1_2;Z;Z|]
                [|-_1_2;_1_2;_1_2;Z|]
                [|NQ(3,2);NQ(-3,2);_1_2;_1_2|]
            |]
            [|NQ(3,2);NQ(-3,2);_1_2;_1_2|]
            [|_1_2;_2_3;_1_2;_1|]
            _3
            4
            "Runge-Kutta diagonal implicit third-order method"

    let lobattoIIIA2Id = Guid("004a69ae-b05f-4ca1-8efe-7c2dbc26157f")   
    let lobattoIIIA2 =
        EmbeddedButcherTableauHelpers.createAndRegister
            lobattoIIIA2Id
            [|
                [|Z;Z|]
                [|_1_2;_1_2|]
            |]
            [|_1_2;_1_2|]
            [|_1;Z|]
            [|Z;_1|]
            _2
            _2
            2
            "Lobatto IIIA method of second order" 

    let lobattoIIIA4Id = Guid("12d25856-4a4e-42ca-b0e8-b280e5ab9d34")   
    let lobattoIIIA4 =
        EmbeddedButcherTableauHelpers.createAndRegister
            lobattoIIIA4Id
            [|
                [|Z;Z;Z|]
                [|NQ(5,24);_1_3;NQ(-1,24)|]
                [|_1_6;_2_3;_1_6|]
            |]
            [|_1_6;_2_3;_1_6|]
            [|-_1_2;_2;-_1_2|]
            [|Z;_1_2;_1|]
            _4
            _4
            3
            "Lobatto IIIA method of fourth order"

    let lobattoIIIB2id = Guid("96985671-1A60-46F6-AD84-4EEF8BEF470E")
    let lobattoIIIB2 =
        EmbeddedButcherTableauHelpers.createAndRegister
            lobattoIIIB2id
            [|
                [|_1_2;Z|]
                [|_1_2;Z|]
            |]
            [|_1_2;_1_2|]
            [|_1;Z|]
            [|Z;_1|]
            _2
            _2
            2
            "Lobatto IIIB method of second order"

    let lobattoIIIB4id = Guid("7788914C-9DC3-452D-A43D-CA986FED7DB1")
    let lobattoIIIB4 =
        EmbeddedButcherTableauHelpers.createAndRegister
            lobattoIIIB4id
            [|
                [|_1_6;-_1_6;Z|]
                [|_1_6;_1_3;Z|]
                [|_1_6;NQ(5,6);Z|]
            |]
            [|_1_6;_2_3;_1_6|]
            [|-_1_2;_2;-_1_2|]
            [|Z;_1_2;_1|]
            _4
            _4
            3
            "Lobatto IIIB method of fourth order"

    let lobattoIIIC2Id = Guid("F55817B7-F371-446B-89EF-46FC0B1297FD")
    let lobattoIIIC2 =
        EmbeddedButcherTableauHelpers.createAndRegister
            lobattoIIIC2Id
            [|
                [|_1_2;-_1_2|]
                [|_1_2;_1_2|]
            |]
            [|_1_2;_1_2|]
            [|_1;Z|]
            [|Z;_1|]
            _2
            _2
            2
            "Lobatto IIIC method of second order"

    let lobattoIIIC4Id = Guid("176B963E-7573-4B1B-8010-2AC7E905F66E")
    let lobattoIIIC4 =
        EmbeddedButcherTableauHelpers.createAndRegister
            lobattoIIIC4Id
            [|
                [|_1_6;-_1_3;_1_6|]
                [|_1_6;NQ(5,12);NQ(-1,12)|]
                [|_1_6;_2_3;_1_6|]
            |]
            [|_1_6;_2_3;_1_6|]
            [|-_1_2;_2;-_1_2|]
            [|Z;_1_2;_1|]
            _4
            _4
            3
            "Lobatto IIIC method of fourth order"

    let lobattoIIID2Id = Guid("29F5287D-30E6-444A-A9F8-6085EEF1F952")
    let lobattoIIID2 =
        ButcherTableauHelpers.createAndRegister
            lobattoIIID2Id
            [|
                [|_1_2;_1_2|]
                [|-_1_2;_1_2|]
            |]
            [|_1_2;_1_2|]
            [|Z;_1|]
            _2
            2
            "Lobatto IIID/IIINW method of second order"

    let lobattoIIID4Id = Guid("A018012A-635D-42B2-954A-9F3BB3EB284B")
    let lobattoIIID4 =
        ButcherTableauHelpers.createAndRegister
            lobattoIIID4Id
            [|
                [|_1_6;Z;_1_6|]
                [|NQ(1,12);NQ(5,12);Z|]
                [|_1_2;_1_3;_1_6|]
            |]
            [|_1_6;_2_3;_1_6|]
            [|Z;_1_2;_1|]
            _4
            3
            "Lobatto IIID/IIINW method of fourth order"

    let radauIA3Id = Guid("BF4C93B8-41C3-4A56-A39D-2CB14DCBDD39")
    let radauIA3 =
        ButcherTableauHelpers.createAndRegister
            radauIA3Id
            [|
                [|_1_4;-_1_4|]
                [|_1_4;NQ(5,12)|]
            |]
            [|_1_4;_3_4|]
            [|Z;_2_3|]
            _3
            2
            "Radau IA method of third order"

    let radauIA5Id = Guid("987F0263-A6FB-4E4F-AD5C-72020EE01524")
    let radauIA5 =
        let _7 = N 7
        let _43 = N 43
        let _18 = N 18
        let _36 = N 36
        let _360 = N 360
        let sqrt6 = R (sqrt 6.0)
        let sqrt6_10 = R (sqrt 6.0 / 10.0)
        let _3_5 = NQ (3, 5)
        let _11_45 = NQ (11, 45)
        ButcherTableauHelpers.createAndRegister
            radauIA5Id
            [|
                [|_1_9;(-_1-sqrt6)/_18;(-_1+sqrt6)/_18|]
                [|_1_9;_11_45+_7*sqrt6/_360;_11_45-_43*sqrt6/_360|]
                [|_1_9;_11_45+_43*sqrt6/_360;_11_45-_7*sqrt6/_360|]
            |]
            [|_1_9;_4_9+sqrt6/_36;_4_9-sqrt6/_36|]
            [|Z;_3_5-sqrt6_10;_3_5+sqrt6_10|]
            _5
            3
            "Radau IA method of fifth order"

    let radauIIA3Id = Guid("51819526-C3E5-4149-9D1B-867D141479F4")
    let radauIIA3 =
        ButcherTableauHelpers.createAndRegister
            radauIIA3Id
            [|
                [|_5_12;NQ(-1,12)|]
                [|_3_4;_1_4|]
            |]
            [|_1_4;_3_4|]
            [|_1_3;_1|]
            _3
            2
            "Radau IIA method of third order"

    let radauIIA5Id = Guid("CF363A92-7E07-4FED-AC09-E0E276DEA095")
    let radauIIA5 =
        let _7 = N 7
        let _43 = N 43
        let _18 = N 18
        let _36 = N 36
        let _75 = N 75
        let _169 = N 169
        let _360 = N 360
        let _1800 = N 1800
        let sqrt6 = R (sqrt 6.0)
        let sqrt6_10 = R (sqrt 6.0 / 10.0)
        let _2_5 = NQ (2, 5)
        let _3_5 = NQ (3, 5)
        let _11_45 = NQ (11, 45)
        let _2_225 = NQ (2, 225)
        let _37_225 = NQ (37, 225)
        ButcherTableauHelpers.createAndRegister
            radauIIA5Id
            [|
                [|_11_45-_7*sqrt6/_360;_37_225-_169*sqrt6/_1800;-_2_225+sqrt6/_75|]
                [|_37_225+_169*sqrt6/_1800;_11_45+_7*sqrt6/_360;-_2_225-sqrt6/_75|]
                [|_4_9-sqrt6/_36;_4_9+sqrt6/_36;_1_9|]
            |]
            [|_4_9-sqrt6/_36;_4_9+sqrt6/_36;_1_9|]
            [|_2_5-sqrt6_10;_2_5+sqrt6_10;_1|]
            _5
            3
            "Radau IA method of fifth order"
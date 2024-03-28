namespace Andes.DynamicalSystems.ContinuousTime.Ode

open System
open System.Linq.Expressions
open System.Runtime.CompilerServices
open Andes

type Parameter = {
    Name: string
    Value: Number
}
type ParameterSet = Parameter list
[<Struct>]
type State<'T> = 
| Scalar of s: 'T
| Vector of v: 'T array

type OrdinayDifferentialEquation<'T> = 
| Autonomous of Expression<Func<State<'T>, State<'T>>>
| NonAutonomous of Expression<Func<State<'T>, 'T, State<'T>>>

[<Extension>]
type OrdinaryDifferentialEquationExtensions =
    [<Extension>]
    static member inline IsAutonomous 
        (ode: OrdinayDifferentialEquation<'T>) = 
        match ode with
        | Autonomous _ -> true
        | _ -> false

    [<Extension>]
    static member inline IsNonAutonomous 
        (ode: OrdinayDifferentialEquation<'T>) = 
        match ode with
        | NonAutonomous _ -> true
        | _ -> false

module OrdinaryDifferentialEquationRegistry =
    let private registry = 
        ResizeArray<OrdinayDifferentialEquation<Number>>()

    let Register (ode: OrdinayDifferentialEquation<Number>) = 
        registry.Add(ode)

    let GetRegisteredOdes () = 
        registry.ToArray()
    
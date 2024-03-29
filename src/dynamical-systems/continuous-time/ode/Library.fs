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

type AutonomousRightHandSide<'T> = Expression<Func<'T, 'T>>
type NonAutonomousRightHandSide<'T, 'U> = Expression<Func<'T, 'U, 'T>>
type OrdinayDifferentialEquation<'T, 'U> = 
| Autonomous of AutonomousRightHandSide<'T>*ParameterSet
| NonAutonomous of NonAutonomousRightHandSide<'T, 'U>*ParameterSet
module OrdinaryDifferentialEquationExtensions =
    let getParameters (ode: OrdinayDifferentialEquation<'T, 'U>) =
        match ode with
        | Autonomous (_, parameters) -> parameters
        | NonAutonomous (_, parameters) -> parameters
    [<Extension>]
    type OrdinaryDifferentialEquationExtensions =
        [<Extension>]
        static member IsAutonomous 
            (ode: OrdinayDifferentialEquation<'T, 'U>) = 
            match ode with
            | Autonomous _ -> true
            | _ -> false

        [<Extension>]
        static member IsNonAutonomous 
            (ode: OrdinayDifferentialEquation<'T, 'U>) = 
            match ode with
            | NonAutonomous _ -> true
            | _ -> false
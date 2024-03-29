namespace Andes.Ode.SingleStep.RungeKutta

open System
open Andes.DynamicalSystems.ContinuousTime.Ode

module Step = 
    let p = 0
    let createStep tableau ode =
        match tableau with
        | ButcherTableauRegistry.T tableau -> 
            if tableau.IsExplicit() then
                match ode with
                | Autonomous (f, parameters) ->
                    ()
                | NonAutonomous (f, parameters) -> 
                    ()
            else
                raise (NotImplementedException("Implicit Runge-Kutta methods are not supported")) |> ignore
        | _ ->
            raise (NotImplementedException("Only Butcher tableau is supported")) |> ignore
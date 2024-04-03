namespace Andes.Ode.SingleStep.RungeKutta

open System
open System.Linq.Expressions
open Andes.DynamicalSystems.ContinuousTime.Ode

module Step = 
    let p = 0
    //let createStep tableau ode ctor =
    //    match tableau with
    //    | ButcherTableauRegistry.T tableau -> 
    //        if tableau.IsExplicit() then
    //            match ode with
    //            | Autonomous (f, parameters) ->
    //                let kis : Expression<Func<'T,'T>> array = Array.zeroCreate tableau.Steps
    //                [0..tableau.Steps-1] |> 
    //                List.iter (fun i -> 
    //                    let f : Expression<Func<'T,'T>> = 
    //                        if i = 0 then
    //                            f
    //                        else
    //                            let sum = 
    //                                [0..i-1] |> 
    //                                List.fold (fun acc j -> 
    //                                    match tableau.A.[i][j] with
    //                                    | aij when aij.IsZero() -> acc
    //                                    | aij when aij.IsOne() -> 
    //                                        let kj = kis.[j]
    //                                        Expression.Add(acc, Expression.Multiply(Expression.Constant(tableau.B.[j]), kj.Body))
    //                                    let aij = tableau.A.[i,j]
    //                                    let kj = kis.[j]
    //                                    Expression.Add(acc, Expression.Multiply(Expression.Constant(aij), kj.Body))
    //                                ) f.Body
    //                            Expression.Lambda<Func<'T,'T>>(sum, f.Parameters)
    //                    kis.[i] <- fun x -> f.Compile().Invoke(x)
    //                )
    //                ()
    //            | NonAutonomous (f, parameters) -> 
    //                ()
    //        else
    //            raise (NotImplementedException("Implicit Runge-Kutta methods are not supported")) |> ignore
    //    | _ ->
    //        raise (NotImplementedException("Only Butcher tableau is supported")) |> ignore
using static Andes.Ode.SingleStep.RungeKutta.ButcherTableauRegistry;

namespace Andes.Store.Numerics.Ode.SingleStep.RungeKutta;

public sealed class Effects 
{
    [EffectMethod]
    public Task Handle(InitializeAllAction _, IDispatcher dispatcher)
    {
        var tableaus = GetButcherTableaus();
        dispatcher.Dispatch(new ButcherTableausResultAction(tableaus));
        return Task.CompletedTask;
    }

    [EffectMethod]
    public Task Handle(ButcherTableausChangedAction _, IDispatcher dispatcher)
    {
        var tableaus = GetButcherTableaus();
        dispatcher.Dispatch(new ButcherTableausResultAction(tableaus));
        return Task.CompletedTask;
    }
}
using Fluxor;
using static Andes.Ode.SingleStep.RungeKutta.ButcherTableauRegistry;

namespace Andes.Store.Numerics.Ode.SingleStep.RungeKutta;

public sealed class Effects 
{
    [EffectMethod]
    public Task Handle(InitializeAllAction _, IDispatcher dispatcher)
    {
        dispatcher.Dispatch(new ButcherTableausResultAction(GetButcherTableaus()));
        return Task.CompletedTask;
    }

    [EffectMethod]
    public Task Handle(ButcherTableeusChangedAction _, IDispatcher dispatcher)
    {
        dispatcher.Dispatch(new ButcherTableausResultAction(GetButcherTableaus()));
        return Task.CompletedTask;
    }
}
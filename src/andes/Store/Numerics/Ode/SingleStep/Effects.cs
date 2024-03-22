namespace Andes.Store.Numerics.Ode.SingleStep;

public sealed class Effects
{
    [EffectMethod]
    public Task Handle(InitializeAllAction _, IDispatcher dispatcher)
    {
        dispatcher.Dispatch(new RungeKutta.InitializeAllAction());
        return Task.CompletedTask;
    }
}

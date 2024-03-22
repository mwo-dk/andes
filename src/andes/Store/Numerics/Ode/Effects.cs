namespace Andes.Store.Numerics.Ode;

public sealed class Effects
{
    [EffectMethod]
    public Task Handle(InitializeAllAction _, IDispatcher dispatcher)
    {
        dispatcher.Dispatch(new SingleStep.InitializeAllAction());
        return Task.CompletedTask;
    }
}
namespace Andes.Store.Numerics;

public sealed class Effects 
{
    [EffectMethod]
    public Task Handle(InitializeAllAction _, IDispatcher dispatcher)
    {
        dispatcher.Dispatch(new Ode.InitializeAllAction());
        return Task.CompletedTask;
    }
}
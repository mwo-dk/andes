namespace Andes.Store.Initialization;

public sealed class Effects 
{
    [EffectMethod]
    public Task Handle(StoreInitializedAction _, IDispatcher dispatcher)
    {
        dispatcher.Dispatch(new Numerics.InitializeAllAction());
        return Task.CompletedTask;
    }
}
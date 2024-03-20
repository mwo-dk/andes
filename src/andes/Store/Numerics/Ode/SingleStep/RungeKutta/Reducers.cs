using Fluxor;

namespace Andes.Store.Numerics.Ode.SingleStep.RungeKutta;

public static class Reducers
{
	[ReducerMethod]
	public static ButcherTableausState Reduce(ButcherTableausState _, InitializeAllAction __) =>
		new(true, null!);

    [ReducerMethod]
    public static ButcherTableausState Reduce(ButcherTableausState _, ButcherTableeusChangedAction __) =>
        new(true, null!);

   [ReducerMethod]
    public static ButcherTableausState Reduce(ButcherTableausState _, ButcherTableausResultAction action) =>
        new(false, action.ButcherTableaus);
}
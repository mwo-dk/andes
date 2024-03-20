using Andes.Ode.SingleStep.RungeKutta;

using static Andes.Ode.SingleStep.RungeKutta.ButcherTableauRegistry;

namespace Andes.Services.Numerics.Ode.SingleStep.RungeKutta;

public sealed class RungeKuttaService : IDisposable
{
	private readonly int _subscriptionId;

	public RungeKuttaService(Action handler) =>
		_subscriptionId = ButcherTableauRegistry.SubscribeToTableauChanges(handler);

	public ButcherTableaus? ButcherTableaus { get; set; }

	public void Dispose() =>
		ButcherTableauRegistry.UnsubscribeFromTableauChanges(_subscriptionId);
}

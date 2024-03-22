using Fluxor;
using static Andes.Ode.SingleStep.RungeKutta.ButcherTableauRegistry;

namespace Andes.Store.Numerics.Ode.SingleStep.RungeKutta;

[FeatureState]
public sealed record ButcherTableausState(bool IsLoading, IEnumerable<ButcherTableauRegistration>? ButcherTableaus)
{
    private ButcherTableausState() : this(true, null) { }
}
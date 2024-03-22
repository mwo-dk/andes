using static Andes.Ode.SingleStep.RungeKutta.ButcherTableauRegistry;

namespace Andes.Store.Numerics.Ode.SingleStep.RungeKutta;

public record InitializeAllAction();
public record ButcherTableausChangedAction();
public record ButcherTableausResultAction(IEnumerable<ButcherTableauRegistration> ButcherTableaus);
using Microsoft.FluentUI.AspNetCore.Components;

using static Andes.Ode.SingleStep.RungeKutta.ButcherTableauRegistry;

namespace Andes.Pages.Settings.Numerics.Ode.SingleStep.RungeKutta;

public partial class TableauPanel : IDialogContentComponent<ButcherTableauRegistration>
{
    [Parameter]
    public ButcherTableauRegistration Content { get; set; } = default!;

    [Inject]
    private IState<Store.Numerics.Ode.SingleStep.RungeKutta.ButcherTableausState>? State { get; set; }


}

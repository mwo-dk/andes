namespace Andes.Pages.Settings.Numerics.Ode.SingleStep.RungeKutta;

public partial class Home
{
    [Inject]
    private IState<Store.Numerics.Ode.SingleStep.RungeKutta.ButcherTableausState>? State { get; set; }

    [Inject]
    private IDispatcher Dispatcher { get; set; } = null!;

    protected override Task OnInitializedAsync()
    {
        return Task.CompletedTask;
    }
}
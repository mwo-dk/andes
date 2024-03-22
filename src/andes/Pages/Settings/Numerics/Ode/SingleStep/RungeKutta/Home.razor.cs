using Microsoft.FluentUI.AspNetCore.Components;

using static Andes.Ode.SingleStep.RungeKutta.ButcherTableauRegistry;

namespace Andes.Pages.Settings.Numerics.Ode.SingleStep.RungeKutta;

public partial class Home
{
    [Inject]
    private IState<Store.Numerics.Ode.SingleStep.RungeKutta.ButcherTableausState>? State { get; set; }

    [Inject]
    private IDialogService DialogService { get; set; } = null!;

    [Inject]
    private IDispatcher Dispatcher { get; set; } = null!;

    private IDialogReference? _dialog;

    public IQueryable<RungeKuttaMethod> Items
    {
        get
        {
            if (State is null)
            {
                return Enumerable.Empty<RungeKuttaMethod>().AsQueryable();
            }

            return State.Value.ButcherTableaus?.Select(x => new RungeKuttaMethod(x.Id, x.Name, x.IsEmbedded, x)).AsQueryable() ?? Enumerable.Empty<RungeKuttaMethod>().AsQueryable();
        }
    }

    private async Task OpenPanelRightAsync(Guid tableauId)
    {
        var item = State!.Value.ButcherTableaus!.Single(x => x.Id == tableauId)!;
        _dialog = await DialogService.ShowPanelAsync<TableauPanel>(item, new DialogParameters<ButcherTableauRegistration>()
        {
            Content = item,
            Alignment = HorizontalAlignment.Right,
            Title = item.Name,
        });
        DialogResult result = await _dialog.Result;
        HandlePanel(result);

    }

    private static void HandlePanel(DialogResult result)
    {
        if (result.Cancelled)
        {
            return;
        }

        if (result.Data is not null)
        {
            return;
        }
    }

    public record RungeKuttaMethod(Guid Id, string Name, bool IsEmbedded, ButcherTableauRegistration ButcherTableauRegistration);
}
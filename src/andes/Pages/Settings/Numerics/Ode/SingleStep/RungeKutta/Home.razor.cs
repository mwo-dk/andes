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

    public IQueryable<Model> Items
    {
        get
        {
            if (State is null)
            {
                return Enumerable.Empty<Model>().AsQueryable();
            }

            return State.Value.ButcherTableaus?
                .Select(x => 
                    new Model(x.Id, x.Name, x.Steps,
                    x.IsEmbedded, x.IsExplicit, x.IsBuiltIn,
                    x.Order, x.B1Order, x.B2Order, x))
                    .Where(FilterExplicit)
                    .Where(FilterEmbedded)
                    .Where(FilterBuiltIn)
                    .AsQueryable() ?? Enumerable.Empty<Model>().AsQueryable();
        }
    }

    private GridSort<Model> sortByName = GridSort<Model>
        .ByAscending(p => p.Name);
    private bool FilterExplicit(Model item) => 
        _explicitFilter == 1 ? item.IsExplicit : (_explicitFilter == 2 ? !item.IsExplicit : true) ;
    private int _explicitFilter = 0;

    private bool FilterEmbedded(Model item) => 
        _embeddedFilter == 1 ? item.IsEmbedded : (_embeddedFilter == 2 ? !item.IsEmbedded : true) ;
    private int _embeddedFilter = 0;

    private bool FilterBuiltIn(Model item) => 
        _builtInFilter == 1 ? item.IsBuiltIn : (_builtInFilter == 2 ? !item.IsBuiltIn : true) ;
    private int _builtInFilter = 0;
    private async Task OpenPanelRightAsync(Guid tableauId)
    {
        var item = State!.Value.ButcherTableaus!.Single(x => x.Id == tableauId)!;
        _dialog = await DialogService.ShowPanelAsync<TableauPanel>(item, new DialogParameters<ButcherTableauRegistration>()
        {
            Content = item,
            Alignment = HorizontalAlignment.Right,
            Title = item.Name,
            Width = "40%",
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

    public record Model(Guid Id, 
        string Name, 
        int Steps,
        bool IsEmbedded, 
        bool IsExplicit,
        bool IsBuiltIn,
        Number Order,
        Number B1Order,
        Number B2Order,
        ButcherTableauRegistration ButcherTableauRegistration);
}
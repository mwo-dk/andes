using Microsoft.AspNetCore.Components.Rendering;
using Microsoft.JSInterop;

namespace Andes.Shared;

[System.Diagnostics.CodeAnalysis.SuppressMessage("Design", "CA1063:Implement IDisposable Correctly", Justification = "<Pending>")]
public class MathJaxContentFluxorComponent : Fluxor.Blazor.Web.Components.FluxorComponent, IAsyncDisposable
{
    private IJSObjectReference? module;

    [Inject] private IJSRuntime jsRuntime { get; set; } = null!;

    [Parameter] public RenderFragment ChildContent { get; set; } = null!;

    
    protected override void BuildRenderTree(RenderTreeBuilder builder)
    {
        builder?.AddContent(0, ChildContent);
        //base.BuildRenderTree(builder);
    }

    protected override async Task OnAfterRenderAsync(bool firstRender)
    {
        if (firstRender)
        {
            module = await jsRuntime!.InvokeAsync<IJSObjectReference>("import", "./mathJaxBlazor.js");
        }
        if (module != null)
        {
            await module.InvokeVoidAsync("typesetPromise");
        }
        await base.OnAfterRenderAsync(firstRender);
    }

    public async ValueTask DisposeAsync()
    {
        if (module != null)
        {
            await module.InvokeVoidAsync("typesetClear");
            await module.DisposeAsync();
        }
    }
}
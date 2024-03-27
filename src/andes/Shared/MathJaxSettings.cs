using Microsoft.JSInterop;

namespace Andes.Shared;
public class MathJaxSettings : ComponentBase
{
    private IJSObjectReference? module;
    private bool hasRendered;

    [Inject] private IJSRuntime? jsRuntime { get; set; }

    [Parameter] public TexInputSettings Tex { get; set; } = new TexInputSettings();

    protected override async Task OnParametersSetAsync()
    {
        if (hasRendered)
        {
            await ApplySettingsAsync();
        }

        await base.OnParametersSetAsync();
    }

    protected override async Task OnAfterRenderAsync(bool firstRender)
    {
        if (firstRender)
        {
            module = await jsRuntime!.InvokeAsync<IJSObjectReference>("import", "./mathJaxBlazor.js");
            await ApplySettingsAsync();
            hasRendered = true;
        }
        await base.OnAfterRenderAsync(firstRender);
    }

    private async Task ApplySettingsAsync()
    {
        await module!.InvokeVoidAsync("applySettings", Tex);
    }
}
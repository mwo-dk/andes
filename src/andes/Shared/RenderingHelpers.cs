using Microsoft.FluentUI.AspNetCore.Components;

using static Microsoft.FluentUI.AspNetCore.Components.Icons.Regular.Size24;

namespace Andes.Shared;

public static class RenderingHelpers
{
    private static readonly Icon Checked = new CheckboxChecked();
    private static readonly Icon Unchecked = new CheckboxUnchecked();
    public static Icon ToIcon(this bool value) =>
        value ? Checked : Unchecked;
}
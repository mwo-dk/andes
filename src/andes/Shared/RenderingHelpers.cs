using Microsoft.FluentUI.AspNetCore.Components;

namespace Andes.Shared;

public static class RenderingHelpers
{
    private static readonly Icon Checked = new Icons.Regular.Size24.CheckboxChecked();
    private static readonly Icon Unchecked = new Icons.Regular.Size24.CheckboxUnchecked();
    public static Icon ToIcon(this bool value) =>
        value ? Checked : Unchecked;
}
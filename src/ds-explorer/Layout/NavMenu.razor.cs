using Microsoft.Extensions.Localization;

namespace DSE.Layout;

public partial class NavMenu
{
    [Inject]
    IStringLocalizer<Common>? CommonLocalizer { get; set; }

    [Inject]
    IStringLocalizer<NavMenu>? Localizer { get; set; }
}

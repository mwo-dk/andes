using Microsoft.Extensions.Localization;

namespace DSE.Layout;

public partial class NavMenu
{
    [Inject]
    IStringLocalizer<NavMenu>? Localizer { get; set; }
}

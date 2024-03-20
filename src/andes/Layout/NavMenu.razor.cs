using Microsoft.Extensions.Localization;

namespace Andes.Layout;

public partial class NavMenu
{
    [Inject]
    IStringLocalizer<NavMenu>? Localizer { get; set; }
}

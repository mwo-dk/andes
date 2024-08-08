using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Fluxor;

using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;

using Plotly.Blazor.Traces;
using Plotly.Blazor;
using Plotly.Blazor.Traces.ScatterGlLib;

namespace Andes.UI.Plotly;
public partial class Cobweb
{
    [Inject]
    IDispatcher? Dispatcher { get; set; } = default!;

    [Inject]
    IJSRuntime? JSRuntime { get; set; }

    PlotlyChart? chart;
    readonly Config? config = new();
    readonly Layout? layout = new();
    // Using of the interface IList is important for the event callback!
    IList<ITrace> data = new List<ITrace>
    {
        new Scatter
        {
            Name = "ScatterTrace",
            //Mode = new  Nullable<ModeFlag>(ModeFlag.Lines | ModeFlag.Markers),
            X = [1,2,3],
            Y = [1,2,3]
        }
    };
}

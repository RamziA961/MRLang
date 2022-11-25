using System;
using Avalonia.Controls;
using Avalonia.Logging;
using ReactiveUI;

namespace GUI.ViewModels;

public class AlgolTextEditor : ViewModelBase
{
    private string? _SourceCode;

    public AlgolTextEditor()
    {
        this.WhenAnyValue(o => o.SourceCode)
            .Subscribe(o=> this.RaisePropertyChanged(nameof(SourceCode)));
    }

    public string? SourceCode
    {
        get => _SourceCode;
        set
        {
            this.RaiseAndSetIfChanged(ref _SourceCode, value);

            Logger.TryGet(LogEventLevel.Fatal, LogArea.Control)?
                .Log(this, $"sourceCode: {_SourceCode}");
        }
    }
}
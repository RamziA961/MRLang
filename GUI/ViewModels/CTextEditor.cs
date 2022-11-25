using System;
using Avalonia.Controls;
using Avalonia.ReactiveUI;
using ReactiveUI;

namespace GUI.ViewModels;

public class CTextEditor : ViewModelBase
{
    private string? _SourceCode;
    
    public CTextEditor()
    {
        this.WhenAnyValue(o => o.SourceCode)
            .Subscribe(o => this.RaisePropertyChanged(nameof(SourceCode)));
    }

    public string? SourceCode
    {
        get => _SourceCode;
        set => this.RaiseAndSetIfChanged(ref _SourceCode, value);
    }
}
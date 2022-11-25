using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;
using Avalonia.ReactiveUI;
using GUI.ViewModels;
using ReactiveUI;

namespace GUI.Views;

public partial class Main : UserControl
{
    public Main()
    {
        // this.WhenActivated(disposables =>
        // {
        //     this.BindCommand(MainViewModel, x => x.RunCommand, x => x.RunButton)
        //         .DisposeWith(disposables);
        // });
        InitializeComponent();
    }

    private void InitializeComponent()
    {
        AvaloniaXamlLoader.Load(this);
    }
}
using System.Reactive.Disposables;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Logging;
using Avalonia.ReactiveUI;
using GUI.ViewModels;
using ReactiveUI;

namespace GUI;

public partial class MainWindow : Window // was Window
{
    public MainWindow()
    {

        Logger.TryGet(LogEventLevel.Fatal, LogArea.Control)?.Log(this, "Debug Mode");
        InitializeComponent();

        #if DEBUG
                this.AttachDevTools();
        #endif
    }
}
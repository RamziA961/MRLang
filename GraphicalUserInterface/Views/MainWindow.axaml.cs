using System.Reactive.Disposables;
using Avalonia.Controls;
using Avalonia.ReactiveUI;
using GraphicalUserInterface.ViewModels;
using ReactiveUI;

namespace GraphicalUserInterface.Views;

public partial class MainWindow : ReactiveWindow<MainWindowViewModel>
{
    public MainWindow()
    {
        InitializeComponent();
        // var vm = new MainWindowViewModel();

        this.WhenActivated(disposables =>
        {
            this.BindCommand(
                ViewModel,
                vm => vm.BuildCommand,
                v => v.BuildButton
            ).DisposeWith(disposables);

            this.BindCommand(
                ViewModel,
                vm => vm.RunCommand,
                v => v.RunButton
            ).DisposeWith(disposables);

            this.BindCommand(
                ViewModel,
                vm => vm.TranspileCommand,
                v => v.TranspileButton
            ).DisposeWith(disposables);

            this.BindCommand(
                ViewModel,
                vm => vm.SaveAlgolSource,
                v => v.SaveAlgolButton
            ).DisposeWith(disposables);

            this.BindCommand(
                ViewModel,
                vm => vm.SaveCSource,
                v => v.SaveCButton
            ).DisposeWith(disposables);

            this.BindCommand(
                ViewModel,
                vm => vm.LoadAlgolSource,
                v => v.LoadAlgolButton
            ).DisposeWith(disposables);
            
            this.BindCommand(
                ViewModel,
                vm => vm.LoadCSource,
                v => v.LoadCButton
            ).DisposeWith(disposables);
        });
        // this.Weh

    }
}
using System;
using System.Diagnostics;
using System.IO;
using System.Reactive;
using DynamicData.Binding;
using ReactiveUI;
using Transpiler;

namespace GraphicalUserInterface.ViewModels;

public class MainWindowViewModel : ViewModelBase
{
    
    private string? _cSource;
    private string? _algolSource;
    private string? _output;
    
    public MainWindowViewModel()
    {
        // this.WhenPropertyChanged(o => o.AlgolSource, notifyOnInitialValue: false)
        //     .Subscribe(_ => this.RaisePropertyChanged(nameof(AlgolDummy)));
        //
        // this.WhenPropertyChanged(o => o.CSource, notifyOnInitialValue: false)
        //     .Subscribe(_ => this.RaisePropertyChanged(nameof(CDummy)));
        
        BuildCommand = ReactiveCommand.Create(CompileC);
        RunCommand = ReactiveCommand.Create(RunC);
        TranspileCommand = ReactiveCommand.Create(TranspileAlgol);
        
        LoadAlgolSource = ReactiveCommand.Create(LoadAlgol);
        LoadCSource = ReactiveCommand.Create(LoadC);

        
        SaveCSource = ReactiveCommand.Create(() =>
        {
            var path = $"{Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName}/prog.c";
            File.WriteAllText(path, _cSource);

            Output = $"C source code successfully saved at {path}";
        });
        // SaveCSource.BindTo(this, o => o.Output);
        
        SaveAlgolSource = ReactiveCommand.Create(() =>
        {
            var path = $"{Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName}/prog.a68";
            File.WriteAllText(path, _algolSource);
            
            Output = $"Algol source code successfully saved at {path}";
        });
        // SaveAlgolSource.BindTo(this, o => o.Output);
    }
    
    public ReactiveCommand<Unit, Unit> RunCommand { get; }
    public ReactiveCommand<Unit, Unit> BuildCommand { get; }
    public ReactiveCommand<Unit, Unit> TranspileCommand { get; }
    
    public ReactiveCommand<Unit, Unit> SaveCSource { get; }
    public ReactiveCommand<Unit, Unit> SaveAlgolSource { get; }
    
    public ReactiveCommand<Unit, Unit> LoadCSource { get; }
    public ReactiveCommand<Unit, Unit> LoadAlgolSource { get; }

    public string? CSource
    {
        get => _cSource;
        set => this.RaiseAndSetIfChanged(ref _cSource, value);
    }

    public string? AlgolSource
    {
        get => _algolSource;
        set => this.RaiseAndSetIfChanged(ref _algolSource, value);
    }

    // public string CDummy => String.IsNullOrEmpty(_cSource) ? "C Source: N/A" : _cSource;
    //
    // public string? AlgolDummy => String.IsNullOrEmpty(_algolSource) ? "Algol Source: N/A" : _algolSource;

    public string? Output
    {
        get => _output;
        private set => this.RaiseAndSetIfChanged(ref _output, value);
    }


    private void TranspileAlgol()
    {
        try
        {
            var output = Interface.Transpile(_algolSource);
            CSource = output;
            Output = $"Algol source code successfully transpiled.";
        }
        catch (Exception e)
        {
            Output = $"Transpiler failed.\n{e.Message}";
        }
    }
    
    private void CompileC()
    {
        var path = Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName;
        var startInfo = new ProcessStartInfo
        {
            FileName = "gcc",
            Arguments = "prog.c -o prog.o -Wall",
            CreateNoWindow = true,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = path
        };

        var proc = new Process
        {
            StartInfo = startInfo,
            EnableRaisingEvents = true
        };
            
        proc.Exited += delegate(object? _, EventArgs _) {  
            var output = !proc.StandardOutput.EndOfStream ? proc.StandardOutput.ReadToEnd() :  "";
            var error = !proc.StandardError.EndOfStream ? proc.StandardError.ReadToEnd() : "";

            Output = string.IsNullOrWhiteSpace(output) ? error : output;
            
            Output = string.IsNullOrWhiteSpace(Output) ? 
                $"C source code has been successfully compiled and saved at {path}." : Output;
        };

        proc.Start();
    }

    private void RunC()
    {
        var path = Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName;
        var startInfo = new ProcessStartInfo
        {
            FileName = $"{path}/prog.o",
            Arguments = "",
            CreateNoWindow = true,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
        };

        var proc = new Process
        {
            StartInfo = startInfo,
            EnableRaisingEvents = true
        };

        proc.Exited += (_, _) =>
        {
            var output = !proc.StandardOutput.EndOfStream ? proc.StandardOutput.ReadToEnd() : "";
            var error = !proc.StandardError.EndOfStream ? proc.StandardError.ReadToEnd() : "";

            Output = string.IsNullOrWhiteSpace(output) ? error : output;

            Output = string.IsNullOrWhiteSpace(Output)
                ? "Program successfully executed." : Output;
        };

        proc.Start();
    }

    private void LoadAlgol()
    {
        var path = Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName;
        var startInfo = new ProcessStartInfo
        {
            FileName = "cat",
            Arguments = "prog.a68",
            CreateNoWindow = true,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = path
        };
        
        var proc = new Process
        {
            StartInfo = startInfo,
            EnableRaisingEvents = true
        };

        proc.Exited += (_, _) =>
        {
            var output = !proc.StandardOutput.EndOfStream ? proc.StandardOutput.ReadToEnd() : "";
            var error = !proc.StandardError.EndOfStream ? proc.StandardError.ReadToEnd() : "";

            AlgolSource = String.IsNullOrWhiteSpace(error) ? output : AlgolSource;
            Output = String.IsNullOrWhiteSpace(error) ? "Algol source code loaded successfully" : error;
        };

        proc.Start();
    }

    private void LoadC()
    {
        var path = Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName;
        var startInfo = new ProcessStartInfo
        {
            FileName = "cat",
            Arguments = "prog.c",
            CreateNoWindow = true,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = path
        };
        
        var proc = new Process
        {
            StartInfo = startInfo,
            EnableRaisingEvents = true
        };

        proc.Exited += (_, _) =>
        {
            var output = !proc.StandardOutput.EndOfStream ? proc.StandardOutput.ReadToEnd() : "";
            var error = !proc.StandardError.EndOfStream ? proc.StandardError.ReadToEnd() : "";

            CSource = String.IsNullOrWhiteSpace(error) ? output : AlgolSource;
            Output = String.IsNullOrWhiteSpace(error) ? "C source code loaded successfully" : error;
        };

        proc.Start();
    }
    
}
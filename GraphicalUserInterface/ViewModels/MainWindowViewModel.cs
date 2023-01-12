using System;
using System.Diagnostics;
using System.IO;
using System.Reactive;
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
        BuildCommand = ReactiveCommand.Create(CompileC);
        RunCommand = ReactiveCommand.Create(RunC);
        TranspileCommand = ReactiveCommand.Create(TranspileAlgol);
        
        LoadAlgolSource = ReactiveCommand.Create(LoadAlgol);
        
        LoadCSource = ReactiveCommand.Create(LoadC);
        SaveCSource = ReactiveCommand.Create(SaveC);
        
        SaveAlgolSource = ReactiveCommand.Create(() =>
        {
            var path = AppDomain.CurrentDomain.BaseDirectory;
            using var f = new StreamWriter(Path.Combine(path, "prog.a68"));
            f.Write(_algolSource);
            Output = $"Algol source code successfully saved at {path}.";
        });
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
        var path = AppDomain.CurrentDomain.BaseDirectory;;

        SaveC();
        
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
            var output =  
                (!proc.StandardOutput.EndOfStream ? proc.StandardOutput.ReadToEnd() : "") +
                $"C source code has been successfully compiled and saved at {path}.";
            
            var error = !proc.StandardError.EndOfStream ? proc.StandardError.ReadToEnd() : "";

            Output += "\n" + (string.IsNullOrWhiteSpace(error) ? output : error);
        };

        proc.Start();
    }

    private void RunC()
    {
        var path = AppDomain.CurrentDomain.BaseDirectory;
        var startInfo = new ProcessStartInfo
        {
            FileName = Path.Combine(path, "prog.o"),
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
        var path = AppDomain.CurrentDomain.BaseDirectory;
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
            Output = String.IsNullOrWhiteSpace(error) ? "Algol source code loaded successfully." : error;
        };

        proc.Start();
    }

    private void LoadC()
    {
        var path = AppDomain.CurrentDomain.BaseDirectory;;
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
            Output = String.IsNullOrWhiteSpace(error) ? "C source code loaded successfully." : error;
        };

        proc.Start();
    }

    private void SaveC()
    {
        var path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "prog.c");
        File.WriteAllText(path, _cSource);

        Output = $"C source code successfully saved at {path}";
    }
    
}
using System.Diagnostics;
using System.Reactive;

using Avalonia.Logging;
using ReactiveUI;

namespace GUI.ViewModels;

public class MainViewModel : ViewModelBase
{
    public AlgolTextEditor aText { get; } = new AlgolTextEditor();
    public CTextEditor cTex { get; } = new CTextEditor();

    public MainViewModel()
    {

        RunCommand = ReactiveCommand.Create(() =>
        {
            Logger.TryGet(LogEventLevel.Information, LogArea.Control)?.Log(this, "Run Command Execute");
        });

        BuildCommand = ReactiveCommand.Create( () =>
        {
            Logger.TryGet(LogEventLevel.Information, LogArea.Control)?.Log(this, "Build Command Execute");
            var startInfo = new ProcessStartInfo
            {
                FileName = "ls",
                Arguments = "-l",
                CreateNoWindow = true,
                UseShellExecute = false,
                RedirectStandardOutput = true
            };

            var proc = new Process
            {
                StartInfo = startInfo
            };
            proc.Start();

            var stdOut = "";
            while (!proc.StandardOutput.EndOfStream)
            {
                stdOut += proc.StandardOutput.ReadLine() + "\n";
            }
            
            Logger.TryGet(LogEventLevel.Information, LogArea.Control)?.Log(this, stdOut);
        });
    }
    
    
    public ReactiveCommand<Unit, Unit> RunCommand { get; }
    public ReactiveCommand<Unit, Unit> BuildCommand { get; }
}
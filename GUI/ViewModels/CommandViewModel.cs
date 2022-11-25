using Avalonia.Controls;
using Avalonia;
using ReactiveUI;

namespace GUI.ViewModels;

public class CommandViewModel : ReactiveObject
{
    private readonly string _input;
    private readonly string _output;

    public CommandViewModel(string input, string output)
    {
        _input = input;
        _output = output;
    }

}
## Welcome to RALang
### The Algol68 Genie to C Transcompiler.

Test out the transpiler and accompanying GUI tool by heading to **Releases**. Download the directory that matches your system.
To execute the program, launch **GraphicalUserInterface.exe** in the downloaded directory.

**OR**

Download the appropriate zip file from the GitHub release section on the right hand panel. Decompress the zip file and execute the executable.

\
Only a subset of Algol's features have been implemented thus far.
\
Current support includes:
 - Data types - Integers, Floats, and Booleans. (Limited support for strings).
 - Binary, Logical, Relation Expressions.
 - Declaration and Assignment Statements.
 - Conditional Statements and While Loops.
 - Function Declarations (WIP).
 - Function Calls (WIP).


\
You can use the following code snippets.

```
    proc hello = (int y, int z) int:
    begin
        int i = y + z;
    end
    
    proc main = () int:
    begin
        int i := 99;
        int j := 99;
        if i == j then
            j := j + i;
            i := i + i + j + 300;
        else 
            i := j;
            hello(i, j);
        fi
    end 
```

```
    proc main = () int:
    begin
        real x = 321.032;
        real y := -0.54213
        bool b := true;
    
        if y > x then
            y := x * y + (1 - (3 * 6 ^ 2));
            b := b & (y > x)
        elif y == x then
            y := (x - 1) * y;
            b := b & y ~= x;
        else
            y := 1 - 2 - 3 - y;
            b := not b
        fi
    end
```
```
    proc main = () int:
    begin
        int i := 0;
        int j := 0;
        int k := 0;
    
        while i < 1000 do
            while j < i do
                while k < j do
                    if i > 0 & j < 10 | k > 500 then
                        if true then
                            k := k + 1;
                        fi
                    fi
                    k := k + 1;
                od
                j := j + 1;
                k := 0;
            od
            i := i + 1;
            j := 0;
        od
    end
```

# hbf
A BrainF**** interpreter written in Haskell

This interpreter implements BrainF**** with two tapes: A Command Tape and a State Tape.
No optimizations are done

### Running:
The interpreter takes in one command line option: Either the file name, or `repl`. If a file name is given then the file is run through the interpreter, otherwise if `repl` is entered then *repl* mode is activated.

For stack the command could be `stack build --exec "hbf-exe repl`

### REPL Mode:
In REPL mode every line is considered to be a full BrainF**** program and a new Turing Machine is created to run the command tape on an empty State tape
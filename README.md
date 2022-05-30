# BrainHuck
A BrainF*** interpreter written in Haskell

This interpreter implements BrainF*** with two tapes: A Command Tape and a State Tape.
The Command Tape is represented as a Graph G(V,E) where V = (i, cmd) where each node is representative of a command character in the source code file, and there exists an edge for every valid jump in the source code.

For example, Echo:

src.bf: `,[.,]`

representation: 

    V:{(0, ','), (1, '['), (2, '.'), (3, ','), (4, ']'), (5, END)}
    E:{(0, 1), (1, 2), (1, 4), (2, 3), (3, 4), (4, 5), (4, 1)}


## Running:
The interpreter takes in one command line option: Either the file name, or `repl`. If a file name is given then the file is run through the interpreter, otherwise if `repl` is entered then *repl* mode is activated.

For stack the command could be `stack build --exec "hbf-exe repl"`

### REPL Mode:
In REPL mode every line is considered to be a full BrainF*** program and a new Turing Machine is created to run the command tape on an empty State tape


## Implementation
This is an 8-Bit Two Tape implementation of BrainF***. EOF is interpreted as a null byte `\0`.

More formally: 

The program state is represented by `(TM, status, IB)` TM is a Turing Machine representing the State and Command logic. IB is a string representing the Input Buffer.

TM is formal Turing Machines, that is to say that they both have "infinite" size to the left and right of the head position where all values are initialized as 0. 

The "infinite" tape is implemented with two stacks, L and R with a head position, so the tape is only as infinite as your memory.

Technically this interpreter is 8-Bit as I clamp values out of range to stay within range, although that is arbitrary and is set by maxValue in "Model.hs"

## Future:
I have optimized the interpreter to essentially compile the source code to a graph representation, at this point it would not be too difficult to translate this intermediate language into assembly and simply have a binary.

I would like to create a more expansive language that is an extension of BrainF*** that does not have a tape value limit and has the ability to move between tapes.
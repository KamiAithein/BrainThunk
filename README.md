# BrainThunk
An extension of BrainF*** interpreter written in Haskell

This interpreter implements BrainF*** with two categories of tapes: Command Tape and State Tape.
A Command Tape is read only and is represented as a Graph G(V,E) where V = (i, cmd) where each node is representative of a command character in the source code file, and there exists an edge for every valid jump in the source code.

For example, Echo:

src.bf: `,[.,]`

representation: 

    V:{(0, ','), (1, '['), (2, '.'), (3, ','), (4, ']'), (5, END)}
    E:{(0, 1), (1, 2), (1, 4), (2, 3), (3, 4), (4, 5), (4, 1)}

State Tape is the tape to be written and read from. It is a structure with 2 lazily evaluated stacks and a pivot cell. There are "infinite" tapes that can be accessed by going _up_ a tape `'` or _down_ a tape `;`. 

## Running:
The interpreter takes in one command line option: Either the file name, or `repl`. If a file name is given then the file is run through the interpreter, otherwise if `repl` is entered then *repl* mode is activated.

For stack the command could be `stack build --exec "hbf-exe repl"`

### REPL Mode:
In REPL mode every line is considered to be a full BrainF*** program and a new Turing Machine is created to run the command tape on an empty State tape


## Implementation
This is an extension of BrainF***! It is not necessarily compatible with BrainF\*\*\* programs. 

The commands are:

+ `[`
    + Jump on zero to matching bracket
+ `]`
    + Jump on _not_ zero to matching bracket
+ `,`
    + Replace current cell with a read from stdin
        + When prompted for STDIN the entirety of STDIN is taken and put into the "Input Buffer". `,` will read from the input buffer if it is not empty, otherwise prompting for more input.
+ `.` 
    + Print the current cell as ASCII to the terminal
+ `>`
    + Move one cell to the right on the current tape
+ `<`
    + Move one cell to the left on the current tape
+ `+`
    + Increase the value in the cell on the current tape by 1. **NOTE**: Does _NOT_ wrap around at 255. Uses Haskell `Int` internally; I should look at the max value for Int but Yeah!
+ `-`
    + Decrease the value in the cell on the current tape by 1. **NOTE**: See `+`
+ `'`
    + Move "up" one tape: Switches the tape in view! BrainThunk allows "infinite" tapes!
+ `;`
    + Move "down" one tape: **NOTE**: See `'`

EOF is interpreted as a null byte `\0`.

More formally: 

The program state is represented by `(TM, status, IB)` TM is a Turing Machine representing the State and Command logic. IB is a string representing the Input Buffer.

TM is formal Turing Machines, that is to say that they both have "infinite" size to the left and right of the head position where all values are initialized as 0. 

The "infinite" tape is implemented with two stacks, L and R with a head position, so the tape is only as infinite as your memory.

## Future:
I have optimized the interpreter to essentially compile the source code to a graph representation, at this point it would not be too difficult to translate this intermediate language into assembly and simply have a binary.

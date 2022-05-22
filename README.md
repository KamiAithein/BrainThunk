# hbf
A BrainF**** interpreter written in Haskell

This interpreter implements BrainF**** with two tapes: A Command Tape and a State Tape.
No optimizations are done

### Running:
The interpreter takes in one command line option: Either the file name, or `repl`. If a file name is given then the file is run through the interpreter, otherwise if `repl` is entered then *repl* mode is activated.

For stack the command could be `stack build --exec "hbf-exe repl`

### REPL Mode:
In REPL mode every line is considered to be a full BrainF**** program and a new Turing Machine is created to run the command tape on an empty State tape


### Future:
I would like to optimize at the very least the jump procedures: It takes _O(n)_ time to find the "matching" bracket for any jump, where _n_ is the length of the tape. Since we have an "infinite" tape, this can hang. It would be optimal to preprocess the BrainF**** program and almost create "edges" between the brackets and their matching values. This would also remove the possibility of hanging by searching for an unmatched bracket as we could just return an error.

A more abstract representation of the command tape would be helpful in this regard: A formal Turing Machine representation can only move right or left, and doesn't have the ability to jump between arbitrary points of memory. I can easily see a Graph representation being useful: The Command Tape is a form of ROM so we can process the BF program into a graph G(V, E) s.t. forall v. in V: v = (OP, ID) where OP is some BF operator and ID is a unique ID for the node, and forall. e in E: e = (j, (u,w)) where u,w in V and j in {JUMP, NEXT}

On a Jump command the interpreter would see that there are two possible paths, "JUMP" and "NEXT" and would decide to jump or not based on the value of the State Tape.
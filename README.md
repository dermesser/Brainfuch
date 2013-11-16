# Brainfuch - A Haskell Brainfuck interpreter

Brainfuch is a Brainfuck interpreter written in Haskell. Except for
just interpreting Brainfuck code (executable BFexec) it may also used
for debugging Brainfuck code (executable BFfollow).

Example session, interpretation only (text after # is comment):

    ++>-->,. # Code, from stdin
    [EOF]; Interpreting code # Interpreter output
    78 # User input
    78 # Program output
    Current stack: # Status output
    ([2,-2],78,[]) # Last stack state

Example session, with debugging (text after # is comment):

    ++>-->,.><<< # Code from stdin

    Output of script: # Interpreter output
    99 # User input
    99 # Brainfuck output

    Stack states during interpretation: # Interpreter output
    # Columns: <Command> <Stack after this command>
    # Stack format: [x] = inactive cell; {x} = current cell
    +  {1}
    +  {2}
    >  [2]{0}
    -  [2]{-1}
    -  [2]{-2}
    >  [2][-2]{0}
    ,  [2][-2]{99}
    .  [2][-2]{99}
    >  [2][-2][99]{0}
    <  [2][-2]{99}[0]
    <  [2]{-2}[99][0]
    <  {2}[-2][99][0]

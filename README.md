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

Example session with debugging, II (*multiplication: 4 x 4*):

    ++++[>++++<-]

    Output of script:


    Stack states during interpretation:
    +  {1}
    +  {2}
    +  {3}
    +  {4}
    >  [4]{0}
    +  [4]{1}
    +  [4]{2}
    +  [4]{3}
    +  [4]{4}
    <  {4}[4]
    -  {3}[4]
    >  [3]{4}
    +  [3]{5}
    +  [3]{6}
    +  [3]{7}
    +  [3]{8}
    <  {3}[8]
    -  {2}[8]
    >  [2]{8}
    +  [2]{9}
    +  [2]{10}
    +  [2]{11}
    +  [2]{12}
    <  {2}[12]
    -  {1}[12]
    >  [1]{12}
    +  [1]{13}
    +  [1]{14}
    +  [1]{15}
    +  [1]{16}
    <  {1}[16]
    -  {0}[16]

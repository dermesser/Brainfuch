# Brainfuch - A Haskell Brainfuck interpreter

Brainfuch is a Brainfuck interpreter library written in Haskell shipped
with different executable interpreters.

CURRENT FLAWS:
- Does not handle nested loops correctly.

*Note:* Stack *means here the virtual tape used by Brainfuck; a piece of Brainfuck code is called* script.

## BFexec

The standard interpreter. Reads code, evaluates it, prints any outputs made by the program
and the final state of the stack.

Example session, interpretation only (text after # is a comment):

    ++>-->,. # User input
    [EOF]; Interpreting code
    78 # User input
    78 # The script's output
    Current stack:
    ([2,-2],78,[])

## BFdbg

BFdbg not only evaluates a script but also prints every step made on the way to the final result.

Columns: \<Command\> \<Stack after this command\>

Stack format: [x] = inactive cell; {x} = current cell

Example session, with debugging (text after # is a comment):

    ++>-->,.><<< # Code from stdin

    Output of script:
    99 # User input
    99 # Script output

    Stack states during interpretation:
       {0}
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

## BFdbgIA

BFdbgIA stands for "BrainFuck DeBuGger InterActive". That means the interpreter prints the successive
steps not as a list as BFdbg does but sequentially on one line which is updated every second.

    ++++[>++++<-]

    Output of script:


    Stack states during interpretation:
    +  [4]{2}   (/)

and a second later

    ++++[>++++<-]

    Output of script:


    Stack states during interpretation:
    +  [4]{3}   (-)

(looks a bit like what `wget` does)

## BFdbgBreakpoint

This version uses the new symbol '|' to set breakpoints at which the current state is saved
and printed after execution:

    ++++|[>+>+<<-|]

    Output of script:


    Stack states during interpretation:
    {4}
    {3}[1][1]
    {2}[2][2]
    {1}[3][3]
    {0}[4][4]

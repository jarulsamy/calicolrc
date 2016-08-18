Here we go...

This is a toy project I made just for the fun of it after reading a
message on comp.lang.python about a BASIC interpreter written in python.

basic.py is just that... a BASIC interpreter written in python;
by running it you get an interactive environment where you can
issue immediate commands or you can enter programs.

Supported commands
==================

   NEW
     Wipes out the current program from memory.

   LOAD <file name>
     Loads a basic program in memory. If this command is issued from a
     program then the loaded program will also be started (variables
     are NOT cleared in this case).

   CLEAR
     Clears all variables and gosub/for stacks.

   PRINT <expr> [ ; <expr> ] {;}
     Video outputs of a sequence of expressions separated by ";".
     A final ";" means that no linefeed should be sent at the end.

   REM ...
     Does nothing and eats anything up to the end of the line.

   GOTO <line>
     Transfers execution at the specified program line.

   LIST
     Lists the program currently in memory.

   RUN
     Clears all variables and starts the program from the first instruction.

   IF <expr> THEN <stmt> [ : <stmt> ... ]
     If the condition is false then execution continues on next line.
     Otherwise execution continues on the same line with what follows
     "THEN". If what follows "THEN" is just a number then the parser
     assumes a "GOTO" statement. Sorry... no ELSE or multiline IF.

   GOSUB <line>
     Jumps at the subroutine at line xxx remembering where we are now.

   RETURN
     Returns from a subroutine and continues with the statement after
     the GOSUB command that invoked it.

   FOR <var> = <start_expr> TO <stop_expr> { STEP <inc_expr> }
     Begins a loop in which the specified variable will start from the
     value of start_expr and will end with the value of stop_expr,
     incrementing the value at every iteration by inc_expr. Note that
     the first iteration (with value start_expr) will be ALWAYS done,
     even if the value of stop_expr is lower and inc_expr is positive.
     If step_expr is omitted then the parser assumes "STEP 1".

   NEXT
     Makes a new iteration of last opened FOR or closes the FOR if the
     incremented variable reached the end of the loop. After incrementing
     the variable the execution resumes at the statement following the
     FOR statement.

   END
     Stops the program execution and returns to the interactive prompt.

   INPUT { <prompt> ; } <var>
     Asks the user for a value to put in the specified variable, first
     displaying the prompt. If no prompt is provided then the parser
     assumes "?".

Variables
=========
Variables names must match /[a-zA-Z_][a-zA-Z_0-9]*[%$]?/, i.e. they
must start with a letter or an underscore, may contain letters, digits
or underscore and may optionally end with "%" or "$".
Strings are double-quote delimited, with no provision for escape codes.

Predefined functions
====================

    LEN(s$)        length of a string
    STR$(x)        number->string conversion
    VAL(s$)        string->number conversion
    LEFT$(s$,n)    first n characters of a string
    RIGHT$(s$,n)   last n characters of a string
    MID$(s$,x,n)   n characters of a string starting from position x
    INT(x)         float->int conversion
    ABS(x)         absolute value
    SIN(x)         sine of x
    COS(x)         cosine of x
    TAN(x)         tangent of x
    ATN(x)         arc-tangent of x
    SQR(x)         square root of x
    LOG(x)         natural logarithm of x
    EXP(x)         exponential of x
    TIME(x)        current unix time (argument ignored)
    RND(x)         random number 0 < RND(x) < 1  (argument ignored)

Note that passing a wrong number of arguments is an error that will be
caught at *runtime* (and by python, using also an ugly error message).
There is no provision for user-defined functions... use subroutines
instead.

Operators
=========
Binary operators (listed by priority) are:

    * / MOD          Multiplication, division, modulo
    + -              Addition, subtraction
    <= < >= > <> =   Comparision
    AND              Bitwise AND
    OR               Bitwise OR
    XOR              Bitwise XOR

All comparision operators return 0 on false or -1 on true.

Available unary operators are NOT (logical not) and unary minus.
They have a priority *higher* than any binary operator.

Unsupported BASIC features
==========================
A lot :-) ... actually most of them... the most annoying and evident
is probably missing support for arrays (!!!!).

Oh... there is "LOAD", but no "SAVE" ... It's way better for you to
use your preferred editor for anything but just little tweaking.

Differences from BASIC
======================
Because I'm lazy the language is indeed "typeless" ... so variable names
may end with "%" or "$", but that character has no special meaning.

Technichalities
===============
There is tokenizer that recognizes identifiers, numbers, strings and
special characters (or two-character sequences like '>=').

The parser for expressions is a recursive-descent explicitly coded and
the result is stored in a tree of python class instances. The tree is
traversed for expression evaluation.

The parser for statements is non recursive (this BASIC is unstructured)
and stores the result in a single list of (line,stm) pairs.
The line may be -1 if the statement is a continuation (":") on the
previous line. The parsing of IF...THEN is hacked so there is no need
of ":" after the "THEN" even if that would have been more uniform for
this structure. "LIST" is hacked too to avoid generating annoying ":".

There are two stacks (one for GOSUBs and one for FORs) and one dictionary
for variables. There is also a dictionary for mapping line numbers to
indexes in the statement list.

All this is a somewhat strange mix. Normally compilers I wrote were
either interpreting directly a parsed tree (with both statements and
expressions being nodes of the tree) OR contained a virtual machine
and BOTH statements and expressions where compiled into instructions
for the VM.

Yes... I agree with you this mix is uglier than either.

One strange side-effect is that I can get back a listing from the
result of parsing, but because that for example for expressions only
the resulting tree is stored, the parenthesis that have been used
for grouping will disappear, but a lot of parenthesis will be added.
So if you enter:

   100 x = (((1))): y=-2

you will get in the listing something different...

   100 X = 1 : Y = -(2)

Also other syntax changes may appear in listing, for example...

   100 if x=3 then 220
   110 for x=1 to 10

will appear as

   100 IF (X = 3) THEN  GOTO 220
   110 FOR X=1 TO 10 STEP 1

because also statement objects don't remember exactly the text
they were parsed from.

In Apple ][ BASIC the approach was different. What it was stored
was just the sequence of tokens... everything else was done at
runtime doing for example expr evaluation *while* parsing the
sequence of tokens.


BASIC Examples
==============
There are four programs included as an example:

  menu.bas
  Just asks which one of the other programs the user would like to run

  hanoi.bas
  Classical towers of hanoi problem. Input is the number of rings.

  primes.bas
  Counts how many prime numbers do exist below a certain limit.

  ttt.bas
  Tic-Tac-Toe player (user against computer). Look ma... no arrays!

To run them just run the interpreter with "python basic.py" and
then issue the commands

  load "menu.bas"
  run


FINAL WORDS
===========
If you have any comment or question about all this feel free to drop
an email or even insult me publically in comp.lang.python ;-)

I've really no idea if this is helpful or just adding noise for someone
that is new in this area. But I thought it would have been better to
allow others to see it.

Andrea Griffini
agriff@tin.it



DISCLAIMER
==========
Blah blah blah blah blah ...

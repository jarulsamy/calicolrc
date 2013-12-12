.SET MODE GRAPHICS
.ORIG x3000
        CLEAR
        AND R0,R0,#0
        AND R1,R1,#0
        AND R3,R3,#0
        LD R4,ROWS
LOOP:   POKE R0,R1,R3
        ADD R3,R3,#1
        ADD R0,R0,#1
        ADD R5,R1,R4
        BRz NEXT
        BR LOOP
        HALT
NEXT    AND R0,R0,#0
        ADD R1,R1,#1
        BR LOOP
ROWS .FILL #-64
.END

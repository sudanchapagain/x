; 8085 MICROPROCESSOR by Intel in 1976.

; - Accumulator (Acc): Stores intermediate results during calculations.
; - Registers: General-purpose (B, C, D, E, H, L) and special-purpose (SP, PC).
; - ALU: Performs arithmetic and logic operations.
; - PC (Program Counter): Holds the address of the next instruction.
; - IR (Instruction Register): Holds the current instruction.
; - Timing and Control Unit: Manages the sequence of operations.




;   INSTRUCTION CYCLE

;   +----------------------------+
;   | Fetch | Decode | Execute   |
;   +----------------------------+




;   FLAGS REGISTER

;   - S  (Sign): Indicates the sign of the result.
;   - Z  (Zero): Set if the result is zero.
;   - AC (Auxiliary Carry): Used for BCD arithmetic.
;   - P  (Parity): Set if the result has an even number of 1s.
;   - CY (Carry): Set if there is a carry out.




;_______________________________________________________________________________
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;                              EXAMPLE PROGRAMS



;- 16 bit addition with carry

        LXI H, EEEEH
        LXI D, FFFFH
        DAD
        HLT



;- Transfer entire block of data to the location 3000H to 3005H in inverse order
;- Assumption is data is stored in 2000H to 2005H

        LXI B, 2000
        LXi D, 3005
        MVI L, 06
BACK:   LDAX B
        STAX D
        INX B
        DCX 0
        DCR L
        JNZ BACK
        HLT



;- Sort numbers in ascending order
;- (data in 1050H - 1055H)

        MVI C, 05
LOOP1:  MOV B, C
        LXI H, 1050
LOOP2:  MOV A,M
        INX H
        CMP M
        JC SKIP
        MOV D, M
        MOV M, A
        DCX H
        MOV M, D
        INX H
SKIP:   DCR B
        JNZ LOOP2
        DCR C
        JNC LOOP1
        HLT



;- Square root of given number

        LDA 2000
        MVI B, 01
        MVI C, 00
LOOP:   INR C
        SUB B
        JZ HERE
        INR B
        INR B
        JMP LOOP
HERE:   MOV A, C
        STA 2005
        HLT
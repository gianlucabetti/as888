000001000000             CMD        PROMPT('Convert PDF to PCL')
000002000000
000003000000             PARM       KWD(PDF) TYPE(*CHAR) LEN(63) MIN(1) +
000004000000                          CHOICE('PDF Path') PROMPT('PDF name')
000005000000             PARM       KWD(PCL) TYPE(*CHAR) LEN(63) MIN(1) +
000006000000                          CHOICE('PCL Path') PROMPT('PCL name')
000007000000             PARM       KWD(DEVICE) TYPE(*CHAR) LEN(20) +
000008000000                          PROMPT('Device Name')
000009000000             PARM       KWD(RESOLUTION) TYPE(*DEC) LEN(4 0) +
000010000000                          RANGE(100 1200) CHOICE('100-1200') +
000011000000                          PROMPT(RISOLUZIONE)
000012000000             PARM       KWD(OUTPUT) TYPE(*CHAR) LEN(8) RSTD(*YES) +
000013000000                          DFT(*NONE) VALUES(* *NONE *PRINT) +
000014000000                          PROMPT('Log')

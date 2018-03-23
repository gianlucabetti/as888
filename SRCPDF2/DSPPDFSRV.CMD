000100130206             CMD        PROMPT('Display PDF Server')
000200000000             PARM       KWD(LOG) TYPE(*CHAR) LEN(63) RSTD(*NO) +
000300000000                          DFT(*CURRENT) PROMPT('Log')
000400000000             PARM       KWD(OUTPUT) TYPE(*CHAR) LEN(6) RSTD(*YES) +
000500000000                          DFT(*) VALUES(* *PRINT) +
000600000000                          PROMPT('Stampa Log')

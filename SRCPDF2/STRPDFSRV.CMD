000100130206             CMD        PROMPT('Start PDF Server')
000200000000
000300000000             PARM       KWD(MODE) TYPE(*CHAR) LEN(6) RSTD(*YES) +
000400000000                          DFT(*BATCH) VALUES(*INT *BATCH) +
000500000000                          PROMPT('Modalità server')
000600000000
000700000000             PARM       KWD(CONSOLE) TYPE(*CHAR) LEN(4) RSTD(*YES) +
000800000000                          DFT(*NO) VALUES(*YES *NO) PROMPT('Console +
000900000000                          Java')

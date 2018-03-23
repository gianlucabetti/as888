000001000000             CMD        PROMPT('Messaggio prova PDF Server')
000002000000
000003000000             PARM       KWD(MSG) TYPE(*PNAME) LEN(63) DFT('Test +
000004000000                          Server') MIN(0) PROMPT('Messaggio')
000005000000
000006000000             PARM       KWD(TIMEOUT) TYPE(*DEC) LEN(3) DFT(60) +
000007000000                          RANGE(1 300) MIN(0) PROMPT('Timeout')
000008000000
000009000000             PARM       KWD(JDEBUG) TYPE(*CHAR) LEN(4) RSTD(*YES) +
000010000000                          DFT(*NO) VALUES(*YES *NO *PRT) +
000011000000                          PROMPT('Debug Java')
000012000000

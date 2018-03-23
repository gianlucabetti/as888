000100130206             CMD        PROMPT('Messaggio prova PDF Server')
000200000000
000300000000             PARM       KWD(MSG) TYPE(*PNAME) LEN(63) DFT('Test +
000400000000                          Server') MIN(0) PROMPT('Messaggio')
000500000000
000600000000             PARM       KWD(TIMEOUT) TYPE(*DEC) LEN(3) DFT(60) +
000700000000                          RANGE(1 300) MIN(0) PROMPT('Timeout')
000800000000
000900000000             PARM       KWD(JDEBUG) TYPE(*CHAR) LEN(4) RSTD(*YES) +
001000000000                          DFT(*NO) VALUES(*YES *NO *PRT) +
001100000000                          PROMPT('Debug Java')
001200000000

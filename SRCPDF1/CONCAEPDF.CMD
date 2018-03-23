000100130207             CMD        PROMPT('Concatenate PDF')
000200000000
000300130207             PARM       KWD(INPDF) TYPE(*PNAME) LEN(63) MIN(1) +
000400130219                          EXPR(*YES) PROMPT('Input PDF List')
000500000000
000600130207             PARM       KWD(OUTPDF) TYPE(*PNAME) LEN(63) MIN(1) +
000700130207                          PROMPT('Output PDF')
000800130207
000900130207             PARM       KWD(JSERVER) TYPE(*CHAR) LEN(4) RSTD(*YES) +
001000130207                          DFT(*NO) VALUES(*YES *NO) PROMPT('Use PDF +
001100130207                          Server')
001200000000
001300130207             PARM       KWD(JDEBUG) TYPE(*CHAR) LEN(4) RSTD(*YES) +
001400130207                          DFT(*NO) VALUES(*YES *NO) PROMPT('Debug +
001500130207                          Java')
001600000000

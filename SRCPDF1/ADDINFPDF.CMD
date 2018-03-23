000100130207             CMD        PROMPT('Add Info data to Pdf')
000200000000
000300130219             PARM       KWD(SPCINPDF) TYPE(*PNAME) LEN(63) MIN(1) +
000400130219                          EXPR(*YES) PROMPT('PDF INPUT List')
000500000000
000600130206             PARM       KWD(SPCINDTA) TYPE(*PNAME) LEN(63) MIN(1) +
000700130219                          EXPR(*YES) PROMPT('PDF INFO DATA List')
000800000000
000900000000             PARM       KWD(SPCOUPDF) TYPE(*PNAME) LEN(63) MIN(1) +
001000130219                          EXPR(*YES) PROMPT('PDF OUTPUT List')
001100000000
001200130206             PARM       KWD(TMPPATH) TYPE(*PNAME) LEN(63) MIN(1) +
001300130206                          PROMPT('Temporary Path')
001400000000
001500130207             PARM       KWD(JSERVER) TYPE(*CHAR) LEN(4) RSTD(*YES) +
001600130207                          DFT(*NO) VALUES(*YES *NO) PROMPT('Use PDF +
001700130207                          Server')
001800130207
001900130206             PARM       KWD(JDEBUG) TYPE(*CHAR) LEN(4) RSTD(*YES) +
002000130206                          DFT(*NO) VALUES(*YES *NO *PRT) +
002100130206                          PROMPT('Debug Java')
002200000000

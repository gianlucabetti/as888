000100130219  CMD   PROMPT('Execute SQL')
000200000000
000300000000  PARM  KWD(SQLSTMT)   TYPE(*CHAR) LEN(5000) MIN(1) +
000400000000                       EXPR(*YES) PROMPT('SQL statement')
000500000000
000600000000  PARM  KWD(LOGSQL)    TYPE(*CHAR) LEN(4) RSTD(*YES) +
000700000000                       DFT(*YES) VALUES(*YES *NO *RMT) +
000800000000                       PROMPT('Log SQL command')
000900000000
001000000000  PARM  KWD(SQLLOCVAL) TYPE(*CHAR) LEN(4) RSTD(*YES) +
001100000000                       DFT(*NO) VALUES(*YES *NO) +
001200000000                       PROMPT('Use local SQL sys values')
001300000000
001400000000  PARM  KWD(ESCAPE)    TYPE(*CHAR) LEN(4) RSTD(*YES) +
001500000000                       DFT(*NO) VALUES(*NO *ERR *ALL) +
001600000000                       PROMPT('Escape SQL errors')

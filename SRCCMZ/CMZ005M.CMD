000100900227             CMD        PROMPT('Immissione file per invio')
000200900426             PARM       KWD(DST) TYPE(*DEC) LEN(3 0) MIN(0) MAX(1) +
000300900426                          ALWVAR(*YES) PROMPT('Destinatario')
000400900426             PARM       KWD(FLD) TYPE(*CHAR) LEN(10) MIN(1) MAX(1) +
000500900426                          ALWVAR(*YES) PROMPT('Dal file')
000600900426             PARM       KWD(MBD) TYPE(*CHAR) LEN(10) MIN(1) MAX(1) +
000700900426                          ALWVAR(*YES) PROMPT('Dal membro')
000800900426             PARM       KWD(LBD) TYPE(*CHAR) LEN(10) DFT(*LIBL) +
000900900426                          MIN(0) MAX(1) ALWVAR(*YES) PROMPT('Dalla +
001000900426                          libreria')
001100900426             PARM       KWD(FLA) TYPE(*CHAR) LEN(10) MIN(0) MAX(1) +
001200900426                          ALWVAR(*YES) PROMPT('Al file')
001300900412             PARM       KWD(MBA) TYPE(*CHAR) LEN(10) MIN(0) MAX(1) +
001400900426                          ALWVAR(*YES) PROMPT('All membro')
001500900412             PARM       KWD(LBA) TYPE(*CHAR) LEN(10) MIN(0) MAX(1) +
001600900426                          ALWVAR(*YES) PROMPT('Alla libreria')
001700900227             PARM       KWD(ORI) TYPE(*DEC) LEN(4 0) MIN(0) MAX(1) +
001800900426                          ALWVAR(*YES) PROMPT('Ora invio')
001900900426             PARM       KWD(DTI) TYPE(*DEC) LEN(7 0) MIN(0) MAX(1) +
002000900426                          ALWVAR(*YES) PROMPT('Data invio')
002100900227             PARM       KWD(URG) TYPE(*CHAR) LEN(1) RSTD(*YES) +
002200900227                          DFT('N') VALUES('S' 'N') MIN(0) MAX(1) +
002300900426                          ALWVAR(*YES) PROMPT('Urgente')
002400900227             PARM       KWD(ADD) TYPE(*CHAR) LEN(1) RSTD(*YES) +
002500900227                          DFT('A') VALUES('A' 'R') MIN(0) MAX(1) +
002600900426                          ALWVAR(*YES) PROMPT('ADD/Replace')
002700940418             PARM       KWD(CLR) TYPE(*CHAR) LEN(1) RSTD(*YES) +
002800940418                          DFT('N') VALUES('S' 'N' '1' '2') MIN(0) +
002900940418                          MAX(1) ALWVAR(*YES) PROMPT('Pulizia file +
003000940418                          spedito')
003100900227             PARM       KWD(FTR) TYPE(*CHAR) LEN(1) RSTD(*YES) +
003200900227                          DFT('S') VALUES('S' 'N') MIN(0) MAX(1) +
003300900426                          ALWVAR(*YES) PROMPT('Copia su file transito')
003400940418             PARM       KWD(ERR) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
003500940418                          RSTD(*NO) MIN(1) MAX(1) ALWVAR(*YES) +
003600940418                          PROMPT('Flag errore')
003700900412             PARM       KWD(AZN) TYPE(*CHAR) LEN(4) MIN(0) MAX(1) +
003800900426                          ALWVAR(*YES) PROMPT('Azione')
003900900412             PARM       KWD(SYF) TYPE(*CHAR) LEN(10) MIN(0) MAX(1) +
004000900426                          ALWVAR(*YES) PROMPT('Sistema informativo')
004100900508             PARM       KWD(REC) TYPE(*DEC) LEN(7 0) MIN(0) MAX(1) +
004200900508                          ALWVAR(*YES) PROMPT('Numero record')

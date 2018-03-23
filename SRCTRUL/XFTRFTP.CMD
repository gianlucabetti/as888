000100061214             CMD        PROMPT('Invio file DB2/400 via FTP')
000200000000
000300061214             PARM       KWD(FTPADR) TYPE(*NAME) MIN(1) MAX(1) +
000400060720                          FILE(*OUT) EXPR(*YES) PROMPT('Nome file +
000500060720                          in spool')
000600991031
000700060720             PARM       KWD(TOSTMF) TYPE(*PNAME) LEN(256) MIN(1) +
000800060720                          EXPR(*YES) CASE(*MIXED) +
000900060720                          PROMPT('Indirizziario IFS')
001000000312
001100060720             PARM       KWD(WSCST) TYPE(Q1) MIN(1) MAX(1) +
001200060720                          PROMPT('Oggetto personalizzazione')
001300040130 Q1:         QUAL       TYPE(*NAME) MIN(1) EXPR(*YES)
001400060720             QUAL       TYPE(*NAME) DFT(*LIBL) SPCVAL((*LIBL) +
001500060720                          (*CURLIB)) MIN(0) EXPR(*YES) PROMPT('Libreria')
001600991031
001700040130             PARM       KWD(JOB) TYPE(Q2) DFT(*) SNGVAL((*)) MAX(1) +
001800060720                          PROMPT('Nome Job')
001900991031 Q2:         QUAL       TYPE(*NAME) LEN(10) MIN(1) EXPR(*YES)
002000040130             QUAL       TYPE(*NAME) LEN(10) MIN(1) EXPR(*YES) +
002100060720                          PROMPT('Utente')
002200040130             QUAL       TYPE(*CHAR) LEN(6) RANGE(000000 999999) +
002300040130                          SPCVAL(('      ')) EXPR(*YES) +
002400060720                          PROMPT('Numero')
002500000000
002600040130             PARM       KWD(SPLNBR) TYPE(*DEC) LEN(4) DFT(*LAST) +
002700040130                          RANGE(1 9999) SPCVAL((*ONLY 0) (*LAST +
002800060720                          -1)) PROMPT('Numero file in spool')
002900991031
003000040130             PARM       KWD(REPLACE) TYPE(*CHAR) LEN(4) RSTD(*YES) +
003100040130                          DFT(*YES) VALUES(*YES *NO) +
003200060720                          PROMPT('Replace STMF in IFS')
003300000000
003400040130             PARM       KWD(DEBUG) TYPE(*CHAR) LEN(4) RSTD(*YES) +
003500040130                          DFT(*NO) VALUES(*YES *NO) PMTCTL(*PMTRQS) +
003600060720                          PROMPT('Abilitare debug print')

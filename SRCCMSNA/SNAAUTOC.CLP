000100000000PGM
000200000000DCL &USER *CHAR 10
000300000000DCL &CODA *CHAR 10
000400000000DCL &FILE *CHAR 10
000500000000DCL &MBR  *CHAR 10
000600000000DCL &IDEL *CHAR 8
000700000000DCL &IDGR *CHAR 8
000800000000DCL &DATI *CHAR 100
000900000000DCL &MIC  *CHAR 7
001000000000DCL &KEY  *CHAR 4
001100000000DCL &FILSAV *CHAR 1
001200000000             RTVJOBA    USER(&USER)
001300000000             RTVUSRPRF  MSGQ(&CODA)
001400000000             RTVDTAARA  DTAARA(*LDA (1 1)) RTNVAR(&FILSAV)
001500000000             RTVDTAARA  DTAARA(*LDA (2 10)) RTNVAR(&FILE)
001600000000             RTVDTAARA  DTAARA(*LDA (22 10)) RTNVAR(&MBR)
001700000000             RTVDTAARA  DTAARA(*LDA (40 8)) RTNVAR(&IDEL)
001800000000             RTVDTAARA  DTAARA(*LDA (48 8)) RTNVAR(&IDGR)
001900000000RCV:       RCVMSG MSGQ(*LIBL/&CODA) WAIT(*MAX) RMV(*NO) +
002000000000             KEYVAR(&KEY) MSGDTA(&DATI) MSGID(&MIC)
002100860613  /* riceve il messggio senza rimuoverlo    */
002200000000            IF (&MIC = 'CPI8070')  DO
002300000000           IF (&FILE = %SST(&DATI 33 10)) DO
002400000000           IF (&FILSAV='S') GOTO VAI
002500000000           ELSE    DO
002600000000           IF (&MBR = %SST(&DATI 43 10)) GOTO VAI
002700000000           ELSE GOTO RCV
002800000000                   ENDDO
002900000000                                         ENDDO
003000000000                 ELSE GOTO RCV
003100000000                                   ENDDO
003200000000            ELSE GOTO RCV
003300000000VAI:       RCVMSG MSGQ(*LIBL/&CODA) MSGKEY(&KEY)    /* RIMUOVE MSG OK*/
003400000000CHGMSGQ    MSGQ(*LIBL/&CODA) RESET(*YES)            /*RIPRISTINA CODA*/
003500000000             CHGDTAARA  DTAARA(*LDA (56 6)) VALUE(%SST(&DATI 53 6))
003600000000DLTF       FILE(QTEMP/SNAJOB)
003700000000             MONMSG CPF0000
003800000000CRTSRCPF   FILE(QTEMP/SNAJOB) MBR(SNAJOB) MAXMBRS(1)
003900000000OVRDBF     FILE(SNAJOB) TOFILE(QTEMP/SNAJOB) MBR(SNAJOB)
004000000000CALL       PGM(*LIBL/SNAAUTO)
004100000000SBMNETJOB  FILE(QTEMP/SNAJOB) TOUSRID((&IDEL &IDGR)) MBR(SNAJOB)
004200000000ENDPGM

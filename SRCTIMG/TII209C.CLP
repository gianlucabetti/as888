000100000000             PGM        PARM(&KPJBA)
000200000000  /*-MPH944C-------------------------------------------------------*/
000300000000  /*                                                               */
000400000000  /*           - LANCIO APERTURA NUOVO ESERCIZIO                   */
000500000000  /*           ----------------------------------       AZ.: H994  */
000600000000  /*         PGM.:    MPH944R   - APERTURA NUOVO ESERCIZIO         */
000700000000  /*                                                               */
000800000000  /*---------------------------------------------------------------*/
000900000000      QSYS38/DCLDTAARA  DTAARA(MPHDT1)
001000000000/* CPF0785  Il comando DCLDTAARA non può essere convertito. */
001100000000             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
001200000000   /*                                                              */
001300000000  /*--------- ALLOCAZIONE FILE SALDI MATERIE PRIME ----------------*/
001400990208ALCOBJ     OBJ((*LIBL/TMSMP00F *FILE *EXCL))
001500000000             MONMSG     MSGID(CPF0000) EXEC(DO)
001600891127             CHGVAR     (%SST(&KPJBA 40 7)) VALUE('KPJB207')
001700000000             CHGVAR     VAR(%SST(&KPJBA 453 50)) VALUE('*- Azione : +
001800990317                          TII209R, non eseguibile -RIPROVARE- *')
001900000000             GOTO       CMDLBL(FINE)
002000000000             ENDDO
002100000000
002200000000  /*--------- CONTROLLO DATA AREA X APERTURA NUOVO ESERCIZIO ------*/
002300000000      QSYS38/RCVDTAARA  DTAARA(MPHDT1)
002400000000/* CPF0785  Il comando RCVDTAARA non può essere convertito. */
002500000000
002600000000ALCOBJ     OBJ((*LIBL/MPHDT1 *DTAARA *EXCL))
002700000000             MONMSG     MSGID(CPF0000) EXEC(DO)
002800891127             CHGVAR     (%SST(&KPJBA 40 7)) VALUE('KPJB207')
002900000000             CHGVAR     VAR(%SST(&KPJBA 453 50)) VALUE('*- Azione : +
003000990317                          TII209C, non eseguibile -RIPROVARE- *')
003100000000             GOTO       CMDLBL(FINE)
003200000000             ENDDO
003300000000
003400990317CALL       PGM(*LIBL/TII209R) PARM(&KPJBA)
003500000000
003600000000             CHGVAR     VAR(%SST(&MPHDT1 1 5)) VALUE(00002)
003700000000             CHGVAR     VAR(%SST(&MPHDT1 9 4)) VALUE(0101)
003800000104             CHGVAR     VAR(%SST(&MPHDT1 13 2)) VALUE(%SST(&KPJBA +
003900000104                          249 2))
004000000104             CHGVAR     VAR(%SST(&MPHDT1 18 2)) VALUE(%SST(&KPJBA +
004100000104                          249 2))
004200000104             CHGVAR     VAR(%SST(&MPHDT1 501 10)) VALUE(%SST(&KPJBA +
004300000104                          1 10))
004400000000 /*------------ AGGIORNA DATA AREA MATERIE PRIME ------------------*/
004500000000      QSYS38/SNDDTAARA  DTAARA(MPHDT1)
004600000000/* CPF0785  Il comando SNDDTAARA non può essere convertito. */
004700000000
004800000000 /*----------- RILASCIO VINCOLI DTAARA E FILE ---------------------*/
004900000000
005000990208DLCOBJ     OBJ((*LIBL/TMSMP00F *FILE *EXCL))
005100000000DLCOBJ     OBJ((*LIBL/MPHDT1 *DTAARA *EXCL))
005200000000
005300000000  /*--- USCITA PROGRAMMA ------------------------------------------*/
005400000000 FINE:       ENDPGM

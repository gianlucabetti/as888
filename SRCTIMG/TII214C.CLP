000100000000             PGM        PARM(&KPJBA)
000200000000  /*-MPH945C-------------------------------------------------------*/
000300000000  /*                                                               */
000400860129  /*           - LANCIO APERTURA NUOVO ESERCIZIO    _              */
000500860129  /*           - GENERA MOVIMENTI GIACENZA INIZIALE _              */
000600860129  /*           --------------------------------------   AZ.: H945  */
000700860129  /*         PGM.: MPH945R - GENERAZ. MOVIMENTI GIAC. INIZIALE     */
000800000000  /*                                                               */
000900000000  /*---------------------------------------------------------------*/
001000860121      QSYS38/DCLDTAARA  DTAARA(MPHDT1)
001100000000/* CPF0785  Il comando DCLDTAARA non può essere convertito. */
001200000000             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
001300000000   /*                                                              */
001400000000  /*--------- ALLOCAZIONE FILE SALDI MATERIE PRIME ----------------*/
001500990422ALCOBJ     OBJ((*LIBL/TMSMP00F *FILE *EXCL))
001600000000             MONMSG     MSGID(CPF0000) EXEC(DO)
001700891127             CHGVAR     (%SST(&KPJBA 40 7)) VALUE('KPJB207')
001800000000             CHGVAR     VAR(%SST(&KPJBA 453 50)) VALUE('*- Azione : +
001900990422                          TII214R, non eseguibile -RIPROVARE- *')
002000000000             GOTO       CMDLBL(FINE)
002100000000             ENDDO
002200000000
002300860125  /*                                                               */
002400000000  /*--------- ALLOCAZIONE FILE MOVIMENTI MATERIE PRIME-------------*/
002500990422ALCOBJ     OBJ((*LIBL/TMMMP00F *FILE *EXCL))
002600000000             MONMSG     MSGID(CPF0000) EXEC(DO)
002700891127             CHGVAR     (%SST(&KPJBA 40 7)) VALUE('KPJB207')
002800000000             CHGVAR     VAR(%SST(&KPJBA 453 50)) VALUE('*- Azione : +
002900990422                          TII214R, non eseguibile -RIPROVARE- *')
003000000000             GOTO       CMDLBL(FINE)
003100000000             ENDDO
003200000000
003300860125  /*--------- ELIMINAZIONE MOVIM. GIAC.INIZ. IN RIPARTENZA --------*/
003400860125
003500990422CALL       PGM(*LIBL/TII214R0) PARM(&KPJBA)
003600860125
003700860125  /*                                                               */
003800860125
003900990422CALL       PGM(*LIBL/TII214R) PARM(&KPJBA)
004000860125
004100860121  /*--------- CONTROLLO DATA AREA X VAR.ANNO GIACENZA INIZIALE ----*/
004200860121
004300860121      QSYS38/RCVDTAARA  DTAARA(MPHDT1)
004400000000/* CPF0785  Il comando RCVDTAARA non può essere convertito. */
004500000000ALCOBJ     OBJ((*LIBL/MPHDT1 *DTAARA *EXCL))
004600860121             MONMSG     MSGID(CPF0000) EXEC(DO)
004700891127             CHGVAR     (%SST(&KPJBA 40 7)) VALUE('KPJB207')
004800860121             CHGVAR     VAR(%SST(&KPJBA 453 50)) VALUE('*- Azione : +
004900990422                          TII214C, Data Area non Disponibile- *')
005000860121             GOTO       CMDLBL(FINE)
005100860121             ENDDO
005200860121             CHGVAR     VAR(%SST(&MPHDT1 151 2)) VALUE(%SST(&KPJBA +
005300860121                          269 2))
005400860121      QSYS38/SNDDTAARA  DTAARA(MPHDT1)
005500000000/* CPF0785  Il comando SNDDTAARA non può essere convertito. */
005600000000
005700860121 /*----------- RILASCIO VINCOLI DTAARA E FILE ---------------------*/
005800860121
005900860121DLCOBJ     OBJ((*LIBL/MPHDT1 *DTAARA *EXCL))
006000860121
006100990422DLCOBJ     OBJ((*LIBL/TMSMP00F *FILE *EXCL))
006200990422DLCOBJ     OBJ((*LIBL/TMMMP00F *FILE *EXCL))
006300860121
006400000000  /*--- USCITA PROGRAMMA ------------------------------------------*/
006500000000 FINE:       ENDPGM

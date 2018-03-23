000100000000/*----------------------------------------------------------------------------*/
000200000000/* REPORT COMMERCIALE - LANCIO CREAZIONE WFILE COMMERCIALI                    */
000300000000/*----------------------------------------------------------------------------*/
000400000000 TIST11C:    PGM        PARM(&KPJBA)
000500000000
000600000000/* Dichiarazione variabili */
000700000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000800110127             DCL        VAR(&AS400)   TYPE(*CHAR) LEN(8)
000900110127 /* Reprisco il sistema AS/400 corrente */
001000110127             RTVNETA    SYSNAME(&AS400)
001100110127
001200000000
001300000000/* Cancella e crea il file di lavoro commerciali */
001400110131 /* Se presenti elimino dalla QTEMP l'eventuale file WFCOM* già esistente */
001500110131             DLTF       FILE(QTEMP/WFCOM*)
001600110131             MONMSG     MSGID(CPF0000)
001700110131
001800110131 /* Duplica in QTEMP del file WFCOM*  */
001900110131             IF         COND(&AS400 *EQ 'AS888') THEN(DO)
002000110131             CRTDUPOBJ  OBJ(WFCOM*) FROMLIB(GAITRAAZP) +
002100110131                          OBJTYPE(*FILE) TOLIB(QTEMP)
002200110131             ENDDO
002300110131             IF         COND(&AS400 *EQ 'SETRAS') THEN(DO)
002400110131             CRTDUPOBJ  OBJ(WFCOM*) FROMLIB(GAITRAAZM) OBJTYPE(*FILE) +
002500110131                          TOLIB(QTEMP)
002600110131             ENDDO
002700110131
002800020409             CALL       PGM(TIST11R) PARM(&KPJBA)
002900000000
003000000000/* Crea il file del report - Parte 1 */
003100020409             CALL       PGM(TIST12R) PARM(&KPJBA)
003200000000
003300000000/* Crea il file del report - Parte 2 */
003400020409             CALL       PGM(TIST13R) PARM(&KPJBA)
003500981118
003600981118/* Sistema organigramma report */
003700020408             CALL       PGM(TIST14R) PARM(&KPJBA)
003800110127
003900110127/* Crea il file del report - Parte 3 */
004000110127
004100110127 /* Se presenti elimino dalla QTEMP gli eventuali file WFRCA* già esistenti */
004200110127 /* Se presenti elimino dalla QTEMP gli eventuali file WFRPA* già esistenti */
004300110127             DLTF       FILE(QTEMP/WFRCA*)
004400110127             MONMSG     MSGID(CPF0000)
004500110127
004600110127             DLTF       FILE(QTEMP/WFRPA*)
004700110127             MONMSG     MSGID(CPF0000)
004800110127
004900110127
005000110127 /* Duplica in QTEMP dei file WFRCA* WFRPA* */
005100110127             IF         COND(&AS400 *EQ 'AS888') THEN(DO)
005200110927             CRTDUPOBJ  OBJ(WFRCA*) FROMLIB(UNITRAGRP) OBJTYPE(*FILE) +
005300110127                          TOLIB(QTEMP)
005400110927             CRTDUPOBJ  OBJ(WFRPA*) FROMLIB(UNITRAGRP) OBJTYPE(*FILE) +
005500110127                          TOLIB(QTEMP)
005600110127             ENDDO
005700110127             IF         COND(&AS400 *EQ 'SETRAS') THEN(DO)
005800110927             CRTDUPOBJ  OBJ(WFRCA*) FROMLIB(UNITRAGRU) OBJTYPE(*FILE) +
005900110127                          TOLIB(QTEMP)
006000110927             CRTDUPOBJ  OBJ(WFRPA*) FROMLIB(UNITRAGRU) OBJTYPE(*FILE) +
006100110127                          TOLIB(QTEMP)
006200110127             ENDDO
006300110131
006400110127
006500110127/* Richiamo programma */
006600110127
006700110127             CALL       PGM(TIST24R) PARM(&KPJBA)
006800110127
006900981118
007000000000             RCLRSC     LVL(*CALLER)
007100000000             ENDPGM

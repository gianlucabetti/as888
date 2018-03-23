000100900427/*                                                                   */
000200900427/*  ESEGUE  LA STAMPA DELLE ANOMALIE                                 */
000300900427/*    SE CI SONO ( FLG= 1)                                           */
000400900427/*     INVIA STAMPA A MASTER                                         */
000500900427/*                                                                   */
000600900427             PGM        PARM(&DST &DAT &ORA)
000700900427             DCL        VAR(&DST) TYPE(*CHAR) LEN(3)
000800900509             DCL        VAR(&LOG) TYPE(*CHAR) LEN(10)
000900900427             DCL        VAR(&DAT) TYPE(*CHAR) LEN(7)
001000900427             DCL        VAR(&ORA) TYPE(*CHAR) LEN(6)
001100900427             DCL        VAR(&FLG) TYPE(*CHAR) LEN(1)
001200900427             DCL        VAR(&IND1) TYPE(*CHAR) LEN(8)
001300900427             DCL        VAR(&IND2) TYPE(*CHAR) LEN(8)
001400900427             DCL        VAR(&FMS) TYPE(*CHAR) LEN(1)
001500900427             DCL        VAR(&TXT) TYPE(*CHAR) LEN(80)
001600900427
001700901003             DLYJOB     DLY(30)
001800901107             CLRPFM     FILE(CMCTL00F)
001900900427             CALL       PGM(CMZ214R) PARM(&DST &DAT &ORA &FLG)
002000901107             CALL       PGM(CMZ214R1) PARM(&DST &DAT &ORA &FLG)
002100900427
002200900509             RTVDTAARA  DTAARA(CMZ001 (13 1)) RTNVAR(&FMS)
002300900427             IF         COND(&FLG *NE ' ' *AND &FMS *NE 'S') THEN(DO)
002400900509             RTVDTAARA  DTAARA(CMZ001 (14 8)) RTNVAR(&IND1)
002500900509             RTVDTAARA  DTAARA(CMZ001 (22 8)) RTNVAR(&IND2)
002600900509             RTVDTAARA  DTAARA(CMZ001 (33 3)) RTNVAR(&DST)
002700900509             CHGVAR     VAR(&LOG) VALUE(&DST *CAT '       ')
002800900427             SNDNETSPLF FILE(QSYSPRT) TOUSRID((&IND1 &IND2)) +
002900900427                          SPLNBR(*LAST)
003000900427             MONMSG     MSGID(CPF0000)
003100900427             CHGVAR     VAR(&TXT) VALUE('Lista errori in coda di +
003200900427                          distribuzione.')
003300900427             CALL       PGM(CMZ003C) PARM(' ' &TXT &LOG '2')
003400900427             ENDDO
003500900427             ENDPGM
003600900427

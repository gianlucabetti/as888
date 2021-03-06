000100000000/*----------------------------------------------------------------------------*/
000200000000/* IMPORTAZIONE SCHEDE CONTABILI SU OTTICO                                    */
000300000000/*----------------------------------------------------------------------------*/
000400000000 TREC31C:    PGM        PARM(&KPJBA)
000500000000
000600000000/* DICHIARAZIONE VARIABILI */
000700000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(512)
000800000000             DCL        VAR(&MES) TYPE(*DEC)  LEN(2)
000900000000             DCL        VAR(&CODA)      TYPE(*CHAR) LEN(10)
001000000000             DCL        VAR(&SPLFILE)   TYPE(*CHAR) LEN(10)
001100000000             DCL        VAR(&FORMTYPE)  TYPE(*CHAR) LEN(10)
001200000000             DCL        VAR(&STATO)     TYPE(*CHAR) LEN(3)
001300000000             DCL        VAR(&LIBOUT)    TYPE(*CHAR) LEN(10)
001400000000             DCL        VAR(&FILOUT)    TYPE(*CHAR) LEN(10)
001500000000             DCL        VAR(&LIBOUTQ)   TYPE(*CHAR) LEN(10)
001600000000             DCL        VAR(&NAMOUTQ)   TYPE(*CHAR) LEN(10)
001700000000
001800000000/* VALORIZZAZIONI VARIABILI */
001900000000             CHGVAR     VAR(&MES) VALUE(00)
002000000000
002100000000/* CANCELLA FILES PER OTTICO */
002200000000             DLTF       FILE(LIBASP4/SCHCON00)
002300000000             MONMSG     MSGID(CPF0000)
002400000000
002500000000/* CONVERTE SPOOL PER OTTICO E LI SPOSTA NELLA CODA "SCHCONTAB */
002600000822             CHGVAR     VAR(&CODA) VALUE('OTTICOIN')
002700000000             CHGVAR     VAR(&SPLFILE)  VALUE('NDC132P')
002800000000             CHGVAR     VAR(&FORMTYPE) VALUE('MastriCD')
002900000000             CHGVAR     VAR(&STATO)    VALUE('HLD')
003000000000             CHGVAR     VAR(&LIBOUT)   VALUE('LIBASP4')
003100000000             CHGVAR     VAR(&FILOUT)   VALUE('SCHCONT')
003200000000             CHGVAR     VAR(&LIBOUTQ)  VALUE('GAITRAOBJ ')
003300000822             CHGVAR     VAR(&NAMOUTQ)  VALUE('OTTICOOUT ')
003400000000             CALL       PGM(TREC30C) PARM(&CODA &SPLFILE &FORMTYPE +
003500000000                          &STATO &LIBOUT &FILOUT &LIBOUTQ &NAMOUTQ +
003600000000                          &MES)
003700000000
003800000000             ENDPGM

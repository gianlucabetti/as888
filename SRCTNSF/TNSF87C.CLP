000001980108/* */
000002980108/* CONVERSIONE SPOOL FATTURE CODIFICATE SDI  */
000003980108/* */
000100980108 TNSF87C:    PGM        PARM(&KPJBA)
000200980108             DCL        VAR(&KPJBA)     TYPE(*CHAR) LEN(502)
000300980108             DCL        VAR(&CODA)      TYPE(*CHAR) LEN(10)
000400980108             DCL        VAR(&SPLFILE)   TYPE(*CHAR) LEN(10)
000401980108             DCL        VAR(&FORMTYPE)  TYPE(*CHAR) LEN(10)
000500980108             DCL        VAR(&STATO)     TYPE(*CHAR) LEN(3)
000600980108             DCL        VAR(&LIBOUT)    TYPE(*CHAR) LEN(10)
000700980108             DCL        VAR(&FILOUT)    TYPE(*CHAR) LEN(10)
000800980108             DCL        VAR(&LIBOUTQ)   TYPE(*CHAR) LEN(10)
000900980108             DCL        VAR(&NAMOUTQ)   TYPE(*CHAR) LEN(10)
001000961002/* LANCIO FATTURE FTWCODIM  */
001100980108             CHGVAR     VAR(&CODA)     VALUE('QPRINTS')
001200980108             CHGVAR     VAR(&SPLFILE)  VALUE('FTWCODIM')
001201980108             CHGVAR     VAR(&FORMTYPE) VALUE('FATIM101')
001300980108             CHGVAR     VAR(&STATO)    VALUE('RDY')
001400980108             CHGVAR     VAR(&LIBOUT)   VALUE('LIBASP3')
001500980123             CHGVAR     VAR(&FILOUT)   VALUE('FTSORIG')
001600980108             CHGVAR     VAR(&LIBOUTQ)  VALUE('PICCIONEDB')
001700980108             CHGVAR     VAR(&NAMOUTQ)  VALUE('BARPICSTM ')
001800980108             CALL       PGM(TNSF88C) PARM(&CODA &SPLFILE &FORMTYPE +
001900980108                          &STATO &LIBOUT &FILOUT &LIBOUTQ &NAMOUTQ)
002000961002/* LANCIO FATTURE FTWCODFM  */
002100980108             CHGVAR     VAR(&CODA)     VALUE('QPRINTS')
002200980108             CHGVAR     VAR(&SPLFILE)  VALUE('FTWCODFM')
002201980108             CHGVAR     VAR(&FORMTYPE) VALUE('FATFM101')
002300980108             CHGVAR     VAR(&STATO)    VALUE('RDY')
002400980108             CHGVAR     VAR(&LIBOUT)   VALUE('LIBASP3')
002500980123             CHGVAR     VAR(&FILOUT)   VALUE('FTSORIG')
002600980108             CHGVAR     VAR(&LIBOUTQ)  VALUE('PICCIONEDB')
002700980108             CHGVAR     VAR(&NAMOUTQ)  VALUE('BARPICSTM ')
002800980108             CALL       PGM(TNSF88C) PARM(&CODA &SPLFILE &FORMTYPE +
002900980108                          &STATO &LIBOUT &FILOUT &LIBOUTQ &NAMOUTQ)
003000961002/* LANCIO FATTURE FTWCODDS  */
003100980108             CHGVAR     VAR(&CODA)     VALUE('QPRINTS')
003200980108             CHGVAR     VAR(&SPLFILE)  VALUE('FTWCODDS')
003201980108             CHGVAR     VAR(&FORMTYPE) VALUE('FATDS101')
003300980108             CHGVAR     VAR(&STATO)    VALUE('RDY')
003400980108             CHGVAR     VAR(&LIBOUT)   VALUE('LIBASP3')
003500980123             CHGVAR     VAR(&FILOUT)   VALUE('FTSDIST')
003600980108             CHGVAR     VAR(&LIBOUTQ)  VALUE('GAITRAOBJ ')
003700980108             CHGVAR     VAR(&NAMOUTQ)  VALUE('FATTURAZIO')
003800980108             CALL       PGM(TNSF88C) PARM(&CODA &SPLFILE &FORMTYPE +
003900980108                          &STATO &LIBOUT &FILOUT &LIBOUTQ &NAMOUTQ)
004000961002/* LANCIO FATTURE FTWFAT89  */
004100980108             CHGVAR     VAR(&CODA)     VALUE('QPRINTS')
004200980108             CHGVAR     VAR(&SPLFILE)  VALUE('FTWFAT89')
004201980108             CHGVAR     VAR(&FORMTYPE) VALUE('FAT89101')
004300980108             CHGVAR     VAR(&STATO)    VALUE('RDY')
004400980108             CHGVAR     VAR(&LIBOUT)   VALUE('LIBASP3')
004500980123             CHGVAR     VAR(&FILOUT)   VALUE('FTS8899')
004600980108             CHGVAR     VAR(&LIBOUTQ)  VALUE('GAITRAOBJ ')
004700980108             CHGVAR     VAR(&NAMOUTQ)  VALUE('FATTURAZIO')
004800980108             CALL       PGM(TNSF88C) PARM(&CODA &SPLFILE &FORMTYPE +
004900980108                          &STATO &LIBOUT &FILOUT &LIBOUTQ &NAMOUTQ)
005000000000             ENDPGM

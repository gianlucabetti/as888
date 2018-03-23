000100000000/********************************************************************/
000200000000/*                                                                  */
000300000000/* 5769SS1 V4R3M0 980729    Emissione RTVCLSRC   04/04/00 12:39:23  */
000400000000/*                                                                  */
000500000000/* Nome programma . . . . . . . . . . . . . :   TNSY84C1          PN*/
000600000000/* Nome libreria  . . . . . . . . . . . . . :   GAITRAOBJ         PL*/
000700000000/* File origine originale . . . . . . . . . :   SRCTNSY           SN*/
000800000000/* Nome libreria  . . . . . . . . . . . . . :   GAITRASRC         SL*/
000900000000/* Membro origine originale . . . . . . . . :   TNSY84C1          SM*/
001000000000/* Modifica file origine                                            */
001100000000/*   data/ora . . . . . . . . . . . . . . . :   02/01/00 14:47:26 SC*/
001200000000/* Opzione di correzione  . . . . . . . . . :   *NOPATCH          PO*/
001300000000/* Profilo utente . . . . . . . . . . . . . :   *USER             UP*/
001400000000/* Testo  . . : Cancellazione ricevitori Journal salvati - Esegue TX*/
001500000000/* Proprietario . . . . . . . . . . . . . . :   QPGMR             OW*/
001600000000/* ID modifica correzione . . . . . . . . . :                     PC*/
001700000000/* ID correzione APAR . . . . . . . . . . . :                     PA*/
001800000000/* Indicatore di modifica utente  . . . . . :   *YES              UM*/
001900000000/*                                                                ED*/
002000000000/********************************************************************/
002100000000TNSY84C1: +
002200000000     PGM
002300000404             DCL        VAR(&DTSV) TYPE(*CHAR) LEN(7) /* data +
002400000404                          salvataggio in formato SAAMMGG ALFABETICO */
002500000404             DCL        VAR(&OGGI) TYPE(*CHAR) LEN(7) /* udate in +
002600000404                          formato SAAMMGG ALFABETICO */
002700000404             DCL        VAR(&AAA) TYPE(*CHAR) LEN(2)
002800000404             DCL        VAR(&MMM) TYPE(*CHAR) LEN(2)
002900000404             DCL        VAR(&GGG) TYPE(*CHAR) LEN(2)
003000000404             DCL        VAR(&SSS) TYPE(*CHAR) LEN(1)
003100000404             DCL        VAR(&Q) TYPE(*CHAR) LEN(1)
003200000404
003300000404             DCLF       FILE(QSYS/QADSPOBJ)
003400000404
003500000404             RTVSYSVAL  SYSVAL(QDAY) RTNVAR(&GGG)
003600000404             RTVSYSVAL  SYSVAL(QYEAR) RTNVAR(&AAA)
003700000404             RTVSYSVAL  SYSVAL(QMONTH) RTNVAR(&MMM)
003800000404             RTVSYSVAL  SYSVAL(QCENTURY) RTNVAR(&SSS)
003900000404
004000000404             CHGVAR     VAR(%SUBSTRING(&OGGI 1 1)) VALUE(&SSS)
004100000404             CHGVAR     VAR(%SUBSTRING(&OGGI 2 2)) VALUE(&AAA)
004200000404             CHGVAR     VAR(%SUBSTRING(&OGGI 4 2)) VALUE(&MMM)
004300000404             CHGVAR     VAR(%SUBSTRING(&OGGI 6 2)) VALUE(&GGG)
004400000404
004500000404
004600000404/* RICERCA TUTTI I JOURNAL DELLE LIBRERIE         */
004700000404
004800000404             DSPOBJD    OBJ(*ALL/*ALL) OBJTYPE(*JRNRCV) +
004900000404                          DETAIL(*SERVICE) OUTPUT(*OUTFILE) +
005000000404                          OUTFILE(QTEMP/§JRN)
005100000404
005200000404             OVRDBF     FILE(QADSPOBJ) TOFILE(QTEMP/§JRN)
005300000000READ:
005400000404             RCVF
005500000404             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
005600000404
005700000404/* CANCELLA SE GIA' SALVATO DA IERI */
005800000404
005900000404             CHGVAR     VAR(&Q) VALUE(%SUBSTRING(&ODOBNM 1 1))
006000000404             IF         COND(&Q *EQ 'Q') THEN(GOTO CMDLBL(READ))
006100000404             IF         COND(&ODSDAT *LE '000000') THEN(GOTO +
006200000404                          CMDLBL(READ))
006300000404
006400000404             CHGVAR     VAR(&MMM) VALUE(%SST(&ODSDAT 1 2))
006500000404             CHGVAR     VAR(&GGG) VALUE(%SST(&ODSDAT 3 2))
006600000404             CHGVAR     VAR(&AAA) VALUE(%SST(&ODSDAT 5 2))
006700000404             CHGVAR     VAR(&SSS) VALUE(&ODSCEN)
006800000404             CHGVAR     VAR(%SUBSTRING(&DTSV 1 1)) VALUE(&SSS)
006900000404             CHGVAR     VAR(%SUBSTRING(&DTSV 2 2)) VALUE(&AAA)
007000000404             CHGVAR     VAR(%SUBSTRING(&DTSV 4 2)) VALUE(&MMM)
007100000404             CHGVAR     VAR(%SUBSTRING(&DTSV 6 2)) VALUE(&GGG)
007200000404
007300000404             IF         COND(&DTSV *LE &OGGI) THEN(DO)
007400000404             DLTJRNRCV  JRNRCV(&ODLBNM/&ODOBNM)
007500000404             MONMSG     MSGID(CPF2105 CPF7022 CPF7023 CPF9803 +
007600000404                          CPA7025 CPA0701) EXEC(GOTO CMDLBL(READ))
007700000404             ENDDO
007800000404
007900000404             GOTO       CMDLBL(READ)
008000000404
008100000404 ENDF:       ENDPGM

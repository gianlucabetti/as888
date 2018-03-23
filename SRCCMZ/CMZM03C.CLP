000100930301             PGM        PARM(&DOL &OUTQ)
000200921103/*---------- Dichiarazione parametri                   --------------*/
000300921103             DCL        VAR(&DOL) TYPE(*DEC) LEN(7 0)
000400930301             DCL        VAR(&OUTQ) TYPE(*CHAR) LEN(10)
000500960318             DCL        VAR(&OUTJOB) TYPE(*CHAR) LEN(10)
000600921103
000700960318/*-------------------------------------------------------------------*/
000800960318/*           IMPOSTA CODA DI STAMPA                                  */
000900960318/*-------------------------------------------------------------------*/
001000960318             RTVJOBA    OUTQ(&OUTJOB)
001100960318             IF         COND(&OUTQ = '          ') THEN(CHGVAR +
001200960318                          VAR(&OUTQ) VALUE(&OUTJOB))
001300960318
001400921103/*-------------------------------------------------------------------*/
001500921104/*           Creo file STAMPA in QTEMP  lungo 199 .                  */
001600921103/*-------------------------------------------------------------------*/
001700921103
001800921104             CRTPF      FILE(QTEMP/STAMPA) RCDLEN(199) LVLCHK(*NO)
001900921103             MONMSG     MSGID(CPF5813) /* Il file STAMPA esiste già +
002000921106                                          in QTEMP.                 */
002100921106             MONMSG     MSGID(CPF7302) /* Il file STAMPA non è stato+
002200921106                                          creato nella lib. QTEMP.  */
002300921103             CLRPFM     FILE(QTEMP/STAMPA)
002400921103
002500921113             OVRDBF     FILE(STAMPA) TOFILE(QTEMP/STAMPA)
002600960318
002700921103/*           Valorizzo il file STAMPA                                */
002800921103
002900921103             CALL       PGM(CMZM03R) PARM(&DOL)
003000921103
003100921103/*           Eseguo stampa                                           */
003200921103
003300940624             OVRPRTF    FILE(QSYSPRT) CTLCHAR(*FCFC) DFRWRT(*YES) +
003400940624                          OUTQ(&OUTQ) HOLD(*YES)
003500921113             CPYF       FROMFILE(QTEMP/STAMPA) TOFILE(QSYSPRT)
003600921103             DLTOVR *ALL
003700921103
003800921103             ENDPGM

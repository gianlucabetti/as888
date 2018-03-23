000100000000     /*****************************************************************/
000200950118     /*  Nome programma:  FNLSC2C                                     */
000300000000     /*  Descrizione   :  Stampa statistica a terra                   */
000400000000     /*                   Programma esecuzione (stampa)               */
000500000000     /*  Autore        :  STEFANO (SC)                                */
000600000000     /*  Data creazione:  01 DIC 1993                                 */
000700000000     /*****************************************************************/
000800000000             PGM        PARM(&KPJBA)
000900000000
001000000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
001100000000             DCL        VAR(&RTNLIB) TYPE(*CHAR) LEN(10)
001200970213             DCL        VAR(&DS1ADA) TYPE(*CHAR) LEN(8)
001300970213             DCL        VAR(&MBR) TYPE(*CHAR) LEN(10) +
001400970213                          VALUE('M        ')
001500970213             DCLF       FILE(QSYS/QAFDMBRL)
001600970213             CHGVAR     VAR(&DS1ADA) VALUE(%SST(&KPJBA 287 8))
001700970213             CHGVAR     VAR(%SST(&MBR 2 8)) VALUE(&DS1ADA)
001800000000
001900000000     /*  Duplico il file di work in QTEMP per evitare conflitti di    */
002000000000     /*  elaborazione; cosi' non devo neppure pulirlo                 */
002100000000
002200950118             CHKOBJ     OBJ(QTEMP/FNWRK00F) OBJTYPE(*FILE)
002300000000             MONMSG     MSGID(CPF9801) EXEC(DO)
002400950118             RTVOBJD    OBJ(FNWRK00F) OBJTYPE(*FILE) RTNLIB(&RTNLIB)
002500950118             CRTDUPOBJ  OBJ(FNWRK00F) FROMLIB(&RTNLIB) +
002600000000                          OBJTYPE(*FILE) TOLIB(QTEMP) DATA(*NO)
002700000000             ENDDO
002800000000
002900950118             CLRPFM     FILE(QTEMP/FNWRK00F)
003000000000             MONMSG     MSGID(CPF0000)
003100000000
003200970807             CALL       PGM(FNLSC2R) PARM(&KPJBA)
003300000000             MONMSG     MSGID(CPF0000)
003400970213/* SE FILE DI WORK PIENO MI SALVO I DATI IN FILE MULTIMEMBRO */
003500970213/* IL NOME DEL MEMBRO E' M+DATA ELABORATA                    */
003600970213             DSPFD      FILE(QTEMP/FNWRK00F) TYPE(*MBRLIST) +
003700970213                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/§MBR)
003800970213             OVRDBF     FILE(QAFDMBRL) TOFILE(QTEMP/§MBR)
003900970213             RCVF
004000970213             IF         COND(&MLNRCD *NE 0) THEN(DO)
004100970214             CHKOBJ     OBJ(FNWRK00S) OBJTYPE(*FILE) MBR(&MBR)
004200970213             MONMSG     MSGID(CPF9815) EXEC(ADDPFM FILE(FNWRK00S) +
004300970213                          MBR(&MBR))
004400970213             CPYF       FROMFILE(QTEMP/FNWRK00F) TOFILE(FNWRK00S) +
004500970213                          TOMBR(&MBR) MBROPT(*REPLACE) INCREL((*IF +
004600970213                          WRKFLG *NE 'P'))
004700970213             ENDDO
004800000000
004900000000             ENDPGM

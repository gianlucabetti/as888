000100000000     /*****************************************************************/
000200950118     /*  Nome programma:  FNLSC2C                                     */
000300000000     /*  Descrizione   :  Stampa statistica a terra                   */
000400000000     /*                   Programma esecuzione (stampa)               */
000500000000     /*  Data creazione:  01 DIC 1993                                 */
000600000000     /*****************************************************************/
000700000000             PGM        PARM(&KPJBA)
000800000000
000900000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
001000000000             DCL        VAR(&RTNLIB) TYPE(*CHAR) LEN(10)
001100970213             DCL        VAR(&DS1ADA) TYPE(*CHAR) LEN(8)
001200970213             DCL        VAR(&MBR) TYPE(*CHAR) LEN(10) +
001300970213                          VALUE('M        ')
001400970213             DCLF       FILE(QSYS/QAFDMBRL)
001500970213             CHGVAR     VAR(&DS1ADA) VALUE(%SST(&KPJBA 287 8))
001600970213             CHGVAR     VAR(%SST(&MBR 2 8)) VALUE(&DS1ADA)
001700000000
001800000000     /*  Duplico il file di work in QTEMP per evitare conflitti di    */
001900000000     /*  elaborazione; cosi' non devo neppure pulirlo                 */
002000000000
002100950118             CHKOBJ     OBJ(QTEMP/FNWRK00F) OBJTYPE(*FILE)
002200000000             MONMSG     MSGID(CPF9801) EXEC(DO)
002300950118             RTVOBJD    OBJ(FNWRK00F) OBJTYPE(*FILE) RTNLIB(&RTNLIB)
002400950118             CRTDUPOBJ  OBJ(FNWRK00F) FROMLIB(&RTNLIB) +
002500000000                          OBJTYPE(*FILE) TOLIB(QTEMP) DATA(*NO)
002600000000             ENDDO
002700000000
002800950118             CLRPFM     FILE(QTEMP/FNWRK00F)
002900000000             MONMSG     MSGID(CPF0000)
003000000000
003100970807             CALL       PGM(FNLSC2R) PARM(&KPJBA)
003200000000             MONMSG     MSGID(CPF0000)
003300970213/* SE FILE DI WORK PIENO MI SALVO I DATI IN FILE MULTIMEMBRO */
003400970213/* IL NOME DEL MEMBRO E' M+DATA ELABORATA                    */
003500051019/*           DSPFD      FILE(QTEMP/FNWRK00F) TYPE(*MBRLIST) +      */
003600051019/*                        OUTPUT(*OUTFILE) OUTFILE(QTEMP/§MBR)     */
003700051019/*           OVRDBF     FILE(QAFDMBRL) TOFILE(QTEMP/§MBR)          */
003800051019/*           RCVF                                                  */
003900051019/*           IF         COND(&MLNRCD *NE 0) THEN(DO)               */
004000051019/*           CHKOBJ     OBJ(FNWRK00S) OBJTYPE(*FILE) MBR(&MBR)     */
004100051019/*           MONMSG     MSGID(CPF9815) EXEC(ADDPFM FILE(FNWRK00S) +*/
004200051019/*                        MBR(&MBR))                               */
004300051019/*           CPYF       FROMFILE(QTEMP/FNWRK00F) TOFILE(FNWRK00S) +*/
004400051019/*                        TOMBR(&MBR) MBROPT(*REPLACE) INCREL((*IF +*/
004500051019/*                        WRKFLG *NE 'P'))                          */
004600051019/*           ENDDO                                                  */
004700000000
004800000000             ENDPGM

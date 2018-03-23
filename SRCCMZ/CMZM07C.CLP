000100921218             PGM        PARM(&TXT &UTL &UTR &FIRC)
000200921103/*---------- Dichiarazione parametri                   --------------*/
000300921110             DCL        VAR(&TXT) TYPE(*CHAR) LEN(70)
000400921110             DCL        VAR(&UTL) TYPE(*CHAR) LEN(10)
000500921218             DCL        VAR(&UTR) TYPE(*CHAR) LEN(10)
000600921218             DCL        VAR(&FIRC) TYPE(*CHAR) LEN(3)
000700921103
000800921103/*-------------------------------------------------------------------*/
000900921110/*           Invio messaggio                                         */
001000921103/*-------------------------------------------------------------------*/
001100921103
001200921218             SNDPGMMSG  MSG('Documento ' *CAT &TXT *CAT ' ricevuto +
001300921218                          per utente ' *CAT &UTL *CAT 'inviato da ' +
001400921218                          *CAT &UTR *CAT 'filiale ' *CAT &FIRC) +
001500921218                          TOMSGQ(&UTL)
001600040607             MONMSG     MSGID(CPF0000)
001700040607 /*          SNDPGMMSG  MSG('Documento ' *CAT &TXT *CAT ' ricevuto */
001800040607 /*                       per utente ' *CAT &UTL *CAT 'inviato da '*/
001900040607 /*                       *CAT &UTR *CAT 'filiale ' *CAT &FIRC) +  */
002000040607 /*                       TOMSGQ(*SYSOPR)                          */
002100040607 /*          ENDDO                                                 */
002200921110
002300921103/*-------------------------------------------------------------------*/
002400921103
002500921103             ENDPGM

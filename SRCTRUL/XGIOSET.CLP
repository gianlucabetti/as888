000100991216/*----------------------------------------------------------------------------*/
000200991216/* RECUPERA IL GIORNO DELLA SETTIMANA                                         */
000300991216/* Input : ggmmaa = data nel formato gg+mm+aa                                 */
000400991216/* Output: gioset = giorno della settimana ('1'=Domenica)                     */
000500991216/*----------------------------------------------------------------------------*/
000600991216 XGIOSET:    PGM        PARM(&GGMMAA &GIOSET)
000700991216
000800991216/* Dischiarazione variabili --------------------------------------------------*/
000900991216             DCL        VAR(&GGMMAA) TYPE(*CHAR) LEN(6) /* Input  */
001000991216             DCL        VAR(&AA)     TYPE(*CHAR) LEN(2)
001100991216             DCL        VAR(&GIOSET) TYPE(*CHAR) LEN(1) /* Output */
001200991216             DCL        VAR(&WDAT7)  TYPE(*CHAR) LEN(7) /* Di lavoro */
001300991216             DCL        VAR(&WSEC)   TYPE(*CHAR) LEN(1)
001400991216
001500991216/* Valorizzazione variabili --------------------------------------------------*/
001600991216             CHGVAR     VAR(&AA) VALUE(%SUBSTRING(&GGMMAA 5 2))
001700991216
001800991216/* Imposta il secolo da aggiungere alla data (0=XX secolo, 1=XXI secolo) -----*/
001900991216             IF         COND(&AA *GE '40') THEN(DO)
002000991216             CHGVAR     VAR(&WSEC) VALUE('0')
002100991216             ENDDO
002200991216             IF         COND(&AA *LT '40') THEN(DO)
002300991216             CHGVAR     VAR(&WSEC) VALUE('1')
002400991216             ENDDO
002500991216
002600991216/* Imposta la data da passare al comando (gg+mm+aa+secolo) -------------------*/
002700991216             CHGVAR     VAR(&WDAT7) VALUE(&GGMMAA *CAT &WSEC)
002800991216
002900991216/* Lancio comando che recupera informazioni dalla data -----------------------*/
003000991216             RTVDAT     DATE(&WDAT7) DAYOFWEEK(&GIOSET)
003100991216
003200991216/* Se data errata, ritorna il giorno della settimana = zero ------------------*/
003300991216             MONMSG     MSGID(CPF0555)
003400991216
003500991216ENDPGM:      ENDPGM

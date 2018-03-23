000100991216/*----------------------------------------------------------------------------*/
000200991216/* RECUPERA IL GIORNO DELLA SETTIMANA                                         */
000300991216/* Input : ggmmaa = data nel formato gg+mm+aa                                 */
000400991216/* Output: gioset = giorno della settimana ('1'Lunedì)                        */
000500991216/*----------------------------------------------------------------------------*/
000600991216 XGIOSE1:    PGM        PARM(&GGMMAA &GIOSET)
000700991216
000800991216/* Dischiarazione variabili --------------------------------------------------*/
000900991216             DCL        VAR(&GGMMAA) TYPE(*CHAR) LEN(6) /* Input  */
001000991216             DCL        VAR(&AA)     TYPE(*CHAR) LEN(2)
001100991216             DCL        VAR(&GIOSET) TYPE(*CHAR) LEN(1) /* Output */
001200991216             DCL        VAR(&GIOSE1) TYPE(*DEC) LEN(1 0)
001300991216             DCL        VAR(&WDAT7)  TYPE(*CHAR) LEN(7) /* Di lavoro */
001400991216             DCL        VAR(&WSEC)   TYPE(*CHAR) LEN(1)
001500991216
001600991216/* Valorizzazione variabili --------------------------------------------------*/
001700991216             CHGVAR     VAR(&AA) VALUE(%SUBSTRING(&GGMMAA 5 2))
001800991216
001900991216/* Imposta il secolo da aggiungere alla data (0=XX secolo, 1=XXI secolo) -----*/
002000991216             IF         COND(&AA *GE '40') THEN(DO)
002100991216             CHGVAR     VAR(&WSEC) VALUE('0')
002200991216             ENDDO
002300991216             IF         COND(&AA *LT '40') THEN(DO)
002400991216             CHGVAR     VAR(&WSEC) VALUE('1')
002500991216             ENDDO
002600991216
002700991216/* Imposta la data da passare al comando (gg+mm+aa+secolo) -------------------*/
002800991216             CHGVAR     VAR(&WDAT7) VALUE(&GGMMAA *CAT &WSEC)
002900991216
003000991216/* Lancio comando che recupera informazioni dalla data -----------------------*/
003100950915             RTVDAT     DATE(&GGMMAA) DAYOFWEEK(&GIOSET)
003200930401
003300991216/* Se data errata, ritorna il giorno della settimana = *blank ----------------*/
003400930401             MONMSG     MSGID(CPF0555)
003500991216
003600991216/* Imposta il campo giorno della settimana numerico --------------------------*/
003700950915             IF         COND(&GIOSET *NE ' ') THEN(DO)
003800991216             IF         COND(&GIOSET = '1') THEN(CHGVAR VAR(&GIOSET) +
003900991216                          VALUE('7'))
004000991216             ELSE       CMD(DO)
004100991216             CHGVAR     VAR(&GIOSE1) VALUE(&GIOSET)
004200950915             CHGVAR     VAR(&GIOSE1) VALUE(&GIOSE1 - 1)
004300950915             CHGVAR     VAR(&GIOSET) VALUE(&GIOSE1)
004400950915             ENDDO
004500950915             ENDDO
004600900412
004700901214ENDPGM:      ENDPGM

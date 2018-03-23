000100961213 FNLG21C:    PGM        PARM(&KPJBA)
000200961213
000300961213         /*               Dichiaro variabili               */
000400961213             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
000500971203             DCL        VAR(&PRTFAX)   TYPE(*CHAR)  LEN(10)
000600961213
000700971203/* Reperisco stampante per invio fax                        */
000800990706             CHGVAR     VAR(&PRTFAX) VALUE(%SST(&KPJBA 282 10))
000900961213
001000961213         /*      Eseguo OVRPRTF su letest                  */
001100971203             OVRPRTF    FILE(LETEST) OUTQ(&PRTFAX)
001200961213
001300961213         /*      Richiamo pgm stampa LETEST                */
001400961213             CALL     PGM(FNLG21R) PARM(&KPJBA)
001500961213
001600961213         /*      Fine OVRDBF                               */
001700961213             DLTOVR   FILE(LETEST)
001800961213
001900961213
002000961213             ENDPGM

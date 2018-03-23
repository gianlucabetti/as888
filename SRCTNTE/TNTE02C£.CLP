000100970117/*----------------------------------------------------------------------------*/
000200970117/* TNTE01C - RICHIESTA DATI TIPO - GUIDA                                      */
000300970117/*----------------------------------------------------------------------------*/
000400970117             PGM        PARM(&KPJBA &TNTE00DS)
000500970117
000600970117             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000700980504             DCL        VAR(&TNTE00DS) TYPE(*CHAR) LEN(154)
000800970117             DCL        VAR(&OP0)   TYPE(*CHAR) LEN(3)
000900970117
001000970117             CHGVAR     VAR(&OP0) VALUE(%SUBSTRING(&TNTE00DS 1 3))
001100970117
001200970117 /* CHIAMA IL PROGRAMMA DI GESTIONE DEI DATI TIPO                             */
001300971009             CHGJOB     RUNPTY(25)
001400970117             CALL       PGM(TNTE01R) PARM(&KPJBA &TNTE00DS)
001500971009             CHGJOB     RUNPTY(20)
001600970117
001700970117             DLTOVR     *ALL
001800970117             RCLRSC     LVL(*CALLER)
001900971009
002000970117             ENDPGM

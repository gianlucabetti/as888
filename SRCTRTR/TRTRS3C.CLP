000100931020             PGM        PARM(&KPJBA)
000200000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000300931020/*                                                                           */
000400931020/*                   RICHIAMO PGM DI RICEZIONE MOVIMENTI                     */
000500941102             CALL       PGM(TRTRS3R) PARM(&KPJBA)
000600910417
000700910417             RCLRSC     LVL(*)
000800931020             ENDPGM

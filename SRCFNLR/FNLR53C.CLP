000100941123 FNLR53C:    PGM        PARM(&KPJBA)
000200911021             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
000300920914             DCL        VAR(&DSP) TYPE(*CHAR) LEN(10)
000400920914             CHGVAR     VAR(&DSP) VALUE(%SST(&KPJBA 20 10))
000500920914
000600920914             SNDBRKMSG  MSG('ATTENZIONE ! Qualcuno sta lavorando +
000700920914                          sulle Bolle in Arrivo : fare chiudere il +
000800920914                          lavoro e successivamente RISPONDETE +
000900920914                          PREMENDO "INVIO" a questo messaggio') +
001000920914                          TOMSGQ(&DSP)
001100911021ENDPGM

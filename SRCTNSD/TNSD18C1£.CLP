000100030331 TNSD18C1:   PGM        PARM(&KPJBA &DSLV45 &TNSD17DS)
000200000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000300020626             DCL        VAR(&TNSD17DS) TYPE(*CHAR) LEN(15)
000400050913             DCL        VAR(&DSLV45) TYPE(*CHAR) LEN(689)
000500940316
000600030331             OVRPRTF    FILE(LET017) FORMTYPE(LET1)
000700940316
000800030331             CALL       PGM(TNSD18R) PARM(&KPJBA &DSLV45 &TNSD17DS)
000900000000
001000000000/* CHIUDI PROGRAMMI CHIAMATI */
001100000000             RCLRSC     LVL(*CALLER)
001200000000             ENDPGM

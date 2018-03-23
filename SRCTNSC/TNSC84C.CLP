000100950717             PGM        PARM(&KPJBA)
000200950717             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000301150119             DCL        VAR(&KNSIF) TYPE(*CHAR) LEN(10)
000302150119             DCL        VAR(&SIF) TYPE(*CHAR) LEN(10)
000401150119             CHGVAR     VAR(&KNSIF) VALUE(%SST(&KPJBA 92 10))
000500950717 /*                                                                          */
000501150119/* Se S.I. di prova imposto GAITRAPSP */
000502150119             IF         COND(%SST(&KNSIF 7 1) = 'P') THEN(DO)
000503150119             CHGVAR     VAR(&SIF) VALUE('GAITRAPSP ')
000504150119             ENDDO
000505150119             ELSE       CMD(DO)
000506150119             CHGVAR     VAR(&SIF) VALUE('GAITRA201 ')
000507150119             ENDDO
000508150119/* Forzo S.I. di sede  ____________________________________________*/
000509150119
000510150119             CALL       PGM(TRUL07C) PARM(&SIF)
000511150119
000600950717 /* Lancio pgm.:                                                             */
000602150119             CALL       PGM(TNSC84R) PARM(&KPJBA)
000603150119             MONMSG     MSGID(CPF0000)
001300950717
001400950717ENDPGM

000100990317 TII311C:    PGM        PARM(&KPJBA) /* BOLLA INTER. */
000200000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000300000000             DCL        VAR(&SYS)   TYPE(*CHAR) LEN(3)
000400891127             CHGVAR     VAR(&SYS)    VALUE(%SUBSTRING(&KPJBA  98 3))
000500890607             IF (&SYS *EQ '201') THEN(DO)
000600081119      /*     OVRPRTF    FILE(BOLLEPRT) DEV(P046127S) +
000700000320                          OUTQ(*LIBL/P046127S) FORMTYPE('bolle201') +
000800081119                          MAXRCDS(500000) FILESEP(0) LVLCHK(*NO)         */
000900081119             OVRPRTF    FILE(BOLLEPRT) OUTQ(*DEV) +
001000081119                          FORMTYPE('bolle201') MAXRCDS(500000) +
001100081119                          FILESEP(0) LVLCHK(*NO)
001200890607             ENDDO
001300990317CALL       PGM(*LIBL/TII311R) PARM(&KPJBA)
001400000000             ENDPGM

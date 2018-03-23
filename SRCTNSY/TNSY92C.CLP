000100900118             PGM        PARM(&KPJBA)
000200900118             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000300911102             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000400911102             DCL        VAR(&NOME) TYPE(*CHAR) LEN(6)
000500901011             DCL        VAR(&DATA) TYPE(*CHAR) LEN(6)
000600900118             DCL        VAR(&RISP) TYPE(*CHAR) LEN(2)
000700911102             RTVNETA    SYSNAME(&SYSNAME)
000800901011             RTVSYSVAL  SYSVAL(QDATE) RTNVAR(&DATA)
000900920717/*  NOME CASSETTA = XXGGMM  */
001100920717
001200911102             CHGVAR     VAR(%SUBSTRING(&NOME 1 2)) +
001300911102                          VALUE(%SUBSTRING(&SYSNAME 4 2))
001600911102             CHGVAR     VAR(%SUBSTRING(&NOME 3 4)) +
001700911102                          VALUE(%SUBSTRING(&DATA 1 4))
001800901011
001900950201             SNDUSRMSG  MSG('Inserire CASSETTA Salvataggi sulla +
002000950201                          TAP01 e rispondere: OK .. oppure: NO.') +
002100950201                          VALUES(OK NO) DFT(NO) MSGRPY(&RISP)
002200990123             IF         COND((&RISP = 'OK') *OR (&RISP = 'ok')) +
002300990123                          THEN(DO)
002700990123             INZTAP     DEV(TAP01) NEWVOL(&NOME) CHECK(*NO) +
002800990123                          DENSITY(*QIC2GB)
002900990123             ENDDO
003000950201             IF         COND((&RISP *EQ 'NO') *OR (&RISP *EQ 'no')) +
003100950201                          THEN(SNDUSRMSG MSG('Il NASTRO non è stato +
003200950201                          inizializzato') MSGTYPE(*INFO))
003300900118
003400900118        ENDPGM

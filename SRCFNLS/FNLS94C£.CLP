000100940920 FNLS94C:    PGM        PARM(&KPJBA)
000200941006             DCL        VAR(&KPJBA)   TYPE(*CHAR)  LEN(502)
000300941006             DCL        VAR(&KPJBU)   TYPE(*CHAR)  LEN(256)
000400941006             DCL        VAR(&PA2PRT)  TYPE(*CHAR)  LEN(10)
000500941006             DCL        VAR(&PA2MDT)  TYPE(*CHAR)  LEN(10)
000600941006
000700941006             CHGVAR     VAR(&KPJBU)  VALUE(%SST(&KPJBA 247 256))
000800941006             CHGVAR     VAR(&PA2PRT) VALUE(%SST(&KPJBU 164 10))
000900941006             CHGVAR     VAR(&PA2MDT) VALUE(%SST(&KPJBU 174 10))
001000941006
001100941006
001200950112             IF         COND(&PA2PRT *NE '          ') THEN(OVRPRTF +
001300950112                          FILE(SYSPRT) OUTQ(&PA2PRT) +
001400950112                          FORMTYPE(&PA2MDT) COPIES(2) OUTPTY(6) +
001500950112                          USRDTA('Foglio V.'))
001600950112
001700950112             ELSE       CMD(OVRPRTF FILE(SYSPRT) COPIES(2) OUTPTY(6) +
001800950112                          USRDTA('Foglio V.'))
001900900112
002000940920             CALL       PGM(FNLS94R) PARM(&KPJBA)
002100941006
002200941006             RCLRSC     LVL(*CALLER)
002300941006             DLTOVR     FILE(*ALL)
002400891012
002500000000             ENDPGM

000100941201 FNLR04C:    PGM        PARM(&KPJBA)
000200990705             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
000300941212
000400921023
000500920810             OVRDBF     FILE(AZORG01L) SHARE(*YES)
000600920810             OPNDBF     FILE(AZORG01L) OPTION(*ALL)
000700921023             OVRDBF     FILE(TABEL00F) SHARE(*YES)
000800921023             OPNDBF     FILE(TABEL00F) OPTION(*ALL)
000900921023
001000941201             OVRDBF     FILE(FNFGV01L) SHARE(*YES)
001100941201             OPNDBF     FILE(FNFGV01L) OPTION(*ALL)
001200941201             OVRDBF     FILE(FNFVV01L) SHARE(*YES)
001300941201             OPNDBF     FILE(FNFVV01L) OPTION(*ALL)
001400921023
001500941205             OVRDBF     FILE(FNART27L) SHARE(*YES)
001600941205             OPNDBF     FILE(FNART27L) OPTION(*ALL)
001700941205             OVRDBF     FILE(FNART01L) SHARE(*YES)
001800941205             OPNDBF     FILE(FNART01L) OPTION(*ALL)
001900941205             OVRDBF     FILE(FNARB01L) SHARE(*YES)
002000941205             OPNDBF     FILE(FNARB01L) OPTION(*ALL)
002100921023
002200941205             OVRDBF     FILE(FNBTT27L) SHARE(*YES)
002300941205             OPNDBF     FILE(FNBTT27L) OPTION(*ALL)
002400941201             OVRDBF     FILE(FNBTP01L) SHARE(*YES)
002500941201             OPNDBF     FILE(FNBTP01L) OPTION(*ALL)
002600921023
002700941205             OVRDBF     FILE(FNBLT27L) SHARE(*YES)
002800941205             OPNDBF     FILE(FNBLT27L) OPTION(*ALL)
002900941201             OVRDBF     FILE(FNBLT01L) SHARE(*YES)
003000941201             OPNDBF     FILE(FNBLT01L) OPTION(*ALL)
003100941201             OVRDBF     FILE(FNBLP01L) SHARE(*YES)
003200941201             OPNDBF     FILE(FNBLP01L) OPTION(*ALL)
003300900925
003400941201             CALL       PGM(FNLR04R) PARM(&KPJBA)
003500941206             MONMSG     MSGID(CPF0000)
003600900924             RCLRSC     LVL(*CALLER)
003700900925             DLTOVR     FILE(*ALL)
003800910305ENDF:        ENDPGM

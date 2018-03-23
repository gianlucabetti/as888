000100000000/*----------------------------------------------------------------------------*/
000200000000/* TNTL01C - GESTIONE COMPLETA TRAINI - GUIDA PER IL CONTROLLO DI SINCRONIA   */
000300000000/*----------------------------------------------------------------------------*/
000400000000   TNTL01C:  PGM        PARM(&KPJBA)
000500000000
000600000000             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000700000000
000800000000/* APERTURA CONDIVISA                                                         */
000900000000             OVRDBF     FILE(TNTLT01L) TOFILE(*LIBL/TNTLT01L) +
001000000000                          SHARE(*YES)
001100000000             OVRDBF     FILE(TNTLR01L) TOFILE(*LIBL/TNTLR01L) +
001200000000                          SHARE(*YES)
001300000000             OVRDBF     FILE(TNTLO01L) TOFILE(*LIBL/TNTLO01L) +
001400000000                          SHARE(*YES)
001500000000             OVRDBF     FILE(TNTLM01L) TOFILE(*LIBL/TNTLM01L) +
001600000000                          SHARE(*YES)
001700010723             OVRDBF     FILE(TITLL01L) TOFILE(*LIBL/TITLL01L) +
001800000000                          SHARE(*YES)
001900000000             OVRDBF     FILE(TNTLN01L) TOFILE(*LIBL/TNTLN01L) +
002000000000                          SHARE(*YES)
002100000000
002200000000/* INIZIO CONTROLLO                                                           */
002300960320             STRCMTCTL  LCKLVL(*CHG)
002400000000             MONMSG     MSGID(CPF0000)
002500000000
002600000000/* CHIAMA IL PROGRAMMA DI GESTIONE TRAINI                                     */
002700000000             CALL       PGM(TNTL02R) PARM(&KPJBA)
002800000000
002900000000/* FINE CONTROLLO                                                             */
003000000000             ENDCMTCTL
003100000000
003200000000             RCLRSC     LVL(*CALLER)
003300000000             ENDPGM

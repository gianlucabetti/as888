000100000920             PGM        PARM(&KSC &CTR &PRG &FTC &FIE &OFF &DIT)
000200000804             DCL        VAR(&KSC) TYPE(*CHAR) LEN(7)
000300000804             DCL        VAR(&CTR) TYPE(*CHAR) LEN(3)
000400000804             DCL        VAR(&PRG) TYPE(*CHAR) LEN(3)
000500000804             DCL        VAR(&FTC) TYPE(*CHAR) LEN(2)
000600000804             DCL        VAR(&FIE) TYPE(*CHAR) LEN(1)
000700000901             DCL        VAR(&OFF) TYPE(*CHAR) LEN(1)
000800000919             DCL        VAR(&DIT) TYPE(*CHAR) LEN(3)
000900000803  /* TRASCODIFICA DELLA TARIFFA PARTICOLARE */
001000000803             OVRDBF     FILE(TITPTCAR) TOFILE(TITPT01L)
001100000803             OVRDBF     FILE(TITPDCAR) TOFILE(TITPD01L)
001200000901             OVRDBF     FILE(TNTAMCAR) TOFILE(TNTAM01L)
001300000920
001400000920             OVRPRTF    FILE(QSYSPRT) HOLD(*YES)
001500990623
001600990623  /* TARIFFE */
001700000919             CALL       PGM(TNVR48R)  PARM(&KSC &CTR &PRG &FTC &FIE &OFF &DIT)
001800900326
001900000828  /* TRASCODIFICA DELLA TARIFFA PARTICOLARE NEL FILES DELE OFFERTE */
002000000828             OVRDBF     FILE(TITPT01L) TOFILE(TIOPT01L)
002100000828             OVRDBF     FILE(TITPD01L) TOFILE(TIOPD01L)
002200000828             OVRDBF     FILE(TNTAM01L) TOFILE(TNOFM01L)
002300000828
002400000828  /* OFFERTE */
002500000901             CHGVAR     VAR(&OFF) VALUE('O')
002600000919             CALL       PGM(TNVR48R)  PARM(&KSC &CTR &PRG &FTC &FIE &OFF &DIT)
002700000828
002800000828             DLTOVR     *ALL
002900000000             ENDPGM

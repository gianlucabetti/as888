000100000000/******************************************************************************/
000200961224/*  CARICA E APPLICA PTF                                                      */
000300000000/******************************************************************************/
000400961224             PGM
000500990122             MONMSG     MSGID(CPF0000)
000600990122             LODPTF     LICPGM(5769999)
000700990122             LODPTF     LICPGM(5769SS1)
000800990303             LODPTF     LICPGM(5769999) DEV(*SAVF) +
000900990303                          SAVF(EDPSED/QMF19559)
001000990303             LODPTF     LICPGM(5769999) DEV(*SAVF) +
001100990303                          SAVF(EDPSED/QMF20168)
001200990303             LODPTF     LICPGM(5769999) DEV(*SAVF) +
001300990303                          SAVF(EDPSED/QMF20721)
001400990303             LODPTF     LICPGM(5769999) DEV(*SAVF) +
001500990303                          SAVF(EDPSED/QMF20811)
001600990304             LODPTF     LICPGM(5769999) DEV(*SAVF) +
001700990304                          SAVF(EDPSED/QMF20927)
001800990304             LODPTF     LICPGM(5769999) DEV(*SAVF) +
001900990304                          SAVF(EDPSED/QMF20928)
002000990304             LODPTF     LICPGM(5769999) DEV(*SAVF) +
002100990304                          SAVF(EDPSED/QMF21044)
002200990304             LODPTF     LICPGM(5769999) DEV(*SAVF) +
002300990304                          SAVF(EDPSED/QMF21111)
002400990304             LODPTF     LICPGM(5769999) DEV(*SAVF) +
002500990304                          SAVF(EDPSED/QMF21209)
002600990304             LODPTF     LICPGM(5769999) DEV(*SAVF) +
002700990304                          SAVF(EDPSED/QMF21363)
002800990304             LODPTF     LICPGM(5769SS1) DEV(*SAVF) +
002900990304                          SAVF(EDPSED/QSF49034)
003000970115
003100990109             APYPTF     LICPGM(5769999) DELAYED(*IMMDLY)
003200990109             APYPTF     LICPGM(5769SS1) DELAYED(*IMMDLY)
003300970827
003400990122             DLTF       FILE(EDPSED/QMF*)
003500990122             DLTF       FILE(EDPSED/QSF*)
003600990304             ADDLIBLE GAICMZ
003700990304             SBMJOB     CMD(CALL PGM(LINEAISD2)) JOBQ(QCTL)
003800000000             ENDPGM

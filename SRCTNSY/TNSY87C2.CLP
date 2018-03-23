000100000000  /*---------------------------------------------------------------*/
000200930529  /*     CONTROLLA ESISTENZA SPUNTE DA AGGIORNARE                  */
000300000000  /*---------------------------------------------------------------*/
000400021010             PGM        PARM(&UTE)
000600920603             DCL        VAR(&UTE)    TYPE(*CHAR)  LEN(3)
001000021010             DCLF       FILE(TNSY87D2)
001600930529
001700021010             CALL       PGM(TNSY87C3) PARM(&FIL0 &DES0 &REC0 &FIL1 +
001800021010                          &DES1 &REC1 &FIL2 &DES2 &REC2 &FIL3 &DES3 +
001900021010                          &REC3 &FIL4 &DES4 &REC4 &FIL5 &DES5 &REC5 +
002000021010                          &FIL6 &DES6 &REC6 &FIL7 &DES7 &REC7 &FIL8 +
002100021010                          &DES8 &REC8 &FIL9 &DES9 &REC9 &FILA &DESA +
002102021010                          &RECA &FILB &DESB &RECB &FILC &DESC &RECC +
002103021010                          &UTE)
002200920603
002300000000 /*------- EMISSIONE FORMATO -----------------------*/
002400000000 EMISS:
002500900103
002600021010             SNDRCVF    RCDFMT(SY87D02)
002700890915             IF         COND(&IN12) THEN(GOTO CMDLBL(FINE))
002800900103
002900000000             GOTO       CMDLBL(EMISS)
003000000000 FINE:       ENDPGM

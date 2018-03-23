000100000000  /*---------------------------------------------------------------*/
000200930529  /*     CONTROLLA ESISTENZA SPUNTE DA AGGIORNARE                  */
000300000000  /*---------------------------------------------------------------*/
000400920401             PGM
000500920603             DCL        VAR(&USER)    TYPE(*CHAR)  LEN(10)
000600920603             DCL        VAR(&UTE)    TYPE(*CHAR)  LEN(3)
000700921109             DCL        VAR(&REM)    TYPE(*CHAR)  LEN(6)
000900930529             DCL        VAR(&X) TYPE(*DEC) LEN(2 0) VALUE(0)
001000941119             DCLF       FILE(TNSY98D)
001100930529
001200930529             QSYS/RTVJOBA USER(&USER)
001300930529
001400930529             CHGVAR     VAR(%SUBSTRING(&UTE 1 3)) +
001500930529                          VALUE(%SUBSTRING(&USER 1 3))
001600930529
001700000713             CALL       PGM(TNSY98C1) PARM(&FIL0 &DES0 &REC0 &FIL1 +
001800000713                          &DES1 &REC1 &FIL2 &DES2 &REC2 &FIL3 &DES3 +
001900000713                          &REC3 &FIL4 &DES4 &REC4 &FIL5 &DES5 &REC5 +
002000000713                          &FIL6 &DES6 &REC6 &FIL7 &DES7 &REC7 &FIL8 +
002100000713                          &DES8 &REC8 &FIL9 &DES9 &REC9 &FILA &DESA +
002102000713                          &RECA &FILB &DESB &RECB &FILC &DESC &RECC)
002200920603
002300000000 /*------- EMISSIONE FORMATO -----------------------*/
002400000000 EMISS:
002500900103
002600941119             SNDRCVF    RCDFMT(SY98D01)
002700890915             IF         COND(&IN12) THEN(GOTO CMDLBL(FINE))
002800900103
002900000000             GOTO       CMDLBL(EMISS)
003000000000 FINE:       ENDPGM

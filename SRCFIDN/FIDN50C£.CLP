000100990217             PGM        PARM(&KPJBA)
000200990204
000300040923/* Dichiarazione variabili ______________________________________________________________________ */
000400040923             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
000500040923             DCL        VAR(&L275) TYPE(*CHAR) LEN(275)
000600040923             DCL        VAR(&CMD) TYPE(*CHAR) LEN(350)
000700040923             DCL        VAR(&LUNG) TYPE(*DEC) LEN(15 5) VALUE(350)
000800040923
000900040923/* Reperisco la lista librerie CORRENTE _________________________________________________________ */
001000040923             RTVJOBA    USRLIBL(&L275)
001100040923
001200040923/* FORZO S.I. DEL P.O. __________________________________________________________________________ */
001300040923
001400040923             CALL       PGM(TRUL07C) PARM('FILTRA201')
001500930402
001600040923/* CALCOLO RESPONSABILITA _______________________________________________________________________ */
001700040923
001800040923             CALL       PGM(FIDN50R) PARM(&KPJBA)
001900040923
002000040923/* Reimposto la lista librerie originale ________________________________________________________ */
002100040923             CHGVAR     VAR(&CMD) VALUE('CHGLIBL LIBL(' *CAT &L275 +
002200040923                          *CAT ' )')
002300040923             CALL       PGM(QCMDEXC) PARM(&CMD &LUNG)
002400040923
002500930402
002600980917             ENDPGM

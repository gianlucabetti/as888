000100941229 TRIS30C:    PGM        PARM(&KPJBA)
000200900530 /*    DOPO COD.FILIALE SE C'E' "AGG"  AGGIORNA FLAG TRASM. SU ARCHIVI SEDE   */
000300891006             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
000400891011             DCL        VAR(&SATELI) TYPE(*CHAR) LEN(9)
000500891011             DCL        VAR(&SATELO) TYPE(*CHAR) LEN(9)
000600900525             DCL        VAR(&FIL) TYPE(*CHAR) LEN(3)
000700900525             DCL        VAR(&SYS) TYPE(*CHAR) LEN(3)
000800900525             DCL        VAR(&RISP) TYPE(*CHAR) LEN(1)
000900900530 /*                                                                           */
001000900525             CHGVAR     VAR(&FIL) VALUE(%SUBSTRING(&KPJBA 247 3))
001100900525             CHGVAR     VAR(&SYS) VALUE(%SUBSTRING(&KPJBA 98 3))
001200891011             CHGVAR     VAR(&SATELI) VALUE(%SUBSTRING(&KPJBA 92 9))
001300900525
001400891211
001500900525 /*  PREPARAZIONE ARCHIVI DA TRASFERIRE                                       */
001600900525 /*  ----------------------------------                                       */
001700891128
001800900525 /*  CNACO- CNIND - CNCLP                                                     */
001900900525
002000941229             CALL       PGM(TRIS35R) PARM(&KPJBA)
002100900531
002200921116 /*  TABELLE                                                                  */
002300900531
002400941229             CALL       PGM(TRIS33R) PARM(&KPJBA)
002500900601
002600900525 ENDPGM:     ENDPGM

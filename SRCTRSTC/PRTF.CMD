000100070911/*PARMS PGM(*LIBL/STCPRTF)                                             */
000200070911
000300000000             CMD        PROMPT('Dizionario file')
000400000000
000500000000             PARM       KWD(FILE) TYPE(PRTF1) MIN(1) PROMPT('Nome +
000600000000                          file')
000700000000
000800000000             PARM       KWD(OUTPUT) TYPE(*CHAR) LEN(6) RSTD(*YES) +
000900000000                          DFT(*) VALUES(* *PRINT) CHOICE('*, +
001000000000                          *PRINT') PROMPT('Emissione')
001100000000
001200000000             PARM       KWD(OUTQ) TYPE(PRTF1) PMTCTL(*PMTRQS) +
001300000000                          PROMPT('Coda di emissione')
001400000000
001500000000             PARM       KWD(COPIE) TYPE(*DEC) LEN(3 0) DFT(1) +
001600000000                          RANGE(1 255) CHOICE('1-255') +
001700000000                          PMTCTL(*PMTRQS) PROMPT('Copie')
001800000000
001900000000             PARM       KWD(CAMPI) TYPE(*NAME) LEN(3) RSTD(*YES) +
002000000000                          DFT(*SI) SPCVAL((*SI) (*NO)) CHOICE('*SI, +
002100000000                          *NO') PMTCTL(*PMTRQS) PROMPT('Dettaglio +
002200000000                          campi del file')
002300000000
002400000000             PARM       KWD(SAVE) TYPE(*NAME) LEN(4) RSTD(*YES) +
002500000000                          DFT(*NO) SPCVAL((*YES) (*NO)) +
002600000000                          CHOICE('*YES, *NO') PMTCTL(*PMTRQS) +
002700000000                          PROMPT('File salvataggio')
002800000000
002900000000 PRTF1:      QUAL       TYPE(*NAME) LEN(10)
003000000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) +
003100000000                          SPCVAL((*LIBL)) CHOICE('*LIBL, Nome') +
003200000000                          PROMPT('Nome libreria')

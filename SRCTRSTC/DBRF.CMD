000100000000             CMD        PROMPT('Dizionario file dipendenti')
000200000000
000300000000             PARM       KWD(FILE) TYPE(DBRF1) MIN(1) PROMPT('Nome +
000400000000                          file')
000500000000
000600000000             PARM       KWD(OUTPUT) TYPE(*CHAR) LEN(6) RSTD(*YES) +
000700000000                          DFT(*) VALUES(* *PRINT) CHOICE('*, +
000800000000                          *PRINT') PROMPT('Emissione')
000900000000
001000000000             PARM       KWD(OUTQ) TYPE(DBRF1) PMTCTL(*PMTRQS) +
001100000000                          PROMPT('Coda di emissione')
001200000000
001300000000             PARM       KWD(COPIE) TYPE(*DEC) LEN(3 0) DFT(1) +
001400000000                          RANGE(1 255) CHOICE('1-255') +
001500000000                          PMTCTL(*PMTRQS) PROMPT('Copie')
001600000000
001700000000             PARM       KWD(CAMPI) TYPE(*NAME) LEN(3) RSTD(*YES) +
001800000000                          DFT(*SI) SPCVAL((*SI) (*NO)) CHOICE('*SI, +
001900000000                          *NO') PMTCTL(*PMTRQS) PROMPT('Dettaglio +
002000000000                          campi del file')
002100000000
002200000000             PARM       KWD(SAVE) TYPE(*NAME) LEN(4) RSTD(*YES) +
002300000000                          DFT(*NO) SPCVAL((*YES) (*NO)) +
002400000000                          CHOICE('*YES, *NO') PMTCTL(*PMTRQS) +
002500000000                          PROMPT('File salvataggio')
002600000000
002700000000 DBRF1:      QUAL       TYPE(*NAME) LEN(10)
002800000000             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) +
002900000000                          SPCVAL((*LIBL)) CHOICE('*LIBL, Nome') +
003000000000                          PROMPT('Nome libreria')

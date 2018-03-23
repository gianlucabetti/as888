000100100817/*PRM PGM(*LIBL/UBONOFFC)                                            */
000200100817/*END                                                                */
000300100817
000400100817             CMD        PROMPT('Visualizzazione utilizzo *in')
000500100817
000600100817             PARM       KWD(LIBSRCF) TYPE(P1)    MIN(1)  MAX(1)     +
000700100817                          ALWVAR(*YES) PROMPT('Source file')
000800100817             PARM       KWD(MBR)     TYPE(*CHAR) LEN(10) MIN(1)     +
000900100817                          PROMPT('Source member')
001000100817             PARM       KWD(TYPE)    TYPE(*CHAR) LEN(10) RSTD(*YES) +
001100100817                          DFT(RPGLE) VALUES(RPG +
001200100817                                            SQLRPG +
001300100817                                            RPGLE +
001400100817                                            SQLRPGLE +
001500100817                                            DSPF +
001600100817                                            PRTF)    +
001700100817                          PROMPT('Source type')
001800100817
001900100817             PARM       KWD(INDDS) TYPE(*CHAR) LEN(4) RSTD(*YES) +
002000100817                          DFT(*YES) VALUES(*NO *YES) PMTCTL(INDDS) +
002100100817                          PROMPT('Even INDDS''s indicators')
002200100817
002300100817 P1:         QUAL       TYPE(*NAME)  LEN(10)     MIN(1)
002400100817             QUAL       TYPE(*NAME)  LEN(10)     DFT(GAITRASRC) +
002500100817                          SPCVAL((*LIBL)) PROMPT('Library')
002600100817
002700100817 INDDS:      PMTCTL     CTL(TYPE) COND((*EQ 'RPGLE') +
002800100817                                       (*EQ 'SQLRPGLE')) +
002900100817                          NBRTRUE(*EQ 1)

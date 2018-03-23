000100100811/*PRM PGM(*LIBL/UBDDSC)                                                 */
000200100811/*END                                                                   */
000300100811             CMD        PROMPT('Display FFD')
000400100811
000500100811             PARM       KWD(FILE) TYPE(Q1) MIN(1) PROMPT('File')
000600100811
000700100811             PARM       KWD(FLDNAM) TYPE(*NAME) DFT(*NONE) +
000800100811                          SPCVAL((*NONE)) MIN(0) PROMPT('Ricerca +
000900100811                          campo')
001000100811
001100100811 Q1:         QUAL       TYPE(*NAME) MIN(1)
001200100811             QUAL       TYPE(*NAME) DFT(*LIBL) SPCVAL((*LIBL)) +
001300100811                          PROMPT('Libreria')

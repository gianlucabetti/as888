000100150430/*PARMS PGM(UBLANCIOC)                                                */
000200150430/*PRM   PGM(UBLANCIOC)                                                */
000300150430/*END                                                                 */
000400150504             CMD        PROMPT('Lancio interattivo pgm in ABPB')
000500031020
000600031020/* X0206 */  PARM       KWD(PGM) TYPE(*NAME) LEN(10) DFT(*PROMPT) +
000700031020                          SPCVAL((*PROMPT)) PROMPT('Programma')

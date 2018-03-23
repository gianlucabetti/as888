000100170503/*PRM PGM(*LIBL/TRULDIRC)                                               */
000200110221/*END                                                                  */
000300170503             CMD        PROMPT('ALWSAV *YES su obj in DIR IFS')
000400110221
000500170503             PARM       KWD(IFSDIR) TYPE(*CHAR) LEN(256) PROMPT('DIR +
000600170503                          IFS')
000700170508             PARM       KWD(FOLDERSN) TYPE(*CHAR) LEN(1) RSTD(*YES) +
000800170508                          DFT('S') VALUES('S' 'N') MIN(0) +
000900170508                          FULL(*YES) EXPR(*NO) PROMPT('Folder Si/No')

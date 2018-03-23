000100130204             CMD        PROMPT('Convert Tiff to Pdf')
000200000000
000300000000             PARM       KWD(TIFFNAME) TYPE(*PNAME) LEN(120) MIN(1) +
000400000000                          PROMPT('TIFF name')
000500000000
000600000000             PARM       KWD(PDFNAME) TYPE(*PNAME) LEN(120) MIN(1) +
000700000000                          PROMPT('PDF name')
000800000000
000900000000             PARM       KWD(ORIENT) TYPE(*CHAR) LEN(10) RSTD(*YES) +
001000000000                          DFT(*PORTRAIT) VALUES(*PORTRAIT +
001100000000                          *LANDSCAPE *AUTO) SPCVAL((*PORTRAIT P) +
001200000000                          (*LANDSCAPE L) (*AUTO A)) MIN(0) +
001300000000                          CHOICE('*PORTRAIT, *LANDSCAPE, *AUTO') +
001400000000                          PROMPT('Orientation')
001500000000
001600000000             PARM       KWD(RESIZE) TYPE(*CHAR) LEN(4) RSTD(*YES) +
001700000000                          DFT(*NO) VALUES(*YES *NO) SPCVAL((*YES Y) +
001800000000                          (*NO N)) CHOICE('*YES, *NO') +
001900000000                          PROMPT('Resize image')
002000000000
002100130204             PARM       KWD(JSERVER) TYPE(*CHAR) LEN(4) RSTD(*YES) +
002200130204                          DFT(*NO) VALUES(*YES *NO) PROMPT('Use PDF +
002300130204                          Server')
002400000000
002500000000             PARM       KWD(JDEBUG) TYPE(*CHAR) LEN(4) RSTD(*YES) +
002600130214                          DFT(*NO) VALUES(*YES *NO *PRT) MIN(0) +
002700000000                          PROMPT('Debug')
002800000000

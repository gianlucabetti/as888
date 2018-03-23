000100000000             CMD        PROMPT('Convert PDF to TIFF')
000200000000
000300000000             PARM       KWD(PDFNAME) TYPE(*PNAME) LEN(128) MIN(1) +
000400000000                          PROMPT('PDF name')
000500000000
000600000000             PARM       KWD(TIFFNAME) TYPE(*PNAME) LEN(128) MIN(1) +
000700000000                          PROMPT('TIFF Name')
000800000000
000900000000             PARM       KWD(DEVICE) TYPE(*CHAR) LEN(20) +
001000000000                          DFT('tiffg4') PROMPT('Device name')
001100000000
001200000000             PARM       KWD(RESOLUTION) TYPE(*DEC) LEN(4 0) DFT(300) +
001300130212                          RANGE(0 1200) SPCVAL((*DFT 0)) +
001400130212                          CHOICE('0-1200') PROMPT('Resolution')
001500000000
001600000000             PARM       KWD(OUTPUT) TYPE(*CHAR) LEN(8) RSTD(*YES) +
001700000000                          DFT(*NONE) VALUES(* *NONE *PRINT) +
001800000000                          PROMPT('Log')

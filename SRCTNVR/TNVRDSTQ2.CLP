000100981203             PGM
000101000527             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
001301000527             DCL        VAR(&WDST) TYPE(*CHAR) LEN(3)
001302000527             DCL        VAR(&CODAAS) TYPE(*CHAR) LEN(7) +
001303000527                          VALUE('STTOxxx')
001304000527             DCLF       FILE(GAICMZ/CMDST00F)
001305000527             RTVNETA    SYSNAME(&SYSNAME)
004604000527
004605000527READ:        RCVF
004606000527             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(ENDF))
004607000527             CHGVAR     VAR(&WDST) VALUE(&DSTDST)
004608000527             CHGVAR     VAR(%SST(&CODAAS 5 3)) VALUE(%SST(&WDST 1 3))
004609000608             IF         COND(&DSTCPN *EQ &SYSNAME) THEN(+
004611000527             CRTOUTQ    OUTQ(QGPL/&CODAAS) RMTSYS(&DSTCPN) +
004612000527                          RMTPRTQ(STFRMAS) AUTOSTRWTR(1) +
004613000608                          TEXT('Stampe da Altri AS'))
004653000527
004654000527             GOTO       CMDLBL(READ)
004655000527
004656000527ENDF:
004657000527
004700970227             ENDPGM

000100030509             PGM        PARM(&KPJBA)
000200030910
000300030910 /* CL DI CREAZIONE FILE DI PROVA FATTURAZIONE    */
000400030910
000500030509             DCL        VAR(&KPJBA) TYPE(*CHAR) LEN(502)
000600030910             DCL        VAR(&PULISCO) TYPE(*CHAR) LEN(2)
000700030910             DCL        VAR(&INVIO) TYPE(*CHAR) LEN(2)
000800030910             CHGVAR     VAR(&PULISCO) VALUE(%SUBSTRING(&KPJBA 276 2))
000900030910             CHGVAR     VAR(&INVIO) VALUE(%SUBSTRING(&KPJBA 278 2))
001000030910
001100030910 /* SE RICHIESTA PULIZIA DEGLI ARCHIVI DI PROVA */
001200030910
001300030910             IF         COND(&PULISCO = 'SI') THEN(DO)
001400030910             CLRPFM     FILE(EDPPRVFAT/TITAS00F)
001500030910             CLRPFM     FILE(EDPPRVFAT/TITA700F)
001600030910             CLRPFM     FILE(EDPPRVFAT/TITAA00F)
001700030910             CLRPFM     FILE(EDPPRVFAT/TITA400F)
001800030910             CLRPFM     FILE(EDPPRVFAT/TNCSB00F)
001900030910             CLRPFM     FILE(EDPPRVFAT/FIAR500F)
002000050930             CLRPFM     FILE(EDPPRVFAT/FNEVB00F)
002100030910             ENDDO
002200030910
002300030206             OVRDBF     FILE(TITAS38C) TOFILE(GAITRAGRU/TITAS38C)
002400030509             OVRDBF     FILE(TITAS31C) TOFILE(GAITRAGRU/TITAS31C)
002500030206             OVRDBF     FILE(TITA730C) TOFILE(GAITRAGRU/TITA730C)
002600030206             OVRDBF     FILE(TITAA30C) TOFILE(GAITRAGRU/TITAA30C)
002700030206             OVRDBF     FILE(TITA430C) TOFILE(GAITRAGRU/TITA430C)
002800030206             OVRDBF     FILE(TNCSB03L) TOFILE(GAITRAGRU/TNCSB03L)
002900040531             OVRDBF     FILE(FIAR531C) TOFILE(GAITRAGRU/FIAR531C)
003000050930             OVRDBF     FILE(FNEVB04L) TOFILE(UNITRAGRU/FNEVB04L)
003100030910
003200030910             OVRDBF     FILE(TITAS00F) TOFILE(EDPPRVFAT/TITAS00F)
003300030910             OVRDBF     FILE(TITA700F) TOFILE(EDPPRVFAT/TITA700F)
003400030910             OVRDBF     FILE(TITAA00F) TOFILE(EDPPRVFAT/TITAA00F)
003500030910             OVRDBF     FILE(TITA400F) TOFILE(EDPPRVFAT/TITA400F)
003600030910             OVRDBF     FILE(TNCSB00F) TOFILE(EDPPRVFAT/TNCSB00F)
003700030910             OVRDBF     FILE(FIAR500F) TOFILE(EDPPRVFAT/FIAR500F)
003800050930             OVRDBF     FILE(FNEVB00F) TOFILE(EDPPRVFAT/FNEVB00F)
003900030910
004000030910             CALL       PGM(TNVRPF2R) PARM(&KPJBA)
004100030910
004200030910 /* SE RICHIESTO INVIO  DEGLI ARCHIVI DI PROVA */
004300030910
004400030910             IF         COND(&INVIO = 'SI') THEN(DO)
004500030910
004600030910
004700030910             DLTF       FILE(QTEMP/SPIA)
004800030910             MONMSG     MSGID(CPF0000) EXEC(DO)
004900030910             CRTDDMF    FILE(QTEMP/SPIA) RMTFILE(QGPL/SPIA) +
005000030910                          RMTLOCNAME(AS888)
005100030910             MONMSG     MSGID(CPF0000)  EXEC(DO)
005200030910             SNDPGMMSG  MSG('Errore fare pulizia ed invii a mano')
005300030910             GOTO       CMDLBL(FINE)
005400030910             ENDDO
005500030910             ENDDO
005600030910
005700030910 /* PRIMA PULISCO I DATI IN AS888 */
005800030910
005900030910             SBMRMTCMD  CMD('DLTF FILE(EDPPRVFAT/TITAS00F)') +
006000030910                          DDMFILE(QTEMP/SPIA)
006100030910             MONMSG     MSGID(CPF0000)
006200030910             SBMRMTCMD  CMD('DLTF FILE(EDPPRVFAT/TITA700F)') +
006300030910                          DDMFILE(QTEMP/SPIA)
006400030910             MONMSG     MSGID(CPF0000)
006500030910             SBMRMTCMD  CMD('DLTF FILE(EDPPRVFAT/TITA400F)') +
006600030910                          DDMFILE(QTEMP/SPIA)
006700030910             MONMSG     MSGID(CPF0000)
006800030910             SBMRMTCMD  CMD('DLTF FILE(EDPPRVFAT/TITAA00F)') +
006900030910                          DDMFILE(QTEMP/SPIA)
007000030910             MONMSG     MSGID(CPF0000)
007100030910             SBMRMTCMD  CMD('DLTF FILE(EDPPRVFAT/TNCSB00F)') +
007200030910                          DDMFILE(QTEMP/SPIA)
007300030910             MONMSG     MSGID(CPF0000)
007400050930             SBMRMTCMD  CMD('DLTF FILE(EDPPRVFAT/FIAR500F)') +
007500030910                          DDMFILE(QTEMP/SPIA)
007600030910             MONMSG     MSGID(CPF0000)
007700050930             SBMRMTCMD  CMD('DLTF FILE(EDPPRVFAT/FNEVB00F)') +
007800050930                          DDMFILE(QTEMP/SPIA)
007900050930             MONMSG     MSGID(CPF0000)
008000030910
008100030910 /* INVIO I FILE  IN AS888 */
008200030910
008300030910             SAVRSTOBJ  OBJ(TITAS00F) LIB(EDPPRVFAT) +
008400030910                          RMTLOCNAME(AS888) ALWOBJDIF(*NONE) +
008500030910                          RSTLIB(EDPPRVFAT)
008600030910
008700030910             SAVRSTOBJ  OBJ(TITA700F) LIB(EDPPRVFAT) +
008800030910                          RMTLOCNAME(AS888) ALWOBJDIF(*NONE) +
008900030910                          RSTLIB(EDPPRVFAT)
009000030910
009100030910             SAVRSTOBJ  OBJ(TITA400F) LIB(EDPPRVFAT) +
009200030910                          RMTLOCNAME(AS888) ALWOBJDIF(*NONE) +
009300030910                          RSTLIB(EDPPRVFAT)
009400030910
009500030910             SAVRSTOBJ  OBJ(TITAA00F) LIB(EDPPRVFAT) +
009600030910                          RMTLOCNAME(AS888) ALWOBJDIF(*NONE) +
009700030910                          RSTLIB(EDPPRVFAT)
009800030910
009900030910             SAVRSTOBJ  OBJ(TNCSB00F) LIB(EDPPRVFAT) +
010000030910                          RMTLOCNAME(AS888) ALWOBJDIF(*NONE) +
010100030910                          RSTLIB(EDPPRVFAT)
010200030910
010300030910             SAVRSTOBJ  OBJ(FIAR500F) LIB(EDPPRVFAT) +
010400030910                          RMTLOCNAME(AS888) ALWOBJDIF(*NONE) +
010500030910                          RSTLIB(EDPPRVFAT)
010600050930
010700050930             SAVRSTOBJ  OBJ(FNEVB00F) LIB(EDPPRVFAT) +
010800050930                          RMTLOCNAME(AS888) ALWOBJDIF(*NONE) +
010900050930                          RSTLIB(EDPPRVFAT)
011000030910
011100030910             DLTF       FILE(QTEMP/SPIA)
011200030910             MONMSG     MSGID(CPF0000)
011300030910
011400030910             ENDDO
011500030910FINE:
011600030910
011700011113             DLTOVR *ALL
011800011113ENDPGM

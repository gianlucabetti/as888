000100120703     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('UBBNDDIR')
000200141103     FDATE_CONV UF   E             DISK
000300120703     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
000400141103      /free
000500141103        read DATE_CONV;
000600141103        dou %eof();
000700141103          // DC_MD
000800141103          DC_MD = DC_MM * 100 + DC_DD;
000900141103          // DC_MD_CHAR
001000141107          DC_MD_CHAR= %editc(DC_MD : 'X');
001100141103          // DC_DMYY_Z
001200141103          DC_DMYY_Z = %dec(
001300141103                       UBFMTDATE_Convert
001400141103                       (DC_YYMD_C:'YYYYMMDD':'DDMMYYYY')
001500141103                       : 8 : 0);
001600141103          // DC_DMYY_A1
001700141103          DC_DMYY_A1 =
001800141103                       UBFMTDATE_Convert
001900141103                       (DC_YYMD_C:'YYYYMMDD':'DD/MM/YYYY');
002000141103          // DC_DMYY_A2
002100141103          DC_DMYY_A2 =
002200141103                       UBFMTDATE_Convert
002300141103                       (DC_YYMD_C:'YYYYMMDD':'DD-MM-YYYY');
002400141103          // DC_DMYY_A3
002500141103          DC_DMYY_A3 =
002600141103                       UBFMTDATE_Convert
002700141103                       (DC_YYMD_C:'YYYYMMDD':'DD.MM.YYYY');
002800141103          // DC_DMY_Z
002900141103          DC_DMY_Z =  %dec(
003000141103                       %subst(%editc(DC_DMYY_Z:'X'):1:4) +
003100141103                       %subst(%editc(DC_DMYY_Z:'X'):7:2)
003200141103                       : 6 : 0);
003300141103          // DC_DMY_A
003400141107          DC_DMY_A= %editc(DC_DMY_Z : 'X');
003500141103
003600141103          update DATECONV;
003700141103
003800141103          read DATE_CONV;
003900141103        enddo;
004000141103      /end-free
004100111214     c                   seton                                        lr

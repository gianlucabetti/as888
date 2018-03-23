000100120703     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('UBBNDDIR')
000200141110     FDATECNV0F UF   E             DISK
000300141110     D dayNbr          S             11p 0
000400120703     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
000500141103      /free
000600141110
000700141110           // 1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6=Sat, 7=Sun
000800141110           exec sql  set :dayNbr = dayofweek_iso(curdate());
000900141110
001000141110        read DATECONV;
001100141103        dou %eof();
001200141110          // DC_YYMD_A1
001300141110          DC_YYMD_A1 =
001400141103                       UBFMTDATE_Convert
001500141110                       (DC_YYMD_C:'YYYYMMDD':'YYYY/MM/DD');
001600141110          // DC_YYMD_A2
001700141110          DC_YYMD_A2 =
001800141110                       UBFMTDATE_Convert
001900141110                       (DC_YYMD_C:'YYYYMMDD':'YYYY-MM-DD');
002000141110          // DC_YYMD_A3
002100141110          DC_YYMD_A3 =
002200141110                       UBFMTDATE_Convert
002300141110                       (DC_YYMD_C:'YYYYMMDD':'YYYY.MM.DD');
002400141110          // DC_DAY_NAI
002500141110        // dateAlfa = %char(%date(date80 : *iso));
002600141110           exec sql  set :dayNbr = dayofweek_iso(:dc_date);
002700141110           select;
002800141110             when dayNbr = 1;
002900141110               DC_DAY_NAI = 'Lunedì';
003000141110             when dayNbr = 2;
003100141110               DC_DAY_NAI = 'Martedì';
003200141110             when dayNbr = 3;
003300141110               DC_DAY_NAI = 'Mercoledì';
003400141110             when dayNbr = 4;
003500141110               DC_DAY_NAI = 'Giovedì';
003600141110             when dayNbr = 5;
003700141110               DC_DAY_NAI = 'Venerdì';
003800141110             when dayNbr = 6;
003900141110               DC_DAY_NAI = 'Sabato';
004000141110             when dayNbr = 7;
004100141110               DC_DAY_NAI = 'Domenica';
004200141110           endsl;
004300141103
004400141103          update DATECONV;
004500141103
004600141110          read DATECONV;
004700141103        enddo;
004800141103      /end-free
004900111214     c                   seton                                        lr

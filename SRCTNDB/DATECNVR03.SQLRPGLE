000100120703     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('UBBNDDIR')
000200141110     FDATECNV0F UF   E             DISK
000300141112     D monthNbr        S              3p 0
000400120703     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
000500141103      /free
000600141110
000700141110        read DATECONV;
000800141103        dou %eof();
000900141112          // DC_MO_NMI
001000141112          // DC_MO_ABVI
001100141112           exec sql  set :monthNbr = month(:dc_date);
001200141110           select;
001300141112             when monthNbr = 1;
001400141112               DC_MO_NMI = 'Gennaio';
001500141112               DC_MO_ABVI = 'Gen';
001600141112             when monthNbr = 2;
001700141112               DC_MO_NMI = 'Febbraio';
001800141112               DC_MO_ABVI = 'Feb';
001900141112             when monthNbr = 3;
002000141112               DC_MO_NMI = 'Marzo';
002100141112               DC_MO_ABVI = 'Mar';
002200141112             when monthNbr = 4;
002300141112               DC_MO_NMI = 'Aprile';
002400141112               DC_MO_ABVI = 'Apr';
002500141112             when monthNbr = 5;
002600141112               DC_MO_NMI = 'Maggio';
002700141112               DC_MO_ABVI = 'Mag';
002800141112             when monthNbr = 6;
002900141112               DC_MO_NMI = 'Giugno';
003000141112               DC_MO_ABVI = 'Giu';
003100141112             when monthNbr = 7;
003200141112               DC_MO_NMI = 'Luglio';
003300141112               DC_MO_ABVI = 'Lug';
003400141112             when monthNbr = 8;
003500141112               DC_MO_NMI = 'Agosto';
003600141112               DC_MO_ABVI = 'Ago';
003700141112             when monthNbr = 9;
003800141112               DC_MO_NMI = 'Settembre';
003900141112               DC_MO_ABVI = 'Set';
004000141112             when monthNbr = 10;
004100141112               DC_MO_NMI = 'Ottobre';
004200141112               DC_MO_ABVI = 'Ott';
004300141112             when monthNbr = 11;
004400141112               DC_MO_NMI = 'Novembre';
004500141112               DC_MO_ABVI = 'Nov';
004600141112             when monthNbr = 12;
004700141112               DC_MO_NMI = 'Dicembre';
004800141112               DC_MO_ABVI = 'Dic';
004900141110           endsl;
005000141103
005100141103          update DATECONV;
005200141103
005300141110          read DATECONV;
005400141103        enddo;
005500141103      /end-free
005600111214     c                   seton                                        lr

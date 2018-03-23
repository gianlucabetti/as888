000100120703     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('UBBNDDIR')
000200141110     FDATECNV0F UF   E             DISK
000300120703     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
000400141103      /free
000500141110
000600141110        read DATECONV;
000700141103        dou %eof();
000800150112          // DC_YOY_ISO
000900141110           select;
001000150113             when DC_WOY_ISO > 51
001100150113              and DC_WOY < 4;
001200150112               DC_YOY_ISO = DC_YEAR - 1;
001300150113             when DC_WOY_ISO < 4
001400150113              and DC_WOY > 51;
001500150112               DC_YOY_ISO = DC_YEAR + 1;
001600150112             other;
001700150112               DC_YOY_ISO = DC_YEAR;
001800141110           endsl;
001900141103
002000141103          update DATECONV;
002100141103
002200141110          read DATECONV;
002300141103        enddo;
002400141103      /end-free
002500111214     c                   seton                                        lr

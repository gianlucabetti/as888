000100140114      /TITLE UDF Reperisce Descrizione Disposizione
000200020712
000300140117     **  CREATE FUNCTION GAITRAOBJ/getdispdes
000400140117     **  (societa char(3) ,
000500140117     **  datadistinta Date ,
000600140117     **  nrdistinta DECIMAL(9, 0) ,
000700140117     **  nrdisposizione decimal(15, 0) )
000800140117     **  RETURNS char(560)
000900140117     **  RETURNS NULL ON NULL INPUT
001000140117     **  LANGUAGE RPGLE
001100140117     **  modifies sql  data
001200140117     **  parameter style sql
001300140117     **  external name getdispdes
001400140117     **  not fenced
001500140115     **
001600140115     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('PJXBND')
001700140115
001800140114     D societa         S              3
001900140114     D datadist        S               D
002000140114     D nrdist          S              9  0
002100140114     D nrdispo         S             15  0
002200140114     D dispodes        S            560
002300140114     D INsocieta       S              5I 0
002400140114     D INdatadist      S              5I 0
002500140114     D INnrdist        S              5I 0
002600140114     D INnrdispo       S              5I 0
002700140114     D INdispodes      S              5I 0
002800020705     D SQLState        S              5
002900020705     D FunctionName    S            139
003000020705     D SpecificName    S            128
003100140114     D MsgText         S             70    VARYING
003200140114     D rpyEsito        S             10I 0
003300140114     D  rtnk61         S              1
003400140114
003500140114     D YK61R           PR                  EXTPGM('YK61R')
003600140114     D  rtnk61                        1
003700140114
003800140114     D yk531ddpI     E DS                  QUALIFIED
003900140114     D                                     INZ
004000140114     D yk531ddpO     E DS                  QUALIFIED
004100140114     D  ddpAry                       90A   DIM(5)
004200140114     D                                     OVERLAY(ddp)
004300140114     D yk531ddpr       PR                  EXTPGM('YK531DDPR')
004400140114     D  rqsOpCode...
004500140114     D                               10A   CONST
004600140114     D  rpyEsito...
004700140114     D                               10I 0 CONST
004800140114     D  rqsFormatName...
004900140114     D                               10A   CONST
005000140114     D  rqsData...
005100140114     D                                     LIKE(yk531ddpI)
005200140114     D  rqsDataSize...
005300140114     D                               10I 0 CONST
005400140114     D  rpyFormatName...
005500140114     D                               10A   CONST
005600140114     D  rpyData...
005700140114     D                                     LIKE(yk531ddpO)
005800140114     D  rpyDataSize...
005900140114     D                               10I 0 CONST
006000140114
006100020705     C     *ENTRY        PLIST
006200020705     ** Parametri di input.
006300140114     C                   PARM                    societa
006400140114     C                   PARM                    datadist
006500140114     C                   PARM                    nrdist
006600140114     C                   PARM                    nrdispo
006700020705     ** Parametri di output.
006800140114     C                   PARM                    DispoDes
006900020705     ** Parametri di indicatore nullo.
007000140114     C                   PARM                    INsocieta
007100140114     C                   PARM                    INdatadist
007200140114     C                   PARM                    INnrdist
007300140114     C                   PARM                    INnrdispo
007400140114     C                   PARM                    INdispoDes
007500140114     ** Parametri di funzione SQL.
007600140114     C                   PARM                    SQLState
007700140114     C                   PARM                    FunctionName
007800140114     C                   PARM                    SpecificName
007900140114     C                   PARM                    MsgText
008000140114
008100140114      /free
008200140114          reset INDispodes;
008300140114          SQLState = *ZERO;
008400031110
008500140114          exsr desdistinta;
008600140114
008700140114          if dispodes = *blanks;
008800140114          INDispoDes = -1;
008900140114          endif;
009000140114
009100140114          return;
009200140114
009300140114          begsr *inzsr;
009400140115          // callp(e) YK61R(rtnk61);
009500140115          //  if %error;
009600140115          //  INDispoDes = -1;
009700140115          //  return;
009800140115          //  endif;
009900140114          endsr;
010000140114
010100140114          begsr desDistinta;
010200140114
010300140114          reset  yk531ddpI;
010400140114          reset  yk531ddpO;
010500140114          yk531ddpI.societa = societa;
010600140114          yk531ddpI.ddt = datadist;
010700140114          yk531ddpI.ndt = nrdist;
010800140114          yk531ddpI.nbn = nrdispo;
010900140114          yk531ddpr(*BLANK : rpyEsito : 'YK531DDPI'
011000140114           : yk531ddpI : %SIZE(yk531ddpI) : 'YK531DDPO'
011100140114           : yk531ddpO : %SIZE(yk531ddpO) );
011200140114
011300140114          dispodes = yk531ddpo;
011400140114
011500140114            IF        rpyEsito < *ZERO;
011600140114            ENDIF;
011700140114
011800140114         ENDSR;
011900140114
012000140114      /end-free
012100140114

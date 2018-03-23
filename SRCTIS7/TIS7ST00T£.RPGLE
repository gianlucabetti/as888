000100120223     ***************************************************************************
000200120223     **
000300120302     ** Tester di TIS7ST00R.
000400120223     **
000500120223     ***************************************************************************
000600120224     H DFTACTGRP(*NO) ACTGRP(*NEW)
000700120305     Ftis7st00d CF   E             WORKSTN
000800120223
000900120223     ***************************************************************************
001000120223     **
001100120223     ** Costanti.
001200120223     **
001300120223     ***************************************************************************
001400120223     D ESITO_ERROR...
001500120223     D                 C                   -1
001600120223     D ESITO_OK...
001700120223     D                 C                   0
001800120223     D RPYOPCODE_DONE...
001900120223     D                 C                   'DONE'
002000120223     D RPYOPCODE_ERROR...
002100120223     D                 C                   'ERROR'
002200120305     D RQSOPCODE_GETTAM...
002300120305     D                 C                   'GETTAM'
002400120223
002500120223     ***************************************************************************
002600120223     **
002700120223     ** Strutture dati.
002800120223     **
002900120223     ***************************************************************************
003000120302     D tis7st00i     E DS                  INZ(*EXTDFT) QUALIFIED
003100120302     D tis7st00o     E DS                  INZ(*EXTDFT) QUALIFIED
003200120306     D tis7st01i     E DS                  INZ(*EXTDFT) QUALIFIED
003300120306     D tis7st01o     E DS                  INZ(*EXTDFT) QUALIFIED
003400120223
003500120223     ***************************************************************************
003600120223     **
003700120223     ** Campi.
003800120223     **
003900120223     ***************************************************************************
004000120223     D rqsOpCode       S             10A
004100120223     D rqsData         S           9999A
004200120223     D rqsDataLen      S              4P 0
004300120223     D rpyOpCode       S             10A
004400120223     D rpyData         S           9999A
004500120223     D rpyDataLen      S              4P 0
004600120223
004700120223     ***************************************************************************
004800120223     **
004900120223     ** Prototipi.
005000120223     **
005100120223     ***************************************************************************
005200120302     D Tis7st00r       PR                  EXTPGM('TIS7ST00R')
005300120224     D  rqsOpCode                    10A
005400120224     D  rqsData                    9999A
005500120224     D  rqsDataLen                    4P 0
005600120224     D  rpyOpCode                    10A
005700120224     D  rpyData                    9999A
005800120224     D  rpyDataLen                    4P 0
005900120223
006000120223     ***************************************************************************
006100120223     **
006200120223     ** Main.
006300120223     **
006400120223     ***************************************************************************
006500120223
006600120223      /FREE
006700120224
006800120224       *INLR = *ON;
006900120224
007000120306 1     dow  not *inkc   ;
007100120306
007200120306       exfmt  s7st00d  ;
007300120306
007400120306 2     if not *inkc   ;
007500120305
007600120305       rqsOpCode = RQSOPCODE_GETTAM + code  ;
007700120224
007800120302       RESET tis7st00i;
007900120306       RESET tis7st01i;
008000120306
008100120306       if code='LIST'   ;
008200120305       tis7st00i.ksu = '0'+%editc(cliente:'X')  ;
008300120305       tis7st00i.idcliente = cliente;
008400120305       tis7st00i.dtspediz  = %subst(spediz:1:2)+'.' +
008500120305                             %subst(spediz:3:2)+'.' +
008600120305                             %subst(spediz:5:4) ;
008700120302       tis7st00i.formato   = 'TIS7ST00I'  ;
008800120305       tis7st00i.LINISo2   = lingua      ;
008900120224
009000120224
009100120302         rqsData = tis7st00i;
009200120302         rqsDataLen = %SIZE(tis7st00i);
009300120306       else ;
009400120306
009500120306       tis7st01i.ksu = '0'+%editc(cliente:'X')  ;
009600120306       tis7st01i.idcliente = cliente;
009700120306       tis7st01i.dtspediz  = %subst(spediz:1:2)+'.' +
009800120306                             %subst(spediz:3:2)+'.' +
009900120306                             %subst(spediz:5:4) ;
010000120306       tis7st01i.idcodtar  = codtar ;
010100120306       tis7st01i.tipocli   = tipocli;
010200120306       tis7st01i.formato   = 'TIS7ST01I'  ;
010300120306       tis7st01i.LINISo2   = lingua      ;
010400120306
010500120306
010600120306         rqsData = tis7st01i;
010700120306         rqsDataLen = %SIZE(tis7st01i);
010800120306       endif;
010900120224
011000120306 3       if   *inkf    ;
011100120302         Tis7st00r( rqsOpCode : rqsData : rqsDataLen
011200120224                  : rpyOpCode : rpyData : rpyDataLen
011300120224                  );
011400120224
011500120306 4       IF rpyOpCode = RPYOPCODE_ERROR;
011600120224           DUMP(A);
011700120306           output=rpydata   ;
011800120306 4       else ;
011900120224
012000120306 5     if code='LIST'   ;
012100120302         tis7st00o = rpyData;
012200120306
012300120306         // visulizzo i primi 10
012400120306
012500120306         output='NumEle '+%editc(tis7st00o.nrelementi:'X')+ '  codTar ' +
012600120306         %subst(tis7st00o.skitariffa:1:30) +'  Descriz '+
012700120306         %subst(tis7st00o.skidescriz:1:150)+
012800120306         '  Ntw '+%subst(tis7st00o.skinetwork:1:120) + '  TipoSer ' +
012900120306         %subst(tis7st00o.skitiposer:1:150) ;
013000120306
013100120306       else   ;
013200120306         tis7st01o = rpyData;
013300120306
013400120306         output='DescTariffa '+ tis7st01o.iddescriz +'  Network ' +
013500120306         tis7st01o.idnetwork+'-'+tis7st01o.iddescrntw+
013600120306          '  Visual.quantità Fatt '+
013700120306         tis7st01o.visualqta+'  Visual.Nazione Mitt '+tis7st01o.visualnzm+
013800120306         '  Visual.NazioneDest '+tis7st01o.visualnzd  ;
013900120306 5     endif  ;
014000120306 4     endif  ;
014100120306
014200120306       *in01=*on    ;
014300120306       exfmt  s7st00d  ;
014400120306
014500120306        clear output   ;
014600120306        clear rpyOpCode  ;
014700120306       *in01=*off ;
014800120306
014900120306 3         endif  ;
015000120306 2         endif  ;
015100120306
015200120306 1        enddo  ;
015300120224
015400120224      /END-FREE

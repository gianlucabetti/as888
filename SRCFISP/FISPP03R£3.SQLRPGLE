000100080603     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700080630     Ftabel00f  iF   E           K DISK
000800080630     D codicepc        S              1
000900080630     D filiale         S              3
001000080702     D inizio          S              1
001100080703     D catfoglio       S              1
001200080703     D numfoglio       S              7
001300080626     D Ticket          S             17
001400080702     D RTNrcdcnt       S             10I 0
001500061017     D RTNesito        S             10I 0
001600061017     D RTNopcode       S             10
001700061017     D RTNstatus       S             10I 0
001800080702
001900080630     d dataiso         s               d   datfmt(*iso)
002000080630     d kTnp            S                   LIKE(SGNTNP)
002100080630     d codut           S                   LIKE(tblkut) inz(1)
002200080630     d cod             S                   LIKE(tblcod) inz('5A')
002300080630     d key             S                   LIKE(tblkey) inz('2 ')
002400080630     d wdpprt          S              8  0
002401151130     d wdpprt7         S              8  0
002500080630     d datapul         S              8  0
002600080630     d wtime           s             17
002700080630
002800080630     d fisgnds       e ds                  extname(fisgn00f)
002900080630
003000140403     D xgiolavds     E DS
003001140403     D DS5A2         E DS
003100080630     D fnlv55ds      E Ds
003200080702      *   C O S T A N T I
003300080702     d digitn          c                   const('1234567890')
003400060918
003500060918      //********************************************************************************************
003600060918      //
003700060918      // Definizione parametri procedura.
003800060918      //
003900060918      //********************************************************************************************
004000060918     C     *ENTRY        PLIST
004100080626     C                   PARM                    codicepc
004200080626     C                   PARM                    filiale
004300080626     C                   PARM                    inizio
004400080702     c                   parm                    catfoglio
004500080702     c                   parm                    numfoglio
004600080701     C                   PARM                    ticket
004700080702     C                   PARM                    rtnrcdcnt
004800061017     C                   PARM                    rtnesito
004900061017     C                   PARM                    rtnopcode
005000061017     C                   PARM                    rtnstatus
005100080702
005200080702     c                   eval      *inlr=*on
005300080630      *
005400080702     c                   IF        Filiale = *blanks or
005500080702     c                             %check(digitn:filiale)>0
005600080702     c                   eval      RTNstatus = 99
005700080702     c                   eval      RTNopcode = 'ERRORE '
005800080702     c                   eval      RTNesito  = -1
005900080702     c                   return
006000080702     c                   ENDIF
006100080702
006200080630     c                   exsr      operaz_ini
006300080603
006400060918      /FREE
006500060918
006600060918       //*******************************************************************************************
006700060918       //
006800060918       // Main.
006900060918       //
007000060918       //*******************************************************************************************
007100061017       CLEAR rtnesito;
007200061017       CLEAR rtnopcode;
007300061017       CLEAR rtnstatus;
007400080702       CLEAR rtnrcdcnt;
007500080626       // imposto la data e ora del lavoro
007600080626       wtime = %char(%timestamp:*iso0);
007700080626
007800080618       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
007900080603
008000080618      /END-FREE
008100080618
008200080630     c                   if        inizio=' '
008300080618     C/EXEC SQL
008400080702     C+ DECLARE FISGN INSENSITIVE CURSOR FOR
008500131029     C+ SELECT
008507131029     c+ case
008508131029     c+ when sgnatb <> ' ' then 'A'
008509131029     c+ else ' '
008510131029     c+ end  as tr6atb,
008511131029     c+ sgnwho as
008600080630     C+ TR6WHO, SGNSEA as TR6SEA, SGNST1 as TR6ST1, SGNST2 as TR6ST2,
008700080630     C+ SGNSQU as TR6SQU, SGNFL1 as TR6FL1, SGNFL2 as TR6FL2, SGNFL3 as
008800080630     C+ TR6FL3, SGNFL4 as TR6FL4, SGNFLX as TR6FLX, SGNKSC as TR6KSC,
008900080630     C+ SGNFLS as TR6FLS, SGNDLP as TR6DLP, SGNCDP as TR6CDP, SGNNRS as
009000080630     C+ TR6NRS, SGNNCD as TR6NCD, SGNLNA as TR6LNA, SGNDLA as TR6DLA,
009100080630     C+ SGNTNA as TR6TNA, SGNTSP as TR6TSP, SGNZSC as TR6ZSC, SGNNC1 as
009200080630     C+ TR6NC1, SGNNC2 as TR6NC2, SGNRSM as TR6RSM, SGNPRM as TR6PRM,
009300080630     C+ SGNRSD as TR6RSD, SGNCAD as TR6CAD, SGNIND as TR6IND, SGNLOD as
009400080630     C+ TR6LOD, SGNPRD as TR6PRD, SGNCPD as TR6CPD, SGNNCP as TR6NCP,
009500080630     C+ SGNNCU as TR6NCU, SGNRST as TR6RST, SGNVOL as TR6VOL, SGNPKG as
009600080630     C+ TR6PKG, SGNFVR as TR6FVR, SGNAAS as TR6AAS, SGNMGS as TR6MGS,
009700080630     C+ SGNTST as TR6TST, SGNTSE as TR6TSE, SGNSSE as TR6SSE, SGNSIP as
009800080630     C+ TR6SIP, SGNSIA as TR6SIA, SGNAWB as TR6AWB, SGNISA as TR6ISA,
009900080630     C+ SGNFCA as TR6FCA, SGNEUR as TR6EUR, SGNXX1 as TR6XX1, SGNXX2 as
010000080630     C+ TR6XX2, '' as TR6BAI, 0 as TR6POC, 0 as TR6NPG, 0 as TR6NFV, 0 as
010100080630     C+ TR6DFV, '' as TR6FPP, 0 as TR6PDC, '' as TR6DPD, '' as TR6DS1, ''
010200131029     C+ as TR6DS2, '' as TR6BAU
010204131029     C+ FROM FISGN00F WHERE SGNTNP=:ktnp and
010300080630     C+ SGNDATORA>=:ticket
010400080618     C/END-EXEC
010500080630     C/EXEC SQL
010600080630     C+ OPEN FISGN
010700080630     C/END-EXEC
010800080630     c                   else
010801151130     c* ---------------------------------------------------------------
010802151130     c* Pulizia record di fisgn di bolle "normali" NON chiuse in part.x
010803151130     c* Merce Mai Affidata:
010804151130     c* ---------------------------------------------------------------
010900080630     c* pulisco record di fisgn dei giorni precedenti e già stampati
011000080630     c* pulisco record di fisgn per i quali sono trascorsi i giorni di puli-
011100080630     c* zia
011200080630     C/EXEC SQL
011300151130     C+ DELETE FROM FISGN00F WHERE sgntnp=:ktnp and
011301151130     c+ (sgnaas, sgnlnp, sgnnrs, sgnnsp) not in ( select blpaas, blplnp,
011302151130     c+  blpnrs, blpnsp from fnblp01l where blpcca='7' and
011303151130     C+  blpft1='N') and ((sgnst2>0 and
011400080703     C+ sgnatb=' ' and sgnaas*10000+sgnmgs between 1 and (:wdpprt-1)) or
011500080703     C+ (sgnaas*10000+sgnmgs<:datapul))
011600080630     C/END-EXEC
011601151130     c* ---------------------------------------------------------------
011602151130     c* Pulizia record di fisgn di bolle chiuse in part.x Merce Mai Affidata:
011603151130     c* ---------------------------------------------------------------
011604151130     c* pulisco record di fisgn per i quali sono trascorsi i giorni di pulizia
011606151130     C/EXEC SQL
011607151130     C+ DELETE FROM FISGN00F WHERE sgntnp=:ktnp and
011608151130     c+ (sgnaas, sgnlnp, sgnnrs, sgnnsp) in ( select blpaas, blplnp,
011609151130     c+  blpnrs, blpnsp from fnblp01l where blpcca='7' and
011610151130     C+  blpft1='N') and (sgnaas*10000+sgnmgs<:wdpprt7)
011613151130     C/END-EXEC
011700080630     C/EXEC SQL
011800080702     C+ DECLARE FISGNI INSENSITIVE CURSOR FOR
011900131029     C+ SELECT
011901131029     c+ case
011902131029     c+ when sgnatb <> ' ' then 'A'
011903131029     c+ else ' '
011904131029     c+ end  as tr6atb,
011905131029     C+ SGNWHO as
012000080630     C+ TR6WHO, SGNSEA as TR6SEA, SGNST1 as TR6ST1, SGNST2 as TR6ST2,
012100080630     C+ SGNSQU as TR6SQU, SGNFL1 as TR6FL1, SGNFL2 as TR6FL2, SGNFL3 as
012200080630     C+ TR6FL3, SGNFL4 as TR6FL4, SGNFLX as TR6FLX, SGNKSC as TR6KSC,
012300080630     C+ SGNFLS as TR6FLS, SGNDLP as TR6DLP, SGNCDP as TR6CDP, SGNNRS as
012400080630     C+ TR6NRS, SGNNCD as TR6NCD, SGNLNA as TR6LNA, SGNDLA as TR6DLA,
012500080630     C+ SGNTNA as TR6TNA, SGNTSP as TR6TSP, SGNZSC as TR6ZSC, SGNNC1 as
012600080630     C+ TR6NC1, SGNNC2 as TR6NC2, SGNRSM as TR6RSM, SGNPRM as TR6PRM,
012700080630     C+ SGNRSD as TR6RSD, SGNCAD as TR6CAD, SGNIND as TR6IND, SGNLOD as
012800080630     C+ TR6LOD, SGNPRD as TR6PRD, SGNCPD as TR6CPD, SGNNCP as TR6NCP,
012900080630     C+ SGNNCU as TR6NCU, SGNRST as TR6RST, SGNVOL as TR6VOL, SGNPKG as
013000080630     C+ TR6PKG, SGNFVR as TR6FVR, SGNAAS as TR6AAS, SGNMGS as TR6MGS,
013100080630     C+ SGNTST as TR6TST, SGNTSE as TR6TSE, SGNSSE as TR6SSE, SGNSIP as
013200080630     C+ TR6SIP, SGNSIA as TR6SIA, SGNAWB as TR6AWB, SGNISA as TR6ISA,
013300080630     C+ SGNFCA as TR6FCA, SGNEUR as TR6EUR, SGNXX1 as TR6XX1, SGNXX2 as
013400080630     C+ TR6XX2, '' as TR6BAI, 0 as TR6POC, 0 as TR6NPG, 0 as TR6NFV, 0 as
013500080630     C+ TR6DFV, '' as TR6FPP, 0 as TR6PDC, '' as TR6DPD, '' as TR6DS1, ''
013600080630     C+ as TR6DS2, '' as TR6BAU FROM FISGN00F WHERE sgntnp=:ktnp
013700080630     C/END-EXEC
013800080630     C/EXEC SQL
013900080630     C+ OPEN FISGNI
014000080630     C/END-EXEC
014100080630     c                   endif
014200080618
014300080618      /FREE
014400080618
014500061017       select;
014600061017       When sqlCode = 100;
014700061017         RTNstatus = sqlCode;
014800061017         RTNopcode = 'SQLCODE';
014900061017         RTNesito  = -1;
015000061017         RETURN;
015100061017       When sqlCode < 0;
015200061017         RTNstatus = sqlCode;
015300061017         RTNopcode = 'SQLCODE';
015400061017         RTNesito  = -1;
015500060918         RETURN;
015600061017       Endsl;
015700080702       RTNrcdcnt = sqlErrD(2);
015800080618
015900080618      /END-FREE
016000060918
016100080630     c                   if        inizio=' '
016200080618     C/EXEC SQL
016300080618     C+ SET RESULT SETS CURSOR FISGN
016400080618     C/END-EXEC
016500080630     c                   else
016600080630     C/EXEC SQL
016700080630     C+ SET RESULT SETS CURSOR FISGNI
016800080630     C/END-EXEC
016900080630     c                   endif
017000061017
017100080618      /FREE
017200061016       IF sqlCode < 0;
017300061017         RTNstatus = sqlCode;
017400061017         RTNopcode = 'SQLCODE';
017500061017         RTNesito  = -1;
017600060918         RETURN;
017700060918       ENDIF;
017800060918
017900080626       ticket = wtime;
018000060918       RETURN;
018100060918
018200060918      /END-FREE
018300080630      *---------------------------------------------------------*
018400080630      * Operazioni iniziali                                     *
018500080630      *---------------------------------------------------------*
018600080630     c     OPERAZ_INI    BEGSR
018700080630     c                   time                    dataiso
018800080630     c*
018900080630     c                   move      dataiso       wdpprt
018901151130     c                   move      dataiso       wdpprt7
019000080630
019100080630      * Ricavo il Terminal di Partenza
019200080630     C                   clear                   fnlv55ds
019300080630     C                   eval      d55tpt = 'P'
019400080630     C                   movel     filiale       D55LIN
019500080630     c     *iso          movel     dataiso       D55DRF
019600080630     C                   call      'FNLV55R'
019700080630     C                   parm                    fnlv55ds
019800080630      *
019900080630     c                   IF        d55err = *blanks
020000080630     C                   movel     D55TFP        ktnp
020100080630     C                   ENDIF
020200080630
020300080630     c     ktab          chain     tabel
020400080630     c                   IF        %FOUND
020500080630     c                   movel     tbluni        ds5a2
020600080630     c                   ELSE
020700080630     c                   z-add     16            §5asgn
020701140403     c                   z-add     2             §5asgns
020702151130     c                   z-add     10            §5asgn7
020800080630     c                   ENDIF
020900080630      *
021000080630     c     dataiso       subdur    §5asgn:*d     dataiso
021100080630     c                   move      dataiso       datapul
021101140403
021102140403     c*  Sottaggo eventuali giorni lavorativi dalla data pulizia record già stampati
021103140403     c                   if        §5asgns>0
021104140403     c                   clear                   xgiolavds
021105140403     c                   eval      ixgldata=wdpprt
021106140403     c                   eval      ixglfil =ktnp
021107140403     c                   eval      ixglpa  ='P'
021108140403     c                   eval      ixglsub ='S'
021109140403     c                   eval      ixglgg  =§5asgns
021110140403     c                   eval      ixgllav ='S'
021111140403     c                   call      'XGIOLAV'
021112140403     c                   parm                    xgiolavds
021113140403     c                   Move      oxgldata      wdpprt
021114140403     c                   endif
021115151130     c*  Sottaggo eventuali giorni lavorativi dalla data pulizia record chiusi per merce mai aff.
021117151130     c                   clear                   xgiolavds
021118151130     c                   eval      ixgldata=wdpprt7
021119151130     c                   eval      ixglfil =ktnp
021120151130     c                   eval      ixglpa  ='P'
021121151130     c                   eval      ixglsub ='S'
021122151130     c                   if        §5asgn7>0
021123151130     c                   eval      ixglgg  =§5asgn7
021124151130     c                   else
021125151130     c                   eval      ixglgg  =10
021126151130     c                   endif
021127151130     c                   eval      ixgllav ='S'
021128151130     c                   call      'XGIOLAV'
021129151130     c                   parm                    xgiolavds
021130151130     c                   Move      oxgldata      wdpprt7
021200080630
021300080630     c     ktab          klist
021400080630     c                   kfld                    codut
021500080630     c                   kfld                    cod
021600080630     c                   kfld                    key
021700080630     c                   endsr

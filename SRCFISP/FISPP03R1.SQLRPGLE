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
002500151130     d wdpprt7         S              8  0
002600080630     d datapul         S              8  0
002700080630     d wtime           s             17
002800080630
002900080630     d fisgnds       e ds                  extname(fisgn00f)
003000080630
003100140403     D xgiolavds     E DS
003200140403     D DS5A2         E DS
003300080630     D fnlv55ds      E Ds
003400080702      *   C O S T A N T I
003500080702     d digitn          c                   const('1234567890')
003600060918
003700060918      //********************************************************************************************
003800060918      //
003900060918      // Definizione parametri procedura.
004000060918      //
004100060918      //********************************************************************************************
004200170307     C*
004300170307     C* Forzatura query-time-limit
004400170307     C                   call(e)   'FISPP03C1'
004500170307     C*
004600170307     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004700170307     C
004800170307     C/EXEC SQL
004900170307     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD,
005000170307     C+            COMMIT = *NONE, DLYPRP = *YES
005100170307     C/END-EXEC
005200170307     C*
005300060918     C     *ENTRY        PLIST
005400080626     C                   PARM                    codicepc
005500080626     C                   PARM                    filiale
005600080626     C                   PARM                    inizio
005700080702     c                   parm                    catfoglio
005800080702     c                   parm                    numfoglio
005900080701     C                   PARM                    ticket
006000080702     C                   PARM                    rtnrcdcnt
006100061017     C                   PARM                    rtnesito
006200061017     C                   PARM                    rtnopcode
006300061017     C                   PARM                    rtnstatus
006400080702
006500080702     c                   eval      *inlr=*on
006600080630      *
006700080702     c                   IF        Filiale = *blanks or
006800080702     c                             %check(digitn:filiale)>0
006900080702     c                   eval      RTNstatus = 99
007000080702     c                   eval      RTNopcode = 'ERRORE '
007100080702     c                   eval      RTNesito  = -1
007200080702     c                   return
007300080702     c                   ENDIF
007400080702
007500080630     c                   exsr      operaz_ini
007600080603
007700060918      /FREE
007800060918
007900060918       //*******************************************************************************************
008000060918       //
008100060918       // Main.
008200060918       //
008300060918       //*******************************************************************************************
008400061017       CLEAR rtnesito;
008500061017       CLEAR rtnopcode;
008600061017       CLEAR rtnstatus;
008700080702       CLEAR rtnrcdcnt;
008800080626       // imposto la data e ora del lavoro
008900080626       wtime = %char(%timestamp:*iso0);
009000080626
009100080618       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009200080603
009300080618      /END-FREE
009400080618
009500080630     c                   if        inizio=' '
009600080618     C/EXEC SQL
009700080702     C+ DECLARE FISGN INSENSITIVE CURSOR FOR
009800131029     C+ SELECT
009900131029     c+ case
010000131029     c+ when sgnatb <> ' ' then 'A'
010100131029     c+ else ' '
010200131029     c+ end  as tr6atb,
010300131029     c+ sgnwho as
010400080630     C+ TR6WHO, SGNSEA as TR6SEA, SGNST1 as TR6ST1, SGNST2 as TR6ST2,
010500080630     C+ SGNSQU as TR6SQU, SGNFL1 as TR6FL1, SGNFL2 as TR6FL2, SGNFL3 as
010600080630     C+ TR6FL3, SGNFL4 as TR6FL4, SGNFLX as TR6FLX, SGNKSC as TR6KSC,
010700080630     C+ SGNFLS as TR6FLS, SGNDLP as TR6DLP, SGNCDP as TR6CDP, SGNNRS as
010800080630     C+ TR6NRS, SGNNCD as TR6NCD, SGNLNA as TR6LNA, SGNDLA as TR6DLA,
010900080630     C+ SGNTNA as TR6TNA, SGNTSP as TR6TSP, SGNZSC as TR6ZSC, SGNNC1 as
011000080630     C+ TR6NC1, SGNNC2 as TR6NC2, SGNRSM as TR6RSM, SGNPRM as TR6PRM,
011100080630     C+ SGNRSD as TR6RSD, SGNCAD as TR6CAD, SGNIND as TR6IND, SGNLOD as
011200080630     C+ TR6LOD, SGNPRD as TR6PRD, SGNCPD as TR6CPD, SGNNCP as TR6NCP,
011300080630     C+ SGNNCU as TR6NCU, SGNRST as TR6RST, SGNVOL as TR6VOL, SGNPKG as
011400080630     C+ TR6PKG, SGNFVR as TR6FVR, SGNAAS as TR6AAS, SGNMGS as TR6MGS,
011500080630     C+ SGNTST as TR6TST, SGNTSE as TR6TSE, SGNSSE as TR6SSE, SGNSIP as
011600080630     C+ TR6SIP, SGNSIA as TR6SIA, SGNAWB as TR6AWB, SGNISA as TR6ISA,
011700080630     C+ SGNFCA as TR6FCA, SGNEUR as TR6EUR, SGNXX1 as TR6XX1, SGNXX2 as
011800080630     C+ TR6XX2, '' as TR6BAI, 0 as TR6POC, 0 as TR6NPG, 0 as TR6NFV, 0 as
011900080630     C+ TR6DFV, '' as TR6FPP, 0 as TR6PDC, '' as TR6DPD, '' as TR6DS1, ''
012000131029     C+ as TR6DS2, '' as TR6BAU
012100131029     C+ FROM FISGN00F WHERE SGNTNP=:ktnp and
012200080630     C+ SGNDATORA>=:ticket
012300080618     C/END-EXEC
012400080630     C/EXEC SQL
012500080630     C+ OPEN FISGN
012600080630     C/END-EXEC
012700080630     c                   else
012800151130     c* ---------------------------------------------------------------
012900151130     c* Pulizia record di fisgn di bolle "normali" NON chiuse in part.x
013000151130     c* Merce Mai Affidata:
013100151130     c* ---------------------------------------------------------------
013200080630     c* pulisco record di fisgn dei giorni precedenti e già stampati
013300080630     c* pulisco record di fisgn per i quali sono trascorsi i giorni di puli-
013400080630     c* zia
013500080630     C/EXEC SQL
013600151130     C+ DELETE FROM FISGN00F WHERE sgntnp=:ktnp and
013700170125     C+  sgnatb=' ' and  sgnst2>0 AND
013800170125     C+  sgnaas*10000+sgnmgs < (:wdpprt) and
013900151130     c+ (sgnaas, sgnlnp, sgnnrs, sgnnsp) not in ( select blpaas, blplnp,
014000170117     c+  blpnrs, blpnsp from fnblp00f where blpcca='7' and
014100170125     C+  blpft1='N')
014200080630     C/END-EXEC
014300170125     C/EXEC SQL
014400170125     C+ DELETE FROM FISGN00F WHERE sgntnp=:ktnp and
014500170125     C+  sgnaas*10000+sgnmgs<:datapul  AND
014600170125     c+ (sgnaas, sgnlnp, sgnnrs, sgnnsp) not in ( select blpaas, blplnp,
014700170125     c+  blpnrs, blpnsp from fnblp00f where blpcca='7' and
014800170125     C+  blpft1='N')
014900170125     C/END-EXEC
015000151130     c* ---------------------------------------------------------------
015100151130     c* Pulizia record di fisgn di bolle chiuse in part.x Merce Mai Affidata:
015200151130     c* ---------------------------------------------------------------
015300151130     c* pulisco record di fisgn per i quali sono trascorsi i giorni di pulizia
015400151130     C/EXEC SQL
015500151130     C+ DELETE FROM FISGN00F WHERE sgntnp=:ktnp and
015600170117     C+ (sgnaas*10000+sgnmgs<:wdpprt7) and
015700151130     c+ (sgnaas, sgnlnp, sgnnrs, sgnnsp) in ( select blpaas, blplnp,
015800170117     c+  blpnrs, blpnsp from fnblp00f where blpcca='7' and
015900151130     C+  blpft1='N') and (sgnaas*10000+sgnmgs<:wdpprt7)
016000151130     C/END-EXEC
016100080630     C/EXEC SQL
016200080702     C+ DECLARE FISGNI INSENSITIVE CURSOR FOR
016300131029     C+ SELECT
016400131029     c+ case
016500131029     c+ when sgnatb <> ' ' then 'A'
016600131029     c+ else ' '
016700131029     c+ end  as tr6atb,
016800131029     C+ SGNWHO as
016900080630     C+ TR6WHO, SGNSEA as TR6SEA, SGNST1 as TR6ST1, SGNST2 as TR6ST2,
017000080630     C+ SGNSQU as TR6SQU, SGNFL1 as TR6FL1, SGNFL2 as TR6FL2, SGNFL3 as
017100080630     C+ TR6FL3, SGNFL4 as TR6FL4, SGNFLX as TR6FLX, SGNKSC as TR6KSC,
017200080630     C+ SGNFLS as TR6FLS, SGNDLP as TR6DLP, SGNCDP as TR6CDP, SGNNRS as
017300080630     C+ TR6NRS, SGNNCD as TR6NCD, SGNLNA as TR6LNA, SGNDLA as TR6DLA,
017400080630     C+ SGNTNA as TR6TNA, SGNTSP as TR6TSP, SGNZSC as TR6ZSC, SGNNC1 as
017500080630     C+ TR6NC1, SGNNC2 as TR6NC2, SGNRSM as TR6RSM, SGNPRM as TR6PRM,
017600080630     C+ SGNRSD as TR6RSD, SGNCAD as TR6CAD, SGNIND as TR6IND, SGNLOD as
017700080630     C+ TR6LOD, SGNPRD as TR6PRD, SGNCPD as TR6CPD, SGNNCP as TR6NCP,
017800080630     C+ SGNNCU as TR6NCU, SGNRST as TR6RST, SGNVOL as TR6VOL, SGNPKG as
017900080630     C+ TR6PKG, SGNFVR as TR6FVR, SGNAAS as TR6AAS, SGNMGS as TR6MGS,
018000080630     C+ SGNTST as TR6TST, SGNTSE as TR6TSE, SGNSSE as TR6SSE, SGNSIP as
018100080630     C+ TR6SIP, SGNSIA as TR6SIA, SGNAWB as TR6AWB, SGNISA as TR6ISA,
018200080630     C+ SGNFCA as TR6FCA, SGNEUR as TR6EUR, SGNXX1 as TR6XX1, SGNXX2 as
018300080630     C+ TR6XX2, '' as TR6BAI, 0 as TR6POC, 0 as TR6NPG, 0 as TR6NFV, 0 as
018400080630     C+ TR6DFV, '' as TR6FPP, 0 as TR6PDC, '' as TR6DPD, '' as TR6DS1, ''
018500080630     C+ as TR6DS2, '' as TR6BAU FROM FISGN00F WHERE sgntnp=:ktnp
018600080630     C/END-EXEC
018700080630     C/EXEC SQL
018800080630     C+ OPEN FISGNI
018900080630     C/END-EXEC
019000080630     c                   endif
019100080618
019200080618      /FREE
019300080618
019400061017       select;
019500061017       When sqlCode = 100;
019600061017         RTNstatus = sqlCode;
019700061017         RTNopcode = 'SQLCODE';
019800061017         RTNesito  = -1;
019900061017         RETURN;
020000061017       When sqlCode < 0;
020100061017         RTNstatus = sqlCode;
020200061017         RTNopcode = 'SQLCODE';
020300061017         RTNesito  = -1;
020400060918         RETURN;
020500061017       Endsl;
020600080702       RTNrcdcnt = sqlErrD(2);
020700080618
020800080618      /END-FREE
020900060918
021000080630     c                   if        inizio=' '
021100080618     C/EXEC SQL
021200170124     C+ SET RESULT SETS for return to client CURSOR FISGN
021300080618     C/END-EXEC
021400080630     c                   else
021500080630     C/EXEC SQL
021600170124     C+ SET RESULT SETS for return to client CURSOR FISGNI
021700080630     C/END-EXEC
021800080630     c                   endif
021900061017
022000080618      /FREE
022100061016       IF sqlCode < 0;
022200061017         RTNstatus = sqlCode;
022300061017         RTNopcode = 'SQLCODE';
022400061017         RTNesito  = -1;
022500060918         RETURN;
022600060918       ENDIF;
022700060918
022800080626       ticket = wtime;
022900060918       RETURN;
023000060918
023100060918      /END-FREE
023200080630      *---------------------------------------------------------*
023300080630      * Operazioni iniziali                                     *
023400080630      *---------------------------------------------------------*
023500080630     c     OPERAZ_INI    BEGSR
023600080630     c                   time                    dataiso
023700080630     c*
023800080630     c                   move      dataiso       wdpprt
023900151130     c                   move      dataiso       wdpprt7
024000080630
024100080630      * Ricavo il Terminal di Partenza
024200080630     C                   clear                   fnlv55ds
024300080630     C                   eval      d55tpt = 'P'
024400080630     C                   movel     filiale       D55LIN
024500080630     c     *iso          movel     dataiso       D55DRF
024600080630     C                   call      'FNLV55R'
024700080630     C                   parm                    fnlv55ds
024800080630      *
024900080630     c                   IF        d55err = *blanks
025000080630     C                   movel     D55TFP        ktnp
025100080630     C                   ENDIF
025200080630
025300080630     c     ktab          chain     tabel
025400080630     c                   IF        %FOUND
025500080630     c                   movel     tbluni        ds5a2
025600080630     c                   ELSE
025700080630     c                   z-add     16            §5asgn
025800140403     c                   z-add     2             §5asgns
025900151130     c                   z-add     10            §5asgn7
026000080630     c                   ENDIF
026100080630      *
026200080630     c     dataiso       subdur    §5asgn:*d     dataiso
026300080630     c                   move      dataiso       datapul
026400140403
026500140403     c*  Sottaggo eventuali giorni lavorativi dalla data pulizia record già stampati
026600140403     c                   if        §5asgns>0
026700140403     c                   clear                   xgiolavds
026800140403     c                   eval      ixgldata=wdpprt
026900140403     c                   eval      ixglfil =ktnp
027000140403     c                   eval      ixglpa  ='P'
027100140403     c                   eval      ixglsub ='S'
027200140403     c                   eval      ixglgg  =§5asgns
027300140403     c                   eval      ixgllav ='S'
027400140403     c                   call      'XGIOLAV'
027500140403     c                   parm                    xgiolavds
027600140403     c                   Move      oxgldata      wdpprt
027700140403     c                   endif
027800151130     c*  Sottaggo eventuali giorni lavorativi dalla data pulizia record chiusi per merce mai aff.
027900151130     c                   clear                   xgiolavds
028000151130     c                   eval      ixgldata=wdpprt7
028100151130     c                   eval      ixglfil =ktnp
028200151130     c                   eval      ixglpa  ='P'
028300151130     c                   eval      ixglsub ='S'
028400151130     c                   if        §5asgn7>0
028500151130     c                   eval      ixglgg  =§5asgn7
028600151130     c                   else
028700151130     c                   eval      ixglgg  =10
028800151130     c                   endif
028900151130     c                   eval      ixgllav ='S'
029000151130     c                   call      'XGIOLAV'
029100151130     c                   parm                    xgiolavds
029200151130     c                   Move      oxgldata      wdpprt7
029300080630
029400080630     c     ktab          klist
029500080630     c                   kfld                    codut
029600080630     c                   kfld                    cod
029700080630     c                   kfld                    key
029800080630     c                   endsr

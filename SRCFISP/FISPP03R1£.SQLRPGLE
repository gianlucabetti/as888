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
004300170307     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004400170307     C
004500170307     C/EXEC SQL
004600170307     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD,
004700170307     C+            COMMIT = *NONE, DLYPRP = *YES
004800170307     C/END-EXEC
004900170307     C*
005000060918     C     *ENTRY        PLIST
005100080626     C                   PARM                    codicepc
005200080626     C                   PARM                    filiale
005300080626     C                   PARM                    inizio
005400080702     c                   parm                    catfoglio
005500080702     c                   parm                    numfoglio
005600080701     C                   PARM                    ticket
005700080702     C                   PARM                    rtnrcdcnt
005800061017     C                   PARM                    rtnesito
005900061017     C                   PARM                    rtnopcode
006000061017     C                   PARM                    rtnstatus
006100080702
006200080702     c                   eval      *inlr=*on
006300080630      *
006400080702     c                   IF        Filiale = *blanks or
006500080702     c                             %check(digitn:filiale)>0
006600080702     c                   eval      RTNstatus = 99
006700080702     c                   eval      RTNopcode = 'ERRORE '
006800080702     c                   eval      RTNesito  = -1
006900080702     c                   return
007000080702     c                   ENDIF
007100080702
007200080630     c                   exsr      operaz_ini
007300080603
007400060918      /FREE
007500060918
007600060918       //*******************************************************************************************
007700060918       //
007800060918       // Main.
007900060918       //
008000060918       //*******************************************************************************************
008100061017       CLEAR rtnesito;
008200061017       CLEAR rtnopcode;
008300061017       CLEAR rtnstatus;
008400080702       CLEAR rtnrcdcnt;
008500080626       // imposto la data e ora del lavoro
008600080626       wtime = %char(%timestamp:*iso0);
008700080626
008800080618       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
008900080603
009000080618      /END-FREE
009100080618
009200080630     c                   if        inizio=' '
009300080618     C/EXEC SQL
009400080702     C+ DECLARE FISGN INSENSITIVE CURSOR FOR
009500131029     C+ SELECT
009600131029     c+ case
009700131029     c+ when sgnatb <> ' ' then 'A'
009800131029     c+ else ' '
009900131029     c+ end  as tr6atb,
010000131029     c+ sgnwho as
010100080630     C+ TR6WHO, SGNSEA as TR6SEA, SGNST1 as TR6ST1, SGNST2 as TR6ST2,
010200080630     C+ SGNSQU as TR6SQU, SGNFL1 as TR6FL1, SGNFL2 as TR6FL2, SGNFL3 as
010300080630     C+ TR6FL3, SGNFL4 as TR6FL4, SGNFLX as TR6FLX, SGNKSC as TR6KSC,
010400080630     C+ SGNFLS as TR6FLS, SGNDLP as TR6DLP, SGNCDP as TR6CDP, SGNNRS as
010500080630     C+ TR6NRS, SGNNCD as TR6NCD, SGNLNA as TR6LNA, SGNDLA as TR6DLA,
010600080630     C+ SGNTNA as TR6TNA, SGNTSP as TR6TSP, SGNZSC as TR6ZSC, SGNNC1 as
010700080630     C+ TR6NC1, SGNNC2 as TR6NC2, SGNRSM as TR6RSM, SGNPRM as TR6PRM,
010800080630     C+ SGNRSD as TR6RSD, SGNCAD as TR6CAD, SGNIND as TR6IND, SGNLOD as
010900080630     C+ TR6LOD, SGNPRD as TR6PRD, SGNCPD as TR6CPD, SGNNCP as TR6NCP,
011000080630     C+ SGNNCU as TR6NCU, SGNRST as TR6RST, SGNVOL as TR6VOL, SGNPKG as
011100080630     C+ TR6PKG, SGNFVR as TR6FVR, SGNAAS as TR6AAS, SGNMGS as TR6MGS,
011200080630     C+ SGNTST as TR6TST, SGNTSE as TR6TSE, SGNSSE as TR6SSE, SGNSIP as
011300080630     C+ TR6SIP, SGNSIA as TR6SIA, SGNAWB as TR6AWB, SGNISA as TR6ISA,
011400080630     C+ SGNFCA as TR6FCA, SGNEUR as TR6EUR, SGNXX1 as TR6XX1, SGNXX2 as
011500080630     C+ TR6XX2, '' as TR6BAI, 0 as TR6POC, 0 as TR6NPG, 0 as TR6NFV, 0 as
011600080630     C+ TR6DFV, '' as TR6FPP, 0 as TR6PDC, '' as TR6DPD, '' as TR6DS1, ''
011700131029     C+ as TR6DS2, '' as TR6BAU
011800131029     C+ FROM FISGN00F WHERE SGNTNP=:ktnp and
011900080630     C+ SGNDATORA>=:ticket
012000080618     C/END-EXEC
012100080630     C/EXEC SQL
012200080630     C+ OPEN FISGN
012300080630     C/END-EXEC
012400080630     c                   else
012500151130     c* ---------------------------------------------------------------
012600151130     c* Pulizia record di fisgn di bolle "normali" NON chiuse in part.x
012700151130     c* Merce Mai Affidata:
012800151130     c* ---------------------------------------------------------------
012900080630     c* pulisco record di fisgn dei giorni precedenti e già stampati
013000080630     c* pulisco record di fisgn per i quali sono trascorsi i giorni di puli-
013100080630     c* zia
013200080630     C/EXEC SQL
013300151130     C+ DELETE FROM FISGN00F WHERE sgntnp=:ktnp and
013400170125     C+  sgnatb=' ' and  sgnst2>0 AND
013500170125     C+  sgnaas*10000+sgnmgs < (:wdpprt) and
013600151130     c+ (sgnaas, sgnlnp, sgnnrs, sgnnsp) not in ( select blpaas, blplnp,
013700170117     c+  blpnrs, blpnsp from fnblp00f where blpcca='7' and
013800170125     C+  blpft1='N')
013900080630     C/END-EXEC
014000170125     C/EXEC SQL
014100170125     C+ DELETE FROM FISGN00F WHERE sgntnp=:ktnp and
014200170125     C+  sgnaas*10000+sgnmgs<:datapul  AND
014300170125     c+ (sgnaas, sgnlnp, sgnnrs, sgnnsp) not in ( select blpaas, blplnp,
014400170125     c+  blpnrs, blpnsp from fnblp00f where blpcca='7' and
014500170125     C+  blpft1='N')
014600170125     C/END-EXEC
014700151130     c* ---------------------------------------------------------------
014800151130     c* Pulizia record di fisgn di bolle chiuse in part.x Merce Mai Affidata:
014900151130     c* ---------------------------------------------------------------
015000151130     c* pulisco record di fisgn per i quali sono trascorsi i giorni di pulizia
015100151130     C/EXEC SQL
015200151130     C+ DELETE FROM FISGN00F WHERE sgntnp=:ktnp and
015300170117     C+ (sgnaas*10000+sgnmgs<:wdpprt7) and
015400151130     c+ (sgnaas, sgnlnp, sgnnrs, sgnnsp) in ( select blpaas, blplnp,
015500170117     c+  blpnrs, blpnsp from fnblp00f where blpcca='7' and
015600151130     C+  blpft1='N') and (sgnaas*10000+sgnmgs<:wdpprt7)
015700151130     C/END-EXEC
015800080630     C/EXEC SQL
015900080702     C+ DECLARE FISGNI INSENSITIVE CURSOR FOR
016000131029     C+ SELECT
016100131029     c+ case
016200131029     c+ when sgnatb <> ' ' then 'A'
016300131029     c+ else ' '
016400131029     c+ end  as tr6atb,
016500131029     C+ SGNWHO as
016600080630     C+ TR6WHO, SGNSEA as TR6SEA, SGNST1 as TR6ST1, SGNST2 as TR6ST2,
016700080630     C+ SGNSQU as TR6SQU, SGNFL1 as TR6FL1, SGNFL2 as TR6FL2, SGNFL3 as
016800080630     C+ TR6FL3, SGNFL4 as TR6FL4, SGNFLX as TR6FLX, SGNKSC as TR6KSC,
016900080630     C+ SGNFLS as TR6FLS, SGNDLP as TR6DLP, SGNCDP as TR6CDP, SGNNRS as
017000080630     C+ TR6NRS, SGNNCD as TR6NCD, SGNLNA as TR6LNA, SGNDLA as TR6DLA,
017100080630     C+ SGNTNA as TR6TNA, SGNTSP as TR6TSP, SGNZSC as TR6ZSC, SGNNC1 as
017200080630     C+ TR6NC1, SGNNC2 as TR6NC2, SGNRSM as TR6RSM, SGNPRM as TR6PRM,
017300080630     C+ SGNRSD as TR6RSD, SGNCAD as TR6CAD, SGNIND as TR6IND, SGNLOD as
017400080630     C+ TR6LOD, SGNPRD as TR6PRD, SGNCPD as TR6CPD, SGNNCP as TR6NCP,
017500080630     C+ SGNNCU as TR6NCU, SGNRST as TR6RST, SGNVOL as TR6VOL, SGNPKG as
017600080630     C+ TR6PKG, SGNFVR as TR6FVR, SGNAAS as TR6AAS, SGNMGS as TR6MGS,
017700080630     C+ SGNTST as TR6TST, SGNTSE as TR6TSE, SGNSSE as TR6SSE, SGNSIP as
017800080630     C+ TR6SIP, SGNSIA as TR6SIA, SGNAWB as TR6AWB, SGNISA as TR6ISA,
017900080630     C+ SGNFCA as TR6FCA, SGNEUR as TR6EUR, SGNXX1 as TR6XX1, SGNXX2 as
018000080630     C+ TR6XX2, '' as TR6BAI, 0 as TR6POC, 0 as TR6NPG, 0 as TR6NFV, 0 as
018100080630     C+ TR6DFV, '' as TR6FPP, 0 as TR6PDC, '' as TR6DPD, '' as TR6DS1, ''
018200080630     C+ as TR6DS2, '' as TR6BAU FROM FISGN00F WHERE sgntnp=:ktnp
018300080630     C/END-EXEC
018400080630     C/EXEC SQL
018500080630     C+ OPEN FISGNI
018600080630     C/END-EXEC
018700080630     c                   endif
018800080618
018900080618      /FREE
019000080618
019100061017       select;
019200061017       When sqlCode = 100;
019300061017         RTNstatus = sqlCode;
019400061017         RTNopcode = 'SQLCODE';
019500061017         RTNesito  = -1;
019600061017         RETURN;
019700061017       When sqlCode < 0;
019800061017         RTNstatus = sqlCode;
019900061017         RTNopcode = 'SQLCODE';
020000061017         RTNesito  = -1;
020100060918         RETURN;
020200061017       Endsl;
020300080702       RTNrcdcnt = sqlErrD(2);
020400080618
020500080618      /END-FREE
020600060918
020700080630     c                   if        inizio=' '
020800080618     C/EXEC SQL
020900170124     C+ SET RESULT SETS for return to client CURSOR FISGN
021000080618     C/END-EXEC
021100080630     c                   else
021200080630     C/EXEC SQL
021300170124     C+ SET RESULT SETS for return to client CURSOR FISGNI
021400080630     C/END-EXEC
021500080630     c                   endif
021600061017
021700080618      /FREE
021800061016       IF sqlCode < 0;
021900061017         RTNstatus = sqlCode;
022000061017         RTNopcode = 'SQLCODE';
022100061017         RTNesito  = -1;
022200060918         RETURN;
022300060918       ENDIF;
022400060918
022500080626       ticket = wtime;
022600060918       RETURN;
022700060918
022800060918      /END-FREE
022900080630      *---------------------------------------------------------*
023000080630      * Operazioni iniziali                                     *
023100080630      *---------------------------------------------------------*
023200080630     c     OPERAZ_INI    BEGSR
023300080630     c                   time                    dataiso
023400080630     c*
023500080630     c                   move      dataiso       wdpprt
023600151130     c                   move      dataiso       wdpprt7
023700080630
023800080630      * Ricavo il Terminal di Partenza
023900080630     C                   clear                   fnlv55ds
024000080630     C                   eval      d55tpt = 'P'
024100080630     C                   movel     filiale       D55LIN
024200080630     c     *iso          movel     dataiso       D55DRF
024300080630     C                   call      'FNLV55R'
024400080630     C                   parm                    fnlv55ds
024500080630      *
024600080630     c                   IF        d55err = *blanks
024700080630     C                   movel     D55TFP        ktnp
024800080630     C                   ENDIF
024900080630
025000080630     c     ktab          chain     tabel
025100080630     c                   IF        %FOUND
025200080630     c                   movel     tbluni        ds5a2
025300080630     c                   ELSE
025400080630     c                   z-add     16            §5asgn
025500140403     c                   z-add     2             §5asgns
025600151130     c                   z-add     10            §5asgn7
025700080630     c                   ENDIF
025800080630      *
025900080630     c     dataiso       subdur    §5asgn:*d     dataiso
026000080630     c                   move      dataiso       datapul
026100140403
026200140403     c*  Sottaggo eventuali giorni lavorativi dalla data pulizia record già stampati
026300140403     c                   if        §5asgns>0
026400140403     c                   clear                   xgiolavds
026500140403     c                   eval      ixgldata=wdpprt
026600140403     c                   eval      ixglfil =ktnp
026700140403     c                   eval      ixglpa  ='P'
026800140403     c                   eval      ixglsub ='S'
026900140403     c                   eval      ixglgg  =§5asgns
027000140403     c                   eval      ixgllav ='S'
027100140403     c                   call      'XGIOLAV'
027200140403     c                   parm                    xgiolavds
027300140403     c                   Move      oxgldata      wdpprt
027400140403     c                   endif
027500151130     c*  Sottaggo eventuali giorni lavorativi dalla data pulizia record chiusi per merce mai aff.
027600151130     c                   clear                   xgiolavds
027700151130     c                   eval      ixgldata=wdpprt7
027800151130     c                   eval      ixglfil =ktnp
027900151130     c                   eval      ixglpa  ='P'
028000151130     c                   eval      ixglsub ='S'
028100151130     c                   if        §5asgn7>0
028200151130     c                   eval      ixglgg  =§5asgn7
028300151130     c                   else
028400151130     c                   eval      ixglgg  =10
028500151130     c                   endif
028600151130     c                   eval      ixgllav ='S'
028700151130     c                   call      'XGIOLAV'
028800151130     c                   parm                    xgiolavds
028900151130     c                   Move      oxgldata      wdpprt7
029000080630
029100080630     c     ktab          klist
029200080630     c                   kfld                    codut
029300080630     c                   kfld                    cod
029400080630     c                   kfld                    key
029500080630     c                   endsr

000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300161122     H* STAMPA mappatura stati clienti
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900161122     FTIS7VMS6P O    E             PRINTER OFLIND(*IN05)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300991214     D*------------
001400970526     D* ARCHITETTURA
001500970526     D*------------
001600970526     D KPJBA         E DS
001700991214     D*------------------
001800991214     D* DS X PASSAGGIO PARAMETRI
001900991214     D*------------------
002000161122     d Param           ds                  inz qualified
002100161122     d  Proc                         10
002200161122     d  KSU                           8
002300161122     d  DDE                           8s 0
002400161122     d  CauBRT                       15
002500161122     d  StsCli                       15
002600161122     d  DrillUp                        n
002700161122     d  Squad                          n
002800161122       // - Stringa SQL da eseguire
002900161122     d wSQl            s           1024    inz  varying
003000161122     d VMS_Drill       ds                  qualified
003100161122     d  Proc                         10
003200161122     d  KSU                           8
003300161122     d  Count                         9s 0
003400161122     d  DDE                           8s 0
003500161122     d  DSC                           8s 0
003600161122     d VMS_Squad       ds                  qualified
003700161122     d  Proc                         10
003800161122     d  KSU                           8
003900161122     d  CauTip                        3
004000161122     d  CauBRT                       15
004100161122     d  CauTT                        15
004200161122     d  StsCli                       15
004300161122     d  DDE                           8s 0
004400161122     d  DSC                           8s 0
004500161123     d  DescIT                       50
004600161123     d VMS             ds                  qualified
004700161123     d  Proc                         10
004800161123     d  KSU                           8
004900161123     d  CauTip                        3
005000161123     d  CauBRT                       15
005100161123     d  CauTT                        15
005200161123     d  StsCli                       15
005300161123     d  DDE                           8s 0
005400161123     d  DSC                           8s 0
005500161123     d  DescIT                       50
005600011206     D*-------------------
005700011206     D* VARIABILI DI WRK
005800011206     D*-------------------
005900970526     C*------------------------------------------------------------------------*
006000991214     C* MAIN LINES
006100970526     C*------------------------------------------------------------------------*
006200970526     C*
006300161122     C                   EXSR      sr_RoutInz
006400991214     C* ESEGUE ROUTINE DI CREAZIONE E STAMPA
006500991214     C                   EXSR      ESEGUI
006600011205     C                   EXSR      FINSR
006700000306     C*
006800970526     C                   SETON                                        LR
006900161122     C*------------------------------------------------------------------------*
007000161122     C* sr_RoutInz - OPERAZIONI INIZIALI
007100161122     C*------------------------------------------------------------------------*
007200161122     C     sr_RoutInz    BEGSR
007300161123     C*
007400161123     C* valorizzo i parametri di lancio in stampa
007500161123     C                   exsr      valPAR
007600161123     C*
007700161123     C* definisco quale SQL lanciare
007800161123     C                   exsr      defSQL
007900161122     C*
008000161123     C* stampo l'intestazione
008100161122     C                   seton                                        05
008200161122     C                   exsr      stpTES
008300161122     C*
008400161122     C                   ENDSR
008500011205     C*------------------------------------------------------------------------*
008600011205     C* FINSR - OPERAZIONI FINALI
008700011205     C*------------------------------------------------------------------------*
008800011205     C     FINSR         BEGSR
008900011205     C*
009000011205     C*
009100011205     C                   ENDSR
009200970526     C*------------------------------------------------------------------------*
009300991217     C* ESEGUI - OPERAZIONI PRINCIPALI
009400970526     C*------------------------------------------------------------------------*
009500991214     C     ESEGUI        BEGSR
009600020207     C*
009700161123     C                   select
009800161123
009900161123     C                   when      Param.DrillUp = *on
010000161123     C                   exsr      LeggoSQLDrill
010100161123
010200161123     C* se sono richieste le squadrature, automaticamente siamo in drill-down
010300161123     C                   when      Param.Squad  = *on
010400161123     C                   exsr      LeggoSQLSquad
010500161123
010600161123     C                   other
010700161123     C                   exsr      LeggoSQL
010800161123
010900161123     C                   endsl
011000011205     C*
011100011205     C* Al termine chiudo il prospetto
011200161122     C                   write     S7V6RI
011300161122     C                   write     S7V6FI
011400970526     C*
011500970526     C                   ENDSR
011600011205     C*------------------------------------------------------------------------*
011700161123     C* DEFSQL - definisco quale SQL lanciare
011800011205     C*------------------------------------------------------------------------*
011900161123     C     DEFSQL        BEGSR
012000161123     C*
012100161123     C                   select
012200161123
012300161123     C                   when      Param.DrillUp = *on
012400161123     C                   exsr      PrepSQLDrill
012500161123
012600161123     C* se sono richieste le squadrature, automaticamente siamo in drill-down
012700161123     C                   when      Param.Squad  = *on
012800161123     C                   exsr      PrepSQLSquad
012900161123
013000161123     C                   other
013100161123     C                   exsr      PrepSQL
013200161123
013300161123     C                   endsl
013400161123     C*
013500161123     C                   ENDSR
013600161123     C*------------------------------------------------------------------------*
013700161123     C* VALPAR - VALORIZZA I PARAMETRI DI LANCIO IN STAMPA
013800161123     C*------------------------------------------------------------------------*
013900161123     C     VALPAR        BEGSR
014000161123     C*
014100161123     C                   eval      StpSif = knsif
014200161123     C*
014300161122     C                   eval      StpProc   = Param.Proc
014400161122     C                   eval      StpKsu    = Param.Ksu
014500161123     C                   if        Param.DDE > 0
014600161123     C                   eval      StpDDE    = %dec(%date(Param.DDE):*eur)
014700161123     C                   else
014800161123     C                   eval      StpDDE    = 0
014900161123     C                   endif
015000161122     C                   eval      StpCauBRT = Param.CauBRT
015100161122     C                   eval      StpStsCli = Param.StsCli
015200161122     C                   eval      StpDrillUp = 'NO'
015300161122     C                   if        Param.DrillUp = *on
015400161122     C                   eval      StpDrillUp = 'SI'
015500161122     C                   endif
015600161122     C                   eval      StpSquad   = 'NO'
015700161122     C                   if        Param.Squad  = *on
015800161122     C                   eval      StpSquad = 'SI'
015900161122     C                   endif
016000011205     C*
016100011205     C                   ENDSR
016200011205     C*------------------------------------------------------------------------*
016300011205     C* STPTES - ESEGUE LA STAMPA DELLA TESTATA QUANDO NECESSARIO
016400011205     C*------------------------------------------------------------------------*
016500011205     C     STPTES        BEGSR
016600011205     C*
016700011205     C                   if        *in05 = *on
016800161122     C                   write     S7V6TE
016900161122     C                   write     S7V6IN
017000011205     C                   setoff                                       05
017100011205     C                   endif
017200011205     C*
017300011205     C                   ENDSR
017400970526     C*------------------------------------------------------------------------*
017500970526     C* *INZSR - OPERAZIONI INIZIALI
017600970526     C*------------------------------------------------------------------------*
017700970526     C     *INZSR        BEGSR
017800970526     C*---
017900970526     C* RICEVIMENTO PARAMETRI
018000970526     C*---
018100970526     C     *ENTRY        PLIST
018200970612     C                   PARM                    KPJBA
018300991214     C                   MOVEL     KPJBU         PARAM
018400011205     C*
018500000810     C* CARICA LE TABELLE OCCORRENTI
018600161123     C***                EXSR      CARTAB
018700970526     C*---
018800991214     C* CHIAVI DI LETTURA
018900991214     C*---
019000991214     C*
019100970526     C                   ENDSR
019200161122
019300161122       //--------------------------------------------------------------
019400161122       // Preparazione stringa SQL per Drill up                        ?
019500161122       //--------------------------------------------------------------
019600161122       BEGSR  PrepSQLDrill;
019700161122
019800161122         wSQL = 'select vmsproc, vmsksu, count(*), vmsdde, vmsdsc +
019900161122                 from tivms00f +
020000161122                 where vmsatb = '' ''' ;
020100161122         if StpProc <> *blank;
020200161122           wSQL = %trim(wSQL) +
020300161122                ' and vmsproc = ''' + StpProc + '''';
020400161122         endif;
020500161122         if StpKsu  <> *blank;
020600161122           wSQL = %trim(wSQL) +
020700161122                ' and vmsksu = ''' + StpKsu  + '''';
020800161122         endif;
020900161122         if StpDDE    <> 0     ;
021000161122           wSQL = %trim(wSQL) +
021100161122                ' and vmsdde <= ' + %char(%dec(%date(StpDDE:*eur))) + ' and +
021200161122                  vmsdsc >= ' + %char(%dec(%date(StpDDE:*eur)));
021300161122         endif;
021400161122         if StpCauBRT <> *blank;
021500161122           wSQL = %trim(wSQL) +
021600161122                ' and vmscaubrt like ''%' + %trim(StpCauBRT) + '%''';
021700161122         endif;
021800161122         if StpStsCli <> *blank;
021900161122           wSQL = %trim(wSQL) +
022000161122                ' and vmsstscli like ''%' + %trim(StpStsCli) + '%''';
022100161122         endif;
022200161122         wSQL = %trim(wSQL) +
022300161122                ' group by vmsproc, vmsksu, vmsdde, vmsdsc' +
022400161122                ' order by vmsproc, vmsksu, vmsdde, vmsdsc';
022500161122
022600161122         // Dichiarazione cursore
022700161122         exec sql   prepare S1   from :wSQL;
022800161122         exec sql   declare C1  asensitive   cursor for S1;
022900161122
023000161122         // Apertura del cursore
023100161122         exec sql   open C1;
023200161122
023300161122         *in99 = *off;
023400161122         if SQLCode < 0;
023500161122           *in99 = *on;
023600161122         endif;
023700161122
023800161122       ENDSR;
023900161122
024000161122       //--------------------------------------------------------------
024100161122       // Preparazione stringa SQL per ricerca squadrature             ?
024200161122       // le Squadrature sono quei rcd il cui valore di CauBRT non è presente in un altro rcd con
024300161122       // procedura standard (*STD)
024400161122       //--------------------------------------------------------------
024500161122       BEGSR  PrepSQLSquad;
024600161122
024700161122         wSQL = 'select vmsproc, vmsksu, vmscautip, vmscaubrt, vmscautt , +
024800161122                 vmsstscli , vmsdde, vmsdsc, vmsdescriz +
024900161122                 from tivms00f +
025000161122                 where vmsatb = '' '' +
025100161122                 and vmscaubrt not in +
025200161122                 (select vmscaubrt from tivms00f +
025300161122                 where vmsatb = '' '' +
025400161122                 and vmsproc=''*STD'')';
025500161122         if StpProc <> *blank;
025600161122           wSQL = %trim(wSQL) +
025700161122                ' and vmsproc = ''' + StpProc + '''';
025800161122         endif;
025900161122         if StpKsu  <> *blank;
026000161122           wSQL = %trim(wSQL) +
026100161122                ' and vmsksu = ''' + StpKsu  + '''';
026200161122         endif;
026300161122         if StpDDE    <> 0     ;
026400161122           wSQL = %trim(wSQL) +
026500161122                ' and vmsdde <= ' + %char(%dec(%date(StpDDE:*eur))) + ' and +
026600161122                  vmsdsc >= ' + %char(%dec(%date(StpDDE:*eur)));
026700161122         endif;
026800161122         if StpCauBRT <> *blank;
026900161122           wSQL = %trim(wSQL) +
027000161122                ' and vmscaubrt like ''%' + %trim(StpCauBRT) + '%''';
027100161122         endif;
027200161122         if StpStsCli <> *blank;
027300161122           wSQL = %trim(wSQL) +
027400161122                ' and vmsstscli like ''%' + %trim(StpStsCli) + '%''';
027500161122         endif;
027600161122         wSQL = %trim(wSQL) +
027700161122                ' order by vmsproc, vmsksu, vmsdde, vmscautip, vmscaubrt';
027800161122
027900161122         // Dichiarazione cursore
028000161122         exec sql   prepare S2   from :wSQL;
028100161122         exec sql   declare C2  asensitive   cursor for S2;
028200161122
028300161122         // Apertura del cursore
028400161122         exec sql   open C2;
028500161122
028600161122         *in99 = *off;
028700161122         if SQLCode < 0;
028800161122           *in99 = *on;
028900161122         endif;
029000161122
029100161122       ENDSR;
029200161123
029300161123       //--------------------------------------------------------------
029400161123       // Preparazione stringa SQL no drill no Squadrature             ?
029500161123       //--------------------------------------------------------------
029600161123       BEGSR  PrepSQL;
029700161123
029800161123         wSQL = 'select vmsproc, vmsksu, vmscautip, vmscaubrt, vmscautt , +
029900161123                 vmsstscli , vmsdde, vmsdsc, vmsdescriz +
030000161123                 from tivms00f +
030100161123                 where vmsatb = '' ''';
030200161123         if StpProc <> *blank;
030300161123           wSQL = %trim(wSQL) +
030400161123                ' and vmsproc = ''' + StpProc + '''';
030500161123         endif;
030600161123         if StpKsu  <> *blank;
030700161123           wSQL = %trim(wSQL) +
030800161123                ' and vmsksu = ''' + StpKsu  + '''';
030900161123         endif;
031000161123         if StpDDE    <> 0     ;
031100161123           wSQL = %trim(wSQL) +
031200161123                ' and vmsdde <= ' + %char(%dec(%date(StpDDE:*eur))) + ' and +
031300161123                  vmsdsc >= ' + %char(%dec(%date(StpDDE:*eur)));
031400161123         endif;
031500161123         if StpCauBRT <> *blank;
031600161123           wSQL = %trim(wSQL) +
031700161123                ' and vmscaubrt like ''%' + %trim(StpCauBRT) + '%''';
031800161123         endif;
031900161123         if StpStsCli <> *blank;
032000161123           wSQL = %trim(wSQL) +
032100161123                ' and vmsstscli like ''%' + %trim(StpStsCli) + '%''';
032200161123         endif;
032300161123         wSQL = %trim(wSQL) +
032400161123                ' order by vmsproc, vmsksu, vmsdde, vmscautip, vmscaubrt';
032500161123
032600161123         // Dichiarazione cursore
032700161123         exec sql   prepare S3   from :wSQL;
032800161123         exec sql   declare C3  asensitive   cursor for S3;
032900161123
033000161123         // Apertura del cursore
033100161123         exec sql   open C3;
033200161123
033300161123         *in99 = *off;
033400161123         if SQLCode < 0;
033500161123           *in99 = *on;
033600161123         endif;
033700161123
033800161123       ENDSR;
033900161122
034000161122       //--------------------------------------------------------------
034100161123       // Lettura occorrenza per drill up                              ?
034200161122       //--------------------------------------------------------------
034300161123       BEGSR  LeggoSQLDrill;
034400161122
034500161122         exec sql  fetch next  from C1  into :VMS_Drill;
034600161122
034700161123         dow SQLCode = 0;
034800161123
034900161123           // valorizzo campi stampa
035000161123           de_Proc   = VMS_Drill.Proc   ;
035100161123           de_KsU    = VMS_Drill.KsU    ;
035200161123           de_CauTip = *blank           ;
035300161123           evalr de_CauBRT = 'Tot:'   ;
035400161123           evalr de_CauTT  = %char(VMS_Drill.Count);
035500161123           de_StsCli = *blank           ;
035600161123           de_DDe    = %dec(%date(VMS_Drill.DDE):*eur);
035700161123           de_DSc    = %dec(%date(VMS_Drill.DSC):*eur);
035800161123           de_DescIT = *blank           ;
035900161123
036000161123           exsr      stpTES;
036100161123           write     S7V6DE;
036200161123           clear     S7V6DE;
036300161123
036400161123           exec sql  fetch next  from C1  into :VMS_Drill;
036500161123         enddo;
036600161123
036700161123         exec sql  close C1;
036800161122
036900161122       ENDSR;
037000161122
037100161122       //--------------------------------------------------------------
037200161123       // Lettura occorrenza per ricerca squadrature                   ?
037300161122       //--------------------------------------------------------------
037400161123       BEGSR  LeggoSQLSquad;
037500161122
037600161122         exec sql  fetch next  from C2  into :VMS_Squad;
037700161123
037800161123         dow SQLCode = 0;
037900161123
038000161123           // valorizzo campi stampa
038100161123           de_Proc   = VMS_Squad.Proc   ;
038200161123           de_KsU    = VMS_Squad.KsU    ;
038300161123           de_CauTip = VMS_Squad.CauTip ;
038400161123           de_CauBRT = VMS_Squad.CauBRT ;
038500161123           de_CauTT  = VMS_Squad.CauTT  ;
038600161123           de_StsCli = VMS_Squad.StsCli ;
038700161123           de_DDe    = %dec(%date(VMS_Squad.DDE):*eur);
038800161123           de_DSc    = %dec(%date(VMS_Squad.DSC):*eur);
038900161123           de_DescIT = VMS_Squad.DescIT ;
039000161123
039100161123           exsr      stpTES;
039200161123           write     S7V6DE;
039300161123           clear     S7V6DE;
039400161123
039500161123           exec sql  fetch next  from C2  into :VMS_Squad;
039600161123         enddo;
039700161123
039800161123         exec sql  close C2;
039900161122
040000161122       ENDSR;
040100161123
040200161123       //--------------------------------------------------------------
040300161123       // Lettura occorrenza
040400161123       //--------------------------------------------------------------
040500161123       BEGSR  LeggoSQL;
040600161123
040700161123         exec sql  fetch next  from C3  into :VMS;
040800161123
040900161123         dow SQLCode = 0;
041000161123
041100161123           // valorizzo campi stampa
041200161123           de_Proc   = VMS.Proc   ;
041300161123           de_KsU    = VMS.KsU    ;
041400161123           de_CauTip = VMS.CauTip ;
041500161123           de_CauBRT = VMS.CauBRT ;
041600161123           de_CauTT  = VMS.CauTT  ;
041700161123           de_StsCli = VMS.StsCli ;
041800161123           de_DDe    = %dec(%date(VMS.DDE):*eur);
041900161123           de_DSc    = %dec(%date(VMS.DSC):*eur);
042000161123           de_DescIT = VMS.DescIT ;
042100161123
042200161123           exsr      stpTES;
042300161123           write     S7V6DE;
042400161123           clear     S7V6DE;
042500161123
042600161123           exec sql  fetch next  from C3  into :VMS;
042700161123         enddo;
042800161123
042900161123         exec sql  close C3;
043000161123
043100161123       ENDSR;
043200161122

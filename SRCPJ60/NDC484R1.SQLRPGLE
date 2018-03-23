000100981211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300981016     H*PARMS ACTGRP(PULPARTITE) datFmt(*iso) Commit(*none) rdb(*none)
000400000612D0525H*PARMS DYNUSRPRF(*OWNER)
000500970526     H*--------INIZIO SPECIFICHE --------------------------*
000600970526     H DECEDIT('0,') DATEDIT(*DMY.)
000700970526      *
000800980922      *        delete files
000900980917      *       -------------------------------------------------
001000980922      *
001100980924    >FNdPpa00f  o    E             DISK    RENAME(NdPpa000:NdPpa000)
001200980922     F                                     UsrOpn Commit
001300980924    >FNdPpa01l  uf   E           K DISK    RENAME(NdPpa000:NdPpa001)
001400980922     F                                     UsrOpn Commit
001500980924    >FNdPsc00f  o    E             DISK    RENAME(NdPsc000:NdPsc000)
001600980924     F                                     UsrOpn Commit
001700980924    >FNdPsc01l  uf   E           K DISK    RENAME(NdPsc000:NdPsc001)
001800980924     F                                     UsrOpn Commit
001900980924    >FNdPps00f  o    E             DISK    RENAME(NdPps000:NdPps000)
002000980924     F                                     UsrOpn Commit
002100980924    >FNdPps01l  uf   E           K DISK    RENAME(NdPps000:NdPps001)
002200980924     F                                     UsrOpn Commit
002300980924    >FNdMix00f  o    E             DISK    RENAME(NdMix000:NdMix000)
002400980924     F                                     UsrOpn Commit
002500980924    >FNdMix01l  uf   E           K DISK    RENAME(NdMix000:NdMix001)
002600980924     F                                     UsrOpn Commit
002700000410D0525FNdCpa00f  o    E             DISK    RENAME(NdCpa000:NdCpa000)
002800000410  "  F                                     UsrOpn Commit
002900000410  "  FNdCpa01l  uf   E           K DISK    RENAME(NdCpa000:NdCpa001)
003000000410D0525F                                     UsrOpn Commit
003100981008    >FAnTcm00f  o    E             DISK    RENAME(AnTcm000:AnTcm000)
003200981008     F                                     UsrOpn Commit
003300981006    >FAnTcm01l  uf   E           K DISK    RENAME(AnTcm000:AnTcm001)
003400981008     F                                     UsrOpn Commit
003500981008    >FAnRcm00f  o    E             DISK    RENAME(AnRcm000:AnRcm000)
003600981008     F                                     UsrOpn Commit
003700981006    >FAnRcm01l  uf   E           K DISK    RENAME(AnRcm000:AnRcm001)
003800981008     F                                     UsrOpn Commit
003900980915      *
004000980915      * Passaggio Parametri
004100970526     D KPJBA         E DS
004200980915      *-------------
004300980915      * Dati di ambiente ottenuti da XSOC
004400970526     D SOC001        E DS                  EXTNAME(XSOC001DS)
004500980915      *-------------
004600980915      * DS Interna per dati di output di XSOC
004700970526     D XSOCDS          DS          1000
004800980924      *
004900980922     D Ndc484Ds      E DS
005000980922      *-------------
005100000410D0525X*DimFisici       C                   Const(6)
005200000410  "  X*DimLogici       C                   Const(6)
005300000410  "   *
005400000410  "  D DimFisici       C                   Const(7)
005500000410D0525D DimLogici       C                   Const(7)
005600980922      * Schiere file
005700980922     D Fisici          S             10    Dim(DimFisici) CTDATA PERRCD(1)
005800980922     D Logici          S             10    Dim(DimLogici) CTDATA PERRCD(1)
005900980922      *
006000980922     D  x              S              3  0
006100980922     D  WrkLibr        S             10
006200980922     D  WrkFile        S             10
006300980922     D  FlgBlock       S              1
006400980925     D  Sql            S            500
006500980915      *
006600980924     D  WrkSys         S                   like(PpaSys)
006700980924     D  WrkNrAsInt     S                   like(PpaNrAsInt)
006800980924      *
006900980915      *-------------
007000970526      * Reperimento nome PGM
007100970526     D STATUS         SDS           333
007200970526     D  DSPGM            *Proc
007300970526     D  Utente               254    263
007400980924      *-------------
007500980925      *Indice DsMultipla
007600980925     D $Ind            S              4  0
007700980929      * Ds fetch multipla
007800980929     D DsFetch         DS                  Occurs(1000)
007900980929     D DsCtb                               like(PpaCtb)
008000980929     D DsKcc                               like(PpaKcc)
008100980929     D DsKsc                               like(PpaKsc)
008200980929     D DsDtPar                             like(PpaDtPar)
008300980929     D DsNrPar                             like(PpaNrPar)
008400980929     D DsSerPar                            like(PpaSerPar)
008500980925      *
008600980929     D KSql1           C                   Const('Select distinct +
008700980925     D                                     PasCtb , PasKcc , PasKsc , +
008800980925     D                                     PasDtPar , +
008900980925     D                                     PasNrPar , PasSerPar  +
009000980925     D                                     from ')
009100980925     D KSql2           C                   Const('/NdPas00f +
009200980925     D                                     Where PasSocieta =''')
009300981013     D KSql3           C                   Const(''' for fetch only ')
009400981013      *
009500981013     D KSql1B          C                   Const('Select +
009600981013     D                                     PpaCtb , PpaKcc , PpaKsc , +
009700981013     D                                     PpaDtPar , +
009800981013     D                                     PpaNrPar , PpaSerPar  +
009900070124C2003x***                                  from NdPpa01L +
010000070124  "  D                                     from NdPpa00F +
010100070124  "  x***                                  exception join NdPas01l +
010200070124  "  D                                     where not exists(select * +
010300070124  "  D                                     from NdPas00F +
010400070124  "  D                                     where PpaSocieta = PasSocieta +
010500070124c2003x***                                  on PpaSocieta = PasSocieta +
010600981013     D                                     and PpaCtb = PasCtb +
010700981013     D                                     and PpaKcc = PasKcc +
010800981013     D                                     and PpaKsc = PasKsc +
010900981013     D                                     and PpaDtPar = PasDtPar +
011000981013     D                                     and PpaNrPar = PasNrPar +
011100070124C2003X***                                  and PpaSerPar = PasSerPar ')
011200070124C2003D                                     and PpaSerPar = PasSerPar) ')
011300981013     D KSql2B          C                   Const('for fetch only ')
011400980924      *
011500970526      *----------------------------------------------------*
011600970526      *                MAIN LINE PROGRAM
011700970526      *----------------------------------------------------*
011800970526      *
011900970526      * Inizializzazione variabili
012000980916     C                   Exsr      InzVar
012100980915      *
012200980923     C                   Select
012300980923      * Ciclo x cancellazione
012400980923     C                   When      Ripris484 = *off
012500981013      * Cancella i ppa dei movimenti cancellati
012600980923     C                   Exsr      Cancella
012700981013      * Cancella i ppa senza pas
012800981013     C                   Exsr      Cancella2
012900981013      * Ciclo x ripristino
013000980923     C                   When      Ripris484 = *on
013100980923     C                   Exsr      Ripristina
013200980923      * Errore
013300980923     C                   Other
013400980923     C                   Eval      Err484 = '1'
013500980923     C                   Exsr      EndPGM
013600980923     C                   EndSl
013700981015      *
013800981015      * Tenta la rgzpfm dei files cancellati
013900981015     C                   Exsr      CloseFiles
014000981015     C                   Exsr      DltOvr
014100090504>>>>>C**                 Exsr      OrgFiles
014200970909      *
014300980915     C                   Exsr      EndPGM
014400970526      *
014500980924      ******************************************************
014600980924      * Open cursore sql
014700980924      ******************************************************
014800981013     C     InzSql        Begsr
014900980925      *
015000981013     C                   Eval      Sql = KSql1
015100981013     C                   Cat       Libr484:1     Sql
015200981013     C                   Cat       KSql2:0       Sql
015300981013     C                   Cat       Societa484:0  Sql
015400981013     C                   Cat       KSql3:0       Sql
015500981013      *
015600981013     C                   Endsr
015700981013      ******************************************************
015800981013      * InzSql2
015900981013      ******************************************************
016000981013     C     InzSql2       Begsr
016100981013      *
016200981013     C                   Eval      Sql = KSql1B
016300981013      * Where
016400981013     C                   Cat       'Where':1     Sql
016500981013      * Societa NdMoa
016600981013     C                   Cat       'PpaSocieta':1Sql
016700981013     C                   Cat       '=''':1       Sql
016800981013     C                   Cat       Societa484:0  Sql
016900981013     C                   Cat       '''':0        Sql
017000981013      * Data reg
017100981013     C                   Cat       'and':1       Sql
017200981013     C                   Cat       'PpaDtPar':1  Sql
017300981013     C                   Cat       '<=''':1      Sql
017400981013     C                   Movel     DtPul484      Alfa10           10
017500981013     C                   Cat       Alfa10:0      Sql
017600981013     C                   Cat       '''':0        Sql
017700981013      * for fetch only
017800981013     C                   Cat       KSql2B:1      Sql
017900981013      *
018000981013     C                   Endsr
018100981013      ******************************************************
018200981013      * Open cursore sql
018300981013      ******************************************************
018400981013     C     OpenSql       Begsr
018500981013      *
018600981013     C/EXEC SQL
018700981013     C+     PREPARE S1 FROM :SQL
018800981013     C/END-EXEC
018900981013      *
019000981013     C/EXEC SQL
019100981013     C+     Declare A1 cursor for S1
019200981013     C/END-EXEC
019300981013      *
019400981013     C/EXEC SQL
019500981013     C+     OPEN A1
019600981013     C/END-EXEC
019700981013      *
019800981013     C                   Endsr
019900980924      ********************************************************************
020000980924      * Fetch
020100980924      ********************************************************************
020200980924     C     Fetch         Begsr
020300980924      *
020400980924     C     $Ind          Ifeq      SqlEr3
020500980924     C/EXEC SQL
020600980929    >C+     FETCH A1 FOR 1000 ROWS INTO :DsFetch
020700980924     C/END-EXEC
020800980924     C                   Z-Add     1             $Ind
020900980924     C                   Else
021000980924     C                   Add       1             $Ind
021100980924     C                   EndIf
021200980924      *
021300980924     C                   EndSr
021400980925      ********************************************************************
021500980925      * Close Sql
021600980925      ********************************************************************
021700980925     C     CloseSql      Begsr
021800980925      *
021900980925     C/EXEC SQL
022000980925    >C+     Close A1
022100980925     C/END-EXEC
022200980925      *
022300980925     C                   EndSr
022400980922      ************************************************************
022500980924      * Cancella
022600980922      ************************************************************
022700980924     C     Cancella      BegSr
022800981013      *
022900981013      * InzSql
023000981013     C                   Exsr      InzSql
023100981013      * OpenSql
023200981013     C                   Exsr      OpenSql
023300980923      *
023400980924     C                   Z-ADD     *zeros        $Ind
023500980924     C                   Z-ADD     *zeros        SqlEr3
023600980922      *
023700980922     C                   Do        *hival
023800981013     C                   Exsr      Fetch
023900980925      * Test sqlcod x esito fetch
024000980924     C                   Select
024100030805D1414X****               When      SqlCod = 0
024200030805D1414C*
024300030805  "  C                   When      SqlCod     >= 0      and
024400030805D1414C                             SqlCod     <> 100
024500980929    >C     $Ind          Occur     DsFetch
024600980924     C                   When      SqlCod = 100
024700980924     C                   Leave
024800980924     C                   Other
024900980924     C                   Eval      Err484 = '1'
025000980924     C                   Exsr      EndPgm
025100980924     C                   Leave
025200980924     C                   EndSl
025300980929      *
025400980929     C                   Eval      PpaSocieta = Societa484
025500980929     C                   Eval      PpaCtb     = DsCtb
025600980929     C                   Eval      PpaKcc     = DsKcc
025700980929     C                   Eval      PpaKsc     = DsKsc
025800980929     C                   Eval      PpaDtPar   = DsDtPar
025900980929     C                   Eval      PpaNrPar   = DsNrPar
026000980929     C                   Eval      PpaSerPar  = DsSerPar
026100980924      *
026200980924     C     K07Ppa01      Chain     NdPpa001                           21
026300980923      *
026400980925     C                   If        *in21 = *off
026500980924     C                   Eval      WrkSys     = PpaSys
026600980924     C                   Eval      WrkNrAsInt = PpaNrAsInt
026700980923     C                   Exsr      DelFiles
026800980923     C                   Commit
026900980924     C                   Endif
027000980925      *
027100980923     C                   Enddo
027200981013      * fine programma
027300981013     C                   Exsr      CloseSql
027400980922      *
027500980922     C                   EndSr
027600980923      ************************************************************
027700981013      * Cancella2 - Call pgm cancellazione Ppa senza Pas
027800980923      ************************************************************
027900981013     C     Cancella2     BegSr
028000981013      *
028100981013      * InzSql2
028200981013     C                   Exsr      InzSql2
028300981013      *
028400981013     C                   Exsr      OpenSql
028500981013      *
028600981013     C                   Z-ADD     *zeros        $Ind
028700981013     C                   Z-ADD     *zeros        SqlEr3
028800981013      *
028900981013     C                   Do        *hival
029000981013     C                   Exsr      Fetch
029100981013      * Test sqlcod x esito fetch
029200981013     C                   Select
029300030805D1414X****               When      SqlCod = 0
029400030805D1414C*
029500030805  "  C                   When      SqlCod     >= 0      and
029600030805D1414C                             SqlCod     <> 100
029700981013    >C     $Ind          Occur     DsFetch
029800981013     C                   When      SqlCod = 100
029900981013     C                   Leave
030000981013     C                   Other
030100981013     C                   Eval      Err484 = '1'
030200981013     C                   Exsr      EndPgm
030300981013     C                   Leave
030400981013     C                   EndSl
030500981013      *
030600981013     C                   Eval      PpaSocieta = Societa484
030700981013     C                   Eval      PpaCtb     = DsCtb
030800981013     C                   Eval      PpaKcc     = DsKcc
030900981013     C                   Eval      PpaKsc     = DsKsc
031000981013     C                   Eval      PpaDtPar   = DsDtPar
031100981013     C                   Eval      PpaNrPar   = DsNrPar
031200981013     C                   Eval      PpaSerPar  = DsSerPar
031300981013      *
031400981013     C     K07Ppa01      Chain     NdPpa001                           21
031500981013      *
031600981013     C                   If        *in21 = *off
031700981013     C                   Eval      WrkSys     = PpaSys
031800981013     C                   Eval      WrkNrAsInt = PpaNrAsInt
031900981013     C                   Exsr      DelFiles
032000981013     C                   Commit
032100981013     C                   Endif
032200981013      *
032300981013     C                   Enddo
032400981013      *
032500981013     C                   Exsr      CloseSql
032600981013      *
032700981013     C                   EndSr
032800981013      ************************************************************
032900981013      * Ciclo su NdPpa x ripristinare
033000981013      ************************************************************
033100981013     C     Ripristina    BegSr
033200981013      *
033300980924     C     *loval        Setll     NdPpa001
033400980923      *
033500980923     C                   Do        *hival
033600980924     C                   Read      NdPpa001                               21
033700980923     C   21              Leave
033800980923      *
033900980924     C                   Eval      WrkSys     = PpaSys
034000980924     C                   Eval      WrkNrAsInt = PpaNrAsInt
034100980924      *
034200980923     C                   Exsr      DelFiles
034300980923     C                   Commit
034400980923     C                   Enddo
034500980923      *
034600980923     C                   EndSr
034700970526      ************************************************************
034800980923      * Guida files da cancellare
034900970526      ************************************************************
035000980923     C     DelFiles      BegSr
035100980923      *
035200980924     C                   Exsr      DelNdPpa
035300980924     C                   Exsr      DelNdPps
035400980924     C                   Exsr      DelNdPsc
035500980924     C                   Exsr      DelNdMix
035600000410D0525C                   Exsr      DelNdCpa
035700981006     C                   Exsr      DelAnTcm
035800980922      *
035900980922     C                   EndSr
036000980924      ************************************************************
036100980924      * Cancella Ndppa
036200980924      ************************************************************
036300980924     C     DelNdppa      BegSr
036400980924      *
036500980924     C                   Write     NdPpa000
036600980924     C                   Delete    NdPpa001
036700980924      *
036800980924     C                   EndSr
036900980923      ************************************************************
037000980924      * Cancella NdPps
037100980923      ************************************************************
037200980924     C     DelNdPps      BegSr
037300980923      *
037400980925     C     WrkKList      Setll     NdPps001
037500980923      *
037600980923     C                   Do        *hival
037700980925     C     WrkKList      Reade     NdPps001                               21
037800980923     C   21              Leave
037900980924     C                   Write     NdPps000
038000980924     C                   Delete    NdPps001
038100980923     C                   Enddo
038200980923      *
038300980923     C                   EndSr
038400980924      ************************************************************
038500980924      * Cancella NdPsc
038600980924      ************************************************************
038700980924     C     DelNdPsc      BegSr
038800980924      *
038900980925     C     WrkKList      Setll     NdPsc001
039000980924      *
039100980924     C                   Do        *hival
039200980925     C     WrkKList      Reade     NdPsc001                               21
039300980924     C   21              Leave
039400980924     C                   Write     NdPsc000
039500980924     C                   Delete    NdPsc001
039600980924     C                   Enddo
039700980924      *
039800980924     C                   EndSr
039900980924      ************************************************************
040000980924      * Cancella NdMix
040100980924      ************************************************************
040200980924     C     DelNdMix      BegSr
040300980924      *
040400980925     C     WrkKList      Setll     NdMix001
040500980924      *
040600980924     C                   Do        *hival
040700980925     C     WrkKList      Reade     NdMix001                               21
040800980924     C   21              Leave
040900980924     C                   Write     NdMix000
041000980924     C                   Delete    NdMix001
041100980924     C                   Enddo
041200980924      *
041300980924     C                   EndSr
041400000410D0525 ************************************************************
041500000410  "   * Cancella NdCpa
041600000410  "   ************************************************************
041700000410D0525C     DelNdCpa      BegSr
041800000410      *
041900000410     C     WrkKList      Setll     NdCpa001
042000000410      *
042100000410     C                   Do        *hival
042200000410     C     WrkKList      Reade     NdCpa001                               21
042300000410     C   21              Leave
042400000410     C                   Write     NdCpa000
042500000410     C                   Delete    NdCpa001
042600000410     C                   Enddo
042700000410      *
042800000410D0525C                   EndSr
042900981006      ************************************************************
043000981006      * Cancella AnTcm - testata commenti
043100981006      ************************************************************
043200981006     C     DelAnTcm      BegSr
043300981006      *
043400981006     C                   Eval      TcmSocieta = Societa484
043500981006     C                   Eval      TcmTpa     = 'PPA'
043600981006     C                   Movel     PpaCtb        TcmKy1
043700981006     C                   Movel     PpaKcc        TcmKy2
043800981006     C                   Move      PpaKsc        TcmKy2
043900981006     C                   Movel     PpaDtPar      TcmKy3
044000981006     C                   Movel     PpaNrPar      TcmKy4
044100981006     C                   Move      PpaSerPar     TcmKy4
044200981006      *
044300981006     C     K06Tcm01      Setll     AnTcm001
044400981006      *
044500981006     C                   Do        *hival
044600981006     C     K06Tcm01      Reade     AnTcm001                               21
044700981006     C   21              Leave
044800981006      * cancella righe
044900981008     C                   Write     AnTcm000
045000981006     C                   Exsr      DelAnRcm
045100981006     C                   Delete    AnTcm001
045200981006     C                   Enddo
045300981006      *
045400981006     C                   EndSr
045500981006      ************************************************************
045600981006      * Cancella AnRcm - righe commenti
045700981006      ************************************************************
045800981006     C     DelAnRcm      BegSr
045900981006      *
046000981006     C                   Eval      RcmKcm = TcmKcm
046100981006      *
046200981006     C     K01Rcm01      Setll     AnRcm001
046300981006      *
046400981006     C                   Do        *hival
046500981006     C     K01Rcm01      Reade     AnRcm001                               21
046600981006     C   21              Leave
046700981008     C                   Write     AnRcm000
046800981006     C                   Delete    AnRcm001
046900981006     C                   Enddo
047000981006      *
047100981006     C                   EndSr
047200980922      ************************************************************
047300980922      * INIZIALIZZAZIONE VARIABILI
047400980922      ************************************************************
047500980922     C     InzVar        BegSr
047600980922      *
047700980922     C                   Movel     KPJBU         Ndc484Ds
047800980922     C                   Eval      Err484 = *off
047900970526      *
048000970526      * Reperimento dati società
048100970526      *
048200980916     C                   Movel     'SOC001'      TIPXSC
048300980922     C                   Movel     Societa484    SOCXSC
048400980915     C                   Exsr      REPSOC
048500980916     C     RTNXSC        Ifne      '1'
048600980916     C                   Movel     XSOCDS        SOC001
048700980916     C                   Else
048800980917      * Errore
048900980922     C                   Eval      Err484 = '1'
049000980915     C                   Exsr      EndPGM
049100980916     C                   Endif
049200980922      * Override
049300980922     C                   Exsr      Override
049400980923      * Open
049500980923     C                   Exsr      OpenFiles
049600980917      *
049700980916     C                   EndSr
049800980915      ************************************************************
049900980915      * Reperimento dati società
050000980915      ************************************************************
050100980917     C     RepSoc        BegSr
050200980915      *
050300980925     C                   Callb     'XSOC'
050400980917     C                   Parm                    TIPXSC            6
050500980917     C                   Parm                    SOCXSC            3
050600980917     C                   Parm                    CDSXSC            9 0
050700980917     C                   Parm                    MODXSC            3
050800980917     C                   Parm      *BLANKS       RTNXSC            1
050900980917     C                   Parm                    XSOCDS
051000980917     C                   Parm                    KPJBA
051100980915      *
051200980916     C                   EndSr
051300980915      ************************************************************
051400980925      * Fine Programma
051500980915      ************************************************************
051600980925     C     EndPgm        BegSr
051700980915      *
051800980922     C                   Eval       Kpjbu = Ndc484Ds
051900980917     C                   Eval       *inlr = *on
052000980917     C                   Return
052100980915      *
052200980916     C                   EndSr
052300980922      ************************************************************
052400980922      * Override files
052500980922      ************************************************************
052600980922     C     Override      BegSr
052700980922      *
052800980922      * Prima schiera (file fisici)
052900980922     C                   If        Ripris484 = *off
053000980922     C                   Eval      WrkLibr = Libr484
053100980922     C                   Else
053200980922     C                   Eval      WrkLibr = '*LIBL'
053300980922     C                   Endif
053400980922      *
053500980922     C                   Do        DimFisici     x
053600980922     C                   Eval      WrkFile = Fisici(x)
053700980922     C                   Eval      FlgBlock = *on
053800980922     C                   Exsr      OvrFile
053900980922     C                   Enddo
054000980922      *
054100980922      * Seconda schiera (file logici)
054200980922     C                   If        Ripris484 = *off
054300980922     C                   Eval      WrkLibr = '*LIBL'
054400980922     C                   Else
054500980922     C                   Eval      WrkLibr = Libr484
054600980922     C                   Endif
054700980922      *
054800980922     C                   Do        DimLogici     x
054900980922     C                   Eval      WrkFile = Logici(x)
055000980922     C                   Eval      FlgBlock = *off
055100980922     C                   Exsr      OvrFile
055200980922     C                   Enddo
055300980922      *
055400980922     C                   EndSr
055500980922      ************************************************************
055600980922      * Override
055700980922      ************************************************************
055800980922     C     OvrFile       BegSr
055900980922      *
056000980922      * OvrDbf
056100980922     C                   Clear                   Cmd
056200980922     C     'OvrDbf'      Cat       'File(':1     Cmd
056300980922     C                   Cat       WrkFile:0     Cmd
056400980922     C                   Cat       ')':0         Cmd
056500980922      *
056600980922     C                   Cat       'Tofile(':1   Cmd
056700980922     C                   Cat       WrkLibr:0     Cmd
056800980922     C                   Cat       '/':0         Cmd
056900980922     C                   Cat       WrkFile:0     Cmd
057000980922     C                   Cat       ')':0         Cmd
057100980922      *
057200980922     C                   If        FlgBlock = *on
057300980922     C                   Cat       'SEQONLY(':1  Cmd
057400980922     C                   Cat       '*YES 1000)':0Cmd
057500980922     C                   Endif
057600980922      *
057700980922     C                   Call      'QCMDEXC'                            21
057800980922     C                   Parm                    Cmd              70
057900980922     C                   Parm      70            LeN              15 5
058000980922      *
058100980922     C                   If        *In21 = *on
058200980922     C                   Eval      Err484 = '1'
058300980922     C                   Exsr      EndPgm
058400980922     C                   Endif
058500980922      *
058600980922     C                   EndSr
058700980915      ************************************************************
058800980922      * Open
058900980915      ************************************************************
059000980922     C     OpenFiles     BegSr
059100980915      *
059200980924     C                   Open      NdPpa00f
059300980924     C                   Open      NdPps00f
059400980924     C                   Open      NdPsc00f
059500980924     C                   Open      NdMix00f
059600000410D0525C                   Open      NdCpa00f
059700981008     C                   Open      AnTcm00f
059800981008     C                   Open      AnRcm00f
059900980922      *
060000980924     C                   Open      NdPpa01l
060100980924     C                   Open      NdPps01l
060200980924     C                   Open      NdPsc01l
060300980924     C                   Open      NdMix01l
060400000410D0525C                   Open      NdCpa01l
060500981008     C                   Open      AnTcm01l
060600981008     C                   Open      AnRcm01l
060700980922      *
060800980922     C                   EndSr
060900981015      ************************************************************
061000981015      * Close - Non obblig. il pgm comunque chiude in lr
061100981015      ************************************************************
061200981015     C     CloseFiles    BegSr
061300981015      *
061400981015     C                   Close     NdPpa00f
061500981015     C                   Close     NdPps00f
061600981015     C                   Close     NdPsc00f
061700981015     C                   Close     NdMix00f
061800000410D0525C                   Close     NdCpa00f
061900981015     C                   Close     AnTcm00f
062000981015     C                   Close     AnRcm00f
062100981015      *
062200981015     C                   Close     NdPpa01l
062300981015     C                   Close     NdPps01l
062400981015     C                   Close     NdPsc01l
062500981015     C                   Close     NdMix01l
062600000410D0525C                   Close     NdCpa01l
062700981015     C                   Close     AnTcm01l
062800981015     C                   Close     AnRcm01l
062900981015      *
063000981015     C                   EndSr
063100981015      ************************************************************
063200981015      * RgzPfm files cancellati
063300981015      ************************************************************
063400981015     C     OrgFiles      BegSr
063500981015      *
063600981015      * Schiera file fisici
063700981015     C                   If        Ripris484 = *off
063800981015     C                   Eval      WrkLibr = '*LIBL'
063900981015     C                   Else
064000981015     C                   Eval      WrkLibr = Libr484
064100981015     C                   Endif
064200981015      *
064300981015     C                   Do        DimFisici     x
064400981015     C                   Eval      WrkFile = Fisici(x)
064500981015     C                   Eval      FlgBlock = *on
064600981015     C                   Exsr      RgzFile
064700981015     C                   Enddo
064800981015      *
064900981015     C                   EndSr
065000981015      ************************************************************
065100981015      * RgzPfm
065200981015      ************************************************************
065300981015     C     RgzFile       BegSr
065400981015      *
065500981015      * RgzPfm
065600981015     C                   Clear                   Cmd
065700981015     C     'RgzPfm'      Cat       'File(':1     Cmd
065800981015     C                   Cat       WrkLibr:0     Cmd
065900981015     C                   Cat       '/':0         Cmd
066000981015     C                   Cat       WrkFile:0     Cmd
066100981015     C                   Cat       ')':0         Cmd
066200981015      *
066300981015     C                   Call      'QCMDEXC'                            21
066400981015     C                   Parm                    Cmd              70
066500981015     C                   Parm      70            LeN              15 5
066600981015      *
066700981015      * Se 21 = *on non faccio nulla
066800981015      *
066900981015     C                   EndSr
067000981015      ************************************************************
067100981015      * DltOvr *all
067200981015      ************************************************************
067300981015     C     DltOvr        BegSr
067400981015      *
067500981015     C                   Clear                   Cmd
067600981015     C     'DltOvr'      Cat       'File(':1     Cmd
067700981015     C                   Cat       '*all)':0     Cmd
067800981015      *
067900981015     C                   Call      'QCMDEXC'                            21
068000981015     C                   Parm                    Cmd              70
068100981015     C                   Parm      70            LeN              15 5
068200981015      *
068300981015      * Se 21 = *on non faccio nulla
068400981015      *
068500981015     C                   EndSr
068600980922      ************************************************************
068700980922      * OPERAZIONI INIZIALI
068800980922      ************************************************************
068900980925     C     *InzSr        BegSr
069000980922      *
069100980925     C     *Entry        PList
069200980917     C                   Parm                    Kpjba
069300980923      *
069400980923      * Eseguo strcmtctl A LIVELLO DI ACTGROUP (che è il default)
069500980923      *
069600980925     C                   CallB     'XSTRCMT'
069700980915      *
069800980916     C                   EndSr
069900980915      ************************************************************
070000980925      * DEFINIZIONE CAMPI : KList,VARIABILI,CONTATORI,INDICI...
070100980915      ************************************************************
070200980925     C     DefCam        BegSr
070300980915      *
070400980925      * KList
070500980915      *
070600980925      * KList di work
070700980925     C     K07Ppa01      KList
070800980929     C                   KFld                    PpaSocieta
070900980929     C                   KFld                    PpaCtb
071000980929     C                   KFld                    PpaKcc
071100980929     C                   KFld                    PpaKsc
071200980929     C                   KFld                    PpaDtPar
071300980929     C                   KFld                    PpaNrPar
071400980929     C                   KFld                    PpaSerPar
071500980925      * KList di work
071600980925     C     WrkKList      KList
071700980925     C                   KFld                    WrkSys
071800980925     C                   KFld                    WrkNrAsInt
071900981006      * AnTcm00f
072000981006     C     K06Tcm01      KLIST
072100981006     C                   KFLD                    TcmSocieta
072200981006     C                   KFLD                    TcmTpa
072300981006     C                   KFLD                    TcmKy1
072400981006     C                   KFLD                    TcmKy2
072500981006     C                   KFLD                    TcmKy3
072600981006     C                   KFLD                    TcmKy4
072700981006      * AnRcm00f
072800981006     C     K01Rcm01      KLIST
072900981006     C                   KFLD                    RcmKcm
073000980917      *
073100980916     C                   EndSr
073200000410D0525 *
073300000410  "   * Inseriti files NDCPA00F e NDCPA01L nelle seguenti due schiere
073400000410D0525 * a tempo di compilazione
073500980922**
073600980924NDPPA00F
073700980924NDPPS00F
073800980924NDPSC00F
073900980924NDMIX00F
074000000410NDCPA00F
074100981022ANTCM00F
074200981022ANRCM00F
074300980922**
074400980924NDPPA01L
074500980924NDPPS01L
074600980924NDPSC01L
074700980924NDMIX01L
074800000410NDCPA01L
074900981022ANTCM01L
075000981022ANRCM01L

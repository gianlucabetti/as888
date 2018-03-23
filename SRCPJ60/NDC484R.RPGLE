000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300980924     H*PARMS ACTGRP(PULREGISTR)
000400970526     H*--------INIZIO SPECIFICHE --------------------------*
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526      *
000700980922      *        delete files
000800980917      *       -------------------------------------------------
000900980916      *
001000980922    >FNdHld01L  If   E           K DISK    UsrOpn
001100980917    >F                                     RENAME(NdHld000:NdHld001)
001200020306C1507 *
001300020306C1507FNdHpe00f  If   E           K DISK    UsrOpn
001400980922      *
001500980922    >FNdReg00f  o    E             DISK    RENAME(NdReg000:NdReg000)
001600980922     F                                     UsrOpn Commit
001700980923    >FNdReg02l  uf   E           K DISK    RENAME(NdReg000:NdReg002)
001800980922     F                                     UsrOpn Commit
001900980922    >FNdMov00f  o    E             DISK    RENAME(NdMov000:NdMov000)
002000980922     F                                     UsrOpn Commit
002100980922    >FNdMov01l  uf   E           K DISK    RENAME(NdMov000:NdMov001)
002200980922     F                                     UsrOpn Commit
002300980922    >FNdMoa00f  o    E             DISK    RENAME(NdMoa000:NdMoa000)
002400980922     F                                     UsrOpn Commit
002500980922    >FNdMoa01l  uf   E           K DISK    RENAME(NdMoa000:NdMoa001)
002600980922     F                                     UsrOpn Commit
002700980922    >FNdPas00f  o    E             DISK    RENAME(NdPas000:NdPas000)
002800980922     F                                     UsrOpn Commit
002900980922    >FNdPas01l  uf   E           K DISK    RENAME(NdPas000:NdPas001)
003000980922     F                                     UsrOpn Commit
003100020306C1507FNdPas03l  uf   E           K DISK    RENAME(NdPas000:NdPas003)
003200020306C1507F                                     UsrOpn Commit
003300980922    >FNdBmv00f  o    E             DISK    RENAME(NdBmv000:NdBmv000)
003400980922     F                                     UsrOpn Commit
003500980922    >FNdBmv01l  uf   E           K DISK    RENAME(NdBmv000:NdBmv001)
003600980922     F                                     UsrOpn Commit
003700980922    >FNdIva00f  o    E             DISK    RENAME(NdIva000:NdIva000)
003800980922     F                                     UsrOpn Commit
003900980922    >FNdIva01l  uf   E           K DISK    RENAME(NdIva000:NdIva001)
004000980922     F                                     UsrOpn Commit
004100980922    >FNdIvc00f  o    E             DISK    RENAME(NdIvc000:NdIvc000)
004200980922     F                                     UsrOpn Commit
004300980922    >FNdIvc01l  uf   E           K DISK    RENAME(NdIvc000:NdIvc001)
004400980922     F                                     UsrOpn Commit
004500980922    >FNdDes00f  o    E             DISK    RENAME(NdDes000:NdDes000)
004600980922     F                                     UsrOpn Commit
004700980922    >FNdDes01l  uf   E           K DISK    RENAME(NdDes000:NdDes001)
004800980922     F                                     UsrOpn Commit
004900980922    >FNdEfp00f  o    E             DISK    RENAME(NdEfp000:NdEfp000)
005000980922     F                                     UsrOpn Commit
005100980922    >FNdEfp01l  uf   E           K DISK    RENAME(NdEfp000:NdEfp001)
005200980922     F                                     UsrOpn Commit
005300980922    >FNdEfa00f  o    E             DISK    RENAME(NdEfa000:NdEfa000)
005400980922     F                                     UsrOpn Commit
005500980922    >FNdEfa01l  uf   E           K DISK    RENAME(NdEfa000:NdEfa001)
005600980922     F                                     UsrOpn Commit
005700980922    >FNdPns00f  o    E             DISK    RENAME(NdPns000:NdPns000)
005800980922     F                                     UsrOpn Commit
005900980922    >FNdPns01l  uf   E           K DISK    RENAME(NdPns000:NdPns001)
006000980922     F                                     UsrOpn Commit
006100980922    >FNdIns00f  o    E             DISK    RENAME(NdIns000:NdIns000)
006200980922     F                                     UsrOpn Commit
006300980922    >FNdIns05l  uf   E           K DISK    RENAME(NdIns000:NdIns005)
006400980922     F                                     UsrOpn Commit
006500981008    >FAnTcm00f  o    E             DISK    RENAME(AnTcm000:AnTcm000)
006600981008     F                                     UsrOpn Commit
006700981008    >FAnTcm01l  uf   E           K DISK    RENAME(AnTcm000:AnTcm001)
006800981008     F                                     UsrOpn Commit
006900981008    >FAnRcm00f  o    E             DISK    RENAME(AnRcm000:AnRcm000)
007000981008     F                                     UsrOpn Commit
007100981006    >FAnRcm01l  uf   E           K DISK    RENAME(AnRcm000:AnRcm001)
007200981008     F                                     UsrOpn Commit
007300980915      *
007400980915      * Passaggio Parametri
007500970526     D KPJBA         E DS
007600980915      *-------------
007700980915      * Dati di ambiente ottenuti da XSOC
007800970526     D SOC001        E DS                  EXTNAME(XSOC001DS)
007900980915      *-------------
008000980915      * DS Interna per dati di output di XSOC
008100970526     D XSOCDS          DS          1000
008200980915      *
008300980922     D Ndc484Ds      E DS
008400980922      *-------------
008500981008     D DimFisici       C                   Const(14)
008600020306C1507D***** DimLogici       C                   Const(14)
008700020306C1507D DimLogici       C                   Const(15)
008800980922      * Schiere file
008900980922     D Fisici          S             10    Dim(DimFisici) CTDATA PERRCD(1)
009000980922     D Logici          S             10    Dim(DimLogici) CTDATA PERRCD(1)
009100980922      *
009200980922     D  x              S              3  0
009300980922     D  WrkLibr        S             10
009400980922     D  WrkFile        S             10
009500980922     D  FlgBlock       S              1
009600980922     D  WrkSys         S                   like(RegSys)
009700980922     D  WrkNrAsReg     S                   like(RegNrAsReg)
009800980915      *
009900020306C1507D  WrkSocieta     S                   like(PasSocieta)
010000010718  "  D  WrkCtb         S                   like(PasCtb)
010100010718  "  D  WrkKcc         S                   like(PasKcc)
010200010718  "  D  WrkKsc         S                   like(PasKsc)
010300010718  "  D  WrkNrPar       S                   like(PasNrPar)
010400010718  "  D  WrkDtPar       S                   like(PasDtPar)
010500010718  "  D  WrkSerPar      S                   like(PasSerPar)
010600020306C1507 *
010700980915      *-------------
010800970526      * Reperimento nome PGM
010900970526     D STATUS         SDS           333
011000970526     D  DSPGM            *Proc
011100970526     D  Utente               254    263
011200970526      *----------------------------------------------------*
011300970526      *                MAIN LINE PROGRAM
011400970526      *----------------------------------------------------*
011500970526      *
011600970526      * Inizializzazione variabili
011700980916     C                   Exsr      InzVar
011800980915      *
011900980923     C                   Select
012000980923      * Ciclo x cancellazione
012100980923     C                   When      Ripris484 = *off
012200980923     C                   Exsr      Cancella
012300020306C1507C                   Exsr      Cancella2
012400980923      * Ciclo x ripristino
012500980923     C                   When      Ripris484 = *on
012600980923     C                   Exsr      Ripristina
012700020306C1507C                   Exsr      Cancella2
012800980923      * Errore
012900980923     C                   Other
013000980923     C                   Eval      Err484 = '1'
013100980923     C                   Exsr      EndPGM
013200980923     C                   EndSl
013300981015      *
013400981015      * Tenta la rgzpfm dei files cancellati
013500981015     C                   Exsr      CloseFiles
013600981015     C                   Exsr      DltOvr
013700041012>>>>>C**                 Exsr      OrgFiles
013800970909      *
013900970526      * fine programma
014000980915     C                   Exsr      EndPGM
014100970526      *
014200980922      ************************************************************
014300980923      * Ciclo su NdReg
014400980923      * Leggo tutte le testate registr. per soc/dtreg
014500980922      * (mi posiz. sulla data e leggo descend)
014600980922      ************************************************************
014700980923     C     Cancella      BegSr
014800980923      *
014900980923      * Imposto la data per posiz.
015000980923     C                   Eval      RegSocieta = Societa484
015100980923     C                   Eval      RegDtReg   = DtPul484
015200980923      *
015300980923     C     K02Reg02      Setgt     NdReg002
015400980922      *
015500980922     C                   Do        *hival
015600980923     C     K01Reg02      Readpe    NdReg002                               21
015700980922     C   21              Leave
015800980923      *
015900980923     C                   Eval      WrkSys     = RegSys
016000980923     C                   Eval      WrkNrAsReg = RegNrAsReg
016100980923      * Test se registraz. da non cancellare
016200980923     C     WrkKlist      Chain     NdHld001                           21
016300980923     C                   If        *in21 = *on
016400980923     C                   Exsr      DelFiles
016500980923     C                   Commit
016600980923     C                   Endif
016700980923      *
016800980923     C                   Enddo
016900980922      *
017000980922     C                   EndSr
017100980923      ************************************************************
017200980923      * Ciclo su NdReg x ripristinare
017300980923      ************************************************************
017400980923     C     Ripristina    BegSr
017500980923      *
017600980923     C     *loval        Setll     NdReg002
017700980923      *
017800980923     C                   Do        *hival
017900980923     C                   Read      NdReg002                               21
018000980923     C   21              Leave
018100980923      *
018200980923     C                   Eval      WrkSys     = RegSys
018300980923     C                   Eval      WrkNrAsReg = RegNrAsReg
018400980923     C                   Exsr      DelFiles
018500980923     C                   Commit
018600980923     C                   Enddo
018700980923      *
018800980923     C                   EndSr
018900020306C1507 ************************************************************
019000010718  "   * Ciclo x cancellare
019100010718  "   * NdPas su partite chiuse di conti di portafoglio
019200010718  "   ************************************************************
019300010718  "  C     Cancella2     BegSr
019400010718  "   *
019500010718  "  C     *loval        Setll     NdHpe000
019600010718  "   *
019700010718  "  C                   Do        *hival
019800010718  "  C                   Read      NdHpe000                               21
019900010718  "  C   21              Leave
020000010718  "   *
020100010718  "  C                   Eval      WrkSocieta = HpeSocieta
020200010718  "  C                   Eval      WrkCtb     = HpeCtb
020300010718  "  C                   Eval      WrkKcc     = HpeKcc
020400010718  "  C                   Eval      WrkKsc     = HpeKsc
020500010718  "  C                   Eval      WrkNrPar   = HpeNrPar
020600010718  "  C                   Eval      WrkDtPar   = HpeDtPar
020700010718  "  C                   Eval      WrkSerPar  = HpeSerPar
020800010718  "  C                   Exsr      DelNdPas2
020900010718  "  C                   Commit
021000010718  "  C                   Enddo
021100010718  "   *
021200020306C1507C                   EndSr
021300970526      ************************************************************
021400980923      * Guida files da cancellare
021500970526      ************************************************************
021600980923     C     DelFiles      BegSr
021700980923      *
021800980923     C                   Exsr      DelNdReg
021900980923     C                   Exsr      DelNdMov
022000980923     C                   Exsr      DelNdDes
022100980923     C                   Exsr      DelNdMoa
022200980923     C                   Exsr      DelNdPas
022300980923     C                   Exsr      DelNdBmv
022400980923     C                   Exsr      DelNdIva
022500980923     C                   Exsr      DelNdIvc
022600980923     C                   Exsr      DelNdIns
022700980923     C                   Exsr      DelNdPns
022800980923     C                   Exsr      DelNdEfp
022900980923     C                   Exsr      DelNdEfa
023000981006     C                   Exsr      DelAnTcm
023100980922      *
023200980922     C                   EndSr
023300980923      ************************************************************
023400980923      * Cancella NdReg
023500980923      ************************************************************
023600980923     C     DelNdReg      BegSr
023700980923      *
023800980923     C                   Write     NdReg000
023900980923     C                   Delete    NdReg002
024000980923      *
024100980923     C                   EndSr
024200980923      ************************************************************
024300980923      * Cancella NdMov
024400980923      ************************************************************
024500980923     C     DelNdMov      BegSr
024600980923      *
024700980923     C     WrkKlist      Setll     NdMov001
024800980923      *
024900980923     C                   Do        *hival
025000980923     C     WrkKlist      Reade     NdMov001                               21
025100980923     C   21              Leave
025200980923     C                   Write     NdMov000
025300980923     C                   Delete    NdMov001
025400980923     C                   Enddo
025500980923      *
025600980923     C                   EndSr
025700980923      ************************************************************
025800980923      * Cancella NdDes
025900980923      ************************************************************
026000980923     C     DelNdDes      BegSr
026100980923      *
026200980923     C     WrkKlist      Setll     NdDes001
026300980923      *
026400980923     C                   Do        *hival
026500980923     C     WrkKlist      Reade     NdDes001                               21
026600980923     C   21              Leave
026700980923     C                   Write     NdDes000
026800980923     C                   Delete    NdDes001
026900980923     C                   Enddo
027000980923      *
027100980923     C                   EndSr
027200980923      ************************************************************
027300980923      * Cancella NdMoa
027400980923      ************************************************************
027500980923     C     DelNdMoa      BegSr
027600980923      *
027700980923     C     WrkKlist      Setll     NdMoa001
027800980923      *
027900980923     C                   Do        *hival
028000980923     C     WrkKlist      Reade     NdMoa001                               21
028100980923     C   21              Leave
028200980923     C                   Write     NdMoa000
028300980923     C                   Delete    NdMoa001
028400980923     C                   Enddo
028500980923      *
028600980923     C                   EndSr
028700980923      ************************************************************
028800980923      * Cancella NdPas
028900980923      ************************************************************
029000980923     C     DelNdPas      BegSr
029100980923      *
029200980923     C     WrkKlist      Setll     NdPas001
029300980923      *
029400980923     C                   Do        *hival
029500980923     C     WrkKlist      Reade     NdPas001                               21
029600980923     C   21              Leave
029700980923     C                   Write     NdPas000
029800980923     C                   Delete    NdPas001
029900980923     C                   Enddo
030000980923      *
030100980923     C                   EndSr
030200020306C1507 ************************************************************
030300010718  "   * Cancella NdPas 2
030400010718  "   * Cioè movimenti di ndpas su partite chiuse di conti di portafoglio
030500010718  "   ************************************************************
030600010718  "  C     DelNdPas2     BegSr
030700010718  "   *
030800010718  "  C     WrkKPas       Setll     NdPas003
030900010718  "   *
031000010718  "  C                   Do        *hival
031100010718  "  C     WrkKPas       Reade     NdPas003                               21
031200010718  "  C   21              Leave
031300010718  "  C                   Write     NdPas000
031400010718  "  C                   Delete    NdPas003
031500010718  "  C                   Enddo
031600010718  "   *
031700020306C1507C                   EndSr
031800980923      ************************************************************
031900980923      * Cancella NdBmv
032000980923      ************************************************************
032100980923     C     DelNdBmv      BegSr
032200980923      *
032300980923     C     WrkKlist      Setll     NdBmv001
032400980923      *
032500980923     C                   Do        *hival
032600980923     C     WrkKlist      Reade     NdBmv001                               21
032700980923     C   21              Leave
032800980923     C                   Write     NdBmv000
032900980923     C                   Delete    NdBmv001
033000980923     C                   Enddo
033100980923      *
033200980923     C                   EndSr
033300980923      ************************************************************
033400980923      * Cancella NdIva
033500980923      ************************************************************
033600980923     C     DelNdIva      BegSr
033700980923      *
033800980923     C     WrkKlist      Setll     NdIva001
033900980923      *
034000980923     C                   Do        *hival
034100980923     C     WrkKlist      Reade     NdIva001                               21
034200980923     C   21              Leave
034300980923     C                   Write     NdIva000
034400980923     C                   Delete    NdIva001
034500980923     C                   Enddo
034600980923      *
034700980923     C                   EndSr
034800980923      ************************************************************
034900980923      * Cancella NdIvc
035000980923      ************************************************************
035100980923     C     DelNdIvc      BegSr
035200980923      *
035300980923     C     WrkKlist      Setll     NdIvc001
035400980923      *
035500980923     C                   Do        *hival
035600980923     C     WrkKlist      Reade     NdIvc001                               21
035700980923     C   21              Leave
035800980923     C                   Write     NdIvc000
035900980923     C                   Delete    NdIvc001
036000980923     C                   Enddo
036100980923      *
036200980923     C                   EndSr
036300980923      ************************************************************
036400980923      * Cancella NdIns
036500980923      ************************************************************
036600980923     C     DelNdIns      BegSr
036700980923      *
036800980923     C     WrkKlist      Setll     NdIns005
036900980923      *
037000980923     C                   Do        *hival
037100980923     C     WrkKlist      Reade     NdIns005                               21
037200980923     C   21              Leave
037300980923     C                   Write     NdIns000
037400980923     C                   Delete    NdIns005
037500980923     C                   Enddo
037600980923      *
037700980923     C                   EndSr
037800980923      ************************************************************
037900980923      * Cancella NdPns
038000980923      ************************************************************
038100980923     C     DelNdPns      BegSr
038200980923      *
038300980923     C     WrkKlist      Setll     NdPns001
038400980923      *
038500980923     C                   Do        *hival
038600980923     C     WrkKlist      Reade     NdPns001                               21
038700980923     C   21              Leave
038800980923     C                   Write     NdPns000
038900980923     C                   Delete    NdPns001
039000980923     C                   Enddo
039100980923      *
039200980923     C                   EndSr
039300980923      ************************************************************
039400980923      * Cancella NdEfp
039500980923      ************************************************************
039600980923     C     DelNdEfp      BegSr
039700980923      *
039800980923     C     WrkKlist      Setll     NdEfp001
039900980923      *
040000980923     C                   Do        *hival
040100980923     C     WrkKlist      Reade     NdEfp001                               21
040200980923     C   21              Leave
040300980923     C                   Write     NdEfp000
040400980923     C                   Delete    NdEfp001
040500980923     C                   Enddo
040600980923      *
040700980923     C                   EndSr
040800980923      ************************************************************
040900980923      * Cancella NdEfa
041000980923      ************************************************************
041100980923     C     DelNdEfa      BegSr
041200980923      *
041300980923     C     WrkKlist      Setll     NdEfa001
041400980923      *
041500980923     C                   Do        *hival
041600980923     C     WrkKlist      Reade     NdEfa001                               21
041700980923     C   21              Leave
041800980923     C                   Write     NdEfa000
041900980923     C                   Delete    NdEfa001
042000980923     C                   Enddo
042100980923      *
042200980923     C                   EndSr
042300981006      ************************************************************
042400981006      * Cancella AnTcm - testata commenti
042500981006      ************************************************************
042600981006     C     DelAnTcm      BegSr
042700981006      *
042800981006     C                   Eval      TcmSocieta = Societa484
042900981006     C                   Eval      TcmTpa     = 'PNO'
043000981006     C                   Movel     RegSys        TcmKy1
043100981006     C                   Movel     RegNrAsReg    TcmKy2
043200981006      *
043300981006     C     K04Tcm01      Setll     AnTcm001
043400981006      *
043500981006     C                   Do        *hival
043600981006     C     K04Tcm01      Reade     AnTcm001                               21
043700981006     C   21              Leave
043800981006      * cancella righe
043900981008     C                   Write     AnTcm000
044000981006     C                   Exsr      DelAnRcm
044100981006     C                   Delete    AnTcm001
044200981006     C                   Enddo
044300981006      *
044400981006     C                   EndSr
044500981006      ************************************************************
044600981006      * Cancella AnRcm - righe commenti
044700981006      ************************************************************
044800981006     C     DelAnRcm      BegSr
044900981006      *
045000981006     C                   Eval      RcmKcm = TcmKcm
045100981006      *
045200981006     C     K01Rcm01      Setll     AnRcm001
045300981006      *
045400981006     C                   Do        *hival
045500981006     C     K01Rcm01      Reade     AnRcm001                               21
045600981006     C   21              Leave
045700981008     C                   Write     AnRcm000
045800981006     C                   Delete    AnRcm001
045900981006     C                   Enddo
046000981006      *
046100981006     C                   EndSr
046200980922      ************************************************************
046300980922      * INIZIALIZZAZIONE VARIABILI
046400980922      ************************************************************
046500980922     C     InzVar        BegSr
046600980922      *
046700980922     C                   Movel     KPJBU         Ndc484Ds
046800980922     C                   Eval      Err484 = *off
046900970526      *
047000970526      * Reperimento dati società
047100970526      *
047200980916     C                   Movel     'SOC001'      TIPXSC
047300980922     C                   Movel     Societa484    SOCXSC
047400980915     C                   Exsr      REPSOC
047500980916     C     RTNXSC        Ifne      '1'
047600980916     C                   Movel     XSOCDS        SOC001
047700980916     C                   Else
047800980917      * Errore
047900980922     C                   Eval      Err484 = '1'
048000980915     C                   Exsr      EndPGM
048100980916     C                   Endif
048200980922      * Override
048300980922     C                   Exsr      Override
048400980923      * Open
048500980923     C                   Exsr      OpenFiles
048600980917      *
048700980916     C                   EndSr
048800980915      ************************************************************
048900980915      * Reperimento dati società
049000980915      ************************************************************
049100980917     C     RepSoc        BegSr
049200980915      *
049300980915     C                   CALLB     'XSOC'
049400980917     C                   Parm                    TIPXSC            6
049500980917     C                   Parm                    SOCXSC            3
049600980917     C                   Parm                    CDSXSC            9 0
049700980917     C                   Parm                    MODXSC            3
049800980917     C                   Parm      *BLANKS       RTNXSC            1
049900980917     C                   Parm                    XSOCDS
050000980917     C                   Parm                    KPJBA
050100980915      *
050200980916     C                   EndSr
050300980915      ************************************************************
050400980915      * FINE PROGRAMMA
050500980915      ************************************************************
050600980916     C     EndPGM        BegSr
050700980915      *
050800980922     C                   Eval       Kpjbu = Ndc484Ds
050900980917     C                   Eval       *inlr = *on
051000980917     C                   Return
051100980915      *
051200980916     C                   EndSr
051300980922      ************************************************************
051400980922      * Override files
051500980922      ************************************************************
051600980922     C     Override      BegSr
051700980922      *
051800980922      * NdHld01l
051900980922     C                   Eval      WrkLibr = Libr484
052000980922     C                   Eval      WrkFile = 'NdHld01l'
052100980922     C                   Eval      FlgBlock = *off
052200980922     C                   Exsr      OvrFile
052300980922     C                   Open      NdHld01l
052400020306C1507 *
052500010718  "   * NdHpe00f
052600010718  "  C                   Eval      WrkLibr = Libr484
052700010718  "  C                   Eval      WrkFile = 'NdHpe00f'
052800010718  "  C                   Eval      FlgBlock = *off
052900010718  "  C                   Exsr      OvrFile
053000020306C1507C                   Open      NdHpe00f
053100980922      *
053200980922      * Prima schiera (file fisici)
053300980922     C                   If        Ripris484 = *off
053400980922     C                   Eval      WrkLibr = Libr484
053500980922     C                   Else
053600980922     C                   Eval      WrkLibr = '*LIBL'
053700980922     C                   Endif
053800980922      *
053900980922     C                   Do        DimFisici     x
054000980922     C                   Eval      WrkFile = Fisici(x)
054100980922     C                   Eval      FlgBlock = *on
054200980922     C                   Exsr      OvrFile
054300980922     C                   Enddo
054400980922      *
054500980922      * Seconda schiera (file logici)
054600980922     C                   If        Ripris484 = *off
054700980922     C                   Eval      WrkLibr = '*LIBL'
054800980922     C                   Else
054900980922     C                   Eval      WrkLibr = Libr484
055000980922     C                   Endif
055100980922      *
055200980922     C                   Do        DimLogici     x
055300980922     C                   Eval      WrkFile = Logici(x)
055400980922     C                   Eval      FlgBlock = *off
055500980922     C                   Exsr      OvrFile
055600980922     C                   Enddo
055700980922      *
055800980922     C                   EndSr
055900980922      ************************************************************
056000980922      * Override
056100980922      ************************************************************
056200980922     C     OvrFile       BegSr
056300980922      *
056400980922      * OvrDbf
056500980922     C                   Clear                   Cmd
056600980922     C     'OvrDbf'      Cat       'File(':1     Cmd
056700980922     C                   Cat       WrkFile:0     Cmd
056800980922     C                   Cat       ')':0         Cmd
056900980922      *
057000980922     C                   Cat       'Tofile(':1   Cmd
057100980922     C                   Cat       WrkLibr:0     Cmd
057200980922     C                   Cat       '/':0         Cmd
057300980922     C                   Cat       WrkFile:0     Cmd
057400980922     C                   Cat       ')':0         Cmd
057500980922      *
057600980922     C                   If        FlgBlock = *on
057700980922     C                   Cat       'SEQONLY(':1  Cmd
057800980922     C                   Cat       '*YES 1000)':0Cmd
057900980922     C                   Endif
058000980922      *
058100980922     C                   Call      'QCMDEXC'                            21
058200980922     C                   Parm                    Cmd              70
058300980922     C                   Parm      70            LeN              15 5
058400980922      *
058500980922     C                   If        *In21 = *on
058600980922     C                   Eval      Err484 = '1'
058700980922     C                   Exsr      EndPgm
058800980922     C                   Endif
058900980922      *
059000980922     C                   EndSr
059100980915      ************************************************************
059200980922      * Open
059300980915      ************************************************************
059400980922     C     OpenFiles     BegSr
059500980915      *
059600980922     C                   Open      NdReg00f
059700980922     C                   Open      NdMoa00f
059800980922     C                   Open      NdMov00f
059900980922     C                   Open      NdPas00f
060000980922     C                   Open      NdBmv00f
060100980922     C                   Open      NdDes00f
060200980922     C                   Open      NdIva00f
060300980922     C                   Open      NdIvc00f
060400980922     C                   Open      NdIns00f
060500980922     C                   Open      NdPns00f
060600980922     C                   Open      NdEfa00f
060700980922     C                   Open      NdEfp00f
060800981008     C                   Open      AnTcm00f
060900981008     C                   Open      AnRcm00f
061000980922      *
061100980923     C                   Open      NdReg02l
061200980922     C                   Open      NdMoa01l
061300980922     C                   Open      NdMov01l
061400980922     C                   Open      NdPas01l
061500980922     C                   Open      NdBmv01l
061600980922     C                   Open      NdDes01l
061700980922     C                   Open      NdIva01l
061800980922     C                   Open      NdIvc01l
061900980922     C                   Open      NdIns05l
062000980922     C                   Open      NdPns01l
062100980922     C                   Open      NdEfa01l
062200980922     C                   Open      NdEfp01l
062300981008     C                   Open      AnTcm01l
062400981008     C                   Open      AnRcm01l
062500020306C1507C                   Open      NdPas03l
062600980922      *
062700980922     C                   EndSr
062800981015      ************************************************************
062900981015      * Close - Non obblig. il pgm comunque chiude in lr
063000981015      ************************************************************
063100981015     C     CloseFiles    BegSr
063200981015      *
063300981015     C                   Close     NdReg00f
063400981015     C                   Close     NdMoa00f
063500981015     C                   Close     NdMov00f
063600981015     C                   Close     NdPas00f
063700981015     C                   Close     NdBmv00f
063800981015     C                   Close     NdDes00f
063900981015     C                   Close     NdIva00f
064000981015     C                   Close     NdIvc00f
064100981015     C                   Close     NdIns00f
064200981015     C                   Close     NdPns00f
064300981015     C                   Close     NdEfa00f
064400981015     C                   Close     NdEfp00f
064500981015     C                   Close     AnTcm00f
064600981015     C                   Close     AnRcm00f
064700981015      *
064800981015     C                   Close     NdReg02l
064900981015     C                   Close     NdMoa01l
065000981015     C                   Close     NdMov01l
065100981015     C                   Close     NdPas01l
065200981015     C                   Close     NdBmv01l
065300981015     C                   Close     NdDes01l
065400981015     C                   Close     NdIva01l
065500981015     C                   Close     NdIvc01l
065600981015     C                   Close     NdIns05l
065700981015     C                   Close     NdPns01l
065800981015     C                   Close     NdEfa01l
065900981015     C                   Close     NdEfp01l
066000981015     C                   Close     AnTcm01l
066100981015     C                   Close     AnRcm01l
066200981015      *
066300981015     C                   EndSr
066400981015      ************************************************************
066500981015      * RgzPfm files cancellati
066600981015      ************************************************************
066700981015     C     OrgFiles      BegSr
066800981015      *
066900981015      * Schiera file fisici
067000981015     C                   If        Ripris484 = *off
067100981015     C                   Eval      WrkLibr = '*LIBL'
067200981015     C                   Else
067300981015     C                   Eval      WrkLibr = Libr484
067400981015     C                   Endif
067500981015      *
067600981015     C                   Do        DimFisici     x
067700981015     C                   Eval      WrkFile = Fisici(x)
067800981015     C                   Eval      FlgBlock = *on
067900981015     C                   Exsr      RgzFile
068000981015     C                   Enddo
068100981015      *
068200981015     C                   EndSr
068300981015      ************************************************************
068400981015      * RgzPfm
068500981015      ************************************************************
068600981015     C     RgzFile       BegSr
068700981015      *
068800981015      * RgzPfm
068900981015     C                   Clear                   Cmd
069000981015     C     'RgzPfm'      Cat       'File(':1     Cmd
069100981015     C                   Cat       WrkLibr:0     Cmd
069200981015     C                   Cat       '/':0         Cmd
069300981015     C                   Cat       WrkFile:0     Cmd
069400981015     C                   Cat       ')':0         Cmd
069500981015      *
069600981015     C                   Call      'QCMDEXC'                            21
069700981015     C                   Parm                    Cmd              70
069800981015     C                   Parm      70            LeN              15 5
069900981015      *
070000981015      * Se 21 = *on non faccio nulla
070100981015      *
070200981015     C                   EndSr
070300981015      ************************************************************
070400981015      * DltOvr *all
070500981015      ************************************************************
070600981015     C     DltOvr        BegSr
070700981015      *
070800981015     C                   Clear                   Cmd
070900981015     C     'DltOvr'      Cat       'File(':1     Cmd
071000981015     C                   Cat       '*all)':0     Cmd
071100981015      *
071200981015     C                   Call      'QCMDEXC'                            21
071300981015     C                   Parm                    Cmd              70
071400981015     C                   Parm      70            LeN              15 5
071500981015      *
071600981015      * Se 21 = *on non faccio nulla
071700981015      *
071800981015     C                   EndSr
071900980922      ************************************************************
072000980922      * OPERAZIONI INIZIALI
072100980922      ************************************************************
072200980922     C     *INZSR        BegSr
072300980922      *
072400980915     C     *ENTRY        PLIST
072500980917     C                   Parm                    Kpjba
072600980923      *
072700980923      * Eseguo strcmtctl A LIVELLO DI ACTGROUP (che è il default)
072800980923      *
072900980923     C                   CallB     'XSTRCMT'
073000980915      *
073100980916     C                   EndSr
073200980915      ************************************************************
073300980915      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
073400980915      ************************************************************
073500980916     C     DEFCAM        BegSr
073600980915      *
073700980915      * klist
073800980915      *
073900980923      * NdReg02l
074000980923     C     K01Reg02      KLIST
074100980922     C                   KFLD                    RegSocieta
074200980922      *
074300980923     C     K02Reg02      KLIST
074400980922     C                   KFLD                    RegSocieta
074500980922     C                   KFLD                    RegdtReg
074600980922      * Klist di work
074700980922     C     WrkKlist      KLIST
074800980922     C                   KFLD                    WrkSys
074900980922     C                   KFLD                    WrkNrAsReg
075000981006      * AnTcm00f
075100981006     C     K04Tcm01      KLIST
075200981006     C                   KFLD                    TcmSocieta
075300981006     C                   KFLD                    TcmTpa
075400981006     C                   KFLD                    TcmKy1
075500981006     C                   KFLD                    TcmKy2
075600981006      * AnRcm00f
075700981006     C     K01Rcm01      KLIST
075800981006     C                   KFLD                    RcmKcm
075900020306C1507 * NdPas03l
076000010718  "  C     WrkKPas       KList
076100010718  "  C                   KFld                    WrkSocieta
076200010718  "  C                   KFld                    WrkCtb
076300010718  "  C                   KFld                    WrkKcc
076400010718  "  C                   KFld                    WrkKsc
076500010718  "  C                   KFld                    WrkDtPar
076600010718  "  C                   KFld                    WrkNrPar
076700020306C1507C                   KFld                    WrkSerPar
076800010716      *
076900980916     C                   EndSr
077000981008**ctdata fisici
077100980922NDREG00F
077200980922NDMOV00F
077300980922NDMOA00F
077400980922NDPAS00F
077500980922NDBMV00F
077600980922NDIVA00F
077700980922NDEFP00F
077800980922NDEFA00F
077900980922NDDES00F
078000980922NDIVC00F
078100980922NDINS00F
078200980922NDPNS00F
078300981008ANTCM00F
078400981008ANRCM00F
078500981008**ctdata logici
078600980923NdReg02L
078700980922NDMOV01L
078800980922NDMOA01L
078900980922NDPAS01L
079000980922NDBMV01L
079100980922NDIVA01L
079200980922NDEFP01L
079300980922NDEFA01L
079400980922NDDES01L
079500980922NDIVC01L
079600980922NDINS05L
079700980922NDPNS01L
079800981008ANTCM01L
079900981008ANRCM01L
080000010716NDPAS03L

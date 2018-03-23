000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020116      * TNVR23R *----------------------------------------------------*
000300020114      * RINUMERAZIONE CODICE VISITE E FILES COLLEGATI
000400020114      *--------------------------------------------------------------*
000500020115      *
000600020117      * Visite clienti S.I Bartolini e SDI
000700020124     FTnVisBis  IF   E           K DISK    rename(TnVis000:TnVis01)
000800020117     F                                     prefix(B_)
000900020117      *
001000020117      * Anagrafiche provvisorie
001100020124     FTfAcoBis  IF   E           K DISK    rename(CnAco000:CnAco01)
001200020117     F                                     prefix(B_)
001300020124     FTfIndBis  IF   E           K DISK    rename(CnInd000:CnInd01)
001400020117     F                                     prefix(B_)
001500020124     FTfClpBis  IF   E           K DISK    rename(CnClp000:CnClp01)
001600020117     F                                     prefix(B_)
001700020117      *
001800020117      * Offerte
001900020124     FTnOfmBis  IF   E           K DISK    rename(TnTam000:TnTam01)
002000020117     F                                     prefix(B_)
002100020124     FTiOfdBis  IF   E           K DISK    rename(TiTad000:TiTad01)
002200020117     F                                     prefix(B_)
002300020124     FTiOptBis  IF   E           K DISK    rename(TiTpt000:TiTpt01)
002400020117     F                                     prefix(B_)
002500020124     FTiOpdBis  IF   E           K DISK    rename(TiTpd000:TiTpd01)
002600020117     F                                     prefix(B_)
002700020124     FTiOgcBis  IF   E           K DISK    rename(TiTgc000:TiTgc01)
002800020117     F                                     prefix(B_)
002900020117      *
003000020118     FAzOrg01l  IF   E           K DISK
003100020308      * Numeratori vecchi
003200020308     FFlnuf01l  IF   E           K DISK
003300020115      *
003400020115      * File contatori filiale
003500020131     FAzNum01L  UF A E           K DISK
003600020117      *
003700020117      * ARCHIVI DA SCRIVERE NELLA LIBRERIA DI GRUPPO
003800020117      *
003900020117      * Visite
004000020129     FTnVis05l  IF A E           K DISK
004100020115      *
004200020111      * Anagrafiche
004300020118     FTfAco00F  O    E             DISK
004400020118     FTfInd00F  O    E             DISK
004500020118     FTfClp00F  O    E             DISK
004600020115      *
004700020111      * Offerte
004800020118     FTnOfm00F  O    E             DISK
004900020118     FTiOfd00f  O    E             DISK
005000020118     FTiOpt00f  O    E             DISK
005100020118     FTiOpd00f  O    E             DISK
005200020118     FTiOgc00f  O    E             DISK
005300020115      *
005400020111      * Note
005500020306     FTfNtc01l  UF   E           K DISK
005600020115      *
005700020111      * Cat
005800020306     FTeEtc01l  UF   E           K DISK
005900020306     FTeEts01l  UF   E           K DISK
006000020306     FTeEtt01l  UF   E           K DISK
006100020128      *
006200020128      * Clienti potenziali
006300020128     FTnCps04l  UF   E           K DISK
006400020115      *
006500020115      * File con vecchi e nuovi numeri visita
006600020308     FTnWNr00f  IF A E           K DISK
006700020118      *
006800020123     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
006900020115      *----------------------------------------------------------------
007000020125     D                 DS
007100020125     D  VisOld                 1      7  0
007200020125     D  CodOld                 1      5  0
007300020125     D  PrgOld                 6      7  0
007400020125      *
007500020125     D                 DS
007600020125     D  VisNew                 1      7  0
007700020125     D  CodNew                 1      6  0
007800020125     D  PrgNew                 7      7  0
007900020125      *
008000020125     D $Vis            DS
008100020125     D  $Cod                   1      5  0
008200020125     D  $Prg                   6      7  0
008300020117      *
008400020123      * Data corrente
008500020123     D DtCorrente      s               d
008600020131      *
008700020131     D wlbda8          DS                  INZ                                  *controlla data
008800020131     D  g08dat                        8  0
008900020131     D  g08inv                        8  0
009000020131     D  g08err                        1
009100020131     D  g08tgi                        5  0
009200020131      *
009300020131     D n14             S             14  0 INZ                                  *numerico 14
009400020131     D n8              S              8  0 INZ                                  *numerico 8
009500020123      *
009600020117     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009700020117     D  TCU                  398    697
009800020117     D                                     DIM(50)
009900020117     D  KCU                  698    847P 0
010000020117     D                                     DIM(50)
010100020117      *
010200020117     D TCUDS           DS
010300020117     D  F1                     1      1
010400020117     D  F3                     3      3
010500020117     D  F2                     2      2
010600020117     D  F4                     4      4
010700020117     D  F56                    5      6
010800020118      *
010900020118     D DsVis         E DS                  EXTNAME(TnVis00F)
011000020118     D DsVisB        E DS                  EXTNAME(TnVis00F) PREFIX(B_)
011100020118      *
011200020118     D DsAco         E DS                  EXTNAME(TfAco00F)
011300020118     D DsAcoB        E DS                  EXTNAME(TfAco00F) PREFIX(B_)
011400020118      *
011500020118     D DsInd         E DS                  EXTNAME(TfInd00F)
011600020118     D DsIndB        E DS                  EXTNAME(TfInd00F) PREFIX(B_)
011700020118      *
011800020118     D DsClp         E DS                  EXTNAME(TfClp00F)
011900020118     D DsClpB        E DS                  EXTNAME(TfClp00F) PREFIX(B_)
012000020118      *
012100020118     D DsOfm         E DS                  EXTNAME(TnOfm00F)
012200020118     D DsOfmB        E DS                  EXTNAME(TnOfm00F) PREFIX(B_)
012300020118      *
012400020118     D DsOfd         E DS                  EXTNAME(TiOfd00F)
012500020118     D DsOfdB        E DS                  EXTNAME(TiOfd00F) PREFIX(B_)
012600020118      *
012700020118     D DsOpt         E DS                  EXTNAME(Tiopt00F)
012800020118     D DsOptB        E DS                  EXTNAME(Tiopt00F) PREFIX(B_)
012900020118      *
013000020118     D DsOpd         E DS                  EXTNAME(Tiopd00F)
013100020118     D DsOpdB        E DS                  EXTNAME(Tiopd00F) PREFIX(B_)
013200020118      *
013300020118     D DsOgc         E DS                  EXTNAME(Tiogc00F)
013400020118     D DsOgcB        E DS                  EXTNAME(Tiogc00F) PREFIX(B_)
013500020118      *
013600921027     C*****************************************************************
013700020131     C     *ENTRY        PLIST                                                  *
013800020131     C                   PARM                    $Print            1            *
013900020118      *
014000020125      * Routine di reperimento capoconto
014100020125     C                   EXSR      REPKCC
014200020111      *
014300020124      * CICLO DI LETTURA SU FILE VISITE
014400020117      *
014500020125     C                   move      *zeros        CodOld
014600020125     C                   move      *zeros        PrgOld
014700020125     C                   move      *zeros        CodNew
014800020125     C                   move      *zeros        PrgNew
014900020125      *
015000020124     C     *loval        setll     TnVisBis
015100020124     C                   read      TnVisBis                               30
015200020118      *
015300020114      * 30 on - fine lettura
015400020115     C     *in30         doweq     '0'
015500020115      *
015600020118      * Mi salvo il numero visita letto
015700020125     C                   move      B_VisNrv      $Vis
015800020125      *
015900020125      * Sono a rottura di codice, quindi elaboro nuovo codice
016000020125     C     $Cod          ifne      CodOld
016100020125      *
016200020125     C                   EXSR      REPCOD
016300020125      *
016400020125     C                   endif
016500020125      *
016600020125      * Elaboro il progressivo
016700020125     C                   EXSR      REPPRG
016800020117      *
016900020124      * Scrivo nella lib. di gruppo i records di TnVisBis e files collegati
017000020125     C                   EXSR      WRTANACOLL
017100020117      *
017200020125      * Scrivi File Storico ddei numeri visita
017300020125     C                   EXSR      FILESTORICO
017400020115      *
017500020124     C                   read      TnVisBis                               30
017600020115     C                   Enddo
017700020117      *
017800020114      *
017900020131      * Stampo il file Storico delle visite solo alla fine
018000020131     C     $Print        ifeq      'P'
018100020125     C                   EXSR      PRINT
018200020131     C                   endif
018300020115      *
018400020115     C                   SETON                                        LR
018500020114      *
018600020115      *---------------------------------------------------------------*
018700020125      * Routine di reperimento codice corretto da utilizzare.
018800020115      *---------------------------------------------------------------*
018900020125     C     RepCod        BegSr
019000020128      *
019100020128      * Mi salvo il codice letto
019200020129     C                   move      $Cod          CodOld
019300020128      *
019400020128      * Indicatore acceso se devo cercare un nuovo codice
019500020128     C                   move      *on           $Cerca_NewCod     1
019600020115      *
019700020115      * Ciclo sino a che non trovo un nuovo numero valido
019800020129     C     $Cod          mult      10            $Cod7             7 0
019900020129     C     $Cod7         setll     TnVis05l
020000020128     C                   read      TnVis05l                               22
020100020128     C                   if        *in22 = *on or  *in22 = *off and
020200020129     C                             (VisNrv-$Cod7)>9
020300020128      *
020400020128      * Ho un codice valido che valorizzo in CodNew
020500020128     C     $Cod7         div       10            CodNew
020600020128     C                   move      *off          $Cerca_NewCod
020700020125     C                   endif
020800020128      *
020900020128     C                   if        $Cerca_NewCod = *on
021000020128     C                   setoff                                       50
021100020206      *
021200020206      * Valorizzo chiave
021300020206     C                   move      *blanks       NumSoc
021400020206     C                   move      *zeros        NumAaa
021500020206     C                   z-add     1             NumCnu
021600020125      *
021700020125      * Ciclo sino a che non trovo un nuovo numero valido
021800020128     C     *in50         doweq     *off
021900020115      *
022000020115     C     Knum          chain     AzNum01l                           21
022100020131      * Se non esiste creo l atabella
022200020131     C     *in21         ifeq      *on
022300020131     C*
022400020131     C* reperisce la data corrente
022500020131     C                   TIME                    n14                            *Ora (6)+ Data (8)
022600020131     C                   MOVE      n14           n8                             *Data (8) in g/m/a
022700020131     C                   Z-ADD     n8            g08dat
022800020131     C                   Z-ADD     *zeros        g08inv
022900020131     C                   MOVEL     '0'           g08err
023000020131     C                   CALL      'XSRDA8'
023100020131     C                   PARM                    wlbda8
023200020131     C                   Z-ADD     g08inv        NumDac                         *Data corrente a/m/g
023300020308     C*
023400020308     C* recupero ultimo numero da flnuf
023500020308     C                   move      n8            NufAAA
023600020308     C                   z-add     1             NufCnu
023700020308     C     KNuf          chain     FlNuf                              30
023800020308     C     *in30         ifeq      *off
023900020308     C     NufNum        div       10            NumUnp
024000020308     C                   else
024100020308     C                   z-add     10            NumUnp
024200020308     C                   endif
024300020131     C                   write     AzNum000
024400020131      *
024500020131     C     Knum          chain     AzNum01l                           21
024600020131     C                   endif
024700020117     C                   add       10            NumUnp
024800020308      *
024900020308      * testo se sono sono tornata a 0
025000020308     C     NumUnp        ifeq      0
025100020308     C* O MAGGIORE DI 9999990 RIPARTO DA 10
025200020308     C     NUMUNP        ORGT      9999990
025300020308     C                   z-add     10            NumUnp
025400020308     C                   endif
025500020308      *
025600020117     C                   update    AzNum000
025700020125      *
025800020129     C                   z-add     NumUnp        $Cod7
025900020129     C     $Cod7         setll     TnVis05l
026000020128     C                   read      TnVis05l                               50
026100020128     C                   if        *in50 = *on or  *in50 = *off and
026200020128     C                             (VisNrv-$Cod7)>9
026300020128      *
026400020128      * Ho un codice valido che valorizzo in CodNew
026500020125     C     NumUnp        div       10            CodNew
026600020129     C                   leave
026700020125     C                   endif
026800020115     C                   EndDo
026900020128     C                   Endif
027000020128      *
027100020128      * Azzero il valore del progressivo
027200020128     C                   z-add     0             PrgNew
027300020115      *
027400020115     C                   EndSr
027500020125      *---------------------------------------------------------------*
027600020125      * Routine di reperimento codice progressivo corretto da utilizzare
027700020125      *---------------------------------------------------------------*
027800020125     C     RepPrg        BegSr
027900020125      *
028000020129     C                   move      $Prg          PrgOld
028100020129      *
028200020125      * Devo valorizzare PrgNew con il progressivo corretto da utilizzare
028300020125     C     PrgOld        ifeq      0
028400020125     C                   z-add     0             PrgNew
028500020125     C                   else
028600020125     C                   add       1             PrgNew
028700020125     C                   endif
028800020125      *
028900020125     C                   EndSr
029000020115      *---------------------------------------------------------------*
029100020117      * Routine di scrittura dei files in libreria di gruppo
029200020115      *---------------------------------------------------------------*
029300020125     C     WrtAnaColl    BegSr
029400020117      *
029500020117      * Valorizzo i campi chiave
029600020117     C                   z-add     1             $kut
029700020117     C                   z-add     Kci           $Kcc
029800020125     C                   z-add     VisOld        $Ksc
029900020117     C                   move      'V'           NtcApl
030000020117     C                   move      Kci           Capo_ita          4
030100020117     C                   movel     Capo_ita      NtcNk1
030200020125     C                   move      VisOld        NtcNk1
030300020117     C                   move      'F'           $Dsf
030400020117     C                   move      'O'           $Cto
030500020128     C     B_VisCmm      div       10000         $Pou              3 0
030600020117      *
030700020117      * File visite
030800020125     C     VisOld        chain     TnVisBis                           21
030900020117     C     *in21         ifeq      *off
031000020118     C                   eval      DsVis = DsVisB
031100020125     C                   z-add     VisNew        VisNrv
031200020117     C                   write     TnVis000
031300020117     C                   endif
031400020115      *
031500020115      * Anagrafiche
031600020115      *
031700020124     C     KAco          chain     TfAcoBis                           21
031800020117     C     *in21         ifeq      *off
031900020118     C                   eval      DsAco = DsAcoB
032000020125     C                   z-add     VisNew        AcoKsc
032100020117     C                   write     CnAco000
032200020117     C                   endif
032300020117      *
032400020124     C     KAco          chain     TfIndBis                           21
032500020117     C     *in21         ifeq      *off
032600020118     C                   eval      DsInd = DsIndB
032700020125     C                   z-add     VisNew        IndKsc
032800020117     C                   write     CnInd000
032900020117     C                   endif
033000020117      *
033100020124     C     KAco          chain     TfClpBis                           21
033200020117     C     *in21         ifeq      *off
033300020118     C                   eval      DsClp = DsClpB
033400020125     C                   z-add     VisNew        ClpKsc
033500020117     C                   write     CnClp000
033600020117     C                   endif
033700020115      *
033800020115      * Offerte
033900020117      *
034000020125     C     VisOld        setll     TnOfmBis
034100020125     C     VisOld        reade     TnOfmBis                               21
034200020115     C     *in21         doweq     *off
034300020118     C                   eval      DsOfm = DsOfmB
034400020125     C                   z-add     VisNew        TamKsc
034500020117     C                   write     TnTam000
034600020125     C     VisOld        reade     TnOfmBis                               21
034700020115     C                   enddo
034800020117      *
034900020125     C     VisOld        setll     TiOfdBis
035000020125     C     VisOld        reade     TiOfdBis                               21
035100020117     C     *in21         doweq     *off
035200020118     C                   eval      DsOfd = DsOfdB
035300020125     C                   z-add     VisNew        TadKsc
035400020117     C                   write     TiTad000
035500020125     C     VisOld        reade     TiOfdBis                               21
035600020117     C                   enddo
035700020117      *
035800020125     C     VisOld        setll     TiOptBis
035900020125     C     VisOld        reade     TiOptBis                               21
036000020117     C     *in21         doweq     *off
036100020118     C                   eval      DsOpt = DsOptB
036200020125     C                   z-add     VisNew        Tptksc
036300020117     C                   write     TitPt000
036400020125     C     VisOld        reade     TiOptBis                               21
036500020117     C                   enddo
036600020117      *
036700020125     C     VisOld        setll     TiOpdBis
036800020125     C     VisOld        reade     TiOpdBis                               21
036900020117     C     *in21         doweq     *off
037000020118     C                   eval      DsOpd = DsOpdB
037100020125     C                   z-add     VisNew        TpdKsc
037200020117     C                   write     TiTpd000
037300020125     C     VisOld        reade     TiOpdBis                               21
037400020117     C                   enddo
037500020117      *
037600020125     C     VisOld        setll     TiOgcBis
037700020125     C     VisOld        reade     TiOgcBis                               21
037800020117     C     *in21         doweq     *off
037900020118     C                   eval      DsOgc = DsOgcB
038000020125     C                   z-add     VisNew        TgcKsc
038100020117     C                   write     TiTgc000
038200020125     C     VisOld        reade     TiOgcBis                               21
038300020117     C                   enddo
038400020115      *
038500020115      * Note
038600020306     C     Kntc          setll     TfNtc01l
038700020306     C     Kntc          reade     TfNtc01l                               21
038800020306     C     *in21         doweq     *off
038900020306     C                   move      VisNew        NtcNk1
039000020306     C                   update    TfNtc
039100020306     C     Kntc          reade     TfNtc01l                               21
039200020306     C                   enddo
039300020117      *
039400020115      *
039500020115      * Cat
039600020306     C     ketc          setll     Teetc01l
039700020306     C     Ketc          reade     Teetc01l                               21
039800020306     C     *in21         doweq     *off
039900020306     C                   z-add     VisNew        EtcKsc
040000020306     C                   move      ' '           EtcFtr
040100020306     C                   update    Teetc000
040200020306     C     Ketc          reade     Teetc01l                               21
040300020306     C                   enddo
040400020306      *
040500020306     C     ketc          setll     Teets01l
040600020306     C     Ketc          reade     Teets01l                               21
040700020306     C     *in21         doweq     *off
040800020306     C                   z-add     VisNew        EtsKsc
040900020306     C                   move      ' '           EtsFtr
041000020306     C                   update    Teets000
041100020306     C     Ketc          reade     Teets01l                               21
041200020306     C                   enddo
041300020306      *
041400020306     C     ketc          setll     Teett01l
041500020306     C     Ketc          reade     Teett01l                               21
041600020306     C     *in21         doweq     *off
041700020306     C                   z-add     VisNew        EttKsc
041800020306     C                   move      ' '           EttFtr
041900020306     C                   update    Teett000
042000020306     C     Ketc          reade     Teett01l                               21
042100020306     C                   enddo
042200020117      *
042300020128      *
042400020128      * Clienti potenziali
042500020128      *
042600020128     C     $Ksc          setll     TnCps04l
042700020128     C     $ksc          reade     Tncps04l                               21
042800020128     C     *in21         doweq     *off
042900020128      * Confronto i due P.O
043000020128     C
043100020128     C     CpsCmm        div       10000         $Pou1             3 0
043200020128     C     $Pou          ifeq      $Pou1
043300020128     C                   z-add     VisNew        CpsNrv
043400020128     C                   move      ' '           CpsFtr
043500020128     C                   update    Tncps000
043600020128     C                   endif
043700020128     C     $Ksc          reade     TnCps04l                               21
043800020128     C                   enddo
043900020128      *
044000020117      *
044100020117      *
044200020115     C                   EndSr
044300020115      *---------------------------------------------------------------*
044400020117      * Routine di scrittura file con vecchi e nuovi numeri
044500020115      *---------------------------------------------------------------*
044600020125     C     FileStorico   BegSr
044700020125      *
044800020125      * Se ho cambiato numero visita o progressivo scrivo lo storico dei
044900020125      * numeratori.
045000020125     C     CodNew        ifne      CodOld
045100020125     C     CodNew        oreq      CodOld
045200020125     C     PrgNew        andne     PrgOld
045300020125      *
045400020115      *
045500020128      * Da codice commerciale prelevo P.O
045600020128     C     B_VisCmm      div       10000         WNrPou
045700020128     C                   z-add     VisOld        WNrOld
045800020128     C                   z-add     VisNew        WNrNew
045900020128     C                   write     TnWNr000
046000020125     C                   endif
046100020115      *
046200020115     C                   EndSr
046300020115      *---------------------------------------------------------------*
046400020115      * Routine di stampa FnVis00f
046500020115      *---------------------------------------------------------------*
046600020115     C     Print         BegSr
046700020131      *
046800020131     C                   z-add     0             $WPou             3 0
046900020115      *
047000020131      * Stampo la testata a rottura di P.O e a fine pagina.
047100020128     C                   read      TnWNr00f                               21
047200020128     C     *in21         doweq     *off
047300020131      *
047400020131     C     $WPou         ifne      WNrPou
047500020131     C                   z-add     WNrPou        $WPou
047600020131     C                   setoff                                       77
047700020131     C                   else
047800020131     C                   seton                                        77
047900020131     C                   endif
048000020115      * stampo
048100020128     C   OF
048200020128     CORN77              except    TesVis
048300020128     C  N77              seton                                        77
048400020128     C
048500020128     C                   except    DetVis
048600020128     C                   read      TnWNr00f                               21
048700020128     C                   enddo
048800020115      *
048900020115     C                   EndSr
049000020125      *---------------------------------------------------------------*
049100020125      * Routine di reperimento capoconto
049200020125      *---------------------------------------------------------------*
049300020125     C     RepKcc        BegSr
049400020125     C                   Z-ADD     1             CODUT
049500020125     C                   CALL      'X§PARUT'
049600020125     C                   PARM                    UT§DSE
049700020125      *
049800020125      *    Ricerca capoconto Italia
049900020125      *
050000020125     C                   Do        50            X                 2 0
050100020125     C                   move      TCU(X)        TCUDS
050200020125     C     F56           ifeq      'CG'
050300020125     C     F4            andeq     '1'
050400020125     C     F3            andeq     '0'
050500020125     C                   z-add     KCU(X)        KCI               4 0
050600020125     C                   leave
050700020125     C                   endif
050800020125     C                   Enddo
050900020125      *
051000020125     C                   EndSr
051100020115      *---------------------------------------------------------------*
051200020114      *
051300020114      * DEFINIZIONE CAMPI
051400020115      *
051500020125      *---------------------------------------------------------------*
051600020115     C     *like         Define    AcoKut        $Kut
051700020115     C     *like         Define    AcoKcc        $Kcc
051800020115     C     *like         Define    AcoKsc        $Ksc
051900020115     C     *like         Define    EtcDsf        $Dsf
052000020115     C     *like         Define    EtcCto        $Cto
052100020111      *
052200020115      *Cnaco
052300020115     C     KAco          Klist
052400020115     C                   Kfld                    $Kut
052500020115     C                   Kfld                    $Kcc
052600020115     C                   Kfld                    $Ksc
052700020115      *Teetc
052800020115     C     KEtc          Klist
052900020115     C                   Kfld                    $Dsf
053000020115     C                   Kfld                    $Cto
053100020115     C                   Kfld                    $Ksc
053200020111      *
053300020115     C     KNum          Klist
053400020206     C                   Kfld                    NumSOC
053500020206     C                   Kfld                    NumAaa
053600020115     C                   Kfld                    NumCnu
053700020115      *Tfntc
053800020115     C     KNtc          Klist
053900020115     C                   Kfld                    NtcApl
054000020115     C                   Kfld                    NtcNk1
054100020308      *Flnuf
054200020308     C     KnUf          Klist
054300020308     C                   Kfld                    NufAAA
054400020308     C                   Kfld                    NUFCnu
054500020118     OQSYSPRT   E            TESVIS         2 02
054600020131     O                                         +  2 'RINUMERAZIONE'
054700020118     O                                         +  1 'VISITE/OFFERTE'
054800020305     O                                         +  2 'DEL P.O. .......:'
054900020131     O                       WNrPou            +  1
055000020118     O                                         +  5 'TNVR23R'
055100020129     O                       UDATE             +  2 '  /  /  '
055200020118     O                                         +  5 'PAG.'
055300020118     O                       PAGE1         Z
055400020118     O          E            TESVIS         2
055500020129     O                                         +  5 'PUNTO OPERATIVO'
055600020129     O                                         +  3 'NUMERO VECCHIO'
055700020118     O                                         +  3 'NUMERO NUOVO'
055800020123     O          E            DETVIS
055900020129     O                       WNrPou            + 12
056000020129     O                       WNrOld            +  9 '     /  '
056100020129     O                       WNrNew            +  9 '      / '

000100140820     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200140820     H*PARMS ACTGRP(QILE)
000300140821     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000400951002?     *--------------------------------------------------------------*
000500140819      * tnsb75r                                                      *
000600951002      *     *----------------------------------------------*         *
000700140819      *         generazione file per autofatturazione                *
000800951002      *     *---------------------------------------------*          *
000900951002      *                                                              *
001000951002?     *--------------------------------------------------------------*
001100951002      * KC - FINE LAVORO                                             *
001200951002?     *--------------------------------------------------------------*
001300951002      * 28 - Visualizzazione messaggio di errore                     *
001400951002?     *--------------------------------------------------------------*
001500140821     Ftnanf02l  IF   E           K DISK
001600140821     Ffiapd01l  IF   E           K DISK
001700140820     Fanrco98j  IF   E           K DISK
001800140821     Fwfanf00f  o    E             DISK
001900140819     Ftnsb75d   CF   E             WORKSTN
002000951002     D*---------------------------------------------------------------*
002100951002     D* SCHIERA
002200951002     D*---------------------------------------------------------------*
002300140822     D ERR             S             70    DIM(10) CTDATA PERRCD(1)             Errori
002400121031     D savpjbu         S                   like(KPJBU)
002500980910?     *--------------------------------------------------------------*
002600980910?     *  DS                                                          *
002700980910?     *--------------------------------------------------------------*
002800980910     D DSFMT           DS
002900980910     D  $TASTO               369    369
003000980910     D  NRG                  370    370
003100980910     D  NCL                  371    371
003200980910     D*-------------
003300980910     D* posizione cursore
003400980910     D CURSOR          DS
003500980910     D  RIRI                   1      2B 0 INZ
003600980910     D  R$$                    2      2
003700980910     D  COCO                   3      4B 0 INZ
003800980910     D  C$$                    4      4
003900980910     D*-------------
004000980910     D* Reperimento nome PGM
004100020510     D                SDS
004200020510     D  nompgm           *PROC
004300951002     D KPJBA         E DS
004400020404     Ddataiso                          d   datfmt(*iso)
004500020415     Doggiiso                          d   datfmt(*iso)
004600980910     Ddataeur                          d   datfmt(*eur)
004700140820     D Ana009ds      E DS                  INZ
004800140820     D Ana400ds      E DS                  INZ
004900140821     d trmz70s1ds    e ds                  inz
005000960215     D*------------------
005100020403     D* DS "TRUL16R" - RICERCA ALFABETICA autotrasportatore
005200960215     D*------------------
005300960215     D PARAM           DS                  INZ
005400020415     D* TIPOLOGIA P_=autotrasport.
005500000811     D  PARTIP                 1      2
005600020415     D* RAGIONE SOCIALE aoutotras.
005700000811     D  PARRSC                 3     37
005800020415     D* S.I.
005900000811     D  PARCSF                38     40
006000020415     D* CODICE
006100000811     D  PARPDR                41     47  0
006200020415     D* PARFLG = "3" --> NON ESISTONO autotras. PER LA CHIAVE
006300000811     D*                  ALFABETICA RICHIESTA
006400000811     D  PARFLG                48     48
006500980910     D soc001        E DS                  EXTNAME(xsoc001ds)
006600980910     D xsocds          DS          1000
006700140821     d                 ds                  inz
006800140821     d wDateTime                     14  0 inz
006900140821     d   wTime                 1      6s 0 inz
007000140821     d   wDate                 7     14s 0 inz
007100990506     D*-------------
007200020403     D com13           s             13    inz('0000000000000')
007300140820     D wsnat           s              1    inz('F')
007400140821     d Data_Iso        s               d   inz  datfmt(*iso)
007500140821     d w0080           s              8  0 inz
007600140822     D scritto         s              1    inz
007700140822     D doppio          s              1    inz
007800951002?     *--------------------------------------------------------------*
007900951002?     *  CICLO PRINCIPALE                                            *
008000951002?     *--------------------------------------------------------------*
008100951002      *  Impostazione parametri prima videata
008200951002     C                   EXSR      INZ01
008300951002      *  Loop gestione videata
008400951002     C     WFINE         DOUEQ     'S'
008500951002     C                   EXSR      GESD01
008600951002     C                   END
008700951002      *  Fine Lavoro
008800951002     C     FINE          TAG
008900951002     C                   SETON                                        LR
009000951002?     *--------------------------------------------------------------*
009100951002?     *  INZ01: Inizializzazione prima videata                       *
009200951002?     *--------------------------------------------------------------*
009300951002     C     INZ01         BEGSR
009400951002      *
009500020403     C                   SETOFF                                       2898
009600020403     C                   movea     com13         *in(40)
009700140820     C                   movel     xscsoc        V1Csoc
009800140820     C                   movel     *blanks       V1cfor
009900140820     C                   movel     *blanks       V1dfor
010000140820     C                   Z-ADD     0             V1Cfat
010100140820     C                   Z-ADD     0             V2Cfat
010200140820     C                   MOVEL     'N'           V1cpul
010300140820     C                   MOVEL     *BLANKS       V1civa
010400140820     C                   MOVEL     *BLANKS       Vtsr
010500951002     C*
010600951002     C                   ENDSR
010700951002?     *--------------------------------------------------------------*
010800951002?     *  GESD01: Gestione prima videata                              *
010900951002?     *--------------------------------------------------------------*
011000951002     C     GESD01        BEGSR
011100951002      *
011200140820     C                   EXFMT     v1
011300140901     c                   clear                   scritto
011400140901     c                   clear                   doppio
011500121031      *
011600020403     C                   SETOFF                                       2898
011700020403     C                   movea     com13         *in(40)
011800951002      *  Fine Lavoro
011900951002     C     *INKC         IFEQ      '1'
012000951002     C                   MOVEL     'S'           WFINE
012100951002     C                   GOTO      FINVD1
012200951002     C                   END
012300020403     C*  effettua ricerche per fornitore
012400980910     c                   if        *inkd = *on
012500980910     c                   exsr      search
012600020503     C                   GOTO      FINVD1
012700980910     c                   end
012800951002      *  Controlli
012900951002     C                   EXSR      CTR01
013000951002     C   28              GOTO      FINVD1
013100140820     c                   if        *inkf
013200140822      * se richiesto blank forzo doppio giro in estrazione
013300140822      *
013400140822     c     rigira        tag
013500140822     c                   if        vtsr = *blank
013600140822     c                   move      'A'           vtsr
013700140822     c                   move      'X'           doppio
013800140822     c                   endif
013900140821     c                   exsr      testrek
014000140822     c                   if        trovato = *blank and doppio = *blank
014100140821     C                   seton                                        28
014200140821     C                   movel     ERR(03)       $MSG
014300140821     C                   goto      finvd1
014400140821     c                   endif
014500140821     c                   exsr      pulisci
014600140820     c                   exsr      elabora
014700140822     c                   if        doppio <> *blank and vtsr = 'A'
014800140822     c                   move      'D'           vtsr
014900140822     c                   goto      rigira
015000140822     c                   endif
015100140820     c                   endif
015200140822     c                   if        scritto <> *blank
015300140822     C                   seton                                        28
015400140822     C                   movel     ERR(10)       $MSG
015500140822     C                   goto      finvd1
015600140822     c                   endif
015700951002      *
015800951002     C     FINVD1        ENDSR
015900140821?     *--------------------------------------------------------------*
016000140821?     *  verifica se esiste almeno un rekord con i parametri richiesti
016100140821?     *--------------------------------------------------------------*
016200140821     C     testrek       BEGSR
016300140821     c                   clear                   trovato           1
016400140821     c                   movel     vtsr          wtip
016500140821     c     kanf          setll     tnanf02l
016600140821     c                   do        *hival
016700140821     c     kanf2         reade     tnanf02l
016800140821     c                   if        %eof(tnanf02l)
016900140821     c                   leave
017000140821     c                   endif
017100140821     c                   if        anfdft >= wdatda and
017200140821     c                             anfdft <= wdataa
017300140821     c                   move      'X'           trovato
017400140821     c                   leave
017500140821     c                   endif
017600140821     c                   enddo
017700140821     c                   endsr
017800140821?     *--------------------------------------------------------------*
017900140821?     *  elabora estrae dati per w.f
018000140821?     *--------------------------------------------------------------*
018100140821     C     elabora       BEGSR
018200140821     c     kanf          setll     tnanf02l
018300140821     c                   do        *hival
018400140821     c     kanf2         reade     tnanf02l
018500140821     c                   if        %eof(tnanf02l)
018600140821     c                   leave
018700140821     c                   endif
018800140821     c                   if        anfdft >= wdatda and
018900140821     c                             anfdft <= wdataa
019000140822     c                   CLEAR                   wfanf000
019100140821     c                   move      knmus         WANFUTE
019200140821     c                   move      *date         WANFDTA
019300140821     c                   move      ANFTIP        WANFTIP
019400140821     c                   move      ANFTSR        WANFTSR
019500140821     c                   move      ANFSOC        WANFSOC
019600140821     c                   move      ANFCDF        WANFCDF
019700140821     c                   move      v1dfor        WANFDECF
019800140821     c                   move      v1cIVA        WANFPIVA
019900140821     c                   move      ANFNFF        WANFNFF
020000140821     c                   move      ANFDFT        WANFDFT
020100140821     c                   move      ANFPDR        WANFPDR
020200140821     c     kapd          chain     fiapd01l
020300140821     c                   if        %found(fiapd01l)
020400140821     c                   move      APDRSF        WAPDRSF
020500140821     c                   endif
020600140821     c                   write     wfanf000
020700140822     c                   move      'X'           scritto
020800140821     c                   endif
020900140821     c                   enddo
021000140821     c                   endsr
021100140820?     *--------------------------------------------------------------*
021200140821?     *  Pulizia file di work
021300140820?     *--------------------------------------------------------------*
021400140821     C     pulisci       BEGSR
021500140821      /free
021600140821
021700140821         w0080 = %dec( %date( wDataymd : *ISO ) - %months(2) );
021800140821
021900140821         // - Richiesta pulizia del workfile WFanf00F
022000140821         if  v1cpul = 'S';
022100140821           exec sql     DELETE from WFanf00F
022200140821                         where Wanfute = :KNMUS
022300140821                            or Wanfdta <= :w0080;
022400140821         else;
022500140821           // - ...Comunque vengono cancellati i record relativi alle
022600140821           //   estrazioni di più di 2 (due) mesi fa - di tutti gli
022700140821           //   utenti!!!
022800140821           exec sql     DELETE from WFanf00F
022900140821                         where Wanfdta <= :w0080;
023000140821         endif;
023100140821
023200140821      /end-free
023300140821      *
023400140821     c                   endsr
023500951002?     *--------------------------------------------------------------*
023600951002?     *  CTR01: Controlli prima videata                              *
023700951002?     *--------------------------------------------------------------*
023800951002     C     CTR01         BEGSR
023900140820     C* Verifico società
024000140820     c                   if        v1csoc = *blanks
024100140820     C                   SETON                                        4228
024200140820     C                   MOVEL     ERR(07)       $MSG
024300020423     C                   GOTO      FINCT1
024400140820     c                   endif
024500140820     C     V1Cfor        IFEQ      *blanks
024600140820     C     V1Civa        andeq     *blanks
024700140820     C                   SETON                                        4028
024800140820     C                   MOVEL     ERR(08)       $MSG
024900020403     C                   GOTO      FINCT1
025000020403     C                   END
025100020403     c* data da
025200140820     c                   if        v1cfat <> 0
025300140820     c     *eur          test(d)                 v1cfat                 45
025400140820     c                   if        *in45
025500140820     c     *dmy          test(d)                 v1cfat                 45
025600140820     c  n45*dmy          move      v1cfat        dataeur
025700140820     c  n45              move      dataeur       v1cfat
025800980911     c                   end
025900140821     c                   else
026000140821     c                   seton                                          2845
026100140821     c                   movel     err(4)        $msg
026200140821     c                   goto      finct1
026300020404     c                   end
026400020403     c* data al
026500140820     c                   if        v2cfat <> 0
026600140820     c     *eur          test(d)                 v2cfat                 46
026700140820     c                   if        *in46
026800140820     c     *dmy          test(d)                 v2cfat                 46
026900140820     c  n46*dmy          move      v2cfat        dataeur
027000140820     c  n46              move      dataeur       v2cfat
027100980911     c                   end
027200140821     c                   else
027300140821     c                   seton                                          2846
027400140821     c                   movel     err(4)        $msg
027500140821     c                   goto      finct1
027600020404     c                   end
027700140820     c                   if        *in45 or *in46
027800980910     c                   seton                                          28
027900980910     c                   movel     err(5)        $msg
028000980910     c                   goto      finct1
028100020404     c                   else
028200140820     c                   if        v1cfat <> 0
028300140820     c                   movel     v1cfat        dataeur
028400980910     c                   movel     dataeur       dataiso
028500980910     c                   movel     dataiso       wdatda            8 0
028600020404     c                   else
028700020404     c                   z-add     0             wdatda
028800020404     c                   end
028900140820     c                   movel     v2cfat        dataeur
029000980910     c                   movel     dataeur       dataiso
029100980910     c                   movel     dataiso       wdataa            8 0
029200951002      *  Controllo limiti due date
029300951002     C     WDATDA        IFGT      WDATAA
029400140820     C                   SETON                                        464528
029500951002     C                   MOVEL     ERR(6)        $MSG                           Data iniz.>fina.
029600951002     C                   GOTO      FINCT1
029700951002     C                   END
029800980910     c                   end
029900140821     c                   if        v1cfor <> *blank
030000140821     c     krco          chain     anrco98j
030100140821     c                   if        %found(anrco98j)
030200140821     C                   movel(p)  sogpartiva    V1civa
030300140821     C                   movel(p)  sogdes        V1dfor
030400140821     c                   else
030500140821     C                   SETON                                        4028
030600140821     C                   MOVEL     ERR(2)        $MSG                           Data iniz.>fina.
030700140821     C                   GOTO      FINCT1
030800140821     c                   endif
030900140821     c                   else
031000140821     C                   clear                   trmz70s1ds                     Input
031100140821     C                   movel(p)  V1civa        PartitaIVA                     Input
031200140821     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
031300140821     C                   movel(p)  V1csoc        Societa                        Input/Output
031400140821     c                   call      'TRMZ70SR1'
031500140821     C                   PARM                    trmz70s1ds                     Input
031600140821     c                   if        trovato = *on
031700140821     c                   move      KEYKSC        v1cfor
031800140821     c                   move      RAGSOCKSC     v1dfor
031900140821     c                   else
032000140821     C                   SETON                                        4028
032100140821     C                   MOVEL     ERR(9)        $MSG                           Data iniz.>fina.
032200140821     C                   GOTO      FINCT1
032300140821     c                   endif
032400140821     c                   endif
032500951003      *
032600951009     C     FINCT1        ENDSR
032700020404     C************************************************************
032800020404     C* RICERCHE
032900020404     C************************************************************
033000020404     C     SEARCH        BEGSR
033100980910     C*
033200980910     C* determino Riga/Colonna del cursore
033300980910     C                   MOVE      nrg           R$$
033400980910     C                   MOVE      ncl           C$$
033500980910     C                   Z-ADD     RIRI          CURR
033600980910     C                   Z-ADD     COCO          CURC
033700980910     C*
033800980910     C* I campi che al momento dell'F4 risultano protetti non devono
033900980910     C* essere abilitati alla funzione di ricerca
034000980910     C*
0341009809101    C                   SELECT
034200020423     C* FMTD1  - ricerca unità di reg.
034300140820    >C     H1NMFL        WHEneq    'V1CSOC'
034400140820     C                   RESET                   Ana009DS
034500020423     C                   EVAL      OPZ009 = '01'
034600140820     C                   EVAL      Kpjbu = Ana009DS
034700020423     C                   CALL      'ANA009R'
034800020423     C                   PARM                    KPJBA
034900140820     C                   EVAL      Ana009DS = Kpjbu
035000020423      *
035100140820     C     soc009        IFNE      *BLANK
035200140820     C                   MOVE      soc009        v1csoc
035300020423     C                   ENDIF
035400030422     C* FMTD1  - ricerca fornitore 1 AUTOTRASPORTATORE
035500140820    >C                   WHEn      H1NMFL = 'V1CFOR'
035600980910     C*  deposita la KPJBU
035700980910     C                   MOVEL     KPJBU         DKPJBU          256
035800140820     C                   movel     v1csoc        parcsf
035900140820     C                   MOVEl     V1dfor        PARRSC
036000980910     C                   Z-ADD     *ZEROS        PARPDR
036100020403     C                   eval      partip = 'P '
036200980910     C                   MOVEL     *BLANKS       PARFLG
036300020419     C                   MOVEL(p)  PARAM         KPJBU
036400980910     C                   CALL      'TRUL16R'
036500980910     C                   PARM                    KPJBA
036600980910     C                   MOVEL     KPJBU         PARAM
036700980910IF  2C     PARFLG        IFEQ      '3'
036800140820     C                   MOVEL     *BLANKS       V1Dfor
036900020403     C                   SETON                                        5028
037000140820     C                   MOVEL     ERR(02)       $MSG
037100980910X   2C                   ELSE
037200140820     C                   MOVEl     *zeros        V1Cfor
037300140820     C                   MOVE      PARPDR        V1Cfor
037400140820     C                   MOVEL     PARRSC        V1Dfor
037500980910E   2C                   ENDIF
037600140820     C* FMTD1  - ricerca fornitore x partita iva
037700140820    >C                   WHEn      H1NMFL = 'V1CIVA'
037800020403     C*  deposita la KPJBU
037900020403     C                   MOVEL     KPJBU         DKPJBU          256
038000140820     C                   clear                   ana400ds
038100140820     C                   MOVEL     '01'          OPZ400
038200140820     C                   movel(p)  V1civa        iva400
038300140820     C                   movel(p)  v1cfor        ksc400
038400140820     C                   movel(p)  V1dfor        des400
038500140820     c                   movel     ana400ds      kpjbu
038600140820     c                   call      'ANA400R'
038700140820     C                   PARM                    kpjba
038800140820     c                   movel     kpjbu         ana400ds
038900140820     C                   movel(p)  iva400        V1civa
039000140820     C                   movel(p)  ksc400        v1cfor
039100140820     C                   movel(p)  des400        V1dfor
039200140820     c*
039300020510     C                   ENDsl
039400980910     C* REIMPOSTA LA KPJBU
039500980910     C                   MOVEL     DKPJBU        KPJBU
039600980910     C                   SETON                                        98
039700980910     C* imposto pos. del cursore
039800980910     C                   Z-ADD     CURR          H1RIGA
039900980910     C                   Z-ADD     CURC          H1COLO
040000980910     C                   ENDsr
040100980910     C*----------------------------------------------------*
040200980910     C* Reperimento dati società
040300980910     C*----------------------------------------------------*
040400980910     C     REPSOC        BEGSR
040500980910     C*
040600980910     C                   CALLB     'XSOC'
040700980910     C                   PARM                    TIPXSC            6
040800980910     C                   PARM                    SOCXSC            3
040900980910     C                   PARM                    CDSXSC            9 0
041000980910     C                   PARM                    MODXSC            3
041100980910     C                   PARM      *blanks       RTNXSC            1
041200980910     C                   PARM                    XSOCDS
041300980910     C                   PARM                    KPJBA
041400980910     C*
041500980910     C                   ENDSR
041600951006?     *--------------------------------------------------------------*
041700951006?     * *INZSR: Operazioni iniziali                                  *
041800951006?     *--------------------------------------------------------------*
041900951009     C     *INZSR        BEGSR
042000951006      *
042100951006     C     *ENTRY        PLIST
042200951006     C                   PARM                    KPJBA
042300140820     c     krco          klist
042400140820     c                   kfld                    v1csoc
042500140820     c                   kfld                    wsnat
042600140820     c                   kfld                    v1cfor
042700140821     c     kanf          klist
042800140821     c                   kfld                    wtip              2
042900140821     c                   kfld                    vtsr
043000140821     c                   kfld                    v1csoc
043100140821     c                   kfld                    v1cfor
043200140821     c                   kfld                    wdatda
043300140821     c     kanf2         klist
043400140821     c                   kfld                    wtip
043500140821     c                   kfld                    vtsr
043600140821     c                   kfld                    v1csoc
043700140821     c                   kfld                    v1cfor
043800140821     c     kapd          klist
043900140821     c                   kfld                    vtsr
044000140821     c                   kfld                    anfpdr
044100140820     c*
044200140821     c                   time                    wDateTime
044300140821     c     *eur          movel     wDate         Data_Iso
044400140821     c                   move      Data_Iso      Wdataymd          8 0
044500140821     c                   movel     wTime         wTimeHM           6 0
044600980910     C*---------- RICERCA DITTA :
044700980910     C                   MOVEL     'SOC001'      TIPXSC
044800980910     C                   MOVEL     *BLANK        SOCXSC
044900980910     C                   EXSR      REPSOC
045000980910     C     RTNXSC        IFNE      '1'
045100980910     C                   MOVEL     XSOCDS        SOC001
045200980910     C                   MOVEL     xscrgs        RSUT             20
045300020417     c                   else
045400020417     c                   goto      fine
045500980910     c                   end
045600951006      *  Definisco variabili
045700980910     C                   Z-ADD     0             CURR              2 0
045800980910     C                   Z-ADD     0             CURC              2 0
045900951006      *  Inizializzo variabili
046000951006     C                   MOVEL     'N'           WFINE             1
046100980910      *
046200980910     c                   endsr
046300951012** ERR
046400020404Codice fornitore obbligatorio                                         01
046500020404Codice fornitore inesiste o annullato                                 02
046600140820Non esistono in archivio dati relativi a questo fornitore nel periodo 03
046700951006Data obbligatoria                                                     04
046800951006Data errata                                                           05
046900951006Limiti incompatibili: data limite iniziale > finale                   06
047000140820Codice società obbligatorio                                           07
047100140820Inserire o codice fornitore o partita IVA                             08
047200140821Partita IVA non corrispondente a fornitore verificare                 09
047300140822Elaborazione completata con successo                                  10

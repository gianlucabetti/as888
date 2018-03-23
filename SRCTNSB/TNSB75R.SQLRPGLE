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
001800140902     Fansog08j  IF   E           K DISK
001900140902     Fansog01l  IF   E           K DISK      rename(ansog000:ansog)
002000140821     Fwfanf00f  o    E             DISK
002100140819     Ftnsb75d   CF   E             WORKSTN
002200951002     D*---------------------------------------------------------------*
002300951002     D* SCHIERA
002400951002     D*---------------------------------------------------------------*
002500140822     D ERR             S             70    DIM(10) CTDATA PERRCD(1)             Errori
002600121031     D savpjbu         S                   like(KPJBU)
002700980910?     *--------------------------------------------------------------*
002800980910?     *  DS                                                          *
002900980910?     *--------------------------------------------------------------*
003000980910     D DSFMT           DS
003100980910     D  $TASTO               369    369
003200980910     D  NRG                  370    370
003300980910     D  NCL                  371    371
003400980910     D*-------------
003500980910     D* posizione cursore
003600980910     D CURSOR          DS
003700980910     D  RIRI                   1      2B 0 INZ
003800980910     D  R$$                    2      2
003900980910     D  COCO                   3      4B 0 INZ
004000980910     D  C$$                    4      4
004100980910     D*-------------
004200980910     D* Reperimento nome PGM
004300020510     D                SDS
004400020510     D  nompgm           *PROC
004500951002     D KPJBA         E DS
004600020404     Ddataiso                          d   datfmt(*iso)
004700020415     Doggiiso                          d   datfmt(*iso)
004800980910     Ddataeur                          d   datfmt(*eur)
004900140902     D Ana007ds      E DS                  INZ
005000140820     D Ana400ds      E DS                  INZ
005100140902     D Ana040ds      E DS                  INZ
005200140902     D  RET040       E                     INZ('0')
005300140902     D  OPR040       E                     INZ('0')
005400140902     D  ERR040       E                     INZ('0')
005500960215     D*------------------
005600020403     D* DS "TRUL16R" - RICERCA ALFABETICA autotrasportatore
005700960215     D*------------------
005800960215     D PARAM           DS                  INZ
005900020415     D* TIPOLOGIA P_=autotrasport.
006000000811     D  PARTIP                 1      2
006100020415     D* RAGIONE SOCIALE aoutotras.
006200000811     D  PARRSC                 3     37
006300020415     D* S.I.
006400000811     D  PARCSF                38     40
006500020415     D* CODICE
006600000811     D  PARPDR                41     47  0
006700020415     D* PARFLG = "3" --> NON ESISTONO autotras. PER LA CHIAVE
006800000811     D*                  ALFABETICA RICHIESTA
006900000811     D  PARFLG                48     48
007000980910     D soc001        E DS                  EXTNAME(xsoc001ds)
007100980910     D xsocds          DS          1000
007200140821     d                 ds                  inz
007300140821     d wDateTime                     14  0 inz
007400140821     d   wTime                 1      6s 0 inz
007500140821     d   wDate                 7     14s 0 inz
007600990506     D*-------------
007700020403     D com13           s             13    inz('0000000000000')
007800140820     D wsnat           s              1    inz('F')
007900140821     d Data_Iso        s               d   inz  datfmt(*iso)
008000140821     d w0080           s              8  0 inz
008100140822     D scritto         s              1    inz
008200140902     D savtsr          s              1    inz
008300140822     D doppio          s              1    inz
008400140902     d cdfor           s              8    dim(100)
008500140902     d cdsoc           s              3    dim(100)
008600140902     d i               s              3  0
008700951002?     *--------------------------------------------------------------*
008800951002?     *  CICLO PRINCIPALE                                            *
008900951002?     *--------------------------------------------------------------*
009000951002      *  Impostazione parametri prima videata
009100951002     C                   EXSR      INZ01
009200951002      *  Loop gestione videata
009300951002     C     WFINE         DOUEQ     'S'
009400951002     C                   EXSR      GESD01
009500951002     C                   END
009600951002      *  Fine Lavoro
009700951002     C     FINE          TAG
009800951002     C                   SETON                                        LR
009900951002?     *--------------------------------------------------------------*
010000951002?     *  INZ01: Inizializzazione prima videata                       *
010100951002?     *--------------------------------------------------------------*
010200951002     C     INZ01         BEGSR
010300951002      *
010400020403     C                   SETOFF                                       2898
010500020403     C                   movea     com13         *in(40)
010600140908     C                   movel     *blank        V1Csoc
010700140820     C                   movel     *blanks       V1cfor
010800140820     C                   movel     *blanks       V1dfor
010900140820     C                   Z-ADD     0             V1Cfat
011000140820     C                   Z-ADD     0             V2Cfat
011100140820     C                   MOVEL     'N'           V1cpul
011200140820     C                   MOVEL     *BLANKS       V1civa
011300140820     C                   MOVEL     *BLANKS       Vtsr
011400951002     C*
011500951002     C                   ENDSR
011600951002?     *--------------------------------------------------------------*
011700951002?     *  GESD01: Gestione prima videata                              *
011800951002?     *--------------------------------------------------------------*
011900951002     C     GESD01        BEGSR
012000951002      *
012100140820     C                   EXFMT     v1
012200140901     c                   clear                   scritto
012300140901     c                   clear                   doppio
012400121031      *
012500020403     C                   SETOFF                                       2898
012600020403     C                   movea     com13         *in(40)
012700951002      *  Fine Lavoro
012800951002     C     *INKC         IFEQ      '1'
012900951002     C                   MOVEL     'S'           WFINE
013000951002     C                   GOTO      FINVD1
013100951002     C                   END
013200020403     C*  effettua ricerche per fornitore
013300980910     c                   if        *inkd = *on
013400980910     c                   exsr      search
013500020503     C                   GOTO      FINVD1
013600980910     c                   end
013700951002      *  Controlli
013800951002     C                   EXSR      CTR01
013900951002     C   28              GOTO      FINVD1
014000140820     c                   if        *inkf
014100140902     c                   exsr      pulisci
014200140902     c* se richiesto codice fornitore elaboro solo quello altrimenti preparo loop
014300140902     c                   if        v1cfor = *blank
014400140902     c                   move      vtsr          savtsr
014500140902     c                   exsr      caricaloop
014600140902     c                   else
014700140902     c                   movel     v1cfor        cdfor(1)
014800140902     c                   movel     v1csoc        cdsoc(1)
014900140902     c                   z-add     1             i
015000140902     c                   endif
015100140902     c                   if        i > 0
015200140902     c     1             do        i             ii                3 0
015300140902     c                   movel     savtsr        vtsr
015400140902     c                   clear                   doppio
015500140902     c                   movel     cdfor(ii)     v1cfor
015600140902     c                   movel     cdsoc(ii)     v1csoc
015700140822      * se richiesto blank forzo doppio giro in estrazione
015800140822      *
015900140822     c     rigira        tag
016000140822     c                   if        vtsr = *blank
016100140822     c                   move      'A'           vtsr
016200140822     c                   move      'X'           doppio
016300140822     c                   endif
016400140902      * effettua test di verifica solo se richiesto singolo fornitore
016500140904     c                   if        i = 1
016600140821     c                   exsr      testrek
016700140822     c                   if        trovato = *blank and doppio = *blank
016800140821     C                   seton                                        28
016900140821     C                   movel     ERR(03)       $MSG
017000140821     C                   goto      finvd1
017100140821     c                   endif
017200140902     c                   endif
017300140902      *
017400140820     c                   exsr      elabora
017500140822     c                   if        doppio <> *blank and vtsr = 'A'
017600140822     c                   move      'D'           vtsr
017700140822     c                   goto      rigira
017800140822     c                   endif
017900140902     c                   enddo
018000140820     c                   endif
018100140822     c                   if        scritto <> *blank
018200140822     C                   seton                                        28
018300140822     C                   movel     ERR(10)       $MSG
018400140902     c                   movel     savtsr        vtsr
018500140822     C                   goto      finvd1
018600140903     c                   else
018700140903     C                   seton                                        28
018800140903     C                   movel     ERR(03)       $MSG
018900140903     C                   goto      finvd1
019000140822     c                   endif
019100140902      *
019200140902     c                   endif
019300951002      *
019400951002     C     FINVD1        ENDSR
019500140821?     *--------------------------------------------------------------*
019600140821?     *  verifica se esiste almeno un rekord con i parametri richiesti
019700140821?     *--------------------------------------------------------------*
019800140821     C     testrek       BEGSR
019900140821     c                   clear                   trovato           1
020000140821     c                   movel     vtsr          wtip
020100140821     c     kanf          setll     tnanf02l
020200140821     c                   do        *hival
020300140821     c     kanf2         reade     tnanf02l
020400140821     c                   if        %eof(tnanf02l)
020500140821     c                   leave
020600140821     c                   endif
020700140821     c                   if        anfdft >= wdatda and
020800140821     c                             anfdft <= wdataa
020900140821     c                   move      'X'           trovato
021000140821     c                   leave
021100140821     c                   endif
021200140821     c                   enddo
021300140821     c                   endsr
021400140821?     *--------------------------------------------------------------*
021500140821?     *  elabora estrae dati per w.f
021600140821?     *--------------------------------------------------------------*
021700140821     C     elabora       BEGSR
021800140903     c                   movel     vtsr          wtip
021900140821     c     kanf          setll     tnanf02l
022000140821     c                   do        *hival
022100140821     c     kanf2         reade     tnanf02l
022200140821     c                   if        %eof(tnanf02l)
022300140821     c                   leave
022400140821     c                   endif
022500140821     c                   if        anfdft >= wdatda and
022600140821     c                             anfdft <= wdataa
022700140822     c                   CLEAR                   wfanf000
022800140821     c                   move      knmus         WANFUTE
022900140821     c                   move      *date         WANFDTA
023000140821     c                   move      ANFTIP        WANFTIP
023100140821     c                   move      ANFTSR        WANFTSR
023200140821     c                   move      ANFSOC        WANFSOC
023300140821     c                   move      ANFCDF        WANFCDF
023400140821     c                   move      v1dfor        WANFDECF
023500140821     c                   move      v1cIVA        WANFPIVA
023600140821     c                   move      ANFNFF        WANFNFF
023700140821     c                   move      ANFDFT        WANFDFT
023800140821     c                   move      ANFPDR        WANFPDR
023900140821     c     kapd          chain     fiapd01l
024000140821     c                   if        %found(fiapd01l)
024100140821     c                   move      APDRSF        WAPDRSF
024200140821     c                   endif
024300140821     c                   write     wfanf000
024400140822     c                   move      'X'           scritto
024500140821     c                   endif
024600140821     c                   enddo
024700140821     c                   endsr
024800140820?     *--------------------------------------------------------------*
024900140821?     *  Pulizia file di work
025000140820?     *--------------------------------------------------------------*
025100140821     C     pulisci       BEGSR
025200140821      /free
025300140821
025400140821         w0080 = %dec( %date( wDataymd : *ISO ) - %months(2) );
025500140821
025600140821         // - Richiesta pulizia del workfile WFanf00F
025700140821         if  v1cpul = 'S';
025800140821           exec sql     DELETE from WFanf00F
025900140821                         where Wanfute = :KNMUS
026000140821                            or Wanfdta <= :w0080;
026100140821         else;
026200140821           // - ...Comunque vengono cancellati i record relativi alle
026300140821           //   estrazioni di più di 2 (due) mesi fa - di tutti gli
026400140821           //   utenti!!!
026500140821           exec sql     DELETE from WFanf00F
026600140821                         where Wanfdta <= :w0080;
026700140821         endif;
026800140821
026900140821      /end-free
027000140821      *
027100140821     c                   endsr
027200140902?     *--------------------------------------------------------------*
027300140902?     *   estrae i codici fornitori da elaborare
027400140902?     *--------------------------------------------------------------*
027500140902     C     caricaloop    BEGSR
027600140902     c                   clear                   cdfor
027700140902     c                   clear                   cdsoc
027800140902     c                   clear                   i
027900140902     c                   movel     v1civa        w1civa           20
028000140902     c     w1civa        setll     ansog08j
028100140902     c                   do        *hival
028200140902     c     w1civa        reade     ansog08j
028300140902     c                   if        %eof(ansog08j)
028400140902     c                   leave
028500140902     c                   endif
028600140902     c                   if        v1csoc <> *blank  and
028700140902     c                             v1csoc <> frnsocieta
028800140902     c                   iter
028900140902     c                   endif
029000140902     c                   add       1             i
029100140902     c                   move      frnksc        cdfor(i)
029200140902     c                   move      frnsocieta    cdsoc(i)
029300140902     c                   enddo
029400140902     c                   endsr
029500951002?     *--------------------------------------------------------------*
029600951002?     *  CTR01: Controlli prima videata                              *
029700951002?     *--------------------------------------------------------------*
029800951002     C     CTR01         BEGSR
029900140820     C     V1Cfor        IFEQ      *blanks
030000140820     C     V1Civa        andeq     *blanks
030100140820     C                   SETON                                        4028
030200140820     C                   MOVEL     ERR(08)       $MSG
030300020403     C                   GOTO      FINCT1
030400020403     C                   END
030500020403     c* data da
030600140820     c                   if        v1cfat <> 0
030700140820     c     *eur          test(d)                 v1cfat                 45
030800140820     c                   if        *in45
030900140820     c     *dmy          test(d)                 v1cfat                 45
031000140820     c  n45*dmy          move      v1cfat        dataeur
031100140820     c  n45              move      dataeur       v1cfat
031200980911     c                   end
031300140821     c                   else
031400140821     c                   seton                                          2845
031500140821     c                   movel     err(4)        $msg
031600140821     c                   goto      finct1
031700020404     c                   end
031800020403     c* data al
031900140820     c                   if        v2cfat <> 0
032000140820     c     *eur          test(d)                 v2cfat                 46
032100140820     c                   if        *in46
032200140820     c     *dmy          test(d)                 v2cfat                 46
032300140820     c  n46*dmy          move      v2cfat        dataeur
032400140820     c  n46              move      dataeur       v2cfat
032500980911     c                   end
032600140821     c                   else
032700140821     c                   seton                                          2846
032800140821     c                   movel     err(4)        $msg
032900140821     c                   goto      finct1
033000020404     c                   end
033100140820     c                   if        *in45 or *in46
033200980910     c                   seton                                          28
033300980910     c                   movel     err(5)        $msg
033400980910     c                   goto      finct1
033500020404     c                   else
033600140820     c                   if        v1cfat <> 0
033700140820     c                   movel     v1cfat        dataeur
033800980910     c                   movel     dataeur       dataiso
033900980910     c                   movel     dataiso       wdatda            8 0
034000020404     c                   else
034100020404     c                   z-add     0             wdatda
034200020404     c                   end
034300140820     c                   movel     v2cfat        dataeur
034400980910     c                   movel     dataeur       dataiso
034500980910     c                   movel     dataiso       wdataa            8 0
034600951002      *  Controllo limiti due date
034700951002     C     WDATDA        IFGT      WDATAA
034800140820     C                   SETON                                        464528
034900951002     C                   MOVEL     ERR(6)        $MSG                           Data iniz.>fina.
035000951002     C                   GOTO      FINCT1
035100951002     C                   END
035200980910     c                   end
035300140821     c                   if        v1cfor <> *blank
035400140821     c     krco          chain     anrco98j
035500140821     c                   if        %found(anrco98j)
035600140821     C                   movel(p)  sogpartiva    V1civa
035700140821     C                   movel(p)  sogdes        V1dfor
035800140821     c                   else
035900140821     C                   SETON                                        4028
036000140821     C                   MOVEL     ERR(2)        $MSG                           Data iniz.>fina.
036100140821     C                   GOTO      FINCT1
036200140821     c                   endif
036300140821     c                   else
036400140909     c                   movel     v1civa        w1civa
036500140909     c     w1civa        chain     ansog08j
036600140909     c                   if        %found(ansog08j)
036700140909     c                   move      sogdes        v1dfor
036800140909     c                   else
036900140909     C                   SETON                                        4228
037000140909     C                   MOVEL     ERR(9)        $MSG
037100140909     C                   GOTO      FINCT1
037200140909     c                   endif
037300140821     c                   endif
037400951003      *
037500951009     C     FINCT1        ENDSR
037600020404     C************************************************************
037700020404     C* RICERCHE
037800020404     C************************************************************
037900020404     C     SEARCH        BEGSR
038000980910     C*
038100980910     C* determino Riga/Colonna del cursore
038200980910     C                   MOVE      nrg           R$$
038300980910     C                   MOVE      ncl           C$$
038400980910     C                   Z-ADD     RIRI          CURR
038500980910     C                   Z-ADD     COCO          CURC
038600980910     C*
038700980910     C* I campi che al momento dell'F4 risultano protetti non devono
038800980910     C* essere abilitati alla funzione di ricerca
038900980910     C*
0390009809101    C                   SELECT
039100020423     C* FMTD1  - ricerca unità di reg.
039200140820    >C     H1NMFL        WHEneq    'V1CSOC'
039300140902     C                   RESET                   Ana007DS
039400140902     C                   EVAL      OPZ007 = '01'
039500140902     C                   EVAL      Kpjbu = Ana007DS
039600140902     C                   CALL      'ANA007R'
039700020423     C                   PARM                    KPJBA
039800140902     C                   EVAL      Ana007DS = Kpjbu
039900020423      *
040000140902     C     soc007        IFNE      *BLANK
040100140902     C                   MOVE      soc007        v1csoc
040200020423     C                   ENDIF
040300030422     C* FMTD1  - ricerca fornitore 1 AUTOTRASPORTATORE
040400140820    >C                   WHEn      H1NMFL = 'V1CFOR'
040500980910     C*  deposita la KPJBU
040600980910     C                   MOVEL     KPJBU         DKPJBU          256
040700140820     C                   movel     v1csoc        parcsf
040800140820     C                   MOVEl     V1dfor        PARRSC
040900980910     C                   Z-ADD     *ZEROS        PARPDR
041000020403     C                   eval      partip = 'P '
041100980910     C                   MOVEL     *BLANKS       PARFLG
041200020419     C                   MOVEL(p)  PARAM         KPJBU
041300980910     C                   CALL      'TRUL16R'
041400980910     C                   PARM                    KPJBA
041500980910     C                   MOVEL     KPJBU         PARAM
041600980910IF  2C     PARFLG        IFEQ      '3'
041700140820     C                   MOVEL     *BLANKS       V1Dfor
041800020403     C                   SETON                                        5028
041900140820     C                   MOVEL     ERR(02)       $MSG
042000980910X   2C                   ELSE
042100140902     C                   MOVEl     *blank        V1Cfor
042200140820     C                   MOVE      PARPDR        V1Cfor
042300140903     C                   MOVEl     0             V1Cfor
042400140820     C                   MOVEL     PARRSC        V1Dfor
042500140910     C                   MOVEL     PARcsf        V1csoc
042600980910E   2C                   ENDIF
042700140820     C* FMTD1  - ricerca fornitore x partita iva
042800140820    >C                   WHEn      H1NMFL = 'V1CIVA'
042900020403     C*  deposita la KPJBU
043000140902      * chiamata con società impostata
043100140902     c                   if        v1csoc <> *blank
043200020403     C                   MOVEL     KPJBU         DKPJBU          256
043300140820     C                   clear                   ana400ds
043400140820     C                   MOVEL     '01'          OPZ400
043500140902     C                   movel(p)  V1csoc        societa400
043600140820     C                   movel(p)  V1civa        iva400
043700140820     C                   movel(p)  v1cfor        ksc400
043800140820     C                   movel(p)  V1dfor        des400
043900140820     c                   movel     ana400ds      kpjbu
044000140820     c                   call      'ANA400R'
044100140820     C                   PARM                    kpjba
044200140820     c                   movel     kpjbu         ana400ds
044300140820     C                   movel(p)  iva400        V1civa
044400140902     C                   movel(p)  ksc400        V1cfor
044500140820     C                   movel(p)  des400        V1dfor
044600140902     c                   else
044700140902      *ana040 sui soggetti senza società
044800140902     C                   MOVEL     KPJBU         DKPJBU          256
044900140902     C                   reset                   ana040ds
045000140902     C                   MOVEL     '01'          OPZ040
045100140902     C                   movel(p)  V1civa        iva040
045200140902     C                   movel(p)  V1dfor        des040
045300140902     c                   movel     ana040ds      kpjbu
045400140902     c                   call      'ANA040R'
045500140902     C                   PARM                    kpjba
045600140902     c                   movel     kpjbu         ana040ds
045700140902     C     COD040        IFNE      *BLANK
045800140902     c                   clear                   v1civa
045900140902     c                   clear                   v1dfor
046000140902     C     cod040        chain     ansog01l
046100140902     c                   if        %found(ansog01l)
046200140902     c                   movel     sogpartiva    v1civa
046300140902     c                   movel     sogdes        v1dfor
046400140902     c                   endif
046500140902     C                   ENDIF
046600140902     c                   endif
046700140820     c*
046800020510     C                   ENDsl
046900980910     C* REIMPOSTA LA KPJBU
047000980910     C                   MOVEL     DKPJBU        KPJBU
047100980910     C                   SETON                                        98
047200980910     C* imposto pos. del cursore
047300980910     C                   Z-ADD     CURR          H1RIGA
047400980910     C                   Z-ADD     CURC          H1COLO
047500980910     C                   ENDsr
047600140905     C*----------------------------------------------------*
047700140905     C* Reperimento dati società
047800140905     C*----------------------------------------------------*
047900140905     C     REPSOC        BEGSR
048000140905     C*
048100140905     C                   CALLB     'XSOC'
048200140905     C                   PARM                    TIPXSC            6
048300140905     C                   PARM                    SOCXSC            3
048400140905     C                   PARM                    CDSXSC            9 0
048500140905     C                   PARM      *blanks       MODXSC            3
048600140905     C                   PARM      *blanks       RTNXSC            1
048700140905     C                   PARM                    XSOCDS
048800140905     C                   PARM                    KPJBA
048900140905     C*
049000140905     C                   ENDSR
049100951006?     *--------------------------------------------------------------*
049200951006?     * *INZSR: Operazioni iniziali                                  *
049300951006?     *--------------------------------------------------------------*
049400951009     C     *INZSR        BEGSR
049500951006      *
049600951006     C     *ENTRY        PLIST
049700951006     C                   PARM                    KPJBA
049800140820     c     krco          klist
049900140820     c                   kfld                    v1csoc
050000140820     c                   kfld                    wsnat
050100140820     c                   kfld                    v1cfor
050200140821     c     kanf          klist
050300140821     c                   kfld                    wtip              2
050400140821     c                   kfld                    vtsr
050500140821     c                   kfld                    v1csoc
050600140821     c                   kfld                    v1cfor
050700140821     c                   kfld                    wdatda
050800140821     c     kanf2         klist
050900140821     c                   kfld                    wtip
051000140821     c                   kfld                    vtsr
051100140821     c                   kfld                    v1csoc
051200140821     c                   kfld                    v1cfor
051300140821     c     kapd          klist
051400140821     c                   kfld                    vtsr
051500140821     c                   kfld                    anfpdr
051600140820     c*
051700140821     c                   time                    wDateTime
051800140821     c     *eur          movel     wDate         Data_Iso
051900140821     c                   move      Data_Iso      Wdataymd          8 0
052000140821     c                   movel     wTime         wTimeHM           6 0
052100951006      *  Definisco variabili
052200980910     C                   Z-ADD     0             CURR              2 0
052300980910     C                   Z-ADD     0             CURC              2 0
052400951006      *  Inizializzo variabili
052500951006     C                   MOVEL     'N'           WFINE             1
052600980910      *
052700140905     C                   eval      tipxsc = 'SOC002'
052800140919     C                   eval      socxsc = '201'
052900140905     C                   exsr      repsoc
053000140905     C     RTNXSC        IFNE      '1'
053100140905     C                   MOVEL     XSOCDS        SOC001
053200140905     C                   MOVEL     xscrgs        RSUT             20
053300140905     c                   else
053400140905     C                   return
053500140905     c                   end
053600140905      *
053700980910     c                   endsr
053800140905      *--------------------------------------------------------------
053900951012** ERR
054000020404Codice fornitore obbligatorio                                         01
054100020404Codice fornitore inesiste o annullato                                 02
054200140820Non esistono in archivio dati relativi a questo fornitore nel periodo 03
054300951006Data obbligatoria                                                     04
054400951006Data errata                                                           05
054500951006Limiti incompatibili: data limite iniziale > finale                   06
054600140820Codice società obbligatorio                                           07
054700140820Inserire o codice fornitore o partita IVA                             08
054800140821Partita IVA non corrispondente a fornitore verificare                 09
054900140822Elaborazione completata con successo                                  10

000100990506     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990506     H*PARMS ACTGRP(QILE)
000300980910     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400951002?     *--------------------------------------------------------------*
000500951002      *     *----------------------------------------------*         *
000600031202      *      AUTOFATTURAZIONE EXTRA Fornitori                        *
000700140121      *     *----------------------------------------------*         *
000800951002?     *--------------------------------------------------------------*
000900951002      * KC - FINE LAVORO                                             *
001000951002?     *--------------------------------------------------------------*
001100951002      * 28 - Visualizzazione messaggio di errore                     *
001200951002?     *--------------------------------------------------------------*
001300031119     Ffiapd01l  IF   E           K DISK
001400031119     Fansog01l  IF   E           K DISK
001500101213     Fansif01l  IF   E           K DISK    usropn
001600101213     Fanfrn01l  IF   E           K DISK
001700020417     Fanrco01l  IF   E           K DISK
001800020417     Ftntbe01l  IF   E           K DISK
001900031119     Fficn92D   CF   E             WORKSTN
002000980910?     *--------------------------------------------------------------*
002100980910?     *  DS                                                          *
002200980910?     *--------------------------------------------------------------*
002300031119     D* DS "TRUL16R" - RICERCA ALFABETICA autotrasportatore
002400031119     D PARAM           DS                  INZ
002500031119     D  PARTIP                 1      2
002600031119     D  PARRSC                 3     37
002700031119     D  PARCSF                38     40
002800031119     D  PARPDR                41     47  0
002900031119     D  PARFLG                48     48
003000980910     D DSFMT           DS
003100980910     D  $TASTO               369    369
003200980910     D  NRG                  370    370
003300980910     D  NCL                  371    371
003400980910     D* posizione cursore
003500980910     D CURSOR          DS
003600980910     D  RIRI                   1      2B 0 INZ
003700980910     D  R$$                    2      2
003800980910     D  COCO                   3      4B 0 INZ
003900980910     D  C$$                    4      4
004000951002     D KPJBA         E DS
004100031119     D ana007ds      e ds                  inz
004200031119     D tibs02ds      E DS
004300031119     D ficn92ds      E DS
004400031119     D fnlv24ds      E DS
004500031120     D ddatiute      e ds
004600031120     D azuteds       e ds                  extname(AZUTE00F)
004700031120     D tibs34ds      E DS                  inz
004800031119     D dcaf          E DS
004900131008     D tibs42ds      E DS                  inz
005000131008     D uteautds      E DS
005100100617     d dvpocont      e ds
005200031119     D* Reperimento nome PGM
005300031119     D                SDS
005400031119     D  nompgm           *PROC
005500990506     D*-------------
005600031119     D com10           s             10    inz('0000000000')
005700031119     Ddataiso          s               d   datfmt(*iso)
005800031119     Ddataeur          s               d   datfmt(*eur)
005900031119     Dkksc             s                   like(v1cfor)
006000031119     Dvpdr             s                   like(v1cpdr)
006100031120     Ddutpous          s              3
006200031120     Dcom03            s              3
006201101213     D Cmd             S             48                                         QCAEXEC
006202101213     D Cmd1            S             48    dim(1) ctdata perrcd(1)              QCAEXEC
006300951002?     *--------------------------------------------------------------*
006400951002?     *  CICLO PRINCIPALE                                            *
006500951002?     *--------------------------------------------------------------*
006600951002      *  Impostazione parametri prima videata
006700951002     C                   EXSR      INZ01
006800951002      *  Loop gestione videata
006900031120     C                   DO        *hival
007000951002     C                   EXSR      GESD01
007100031120     C                   ENDdo
007200951002?     *--------------------------------------------------------------*
007300951002?     *  INZ01: Inizializzazione prima videata                       *
007400951002?     *--------------------------------------------------------------*
007500951002     C     INZ01         BEGSR
007600951002      *
007700031119     C                   SETOFF                                       28
007800031119     C                   movea     com10         *in(40)
007900031119     C                   z-add     0             V1CPDR
008000031119     C                   z-add     0             V2CPDR
008100031119     C                   movel     *blanks       V1dPDR
008200031119     C                   movel     *blanks       V2dPDR
008300031119     C                   movel     *blanks       V1Csoc
008400031119     C                   movel     *blanks       V1dsoc
008500031119     C                   movel     *blanks       V1Cfor
008600031119     C                   movel     *blanks       V2Cfor
008700031119     C                   movel     *blanks       V1dfor
008800031119     C                   movel     *blanks       V2dfor
008900031120     C                   movel     *blanks       V1ctip
009000020509     C                   Z-ADD     0             V1Cdti
009100031119     C                   Z-ADD     0             V1Cdtf
009200031119     C                   MOVEL     *blanks       v1ctab
009300031119     C                   MOVEL     *blanks       v1dtab
009400951002     C*
009500951002     C                   ENDSR
009600951002?     *--------------------------------------------------------------*
009700951002?     *  GESD01: Gestione prima videata                              *
009800951002?     *--------------------------------------------------------------*
009900951002     C     GESD01        BEGSR
010000951002      *
010100031120     c                   if        *in28
010200031120     c                   setoff                                       28
010300031120     C                   write     y350d01
010400031120     c                   seton                                        28
010500031120     c                   end
010600031120     C*
010700031120     C                   EXFMT     y350d01
010800031119     C                   SETOFF                                       28
010900031119     C                   movea     com10         *in(40)
011000951002      *  Fine Lavoro
011100951002     C     *INKC         IFEQ      '1'
011200031120     C                   exsr      fine
011300951002     C                   END
011400020403     C*  effettua ricerche per fornitore
011500980910     c                   if        *inkd = *on
011600980910     c                   exsr      search
011700020503     C                   GOTO      FINVD1
011800980910     c                   end
011900951002      *  Controlli
012000951002     C                   EXSR      CTR01
012100951002     C   28              GOTO      FINVD1
012200031119      *  No errori
012300031119     C                   EXSR      CALL93
012400951002      *
012500951002     C     FINVD1        ENDSR
012600951002?     *--------------------------------------------------------------*
012700951002?     *  CTR01: Controlli prima videata                              *
012800951002?     *--------------------------------------------------------------*
012900951002     C     CTR01         BEGSR
013000031119     c                   do
013100031119      *  Controllo codice autotrasportatore
013200031119     c                   clear                   v1dpdr
013300031119     c                   if        v1cpdr <> 0
013400031120     c* controllo il p.o.
013500031120     c                   if        dutpous <> '046'
013600031120     c                   movel     v1cpdr        com03
013700031120     c                   if        com03 <> dutpous
013800031203     c                   seton                                        4028
013900031120     c                   leave
014000031120     c                   end
014100031120     c                   end
014200031119     c                   move      v1cpdr        vpdr
014300031119     c     kapd          chain     fiapd01l                           28
014400031119     c                   if        *in28
014500031119     c                   seton                                        40
014600031119     c                   leave
014700031119     c                   else
014800100223     c                   movel     apdrsf        v1dpdr
014900031119     c                   end
015000031119     c                   end
015100031119     c*
015200031120     c                   if        v2cpdr = 0
015300031120     c                   move      v1cpdr        v2cpdr
015400031120     c                   end
015500031120     c*
015600031119     c                   clear                   v2dpdr
015700031119     c                   if        v2cpdr <> 0
015800031120     c* controllo il p.o.
015900031120     c                   if        dutpous <> '046'
016000031120     c                   movel     v2cpdr        com03
016100031120     c                   if        com03 <> dutpous
016200031203     c                   seton                                        4128
016300031120     c                   leave
016400031120     c                   end
016500031120     c                   end
016600031119     c                   move      v2cpdr        vpdr
016700031119     c     kapd          chain     fiapd01l                           28
016800031119     c                   if        *in28
016900031119     c                   seton                                        41
017000031119     c                   leave
017100031119     c                   else
017200100223     c                   movel     apdrsf        v2dpdr
017300031119     c                   end
017400031119     c                   end
017500031119      *  Controllo codice società
017600031120     c                   if        (v1cfor <> ' ' or v2cfor <> ' ') and
017700031120     c                             v1csoc = ' '
017800031119     c                   seton                                        2842
017900031119     c                   leave
018000031120     c                   end
018100031120     c                   if        v1csoc <> ' '
018200031119     c     v1csoc        chain     ansif01l                           28
018300031119     c                   if        *in28
018400031119     c                   seton                                        42
018500031119     c                   leave
018600031119     c                   else
018700031119     c                   movel     sifdesbrev    v1dsoc
018800031119     c                   end
018900031120     c                   end
019000031119      *  Controllo codice fornitore   dal
019100031119     C                   clear                   v1dfor
019200031119     C     V1Cfor        IFne      *blanks
019300031119     C                   move      V1Cfor        kksc
019400980910     c                   movel     *blank        sogdes
019500980910     C     Kfrn          CHAIN     anfrn01l                           30
019600980910     C  N30Krco          CHAIN     anrco01l                           30
019700980910     C  N30rcosogg       CHAIN     ansog01l                           30
019800031119     C                   MOVEL(p)  sogdes        v1dfor
019900951002     C     *IN30         IFEQ      '1'
020000031119     C                   SETON                                        4328
020100031119     C                   leave
020200951002     C                   END
020300020423     c                   end
020400020404      *  Controllo codice fornitore   al
020500031119     C                   clear                   v2dfor
020600031119     C     V2Cfor        IFEQ      *blanks
020700031119     c                   movel     v1cfor        v2cfor
020800020411     C                   END
020900031119     C     V2Cfor        IFne      *blanks
021000031119     C                   move      V2Cfor        kksc
021100020403     c                   movel     *blank        sogdes
021200020403     C     Kfrn          CHAIN     anfrn01l                           30
021300020403     C  N30Krco          CHAIN     anrco01l                           30
021400020403     C  N30rcosogg       CHAIN     ansog01l                           30
021500031119     C                   MOVEL(p)  sogdes        v2dfor
021600020403     C     *IN30         IFEQ      '1'
021700031119     C                   SETON                                        4528
021800031119     C                   leave
021900020403     C                   END
022000020423     c                   end
022100020411     C*
022200031119     c                   if        v1cfor > v2cfor
022300031119     C                   SETON                                        434528
022400031119     C                   leave
022500020411     c                   end
022501130813      * controllo autorizzazione del profilo
022502130813     c                   if        wuda    >= §vpoaut
022503131008     c                   clear                   tibs42ds
022505130813     c                   if        v1cpdr <> 0
022506131008     c                   movel     v1cpdr        i42pge
022507130813     c                   else
022508130813     c                   if        v1cfor <> ' '
022509130813     c                   movel     v1cfor        cfor4             4
022510131008     c                   move      cfor4         i42pge
022511130813     c                   endif
022512130813     c                   endif
022513140121     c*                  clear                   tibs42ds
022514131008     c                   call      'TIBS42R'
022515131008     c                   parm                    tibs42ds
022516131008     c                   movel     o42uni        uteautds
022517130813     c                   if        §AUTCPRRE <> 'S'
022518130813     C                   SETON                                        4628
022519130813     C                   leave
022521130813     c                   endif
022522130813     c                   endif
022600020403     c* data da
022700020404     c                   if        v1cdti <> 0
022800031119     c     *eur          test(d)                 v1cdti                 47
022900031119     c                   if        *in47
023000031119     c     *dmy          test(d)                 v1cdti                 47
023100031119     c  n47*dmy          move      v1cdti        dataeur
023200031120     c                   else
023300031120     c                   move      v1cdti        dataeur
023400031120     c                   end
023500031119     c  n47              move      dataeur       v1cdti
023600020404     c                   end
023700020403     c* data al
023800020404     c                   if        v1cdtf <> 0
023900031119     c     *eur          test(d)                 v1cdtf                 48
024000031119     c                   if        *in48
024100031119     c     *dmy          test(d)                 v1cdtf                 48
024200031119     c  n48*dmy          move      v1cdtf        dataeur
024300031120     c                   else
024400031120     c                   move      v1cdtf        dataeur
024500031120     c                   end
024600031119     c  n48              move      dataeur       v1cdtf
024700020404     c                   end
024800031119     c*
024900031119     c                   if        *in47 or *in48
025000980910     c                   seton                                          28
025100031119     c                   leave
025200031119     c                   end
025300031119     c*
025400020404     c                   if        v1cdti <> 0
025500980910     c                   movel     v1cdti        dataeur
025600980910     c                   movel     dataeur       dataiso
025700980910     c                   movel     dataiso       wdatda            8 0
025800020404     c                   else
025900020404     c                   z-add     0             wdatda
026000020404     c                   end
026100031119     c                   if        v1cdtf <> 0
026200980910     c                   movel     v1cdtf        dataeur
026300980910     c                   movel     dataeur       dataiso
026400980910     c                   movel     dataiso       wdataa            8 0
026500031120     c                   else
026600031120     c                   z-add     99991231      wdataa
026700031119     c                   end
026800951002      *  Controllo limiti due date
026900951002     C     WDATDA        IFGT      WDATAA
027000031119     C                   SETON                                        474828
027100031119     C                   leave                                                  Data iniz.>fina.
027200951002     C                   END
027300031119      *  Controllo tabella CAF
027400031119     c                   clear                   v1dtab
027500031119     c                   if        v1ctab <> ' '
027600031119     c                   movel     v1ctab        tbeke1
027700031119     c     ktbe          chain     tntbe01l                           28
027800031120     c                   if        *in28 or tbeatb = 'A'
027900031120     c                   seton                                        4928
028000031119     c                   else
028100031119     c                   movel     tbeuni        dcaf
028200031119     c                   movel     §cafdes       v1dtab
028300031119     c                   end
028400031119     c                   end
028500031119     C                   ENDdo
028600951003      *
028700031119     C                   ENDSR
028800020404     C************************************************************
028900020404     C* RICERCHE
029000020404     C************************************************************
029100020404     C     SEARCH        BEGSR
029200980910     C* determino Riga/Colonna del cursore
029300980910     C                   MOVE      nrg           R$$
029400980910     C                   MOVE      ncl           C$$
029500980910     C                   Z-ADD     RIRI          CURR
029600980910     C                   Z-ADD     COCO          CURC
029700980910     C*
0298009809101    C                   SELECT
029900031119     C* FMTD1  - ricerca autotrasp.
030000031119    >C                   WHEn      h1nmfl = 'V1CPDR' or
030100031119    >C                             h1nmfl = 'V2CPDR'
030200031119     C                   clear                   fnlv24ds
030300031120     c* imposto il p.o.
030400031120     c                   if        dutpous <> '046'
030500031120     C                   MOVEL     dutpou        d24fil
030600031120     c                   end
030700031120     C                   MOVEL     v1ctip        d24tip
030800031119     C                   MOVEL     'R'           d24FLG
030900031119     C                   MOVEL(p)  fnlv24ds      KPJBU
031000031119     C                   CALL      'FNLV24R'
031100031119     C                   PARM                    KPJBA
031200031119     C                   MOVEL     KPJBU         fnlv24ds
031300031119     c                   if        h1nmfl = 'V1CPDR'
031400031119     c                   move      d24pdr        v1cpdr
031500031119     c                   movel     d24rsc        v1dpdr
031600031119     c                   else
031700031119     c                   move      d24pdr        v2cpdr
031800031119     c                   movel     d24rsc        v2dpdr
031900031119     c                   end
032000031119     C* FMTD1  - ricerca società
032100031119    >C     H1NMFL        WHEneq    'V1CSOC'
032200031119     C                   reset                   ana007ds
032300031119     C                   eval      opz007 = '01'
032400031119     C                   eval      kpjbu = ana007ds
032500031119     C                   call      'ANA007R'
032600031119     C                   parm                    kpjba
032700031119     C                   eval      ana007ds = kpjbu
032800031119     C                   if        err007 <> '1' and soc007 <> *blanks
032900031119     C                   eval      v1csoc = soc007
033000031119     C                   endif
033100030422     C* FMTD1  - ricerca fornitore 1 AUTOTRASPORTATORE
033200031119    >C                   WHEn      H1NMFL = 'V1CFOR' AND
033300031119     c                             v1ctip = 'A'
033400980910     C*  deposita la KPJBU
033500031119     C                   movel     v1csoc        parcsf
033600031119     C                   MOVEl     V1Dfor        PARRSC
033700980910     C                   Z-ADD     *ZEROS        PARPDR
033800020403     C                   eval      partip = 'P '
033900980910     C                   MOVEL     *BLANKS       PARFLG
034000020419     C                   MOVEL(p)  PARAM         KPJBU
034100980910     C                   CALL      'TRUL16R'
034200980910     C                   PARM                    KPJBA
034300980910     C                   MOVEL     KPJBU         PARAM
034400980910IF  2C     PARFLG        IFEQ      '3'
034500031119     C                   MOVEL     *BLANKS       V1Dfor
034600031119     C                   SETON                                        4328
034700980910X   2C                   ELSE
034800031119     C                   MOVEl     *zeros        V1Cfor
034900031119     C                   MOVE      PARPDR        V1Cfor
035000031119     C                   MOVEL     PARRSC        V1Dfor
035100980910E   2C                   ENDIF
035200030422     C* FMTD1  - ricerca fornitore 2 AUTOTRASPORTATRE
035300031119    >C                   WHEn      H1NMFL = 'V2CFOR' AND
035400031119     c                             v1ctip = 'A'
035500020403     C*  deposita la KPJBU
035600020404     C                   MOVEl     V2DPDR        PARRSC
035700020403     C                   Z-ADD     *ZEROS        PARPDR
035800020403     C                   eval      partip = 'P '
035900031119     C                   movel     v1csoc        parcsf
036000020403     C                   MOVEL     *BLANKS       PARFLG
036100020419     C                   MOVEL(P)  PARAM         KPJBU
036200020403     C                   CALL      'TRUL16R'
036300020403     C                   PARM                    KPJBA
036400020403     C                   MOVEL     KPJBU         PARAM
036500020403IF  2C     PARFLG        IFEQ      '3'
036600031119     C                   MOVEL     *BLANKS       V2Dfor
036700031119     C                   SETON                                        4428
036800020403X   2C                   ELSE
036900031119     C                   MOVEl     *zeros        V2Cfor
037000031119     C                   MOVE      PARPDR        V2Cfor
037100031119     C                   MOVEL     PARRSC        V2Dfor
037200020403E   2C                   ENDIF
037300031202     C* FMTD1  - ricerca tabella AUTOFATTURAZIONE EXTRA
037400031119    >C     H1NMFL        WHEneq    'V1CTAB'
037500031119     C                   clear                   Tibs02DS
037600031119     C                   eval      t02tla = 'L'
037700031119     C                   movel     'R'           t02mod
037800031119     C                   movel     'CAF'         t02cod
037900031119     C                   CALL      'TIBS02R'
038000031119     C                   parm                    KPJBA
038100031119     C                   parm                    TIBS02DS
038200031119    2C                   IF        T02err = *blanks
038300031119     c                   movel     t02ke1        v1ctab
038400031119     c                   movel     t02uni        dcaf
038500031119     c                   movel     §cafdes       v1dtab
038600031119     c                   end
038700020510     C                   ENDsl
038800980910     C* imposto pos. del cursore
038900980910     C                   Z-ADD     CURR          H1RIGA
039000980910     C                   Z-ADD     CURC          H1COLO
039100980910     C                   ENDsr
039200951006?     *--------------------------------------------------------------*
039300031119?     *  CALL93: Controlli prima videata                             *
039400951006?     *--------------------------------------------------------------*
039500031119     C     CALL93        BEGSR
039600951006      *
039700031119     c                   clear                   ficn92ds
039800031119     C                   movel     v1ctip        f92tip
039900031119     C                   movel     v1ctab        f92tab
040000031119     C                   movel     v1csoc        f92soc
040100031119     C                   movel     v1cpdr        f92pd1
040200031124     c                   if        dutpou <> 46
040300031124     C                   movel     dutpou        f92pd1
040400031124     c                   end
040500031119     c                   if        v2cpdr = 0
040600031119     c                   move      *all'9'       f92pd2
040700031124     c                   if        dutpou <> 46
040800031124     C                   movel     dutpou        f92pd2
040900031124     c                   end
041000020423     c                   else
041100031119     C                   move      V2CPDR        f92pd2
041200020423     c                   end
041300031119     c                   if        v1cfor = *blanks
041400031119     c                   movel     *all'0'       f92fo1
041500031119     c                   else
041600031119     C                   movel     v1cfor        f92fo1
041700031119     c                   end
041800031119     c                   if        v2cfor = *blanks
041900031119     c                   movel     *all'9'       f92fo2
042000031119     c                   else
042100031119     C                   movel     V2Cfor        f92fo2
042200031119     c                   end
042300031119     C                   Z-ADD     WDATDA        f92DTI
042400031119     C                   Z-ADD     WDATAA        f92DTF
042500031119     C                   MOVEL(p)  ficn92ds      KPJBU
042600030128     c* AUTOTRASPORTATORI
042700031119     C                   CALL      'FICN93R'
042800951006     C                   PARM                    KPJBA
042900031119     C                   MOVEL     KPJBU         ficn92ds
043000951006      * Se premuto F3 vado a fine lavoro
043100031119     C     f92CMD        IFEQ      '03'
043200031120     C                   exsr      fine
043300951006     C                   ELSE
043400031119     C     f92CMD        IFNE      '12'
043500951006     C                   EXSR      INZ01
043600951006     C                   END
043700960402     C                   END
043800951006      *
043900951006     C                   ENDSR
044000980910     C*----------------------------------------------------*
044100031120     C* fine
044200980910     C*----------------------------------------------------*
044300031120     C     fine          begsr
044400980910     C*
044500031120     c                   seton                                        lr
044600031120     c                   return
044700031120     c*
044800031120     C                   ENDSR
044900951006?     *--------------------------------------------------------------*
045000951006?     * *INZSR: Operazioni iniziali                                  *
045100951006?     *--------------------------------------------------------------*
045200951009     C     *INZSR        BEGSR
045300951006      *
045400951006     C     *ENTRY        PLIST
045500951006     C                   PARM                    KPJBA
045600031120     c     *dtaara       define    §azute        azuteds
045700031120     c     *dtaara       define    §datiute      ddatiute
045800031120     C                   in(E)     *dtaara
045900031120     C                   IF        %Error  or  RSUT = *blanks
046000031120     C                   call      'TIBS34R'
046100031120     C                   parm                    Tibs34Ds
046200031120     C                   in        *dtaara
046300031120     c                   ENDIF
046400100617     c                   move      *date         dataiso
046500100617     c                   move      dataiso       wuda              8 0
046600100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
046700100617     c                   clear                   tibs02ds
046800100617     c                   clear                   dvpocont
046900100617     c                   eval      t02mod = 'C'
047000100617     c                   eval      t02sif = knsif
047100100617     c                   eval      t02cod = 'VPO'
047200100617     c                   eval      t02ke1 = 'CONT'
047300100617     c                   call      'TIBS02R'
047400100617     c                   parm                    kpjba
047500100617     c                   parm                    tibs02ds
047600100617    2c                   if        t02err = *blanks
047700100617     c                   eval      dvpocont  = t02uni
047800100617    2c                   endif
049100100318     c
049200031120     c                   movel     dutpou        dutpous
049300100729     C                   movel     '000400'      forita            6
049400020423     C     Kfrn          KLIST
049500031119     C                   KFLD                    v1csoc
049600020423     C                   KFLD                    kksc
049700980910     C     Krco          KLIST
049800031119     C                   KFLD                    v1csoc
049900020417     C                   KFLD                    forita
050000980910     C                   KFLD                    KKSC
050100031119     C     Ktbe          KLIST
050200031119     C                   KFLD                    tbecod
050300031119     C                   KFLD                    tbeke1
050400031119     c                   eval      tbecod='CAF'
050500031119     C     Kapd          KLIST
050600031119     C                   KFLD                    v1ctip
050700031119     C                   KFLD                    vpdr
050800951006      *  Definisco variabili
050900980910     C                   Z-ADD     0             CURR              2 0
051000980910     C                   Z-ADD     0             CURC              2 0
051001101213      *
051002101213     c                   exsr      comando
051100980910      *
051200980910     c                   endsr
051201101213      *---------------------------------------------------------------------
051202101213     C     comando       BEGSR
051203101213      *---------------------------------------------------------------------
051205101213     C                   movea     cmd1          cmd
051209101213     C                   movel(p)  cmd           comman
051210101213     C                   call      'QCMDEXC'
051211101213     C                   parm                    comman           80
051212101213     C                   parm      48            lung             15 5
051213101213     c                   open      ansif01l
051214101213     C*
051215101213     C                   ENDSR
051216101213      *---------------------------------------------------------------------
051300101213**         CMD1
051400101213OVRDBF FILE(ansif01L) TOFILE(PJBARGRU/ansif01l)

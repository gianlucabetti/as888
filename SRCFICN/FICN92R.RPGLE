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
006300101213     D Cmd             S             48                                         QCAEXEC
006400101213     D Cmd1            S             48    dim(1) ctdata perrcd(1)              QCAEXEC
006500951002?     *--------------------------------------------------------------*
006600951002?     *  CICLO PRINCIPALE                                            *
006700951002?     *--------------------------------------------------------------*
006800951002      *  Impostazione parametri prima videata
006900951002     C                   EXSR      INZ01
007000951002      *  Loop gestione videata
007100031120     C                   DO        *hival
007200951002     C                   EXSR      GESD01
007300031120     C                   ENDdo
007400951002?     *--------------------------------------------------------------*
007500951002?     *  INZ01: Inizializzazione prima videata                       *
007600951002?     *--------------------------------------------------------------*
007700951002     C     INZ01         BEGSR
007800951002      *
007900031119     C                   SETOFF                                       28
008000031119     C                   movea     com10         *in(40)
008100031119     C                   z-add     0             V1CPDR
008200031119     C                   z-add     0             V2CPDR
008300031119     C                   movel     *blanks       V1dPDR
008400031119     C                   movel     *blanks       V2dPDR
008500031119     C                   movel     *blanks       V1Csoc
008600031119     C                   movel     *blanks       V1dsoc
008700031119     C                   movel     *blanks       V1Cfor
008800031119     C                   movel     *blanks       V2Cfor
008900031119     C                   movel     *blanks       V1dfor
009000031119     C                   movel     *blanks       V2dfor
009100031120     C                   movel     *blanks       V1ctip
009200020509     C                   Z-ADD     0             V1Cdti
009300031119     C                   Z-ADD     0             V1Cdtf
009400031119     C                   MOVEL     *blanks       v1ctab
009500031119     C                   MOVEL     *blanks       v1dtab
009600951002     C*
009700951002     C                   ENDSR
009800951002?     *--------------------------------------------------------------*
009900951002?     *  GESD01: Gestione prima videata                              *
010000951002?     *--------------------------------------------------------------*
010100951002     C     GESD01        BEGSR
010200951002      *
010300031120     c                   if        *in28
010400031120     c                   setoff                                       28
010500031120     C                   write     y350d01
010600031120     c                   seton                                        28
010700031120     c                   end
010800031120     C*
010900031120     C                   EXFMT     y350d01
011000031119     C                   SETOFF                                       28
011100031119     C                   movea     com10         *in(40)
011200951002      *  Fine Lavoro
011300951002     C     *INKC         IFEQ      '1'
011400031120     C                   exsr      fine
011500951002     C                   END
011600020403     C*  effettua ricerche per fornitore
011700980910     c                   if        *inkd = *on
011800980910     c                   exsr      search
011900020503     C                   GOTO      FINVD1
012000980910     c                   end
012100951002      *  Controlli
012200951002     C                   EXSR      CTR01
012300951002     C   28              GOTO      FINVD1
012400031119      *  No errori
012500031119     C                   EXSR      CALL93
012600951002      *
012700951002     C     FINVD1        ENDSR
012800951002?     *--------------------------------------------------------------*
012900951002?     *  CTR01: Controlli prima videata                              *
013000951002?     *--------------------------------------------------------------*
013100951002     C     CTR01         BEGSR
013200031119     c                   do
013300031119      *  Controllo codice autotrasportatore
013400031119     c                   clear                   v1dpdr
013500031119     c                   if        v1cpdr <> 0
013600031120     c* controllo il p.o.
013700160909     c*m                 if        dutpous <> '046'
013800160909     c*m                 movel     v1cpdr        com03
013900160909     c*m                 if        com03 <> dutpous
014000160909     c*m                 seton                                        4028
014100160909     c*m                 leave
014200160909     c*m                 end
014300160909     c*m                 end
014400031119     c                   move      v1cpdr        vpdr
014500031119     c     kapd          chain     fiapd01l                           28
014600031119     c                   if        *in28
014700031119     c                   seton                                        40
014800031119     c                   leave
014900031119     c                   else
015000100223     c                   movel     apdrsf        v1dpdr
015100031119     c                   end
015200031119     c                   end
015300031119     c*
015400031120     c                   if        v2cpdr = 0
015500031120     c                   move      v1cpdr        v2cpdr
015600031120     c                   end
015700031120     c*
015800031119     c                   clear                   v2dpdr
015900031119     c                   if        v2cpdr <> 0
016000031120     c* controllo il p.o.
016100160909     c*m                 if        dutpous <> '046'
016200160909     c*m                 movel     v2cpdr        com03
016300160909     c*m                 if        com03 <> dutpous
016400160909     c*m                 seton                                        4128
016500160909     c*m                 leave
016600160909     c*m                 end
016700160909     c*m                 end
016800031119     c                   move      v2cpdr        vpdr
016900031119     c     kapd          chain     fiapd01l                           28
017000031119     c                   if        *in28
017100031119     c                   seton                                        41
017200031119     c                   leave
017300031119     c                   else
017400100223     c                   movel     apdrsf        v2dpdr
017500031119     c                   end
017600031119     c                   end
017700031119      *  Controllo codice società
017800031120     c                   if        (v1cfor <> ' ' or v2cfor <> ' ') and
017900031120     c                             v1csoc = ' '
018000031119     c                   seton                                        2842
018100031119     c                   leave
018200031120     c                   end
018300031120     c                   if        v1csoc <> ' '
018400031119     c     v1csoc        chain     ansif01l                           28
018500031119     c                   if        *in28
018600031119     c                   seton                                        42
018700031119     c                   leave
018800031119     c                   else
018900031119     c                   movel     sifdesbrev    v1dsoc
019000031119     c                   end
019100031120     c                   end
019200031119      *  Controllo codice fornitore   dal
019300031119     C                   clear                   v1dfor
019400031119     C     V1Cfor        IFne      *blanks
019500031119     C                   move      V1Cfor        kksc
019600980910     c                   movel     *blank        sogdes
019700980910     C     Kfrn          CHAIN     anfrn01l                           30
019800980910     C  N30Krco          CHAIN     anrco01l                           30
019900980910     C  N30rcosogg       CHAIN     ansog01l                           30
020000031119     C                   MOVEL(p)  sogdes        v1dfor
020100951002     C     *IN30         IFEQ      '1'
020200031119     C                   SETON                                        4328
020300031119     C                   leave
020400951002     C                   END
020500020423     c                   end
020600020404      *  Controllo codice fornitore   al
020700031119     C                   clear                   v2dfor
020800031119     C     V2Cfor        IFEQ      *blanks
020900031119     c                   movel     v1cfor        v2cfor
021000020411     C                   END
021100031119     C     V2Cfor        IFne      *blanks
021200031119     C                   move      V2Cfor        kksc
021300020403     c                   movel     *blank        sogdes
021400020403     C     Kfrn          CHAIN     anfrn01l                           30
021500020403     C  N30Krco          CHAIN     anrco01l                           30
021600020403     C  N30rcosogg       CHAIN     ansog01l                           30
021700031119     C                   MOVEL(p)  sogdes        v2dfor
021800020403     C     *IN30         IFEQ      '1'
021900031119     C                   SETON                                        4528
022000031119     C                   leave
022100020403     C                   END
022200020423     c                   end
022300020411     C*
022400031119     c                   if        v1cfor > v2cfor
022500031119     C                   SETON                                        434528
022600031119     C                   leave
022700020411     c                   end
022800130813      * controllo autorizzazione del profilo
022900130813     c                   if        wuda    >= §vpoaut
023000131008     c                   clear                   tibs42ds
023100130813     c                   if        v1cpdr <> 0
023200131008     c                   movel     v1cpdr        i42pge
023300130813     c                   else
023400130813     c                   if        v1cfor <> ' '
023500130813     c                   movel     v1cfor        cfor4             4
023600131008     c                   move      cfor4         i42pge
023700130813     c                   endif
023800130813     c                   endif
023900140121     c*                  clear                   tibs42ds
024000131008     c                   call      'TIBS42R'
024100131008     c                   parm                    tibs42ds
024200131008     c                   movel     o42uni        uteautds
024300130813     c                   if        §AUTCPRRE <> 'S'
024400130813     C                   SETON                                        4628
024500130813     C                   leave
024600130813     c                   endif
024700130813     c                   endif
024800020403     c* data da
024900020404     c                   if        v1cdti <> 0
025000031119     c     *eur          test(d)                 v1cdti                 47
025100031119     c                   if        *in47
025200031119     c     *dmy          test(d)                 v1cdti                 47
025300031119     c  n47*dmy          move      v1cdti        dataeur
025400031120     c                   else
025500031120     c                   move      v1cdti        dataeur
025600031120     c                   end
025700031119     c  n47              move      dataeur       v1cdti
025800020404     c                   end
025900020403     c* data al
026000020404     c                   if        v1cdtf <> 0
026100031119     c     *eur          test(d)                 v1cdtf                 48
026200031119     c                   if        *in48
026300031119     c     *dmy          test(d)                 v1cdtf                 48
026400031119     c  n48*dmy          move      v1cdtf        dataeur
026500031120     c                   else
026600031120     c                   move      v1cdtf        dataeur
026700031120     c                   end
026800031119     c  n48              move      dataeur       v1cdtf
026900020404     c                   end
027000031119     c*
027100031119     c                   if        *in47 or *in48
027200980910     c                   seton                                          28
027300031119     c                   leave
027400031119     c                   end
027500031119     c*
027600020404     c                   if        v1cdti <> 0
027700980910     c                   movel     v1cdti        dataeur
027800980910     c                   movel     dataeur       dataiso
027900980910     c                   movel     dataiso       wdatda            8 0
028000020404     c                   else
028100020404     c                   z-add     0             wdatda
028200020404     c                   end
028300031119     c                   if        v1cdtf <> 0
028400980910     c                   movel     v1cdtf        dataeur
028500980910     c                   movel     dataeur       dataiso
028600980910     c                   movel     dataiso       wdataa            8 0
028700031120     c                   else
028800031120     c                   z-add     99991231      wdataa
028900031119     c                   end
029000951002      *  Controllo limiti due date
029100951002     C     WDATDA        IFGT      WDATAA
029200031119     C                   SETON                                        474828
029300031119     C                   leave                                                  Data iniz.>fina.
029400951002     C                   END
029500031119      *  Controllo tabella CAF
029600031119     c                   clear                   v1dtab
029700031119     c                   if        v1ctab <> ' '
029800031119     c                   movel     v1ctab        tbeke1
029900031119     c     ktbe          chain     tntbe01l                           28
030000031120     c                   if        *in28 or tbeatb = 'A'
030100031120     c                   seton                                        4928
030200031119     c                   else
030300031119     c                   movel     tbeuni        dcaf
030400031119     c                   movel     §cafdes       v1dtab
030500031119     c                   end
030600031119     c                   end
030700031119     C                   ENDdo
030800951003      *
030900031119     C                   ENDSR
031000020404     C************************************************************
031100020404     C* RICERCHE
031200020404     C************************************************************
031300020404     C     SEARCH        BEGSR
031400980910     C* determino Riga/Colonna del cursore
031500980910     C                   MOVE      nrg           R$$
031600980910     C                   MOVE      ncl           C$$
031700980910     C                   Z-ADD     RIRI          CURR
031800980910     C                   Z-ADD     COCO          CURC
031900980910     C*
0320009809101    C                   SELECT
032100031119     C* FMTD1  - ricerca autotrasp.
032200031119    >C                   WHEn      h1nmfl = 'V1CPDR' or
032300031119    >C                             h1nmfl = 'V2CPDR'
032400031119     C                   clear                   fnlv24ds
032500031120     c* imposto il p.o.
032600160909     c*m                 if        dutpous <> '046'
032700160909     C*m                 MOVEL     dutpou        d24fil
032800160909     c*m                 end
032900160909     c                   if        v1cpdr > 0
033000160909     C                   MOVEL     v1cpdr        d24fil
033100160909     c                   else
033200160909     C                   MOVEL     dutpou        d24fil
033300160909     c                   endif
033400031120     C                   MOVEL     v1ctip        d24tip
033500031119     C                   MOVEL     'R'           d24FLG
033600031119     C                   MOVEL(p)  fnlv24ds      KPJBU
033700031119     C                   CALL      'FNLV24R'
033800031119     C                   PARM                    KPJBA
033900031119     C                   MOVEL     KPJBU         fnlv24ds
034000031119     c                   if        h1nmfl = 'V1CPDR'
034100031119     c                   move      d24pdr        v1cpdr
034200031119     c                   movel     d24rsc        v1dpdr
034300031119     c                   else
034400031119     c                   move      d24pdr        v2cpdr
034500031119     c                   movel     d24rsc        v2dpdr
034600031119     c                   end
034700031119     C* FMTD1  - ricerca società
034800031119    >C     H1NMFL        WHEneq    'V1CSOC'
034900031119     C                   reset                   ana007ds
035000031119     C                   eval      opz007 = '01'
035100031119     C                   eval      kpjbu = ana007ds
035200031119     C                   call      'ANA007R'
035300031119     C                   parm                    kpjba
035400031119     C                   eval      ana007ds = kpjbu
035500031119     C                   if        err007 <> '1' and soc007 <> *blanks
035600031119     C                   eval      v1csoc = soc007
035700031119     C                   endif
035800030422     C* FMTD1  - ricerca fornitore 1 AUTOTRASPORTATORE
035900031119    >C                   WHEn      H1NMFL = 'V1CFOR' AND
036000031119     c                             v1ctip = 'A'
036100980910     C*  deposita la KPJBU
036200031119     C                   movel     v1csoc        parcsf
036300031119     C                   MOVEl     V1Dfor        PARRSC
036400980910     C                   Z-ADD     *ZEROS        PARPDR
036500020403     C                   eval      partip = 'P '
036600980910     C                   MOVEL     *BLANKS       PARFLG
036700020419     C                   MOVEL(p)  PARAM         KPJBU
036800980910     C                   CALL      'TRUL16R'
036900980910     C                   PARM                    KPJBA
037000980910     C                   MOVEL     KPJBU         PARAM
037100980910IF  2C     PARFLG        IFEQ      '3'
037200031119     C                   MOVEL     *BLANKS       V1Dfor
037300031119     C                   SETON                                        4328
037400980910X   2C                   ELSE
037500031119     C                   MOVEl     *zeros        V1Cfor
037600031119     C                   MOVE      PARPDR        V1Cfor
037700031119     C                   MOVEL     PARRSC        V1Dfor
037800980910E   2C                   ENDIF
037900030422     C* FMTD1  - ricerca fornitore 2 AUTOTRASPORTATRE
038000031119    >C                   WHEn      H1NMFL = 'V2CFOR' AND
038100031119     c                             v1ctip = 'A'
038200020403     C*  deposita la KPJBU
038300020404     C                   MOVEl     V2DPDR        PARRSC
038400020403     C                   Z-ADD     *ZEROS        PARPDR
038500020403     C                   eval      partip = 'P '
038600031119     C                   movel     v1csoc        parcsf
038700020403     C                   MOVEL     *BLANKS       PARFLG
038800020419     C                   MOVEL(P)  PARAM         KPJBU
038900020403     C                   CALL      'TRUL16R'
039000020403     C                   PARM                    KPJBA
039100020403     C                   MOVEL     KPJBU         PARAM
039200020403IF  2C     PARFLG        IFEQ      '3'
039300031119     C                   MOVEL     *BLANKS       V2Dfor
039400031119     C                   SETON                                        4428
039500020403X   2C                   ELSE
039600031119     C                   MOVEl     *zeros        V2Cfor
039700031119     C                   MOVE      PARPDR        V2Cfor
039800031119     C                   MOVEL     PARRSC        V2Dfor
039900020403E   2C                   ENDIF
040000031202     C* FMTD1  - ricerca tabella AUTOFATTURAZIONE EXTRA
040100031119    >C     H1NMFL        WHEneq    'V1CTAB'
040200031119     C                   clear                   Tibs02DS
040300031119     C                   eval      t02tla = 'L'
040400031119     C                   movel     'R'           t02mod
040500031119     C                   movel     'CAF'         t02cod
040600031119     C                   CALL      'TIBS02R'
040700031119     C                   parm                    KPJBA
040800031119     C                   parm                    TIBS02DS
040900031119    2C                   IF        T02err = *blanks
041000031119     c                   movel     t02ke1        v1ctab
041100031119     c                   movel     t02uni        dcaf
041200031119     c                   movel     §cafdes       v1dtab
041300031119     c                   end
041400020510     C                   ENDsl
041500980910     C* imposto pos. del cursore
041600980910     C                   Z-ADD     CURR          H1RIGA
041700980910     C                   Z-ADD     CURC          H1COLO
041800980910     C                   ENDsr
041900951006?     *--------------------------------------------------------------*
042000031119?     *  CALL93: Controlli prima videata                             *
042100951006?     *--------------------------------------------------------------*
042200031119     C     CALL93        BEGSR
042300951006      *
042400031119     c                   clear                   ficn92ds
042500031119     C                   movel     v1ctip        f92tip
042600031119     C                   movel     v1ctab        f92tab
042700031119     C                   movel     v1csoc        f92soc
042800031119     C                   movel     v1cpdr        f92pd1
042900031124     c                   if        dutpou <> 46
043000031124     C                   movel     dutpou        f92pd1
043100031124     c                   end
043200031119     c                   if        v2cpdr = 0
043300031119     c                   move      *all'9'       f92pd2
043400031124     c                   if        dutpou <> 46
043500031124     C                   movel     dutpou        f92pd2
043600031124     c                   end
043700020423     c                   else
043800031119     C                   move      V2CPDR        f92pd2
043900020423     c                   end
044000031119     c                   if        v1cfor = *blanks
044100031119     c                   movel     *all'0'       f92fo1
044200031119     c                   else
044300031119     C                   movel     v1cfor        f92fo1
044400031119     c                   end
044500031119     c                   if        v2cfor = *blanks
044600031119     c                   movel     *all'9'       f92fo2
044700031119     c                   else
044800031119     C                   movel     V2Cfor        f92fo2
044900031119     c                   end
045000031119     C                   Z-ADD     WDATDA        f92DTI
045100031119     C                   Z-ADD     WDATAA        f92DTF
045200031119     C                   MOVEL(p)  ficn92ds      KPJBU
045300030128     c* AUTOTRASPORTATORI
045400031119     C                   CALL      'FICN93R'
045500951006     C                   PARM                    KPJBA
045600031119     C                   MOVEL     KPJBU         ficn92ds
045700951006      * Se premuto F3 vado a fine lavoro
045800031119     C     f92CMD        IFEQ      '03'
045900031120     C                   exsr      fine
046000951006     C                   ELSE
046100031119     C     f92CMD        IFNE      '12'
046200951006     C                   EXSR      INZ01
046300951006     C                   END
046400960402     C                   END
046500951006      *
046600951006     C                   ENDSR
046700980910     C*----------------------------------------------------*
046800031120     C* fine
046900980910     C*----------------------------------------------------*
047000031120     C     fine          begsr
047100980910     C*
047200031120     c                   seton                                        lr
047300031120     c                   return
047400031120     c*
047500031120     C                   ENDSR
047600951006?     *--------------------------------------------------------------*
047700951006?     * *INZSR: Operazioni iniziali                                  *
047800951006?     *--------------------------------------------------------------*
047900951009     C     *INZSR        BEGSR
048000951006      *
048100951006     C     *ENTRY        PLIST
048200951006     C                   PARM                    KPJBA
048300031120     c     *dtaara       define    §azute        azuteds
048400031120     c     *dtaara       define    §datiute      ddatiute
048500031120     C                   in(E)     *dtaara
048600031120     C                   IF        %Error  or  RSUT = *blanks
048700031120     C                   call      'TIBS34R'
048800031120     C                   parm                    Tibs34Ds
048900031120     C                   in        *dtaara
049000031120     c                   ENDIF
049100100617     c                   move      *date         dataiso
049200100617     c                   move      dataiso       wuda              8 0
049300100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
049400100617     c                   clear                   tibs02ds
049500100617     c                   clear                   dvpocont
049600100617     c                   eval      t02mod = 'C'
049700100617     c                   eval      t02sif = knsif
049800100617     c                   eval      t02cod = 'VPO'
049900100617     c                   eval      t02ke1 = 'CONT'
050000100617     c                   call      'TIBS02R'
050100100617     c                   parm                    kpjba
050200100617     c                   parm                    tibs02ds
050300100617    2c                   if        t02err = *blanks
050400100617     c                   eval      dvpocont  = t02uni
050500100617    2c                   endif
050600100318     c
050700031120     c                   movel     dutpou        dutpous
050800100729     C                   movel     '000400'      forita            6
050900020423     C     Kfrn          KLIST
051000031119     C                   KFLD                    v1csoc
051100020423     C                   KFLD                    kksc
051200980910     C     Krco          KLIST
051300031119     C                   KFLD                    v1csoc
051400020417     C                   KFLD                    forita
051500980910     C                   KFLD                    KKSC
051600031119     C     Ktbe          KLIST
051700031119     C                   KFLD                    tbecod
051800031119     C                   KFLD                    tbeke1
051900031119     c                   eval      tbecod='CAF'
052000031119     C     Kapd          KLIST
052100031119     C                   KFLD                    v1ctip
052200031119     C                   KFLD                    vpdr
052300951006      *  Definisco variabili
052400980910     C                   Z-ADD     0             CURR              2 0
052500980910     C                   Z-ADD     0             CURC              2 0
052600101213      *
052700101213     c                   exsr      comando
052800980910      *
052900980910     c                   endsr
053000101213      *---------------------------------------------------------------------
053100101213     C     comando       BEGSR
053200101213      *---------------------------------------------------------------------
053300101213     C                   movea     cmd1          cmd
053400101213     C                   movel(p)  cmd           comman
053500101213     C                   call      'QCMDEXC'
053600101213     C                   parm                    comman           80
053700101213     C                   parm      48            lung             15 5
053800101213     c                   open      ansif01l
053900101213     C*
054000101213     C                   ENDSR
054100101213      *---------------------------------------------------------------------
054200101213**         CMD1
054300101213OVRDBF FILE(ansif01L) TOFILE(PJBARGRU/ansif01l)

000100980506      *------------------------------------------------------------------------*
000200980527      * GESTIONE SCRITTURA FASI                                                *
000300981217      *------------------------------------------------------------------------*
000400981217      * Fasi fissate a programma
000500040330      * 050 Sollecito telefonico reso avaria wfaav
000600981217      * 200 Contatto telefonico wfaco
000700990114      * 215 Esito Contatto telefonico wfaes
000800980506      *------------------------------------------------------------------------*
000900980506     H DECEDIT('0,') DATEDIT(*DMY.)
001000980506      *--------------------------------------------
001100980528     Ffidn04D   CF   E             WORKSTN
001200980601     Ffndfa01L  IF   E           k DISK
001300980604     Ffndka01L  IF   E           k DISK
001400980602     Ffndct01L  uF   E           k DISK
001500990114     Ffndcs01L  iF   E           k DISK
001600980601     Fazorg01L  IF   E           k DISK
001700980605     Ftabel00f  IF   E           k DISK
001800980728     Ffnspe01l  IF   E           k DISK
001900990114     Ffndcf01l  uf A E           k DISK
002000980728     FFNBLP01L  IF   E           K DISK
002100991115     FFIAR601L  IF   E           K DISK
002200981105     FFNARB01L  IF   E           K DISK
002300981126      * sede
002400990806     fTITAS30C  IF   E           K DISK     usropn
002500981126     fTNTMD01L  IF   E           K DISK     usropn
002600990114      ****************************************************************
002700990114      *  RIEPILOGO INDICATORI
002800990114      ***************************************************************
002900990114      * 21/27- COMODO
003000990114      * 28   - ERRORE GENERICO
003100990114      * 30/31- COMODO
003200990114      * 40/41- ERRORI A VIDEO
003300990114      * 80   - F18 Lampeggiante ci sono delle note
003400990114      * 90   - COMODO
003500990114      * 99   - COMODO
003600980506      *--------------------------------------------
003700981105     D MSG             S             78    DIM(7) CTDATA PERRCD(1)              MSG VIDEO
003800980528      *--------------------------------------------
003900980616     D                SDS
004000980616     D  VTCPGM                 1     10
004100980616      *
004200980528     D Fidn00ds      E DS
004300980610     d  dsnumca                6     19
004400980528     D Fidn05ds      E DS
004500990223     D Fidn53ds      E DS
004600980610     D DSDN10        E DS                  EXTNAME(FIDN10DS)
004700990114     D DSDN42        E DS                  EXTNAME(FIDN42DS)
004800980528     D fndfa         E DS                  EXTNAME(FNDFA00F)
004900981230     D Ddct01        E DS
005000981126     D DSTB          E DS
005100990311     D TIBS69DS      E DS
005200990311     d DS_aco        E DS                  extname(CNACO00F)
005300990311     d DS_ind        E DS                  extname(CNIND00F)
005400990311     d DS_clp        E DS                  extname(CNCLP00F)
005500990311     d DS_cls        E DS                  extname(FNCLS00F)
005600980507      *--------------------------------------------
005700980507     D KPJBA         E DS
005800980508     D CNCR80        E DS
005900980507     D UT§DSE0F      E DS
006000980507      *--------------------------------------------
006100980527     D W0140           S             14  0
006200980507     D WDTGIO          S              8  0
006300980805     D dateu           s              8  0
006400980507     D WORA            S              6  0
006500980602     D ora             S              2  0
006600980602     D min             S              2  0
006700060118     D com04           S              4  0
006800981126     D flgtb1          S              1
006900981230     D W_porto         S              1
007000980728     d tipkey          s                   like(spefls)
007100980728     d codluo          s                   like(specod)
007200980528     D WDATAC          S                   LIKE(V1Cdfs)
007300990111     D WDAspe          S                   LIKE(V1Cdfs)
007400980528     D Wdfs            S                   LIKE(dcfdfc)
007500980528     D ccc             S                   LIKE(acokcc)
007600980604     D Kaac            S                   LIKE(dctaac)
007700980528     D Kfil            S                   LIKE(V1Cpog)
007800980528     D Knca            S                   LIKE(V1Cnca)
007900980728     D vtcksm          S                   LIKE(v1cksm)
008000980728     D vtcksd          S                   LIKE(v1cksm)
008100991116     D trcar6          S                   LIKE(ar6trc)
008200981217     D Wfaco           S                   LIKE(dcffca) INZ(200)
008300990114     D Wfaes           S                   LIKE(dcffca) INZ(215)
008400990126     D Wfaav           S                   LIKE(dcffca) INZ(050)
008500981230     D wtrc            S                   LIKE(dkatrc)
008600980528      *
008700980528     D WLBDAT          DS                  INZ
008800980528     D  G02DAT                 1      8  0
008900980528     D  G02INV                 9     16  0
009000980528     D  G02ERR                17     17
009100980528     D  G02TGI                18     22  0
009200980728     D Ds3a          E DS
009300981217      *  titolo videata (lunghezza massima 34)
009400981217     D TIT_1           C                   CONST('***    CONTATTO  TELEFONICO  -
009500981217     D                                       ***')
009600981217     D TIT_2           C                   CONST('***    SOLLECITO TELEFONICO  -
009700981217     D                                       ***')
009800980528      *
009900980728     IFNBLP000
010000980728     I              BLPATB                      ARBATB
010100980728     I              BLPAAS                      ARBAAS
010200980728     I              BLPLNP                      ARBLNP
010300980728     I              BLPNRS                      ARBNRS
010400980728     I              BLPNSP                      ARBNSP
010500980728     I              BLPCBO                      ARBCBO
010600980728     I              BLPMGS                      ARBMGS
010700980728     I              BLPFNS                      ARBFNS
010800980728     I              BLPSCL                      ARBSCL
010900980728     I              BLPFBR                      ARBFBR
011000980728     I              BLPFDN                      ARBFDN
011100980728     I              BLPLNA                      ARBLNA
011200980728     I              BLPZNC                      ARBZNC
011300980728     I              BLPKSC                      ARBKSC
011400980728     I              BLPRSM                      ARBRSM
011500980728     I              BLPINM                      ARBINM
011600980728     I              BLPCAM                      ARBCAM
011700980728     I              BLPLOM                      ARBLOM
011800980728     I              BLPNZM                      ARBNZM
011900980728     I              BLPPRM                      ARBPRM
012000980728     I              BLPFAP                      ARBFAP
012100980728     I              BLPCPI                      ARBCPI
012200980728     I              BLPRSD                      ARBRSD
012300980728     I              BLPIND                      ARBIND
012400980728     I              BLPCAD                      ARBCAD
012500980728     I              BLPLOD                      ARBLOD
012600980728     I              BLPNZD                      ARBNZD
012700980728     I              BLPPRD                      ARBPRD
012800980728     I              BLPFIN                      ARBFIN
012900980728     I              BLPGC1                      ARBGC1
013000980728     I              BLPGC2                      ARBGC2
013100980728     I              BLPCTR                      ARBCTR
013200980728     I              BLPCTS                      ARBCTS
013300980728     I              BLPFTM                      ARBFTM
013400980728     I              BLPTSP                      ARBTSP
013500980728     I              BLPNAS                      ARBNAS
013600980728     I              BLPNCL                      ARBNCL
013700980728     I              BLPNCP                      ARBNCP
013800980728     I              BLPPKC                      ARBPKC
013900980728     I              BLPPKB                      ARBPKB
014000980728     I              BLPPKF                      ARBPKF
014100980728     I              BLPNCR                      ARBNCR
014200980728     I              BLPVLC                      ARBVLC
014300980728     I              BLPFVB                      ARBFVB
014400980728     I              BLPVLB                      ARBVLB
014500980728     I              BLPFVF                      ARBFVF
014600980728     I              BLPVLF                      ARBVLF
014700980728     I              BLPQFT                      ARBQFT
014800980728     I              BLPVMD                      ARBVMD
014900980728     I              BLPVAD                      ARBVAD
015000980728     I              BLPIAS                      ARBIAS
015100980728     I              BLPVAS                      ARBVAS
015200980728     I              BLPCCM                      ARBCCM
015300980728     I              BLPRMN                      ARBRMN
015400980728     I              BLPRMA                      ARBRMA
015500980728     I              BLPRMO                      ARBRMO
015600980728     I              BLPFLS                      ARBFLS
015700980728     I              BLPNCD                      ARBNCD
015800980728     I              BLPNCA                      ARBNCA
015900980728     I              BLPXCO                      ARBXCO
016000980728     I              BLPCTM                      ARBCTM
016100980728     I              BLPFFD                      ARBFFD
016200980728     I              BLPTCR                      ARBTCR
016300980728     I              BLPDCR                      ARBDCR
016400980728     I              BLPHCR                      ARBHCR
016500980728     I              BLPDTI                      ARBDTI
016600980728     I              BLPHTI                      ARBHTI
016700980728     I              BLPDCE                      ARBDCE
016800980728     I              BLPNPI                      ARBNPI
016900980728     I              BLPNCI                      ARBNCI
017000980728     I              BLPNRC                      ARBNRC
017100980728     I              BLPDBR                      ARBDBR
017200980728     I              BLPNFV                      ARBNFV
017300980728     I              BLPDPC                      ARBDPC
017400980728     I              BLPDUC                      ARBDUC
017500980728     I              BLPFLP                      ARBFLP
017600980728     I              BLPDET                      ARBDET
017700980728     I              BLPDUT                      ARBDUT
017800980728     I              BLPDAM                      ARBDAM
017900980728     I              BLPNDC                      ARBNDC
018000980728     I              BLPDDC                      ARBDDC
018100980728     I              BLPDCP                      ARBDCP
018200980728     I              BLPDCM                      ARBDCM
018300980728     I              BLPHMC                      ARBHMC
018400980728     I              BLPTC1                      ARBTC1
018500980728     I              BLPTC2                      ARBTC2
018600980728     I              BLPCCA                      ARBCCA
018700980728     I              BLPTMC                      ARBTMC
018800980728     I              BLPGGA                      ARBGGA
018900980728     I              BLPGMA                      ARBGMA
019000980728     I              BLPGVA                      ARBGVA
019100980728     I              BLPFLE                      ARBFLE
019200980728     I              BLPTFP                      ARBTFP
019300980728     I              BLPFEA                      ARBFEA
019400980728     I              BLPTFA                      ARBTFA
019500980728     I              BLPFT1                      ARBFT1
019600980728     I              BLPDT1                      ARBDT1
019700980728     I              BLPFT2                      ARBFT2
019800980728     I              BLPDT2                      ARBDT2
019900980728     I              BLPFT3                      ARBFT3
020000980728     I              BLPDT3                      ARBDT3
020100980506      *--------------------------------------------
020200990806     ITITAS000
020300981126     I              TASAAS                      ARBAAS
020400981126     I              TASLNP                      ARBLNP
020500981126     I              TASNRS                      ARBNRS
020600981126     I              TASNSP                      ARBNSP
020700981126     I              TASMGS                      ARBMGS
020800981126     I              TASNCL                      ARBNCL
020900981126     I              TASKSC                      ARBKSC
021000981126     I              TASCCM                      ARBCCM
021100990806     I              TASPKF                      ARBPKF
021200981126     I              TASFVF                      ARBFVF
021300981126     I              TASVLF                      ARBVLF
021400981126     I              TASLNA                      ARBLNA
021500981126     I              TASIAS                      ARBIAS
021600981126     I              TASVAS                      ARBVAS
021700981126     I              TASTSP                      ARBTSP
021800981126     I              TASCTR                      ARBCTR
021900981126     I              TASLL1                      WDDT
022000990309     i              TASCCA                      ARBCCA
022100981126
022200990806     ITITAS010
022300981126     I              TASAAS                      ARBAAS
022400981126     I              TASLNP                      ARBLNP
022500981126     I              TASNRS                      ARBNRS
022600981126     I              TASNSP                      ARBNSP
022700981126     I              TASMGS                      ARBMGS
022800981126     I              TASNCL                      ARBNCL
022900981126     I              TASKSC                      ARBKSC
023000981126     I              TASCCM                      ARBCCM
023100990806     I              TASPKF                      ARBPKF
023200981126     I              TASFVF                      ARBFVF
023300981126     I              TASVLF                      ARBVLF
023400981126     I              TASLNA                      ARBLNA
023500981126     I              TASIAS                      ARBIAS
023600981126     I              TASVAS                      ARBVAS
023700981126     I              TASTSP                      ARBTSP
023800981126     I              TASCTR                      ARBCTR
023900981126     I              TASLL1                      WDDT
024000990309     i              TASCCA                      ARBCCA
024100981126
024200990806     ITITASP00
024300981126     I              TASAAS                      ARBAAS
024400981126     I              TASLNP                      ARBLNP
024500981126     I              TASNRS                      ARBNRS
024600981126     I              TASNSP                      ARBNSP
024700981126     I              TASMGS                      ARBMGS
024800981126     I              TASNCL                      ARBNCL
024900981126     I              TASKSC                      ARBKSC
025000981126     I              TASCCM                      ARBCCM
025100990806     I              TASPKF                      ARBPKF
025200981126     I              TASFVF                      ARBFVF
025300981126     I              TASVLF                      ARBVLF
025400981126     I              TASLNA                      ARBLNA
025500981126     I              TASIAS                      ARBIAS
025600981126     I              TASVAS                      ARBVAS
025700981126     I              TASTSP                      ARBTSP
025800981126     I              TASCTR                      ARBCTR
025900981126     I              TASLL1                      WDDT
026000990309     i              TASCCA                      ARBCCA
026100981126
026200981126     ITNTMD000
026300981126     I              TMDAAS                      ARBAAS
026400981126     I              TMDLNP                      ARBLNP
026500981126     I              TMDNRS                      ARBNRS
026600981126     I              TMDNSP                      ARBNSP
026700981126     I              TMDRSM                      ARBRSM
026800981126     I              TMDINM                      ARBINM
026900981126     I              TMDCAM                      ARBCAM
027000981126     I              TMDLOM                      ARBLOM
027100981126     I              TMDPRM                      ARBPRM
027200981126     I              TMDNZM                      ARBNZM
027300981126     I              TMDRSD                      ARBRSD
027400981126     I              TMDIND                      ARBIND
027500981126     I              TMDCAD                      ARBCAD
027600981126     I              TMDLOD                      ARBLOD
027700981126     I              TMDPRD                      ARBPRD
027800981126     I              TMDNZD                      ARBNZD
027900981126      * ---------------------------------------------------------
028000980506      *
028100980506      * EMISSIONE PRIMA VIDEATA
028200980605    1C                   dow                     *inkc = *off  and *inkl = *off
028300980528     C                   EXFMT     FI04D01
028400980506     C                   CLEAR                   V1CMSG
028500980602     C                   setoff                                       404128
028600980602     C                   setoff                                       90
028700980506      * F3=FINE
028800980605    2c                   IF        *INKC =*off and
028900980605     c                             *inkl = *off
029000981126      * f14=Interrogazione C.A.
029100981126     C     *INKN         IFEQ      *ON
029200981126     c                   exsr      SUB_F14
029300981126     C                   iter
029400981126     c                   ENDIF
029500980506      * CONTROLLI VIDEATA
029600980506     C                   EXSR      CTRD01
029700980610      * F18=NOTE
029800980610    3c                   IF        *INKS =*on  and
029900980610     C                             *in28 = *off
030000980610     c* valorizzo la DS di gestione note/descrizioni e chiamo il PGM FIDN10R
030100980610     C                   CLEAR                   DSDN10
030200980716     c                   move      v1cdca        i10dta
030300980610     c                   movel     'N'           i10trc
030400980610     c                   movel     'C'           i10tpd
030500980716     c                   movel     i00fca        i10nks
030600980716     c                   movel     dsnumca       i10nkt
030700980610     c                   movel     'M'           i10flm
030800980610     c                   movel(P)  dsdn10        kpjbu
030900980610     C                   CALL      'FIDN10R'                            99
031000980610     C                   PARM                    kpjba
031100980610     C                   END
031200980610     c*
031300980528      * se non ci sono errori registro la fase
031400980528      * e ha dato il CMD 06
031500980528    4C                   if        *in28 = *off and
031600990114     c                             (*inkf = *on or *inkq = *on)
031700980506      * Aggiorno campi file da video e scrivo
031800980528     c                   exsr      RIEDCF
031900990114      * vado A FINE PGM solo se è stato dato KF oppure KQ ed è stato dato F3
032000990118     c                   if        *inkf or (*inkq and o00f03 = 'S') or
032100990118     c                             (*inkq and o00f03 = ' ' and o00f12 = ' ' )
032200981008     c                   leave
032300990114     c                   endif
032400980528      *
032500980528    4c                   endif                                                  *SI CMD 6 Conferma
032600980528    2c                   endif                                                  *NO CMD 3 Fine
032700980506      *
032800980528    1c                   enddo                                                  *fine ciclo KC *on
032900980528      *
033000990126     c                   clear                   o00f12
033100980605     c                   if        *inkc = *on
033200980605     c                   eval      o00f03 = 'S'
033300980605     c                   endif
033400980605      *
033500980605     c                   if        *inkl = *on
033600980605     c                   eval      o00f12 = 'S'
033700980605     c                   endif
033800980605      *
033900980716     c                   clear                   dsdn10
034000980610     c                   movel     'C'           i10tla
034100980610     c                   movel     dsdn10        kpjbu
034200980610     c                   call      'FIDN10R'
034300980610     c                   parm                    kpjba
034400980826     C                   movel     fidn00ds      KPJBU
034500980506     C                   EVAL      *INLR = *ON
034600981126      ****************************************************************
034700981126      * F14 = INTERROGAZIONE C.A.
034800981126      ****************************************************************
034900981126     C     SUB_F14       BEGSR
035000981126      *
035100981126      *
035200981126     c                   movel     'I'           I00mod
035300981126      *
035400981126     c                   movel     FIDN00DS      kpjbu
035500981126      *
035600981126     c                   call      'FIDN01R'
035700981126     c                   parm                    kpjba
035800981126      *
035900981126     c                   endsr
036000980506      *****************************************************************
036100980506      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
036200980506      *****************************************************************
036300980506     C     CTRD01        BEGSR
036400980528      * Controllo data fase
036500980528     C                   IF        V1Cdfs <> 0
036600980528     C                   MOVE      V1Cdfs        G02DAT
036700980528     C                   eval      g02err = *blanks
036800980528     C                   CALL      'XSRDA8'
036900980528     C                   PARM                    WLBDAT
037000980528     C                   if        G02ERR = '1'
037100980528     C                   SETON                                        289040
037200980528     C                   MOVEL     MSG(1)        V1CMSG
037300980528     C                   else
037400980605     C                   z-add     G02DAT        V1Cdfs
037500980528     C                   MOVE      G02INV        wdfs
037600980528     C                   endif
037700980528     C                   else
037800980528     C                   SETON                                        289040
037900980528     C                   MOVEL     MSG(1)        V1CMSG
038000980528     C                   ENDIF
038100980528      * controllo che la data fase sia maggiore della data apertura C.A.
038200980528     c                   if        *in28 = *off and
038300980528     c                             wdatac > wdfs
038400980528     C                   SETON                                        289040
038500980528     C                   MOVEL     MSG(5)        V1CMSG
038600980528     C                   Endif
038700980805      * controllo che la data fase sia minore o uguale ad oggi
038800980805     c                   if        *in28 = *off and
038900980805     c                             dateu < wdfs
039000980805     C                   SETON                                        289040
039100980805     C                   MOVEL     MSG(6)        V1CMSG
039200980805     C                   Endif
039300980528      *controllo il campo dell'ora
039400980528     c                   if        *in28 = *off
039500980602     c                   movel     v1chfc        ora
039600980602     c                   move      v1chfc        min
039700980602     c                   if        ora > 24 or min > 60
039800980602     c                   eval      *in41 = *on
039900980602     c                   endif
040000980528     c                   if        *in41 = *on
040100980601     c                   eval      *in28 = *on
040200980601     c                   movel     MSG(2)        v1cmsg
040300980528     c                   endif
040400980528      *
040500980528     c                   endif
040600980528     c
040700980507      *
040800980506     C                   ENDSR
040900980716      *****************************************************************
041000980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
041100980507      *****************************************************************
041200980528     C     RIEDCF        BEGSR
041300980528      *  caricamento file storico delle fasi
041400980528     C                   movel     i00aac        Dcfaac
041500980528     C                   movel     i00fil        Dcffil
041600980602     C                   z-add     i00nca        Dcfnca
041700980528      *
041800980528     C                   movel     i00fca        Dcffca
041900980528     C                   movel     dctptr        Dcfptr
042000980528     C                   z-add     wdfs          Dcfdfc
042100980528     C                   z-add     v1chfc        Dcfhfc
042200980528     C                   movel     i00fgs        Dcffev
042300980528     C                   movel     knmus         Dcfpru
042400980528      *
042500980528     c                   clear                   dcflet
042600980907     c                   clear                   dcfftr
042700980907     c                   clear                   dcfdtr
042800980507      *
042900980528     c                   WRITE     fndcf000
043000980528      * ricerca fase successiva con la quale aggiorno la testata C.A.
043100990114      * solo se è stato dato F6 di conferma
043200980528      *  caricamento ds per richiamo pgm di comodo
043300990114     c                   if        *inkf = *on
043400990114      *
043500980721     c                   clear                   fidn05ds
043600981030     C                   if        DCTnev > *zeros
043700980605     c                   movel     'E'           I05fpe
043800980605     c                   else
043900980605     c                   movel     'N'           i05fpe
044000980605     c                   endif
044100981030     C                   if        DCTnde > *zeros
044200981030     c                   movel     'D'           I05fde
044300981102     c                   else
044400981102     c                   movel     'N'           I05fde
044500981102     c                   endif
044600980528     C                   movel     'F'           I05MOD
044700980528     C                   movel     i00fca        i05fca
044800980528     C                   movel     i00fpr        i05fpr
044900980528     C                   movel     dctptr        i05tpc
045000980528     C                   movel     'O'           I05ffs
045100980721     c                   z-add     DCFdfc        i05dta
045200980721      * valorizzo il tipo anomalia CA
045300980721     c                   move      DCTtad        i05tad
045400981125      * valorizzo numero c.a.
045500981125     c                   z-add     dctaac        i05aac
045600981125     c                   z-add     dctfil        i05fil
045700981125     c                   z-add     dctnca        i05nca
045800980528      * richiamo il pgm di ricerca
045900980602     C                   call      'FIDN05R'
046000980602     C                   parm                    kpjba
046100980610     C                   parm                    fidn05ds
046200980528     C                   if        o05err = ' '
046300980528     C                   movel     o05rec        fndfa
046400980528      * valorizzo i campi della testata C.A.
046500980528      * fase
046600980528     C                   movel     i00fca        dctfca
046700980528     c* sede
046800980528     c                   if        dfagfs = 'S'
046900980528     c                   z-add     46            dctgfc
047000980528     c                   endif
047100980528     c* P O partenza
047200980528     c                   if        dfagfs = 'P'
047300990111     c                   movel     §dctlnpc      dctgfc
047400980528     c                   endif
047500980528     c* P O arrivo
047600980528     c                   if        dfagfs = 'A'
047700981230     c                   z-add     i00lna        dctgfc
047800980528     c                   endif
047900981023     c* P O apertura C.A.
048000981023     c                   if        dfagfs = 'F'
048100981023     c                   z-add     i00fil        dctgfc
048200981023     c                   endif
048300040330      * flag e data trasmissione
048400040330     c                   clear                   DCTft1
048500040330     c                   clear                   DCTft2
048600980528     c                   endif
048700040330      *
048800990114     c                   update    fndct000
048900990114     c*
049000990114     c                   else
049100990114     c*
049200990114     c                   unlock    fndct01l
049300990114     c*
049400990114     c                   endif
049500990114     c*
049600981126      * Conferma se esiste l'interlocutore
049700980716     c     v1cnco        ifne      *blanks
049800980716     c                   clear                   dsdn10
049900980716     c                   movel     'W'           i10flm
050000980716     c                   movel     'C'           i10tpd
050100980716     c                   movel     'N'           i10std
050200980716     c                   movel     'D'           i10trc
050300980716     c                   movel     dsnumca       i10nkt
050400980716     c                   movel     v1cnco        i10not
050500980716     c                   movel(P)  dsdn10        kpjbu
050600980716     C                   CALL      'FIDN10R'                            99
050700980716     C                   PARM                    kpjba
050800980716     c                   endif
050900980610     c* conferma note
051000980610     C                   CLEAR                   DSDN10
051100980610     C                   MOVEL     'C'           I10FLM
051200980716     c                   movel     'C'           i10tpd
051300980716     c                   movel     'N'           i10trc
051400980610     c                   movel(P)  dsnumca       i10nkt
051500980716     c                   movel     i00fca        i10nks
051600980610     c                   movel(P)  dsdn10        kpjbu
051700980610     C                   CALL      'FIDN10R'                            99
051800980610     C                   PARM                    kpjba
051900990114     c*
052000990114     c* se è stato dato cmd16 vado a gestire l'esito transazione
052100990114     c                   if        *inkq = *on and *in81 = *on
052200990223      * se richiamo l'altro pgm esito transazione chiudo il pgm delle note
052300990223     c                   clear                   dsdn10
052400990223     c                   movel     'C'           i10tla
052500990223     c                   movel     dsdn10        kpjbu
052600990223     c                   call      'FIDN10R'
052700990223     c                   parm                    kpjba
052800990223     C                   movel     fidn00ds      KPJBU
052900990223     c***
053000990114     c                   movel     wfaes         i00fca
053100990114     c                   movel     fidn00ds      kpjbu
053200990114     c                   call      'FIDN08R'
053300990114     c                   parm                    kpjba
053400990114     c* controllo se è stato dato f12 o f03
053500990114     c                   movel     kpjbu         fidn00ds
053600990114     c*
053700990114     c                   if        o00f03 = 'S' or o00f12 = 'S'
053800990114     c*
053900990114     c* annullo e cancello la fase del contatto
054000990114     c     keydcf        chain     fndcf01l                           31
054100990114     c  n31              delete    fndcf000
054200990114     c* riaggancio la testata CA
054300990114     c*
054400990114     C     kdct          CHAIN     fndct000                           31
054500990114     c*
054600990114     c                   movel     wfaco         i00fca
054700990114     c*
054800990114     c                   endif
054900990114     c*
055000990114     c                   endif
055100980528     c
055200980507     C                   ENDSR
055300981126      *****************************************************************
055400981126      *  CONTROLLO TIPO BOLLA
055500981126      *****************************************************************
055600981126     C     CTR_tipobo    BEGSR
055700981126      *
055800981126      *
055900981126     C                   movel     'TB'          COD
056000981126     C                   movel(P)  TAStbl        KEY
056100981126     C     ktab          CHAIN     TABEL                              31
056200981126     C                   movel     TBLUNI        DSTB
056300981126     c                   if        §TBrbl <> 'R'
056400981126     C                   movel     §TBtpo        FLGTB1
056500981126     c                   endif
056600981126      *
056700981126     C                   ENDSR
056800990223      *****************************************************************
056900990223      *   ROUTINE LANCIO CONTROLLO COLLI DA CONSEGNARE
057000990223      *****************************************************************
057100990223     C     COLLI         BEGSR
057200990223      *
057300990223     C                   CLEAR                   FIDN53DS
057400990223     C*
057500990223     c                   eval      i53aas = i00aas
057600990223     c                   eval      i53lnp = i00lnp
057700990223     c                   eval      i53nrs = i00nrs
057800990223     c                   eval      i53nsp = i00nsp
057900990223     c* Richiamo il programma che controlla i colli ancora da consegnare
058000990223     c                   movel     fidn53ds      kpjbu
058100990223     c                   call      'FIDN53R'
058200990223     c                   parm                    kpjba
058300990223     c*
058400990223     c                   movel     kpjbu         fidn53ds
058500990223     c* se c'è errore è perchè ci sono dei colli da consegnare
058600990223     c                   if        o53err = 'E'
058700990223     c                   seton                                        28
058800990223     c                   movel     msg(7)        v1cmsg
058900990223     c                   endif
059000990223     c*
059100990223     c                   movel     fidn00ds      kpjbu
059200990223     c*
059300990223     c                   endsr
059400990311     C*-------------------------------------------------------------------------*
059500990311     c* REC_DKA   Valorizzo i dati del beneficiario dal file FNDKA
059600990311     C*-------------------------------------------------------------------------*
059700990311     c     REC_DKA       BEGSR
059800990311     C*
059900990311     c                   movel     dkarag        v1crsm
060000990311     c                   movel     dkacap        v1ccam
060100990311     c                   movel     dkaloc        v1clom
060200990311     c                   movel     dkaprv        v1cprm
060300990311     c                   movel     dkatel        v1ctel
060400990311     c                   movel     dkafax        v1cfax
060500990311     c                   movel     dkanaz        key
060600990311     c*
060700990311     c                   endsr
060800980506      *****************************************************************
060900980506      *   ROUTINE INIZIALE
061000980506      *****************************************************************
061100980506     C     *INZSR        BEGSR
061200980506      *
061300980506     C     *ENTRY        PLIST
061400980506     C                   PARM                    KPJBA
061500980610     C                   MOVEL     KPJBU         fidn00ds
061600980506      *
061700980506     C                   Z-ADD     1             CODUT
061800980506     C                   CALL      'X§PARUT'
061900980507     C                   PARM                    UT§DSE0F
062000980506     C                   MOVEL     RAGUT         RSUT
062100980508     C                   MOVEL     REC80         CNCR80
062200980506      *
062300981126      *
062400981126     C                   IF        SIMFEL = *zeros
062500990806     C                   open      TITAS30C
062600981126     C                   open      TNTMD01L
062700981126     c                   Endif
062800980728      *
062900980728     c     kbol01        klist
063000980728     c                   kfld                    i00aas
063100980728     c                   kfld                    i00lnp
063200980728     c                   kfld                    i00nrs
063300980728     c                   kfld                    i00nsp
063400991116      *
063500991116     c     kar6          klist
063600991116     c                   kfld                    i00aas
063700991116     c                   kfld                    i00lnp
063800991116     c                   kfld                    i00nrs
063900991116     c                   kfld                    i00nsp
064000991116     c                   kfld                    trcar6
064100980605     C* ACCESSO   TABEL
064200980605     C     KTAB          KLIST
064300980605     C                   KFLD                    CODUT
064400980605     C                   KFLD                    COD               2
064500980605     C                   KFLD                    KEY               8
064600980605     c                   movel     '15'          cod
064700980528      *
064800980528     C     Kdct          klist
064900980528     C                   KFLD                    kaac
065000980528     C                   KFLD                    kfil
065100980528     C                   KFLD                    knca
065200981230      *
065300981230     C     Kdka          klist
065400981230     C                   KFLD                    kaac
065500981230     C                   KFLD                    kfil
065600981230     C                   KFLD                    knca
065700981230     C                   KFLD                    wtrc
065800980721      *
065900980721     c     kdcf          klist
066000980721     c                   kfld                    dctaac
066100980721     c                   kfld                    dctfil
066200980721     c                   kfld                    dctnca
066300990114      *
066400990114     c     keydcf        klist
066500990114     c                   kfld                    dcfaac
066600990114     c                   kfld                    dcffil
066700990114     c                   kfld                    dcfnca
066800990114     c                   kfld                    dcfdfc
066900990114     c                   kfld                    dcfhfc
067000981201      *
067100991115     c*!*  kbl4          klist
067200991115     c*!*                kfld                    i00aas
067300991115     c*!*                kfld                    i00lnp
067400991115     c*!*                kfld                    i00nrs
067500991115     c*!*                kfld                    i00nsp
067600991115     c*!*                kfld                    KTRC
067700980721      *
067800980728     c     kspe          klist
067900980728     c                   kfld                    tipkey
068000980728     c                   kfld                    v1cksm
068100980728     c                   kfld                    codluo
068200981201      *
068300980728     c                   eval      codluo = '006'
068400980728     c                   eval      tipkey = 'L'
068500980528      *
068600990311     C                   TIME                    W0140
068700980507     C                   MOVE      W0140         WDTGIO
068800980507     C                   MOVEL     W0140         WORA
068900980528     c                   eval                    v1cdfs=wdtgio
069000980805      * UDATE IN AAAAMMGG
069100980805     C                   Z-ADD     WDTGIO        G02DAT
069200980805     C                   MOVEL     *BLANK        G02ERR
069300980805     C                   CALL      'XSRDA8'
069400980805     C                   PARM                    WLBDAT
069500980805     C                   MOVEL     G02INV        DATEU
069600980805      *
069700980602     c                   movel     WORA          v1chfc
069800980528      * decodifico il Punto Operatico C.A.
069900980601     c                   move      i00fil        v1cpog
070000980601     c     i00fil        chain     azorg                              31
070100990111     c* n31              movel     orgdes        v1ddpo
070200980528      * valorizzo il capo anno e numero C.A.
070300980601     c                   z-add     i00nca        v1cnca
070400980528      * decodifico la fase selezionata
070500980601     c                   move      i00fca        v1cfca
070600980601     c     i00fca        chain     fndfa01l                           31
070700980528     c  n31              movel     dfades        v1ddfa
070800981217      *
070900981217     c                   movel     *blanks       vcttit
071000981217     c                   if        i00fca = wfaco
071100990114     c                   seton                                        81
071200981217     c                   movel     tit_1         vcttit
071300981217     c                   else
071400981217     c                   movel     tit_2         vcttit
071500981217     c                   endif
071600990126      * controllo se reso per avaria
071700990126      *
071800990126     c                   eval      *in50 = ( i00fca = wfaav )
071900990126      *
072000980528      * aggancio la C.A. per recuperare i dati anagrafici della persona
072100980528     c                   z-add     i00aac        Kaac
072200980528     c                   z-add     i00fil        Kfil
072300980528     c                   z-add     i00nca        Knca
072400980528     C     kdct          CHAIN     fndct000                           31
072500981230     c* flag operativi
072600981230     c                   movel     dctflo        ddct01
072700980528      * controllo se il partner transazione è il MITTENTE o il DESTINATARIO
072800980728      * recupero il mittente ed il destinatario
072900980728    1c                   if         i00tpb = 'P'
073000980728     c     kbol01        chain     fnblp000                           30
073100981126     c                   endif
073200981126    1c                   if         i00tpb = 'A'
073300980728     c     kbol01        chain     fnarb000                           30
073400980728    1c                   endif
073500981126    1c                   if         i00tpb = 'S'
073600990806     c     kbol01        chain     TITAS30c                           30
073700981126     c     kbol01        chain     tntmd000                           30
073800981126     c                   exsr      CTR_tipobo                                   Ctr tipo bolla
073900981126     c                   if        FLGTB1 = *blanks
074000990806     c     kbol01        reade     TITAS30c                               30
074100981126     c                   endif
074200981126    1c                   endif
074300981201      *
074400981230      * Se assegnato in partenza verifico il codice destinatario
074500981230     c* in caso di cambio di porto il tipo porto è il contrario di quello della CA
074600990305     c                   SELECT
074700990305     c*
074800110503    1c**                 when      arbcca <> '9'
074900110503    1c                   when      §DCTCCA <> '9'
075000981230     c                   eval      W_porto = §dctport
075100990305     c*
075200990305    2c                   when      §dctport = 'F'
075300981230     c                   eval      W_porto = 'A'
075400990305     c*
075500990305    2c                   when      §dctport = 'A'
075600981230     c                   eval      W_porto = 'F'
075700990305     c*
075800990305    2c                   endsl
075900990305    1c*
076000981230     c*
076100991115     c*!*                clear                   DSBL4A
076200991115     c                   clear                   AR6KSC
076300990111    1c                   IF        I00tpb = 'P'  and  W_porto  = 'A'
076400991115     c*!*                movel     'A'           KTRC
076500991115     c*!*  Kbl4          CHAIN     FNBL4000                           30
076600991115     c*!*N30              movel     BL4NOT        DSBL4A
076700991116     c                   eval      trcar6 = '1'
076800991116     c     Kar6          CHAIN     FIAR6000                           30
076900990111    1c                   ENDIF
077000981201
077100980728      * dati bolla
077200990111    1c                   IF        W_porto  = 'F'
077300980728     c                   move      arbksc        VTCksm
077400980728     c                   move      *zeros        VTCksd
077500980728     c                   ELSE
077600980728     c                   move      arbccm        VTCksm
077700991115    2c*!*                if        §B4ksf <> *blanks
077800991115     c*!*                movel     §B4ksf        VTCksd
077900991115    2c                   if        AR6ksc <> *zeros
078000991115     c                   movel     AR6ksc        VTCksd
078100981201     c                   else
078200981201     c                   movel     arbksc        VTCksd
078300990111    2c                   endif
078400990111    1c                   ENDIF
078500980728      *
078600990311      *
078700990311     c* cerco i dati anagrafici in fndka
078800990311     c* controllo se c'è RIVALSA
078900990311     c                   movel     'R'           wtrc                           RIVALSA
079000990311     c     kdka          chain     fndka01l                           30
079100990909     c                   if        *in30 = *off  and DKAatb = *blanks
079200990311     c* recupero i dati
079300990311     c                   exsr      REC_DKA
079400990311     c*
079500990311     c                   ELSE
079600990311     c*
079700990311     c                   SELECT
079800990311      *
079900990311      * se e' valorizzato il BENEFICIARIO ALTERNATIVO
080000990311    2c                   WHEN      dctksc <> *zeros
080100990311      *
080200990311     c                   movel     dctksc        v1cksm
080300990311     c                   move      v1cksm        w0040             4 0
080400990311      *
080500990311      * codice BENEFICIARIO NON CODIFICATO
080600990311      *
080700990311    2c                   if        w0040 = 8888
080800990311      *
080900990311     c* cerco i dati anagrafici in fndka
081000990311     c                   movel     'C'           Wtrc                           CLIENTE
081100990311     c     kdka          chain     fndka01l                           30
081200990909     c                   if        *in30 = *off  and DKAatb = *blanks
081300990311     c                   exsr      REC_DKA
081400990311    3c                   endif
081500990311      *
081600990311   x2c                   else
081700990311      *
081800990311      * codice BENEFICIARIO CODIFICATO
081900040519
082000040519      * carico i dati del beneficiario dall'anagrafico luoghi
082100040519     c     kSpe          Chain     Fnspe01l
082200040520if  3c                   If        %Found(Fnspe01l)
082300040519     c                   movel     sperag        v1crsm
082400040519     c                   movel     specap        v1ccam
082500040519     c                   movel     speloc        v1clom
082600040519     c                   movel     spepro        v1cprm
082700040519     c                   movel     spetel        v1ctel
082800040519     c                   movel     spefax        v1cfax
082900040519     c                   movel     spenaz        key
083000040519   x3c                   Else
083100040519      * carico i dati del beneficiario dall'anagrafico clienti
083200990311      *
083300990311     c                   clear                   tibs69ds
083400990311     c                   clear                   ds_aco
083500990311     c                   clear                   ds_ind
083600990311     c                   clear                   ds_clp
083700990311     c                   clear                   ds_cls
083800990311      *
083900990311     c                   movel     'L'           i69tla
084000990311     c                   z-add     dctksc        i69kac
084100990311     c                   z-add     dctksc        i69kin
084200990311     c                   z-add     dctksc        i69kcs
084300990311      *
084400990311     c                   CALL      'TIBS69R'
084500990311     c                   parm                    tibs69ds
084600990311     c                   parm                    ds_aco
084700990311     c                   parm                    ds_ind
084800990311     c                   parm                    ds_clp
084900990311     c                   parm                    ds_cls
085000990311      *
085100990311     c                   movel     acorag        v1crsm
085200990311     c                   movel     indcit        v1clom
085300060119     c                   movel     indcae        v1ccam
085400990311     c                   movel     indprv        v1cprm
085500990311     c                   movel     indtel        v1ctel
085600990311     c                   movel     indtlf        v1cfax
085700990311     c                   movel     indsta        key
085800040519
085900040519e   3c                   EndIf
086000990311      *
086100990311     c                   endif
086200990311      *
086300990311     c                   OTHER
086400990311      *
086500990311      * se non e' valorizzato il beneficiario alternativo prendo o il MITTENTE o il DESTINATARIO
086600990311      *                                                                  S P E D I Z I O N E
086700990311      *                                                               --------------------------
086800990311    2c                   if        dctptr = 'M' or (dctptr = ' ' and *in50)
086900990311     c                   movel     vtcksm        v1cksm
087000990311    2c                   endif
087100990311    2c                   if        dctptr = 'D'
087200990311     c                   movel     vtcksd        v1cksm
087300990311    2c                   endif
087400990311      *
087500060118      * recupero dai dati della bolla se 8888 o 9999
087600990311      *
087700060118     c                   move      v1cksm        Com04
087800060118      *
087900060118     c                   If        Com04 = 8888  or Com04 = 9999
088000060118
088100990311    3c                   if        dctptr = 'M'  or (dctptr = ' ' and *in50)
088200060118     c
088300990311     c                   movel     arbrsm        V1Crsm
088400990311     c                   move      arbcam        V1Ccam
088500990311     c                   movel     arblom        V1Clom
088600990311     c                   movel     arbprm        V1Cprm
088700990311     c                   movel     arbnzm        key
088800990311   x3c                   else
088900990311     c                   movel     arbrsd        V1Crsm
089000990311     c                   move      arbcad        V1Ccam
089100990311     c                   movel     arblod        V1Clom
089200990311     c                   movel     arbprd        V1Cprm
089300990311     c                   movel     arbnzd        key
089400990311    3c                   endif
089500060118
089600060118     c                   else
089700060118      * carico i dati del beneficiario dall'anagrafico clienti
089800060118      *
089900060118     c                   clear                   tibs69ds
090000060118     c                   clear                   ds_aco
090100060118     c                   clear                   ds_ind
090200060118     c                   clear                   ds_clp
090300060118     c                   clear                   ds_cls
090400060118      *
090500060118     c                   movel     'L'           i69tla
090600060119     c                   z-add     v1cksm        i69kac
090700060119     c                   z-add     v1cksm        i69kin
090800060119     c                   z-add     v1cksm        i69kcs
090900060118      *
091000060118     c                   CALL      'TIBS69R'
091100060118     c                   parm                    tibs69ds
091200060118     c                   parm                    ds_aco
091300060118     c                   parm                    ds_ind
091400060118     c                   parm                    ds_clp
091500060118     c                   parm                    ds_cls
091600060118      *
091700060118     c                   movel     acorag        v1crsm
091800060118     c                   movel     indcit        v1clom
091900060118     c                   movel     indprv        v1cprm
092000060119     c                   movel     indcae        v1ccam
092100060118     c                   movel     indtel        v1ctel
092200060118     c                   movel     indtlf        v1cfax
092300060118     c                   movel     indsta        key
092400060118
092500990311      * cerco nell'anagrafico luoghi il cliente relativo al codice luogo 6 specifico
092600990311      * per i contatti in caso di danni ma non per il sollecito avaria
092700060119     c******             if        not *in50
092800990311     c     kspe          chain     fnspe01l                           30
092900990311    3c     *in30         ifeq      *off
093000990311     c                   movel     sperag        v1crsm
093100990311     c                   movel     specap        v1ccam
093200990311     c                   movel     speloc        v1clom
093300990311     c                   movel     spepro        v1cprm
093400990311     c                   movel     spetel        v1ctel
093500990311     c                   movel     spefax        v1cfax
093600990311     c                   movel     spenaz        key
093700990311      *
093800990311     c                   endif
093900060118      *
094000060118     c                   endif
094100990311      *
094200060119     c******             endif                                                  * luogo 6
094300990311      *
094400990311     c                   ENDSL
094500990311      *
094600990311     c                   endif                                                  * no rivalsa
094700980728     c
094800980605     c* decodifico la nazione se è diversa da blank
094900980728    2c                   if        key <> *blanks
095000980605     c     ktab          chain     tabel                              30
095100980728    3c                   if        *in30 = *off and
095200980605     c                             tblflg = ' '
095300980605     c                   movel     tbluni        v1cnaz
095400980728    3c                   endif
095500980728    2c                   endif
095600990311    1c*
095700980604      * calcolo data apertura C.A.
095800980604     C                   eval      wdatac = (DCTaac * 10000) + DCTmgc
095900980604     c*
096000980604     C                   CLEAR                   WLBDAT
096100980604     C                   Z-ADD     wdatac        G02inv
096200980604     C                   movel     '3'           G02err
096300980604     C                   CALL      'XSRDA8'
096400980604     C                   PARM                    WLBDAT
096500980604     c                   z-add     g02dat        v1cdca
096600980604      * numero spedizione
096700040722     c**""**             movel     dctlnp        v1clnp
096800040722     c**""**             movel     dctnrs        v1cnrs
096900040722     c**""**             movel     dctnsp        v1cnsp
097000040722     c                   movel     i00lnp        v1clnp
097100040722     c                   movel     i00nrs        v1cnrs
097200040722     c                   movel     i00nsp        v1cnsp
097300981126      * recupero se esiste l'interlocutore
097400980716     c                   clear                   dsdn10
097500980716     c                   movel     'C'           i10tpd
097600980716     c                   movel     'R'           i10flm
097700980716     c                   movel     'N'           i10std
097800980716     c                   movel     'D'           i10trc
097900980716     c                   movel     dsnumca       i10nkt
098000980716     c                   movel(P)  dsdn10        kpjbu
098100980716     C                   CALL      'FIDN10R'                            99
098200980716     C                   PARM                    kpjba
098300980716     c                   movel     kpjbu         dsdn10
098400980716     c     o10err        ifeq      *blanks
098500980716     c                   movel     o10not        v1cnco
098600980716     c                   else
098700980716     c                   movel     *blanks       v1cnco
098800980716     c                   end
098900981105      * se esiste ancora delle merce da consegnare relativa alla spedizione emetto un messaggio
099000981105      * di avvertimento solo se la CA è in fase di liquidazione transattiva
099100981105    1c                   if        dctfpr =  'T'
099200990121      * routine controllo colli se la data di consegna merce in testata = 0
099300990121     c                   if        arbdcm > 0
099400990121     c*
099500990121     c                   else
099600990223     c*
099700990223     c                   exsr      COLLI
099800990223     c*
099900981105    1c                   endif
100000990121    1c                   endif
100100990111      * calcolo data spedizione
100200990111     C                   eval      wdaspe = (arbaas * 10000) + arbmgs
100300990111     c*
100400990111     C                   CLEAR                   WLBDAT
100500990111     C                   Z-ADD     wdaspe        G02inv
100600990111     C                   movel     '3'           G02err
100700990111     C                   CALL      'XSRDA8'
100800990111     C                   PARM                    WLBDAT
100900990111     c                   z-add     g02dat        v1cdsp
101000990114      *
101100990114      * se ci sono delle note accendo indicatore di reverse immage al tasto funzionale F18
101200990114     c*
101300990114     c* cerco i dati nel file fndcs
101400990114     C                   CLEAR                   DSDN42
101500990114     c                   movel     'N'           i42trc
101600990114     c                   movel     'C'           i42tpd
101700990114     c                   movel     dsnumca       i42nkt
101800990114     c                   movel(P)  dsdn42        kpjbu
101900990114     C                   CALL      'FIDN42R'
102000990114     C                   PARM                    kpjba
102100990114     c*
102200990114     c                   movel     kpjbu         dsdn42
102300990114     c*
102400990114     c                   if        o42err = ' '
102500990114     c                   seton                                        80
102600990114     c                   endif
102700990114     c*
102800990114     c*
102900980506     C                   ENDSR
103000980506      *****************************************************************
103100980508** MSG  (Lungh. 78)                                                          *
103200980528Immettere Data Corretta                                                            1
103300980605Immettere Ora Corretta                                                             2
103400981130LIBERO                                                                             3
103500981130LIBERO                                                                             4
103600980528Immettere Data maggiore di Data Apertura C.A.                                      5
103700980805Immettere Data minore o uguale alla data odierna                                   6
103800981105ATTENZIONE !! Esiste ancora della merce da consegnare per questa spedizione        7

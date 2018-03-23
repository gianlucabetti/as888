000100120723     /*PRM  dbgview(*source)
000200120723     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000300120723     /*CMD         ovrscope(*calllvl)
000400120723     /*CMD  ovrdbf file(TNTMD01L) tofile(GAITRAGRPS/TNTMD01L) +
000500120723     /*CMD         ovrscope(*calllvl)
000600120723     /*END  dltovr file(TITAS30C TNTMD01L) lvl(*)
000700120723     /*END
000800980506      *------------------------------------------------------------------------*
000900980527      * GESTIONE SCRITTURA FASI                                                *
001000981217      *------------------------------------------------------------------------*
001100981217      * Fasi fissate a programma
001200040330      * 050 Sollecito telefonico reso avaria wfaav
001300981217      * 200 Contatto telefonico wfaco
001400990114      * 215 Esito Contatto telefonico wfaes
001500980506      *------------------------------------------------------------------------*
001600980506     H DECEDIT('0,') DATEDIT(*DMY.)
001700980506      *--------------------------------------------
001800980528     Ffidn04D   CF   E             WORKSTN
001900980601     Ffndfa01L  IF   E           k DISK
002000980604     Ffndka01L  IF   E           k DISK
002100980602     Ffndct01L  uF   E           k DISK
002200990114     Ffndcs01L  iF   E           k DISK
002300980601     Fazorg01L  IF   E           k DISK
002400980605     Ftabel00f  IF   E           k DISK
002500980728     Ffnspe01l  IF   E           k DISK
002600990114     Ffndcf01l  uf A E           k DISK
002700980728     FFNBLP01L  IF   E           K DISK
002800991115     FFIAR601L  IF   E           K DISK
002900981105     FFNARB01L  IF   E           K DISK
003000981126      * sede
003100990806     fTITAS30C  IF   E           K DISK     usropn
003200981126     fTNTMD01L  IF   E           K DISK     usropn
003300990114      ****************************************************************
003400990114      *  RIEPILOGO INDICATORI
003500990114      ***************************************************************
003600990114      * 21/27- COMODO
003700990114      * 28   - ERRORE GENERICO
003800990114      * 30/31- COMODO
003900990114      * 40/41- ERRORI A VIDEO
004000990114      * 80   - F18 Lampeggiante ci sono delle note
004100990114      * 90   - COMODO
004200990114      * 99   - COMODO
004300980506      *--------------------------------------------
004400981105     D MSG             S             78    DIM(7) CTDATA PERRCD(1)              MSG VIDEO
004500980528      *--------------------------------------------
004600980616     D                SDS
004700980616     D  VTCPGM                 1     10
004800980616      *
004900980528     D Fidn00ds      E DS
005000980610     d  dsnumca                6     19
005100980528     D Fidn05ds      E DS
005200990223     D Fidn53ds      E DS
005300980610     D DSDN10        E DS                  EXTNAME(FIDN10DS)
005400990114     D DSDN42        E DS                  EXTNAME(FIDN42DS)
005500980528     D fndfa         E DS                  EXTNAME(FNDFA00F)
005600981230     D Ddct01        E DS
005700981126     D DSTB          E DS
005800990311     D TIBS69DS      E DS
005900990311     d DS_aco        E DS                  extname(CNACO00F)
006000990311     d DS_ind        E DS                  extname(CNIND00F)
006100990311     d DS_clp        E DS                  extname(CNCLP00F)
006200990311     d DS_cls        E DS                  extname(FNCLS00F)
006300980507      *--------------------------------------------
006400980507     D KPJBA         E DS
006500980508     D CNCR80        E DS
006600980507     D UT§DSE0F      E DS
006700980507      *--------------------------------------------
006800980527     D W0140           S             14  0
006900980507     D WDTGIO          S              8  0
007000980805     D dateu           s              8  0
007100980507     D WORA            S              6  0
007200980602     D ora             S              2  0
007300980602     D min             S              2  0
007400060118     D com04           S              4  0
007500981126     D flgtb1          S              1
007600981230     D W_porto         S              1
007700980728     d tipkey          s                   like(spefls)
007800980728     d codluo          s                   like(specod)
007900980528     D WDATAC          S                   LIKE(V1Cdfs)
008000990111     D WDAspe          S                   LIKE(V1Cdfs)
008100980528     D Wdfs            S                   LIKE(dcfdfc)
008200980528     D ccc             S                   LIKE(acokcc)
008300980604     D Kaac            S                   LIKE(dctaac)
008400980528     D Kfil            S                   LIKE(V1Cpog)
008500980528     D Knca            S                   LIKE(V1Cnca)
008600980728     D vtcksm          S                   LIKE(v1cksm)
008700980728     D vtcksd          S                   LIKE(v1cksm)
008800991116     D trcar6          S                   LIKE(ar6trc)
008900981217     D Wfaco           S                   LIKE(dcffca) INZ(200)
009000990114     D Wfaes           S                   LIKE(dcffca) INZ(215)
009100990126     D Wfaav           S                   LIKE(dcffca) INZ(050)
009200981230     D wtrc            S                   LIKE(dkatrc)
009300980528      *
009400980528     D WLBDAT          DS                  INZ
009500980528     D  G02DAT                 1      8  0
009600980528     D  G02INV                 9     16  0
009700980528     D  G02ERR                17     17
009800980528     D  G02TGI                18     22  0
009900980728     D Ds3a          E DS
010000981217      *  titolo videata (lunghezza massima 34)
010100981217     D TIT_1           C                   CONST('***    CONTATTO  TELEFONICO  -
010200981217     D                                       ***')
010300981217     D TIT_2           C                   CONST('***    SOLLECITO TELEFONICO  -
010400981217     D                                       ***')
010500980528      *
010600980728     IFNBLP000
010700980728     I              BLPATB                      ARBATB
010800980728     I              BLPAAS                      ARBAAS
010900980728     I              BLPLNP                      ARBLNP
011000980728     I              BLPNRS                      ARBNRS
011100980728     I              BLPNSP                      ARBNSP
011200980728     I              BLPCBO                      ARBCBO
011300980728     I              BLPMGS                      ARBMGS
011400980728     I              BLPFNS                      ARBFNS
011500980728     I              BLPSCL                      ARBSCL
011600980728     I              BLPFBR                      ARBFBR
011700980728     I              BLPFDN                      ARBFDN
011800980728     I              BLPLNA                      ARBLNA
011900980728     I              BLPZNC                      ARBZNC
012000980728     I              BLPKSC                      ARBKSC
012100980728     I              BLPRSM                      ARBRSM
012200980728     I              BLPINM                      ARBINM
012300980728     I              BLPCAM                      ARBCAM
012400980728     I              BLPLOM                      ARBLOM
012500980728     I              BLPNZM                      ARBNZM
012600980728     I              BLPPRM                      ARBPRM
012700980728     I              BLPFAP                      ARBFAP
012800980728     I              BLPCPI                      ARBCPI
012900980728     I              BLPRSD                      ARBRSD
013000980728     I              BLPIND                      ARBIND
013100980728     I              BLPCAD                      ARBCAD
013200980728     I              BLPLOD                      ARBLOD
013300980728     I              BLPNZD                      ARBNZD
013400980728     I              BLPPRD                      ARBPRD
013500980728     I              BLPFIN                      ARBFIN
013600980728     I              BLPGC1                      ARBGC1
013700980728     I              BLPGC2                      ARBGC2
013800980728     I              BLPCTR                      ARBCTR
013900980728     I              BLPCTS                      ARBCTS
014000980728     I              BLPFTM                      ARBFTM
014100980728     I              BLPTSP                      ARBTSP
014200980728     I              BLPNAS                      ARBNAS
014300980728     I              BLPNCL                      ARBNCL
014400980728     I              BLPNCP                      ARBNCP
014500980728     I              BLPPKC                      ARBPKC
014600980728     I              BLPPKB                      ARBPKB
014700980728     I              BLPPKF                      ARBPKF
014800980728     I              BLPNCR                      ARBNCR
014900980728     I              BLPVLC                      ARBVLC
015000980728     I              BLPFVB                      ARBFVB
015100980728     I              BLPVLB                      ARBVLB
015200980728     I              BLPFVF                      ARBFVF
015300980728     I              BLPVLF                      ARBVLF
015400980728     I              BLPQFT                      ARBQFT
015500980728     I              BLPVMD                      ARBVMD
015600980728     I              BLPVAD                      ARBVAD
015700980728     I              BLPIAS                      ARBIAS
015800980728     I              BLPVAS                      ARBVAS
015900980728     I              BLPCCM                      ARBCCM
016000980728     I              BLPRMN                      ARBRMN
016100980728     I              BLPRMA                      ARBRMA
016200980728     I              BLPRMO                      ARBRMO
016300980728     I              BLPFLS                      ARBFLS
016400980728     I              BLPNCD                      ARBNCD
016500980728     I              BLPNCA                      ARBNCA
016600980728     I              BLPXCO                      ARBXCO
016700980728     I              BLPCTM                      ARBCTM
016800980728     I              BLPFFD                      ARBFFD
016900980728     I              BLPTCR                      ARBTCR
017000980728     I              BLPDCR                      ARBDCR
017100980728     I              BLPHCR                      ARBHCR
017200980728     I              BLPDTI                      ARBDTI
017300980728     I              BLPHTI                      ARBHTI
017400980728     I              BLPDCE                      ARBDCE
017500980728     I              BLPNPI                      ARBNPI
017600980728     I              BLPNCI                      ARBNCI
017700980728     I              BLPNRC                      ARBNRC
017800980728     I              BLPDBR                      ARBDBR
017900980728     I              BLPNFV                      ARBNFV
018000980728     I              BLPDPC                      ARBDPC
018100980728     I              BLPDUC                      ARBDUC
018200980728     I              BLPFLP                      ARBFLP
018300980728     I              BLPDET                      ARBDET
018400980728     I              BLPDUT                      ARBDUT
018500980728     I              BLPDAM                      ARBDAM
018600980728     I              BLPNDC                      ARBNDC
018700980728     I              BLPDDC                      ARBDDC
018800980728     I              BLPDCP                      ARBDCP
018900980728     I              BLPDCM                      ARBDCM
019000980728     I              BLPHMC                      ARBHMC
019100980728     I              BLPTC1                      ARBTC1
019200980728     I              BLPTC2                      ARBTC2
019300980728     I              BLPCCA                      ARBCCA
019400980728     I              BLPTMC                      ARBTMC
019500980728     I              BLPGGA                      ARBGGA
019600980728     I              BLPGMA                      ARBGMA
019700980728     I              BLPGVA                      ARBGVA
019800980728     I              BLPFLE                      ARBFLE
019900980728     I              BLPTFP                      ARBTFP
020000980728     I              BLPFEA                      ARBFEA
020100980728     I              BLPTFA                      ARBTFA
020200980728     I              BLPFT1                      ARBFT1
020300980728     I              BLPDT1                      ARBDT1
020400980728     I              BLPFT2                      ARBFT2
020500980728     I              BLPDT2                      ARBDT2
020600980728     I              BLPFT3                      ARBFT3
020700980728     I              BLPDT3                      ARBDT3
020800980506      *--------------------------------------------
020900990806     ITITAS000
021000981126     I              TASAAS                      ARBAAS
021100981126     I              TASLNP                      ARBLNP
021200981126     I              TASNRS                      ARBNRS
021300981126     I              TASNSP                      ARBNSP
021400981126     I              TASMGS                      ARBMGS
021500981126     I              TASNCL                      ARBNCL
021600981126     I              TASKSC                      ARBKSC
021700981126     I              TASCCM                      ARBCCM
021800990806     I              TASPKF                      ARBPKF
021900981126     I              TASFVF                      ARBFVF
022000981126     I              TASVLF                      ARBVLF
022100981126     I              TASLNA                      ARBLNA
022200981126     I              TASIAS                      ARBIAS
022300981126     I              TASVAS                      ARBVAS
022400981126     I              TASTSP                      ARBTSP
022500981126     I              TASCTR                      ARBCTR
022600981126     I              TASLL1                      WDDT
022700990309     i              TASCCA                      ARBCCA
022800981126
022900990806     ITITAS010
023000981126     I              TASAAS                      ARBAAS
023100981126     I              TASLNP                      ARBLNP
023200981126     I              TASNRS                      ARBNRS
023300981126     I              TASNSP                      ARBNSP
023400981126     I              TASMGS                      ARBMGS
023500981126     I              TASNCL                      ARBNCL
023600981126     I              TASKSC                      ARBKSC
023700981126     I              TASCCM                      ARBCCM
023800990806     I              TASPKF                      ARBPKF
023900981126     I              TASFVF                      ARBFVF
024000981126     I              TASVLF                      ARBVLF
024100981126     I              TASLNA                      ARBLNA
024200981126     I              TASIAS                      ARBIAS
024300981126     I              TASVAS                      ARBVAS
024400981126     I              TASTSP                      ARBTSP
024500981126     I              TASCTR                      ARBCTR
024600981126     I              TASLL1                      WDDT
024700990309     i              TASCCA                      ARBCCA
024800981126
024900990806     ITITASP00
025000981126     I              TASAAS                      ARBAAS
025100981126     I              TASLNP                      ARBLNP
025200981126     I              TASNRS                      ARBNRS
025300981126     I              TASNSP                      ARBNSP
025400981126     I              TASMGS                      ARBMGS
025500981126     I              TASNCL                      ARBNCL
025600981126     I              TASKSC                      ARBKSC
025700981126     I              TASCCM                      ARBCCM
025800990806     I              TASPKF                      ARBPKF
025900981126     I              TASFVF                      ARBFVF
026000981126     I              TASVLF                      ARBVLF
026100981126     I              TASLNA                      ARBLNA
026200981126     I              TASIAS                      ARBIAS
026300981126     I              TASVAS                      ARBVAS
026400981126     I              TASTSP                      ARBTSP
026500981126     I              TASCTR                      ARBCTR
026600981126     I              TASLL1                      WDDT
026700990309     i              TASCCA                      ARBCCA
026800981126
026900981126     ITNTMD000
027000981126     I              TMDAAS                      ARBAAS
027100981126     I              TMDLNP                      ARBLNP
027200981126     I              TMDNRS                      ARBNRS
027300981126     I              TMDNSP                      ARBNSP
027400981126     I              TMDRSM                      ARBRSM
027500981126     I              TMDINM                      ARBINM
027600981126     I              TMDCAM                      ARBCAM
027700981126     I              TMDLOM                      ARBLOM
027800981126     I              TMDPRM                      ARBPRM
027900981126     I              TMDNZM                      ARBNZM
028000981126     I              TMDRSD                      ARBRSD
028100981126     I              TMDIND                      ARBIND
028200981126     I              TMDCAD                      ARBCAD
028300981126     I              TMDLOD                      ARBLOD
028400981126     I              TMDPRD                      ARBPRD
028500981126     I              TMDNZD                      ARBNZD
028600981126      * ---------------------------------------------------------
028700980506      *
028800980506      * EMISSIONE PRIMA VIDEATA
028900980605    1C                   dow                     *inkc = *off  and *inkl = *off
029000980528     C                   EXFMT     FI04D01
029100980506     C                   CLEAR                   V1CMSG
029200980602     C                   setoff                                       404128
029300980602     C                   setoff                                       90
029400980506      * F3=FINE
029500980605    2c                   IF        *INKC =*off and
029600980605     c                             *inkl = *off
029700981126      * f14=Interrogazione C.A.
029800981126     C     *INKN         IFEQ      *ON
029900981126     c                   exsr      SUB_F14
030000981126     C                   iter
030100981126     c                   ENDIF
030200980506      * CONTROLLI VIDEATA
030300980506     C                   EXSR      CTRD01
030400980610      * F18=NOTE
030500980610    3c                   IF        *INKS =*on  and
030600980610     C                             *in28 = *off
030700980610     c* valorizzo la DS di gestione note/descrizioni e chiamo il PGM FIDN10R
030800980610     C                   CLEAR                   DSDN10
030900980716     c                   move      v1cdca        i10dta
031000980610     c                   movel     'N'           i10trc
031100980610     c                   movel     'C'           i10tpd
031200980716     c                   movel     i00fca        i10nks
031300980716     c                   movel     dsnumca       i10nkt
031400980610     c                   movel     'M'           i10flm
031500980610     c                   movel(P)  dsdn10        kpjbu
031600980610     C                   CALL      'FIDN10R'                            99
031700980610     C                   PARM                    kpjba
031800980610     C                   END
031900980610     c*
032000980528      * se non ci sono errori registro la fase
032100980528      * e ha dato il CMD 06
032200980528    4C                   if        *in28 = *off and
032300990114     c                             (*inkf = *on or *inkq = *on)
032400980506      * Aggiorno campi file da video e scrivo
032500980528     c                   exsr      RIEDCF
032600990114      * vado A FINE PGM solo se è stato dato KF oppure KQ ed è stato dato F3
032700990118     c                   if        *inkf or (*inkq and o00f03 = 'S') or
032800990118     c                             (*inkq and o00f03 = ' ' and o00f12 = ' ' )
032900981008     c                   leave
033000990114     c                   endif
033100980528      *
033200980528    4c                   endif                                                  *SI CMD 6 Conferma
033300980528    2c                   endif                                                  *NO CMD 3 Fine
033400980506      *
033500980528    1c                   enddo                                                  *fine ciclo KC *on
033600980528      *
033700990126     c                   clear                   o00f12
033800980605     c                   if        *inkc = *on
033900980605     c                   eval      o00f03 = 'S'
034000980605     c                   endif
034100980605      *
034200980605     c                   if        *inkl = *on
034300980605     c                   eval      o00f12 = 'S'
034400980605     c                   endif
034500980605      *
034600980716     c                   clear                   dsdn10
034700980610     c                   movel     'C'           i10tla
034800980610     c                   movel     dsdn10        kpjbu
034900980610     c                   call      'FIDN10R'
035000980610     c                   parm                    kpjba
035100980826     C                   movel     fidn00ds      KPJBU
035200980506     C                   EVAL      *INLR = *ON
035300981126      ****************************************************************
035400981126      * F14 = INTERROGAZIONE C.A.
035500981126      ****************************************************************
035600981126     C     SUB_F14       BEGSR
035700981126      *
035800981126      *
035900981126     c                   movel     'I'           I00mod
036000981126      *
036100981126     c                   movel     FIDN00DS      kpjbu
036200981126      *
036300981126     c                   call      'FIDN01R'
036400981126     c                   parm                    kpjba
036500981126      *
036600981126     c                   endsr
036700980506      *****************************************************************
036800980506      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
036900980506      *****************************************************************
037000980506     C     CTRD01        BEGSR
037100980528      * Controllo data fase
037200980528     C                   IF        V1Cdfs <> 0
037300980528     C                   MOVE      V1Cdfs        G02DAT
037400980528     C                   eval      g02err = *blanks
037500980528     C                   CALL      'XSRDA8'
037600980528     C                   PARM                    WLBDAT
037700980528     C                   if        G02ERR = '1'
037800980528     C                   SETON                                        289040
037900980528     C                   MOVEL     MSG(1)        V1CMSG
038000980528     C                   else
038100980605     C                   z-add     G02DAT        V1Cdfs
038200980528     C                   MOVE      G02INV        wdfs
038300980528     C                   endif
038400980528     C                   else
038500980528     C                   SETON                                        289040
038600980528     C                   MOVEL     MSG(1)        V1CMSG
038700980528     C                   ENDIF
038800980528      * controllo che la data fase sia maggiore della data apertura C.A.
038900980528     c                   if        *in28 = *off and
039000980528     c                             wdatac > wdfs
039100980528     C                   SETON                                        289040
039200980528     C                   MOVEL     MSG(5)        V1CMSG
039300980528     C                   Endif
039400980805      * controllo che la data fase sia minore o uguale ad oggi
039500980805     c                   if        *in28 = *off and
039600980805     c                             dateu < wdfs
039700980805     C                   SETON                                        289040
039800980805     C                   MOVEL     MSG(6)        V1CMSG
039900980805     C                   Endif
040000980528      *controllo il campo dell'ora
040100980528     c                   if        *in28 = *off
040200980602     c                   movel     v1chfc        ora
040300980602     c                   move      v1chfc        min
040400980602     c                   if        ora > 24 or min > 60
040500980602     c                   eval      *in41 = *on
040600980602     c                   endif
040700980528     c                   if        *in41 = *on
040800980601     c                   eval      *in28 = *on
040900980601     c                   movel     MSG(2)        v1cmsg
041000980528     c                   endif
041100980528      *
041200980528     c                   endif
041300980528     c
041400980507      *
041500980506     C                   ENDSR
041600980716      *****************************************************************
041700980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
041800980507      *****************************************************************
041900980528     C     RIEDCF        BEGSR
042000980528      *  caricamento file storico delle fasi
042100980528     C                   movel     i00aac        Dcfaac
042200980528     C                   movel     i00fil        Dcffil
042300980602     C                   z-add     i00nca        Dcfnca
042400980528      *
042500980528     C                   movel     i00fca        Dcffca
042600980528     C                   movel     dctptr        Dcfptr
042700980528     C                   z-add     wdfs          Dcfdfc
042800980528     C                   z-add     v1chfc        Dcfhfc
042900980528     C                   movel     i00fgs        Dcffev
043000980528     C                   movel     knmus         Dcfpru
043100980528      *
043200980528     c                   clear                   dcflet
043300980907     c                   clear                   dcfftr
043400980907     c                   clear                   dcfdtr
043500980507      *
043600980528     c                   WRITE     fndcf000
043700980528      * ricerca fase successiva con la quale aggiorno la testata C.A.
043800990114      * solo se è stato dato F6 di conferma
043900980528      *  caricamento ds per richiamo pgm di comodo
044000990114     c                   if        *inkf = *on
044100990114      *
044200980721     c                   clear                   fidn05ds
044300981030     C                   if        DCTnev > *zeros
044400980605     c                   movel     'E'           I05fpe
044500980605     c                   else
044600980605     c                   movel     'N'           i05fpe
044700980605     c                   endif
044800981030     C                   if        DCTnde > *zeros
044900981030     c                   movel     'D'           I05fde
045000981102     c                   else
045100981102     c                   movel     'N'           I05fde
045200981102     c                   endif
045300980528     C                   movel     'F'           I05MOD
045400980528     C                   movel     i00fca        i05fca
045500980528     C                   movel     i00fpr        i05fpr
045600980528     C                   movel     dctptr        i05tpc
045700980528     C                   movel     'O'           I05ffs
045800980721     c                   z-add     DCFdfc        i05dta
045900980721      * valorizzo il tipo anomalia CA
046000980721     c                   move      DCTtad        i05tad
046100981125      * valorizzo numero c.a.
046200981125     c                   z-add     dctaac        i05aac
046300981125     c                   z-add     dctfil        i05fil
046400981125     c                   z-add     dctnca        i05nca
046500980528      * richiamo il pgm di ricerca
046600980602     C                   call      'FIDN05R'
046700980602     C                   parm                    kpjba
046800980610     C                   parm                    fidn05ds
046900980528     C                   if        o05err = ' '
047000980528     C                   movel     o05rec        fndfa
047100980528      * valorizzo i campi della testata C.A.
047200980528      * fase
047300980528     C                   movel     i00fca        dctfca
047400980528     c* sede
047500980528     c                   if        dfagfs = 'S'
047600980528     c                   z-add     46            dctgfc
047700980528     c                   endif
047800980528     c* P O partenza
047900980528     c                   if        dfagfs = 'P'
048000990111     c                   movel     §dctlnpc      dctgfc
048100980528     c                   endif
048200980528     c* P O arrivo
048300980528     c                   if        dfagfs = 'A'
048400981230     c                   z-add     i00lna        dctgfc
048500980528     c                   endif
048600981023     c* P O apertura C.A.
048700981023     c                   if        dfagfs = 'F'
048800981023     c                   z-add     i00fil        dctgfc
048900981023     c                   endif
049000040330      * flag e data trasmissione
049100040330     c                   clear                   DCTft1
049200040330     c                   clear                   DCTft2
049300980528     c                   endif
049400040330      *
049500990114     c                   update    fndct000
049600990114     c*
049700990114     c                   else
049800990114     c*
049900990114     c                   unlock    fndct01l
050000990114     c*
050100990114     c                   endif
050200990114     c*
050300981126      * Conferma se esiste l'interlocutore
050400980716     c     v1cnco        ifne      *blanks
050500980716     c                   clear                   dsdn10
050600980716     c                   movel     'W'           i10flm
050700980716     c                   movel     'C'           i10tpd
050800980716     c                   movel     'N'           i10std
050900980716     c                   movel     'D'           i10trc
051000980716     c                   movel     dsnumca       i10nkt
051100980716     c                   movel     v1cnco        i10not
051200980716     c                   movel(P)  dsdn10        kpjbu
051300980716     C                   CALL      'FIDN10R'                            99
051400980716     C                   PARM                    kpjba
051500980716     c                   endif
051600980610     c* conferma note
051700980610     C                   CLEAR                   DSDN10
051800980610     C                   MOVEL     'C'           I10FLM
051900980716     c                   movel     'C'           i10tpd
052000980716     c                   movel     'N'           i10trc
052100980610     c                   movel(P)  dsnumca       i10nkt
052200980716     c                   movel     i00fca        i10nks
052300980610     c                   movel(P)  dsdn10        kpjbu
052400980610     C                   CALL      'FIDN10R'                            99
052500980610     C                   PARM                    kpjba
052600990114     c*
052700990114     c* se è stato dato cmd16 vado a gestire l'esito transazione
052800990114     c                   if        *inkq = *on and *in81 = *on
052900990223      * se richiamo l'altro pgm esito transazione chiudo il pgm delle note
053000990223     c                   clear                   dsdn10
053100990223     c                   movel     'C'           i10tla
053200990223     c                   movel     dsdn10        kpjbu
053300990223     c                   call      'FIDN10R'
053400990223     c                   parm                    kpjba
053500120723     ***C                   movel     fidn00ds      KPJBU
053600120723     ***c***
053700120723     ***c                   movel     wfaes         i00fca
053800120723     ***c                   movel     fidn00ds      kpjbu
053900120723     ***c                   call      'FIDN08R'
054000120723     ***c                   parm                    kpjba
054100120723     ***c* controllo se è stato dato f12 o f03
054200120723     ***c                   movel     kpjbu         fidn00ds
054300120723     ***c*
054400120723     ***c                   if        o00f03 = 'S' or o00f12 = 'S'
054500120723     ***c*
054600120723     ***c* annullo e cancello la fase del contatto
054700120723     ***c     keydcf        chain     fndcf01l                           31
054800120723     ***c  n31              delete    fndcf000
054900120723     ***c* riaggancio la testata CA
055000120723     ***c*
055100120723     ***C     kdct          CHAIN     fndct000                           31
055200120723     ***c*
055300120723     ***c                   movel     wfaco         i00fca
055400120723     ***c*
055500120723     ***c                   endif
055600120723     c*
055700120723     c                   endif
055800980528     c
055900980507     C                   ENDSR
056000981126      *****************************************************************
056100981126      *  CONTROLLO TIPO BOLLA
056200981126      *****************************************************************
056300981126     C     CTR_tipobo    BEGSR
056400981126      *
056500981126      *
056600981126     C                   movel     'TB'          COD
056700981126     C                   movel(P)  TAStbl        KEY
056800981126     C     ktab          CHAIN     TABEL                              31
056900981126     C                   movel     TBLUNI        DSTB
057000981126     c                   if        §TBrbl <> 'R'
057100981126     C                   movel     §TBtpo        FLGTB1
057200981126     c                   endif
057300981126      *
057400981126     C                   ENDSR
057500990223      *****************************************************************
057600990223      *   ROUTINE LANCIO CONTROLLO COLLI DA CONSEGNARE
057700990223      *****************************************************************
057800990223     C     COLLI         BEGSR
057900990223      *
058000990223     C                   CLEAR                   FIDN53DS
058100990223     C*
058200990223     c                   eval      i53aas = i00aas
058300990223     c                   eval      i53lnp = i00lnp
058400990223     c                   eval      i53nrs = i00nrs
058500990223     c                   eval      i53nsp = i00nsp
058600990223     c* Richiamo il programma che controlla i colli ancora da consegnare
058700990223     c                   movel     fidn53ds      kpjbu
058800990223     c                   call      'FIDN53R'
058900990223     c                   parm                    kpjba
059000990223     c*
059100990223     c                   movel     kpjbu         fidn53ds
059200990223     c* se c'è errore è perchè ci sono dei colli da consegnare
059300990223     c                   if        o53err = 'E'
059400990223     c                   seton                                        28
059500990223     c                   movel     msg(7)        v1cmsg
059600990223     c                   endif
059700990223     c*
059800990223     c                   movel     fidn00ds      kpjbu
059900990223     c*
060000990223     c                   endsr
060100990311     C*-------------------------------------------------------------------------*
060200990311     c* REC_DKA   Valorizzo i dati del beneficiario dal file FNDKA
060300990311     C*-------------------------------------------------------------------------*
060400990311     c     REC_DKA       BEGSR
060500990311     C*
060600990311     c                   movel     dkarag        v1crsm
060700990311     c                   movel     dkacap        v1ccam
060800990311     c                   movel     dkaloc        v1clom
060900990311     c                   movel     dkaprv        v1cprm
061000990311     c                   movel     dkatel        v1ctel
061100990311     c                   movel     dkafax        v1cfax
061200990311     c                   movel     dkanaz        key
061300990311     c*
061400990311     c                   endsr
061500980506      *****************************************************************
061600980506      *   ROUTINE INIZIALE
061700980506      *****************************************************************
061800980506     C     *INZSR        BEGSR
061900980506      *
062000980506     C     *ENTRY        PLIST
062100980506     C                   PARM                    KPJBA
062200980610     C                   MOVEL     KPJBU         fidn00ds
062300980506      *
062400980506     C                   Z-ADD     1             CODUT
062500980506     C                   CALL      'X§PARUT'
062600980507     C                   PARM                    UT§DSE0F
062700980506     C                   MOVEL     RAGUT         RSUT
062800980508     C                   MOVEL     REC80         CNCR80
062900980506      *
063000981126      *
063100981126     C                   IF        SIMFEL = *zeros
063200990806     C                   open      TITAS30C
063300981126     C                   open      TNTMD01L
063400981126     c                   Endif
063500980728      *
063600980728     c     kbol01        klist
063700980728     c                   kfld                    i00aas
063800980728     c                   kfld                    i00lnp
063900980728     c                   kfld                    i00nrs
064000980728     c                   kfld                    i00nsp
064100991116      *
064200991116     c     kar6          klist
064300991116     c                   kfld                    i00aas
064400991116     c                   kfld                    i00lnp
064500991116     c                   kfld                    i00nrs
064600991116     c                   kfld                    i00nsp
064700991116     c                   kfld                    trcar6
064800980605     C* ACCESSO   TABEL
064900980605     C     KTAB          KLIST
065000980605     C                   KFLD                    CODUT
065100980605     C                   KFLD                    COD               2
065200980605     C                   KFLD                    KEY               8
065300980605     c                   movel     '15'          cod
065400980528      *
065500980528     C     Kdct          klist
065600980528     C                   KFLD                    kaac
065700980528     C                   KFLD                    kfil
065800980528     C                   KFLD                    knca
065900981230      *
066000981230     C     Kdka          klist
066100981230     C                   KFLD                    kaac
066200981230     C                   KFLD                    kfil
066300981230     C                   KFLD                    knca
066400981230     C                   KFLD                    wtrc
066500980721      *
066600980721     c     kdcf          klist
066700980721     c                   kfld                    dctaac
066800980721     c                   kfld                    dctfil
066900980721     c                   kfld                    dctnca
067000990114      *
067100990114     c     keydcf        klist
067200990114     c                   kfld                    dcfaac
067300990114     c                   kfld                    dcffil
067400990114     c                   kfld                    dcfnca
067500990114     c                   kfld                    dcfdfc
067600990114     c                   kfld                    dcfhfc
067700981201      *
067800991115     c*!*  kbl4          klist
067900991115     c*!*                kfld                    i00aas
068000991115     c*!*                kfld                    i00lnp
068100991115     c*!*                kfld                    i00nrs
068200991115     c*!*                kfld                    i00nsp
068300991115     c*!*                kfld                    KTRC
068400980721      *
068500980728     c     kspe          klist
068600980728     c                   kfld                    tipkey
068700980728     c                   kfld                    v1cksm
068800980728     c                   kfld                    codluo
068900981201      *
069000980728     c                   eval      codluo = '006'
069100980728     c                   eval      tipkey = 'L'
069200980528      *
069300990311     C                   TIME                    W0140
069400980507     C                   MOVE      W0140         WDTGIO
069500980507     C                   MOVEL     W0140         WORA
069600980528     c                   eval                    v1cdfs=wdtgio
069700980805      * UDATE IN AAAAMMGG
069800980805     C                   Z-ADD     WDTGIO        G02DAT
069900980805     C                   MOVEL     *BLANK        G02ERR
070000980805     C                   CALL      'XSRDA8'
070100980805     C                   PARM                    WLBDAT
070200980805     C                   MOVEL     G02INV        DATEU
070300980805      *
070400980602     c                   movel     WORA          v1chfc
070500980528      * decodifico il Punto Operatico C.A.
070600980601     c                   move      i00fil        v1cpog
070700980601     c     i00fil        chain     azorg                              31
070800990111     c* n31              movel     orgdes        v1ddpo
070900980528      * valorizzo il capo anno e numero C.A.
071000980601     c                   z-add     i00nca        v1cnca
071100980528      * decodifico la fase selezionata
071200980601     c                   move      i00fca        v1cfca
071300980601     c     i00fca        chain     fndfa01l                           31
071400980528     c  n31              movel     dfades        v1ddfa
071500981217      *
071600981217     c                   movel     *blanks       vcttit
071700981217     c                   if        i00fca = wfaco
071800990114     c                   seton                                        81
071900981217     c                   movel     tit_1         vcttit
072000981217     c                   else
072100981217     c                   movel     tit_2         vcttit
072200981217     c                   endif
072300990126      * controllo se reso per avaria
072400990126      *
072500990126     c                   eval      *in50 = ( i00fca = wfaav )
072600990126      *
072700980528      * aggancio la C.A. per recuperare i dati anagrafici della persona
072800980528     c                   z-add     i00aac        Kaac
072900980528     c                   z-add     i00fil        Kfil
073000980528     c                   z-add     i00nca        Knca
073100980528     C     kdct          CHAIN     fndct000                           31
073200981230     c* flag operativi
073300981230     c                   movel     dctflo        ddct01
073400980528      * controllo se il partner transazione è il MITTENTE o il DESTINATARIO
073500980728      * recupero il mittente ed il destinatario
073600980728    1c                   if         i00tpb = 'P'
073700980728     c     kbol01        chain     fnblp000                           30
073800981126     c                   endif
073900981126    1c                   if         i00tpb = 'A'
074000980728     c     kbol01        chain     fnarb000                           30
074100980728    1c                   endif
074200981126    1c                   if         i00tpb = 'S'
074300990806     c     kbol01        chain     TITAS30c                           30
074400981126     c     kbol01        chain     tntmd000                           30
074500981126     c                   exsr      CTR_tipobo                                   Ctr tipo bolla
074600981126     c                   if        FLGTB1 = *blanks
074700990806     c     kbol01        reade     TITAS30c                               30
074800981126     c                   endif
074900981126    1c                   endif
075000981201      *
075100981230      * Se assegnato in partenza verifico il codice destinatario
075200981230     c* in caso di cambio di porto il tipo porto è il contrario di quello della CA
075300990305     c                   SELECT
075400990305     c*
075500110503    1c**                 when      arbcca <> '9'
075600110503    1c                   when      §DCTCCA <> '9'
075700981230     c                   eval      W_porto = §dctport
075800990305     c*
075900990305    2c                   when      §dctport = 'F'
076000981230     c                   eval      W_porto = 'A'
076100990305     c*
076200990305    2c                   when      §dctport = 'A'
076300981230     c                   eval      W_porto = 'F'
076400990305     c*
076500990305    2c                   endsl
076600990305    1c*
076700981230     c*
076800991115     c*!*                clear                   DSBL4A
076900991115     c                   clear                   AR6KSC
077000990111    1c                   IF        I00tpb = 'P'  and  W_porto  = 'A'
077100991115     c*!*                movel     'A'           KTRC
077200991115     c*!*  Kbl4          CHAIN     FNBL4000                           30
077300991115     c*!*N30              movel     BL4NOT        DSBL4A
077400991116     c                   eval      trcar6 = '1'
077500991116     c     Kar6          CHAIN     FIAR6000                           30
077600990111    1c                   ENDIF
077700981201
077800980728      * dati bolla
077900990111    1c                   IF        W_porto  = 'F'
078000980728     c                   move      arbksc        VTCksm
078100980728     c                   move      *zeros        VTCksd
078200980728     c                   ELSE
078300980728     c                   move      arbccm        VTCksm
078400991115    2c*!*                if        §B4ksf <> *blanks
078500991115     c*!*                movel     §B4ksf        VTCksd
078600991115    2c                   if        AR6ksc <> *zeros
078700991115     c                   movel     AR6ksc        VTCksd
078800981201     c                   else
078900981201     c                   movel     arbksc        VTCksd
079000990111    2c                   endif
079100990111    1c                   ENDIF
079200980728      *
079300990311      *
079400990311     c* cerco i dati anagrafici in fndka
079500990311     c* controllo se c'è RIVALSA
079600990311     c                   movel     'R'           wtrc                           RIVALSA
079700990311     c     kdka          chain     fndka01l                           30
079800990909     c                   if        *in30 = *off  and DKAatb = *blanks
079900990311     c* recupero i dati
080000990311     c                   exsr      REC_DKA
080100990311     c*
080200990311     c                   ELSE
080300990311     c*
080400990311     c                   SELECT
080500990311      *
080600990311      * se e' valorizzato il BENEFICIARIO ALTERNATIVO
080700990311    2c                   WHEN      dctksc <> *zeros
080800990311      *
080900990311     c                   movel     dctksc        v1cksm
081000990311     c                   move      v1cksm        w0040             4 0
081100990311      *
081200990311      * codice BENEFICIARIO NON CODIFICATO
081300990311      *
081400990311    2c                   if        w0040 = 8888
081500990311      *
081600990311     c* cerco i dati anagrafici in fndka
081700990311     c                   movel     'C'           Wtrc                           CLIENTE
081800990311     c     kdka          chain     fndka01l                           30
081900990909     c                   if        *in30 = *off  and DKAatb = *blanks
082000990311     c                   exsr      REC_DKA
082100990311    3c                   endif
082200990311      *
082300990311   x2c                   else
082400990311      *
082500990311      * codice BENEFICIARIO CODIFICATO
082600040519
082700040519      * carico i dati del beneficiario dall'anagrafico luoghi
082800040519     c     kSpe          Chain     Fnspe01l
082900040520if  3c                   If        %Found(Fnspe01l)
083000040519     c                   movel     sperag        v1crsm
083100040519     c                   movel     specap        v1ccam
083200040519     c                   movel     speloc        v1clom
083300040519     c                   movel     spepro        v1cprm
083400040519     c                   movel     spetel        v1ctel
083500040519     c                   movel     spefax        v1cfax
083600040519     c                   movel     spenaz        key
083700040519   x3c                   Else
083800040519      * carico i dati del beneficiario dall'anagrafico clienti
083900990311      *
084000990311     c                   clear                   tibs69ds
084100990311     c                   clear                   ds_aco
084200990311     c                   clear                   ds_ind
084300990311     c                   clear                   ds_clp
084400990311     c                   clear                   ds_cls
084500990311      *
084600990311     c                   movel     'L'           i69tla
084700990311     c                   z-add     dctksc        i69kac
084800990311     c                   z-add     dctksc        i69kin
084900990311     c                   z-add     dctksc        i69kcs
085000990311      *
085100990311     c                   CALL      'TIBS69R'
085200990311     c                   parm                    tibs69ds
085300990311     c                   parm                    ds_aco
085400990311     c                   parm                    ds_ind
085500990311     c                   parm                    ds_clp
085600990311     c                   parm                    ds_cls
085700990311      *
085800990311     c                   movel     acorag        v1crsm
085900990311     c                   movel     indcit        v1clom
086000060119     c                   movel     indcae        v1ccam
086100990311     c                   movel     indprv        v1cprm
086200990311     c                   movel     indtel        v1ctel
086300990311     c                   movel     indtlf        v1cfax
086400990311     c                   movel     indsta        key
086500040519
086600040519e   3c                   EndIf
086700990311      *
086800990311     c                   endif
086900990311      *
087000990311     c                   OTHER
087100990311      *
087200990311      * se non e' valorizzato il beneficiario alternativo prendo o il MITTENTE o il DESTINATARIO
087300990311      *                                                                  S P E D I Z I O N E
087400990311      *                                                               --------------------------
087500990311    2c                   if        dctptr = 'M' or (dctptr = ' ' and *in50)
087600990311     c                   movel     vtcksm        v1cksm
087700990311    2c                   endif
087800990311    2c                   if        dctptr = 'D'
087900990311     c                   movel     vtcksd        v1cksm
088000990311    2c                   endif
088100990311      *
088200060118      * recupero dai dati della bolla se 8888 o 9999
088300990311      *
088400060118     c                   move      v1cksm        Com04
088500060118      *
088600060118     c                   If        Com04 = 8888  or Com04 = 9999
088700060118
088800990311    3c                   if        dctptr = 'M'  or (dctptr = ' ' and *in50)
088900060118     c
089000990311     c                   movel     arbrsm        V1Crsm
089100990311     c                   move      arbcam        V1Ccam
089200990311     c                   movel     arblom        V1Clom
089300990311     c                   movel     arbprm        V1Cprm
089400990311     c                   movel     arbnzm        key
089500990311   x3c                   else
089600990311     c                   movel     arbrsd        V1Crsm
089700990311     c                   move      arbcad        V1Ccam
089800990311     c                   movel     arblod        V1Clom
089900990311     c                   movel     arbprd        V1Cprm
090000990311     c                   movel     arbnzd        key
090100990311    3c                   endif
090200060118
090300060118     c                   else
090400060118      * carico i dati del beneficiario dall'anagrafico clienti
090500060118      *
090600060118     c                   clear                   tibs69ds
090700060118     c                   clear                   ds_aco
090800060118     c                   clear                   ds_ind
090900060118     c                   clear                   ds_clp
091000060118     c                   clear                   ds_cls
091100060118      *
091200060118     c                   movel     'L'           i69tla
091300060119     c                   z-add     v1cksm        i69kac
091400060119     c                   z-add     v1cksm        i69kin
091500060119     c                   z-add     v1cksm        i69kcs
091600060118      *
091700060118     c                   CALL      'TIBS69R'
091800060118     c                   parm                    tibs69ds
091900060118     c                   parm                    ds_aco
092000060118     c                   parm                    ds_ind
092100060118     c                   parm                    ds_clp
092200060118     c                   parm                    ds_cls
092300060118      *
092400060118     c                   movel     acorag        v1crsm
092500060118     c                   movel     indcit        v1clom
092600060118     c                   movel     indprv        v1cprm
092700060119     c                   movel     indcae        v1ccam
092800060118     c                   movel     indtel        v1ctel
092900060118     c                   movel     indtlf        v1cfax
093000060118     c                   movel     indsta        key
093100060118
093200990311      * cerco nell'anagrafico luoghi il cliente relativo al codice luogo 6 specifico
093300990311      * per i contatti in caso di danni ma non per il sollecito avaria
093400060119     c******             if        not *in50
093500990311     c     kspe          chain     fnspe01l                           30
093600990311    3c     *in30         ifeq      *off
093700990311     c                   movel     sperag        v1crsm
093800990311     c                   movel     specap        v1ccam
093900990311     c                   movel     speloc        v1clom
094000990311     c                   movel     spepro        v1cprm
094100990311     c                   movel     spetel        v1ctel
094200990311     c                   movel     spefax        v1cfax
094300990311     c                   movel     spenaz        key
094400990311      *
094500990311     c                   endif
094600060118      *
094700060118     c                   endif
094800990311      *
094900060119     c******             endif                                                  * luogo 6
095000990311      *
095100990311     c                   ENDSL
095200990311      *
095300990311     c                   endif                                                  * no rivalsa
095400980728     c
095500980605     c* decodifico la nazione se è diversa da blank
095600980728    2c                   if        key <> *blanks
095700980605     c     ktab          chain     tabel                              30
095800980728    3c                   if        *in30 = *off and
095900980605     c                             tblflg = ' '
096000980605     c                   movel     tbluni        v1cnaz
096100980728    3c                   endif
096200980728    2c                   endif
096300990311    1c*
096400980604      * calcolo data apertura C.A.
096500980604     C                   eval      wdatac = (DCTaac * 10000) + DCTmgc
096600980604     c*
096700980604     C                   CLEAR                   WLBDAT
096800980604     C                   Z-ADD     wdatac        G02inv
096900980604     C                   movel     '3'           G02err
097000980604     C                   CALL      'XSRDA8'
097100980604     C                   PARM                    WLBDAT
097200980604     c                   z-add     g02dat        v1cdca
097300980604      * numero spedizione
097400040722     c**""**             movel     dctlnp        v1clnp
097500040722     c**""**             movel     dctnrs        v1cnrs
097600040722     c**""**             movel     dctnsp        v1cnsp
097700040722     c                   movel     i00lnp        v1clnp
097800040722     c                   movel     i00nrs        v1cnrs
097900040722     c                   movel     i00nsp        v1cnsp
098000981126      * recupero se esiste l'interlocutore
098100980716     c                   clear                   dsdn10
098200980716     c                   movel     'C'           i10tpd
098300980716     c                   movel     'R'           i10flm
098400980716     c                   movel     'N'           i10std
098500980716     c                   movel     'D'           i10trc
098600980716     c                   movel     dsnumca       i10nkt
098700980716     c                   movel(P)  dsdn10        kpjbu
098800980716     C                   CALL      'FIDN10R'                            99
098900980716     C                   PARM                    kpjba
099000980716     c                   movel     kpjbu         dsdn10
099100980716     c     o10err        ifeq      *blanks
099200980716     c                   movel     o10not        v1cnco
099300980716     c                   else
099400980716     c                   movel     *blanks       v1cnco
099500980716     c                   end
099600981105      * se esiste ancora delle merce da consegnare relativa alla spedizione emetto un messaggio
099700981105      * di avvertimento solo se la CA è in fase di liquidazione transattiva
099800981105    1c                   if        dctfpr =  'T'
099900990121      * routine controllo colli se la data di consegna merce in testata = 0
100000990121     c                   if        arbdcm > 0
100100990121     c*
100200990121     c                   else
100300990223     c*
100400990223     c                   exsr      COLLI
100500990223     c*
100600981105    1c                   endif
100700990121    1c                   endif
100800990111      * calcolo data spedizione
100900990111     C                   eval      wdaspe = (arbaas * 10000) + arbmgs
101000990111     c*
101100990111     C                   CLEAR                   WLBDAT
101200990111     C                   Z-ADD     wdaspe        G02inv
101300990111     C                   movel     '3'           G02err
101400990111     C                   CALL      'XSRDA8'
101500990111     C                   PARM                    WLBDAT
101600990111     c                   z-add     g02dat        v1cdsp
101700990114      *
101800990114      * se ci sono delle note accendo indicatore di reverse immage al tasto funzionale F18
101900990114     c*
102000990114     c* cerco i dati nel file fndcs
102100990114     C                   CLEAR                   DSDN42
102200990114     c                   movel     'N'           i42trc
102300990114     c                   movel     'C'           i42tpd
102400990114     c                   movel     dsnumca       i42nkt
102500990114     c                   movel(P)  dsdn42        kpjbu
102600990114     C                   CALL      'FIDN42R'
102700990114     C                   PARM                    kpjba
102800990114     c*
102900990114     c                   movel     kpjbu         dsdn42
103000990114     c*
103100990114     c                   if        o42err = ' '
103200990114     c                   seton                                        80
103300990114     c                   endif
103400990114     c*
103500990114     c*
103600980506     C                   ENDSR
103700980506      *****************************************************************
103800980508** MSG  (Lungh. 78)                                                          *
103900980528Immettere Data Corretta                                                            1
104000980605Immettere Ora Corretta                                                             2
104100981130LIBERO                                                                             3
104200981130LIBERO                                                                             4
104300980528Immettere Data maggiore di Data Apertura C.A.                                      5
104400980805Immettere Data minore o uguale alla data odierna                                   6
104500981105ATTENZIONE !! Esiste ancora della merce da consegnare per questa spedizione        7

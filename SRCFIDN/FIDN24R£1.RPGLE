000100980506      *------------------------------------------------------------------------*
000200981027      * GESTIONE COLLI RITROVATI                                               *
000300980506      *------------------------------------------------------------------------*
000400980803      ****************************************************************
000500980506     H DECEDIT('0,') DATEDIT(*DMY.)
000600980506      *--------------------------------------------
000700981201      ****************************************************************
000800981201      *  FASI C.A. FISSATE A PROGRAMMA
000900981201      ****************************************************************
001000981201      *    900 = Chiusura CA         (WCHCA)
001100981201      ****************************************************************
001200981201      *  CAUSALE CHIUSURA  FISSATA A PROGRAMMA
001300981201      *_______________________________________________________________
001400981201      *     03 = Pareggio            (WCCH)
001500981201      ***************************************************************
001600981201      *
001700981027     Ffidn24D   CF   E             WORKSTN  sfile(fi24s01:nrr1)
001800981027     f                                      sfile(fi24s02:nrr2)
001900981022     Ffndfa01L  IF   E           k DISK
002000981028     Ffndct01l  uf   E           k DISK
002100981201     Ffndct02l  if   E           k DISK    rename(fndct000:fndct2)
002200981027     Ffndcd03l  if   E           k DISK
002300981028     Ffndcd02l  uf   E           k DISK    rename(fndcd000:fndcd2)
002400981201     Ffndcd01l  uf   E           k DISK    rename(fndcd000:fndcd1)
002500981029     Ffndcs01l  if A E           k DISK
002600981201     Ffndcf01l  O  A E           k DISK
002700040802     Ffndck01l  if   E           k DISK
002800981027     Ffnblp01l  iF   E           k DISK
002900991115     F*!*fnbl401l  iF   E           k DISK
003000991115     Ffiar601l  iF   E           k DISK
003100990202     Ffnarb01l  iF   E           k DISK
003200981027     Fazorg01l  if   E           k DISK
003300981022     Ftabel00f  if   E           k DISK
003400981022      ****************************************************************
003500981022      *  RIEPILOGO INDICATORI
003600981022      ***************************************************************
003700981029      * 05   - Sproteggo P.O. IN GESTIONE
003800981028      * 06   - non abilito il tasto funzionale F18
003900981218      * 11   - non viene gestita la chiusura ma solo la fase di analisi colli
004000981027      * 20/21- Gestione 1° subfile
004100981022      * 28   - ERRORE GENERICO
004200981022      * 29   - RIEMISSIONE FORMATO VIDEO
004300981028      * 30/32- COMODO
004400981029      * 39/44- Errori nella seconda videata
004500981027      * 50/51- Gestione 2° subfile
004600990122      * 81   - Campo Reverse Immage
004700981218      * 83   - Posizionamento Cursore Errore
004800981030      * 90   - COMODO
004900981030      * 91   - Lettura subfile
005000981022      *--------------------------------------------
005100981022     D                SDS
005200981022     D  VTCPGM                 1     10
005300981022      *
005400981028     D fidn00ds      E DS
005500981028     D fidn10ds      E DS
005600981029     D fnlv50ds      E DS
005700991115     D*!* dsbl4a        E DS
005800981028     D TRUL06DS      E DS
005900981028     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
006000981022      * Ds passaggio parametri a pgm ricerca tabelle
006100981028     D TIBS02DS      E DS
006200981022
006300981022      *--------------------------------------------
006400981022     D KPJBA         E DS
006500020502     D* rem                    1      3
006600020502     D* remfil                 4      6
006700981022     D CNCR80        E DS
006800981022     D UT§DSE0F      E DS
006900981022     d  tcu                  398    697
007000981022     D                                     DIM(50)
007100981022     d  kcu                  698    847P 0
007200981022     D                                     DIM(50)
007300981022     D                                     PACKEVEN
007400981027     D DTAD          E DS
007500981230     D DDCT01        E DS
007600070403     d og148         e ds
007700070403     d dslr02        e ds
007800981022      *--------------------------------------------
007900040802     D chisono         s              1
008000981022     D dateu           s              8  0
008100981029     D oramin          S              4  0
008200981022     D W0140           S             14  0
008300981022     D WDTGIO          S              8  0
008400981022     D WORA            S              6  0
008500981201     D prima           S              1
008600981028     D Wf03            S              1
008700981029     D W007a           S              7                                         * campo alfanumerico
008800981029     D W002a           S              2                                         * campo alfanumerico
008900981029     D X               S              1  0                                      * numeratore
009000981029     D wafls           s              3
009100981029     D wanrs           s              2
009200981029     D wAnsc           S              7
009300981029     D wadfv           s              4
009400981029     D wagio           s              2
009500981029     D wames           s              2
009600981029     D wafgs           s              3
009700981029     D wanps           s              2
009800981201     D Wchca           S                   like(dctfca)
009900981201     d                                     inz(900)
010000981201     D Wcch            S                   like(dctcch)
010100981201     d                                     inz('03')
010200981028     D WTPB            S                   like(I00TPB)
010300981028     D Wdch            S                   like(dcddch)
010400981028     D Wnco            S                   like(dctncn)
010500981028     D Wrknco          S                   like(dctncn)
010600981028     D WDatac          S              8  0
010700990309     D W1hpor          S                   like(v1hpor)
010800981028     D kaas            s                   like(dctaas)
010900981028     D klnp            s                   like(dctlnp)
011000981028     D knrs            s                   like(dctnrs)
011100981028     D knsp            s                   like(dctnsp)
011200981027     D kaac            s                   like(dcdaac)
011300981027     D kfil            s                   like(dcdfil)
011400981027     D knca            s                   like(dcdnca)
011500981022     D §cod            S                   LIKE(TBLcod)
011600981022     D §key            S                   LIKE(TBLkey)
011700991116     D trcar6          S                   LIKE(ar6trc)
011800991115     D*!* ktrc            S                   LIKE(BL4trc)
011900991115     d*!*                                     INZ('A')
012000981201     D kfar            S                   LIKE(DCDfar)
012100981027     d                                     INZ('S')
012200981029     d prgnot          S                   LIKE(dcspno)                         *progressivo note
012300981027     D nrr1            S              4  0
012400981027     D nrr2            S              4  0
012500981028     D DATA_eur        S               D   DATFMT(*eur)
012600070403     d wfilgeo         s              1    inz(*off)
012700981022      *
012800981028     d dsnumca         ds
012900981028     d  aac                    1      4  0
013000981028     d  fil                    5      7  0
013100981028     d  nca                    8     14  0
013200981022     D WLBDAT          DS                  INZ
013300981022     D  G02DAT                 1      8  0
013400981022     D  G02INV                 9     16  0
013500981022     D  G02ERR                17     17
013600981022     D  G02TGI                18     22  0
013700981022     D VIDIVA          DS
013800981022     D  w2civa                 1     14
013900981022     D  w2cive                15     16
014000981022     d TCUDS           DS
014100981022     d  f1                     1      1
014200981022     d  f3                     3      3
014300981022     d  f2                     2      2
014400981022     d  f4                     4      4
014500981022     d  f56                    5      6
014600981027      *  titolo videata (lunghezza massima 34)
014700981029     D TIT             C                   CONST('***  GESTIONE COLLI RITROVATI-
014800981029     D                                       ***')
014900981027      *  Descrizione opzioni (lunghezza massima 78)
015000981027     D DOPZ            C                   CONST('2=Manutenzione   4=Note in St-
015100981029     D                                     ampa   5=Visual. C.A.   8=Note C.A. -
015200981029     D                                      G=Gestione')
015300981027      *  titolo videata (lunghezza massima 34)
015400981027     D Kannullata      C                   CONST('   ANNULLATA                 -
015500981027     D                                         ')
015600981028     D Kanalizz        C                   CONST('   ANALIZZATA                -
015700981028     D                                         ')
015800981028      *   S C H I E R E
015900990225     D MSG             S             78    DIM(11) CTDATA PERRCD(1)             MSG VIDEO
016000981028     D L6              S              3  0 DIM(30)                              P.O.GESTITI
016100981027      *------------------------------------------------------------------------*
016200981027     IFNBLP000
016300981027     I              BLPATB                      ARBATB
016400981027     I              BLPAAS                      ARBAAS
016500981027     I              BLPLNP                      ARBLNP
016600981027     I              BLPNRS                      ARBNRS
016700981027     I              BLPNSP                      ARBNSP
016800981027     I              BLPCBO                      ARBCBO
016900981027     I              BLPMGS                      ARBMGS
017000981027     I              BLPFNS                      ARBFNS
017100981027     I              BLPSCL                      ARBSCL
017200981027     I              BLPFBR                      ARBFBR
017300981027     I              BLPFDN                      ARBFDN
017400981027     I              BLPLNA                      ARBLNA
017500981027     I              BLPZNC                      ARBZNC
017600981027     I              BLPKSC                      ARBKSC
017700981027     I              BLPRSM                      ARBRSM
017800981027     I              BLPINM                      ARBINM
017900981027     I              BLPCAM                      ARBCAM
018000981027     I              BLPLOM                      ARBLOM
018100981027     I              BLPNZM                      ARBNZM
018200981027     I              BLPPRM                      ARBPRM
018300981027     I              BLPFAP                      ARBFAP
018400981027     I              BLPCPI                      ARBCPI
018500981027     I              BLPRSD                      ARBRSD
018600981027     I              BLPIND                      ARBIND
018700981027     I              BLPCAD                      ARBCAD
018800981027     I              BLPLOD                      ARBLOD
018900981027     I              BLPNZD                      ARBNZD
019000981027     I              BLPPRD                      ARBPRD
019100981027     I              BLPFIN                      ARBFIN
019200981027     I              BLPGC1                      ARBGC1
019300981027     I              BLPGC2                      ARBGC2
019400981027     I              BLPCTR                      ARBCTR
019500981027     I              BLPCTS                      ARBCTS
019600981027     I              BLPFTM                      ARBFTM
019700981027     I              BLPTSP                      ARBTSP
019800981027     I              BLPNAS                      ARBNAS
019900981027     I              BLPNCL                      ARBNCL
020000981027     I              BLPNCP                      ARBNCP
020100981027     I              BLPPKC                      ARBPKC
020200981027     I              BLPPKB                      ARBPKB
020300981027     I              BLPPKF                      ARBPKF
020400981027     I              BLPNCR                      ARBNCR
020500981027     I              BLPVLC                      ARBVLC
020600981027     I              BLPFVB                      ARBFVB
020700981027     I              BLPVLB                      ARBVLB
020800981027     I              BLPFVF                      ARBFVF
020900981027     I              BLPVLF                      ARBVLF
021000981027     I              BLPQFT                      ARBQFT
021100981027     I              BLPVMD                      ARBVMD
021200981027     I              BLPVAD                      ARBVAD
021300981027     I              BLPIAS                      ARBIAS
021400981027     I              BLPVAS                      ARBVAS
021500981027     I              BLPCCM                      ARBCCM
021600981027     I              BLPRMN                      ARBRMN
021700981027     I              BLPRMA                      ARBRMA
021800981027     I              BLPRMO                      ARBRMO
021900981027     I              BLPFLS                      ARBFLS
022000981027     I              BLPNCD                      ARBNCD
022100981027     I              BLPNCA                      ARBNCA
022200981027     I              BLPXCO                      ARBXCO
022300981027     I              BLPCTM                      ARBCTM
022400981027     I              BLPFFD                      ARBFFD
022500981027     I              BLPTCR                      ARBTCR
022600981027     I              BLPDCR                      ARBDCR
022700981027     I              BLPHCR                      ARBHCR
022800981027     I              BLPDTI                      ARBDTI
022900981027     I              BLPHTI                      ARBHTI
023000981027     I              BLPDCE                      ARBDCE
023100981027     I              BLPNPI                      ARBNPI
023200981027     I              BLPNCI                      ARBNCI
023300981027     I              BLPNRC                      ARBNRC
023400981027     I              BLPDBR                      ARBDBR
023500981027     I              BLPNFV                      ARBNFV
023600981027     I              BLPDPC                      ARBDPC
023700981027     I              BLPDUC                      ARBDUC
023800981027     I              BLPFLP                      ARBFLP
023900981027     I              BLPDET                      ARBDET
024000981027     I              BLPDUT                      ARBDUT
024100981027     I              BLPDAM                      ARBDAM
024200981027     I              BLPNDC                      ARBNDC
024300981027     I              BLPDDC                      ARBDDC
024400981027     I              BLPDCP                      ARBDCP
024500981027     I              BLPDCM                      ARBDCM
024600981027     I              BLPHMC                      ARBHMC
024700981027     I              BLPTC1                      ARBTC1
024800981027     I              BLPTC2                      ARBTC2
024900981027     I              BLPCCA                      ARBCCA
025000981027     I              BLPTMC                      ARBTMC
025100981027     I              BLPGGA                      ARBGGA
025200981027     I              BLPGMA                      ARBGMA
025300981027     I              BLPGVA                      ARBGVA
025400981027     I              BLPFLE                      ARBFLE
025500981027     I              BLPTFP                      ARBTFP
025600981027     I              BLPFEA                      ARBFEA
025700981027     I              BLPTFA                      ARBTFA
025800981027     I              BLPFT1                      ARBFT1
025900981027     I              BLPDT1                      ARBDT1
026000981027     I              BLPFT2                      ARBFT2
026100981027     I              BLPDT2                      ARBDT2
026200981027     I              BLPFT3                      ARBFT3
026300981027     I              BLPDT3                      ARBDT3
026400981027      *
026500981022      *
026600981022      * ---------------------------------------------------------
026700981022      *
026800981030     c                   if        v1cfgs > 0
026900981030     c* emetto la richiesta del P.O. gestione    --------------------------------------------!
027000981030     c*--------------------------------------------------------------------------------------!
027100981218     c*                  exsr      SUB_FIRST                                                 !
027200981030     c*--------------------------------------------------------------------------------------!
027300981022      * EMISSIONE PRIMA VIDEATA
027400981029    1C                   dow       *inkc = *off
027500981027     c* caricamento dati subfile
027600981028     c                   exsr      SUB_CASF1
027700981027     C*
027800981027      * Se non ho caricato record emetto videata con segnalazione di errore
027900981028    2c                   if        nrr1 = 0
028000981029    3c                   dow       *inkc = *off
028100981027     c                   exfmt     fi24d01
028200981029      * f18=Cambio P.O. in gestione
028300981029     c   ks              eval      *in05 = *on
028400981029     c   ks              iter
028500981029
028600981029      * 05 on --> controllo P.O. in gestione
028700981029     c     *in05         ifeq      *on
028800981029     C                   exsr      SUB_CFGS
028900981029     c   28              iter
029000981029     c                   eval      *in05 = *off
029100981029     c                   leave
029200981029     c                   end
029300981029
029400981028    3c                   enddo
029500981028   x2c                   else                                                   * ci sono CA
029600981027      *
029700981028     c                   exsr      SUB_GESF1
029800981022      * F3=FINE
029900981028    3C                   if        *inkc = *on
030000981027     c                   leave
030100981028    3c                   endif                                                  * INKC
030200981027      *
030300981028    2c                   endif                                                  * NRR1 > 0
030400981027      *
030500981022    1c                   enddo                                                  *fine ciclo KC *on
030600981030      *
030700981030    2c                   endif                                                  * non sono in sede
030800981022      *
030900981028     c                   clear                   fidn10ds
031000981022     c                   movel     'C'           i10tla
031100981028     c                   movel     fidn10ds      kpjbu
031200981022     c                   call      'FIDN10R'
031300981022     c                   parm                    kpjba
031400981029     c*
031500981029     c                   clear                   tibs02ds
031600981029     c                   movel     'C'           t02tla
031700981029     c                   call      'TIBS02R'
031800981029     c                   parm                    kpjba
031900981029     c                   parm                    tibs02ds
032000981022      *
032100981022     C                   EVAL      *INLR = *ON
032200981027      *****************************************************************
032300981027      *   ROUTINE SCRITTURA SUBFILE TESTATE CA AVENTI COLLI RITROVATI
032400981027      *****************************************************************
032500981028     C     SUB_CASF1     BEGSR
032600981027     c* pulisco subfile
032700981027     c                   setoff                                           21
032800981027     c                   write     fi24c01
032900981028     c                   z-add     *zeros        nrr1
033000981027     c                   seton                                            21
033100981029     c*
033200981029     c                   clear                   kaac
033300981029     c                   clear                   kfil
033400981029     c                   clear                   knca
033500981027     c*
033600981027     c* leggo dal dettaglio colli se esistono dei records che hanno il
033700981027     c* flag di collo da analizzare uguale ad 'S'
033800981027     c*
033900981028     c     kfar          setll     fndcd03l
034000981028     c     kfar          reade     fndcd03l                               30
034100981027    1c                   dow       *in30 = *off
034200000301     c* i colli devono essere ritrovati da P.O. che è in gestione o dalla sua £6
034300000301     c                   if        dcdfgs <> v1cfgs
034400000301     c* verifico la sua £6
034500000301     c     dcdfgs        lookup    l6                                     45
034600000301     c  n45              goto      leggi
034700000301     c                   endif
034800000301     c*
034900000301    2c*****************+ if        dcdfgs = v1cfgs
035000981027     c* carico una sola c.a. anche se ritrovati + colli per la stessa
035100981028    3c                   if        knca <> dcdnca or
035200981027     c                             kaac <> dcdaac or
035300981027     c                             kfil <> dcdfil
035400981027     c*
035500981027     c                   eval      knca = dcdnca
035600981027     c                   eval      kaac = dcdaac
035700981027     c                   eval      kfil = dcdfil
035800981027     c*
035900981030     c     kdct          chain(n)  fndct01l                           31
036000990225     c*
036100981230     c                   movel     dctflo        ddct01
036200981230     c*
036300981230     c                   movel     §dctport      v1hpor
036400981230     c*
036500981201     c* se CA chiusa non do la possibilità di chiudere di nuovo    accendo l'11
036600981201     c                   if        dctdch > 0
036700981201     c                   eval      v1hi11 = 'S'
036800990225     c                   eval      v1hchi = 'S'
036900981201     c                   else
037000981201     c                   eval      v1hi11 = ' '
037100990225     c                   eval      v1hchi = ' '
037200981201     c                   endif
037300981218      * Cerco in L6 il P.O. gestione fase
037400981218     c                   eval      v1hges   = 'N'
037500990225     C                   if        DCTgfc > *zeros
037600981218     C     DCTgfc        LOOKUP    L6                                     31
037700981218     c   31              eval      v1hges   = 'S'
037800981218     C                   endif
037900990122     c*
038000990122     c                   movel     dctgfc        v1hgfc
038100981218     c* controllo se CA in gestione dal P.O.
038200981201     c*
038300040802      * verifico se sono in arrivo o in partenza
038400040802      *
038500040802     c                   clear                   chisono
038600040802     C     DCTlna        LOOKUP    L6                                     31
038700040802     c   31              eval      chisono  = 'A'
038800040802     c
038900981027     c                   z-add     DCTaac        V1Haac
039000981030     c                   z-add     DCTfca        V1Hfca
039100981027     c                   z-add     DCTaas        V1Haas
039200981027     c                   z-add     DCTlna        V1Hlna
039300981028     c                   z-add     DCTncn        V1Hncn
039400981027     c                   z-add     DCTfil        V1Cfil
039500981027     c                   z-add     DCTnca        V1Cnca
039600981027     c                   z-add     DCTaac        V1Cdca
039700981027     c                   movel     DCTmgc        V1Cdca
039800981027     c     *mdy          move      V1Cdca        DATA_eur
039900981027     c     *dmy          move      DATA_eur      V1Cdca
040000981027     c                   move      DCTaas        V1caas2
040100981027     c                   z-add     DCTlnp        V1Clnp
040200981027     c                   z-add     DCTnrs        V1Cnrs
040300981027     c                   z-add     DCTnsp        V1Cnsp
040400981027     c                   z-add     Dctfca        V1Cfca
040500040802      * se sono l'arrivo valorizzo con il numero spedizione esistente su FNDCK se esiste
040600040802     c                   if        chisono =  'A'
040700040802     c     kdct          chain     fndck01l
040800040802     c                   if        %found(fndck01l)
040900040802     c                   move      DCKaas        V1caas2
041000040802     c                   z-add     DCKaas        V1Haas
041100040802     c                   z-add     DCKlnp        V1Clnp
041200040802     c                   z-add     DCKnrs        V1Cnrs
041300040802     c                   z-add     DCKnsp        V1Cnsp
041400040802     c                   endif
041500040802     c                   endif
041600981027      *
041700981028    4c                   IF        DCTatb <> *blanks
041800981027     c                   movel(P)  Kannullata    V1Dfca
041900981028   x4c                   ELSE
042000981027     c                   clear                   v1dfca
042100981027     c     v1cfca        chain     fndfa01l                           31
042200981027     c  n31              movel     DFAdes        V1Dfca
042300981028    4c                   ENDIF
042400981027      *
042500981027     c                   movel     dcttad        V1Ctad
042600981027      * descrizione
042700981027     C                   MOVEL     'C'           t02mod
042800981027     C                   MOVEL     knsif         t02sif
042900981027     C                   MOVEL     'TAD'         t02cod
043000981027     C                   MOVEL     v1ctad        t02ke1
043100981027      *
043200981027     C                   CALL      'TIBS02R'
043300981027     C                   PARM                    KPJBA
043400981027     C                   PARM                    TIBS02DS
043500981027      *
043600981028    4C                   IF        t02err = *BLANKS
043700981027     C                   movel     T02uni        DTAD
043800981027     C                   movel     §TADdesc      v1dtad
043900981028   x4c                   else
044000981027     c                   clear                   v1dtad
044100981028    4c                   endif
044200981027      *
044300981027      *
044400981027     c                   z-add     DCTnev        V1Cnev
044500981027     c                   z-add     DCTaae        V1Caae
044600981028      * ricerca se bolla partenza o arrivi
044700981028     c                   EXSR      SUB_TPB
044800981028      *
044900990528     c* se tpb non è a blank scrivo il record altrimenti vuol dire che non si trova
045000990528     c* in archivio
045100990528     c                   if        wtpb <> *blanks
045200990528     c*
045300990528     c                   add       1             nrr1
045400981028     c                   move      wtpb          v1htpb
045500981027      *
045600981027     c                   write     fi24s01
045700990528      *
045800990528     c                   endif
045900981027      *
046000981028    3c                   endif
046100981027      *
046200000301    2c**********         endif
046300000301     c     leggi         tag
046400981028      *
046500981028     c     kfar          reade     fndcd03l                               30
046600981027      *
046700981027    1c                   enddo
046800981027     c*
046900981027     c                   endsr
047000981027      *****************************************************************
047100981027      *   ROUTINE GESTIONE PRIMO SUBFILE
047200981027      *****************************************************************
047300981028     C     SUB_GESF1     BEGSR
047400981027     c*
047500981029     c                   seton                                        2021
047600981028     c                   z-add     1             nrr1
047700981027     c* emetto il subfile
047800981028    1c                   dow       not *inkc
047900981029     c                   write     fi24z01
048000981027     c                   exfmt     fi24c01
048100981218     c                   setoff                                       9028
048200981029      * f18=Cambio P.O. in gestione
048300981029     c   ks              eval      *in05 = *on
048400981029     c   ks              iter
048500981029
048600981029      * 05 on --> controllo P.O. in gestione
048700981029     c     *in05         ifeq      *on
048800981029     C                   exsr      SUB_CFGS
048900981029     c   28              iter
049000981029     c                   eval      *in05 = *off
049100981029     c                   leave
049200981029     c                   end
049300981027     c* KC = fine lavoro
049400981028    2c                   if        not *inkc
049500981030     c                   setoff                                       9091
049600981028     c* LETTURA SUBFILE
049700981030    3c                   dow       *in91 = *off
049800981218     c                   setoff                                       818328
049900981030     c                   readc     fi24s01                                91
050000981028     c* valorizzo dsnunca
050100981028     c                   eval      aac = v1haac
050200981028     c                   eval      fil = v1cfil
050300981028     c                   eval      nca = v1cnca
050400981028     c*
050500981028     c                   select
050600981028     c* fine file oppure è stato dato un F3 in qualche pgm chiamato
050700981030     c                   WHEN      *in91 or *in90
050800981028     c*
050900981028     c* OPZIONE  "2" Manutenzione C.A.
051000981218
051100990225     c                   WHEN      v1csce = '2'  and  v1hges = 'N'              non gestibile
051200981218     c                   movel     MSG(10)       V1Cmsg
051300981218     c                   seton                                        288390
051400990225
051500990225     c                   WHEN      v1csce = '2'  and  v1hchi = 'S'              non gestibile
051600990225     c                   movel     MSG(11)       V1Cmsg
051700990225     c                   seton                                        288390
051800981218
051900981218     c                   WHEN      v1csce = '2'  and  v1hges = 'S'
052000981028     c                   exsr      SUB_DS00                                     * car ds fidn00ds
052100981028     c                   movel     'M'           I00mod
052200981028     c                   movel     fidn00ds      kpjbu
052300981028     c                   CALL      'FIDN03R'
052400981028     c                   parm                    kpjba
052500981028     c                   movel     kpjbu         fidn00ds
052600981028     c                   eval      *in90 = (o00f03 = 'S')
052700981028     c* OPZIONE  "4" Note per stampa
052800981028     c                   WHEN      V1csce = '4'
052900981028     c                   exsr      SUB_STNOTE
053000981028     c                   eval      *in90 = (WF03 = 'S')
053100981028     c* OPZIONE  "5" Visualizza   C.A.
053200981028     c                   WHEN      v1csce = '5'
053300981030     c                   exsr      SUB_DS00                                     * car ds fidn00ds
053400981028     c                   movel     'I'           I00mod
053500981028     c                   movel     fidn00ds      kpjbu
053600981030     c                   CALL      'FIDN01R'
053700981028     c                   parm                    kpjba
053800981028     c                   movel     kpjbu         fidn00ds
053900981028     c                   eval      *in90 = (o00f03 = 'S')
054000981028     c* OPZIONE  "8" Note C.A.
054100981028     c                   WHEN      V1Csce = '8'
054200981028     c                   exsr      SUB_NOTE
054300981028     c                   eval      *in90 = (WF03 = 'S')
054400981028     c* OPZIONE  "G" Gestione Ritrovamento Colli
054500990122     c                   WHEN      V1Csce = 'G' and v1dfca <> kanalizz
054600981028     c                   exsr      SUB_GERC
054700981028    4c                   if        *inkf = *on                                  * ca analizzata
054800990122     c                   seton                                        8190      * reverse immage
054900981028     c                   movel     kanalizz      v1dfca
055000981030     c                   clear                   v1cfca
055100990225     c                   movel     'S'           v1hchi
055200981028    4c                   endif
055300981028     c*
055400981028     c                   ENDSL
055500981028     c* aggiorno il record subfile
055600981028     c                   clear                   v1csce
055700981028     c*
055800981030     c  n91              update    fi24s01
055900981028     c*
056000981028    3c  n90              enddo                                                  * lettura subfile
056100981028     c*
056200981028    2c                   endif                                                  * not *inkc
056300981028     c*
056400981028    1c                   enddo                                                  * ciclo
056500981028     c*
056600981028     c                   endsr
056700981028      **********************************************************************
056800981028      * GESTIONE RITROVAMENTO COLLO         2° VIDEATA
056900981028      **********************************************************************
057000981028     C     SUB_GERC      BEGSR
057100981028      *
057200981028     c* routine di caricamento seconda videata
057300981028     c                   exsr      SUB_CAF2
057400981028     c*
057500981028     c* routine di gestione seconda videata
057600981028     c                   exsr      SUB_GEF2
057700981028     c*
057800981028     c                   endsr
057900981028      **********************************************************************
058000981028      * CARICAMENTO VIDEATA GESTIONE COLLI RITROVATI     2° videata
058100981028      **********************************************************************
058200981028     C     SUB_CAF2      BEGSR
058300981028      *
058400981028      * DATI CA
058500981028     c                   eval      v2cfil = v1cfil                              * P.O. apertura CA
058600981028     c                   eval      v2cnca = v1cnca                              * Numero CA
058700981029     c                   movel     v1cdca        v2cdca                         * Data CA
058800981030     c                   move      v1haac        v2cdca
058900981028     c                   eval      v2ctad = v1ctad                              * tipo anomalia
059000981028     c                   movel     v1dtad        v2dtad                         * tipo anomalia des
059100981028     c                   eval      v2clnp = v1clnp                              * linea partenza
059200981028     c                   eval      v2cnrs = v1cnrs                              * numero serie
059300981028     c                   eval      v2cnsp = v1cnsp                              * numero spedizione
059400981028     c                   eval      v2cncn = v1hncn                              * num.colli dannegg.
059500990122     c                   eval      v2cfca = v1hfca                              * fase CA
059600990122     c                   movel     v1dfca        v2dfca                         * decodifica fase CA
059700990122     c                   eval      v2cgfc = v1hgfc                              * P.O. gestione CA
059800990122     c* decodifica p.o. gestione fase ca
059900990122     c     v2cgfc        chain     azorg                              30
060000990122     C  N30              if        orgde5 <> *BLANKS
060100990122     C                   movel     orgde5        v2dgfc
060200990122     C                   else
060300990122     c                   movel     orgdes        v2dgfc
060400990122     c                   endif
060500981028     c*
060600981028     c                   z-add     V1Haas        Kaas
060700981028     c                   z-add     V1Clnp        Klnp
060800981028     c                   z-add     V1Cnrs        Knrs
060900981028     c                   z-add     V1Cnsp        Knsp
061000981028     c* se è partenza vado in blp
061100981028     c                   if        v1htpb  = 'P'
061200981028     c     kspe          chain     fnblp000                           30
061300981028     c* se è arrivo vado in arb
061400981028     c                   else
061500981201     c     kspe          chain(n)  fnarb000                           30
061600981028     c                   endif
061700981028     c*
061800981029     c                   movel     arbaas        v2cdsp
061900981029     c                   move      arbmgs        v2cdsp
062000981029     c     *ymd          move      V2Cdsp        DATA_eur
062100981029     c     *eur          move      DATA_eur      v2cdsp                         * data spedizione
062200981028     c*
062300990528     c                   eval      v2clna=v1hlna                                * linea arrivo
062400981028     c* decodifica
062500981028     c     v2clna        chain     azorg                              30
062600981028     C  N30              if        orgde5 <> *BLANKS
062700981028     C                   movel     orgde5        v2dlna
062800981028     C                   else
062900981028     c                   movel     orgdes        v2dlna
063000981028     c                   endif
063100981230     c                   movel     arbnas        VTCnas
063200981201      * decodifico tipo bolla
063300981230     C                   if        v1hpor = 'F'
063400981230     C                   movel(P)  'FRANCO'      VTDcbo
063500981230     C                   else
063600981230     C                   movel(P)  'ASSEGNATO'   VTDcbo
063700981230     C                   endif
063800990309     c*
063900990309     c                   SELECT
064000990309     c*
064100990309    1c                   when      arbcca <> '9'
064200990309     c                   eval      W1hpor  = v1hpor
064300990309     c*
064400990309    2c                   when      v1hpor = 'F'
064500990309     c                   eval      W1hpor  = 'A'
064600990309     c*
064700990309    2c                   when      v1hpor   = 'A'
064800990309     c                   eval      W1hpor  = 'F'
064900990309     c*
065000990309    2c                   endsl
065100981201      *
065200981201      *  Se assegnato in partenza verifico il codice destinatario
065300991115     c*!*                clear                   DSBL4A
065400991115     c                   clear                   ar6ksc
065500990309     c                   IF        v1htpb  = 'P' and  w1hpor  = 'A'
065600991115     c*!*  Kbl4          CHAIN     FNBL4000                           30
065700991115     c*!*N30              movel     BL4NOT        DSBL4A
065800991116     c                   eval      trcar6 = '1'
065900991116     c     KAR6          CHAIN     FIAR6000                           30
066000981201     c                   ENDIF
066100981201      *
066200990309     c                   IF        w1hpor = 'A'
066300981028     c                   move      arbccm        VTCksm
066400991115     c*!*                if        §B4ksf <> *blanks
066500991115     c*!*                movel     §B4ksf        VTCksd
066600991115     c                   if        AR6ksc <> *zeros
066700991115     c                   movel     AR6ksc        VTCksd
066800981201     c                   else
066900981201     c                   movel     arbksc        VTCksd
067000981201     c                   endif
067100981028     c                   ELSE
067200981028     c                   move      arbksc        VTCksm
067300981028     c                   move      *zeros        VTCksd
067400981028     c                   ENDIF
067500981028
067600981028      * DATI BOLLA
067700981028     c* Mittente Destinatario
067800981028     c                   movel     arbrsm        VTCrsm
067900981028     c                   move      arbcam        VTCcam
068000981028     c                   movel     arblom        VTClom
068100981028     c                   movel     arbprm        VTCprm
068200981028     c                   movel     arbnzm        VTCnam
068300981028     c                   movel     arbrsd        VTCrsd
068400981028     c                   move      arbcad        VTCcad
068500981028     c                   movel     arblod        VTClod
068600981028     c                   movel     arbprd        VTCprd
068700981028     c                   movel     arbnzd        VTCnad
068800981028     c* colli peso volume
068900981028     c                   z-add     arbncl        VTCncl
069000981028     c                   z-add     arbpkf        VTCpkf
069100981028     c                   movel     arbfvf        VTCfvf
069200981028     c                   z-add     arbvlf        VTCvlf
069300981028      * DATI CHIUSURA COLLI / CA
069400981028     c                   clear                   v2ccca
069500981028     c                   clear                   v2ccco
069600981028     c                   clear                   v2ccch
069700981028     c                   clear                   v2cdch
069800981201      * se CA chiusa accendo l'11 proteggo i xcampi di chiusura in quanto la CA è già chiusa
069900981201      * ma do la possibilità di dare come analizzati i segnacolli ancora da ANALIZZARE
070000981201     C                   EVAL      *in11 = (v1hi11 = 'S')
070100981218      * se CA non in gestione da me proteggo i campi di chiusura
070200981218      * ma do la possibilità di dare come analizzati i segnacolli ancora da ANALIZZARE
070300981028      * DATI COLLI RITROVATI
070400981218     C  n11              EVAL      *in11 = (v1hges = 'N')
070500981218      *
070600981028     c                   exsr      SUB_CASF2
070700981028      *
070800981028     c                   endsr
070900981028      *****************************************************************
071000981028      *   ROUTINE SCRITTURA SUBFILE COLLI RITROVATI
071100981028      *****************************************************************
071200981028     C     SUB_CASF2     BEGSR
071300981028     c* pulisco subfile
071400981028     c                   setoff                                           51
071500981028     c                   write     fi24c02
071600981028     c                   z-add     *zeros        nrr2
071700981028     c                   seton                                            51
071800981028     c* preparo la chiave
071900981028     c                   z-add     V1haac        Kaac
072000981028     c                   z-add     V2Cfil        Kfil
072100981028     c                   z-add     V2Cnca        Knca
072200981028     c*
072300981028     c* leggo dettaglio colli della C.A. in gestione
072400981028     c* deve avere flag da analizzare uguale ad 'S'
072500981028     c* deve essere stato ritrovato dal P.O. di gestione o dalla sua £6
072600981028     c     kdcd3         setll     fndcd03l
072700981028     c     kdcd3         reade     fndcd03l                               31
072800981028     c*
072900981028    1c                   dow       *in31 = *off
073000981028     c* controllo il P.O. che ha ritrovato il collo
073100000301    2c************       if        dcdfgs = v1cfgs
073200000301     c                   if        dcdfgs <> v1cfgs
073300000301     c* verifico la sua £6
073400000301     c     dcdfgs        lookup    l6                                     45
073500000301     c  n45              goto      leggi2
073600000301     c                   endif
073700000301     c*
073800981028     c*
073900981028     c                   clear                   v2ssel
074000981028     c                   eval      v2sfls = dcdfls
074100981028     c                   eval      v2snsc = dcdnsc
074200981028     c                   eval      v2sfgs = dcdfgs
074300050314      * campi utili per la chiave
074400050314     c                   eval      v2hlna = dcdlna
074500050314     c                   eval      v2hnrs = dcdnrs
074600050314
074700981028     c     *ymd          move      dcddfv        DATA_eur
074800981029     c     *eur          move      DATA_eur      V2sdfv
074900981028     c                   eval      v2snps = dcdnps
075000981028     c*
075100981028     c                   add       1             nrr2
075200981028      *
075300981028     c                   write     fi24s02
075400981028      *
075500000301    2c***************    endif
075600981028      *
075700000301     c     leggi2        tag
075800981028     c     kdcd3         reade     fndcd03l                               31
075900981028      *
076000981028    1c                   enddo
076100981028     c*
076200981028     c                   endsr
076300981028      *****************************************************************
076400981028      *   ROUTINE GESTIONE VIDEATA COOLI RITROVATI
076500981028      *****************************************************************
076600981028     C     SUB_GEF2      BEGSR
076700981028     c*
076800981028    1c                   if        nrr2 > 0
076900981028     c                   seton                                        50
077000981028     c                   z-add     1             nrr1
077100981028    1c                   endif
077200981028     c                   clear                   prima
077300981028     c                   clear                   wnco
077400981028     c*
077500981028    1c                   do        *hival
077600981029     c*
077700981029     c                   write     fi24z02
077800981028     c* emissione
077900981028     c                   exfmt     fi24c02
078000981104     c                   setoff                                       40  42
078100981028     c                   setoff                                       434428
078200981028     c* controllo le funzioni
078300981028     c                   select
078400981028     c* F12 Videata precedente
078500981028     c                   WHEN      *INKL
078600981028     c                   leave
078700981028     c* F18 Gestione Note CA
078800981028     c                   WHEN      *INKS
078900981028     c                   exsr      SUB_NOTE
079000981028     c*
079100981028     c                   endsl
079200981028     c* controllo del formato
079300981028     c                   exsr      SUB_CTR02
079400981028     c* se non ci sono errori e  CMD 6 confermo i dati
079500981028    2c                   if        *in28 = *off and *inkf = *on
079600981028     C                   exsr      SUB_CONFE
079700981028     c                   leave
079800981028    2c                   endif
079900981028    1c                   enddo
080000981028     c*
080100981028     c                   ENDSR
080200981028      **********************************************************************
080300981028      * CONTROLLO SECONDA VIDEATA
080400981028      **********************************************************************
080500981028     C     SUB_CTR02     BEGSR
080600981028      *
080700981028     c* chiusura C.A.
080800981028    1c                   if        v2ccca = 'SI'
080900981028     c* chiusura colli
081000981028     c                             and v2ccco = 'SI'
081100981028     c* errore
081200981028     c                   movel     msg(1)        v2cmsg
081300981028     c                   seton                                        4028
081400981028     c                   goto      ectr2
081500981028    1c                   endif
081600981028      *
081700981028     c* chiusura colli
081800981028    1c                   if        v2ccco = 'SI'
081900981028     c* controllo se è stata inserita la causale chiusura
082000981028    2c                   if        v2ccch = *blank
082100981028     c* errore
082200981028     c                   movel     msg(2)        v2cmsg
082300981028     c                   seton                                        4228
082400981028     c                   goto      ectr2
082500981028     c*
082600981028   x2c                   else
082700981028     c* controllo se è stata fatta una richiesta di ricerca in tabella
082800981028     c     '?'           scan      v2ccch                                 32
082900981028      *
083000981028    3C                   IF        *IN32 = *ON
083100981028     c                   clear                   tibs02ds
083200981028     c                   movel     'R'           t02mod
083300981028     c                   movel     knsif         t02sif
083400981028     C                   movel     'CCH'         t02cod
083500981028      *
083600981028     C                   CALL      'TIBS02R'
083700981028     c                   parm                    KPJBA
083800981028     c                   parm                    tibs02ds
083900981028      *
084000981028    4c                   if        t02err =  *blanks
084100981028     C                   MOVEL     t02ke1        v2ccch
084200981030     c                   else
084300981030     c                   movel     t02msg        v2cmsg
084400981030     c                   seton                                        4228
084500981030     c                   goto      ectr2
084600981028    4c                   endif
084700981028      *
084800990302   x3C                   Endif
084900981028      *   Controlli di validità'
085000981028     c                   clear                   tibs02ds
085100981028     C                   MOVEL     'C'           t02mod
085200981028     C                   MOVEL     knsif         t02sif
085300981028     C                   MOVEL     'CCH'         t02cod
085400981028     C                   MOVEL(P)  v2ccch        t02ke1
085500981028      *
085600981028     C                   CALL      'TIBS02R'
085700981028     C                   PARM                    KPJBA
085800981028     C                   PARM                    tibs02ds
085900981028      *
086000981028    4C                   if        t02err <> *BLANKS
086100981028     c                   movel     msg(3)        v2cmsg
086200981028     c                   seton                                        4228
086300981028     c                   goto      ectr2
086400981028    4C                   endif
086500981028    3C                   endif
086600981028     c*
086700981028     c* controllo la data di chiusura colli
086800981028    2C                   if        v2cdch <> 0
086900981028     C                   move      v2cdch        g02dat
087000981028     C                   eval      g02err = *blanks
087100981028     C                   call      'XSRDA8'
087200981028     C                   parm                    wlbdat
087300981028    3C                   if        g02err = '1'
087400981028     C                   seton                                        4328
087500981028     C                   movel     msg(4)        v2cmsg
087600981028     c                   goto      ectr2
087700981028   x3C                   else
087800981028     C                   z-add     g02dat        V2cdch
087900981028     C                   move      g02inv        wdch
088000981028    3C                   endif
088100981028   x2C                   else
088200981028     C                   seton                                        2843
088300981028     C                   movel     msg(5)        v2cmsg
088400981028     c                   goto      ectr2
088500981028    2C                   ENDIF
088600981028      * controllo che la data fase sia maggiore della data apertura C.A.
088700981030     c                   move      V2Cdca        g02dat
088800981030     c                   clear                   g02err
088900981030     c                   call      'XSRDA8'
089000981030     c                   parm                    wlbdat
089100981030     c                   movel     g02inv        wdatac
089200981030     c*
089300981028    2c                   if        *in28 = *off and
089400981028     c                             wdatac > wdch
089500981028     C                   seton                                        2843
089600981028     C                   movel     msg(6)        v2cmsg
089700981028     c                   goto      ectr2
089800981028    2C                   Endif
089900981028      * controllo che la data fase sia minore o uguale ad oggi
090000981028    2c                   if        *in28 = *off and
090100981028     c                             dateu < wdch
090200981028     C                   seton                                        2843
090300981028     C                   movel     msg(7)        v2cmsg
090400981028     c                   goto      ectr2
090500981028    2C                   Endif
090600981028     c* controllo che sia stato selezionato almeno un collo ritrovato
090700981028     c                   exsr      SUB_COLLI
090800981028     c* controllo il numero dei colli selezionati sia > 0
090900981028    2c                   if        wnco = 0
091000981028     c* errore
091100981028     c                   movel     msg(8)        v2cmsg
091200981104     c                   seton                                        4428
091300981104     c     1             chain     fi24s02                            91
091400981104     c  n91              update    fi24s02
091500981028     c                   goto      ectr2
091600981028    2c                   endif
091700981028     c* controllo che il totale numero colli selezionati non sia = al TOT colli mancanti della
091800981028     c* CA .. perchè a questo punto conviene chiudere la CA
091900981104     c* il messaggio di errore è forzabile (E' meglio di no xchè quando richiamo FIDN03 non riesco
092000981104     c* a confermare la CA con colli a zero e non ho neppure la possibilità di tornare indietro)
092100981104     c* 4/11/98
092200981104    2c*********          if        prima = ' '  and                             *********
092300981104     c*       *                    wnco = v2cncn                                *       *
092400981104     c*       *          eval      prima = '1'                                  *       *
092500981104     c*********                                                                 *********
092600981104     c                   if        wnco = v2cncn
092700981028     c                   movel     msg(9)        v2cmsg
092800981104     c                   seton                                        4428
092900981028     c                   goto      ectr2
093000981028    2c                   endif
093100981028     c*
093200981028    1C                   endif
093300981028     c*
093400981028     c     ectr2         ENDSR
093500981028      *****************************************************************
093600981028      *   ROUTINE LETTURA COLLI SELEZIONATI PER CHIUSURA COLLI
093700981028      *****************************************************************
093800981028     C     SUB_COLLI     BEGSR
093900981028     c*
094000981028     c                   clear                   wrknco
094100981028     c                   z-add     1             nrr2
094200981028     c*
094300981028     c                   do        *hival
094400981028     c     nrr2          chain     fi24s02                            33
094500981028     c                   if        not *in33
094600981028     c* controllo se è stato selezionato
094700981028     c                   if        v2ssel = 'X'
094800981028     c                   add       1             wrknco
094900981028     c                   endif
095000981028     c                   add       1             nrr2
095100981028     c                   endif
095200981028     c  n33              enddo
095300981028     c*
095400981028     c* se il numero di colli calcolato è differente dal numero colli calcolato nella precedente
095500981028     c* lettura valorizzo il campo e pulisco "PRIMA" per controllo
095600981028     c                   if        wnco <> wrknco
095700981028     c                   eval      wnco = wrknco
095800981028     c                   eval      prima= ' '
095900981028     c                   endif
096000981028     c*
096100981028     c                   endsr
096200981028      **********************************************************************
096300981028      * CONFERMA CHIUSURA
096400981028      **********************************************************************
096500981028     C     SUB_CONFE     BEGSR
096600981028      * CHIUSURA
096700981028     C*           chiusura CA
096800981028     c                   if        v2ccca = 'SI'
096900981201     c* eseguo aggiornamenti nei file CA per la chiusura
097000981201     c                   EXSR      SUB_CHCA
097100990414     c                   else
097200990414     c* altrimenti se chiusura singolo collo oppure tolgo il flag del singolo collo
097300981028     c*           controllo e aggiorno il dettaglio colli
097400981028     c                   z-add     1             nrr2
097500981028     c*
097600981028     c                   do        *hival
097700981028     c     nrr2          chain     fi24s02                            33
097800981028     c                   if        not *in33
097900981028     c* aggancio il file dettaglio colli
098000981028     c     kdcd2         chain     fndcd02l                           34
098100981028     c* controllo se : è stato dato CMD 6 metto causale + data chiusura nel dettaglio collo
098200981028     c                   if        v2ssel = 'X'
098300990302     c* metto data chiusura e cauasale di chiusura solo  se valorizzati
098400990302     c                   if        wdch > 0
098500981028     c                   eval      dcddch = wdch
098600990302     c*
098700981028     c                   eval      dcdcch = v2ccch
098800981028     c                   endif
098900990525     c*
099000990524     c                   endif
099100990525     c* se ho in gestione la CA  sflaggo il record
099200990525     c*
099300990525     c                   if        not *in11
099400990525     c                   clear                   dcdftr
099500990525     c                   endif
099600990525     c*
099700981028     c                   clear                   dcdfar
099800981028     c* aggiorno il file dettaglio colli
099900981028     c  n34              update    fndcd2
100000981028     c*
100100981028     c                   add       1             nrr2
100200981028     c                   endif
100300981028     c  n33              enddo
100400990414     c*
100500990414     c                   endif
100600981028     c*
100700981028     c* se chiusura dei singoli colli richiamo il PGM di manutenzione  per sistemare
100800981028     c* il numero totale dei colli danneggiati
100900981028     c                   if        v2ccco = 'SI'
101000981028     c                   exsr      SUB_DS00
101100981028     c                   movel     'C'           I00mod
101200981028     c                   movel     fidn00ds      kpjbu
101300981028     c                   CALL      'FIDN03R'
101400981028     c                   parm                    kpjba
101500981028     c                   movel     kpjbu         fidn00ds
101600981028     c                   endif
101700981028     c
101800981028     c* aggiorno il flag di trasmissione della testata  solo nel casi in cui tolgo il flag di
101900981028     c* collo da analizzare senza chiudere niente
102000990524     c                   if        v2ccca = '  ' and v2ccco = ' ' and
102100990524     c                             not *in11
102200990524     c     kdct          chain     fndct01l                           34
102300990524     c                   clear                   dctft1
102400990524     c                   clear                   dctft2
102500990524     c  n34              update    fndct000
102600990524     c                   endif
102700981028     c*
102800981029     c* confermo le note scritte da questo PGM
102900990107     C*                  CLEAR                   fidn10ds
103000990107     C*                  MOVEL     'C'           I10FLM
103100990107     c*                  movel     'C'           i10tpd
103200990107     c*                  movel     'N'           i10trc
103300990107     c*                  movel(P)  dsnumca       i10nkt
103400990107     c*                  movel     v1cfca        i10nks
103500990107     c*                  movel(P)  fidn10ds      kpjbu
103600990107     C*                  CALL      'FIDN10R'                            99
103700990107     C*                  PARM                    kpjba
103800981029     c*
103900981029     c* cerco l'ultimo progressivo note della fase in corso
104000981029     c* scrittura nelle note dei dati relativi alla CA da mantenere
104100981029     c                   movel     v1cfca        dcsnks
104200981029     c                   eval      dcstrc='N'
104300981029     c                   eval      dcspno=0
104400981029     c*
104500981029     c* scrittura note preparazione dei campi strandard
104600981029     c                   eval      dcsatb=' '
104700981029     c                   eval      dcstpd='C'
104800981029     c                   eval      dcsnkt=dsnumca
104900981029     c                   eval      dcsstd=' '
105000981029     c                   eval      dcsdim=dateu
105100981029     c                   eval      dcshim=oramin
105200981029     c*
105300981029     c* cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
105400981029     c     keydcs        setgt     fndcs01l
105500981029     c     keydcsp       readpe    fndcs01l                               93
105600981029     c*
105700981029     c                   move      knmus         dcspru
105800981029     c                   movel     simfel        dcspos
105900981029     c                   clear                   dcsstn
106000981029     c                   clear                   dcsft1
106100981029     c                   clear                   dcsdt1
106200981029     c* scrivo 2 righe di note per ogni singolo collo ritrovato
106300981029     c                   z-add     1             nrr2
106400981029     c*
106500981029     c                   do        *hival
106600981029     c     nrr2          chain     fi24s02                            33
106700981029     c                   if        not *in33
106800981029     c* Preparo la prima riga di note
106900981029     c                   add       1             dcspno
107000981029     c                   clear                   dcsnot
107100981029     c* tolgo gli 0 non significativi dal numero segnacollo
107200981029     c                   movel     v2snsc        w007a
107300981029     c     '0'           check     w007a         x
107400981029     c                   eval      wAnsc = %subst(w007a:X)
107500981029     c                   movel     v2sfls        wafls
107600050314     c                   movel     v2hnrs        wanrs
107700981029     c                   movel     v2sdfv        wadfv
107800981029     c                   movel     wadfv         wagio
107900981029     c                   move      wadfv         wames
108000981029     c                   movel     v2sfgs        wafgs
108100981029     c                   movel     v2snps        wanps
108200981029     c                   eval      dcsnot = 'Collo '  + wafls + ' ' + wanrs +
108300981029     c                                      ' ' + wansc + ' ritrovato dal'
108400981029     c*
108500981029     c                   write     fndcs000
108600981029     c* scrivo la seconda riga
108700981029     c                   add       1             dcspno
108800981029     c                   clear                   dcsnot
108900981029     c*
109000981030     c                   eval      dcsnot = 'P.O. '  + wafgs + ' il giorno ' +
109100981029     c                                      wagio + '/' + wames + ' pistola ' +
109200981029     c                                      wanps
109300981029     c*
109400981029     c                   write     fndcs000
109500981029     c*
109600981029     c                   add       1             nrr2
109700981029     c                   endif
109800981029     c  n33              enddo
109900981029     c*
110000981028     c                   endsr
110100981201      *****************************************************************
110200981201      * AGGIORNO ARCHIVI   CHIUSURA C.A.
110300981201      *****************************************************************
110400981201     C     SUB_CHCA      BEGSR
110500981201      *
110600981201      * Imposto campi per trasmissione dati
110700981201     c     kdct          chain     fndct01l                           31
110800981201     c                   z-add     dateu         DCTdch
110900981201     c                   z-add     wchca         DCtfca
111000981201     c                   move      wcch          DCTcch
111100981222     c                   clear                   DCTft1
111200981222     c                   clear                   DCTft2
111300981201      *
111400981201      * Aggiorno testata C.A.
111500981201     c                   EXCEPT    UPDdct
111600981201      * Aggiorno Bolla solo per l'arrivo
111700981230     c*?*                if        v1htpb  = 'A'
111800981201     c* prima di togliere il blocco controllo se esistono delle altre CA su quella bolla
111900981230     c*?*  kspe          chain     fndct02l                           32
112000981201     c* controllo se CA attiva e non già chiusa
112100981230     c*?*                if        not *in32
112200981230     c*?*                eval      *in32 = (dctdch > 0) or (dctatb <> ' ' )
112300981230     c*?*                endif
112400981230     c*?*                if        *in32
112500981230     C*?*  kspe          chain     fnarb01l                           32
112600981230     c*?*                if        not *in32  and arbfbc = 'D'
112700981230     c*?*                clear                   arbfbc
112800981230     c*?*                except    UPDarb
112900981230     c*?*                endif
113000981230     c*?*                endif
113100981230     c*?*                endif
113200981201      *
113300981201      * Aggiorno l dettaglio C.A.
113400981201     c                   EXSR      UPDDCD
113500981201      *
113600981201      * Scrivo fasi C.A.
113700981201     c                   EXSR      WRTDCF
113800070403
113900070403      * filiale gestione con procedura telefonate GEO attiva e LNA della spedizione
114000070403      * in £6 e bolla non in distinta e non consegnata propongo lo sblocco della spedizione
114100070403     c                   if        wfilgeo = *on and chisono = 'A' and
114200070403     c                             arbndc = *zeros and arbdcm = *zeros
114300070403     c                   clear                   dslr02
114400070403     c                   eval      dr2aas = dctaas
114500070403     c                   eval      dr2lnp = dctlnp
114600070403     c                   eval      dr2nrs = dctnrs
114700070403     c                   eval      dr2nsp = dctnsp
114800070403     c                   eval      dr2fgs = v1cfgs
114900070403     c                   eval      dr2ric = '1'
115000070403     c                   eval      kpjbu = dslr02
115100070403     c                   call      'FNLR39R'
115200070403     c                   parm                    kpjba
115300070403     c                   eval      dslr02 = kpjbu
115400070403     c                   endif
115500981201      *
115600981201     c                   ENDSR
115700981201      *****************************************************************
115800981201      * AGGIORNO FNDCD Dettaglio C.A.
115900981201      *****************************************************************
116000981201     C     UPDDCD        BEGSR
116100981201     c*
116200981201     c     kdct          setll     fndcd01l
116300981201     c     kdct          reade     fndcd01l                               31
116400981201     c*
116500981201      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
116600981201     c                   dow       *in31 = *off
116700981201     c                   eval      DCddch = dctdch
116800981201     c                   eval      DCdcch = dctcch
116900981222     c                   eval      DCdftr = dctft1
117000990414     c* pulizia del flag di collo da analizzare
117100990414     c                   clear                   dcdfar
117200981201     c                   update    fndcd1
117300981201     c*
117400981201     c     kdct          reade     fndcd01l                               31
117500981201     c*
117600981201     c                   enddo
117700981201     c*
117800981201     c                   endsr
117900981201      *****************************************************************
118000981201      * AGGIORNO FNDCF FASI C.A.
118100981201      *****************************************************************
118200981201     C     WRTDCF        BEGSR
118300981201      *
118400981201     C                   z-add     DCTaaC        DCFaac
118500981201     C                   z-add     DCTfil        DCFfil
118600981201     C                   z-add     DCTnca        DCFnca
118700981201     c                   z-add     dctfca        DCFfca
118800981201     c                   z-add     dateu         DCFdfc
118900990414     c                   movel     wora          DCFhfc
119000981201     c                   z-add     V1Cfgs        DCFfev
119100981201     c                   movel     dctptr        DCFptr
119200981201     c                   movel     knmus         DCFpru
119300981201      *
119400981201     c                   WRITE     FNDCF000
119500981201      *
119600981201     c                   ENDSR
119700981028      **********************************************************************
119800981028      * VISUALIZZO NOTE PER STAMPA
119900981028      **********************************************************************
120000981028     C     SUB_stnote    BEGSR
120100981028      *
120200981028     c*
120300981028     c*
120400981028     C                   clear                   FIDN10DS
120500981028     c     *dmy          move      V1Cdca        DATA_eur
120600981028     c     *eur          move      DATA_eur      i10dta
120700981028     c                   movel     'D'           i10trc
120800981028     c                   movel     'T'           i10std
120900981028     c                   movel     'C'           i10tpd
121000981028      *
121100981028     c                   movel     'V'           i10flm
121200981028     c                   movel     dsnumca       i10nkt
121300981028      *
121400981028     c                   movel(P)  fidn10ds      kpjbu
121500981028     C                   CALL      'FIDN10R'
121600981028     C                   PARM                    kpjba
121700981028      *
121800981028     c                   movel     *blanks       WF03
121900981028     C                   movel     KPJBU         fidn10ds
122000981028     c                   if        o10f03    <> *BLANKS
122100981028     C                   movel     'S'           WF03
122200981028     c                   endif
122300981028      *
122400981028     C                   ENDSR
122500981022      *****************************************************************
122600981022      *   ROUTINE SCRITTURA NOTE
122700981022      *****************************************************************
122800981022     C     SUB_NOTE      BEGSR
122900981022     c* valorizzo la DS di gestione note/descrizioni e chiamo il PGM FIDN10R
123000981028     C                   CLEAR                   fidn10ds
123100981030     c     *dmy          move      V1Cdca        DATA_eur
123200981030     c     *eur          move      DATA_eur      i10dta
123300981022     c                   movel     'N'           i10trc
123400981022     c                   movel     'C'           i10tpd
123500981027     c                   movel     v1cfca        i10nks
123600981022     c                   movel     dsnumca       i10nkt
123700981028     c* se sono in gestione note dal subfile le scrivo subito nel file fndcs
123800981028     c                   if        v1csce <> 'G'
123900981028     c                   movel     'V'           i10flm
124000981028     c                   else
124100981028     c* se sono in gestione videata collo ritrovati scrivo al CMD 6
124200981030     c* se lascio 'M' in modo tale che le note vengano confermate quando viene confermata la
124300981030     c* gestione del ritrovamento colli rischio di perdere le note in quanto diversi programmi
124400981030     c* chiamati richiamano a loro volta il PGM fidn10.
124500981030     c* metto la 'V' di manutenzione note potendo così confermare tutte le volte che entro
124600981030     c*                  movel     'M'           i10flm
124700981030     c                   movel     'V'           i10flm
124800981028     c                   endif
124900981028     c*
125000981028     c                   movel(P)  fidn10ds      kpjbu
125100981022     C                   CALL      'FIDN10R'                            99
125200981022     C                   PARM                    kpjba
125300981022     c
125400981022     c                   endsr
125500981028      *****************************************************************
125600981028      *   CARICAMENTO DS FIDN00DS
125700981028      *****************************************************************
125800981028     C     SUB_DS00      BEGSR
125900981028     c*
126000981028     c                   clear                   fidn00ds
126100981028     c*
126200981028     c                   z-add     V1hfca        I00fca
126300981028     c                   z-add     V1Cfgs        I00fgs
126400981028     c                   z-add     V1Haac        I00aac
126500981028     c                   z-add     V1Cfil        I00fil
126600981028     c                   z-add     V1Cnca        I00nca
126700981028     c                   z-add     V1Haas        I00aas
126800981028     c                   z-add     V1Clnp        I00lnp
126900981028     c                   z-add     V1Cnrs        I00nrs
127000981028     c                   z-add     V1Cnsp        I00nsp
127100981028     c                   z-add     V1Hlna        I00lna
127200981028     c                   movel     V1Ctad        I00tad
127300981028     c                   movel     v1htpb        I00tpb
127400981028      *
127500981028     C                   endsr
127600981028      **********************************************************************
127700981028      * IMPOSTO TIPO BOLLA  P/A
127800981028      **********************************************************************
127900981028     C     SUB_TPB       BEGSR
128000981028      *
128100981028     c                   movel     *blanks       wtpb
128200981028      *
128300981028     c                   z-add     V1Haas        Kaas
128400981028     c                   z-add     V1Clnp        Klnp
128500981028     c                   z-add     V1Cnrs        Knrs
128600981028     c                   z-add     V1Cnsp        Knsp
128700981028     c     kspe          chain     fnblp000                           30
128800981028      *
128900981028     c                   if        *IN30 = *OFF
129000981028     c                   movel     'P'           wtpb
129100981028     C                   else
129200981028      *
129300981028     c     kspe          chain     fnarb000                           30
129400981028     c  N30              movel     'A'           wtpb
129500981028     c                   endif
129600981028      *
129700981028     C                   ENDSR
129800981030      **********************************************************************
129900981030      * EMISSIONE VIDEATA RICHIESTA P.O. GESTIONE
130000981030      **********************************************************************
130100981030     C     SUB_FIRST     BEGSR
130200981030      *
130300981030     c                   do        *hival
130400981030     c*
130500981030     c                   exfmt     fi24t01
130600981030     c                   setoff                                       3928
130700981030     c* KC = fine
130800981030     c                   if        not *inkc
130900981030     c* ks = cambio p.o. gestione
131000981030     c   ks              eval      *in05 = *on
131100981030     c   ks              iter
131200981030     c*
131300981030      * 05 on --> controllo P.O. in gestione
131400981030     c     *in05         ifeq      *on
131500981030     C                   exsr      SUB_CFGS
131600981030     c   28              iter
131700981030     c                   eval      *in05 = *off
131800981030     c                   leave
131900981030     c                   end
132000981030     c* se è stato dato solo enter vuol dire che deve proseguire
132100981030     c                   leave
132200981201     c                   else
132300981201     c                   leave
132400981030     c                   end
132500981030    3c                   enddo
132600981030     c                   endsr
132700981028      **********************************************************************
132800981028      * CONTROLLO IL P.O. IN IN GESTIONE
132900981028      **********************************************************************
133000981029     C     SUB_CFGS      BEGSR
133100981028      *
133200981029     c                   setoff                                       3928
133300981029     c                   clear                   v1dfgs
133400981029     C                   clear                   FNLV50DS
133500981029     C                   MOVEL     KNMUS         D50PRU
133600981029     C                   MOVEL     V1cfgs        D50FGS
133700981029     C                   CALL      'FNLV50R'
133800981029     C                   PARM                    FNLV50DS
133900981028      *
134000981029     C                   if        D50ERR <> *blanks
134100981029     C                   MOVEL     D50MSG        V1cMSG
134200981029     C                   SETON                                        3928
134300981029     C                   else
134400070403     c                   clear                   og148
134500981029     c     v1cfgs        CHAIN     azorg                              31
134600981029     c  n31              MOVEL     orgdes        v1dfgs
134700070403     c  n31              eval      og148 = orgde8
134800070403      * controllo se Filiale gestione è abilitata a telefonate GEO
134900070403     c                   eval      wfilgeo = (§oggeot = 'S')
135000981029     C                   endif
135100981028      *
135200981029     C  n28              EXSR      CARL6
135300981029     c                   endsr
135400981028      **************************************************************************
135500981028      * CARICO SCHIERA L6 DA TABELLA £6
135600981028      **************************************************************************
135700981028     C     carl6         BEGSR
135800981028      *
135900981028     C                   CLEAR                   L6
136000981028     C                   CLEAR                   TRUL06DS
136100981028     C                   MOVE      '£6'          D06COD
136200981028     C                   MOVEL     v1cfgs        D06key
136300981028     C                   MOVEL(P)  TRUL06DS      KPJBU
136400981028     C                   CALL      'TRUL06R'
136500981028     C                   PARM                    KPJBA
136600981028     C                   MOVEL     KPJBU         TRUL06DS
136700981028     C                   MOVEA     LIN           L6
136800981028      *
136900981028     c                   endsr
137000981022      *****************************************************************
137100981022      *   ROUTINE INIZIALE
137200981022      *****************************************************************
137300981022     C     *INZSR        BEGSR
137400981022      *
137500981022     C     *ENTRY        PLIST
137600981022     C                   PARM                    KPJBA
137700981022      *
137800981022     C                   Z-ADD     1             CODUT
137900981022     C                   CALL      'X§PARUT'
138000981022     C                   PARM                    UT§DSE0F
138100981022     C                   MOVEL     RAGUT         RSUT
138200981022     C                   MOVEL     REC80         CNCR80
138300981029      * titolo videata e opzioni
138400981029     c                   movel     tit           vtctit
138500981029     c                   movel     dopz          v1dse1
138600981022      *
138700981027     C                   TIME                    W0140
138800981022     C                   MOVE      W0140         WDTGIO
138900981022     C                   MOVEL     W0140         WORA
139000981029     C                   MOVEL     Wora          oramin
139100981022      * UDATE IN AAAAMMGG
139200981022     C                   Z-ADD     WDTGIO        G02DAT
139300981022     C                   MOVEL     *BLANK        G02ERR
139400981022     C                   CALL      'XSRDA8'
139500981022     C                   PARM                    WLBDAT
139600981022     C                   MOVEL     G02INV        DATEU
139700981028
139800981028      * IMPOSTO IL P.O. IN GESTIONE
139900020502     c     simtpp        ifeq      '2'
140000020502     c     simtpp        oreq      *blanks
140100020502     c                   movel     simpou        v1cfgs
140200020502     C                   eval      *in06 = *on
140300020502     c                   else
140400020502     c                   movel     simfel        v1cfgs
140500020502     c                   endif
140600020502     C*                  IF        rem = 'REM'  AND  remfil > *ZEROS
140700020502     C*                  movel     REMFIL        V1CFGS
140800020502     C*                  eval      *in06 = *on
140900020502   X2C*                  ELSE
141000020502     C*                  movel     SIMFEL        V1CFGS
141100020502    2C*                  ENDIF
141200981029     c* decofico se non sono la sede
141300981029     c                   if        v1cfgs > 0
141400070403     c                   clear                   og148
141500981029     C     V1CFGS        CHAIN     AZORG                              31
141600981029     C  N31              MOVEL     orgDES        V1DFGS
141700070403     c  n31              eval      og148 = orgde8
141800070403      * controllo se Filiale gestione è abilitata a telefonate GEO
141900070403     c                   eval      wfilgeo = (§oggeot = 'S')
142000981029     C                   exsr      CARL6
142100981029    2C                   ENDIF
142200981027      *
142300981027     C* ACCESSO   TABEL
142400981027     C     KTAB          KLIST
142500981027     C                   KFLD                    CODUT
142600981027     C                   KFLD                    §COD
142700981027     C                   KFLD                    §KEY
142800981027      * accesso fndcd03l
142900981027     c     Kdcd3         KLIST
143000981027     C                   KFLD                    kfar
143100981028     C                   KFLD                    kaac
143200981028     C                   KFLD                    kfil
143300981028     C                   KFLD                    knca
143400981028      * accesso fndcd02l
143500981028     c     Kdcd2         KLIST
143600981028     c                   kfld                    v2sfls
143700050314     c                   kfld                    v2hlna
143800050314     c                   kfld                    v2hnrs
143900981028     c                   kfld                    v2snsc
144000981028     C                   KFLD                    kaac
144100981028     C                   KFLD                    kfil
144200981028     C                   KFLD                    knca
144300981027      * accesso fndct01l
144400981027     C     Kdct          klist
144500981027     C                   KFLD                    kaac
144600981027     C                   KFLD                    kfil
144700981027     C                   KFLD                    knca
144800981027      * accesso archivio bolle
144900991116     c     kspe          klist
145000991116     c                   kfld                    kaas
145100991116     c                   kfld                    klnp
145200991116     c                   kfld                    knrs
145300991116     c                   kfld                    knsp
145400991116     c*
145500991116     c     kar6          klist
145600981028     c                   kfld                    kaas
145700981028     c                   kfld                    klnp
145800981028     c                   kfld                    knrs
145900991116     c                   kfld                    knsp
146000991116     c                   kfld                    trcar6
146100981029     c*
146200991115     c*!*  kbl4          klist
146300991115     c*!*                kfld                    kaas
146400991115     c*!*                kfld                    klnp
146500991115     c*!*                kfld                    knrs
146600991115     c*!*                kfld                    knsp
146700991115     c*!*                kfld                    ktrc
146800991115     c*!*
146900981029     c     keydcs        KLIST
147000981029     c                   KFLD                    dcstpd
147100981029     c                   KFLD                    dcsnkt
147200981029     c                   KFLD                    dcsstd
147300981029     c                   KFLD                    dcsdim
147400981029     c                   KFLD                    dcshim
147500981029     c                   KFLD                    dcsnks
147600981029     c                   KFLD                    dcstrc
147700981029     c                   KFLD                    prgnot
147800981029     c                   eval      prgnot= 999
147900981029     c* di fndcs01l - parziale
148000981029     c*
148100981029     c     keydcsp       KLIST
148200981029     c                   KFLD                    dcstpd
148300981029     c                   KFLD                    dcsnkt
148400981029     c                   KFLD                    dcsstd
148500981029     c                   KFLD                    dcsdim
148600981029     c                   KFLD                    dcshim
148700981029     c                   KFLD                    dcsnks
148800981029     c                   KFLD                    dcstrc
148900981022      *
149000981022     C                   ENDSR
149100981201      *---------------------------------------------------------------------------------------------
149200981201     oFNDCT000  E            UPDDCT
149300981201     O                       DCTdch
149400981201     O                       DCTcch
149500981201     O                       DCTfca
149600981222     O                       DCTft1
149700981222     O                       DCTft2
149800990202     o*/*FNARB000  E            UPDARB
149900990202     O*/*                       ARBFbc
150000981022      *****************************************************************
150100981028**
150200981028Non si possono selezionare entrambe le chiusure                                1
150300981028Inserire la causale chiusura                                                   2
150400981028Causale Chiusura inesistente                                                   3
150500981028Data Chiusura errata                                                           4
150600981028Inserire Data Chiusura                                                         5
150700981028Data Chiusura deve essere maggiore della data di apertura                      6
150800981028Data Chiusura non deve essere maggiore della data del giorno                   7
150900981028Selezionare i segnacolli da chiudere                                           8
151000981104Il tot. segnacolli Selez. è = al tot. colli mancanti CA (Chiudera la C.A.)     9
151100981218C.A. non manutenzionabile: in gestione ad atro P.O.                           10
151200990225C.A. non manutenzionabile perchè chiusa                                       11

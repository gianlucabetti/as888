000100120724     /*PRM dbgview(*source)
000200120724     /*CMD ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000300120724     /*CMD        ovrscope(*calllvl)
000400120724     /*CMD ovrdbf file(TNTMD01L) tofile(GAITRAGRPS/TNTMD01L) +
000500120724     /*CMD        ovrscope(*calllvl)
000600120724     /*END dltovr file(TITAS30C TNTMD01L) lvl(*)
000700120724     /*END
000800990510      *------------------------------------------------------------------------*
000900990510      *                    MODIFICA TIPO GESTIONE C.A.                         *
001000990428      *------------------------------------------------------------------------*
001100990428      *  1)  da Pratica a Transattiva. Sede e Filiale                          *
001200990428      *  2)  da Transattiva a Pratica. Solo Sede                               *
001300990428      *  3)  beneficiario. Solo Sede                                           *
001400990504      *  4)  beneficiario alternativo, rimborso a carico, interlocutore ed     *
001500990504      *      importo concordato. Se abilitati a una delle precedenti variazioni*
001600980506      *------------------------------------------------------------------------*
001700990426
001800980803      ****************************************************************
001900980803      *  FASI C.A. FISSATE A PROGRAMMA  (ricercare con 0 davanti)
002000980803      ****************************************************************
002100990504      * 100 = Conferma C.A.                                 WFCON
002200990504      * 110 = Fittizia in Filiale                           WFFFF
002300990430      * 225 = Fittizia in SEDE per pagamento L.T.           WFFFS
002400990430      * 230 = Contabilizzazione L.T.                        WFCLT
002500990428      * 660 = Invio documenti a Consuldanni                 WFIDC
002600990428      ****************************************************************
002700990426
002800990426      ****************************************************************
002900990426      *  RIEPILOGO INDICATORI
003000990426      ***************************************************************
003100990427      * 01 - VISUALIZZA numero evento
003200990427      * 02 - VISUALIZZA numero pratica assicurativa
003300990428      * 03 - ABILITO F8 Manutenzione anagrafica clienti
003400990427      * 04 -
003500990427      * 05 - SPROTEGGO  "P.O. IN GESTIONE"
003600990427      * 06 - DISABILITO F18
003700990427      * 07 - NON VISUALIZZO "P.O. IN GESTIONE". Abbinare a IN06
003800990427      ***
003900990427      * 28 - ERRORE GENERICO DSPF
004000990427      * 29 - RIEMISSIONE FORMATO VIDEO
004100990427      * 30 - COMODO
004200990427      * 31 - COMODO
004300990427      * 32 - COMODO
004400990426      ***  Video 1
004500990427      * 35 - ERRORE inserimento P.O in gestione
004600990427      * 36 - ERRORE numero C.A.
004700990427      * 37 -
004800990427      * 38 -
004900990427      * 39 -
005000990428      ***  Video 2
005100990428      * 40 - ERRORE tipo gestione
005200990430      * 41 - ERRORE beneficiario
005300080307      * 42 - ERRORE rimborso a carico       NON LO FACCIO PIU'
005400990430      * 43 - ERRORE beneficiario alternativo
005500990430      * 44 - ERRORE interlocutore
005600990430      * 45 - ERRORE importo concordato
005700990430      * 46 -
005800990430      * 47 -
005900990427      * 48 -
006000990427      * 49 -
006100990427      * 50 -
006200990427      ***  Video per inserimento beneficiario alternativo
006300990427      * 60 - ERRORE ragione sociale
006400990427      * 61 - ERRORE indirizzo
006500990427      * 62 - ERRORE C.A.P.
006600990427      * 63 - ERRORE località
006700990427      * 64 - ERRORE provincia
006800990427      * 65 - ERRORE nazione
006900990427      * 66 - ERRORE fax
007000990427      * 67 - ERRORE telefono
007100990427      * 68 - ERRORE Partita IVA
007200990430      * 69 -
007300990426      ***************************************************************
007400990426
007500990426     H DECEDIT('0,') DATEDIT(*DMY.)
007600990426      * ---------------------------------------------------------
007700990426     fFIDN32D   CF   E             WORKSTN
007800990426     fTABEL00F  IF   E           K DISK
007900990426     fFNDCT01L  UF A E           K DISK
008000990426     fFNDCS01L  UF A E           K DISK
008100990426     fFNDET01L  IF   E           K DISK
008200051117     fFNDPT01L  IF   E           K DISK
008300990426     fFNDCF01L  IF A E           k DISK
008400990426     fFNDCL01L  UF A E           k DISK
008500990426     fFNDKA01L  UF A E           k DISK
008600040802     fFNDCK01L  IF   E           k DISK
008700990426     fAZORG01L  IF   E           K DISK
008800990426      * filiale
008900991116     fFIAR601L  IF   E           K DISK     usropn
009000990426     fFNBLP01L  IF   E           K DISK     usropn
009100990426     fFNARB01L  IF   E           K DISK     usropn
009200990426      * sede
009300990809     fTITAS30C  IF   E           K DISK     usropn
009400990426     fTNTMD01L  IF   E           K DISK     usropn
009500990426      * ---------------------------------------------------------
009600050720     D MSG             S             78    DIM(41) CTDATA PERRCD(1)             MSG VIDEO
009700990426     D L6              S              3  0 DIM(30)                              P.O.GESTITI
009800990503      * ---------------------------------------------------------
009900990503     D FIDN00DS      E DS
010000990503     D FIDN05DS      E DS
010100990426     D FIDN10DS      E DS
010200990426     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
010300990426     D TIBS02DS      E DS
010400990428     d TIBS69DS      E DS
010500990426     d DS_cnaco      E DS                  extname(CNACO00F)
010600990426     d DS_cnind      E DS                  extname(CNIND00F)
010700990426     d DS_cnclp      E DS                  extname(CNCLP00F)
010800990426     d DS_fncls      E DS                  extname(FNCLS00F)
010900990426     d TISI95DS      E DS
011000990426     d TRUL42DS      E DS
011100990426     d FNLV13DS      E DS
011200990426     d FNLV14DS      E DS
011300990430     d FNLV50DS      E DS
011400990426     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
011500990426     d  tcu                  398    697    DIM(50)
011600990426     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
011700990426     D TRUL06DS      E DS
011800990426     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
011900990426     D CNCR80        E DS
012000990426     D KPJBA         E DS
012100020502     D* rem                    1      3
012200020502     D* remfil                 4      6
012300990426     D DTAD          E DS
012400010910     D DGEDDN        E DS                  inz
012500990426     D DSTD          E DS
012600990426     D DSTB          E DS
012700990426     D DSCV          E DS
012800990426     D DDCT01        E DS
012900020201     D DDCF01        E DS
013000010905     D YEURCODS      E DS                  inz
013100010905     D   YECTAR      E                     inz('H')
013200011017     D XDECDS        E DS                  inz
013300011017     D   OCDESI      E                     inz(*OFF)
013400990426
013500990428     d                 ds
013600990428     d  DS_numca               1     14
013700990426     d  dctaac                 1      4  0
013800990426     d  dctfil                 5      7  0
013900990426     d  dctnca                 8     14  0
014000990426
014100990426     d                 ds
014200990426     d  DS_dcdkey              1     14
014300990426     d  dcdaac                 1      4  0
014400990426     d  dcdfil                 5      7  0
014500990426     d  dcdnca                 8     14  0
014600990426
014700990426     D WLBDAT          DS                  INZ
014800990426     D  G02DAT                 1      8  0
014900990426     D  G02INV                 9     16  0
015000990426     D  G02ERR                17     17
015100990426     D  G02TGI                18     22  0
015200990426
015300990426     d TCUDS           DS
015400990426     d  f1                     1      1
015500990426     d  f3                     3      3
015600990426     d  f2                     2      2
015700990426     d  f4                     4      4
015800990426     d  f56                    5      6
015900990426
016000990426     D                SDS
016100990426     D  VTCPGM                 1     10
016200990426
016300990510     D TIT             C                   CONST('*   MODIFICA TIPO GESTIONE C.-
016400990510     D                                     A.  *')
016500020130      *
016600020205     d Comm_Fra_I      c                   const('Ins. Automatico Pratica Franc-
016700020205     d                                     higia ')
016800020205     d Comm_Fra_E      c                   const('Elim. Automatico Pratica Fran-
016900020205     d                                     chigia')
017000990426
017100990427     d Datasped        S               D   DATFMT(*eur)
017200990503     D Dataca          S                   LIKE(I10dta)
017300990503     D Wlnp            S                   LIKE(ARBlnp)
017400020130     D Wfra            S                   LIKE(§DCTpfra)
017500990504     D £V2Cipr         S                   LIKE(DCLipr)
017600990504     D £DCLipr         S                   LIKE(DCLipr)
017700990427     D wV2cptr         S                   LIKE(V2Cptr)
017800990430     D WV2Dnoc         S                   LIKE(V2Dnoc)
017900990430     D WV2Dksc         S                   LIKE(V2Dksc)
018000990601     D W1V2Dksc        S                   LIKE(V2Dksc)
018100990430     D Wv2cksc         s                   LIKE(V2Cksc)
018200990504     d WV2cfpr         s                   LIKE(V2Cfpr)
018300990426     D Wtrc            S                   LIKE(DKAtrc)
018400990426     D Kcod            S                   LIKE(TBLcod)
018500990426     D Kkey            S                   LIKE(TBLkey)
018600990426     D §key            S                   LIKE(TBLkey)
018700990426     D ktpd            s                   LIKE(DCStpd)
018800990426     D knkt            s                   LIKE(DCSnkt)
018900990426     D kstd            s                   LIKE(DCSstd)
019000990426     D wstd            s                   LIKE(DCSstd)
019100990426     D knks            s                   LIKE(DCSnks)
019200990426     D ktrc            s                   LIKE(DCStrc)
019300990426     D kdim            s                   LIKE(DCSdim)
019400990426     D khim            s                   LIKE(DCShim)
019500990428     D wsvcap          s                   LIKE(W1Ccap)
019600990428     D wsvloc          s                   LIKE(W1Cloc)
019700990428     D wsvprv          s                   LIKE(W1Cprv)
019800990426     D wmsg            s                   LIKE(O13msg)
019900990426     D wfzv            s                   LIKE(E13fzv)
020000990426     D wfz1            s                   LIKE(E13fz1)
020100990426     D wfz2            s                   LIKE(E13fz2)
020200990426     D wfz3            s                   LIKE(E13fz3)
020300990426     D paxccc          s                   LIKE(ACOkcc)
020400990426     D paxdmt          s                   LIKE(ACOrag)
020500990426     D wvlt            s                   LIKE(DCTvpv)
020600990427     d Wfpr            s                   LIKE(V2Cfpr)
020700990426     D W§dctport       S                   LIKE(§DCTport)
020800991116     D trcar6          S                   LIKE(ar6trc)
020900040802
021000040802     d keyaas          s                   like(dctaas)
021100040802     d keylnp          s                   like(dctlnp)
021200040802     d keynrs          s                   like(dctnrs)
021300040802     d keynsp          s                   like(dctnsp)
021400990426
021500990504     d WFcon           s                   LIKE(DCTfca)   inz(100)
021600990430     d WFfff           s                   LIKE(DCTfca)   inz(110)
021700990430     d WFffs           s                   LIKE(DCTfca)   inz(225)
021800990430     d WFclt           s                   LIKE(DCTfca)   inz(230)
021900990428     d WFidc           s                   LIKE(DCTfca)   inz(660)
022000990426
022100990503     d Wtpb            s              1
022200990503     d Wrivalsa        s              1
022300990430     d WF18frz         s              1
022400990430     d Wmsg18frz       s              1
022500990430     d Wmsg24frz       s              1
022600990430     d Wmsg30frz       s              1
022700990503     d Wmsg36frz       s              1
022800990426     D wfndcl          s              1
022900990430     D wfndcs          s              1
023000990504     d Wbenalt         s              1
023100990426     D §des            S             30
023200990426     D wtibs69r        s              1
023300990426     D werr            s              1
023400990426     D PAXdut          s             30
023500990426     D PAXdit          s              3
023600990426     D PAXflr          s             90
023700990504     D PAXsta          s              1  0
023800010910     D w002a           s              2
023900990426     D w0020           s              2  0
024000990426     D w0030           s              3  0
024100990426     D w0033           s              3  3
024200990426     D w0040           s              4  0
024300990426     D w0133           s             13  3
024400990426     D w0140           s             14  0
024500990426     D XX              s              2  0
024600990426     D wdtgio          s              8  0
024700990430     D wdatasp         s              8  0
024800011126     D wdataca         s               D   datfmt(*eur)
024900990426     D dateu           s              8  0
025000051117     D wmand           s              1
025100990426      * ---------------------------------------------------------
025200990426     IFNBLP000
025300990426     I              BLPATB                      ARBATB
025400990426     I              BLPAAS                      ARBAAS
025500990426     I              BLPLNP                      ARBLNP
025600990426     I              BLPNRS                      ARBNRS
025700990426     I              BLPNSP                      ARBNSP
025800990426     I              BLPCBO                      ARBCBO
025900990426     I              BLPMGS                      ARBMGS
026000990426     I              BLPFNS                      ARBFNS
026100990426     I              BLPSCL                      ARBSCL
026200990426     I              BLPFBR                      ARBFBR
026300990426     I              BLPFST                      WDDT
026400990426     I              BLPFDN                      ARBFDN
026500990426     I              BLPLNA                      ARBLNA
026600990426     I              BLPZNC                      ARBZNC
026700990426     I              BLPKSC                      ARBKSC
026800990426     I              BLPRSM                      ARBRSM
026900990426     I              BLPINM                      ARBINM
027000990426     I              BLPCAM                      ARBCAM
027100990426     I              BLPLOM                      ARBLOM
027200990426     I              BLPNZM                      ARBNZM
027300990426     I              BLPPRM                      ARBPRM
027400990426     I              BLPFAP                      ARBFAP
027500990426     I              BLPCPI                      ARBCPI
027600990426     I              BLPRSD                      ARBRSD
027700990426     I              BLPIND                      ARBIND
027800990426     I              BLPCAD                      ARBCAD
027900990426     I              BLPLOD                      ARBLOD
028000990426     I              BLPNZD                      ARBNZD
028100990426     I              BLPPRD                      ARBPRD
028200990426     I              BLPFIN                      ARBFIN
028300990426     I              BLPGC1                      ARBGC1
028400990426     I              BLPGC2                      ARBGC2
028500990426     I              BLPCTR                      ARBCTR
028600990426     I              BLPCTS                      ARBCTS
028700990426     I              BLPFTM                      ARBFTM
028800990426     I              BLPTSP                      ARBTSP
028900990426     I              BLPNAS                      ARBNAS
029000990426     I              BLPNCL                      ARBNCL
029100990426     I              BLPNCP                      ARBNCP
029200990426     I              BLPPKC                      ARBPKC
029300990426     I              BLPPKB                      ARBPKB
029400990426     I              BLPPKF                      ARBPKF
029500990426     I              BLPNCR                      ARBNCR
029600990426     I              BLPVLC                      ARBVLC
029700990426     I              BLPFVB                      ARBFVB
029800990426     I              BLPVLB                      ARBVLB
029900990426     I              BLPFVF                      ARBFVF
030000990426     I              BLPVLF                      ARBVLF
030100990426     I              BLPQFT                      ARBQFT
030200990426     I              BLPVMD                      ARBVMD
030300990426     I              BLPVAD                      ARBVAD
030400990426     I              BLPIAS                      ARBIAS
030500990426     I              BLPVAS                      ARBVAS
030600990426     I              BLPCCM                      ARBCCM
030700990426     I              BLPRMN                      ARBRMN
030800990426     I              BLPRMA                      ARBRMA
030900990426     I              BLPRMO                      ARBRMO
031000990426     I              BLPFLS                      ARBFLS
031100990426     I              BLPNCD                      ARBNCD
031200990426     I              BLPNCA                      ARBNCA
031300990426     I              BLPXCO                      ARBXCO
031400990426     I              BLPCTM                      ARBCTM
031500990426     I              BLPFFD                      ARBFFD
031600990426     I              BLPTCR                      ARBTCR
031700990426     I              BLPDCR                      ARBDCR
031800990426     I              BLPHCR                      ARBHCR
031900990426     I              BLPDTI                      ARBDTI
032000990426     I              BLPHTI                      ARBHTI
032100990426     I              BLPDCE                      ARBDCE
032200990426     I              BLPNPI                      ARBNPI
032300990426     I              BLPNCI                      ARBNCI
032400990426     I              BLPNRC                      ARBNRC
032500990426     I              BLPDBR                      ARBDBR
032600990426     I              BLPNFV                      ARBNFV
032700990426     I              BLPDPC                      ARBDPC
032800990426     I              BLPDUC                      ARBDUC
032900990426     I              BLPFLP                      ARBFLP
033000990426     I              BLPDET                      ARBDET
033100990426     I              BLPDUT                      ARBDUT
033200990426     I              BLPDAM                      ARBDAM
033300990426     I              BLPNDC                      ARBNDC
033400990426     I              BLPDDC                      ARBDDC
033500990426     I              BLPDCP                      ARBDCP
033600990426     I              BLPDCM                      ARBDCM
033700990426     I              BLPHMC                      ARBHMC
033800990426     I              BLPTC1                      ARBTC1
033900990426     I              BLPTC2                      ARBTC2
034000990426     I              BLPCCA                      ARBCCA
034100990426     I              BLPTMC                      ARBTMC
034200990426     I              BLPGGA                      ARBGGA
034300990426     I              BLPGMA                      ARBGMA
034400990426     I              BLPGVA                      ARBGVA
034500990426     I              BLPFLE                      ARBFLE
034600990426     I              BLPTFP                      ARBTFP
034700990426     I              BLPFEA                      ARBFEA
034800990426     I              BLPTFA                      ARBTFA
034900990426     I              BLPFT1                      ARBFT1
035000990426     I              BLPDT1                      ARBDT1
035100990426     I              BLPFT2                      ARBFT2
035200990426     I              BLPDT2                      ARBDT2
035300990426     I              BLPFT3                      ARBFT3
035400990426     I              BLPDT3                      ARBDT3
035500990426
035600990809     ITITAS000
035700990426     I              TASAAS                      ARBAAS
035800990426     I              TASLNP                      ARBLNP
035900990426     I              TASNRS                      ARBNRS
036000990426     I              TASNSP                      ARBNSP
036100990426     I              TASMGS                      ARBMGS
036200990426     I              TASNCL                      ARBNCL
036300990426     I              TASKSC                      ARBKSC
036400990426     I              TASCCM                      ARBCCM
036500990809     I              TASPKf                      ARBPKF
036600990426     I              TASFVF                      ARBFVF
036700990426     I              TASVLF                      ARBVLF
036800990426     I              TASLNA                      ARBLNA
036900990426     I              TASIAS                      ARBIAS
037000990426     I              TASVAS                      ARBVAS
037100990426     I              TASTSP                      ARBTSP
037200990426     I              TASCTR                      ARBCTR
037300990426     I              TASTFP                      ARBTFP
037400990426     I              TASTFA                      ARBTFA
037500990426     I              TASLL1                      WDDT
037600990426     I              TASCCA                      ARBCCA
037700990426
037800990809     ITITAS010
037900990426     I              TASAAS                      ARBAAS
038000990426     I              TASLNP                      ARBLNP
038100990426     I              TASNRS                      ARBNRS
038200990426     I              TASNSP                      ARBNSP
038300990426     I              TASMGS                      ARBMGS
038400990426     I              TASNCL                      ARBNCL
038500990426     I              TASKSC                      ARBKSC
038600990426     I              TASCCM                      ARBCCM
038700990809     I              TASPKf                      ARBPKF
038800990426     I              TASFVF                      ARBFVF
038900990426     I              TASVLF                      ARBVLF
039000990426     I              TASLNA                      ARBLNA
039100990426     I              TASIAS                      ARBIAS
039200990426     I              TASVAS                      ARBVAS
039300990426     I              TASTSP                      ARBTSP
039400990426     I              TASCTR                      ARBCTR
039500990426     I              TASTFP                      ARBTFP
039600990426     I              TASTFA                      ARBTFA
039700990426     I              TASLL1                      WDDT
039800990426     I              TASCCA                      ARBCCA
039900990426
040000990809     ITITASP00
040100990426     I              TASAAS                      ARBAAS
040200990426     I              TASLNP                      ARBLNP
040300990426     I              TASNRS                      ARBNRS
040400990426     I              TASNSP                      ARBNSP
040500990426     I              TASMGS                      ARBMGS
040600990426     I              TASNCL                      ARBNCL
040700990426     I              TASKSC                      ARBKSC
040800990426     I              TASCCM                      ARBCCM
040900990809     I              TASPKf                      ARBPKF
041000990426     I              TASFVF                      ARBFVF
041100990426     I              TASVLF                      ARBVLF
041200990426     I              TASLNA                      ARBLNA
041300990426     I              TASIAS                      ARBIAS
041400990426     I              TASVAS                      ARBVAS
041500990426     I              TASTSP                      ARBTSP
041600990426     I              TASCTR                      ARBCTR
041700990426     I              TASTFP                      ARBTFP
041800990426     I              TASTFA                      ARBTFA
041900990426     I              TASLL1                      WDDT
042000990426     I              TASCCA                      ARBCCA
042100990426
042200990426     ITNTMD000
042300990426     I              TMDAAS                      ARBAAS
042400990426     I              TMDLNP                      ARBLNP
042500990426     I              TMDNRS                      ARBNRS
042600990426     I              TMDNSP                      ARBNSP
042700990426     I              TMDRSM                      ARBRSM
042800990426     I              TMDINM                      ARBINM
042900990426     I              TMDCAM                      ARBCAM
043000990426     I              TMDLOM                      ARBLOM
043100990426     I              TMDPRM                      ARBPRM
043200990426     I              TMDNZM                      ARBNZM
043300990426     I              TMDRSD                      ARBRSD
043400990426     I              TMDIND                      ARBIND
043500990426     I              TMDCAD                      ARBCAD
043600990426     I              TMDLOD                      ARBLOD
043700990426     I              TMDPRD                      ARBPRD
043800990426     I              TMDNZD                      ARBNZD
043900990426      * ---------------------------------------------------------
044000990427
044100990427      *            G E S T I O N E   V I D E O  1
044200990427     C     EMD01         TAG
044300990427      *
044400990427     C                   WRITE     FI32T01
044500990427     C                   WRITE     FI32Z01
044600990427     C                   EXFMT     FI32D01
044700990427      *
044800990427     C                   CLEAR                   V1CMSG
044900990427     c                   eval      *in29 = *off
045000990427     c                   eval      *in28 = *off
045100990428     c     35            DO        39            XX
045200990427     c                   eval      *IN(XX) = *OFF
045300990427     c                   ENDDO
045400990427
045500990428      * F3  = FINE
045600990427     C                   IF        *INKC = *ON
045700990427     C                   goto      FINE
045800990427     C                   ENDIF
045900990427
046000990428      * F18 = Cambio P.O. in gestione
046100990427     c                   IF        *INKS = *ON
046200990427     c                   eval      *in05 = *on
046300990427     c                   goto      emd01
046400990427     C                   ENDIF
046500990427
046600990428      *     05 on --> controllo P.O. in gestione
046700990427     c                   IF        *in05 = *on
046800990427     C                   exsr      ctrfgs
046900990427     c   28              goto      emd01
047000990427     c                   eval      *in05 = *off
047100990427     C                   ENDIF
047200990427
047300990427     c                   EXSR      CTRD01
047400990427
047500990427     C                   IF        *IN28 = *ON  or  *IN29 = *ON
047600990427     C                   goto      EMD01
047700990427     C                   ENDIF
047800990426
047900990428      * Riempio testata  video 2
048000990426     C                   EXSR      RIETES
048100990426      *
048200990428      * Riempio dettaglio video 2
048300990426     C                   EXSR      RIEDET
048400990426
048500990427      *            G E S T I O N E   V I D E O  2
048600990427     C     EMD02         TAG
048700990426      *
048800990426      * Attivo F8
048900990504     c                   eval      *IN03 = (Wbenalt = 'S')
049000990426      *
049100990427     C                   WRITE     FI32T02
049200990427     C                   WRITE     FI32Z02
049300990427     C                   EXFMT     FI32D02
049400990426      *
049500990427     C                   CLEAR                   V2CMSG
049600990426     c                   eval      *in29 = *off
049700990426     c                   eval      *in28 = *off
049800990430     c     40            DO        50            XX
049900990428     c                   eval      *IN(XX) = *OFF
050000990428     c                   ENDDO
050100990426
050200990426      * F3 = FINE
050300990426     C                   IF        *INKC = *ON
050400990426     C                   goto      FINE
050500990426     C                   ENDIF
050600990426
050700990426      * F12 = RITORNO
050800990426     C                   IF        *INKL = *ON
050900990503     c                   clear                   fi32d02
051000990428     C                   goto      EMD01
051100990426     C                   ENDIF
051200990426
051300990503      * F7 = VISUALIZZA C.A.
051400990503     C                   IF        *INKG = *ON
051500990503     c                   exsr      SUB_F7
051600990503     C                   goto      EMD02
051700990503     c                   ENDIF
051800990503
051900990426      * F18 = NOTE
052000990428     C                   IF        *INKS = *ON
052100990430     c                   eval      WF18frz = 'S'
052200990426     c                   exsr      SUB_F18
052300990428     C                   goto      EMD02
052400990426     c                   ENDIF
052500990426
052600990428      * Controlli video 2
052700990428     C                   EXSR      CTRD02
052800990426      *
052900990426     C                   if        *IN28 = *ON  or  *IN29 = *ON
053000990428     C                   goto      EMD02
053100990426     C                   endif
053200990426
053300990426      * F08 = Manutenzione anagrafica Beneficiario alternativo
053400990428     c                   IF        *INKH = *ON
053500990426     c                   movel     'C'           Wtrc
053600990428     c                   exsr      mancli
053700990428     C                   goto      emd02
053800990428     C                   ENDIF
053900990426
054000990503     C                   IF        *INKF = *OFF
054100990430     c                   goto      EMD02
054200990430     C                   ENDIF
054300990430      * Aggiorno files
054400990430     c                   exsr      AGGIORNA
054500011126      * Obbligo l'inserimento delle note
054600011126     c                   IF        WF18frz = *blanks
054700011126     c                   exsr      SUB_F18
054800011126     C                   ENDIF
054900990430
055000990426      *------------------------
055100990426     C     FINE          TAG
055200990426      *------------------------
055300990426
055400990426      * Chiudo pgm di servizo
055500990426     c                   clear                   FIDN10DS
055600990426     c                   movel     'C'           i10tla
055700990426     c                   movel     FIDN10DS      kpjbu
055800990426     c                   call      'FIDN10R'
055900990426     c                   parm                    kpjba
056000990426      *
056100990426     C                   IF        Wtibs69r  = 'S'
056200990428     C                   clear                   TIBS69DS
056300990426     C                   EVAL      I69TLA  = 'C'
056400990426     C                   CALL      'TIBS69R'
056500990426     C                   PARM                    TIBS69DS
056600990426     C                   ENDIF
056700990426      *
056800990426     c                   clear                   tibs02ds
056900990426     c                   movel     'C'           t02tla
057000990426     c                   call      'TIBS02R'
057100990426     c                   parm                    kpjba
057200990426     c                   parm                    tibs02ds
057300990426      *
057400990426     C                   EVAL      *INLR = *ON
057500990427      **********************************************************************
057600990427      * CONTROLLO IL P.O. IN IN GESTIONE
057700990427      **********************************************************************
057800990427     C     CTRFGS        BEGSR
057900990427      *
058000990427     C                   eval      *in35 = *off
058100990427     c                   clear                   v1dfgs
058200990427     C                   clear                   FNLV50DS
058300990427     C                   MOVEL     KNMUS         D50PRU
058400990427     C                   MOVEL     V1cfgs        D50FGS
058500990427     C                   CALL      'FNLV50R'
058600990427     C                   PARM                    FNLV50DS
058700990427      *
058800990427     C                   IF        D50ERR <> *blanks
058900990427     C                   MOVEL     D50MSG        V1cMSG
059000990427     C                   eval      *in35 = *on
059100990427     C                   eval      *in28 = *on
059200990427     C                   ELSE
059300990427      *
059400990427     C                   EXSR      CARL6
059500990427     c     v1cfgs        CHAIN     azorg                              31
059600990427     c                   IF        *IN31 = *OFF
059700990427     C                   movel     orgDES        V1Dfgs
059800990428     C                   ENDIF
059900990427      *
060000990428     C                   ENDIF
060100990427      *
060200990427     c                   endsr
060300990427      **********************************************************************
060400990427      *  CONTROLLI VIDEATA 1
060500990427      **********************************************************************
060600990427     C     CTRD01        BEGSR
060700990427
060800990427      * SISTEMO ANNO DI DUE CIFRE
060900990427     c                   IF        V1Caac > *zeros  and  V1Caac < 100
061000990427     C                   if        V1Caac > 60
061100990427     C                   ADD       1900          V1Caac
061200990427     C                   else
061300990427     C                   ADD       2000          V1caac
061400990427     C                   endif
061500990427     c                   ENDIF
061600990427
061700990427      * CONTROLLO SE SELEZIONATI DATI VALIDI
061800990427     c                   IF        V1Cfil = *zeros  or  V1Cnca = *zeros
061900990427     c                                              or  V1Caac = *zeros
062000990427     c                   movel     msg(1)        V1Cmsg
062100990427     c                   eval      *in36 = *on
062200990427     c                   eval      *in28 = *on
062300990427     c                   ENDIF
062400990427     c   28              goto      ECTRD01
062500990427
062600990428      * CONTROLLO SE SELEZIONATA C.A. VALIDA
062700990430     c     kca           CHAIN     fndct000                           30
062800990427      *
062900990427     c                   SELECT
063000990427      *
063100990427     c                   WHEN      *IN30 = *ON
063200990427     c                   movel     msg(2)        V1Cmsg
063300990427      *
063400990427     c                   WHEN      DCTatb = 'A'
063500990427     c                   movel     msg(3)        V1Cmsg
063600990427      *
063700990427     c                   WHEN      DCTdch > *zeros
063800990427     c                   movel     msg(4)        V1Cmsg
063900990427      *
064000990427     c                   OTHER
064100990427     C                   if        DCTgfc > *zeros
064200990427     C     DCTgfc        LOOKUP    L6                                     31
064300990503     c  N31              movel     msg(5)        V1Cmsg
064400990427     C                   endif
064500990427      *
064600990427     c                   ENDSL
064700990427      *
064800990427     c                   IF        V1Cmsg <> *blanks
064900990427     c                   eval      *in36 = *on
065000990427     c                   eval      *in28 = *on
065100990427     c                   goto      ECTRD01
065200990427     c                   ENDIF
065300040802      *
065400040802      * se CA con legami e linea di arrivo delle CA presente nella l6 del P.O. gestione
065500040802      * recupero il numero spedizione da FNDCK
065600040802      *
065700040802     c                   eval      Keyaas = dctaas
065800040802     c                   eval      Keylnp = dctlnp
065900040802     c                   eval      Keynrs = dctnrs
066000040802     c                   eval      keynsp = dctnsp
066100040802      *
066200040802     c                   movel     dctflo        ddct01
066300040802     c                   If        §dctlega = 'S'
066400040802     C     DCTlna        LOOKUP    L6                                     31
066500040802     c                   If        *in31
066600040802     c     kca           chain     fndck01l
066700040802     c                   if        %found(fndck01l)
066800040802      * valorizzo la spedizione
066900040802     c                   eval      Keyaas = dckaas
067000040802     c                   eval      Keylnp = dcklnp
067100040802     c                   eval      Keynrs = dcknrs
067200040802     c                   eval      keynsp = dcknsp
067300040802     c                   endif
067400040802     c                   endif
067500040802     c                   endif
067600990427
067700990428      * AGGANCIO SPEDIZIONE
067800990427     c                   IF         SIMFEL = *zeros                             SEDE
067900990430     c                   eval       Wtpb = 'S'
068000990427     c     kbol01        chain(N)  tntmd000                           30
068100990809     c     kbol01        chain     TITAS30c                           30
068200990427      *  se tipo bolla non è valido cerco altro rcd
068300990427     C                   movel     'TB'          kCOD
068400990427     C                   movel(P)  TAStbl        kKEY
068500990427     C     ktab          chain(N)  TABEL                              31
068600990427     C                   movel     TBLUNI        DSTB
068700990427     c                   if        §TBrbl = 'R'
068800990809     c     kbol01        reade     TITAS30c                               30
068900990427     c                   endif
069000990427      *
069100990427     c                   ELSE
069200990427     c     kbol01        chain(N)  fnarb000                           30
069300990430     c                   If         *IN30 = *OFF
069400990430     c                   eval       Wtpb = 'A'
069500990430     c                   Else
069600990427     c     kbol01        chain(N)  fnblp000                           30
069700990430     c  N30              eval       Wtpb = 'P'
069800990427     c                   Endif
069900990427     c                   ENDIF
070000990427      *
070100990428     c                   IF          *IN30 = *off
070200990430     c                   z-add     arbmgs        WDATASP
070300990430     c                   movel     arbaas        WDATASP
070400990430     c     *iso          move      Wdatasp       datasped
070500990428     c                   ELSE
070600990428     c                   movel     msg(6)        V1Cmsg
070700990428     c                   eval      *in36 = *on
070800990428     c                   eval      *in28 = *on
070900990428     c                   goto      ECTRD01
071000990428     c                   ENDIF
071100990428
071200990428      * CONTROLLO SE C.A. ABILITATA ALLE MODIFICHE
071300990428     c                   SELECT
071400990504     c                   WHEN      DCTfca < WFCON
071500990504     c                   movel     msg(7)        V1Cmsg
071600990504     c                   WHEN      DCTfpr = 'P'  and  DCTfca >= WFIDC
071700990428     c                   movel     msg(15)       V1Cmsg
071800990428     c                   WHEN      DCTfpr = 'T'  and  DCTfca >= WFCLT
071900990428     c                   movel     msg(16)       V1Cmsg
072000990428     c                   WHEN      DCTfpr = 'T'  and  SIMFEL <> *zeros
072100990428     c                   movel     msg(17)       V1Cmsg
072200990428     c                   ENDSL
072300990428      *
072400990428     c                   IF        V1Cmsg <> *blanks
072500990428     c                   eval      *in36 = *on
072600990428     c                   eval      *in28 = *on
072700990428     c                   goto      ECTRD01
072800990428     c                   ENDIF
072900990427
073000990427     c     ECTRD01       ENDSR
073100990426      ****************************************************************
073200990427      *  CARICAMENTO DATI DI TESTATA
073300990426      ****************************************************************
073400990426     C     RIETES        BEGSR
073500990426
073600040802     c**""**             movel     dctflo        DDCT01
073700990426     c                   movel     §DCTlnpc      Wlnp
073800020130     c                   movel     §DCTpfra      Wfra
073900990427      *
074000990427     c                   z-add     V1Cfil        vtcfil
074100990427     c                   z-add     V1Cnca        vtcnca
074200990503     c                   z-add     DCTmgc        dataca
074300990503     c                   movel     DCTaac        dataca
074400011126     c     *ISO          movel     dataca        Wdataca
074500011126     c                   move      Wdataca       VTCdc1
074600990426
074700990426     c                   move      datasped      VTCdsp
074800040802      *
074900040802     C                   move      Keylnp        VTClnp
075000040802     C                   move      Keynrs        VTCnrs
075100040802     C                   move      Keynsp        VTCnsp
075200990426
075300990427     C                   move      DCTlna        VTClna
075400990427     C     DCTlna        chain(N)  AZORG                              31
075500990426     C  N31              If        ORGDE5 <> *BLANKS
075600990426     C                   movel     ORGDE5        VTDlna
075700990426     C                   Else
075800990426     C                   movel     ORGDES        VTDlna
075900990426     C                   Endif
076000990426
076100990426      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con quello C.A. tranne se la
076200990426      *  sped. è "Mamma di cambio porto", in questo caso inverto il porto C.A.
076300990426     c                   SELECT
076400110503     c                   WHEN      §DCTcca <> '9'
076500990426     c                   movel     §DCTport      W§DCTport
076600990426     c                   WHEN      §DCTport = 'A'
076700990426     c                   movel     'F'           W§DCTport
076800990426     c                   WHEN      §DCTport = 'F'
076900990426     c                   movel     'A'           W§DCTport
077000990426     c                   ENDSL
077100990426
077200990426      * Codici mittente/destinario
077300990426     c                   IF         w§DCTport = 'A'
077400990426     c                   move      arbccm        VTCksm
077500990427      *    Se assegnato in partenza verifico il codice destinatario
077600991116     c                   clear                   ar6ksc
077700990430     c                   IF        Wtpb = 'P'
077800991116     c                   eval      trcar6 = '1'
077900991116     c     Kar6          chain(N)  FIAR6000                           30
078000990427     c                   ENDIF
078100991116     c                   if        AR6ksc <> *zeros
078200991116     c                   movel     AR6ksc        VTCksd
078300990426     c                   else
078400990426     c                   movel     arbksc        VTCksd
078500990426     c                   endif
078600990427      *
078700990426     c                   ELSE
078800990426     c                   move      arbksc        VTCksm
078900990426     c                   clear                   VTCksd
079000990426     c                   ENDIF
079100990426
079200990426      * dati bolla
079300990426     c                   IF        §DCTport = 'A'
079400990426     c                   movel(P)  'ASSEGNATO'   VTDcbo
079500990426     c                   ELSE
079600990426     c                   movel(P)  'FRANCO'      VTDcbo
079700990426     c                   ENDIF
079800990427      *
079900990426     c                   movel     arbrsm        VTCrsm
080000990426     c                   move      arbcam        VTCcam
080100990426     c                   movel     arblom        VTClom
080200990426     c                   movel     arbprm        VTCprm
080300990426     c                   movel     arbnzm        VTCnam
080400990426     c                   movel     arbrsd        VTCrsd
080500990426     c                   move      arbcad        VTCcad
080600990426     c                   movel     arblod        VTClod
080700990426     c                   movel     arbprd        VTCprd
080800990426     c                   movel     arbnzd        VTCnad
080900990426     c                   z-add     arbncl        VTCncl
081000990426     c                   z-add     arbpkf        VTCpkf
081100990426     c                   movel     arbfvf        VTCfvf
081200990426     c                   z-add     arbvlf        VTCvlf
081300990427
081400990427      * IMPORTO DA ASSICURARE
081500990427      *      in sede solo se non impostato dalla fatturazione
081600011114     c                   if        ARBias <> *zeros and ARBvas = *blanks
081700011114     c                   movel     'ITL'         ARBvas
081800011114     c                   endif
081900990426     c                   SELECT
082000990426     c                   WHEN      SIMFEL <> *zeros
082100990427     C                   z-add     ARBias        VTCias
082200990427     C                   movel     ARBvas        VTCvas
082300990426     c                   WHEN      TASFCM <> 'F'
082400990427     C                   z-add     ARBias        VTCias
082500990427     C                   movel     ARBvas        VTCvas
082600990426     c                   ENDSL
082700990426
082800990426      * FASE C.A.
082900990427     C                   z-add     DCTfca        VTCfca
083000990426     c                   CLEAR                   FIDN05DS
083100990426     c                   movel     'C'           I05MOD
083200990426     c                   z-add     VTCfca        I05fca
083300990426     C                   CALL      'FIDN05R'
083400990426     c                   PARM                    KPJBA
083500990426     c                   PARM                    FIDN05DS
083600990426     c                   IF        O05err <> 'E'
083700990426     c                   movel     O05REC        DS_FNDFA
083800990426     c                   movel     DFAdes        VTDfca
083900990426     c                   ENDIF
084000990426
084100990427      * P.O. GESTIONE
084200990430     C                   z-add     DCTgfc        VTCgfc
084300990426     C     VTCgfc        chain(N)  AZORG                              31
084400990426     C  N31              if        ORGDE5 <> *BLANKS
084500990426     C                   movel     ORGDE5        VTDgfc
084600990426     C                   else
084700990426     C                   movel     ORGDES        VTDgfc
084800990426     C                   endif
084900990426
085000990426      * TIPO ANOMALIA
085100990427     c                   clear                   DTAD
085200990427      *
085300990427     c                   clear                   TIBS02DS
085400990427     C                   MOVEL     'C'           t02mod
085500990427     C                   MOVEL     knsif         t02sif
085600990427     C                   MOVEL     'TAD'         t02cod
085700990427     C                   MOVEL(P)  DCTtad        t02ke1
085800990427      *
085900990427     C                   CALL      'TIBS02R'
086000990427     C                   PARM                    KPJBA
086100990427     C                   PARM                    TIBS02DS
086200990427      *
086300990427     C                   if        t02err = *BLANKS
086400990427     C                   MOVEL     T02UNI        DTAD
086500990504     C                   MOVEL     DCTtad        VTCtad
086600990427     C                   MOVEL     §TADdesc      VTDtad
086700990427     C                   endif
086800990427
086900990427      * EVENTO
087000051117     c                   clear                   *in01
087100051117     c                   clear                   vtcnev
087200051117     c                   clear                   vtcaae
087300051117     c                   clear                   vtcdit
087400051117     c                   clear                   vtcprn
087500051117     c                   clear                   vtcpra
087600051117     c                   clear                   dtad
087700051117
087800990427     c                   IF        DCTnev > *zeros
087900990427     C                   z-add     DCTnev        VTCnev
088000990427     C                   z-add     DCTaae        VTCaae
088100990427     C     KDET          chain     FNDET000                           31
088200990427     C                   IF        *IN31 = *off  and  DETatb <> 'A'
088300990427     C                   z-add     DETmge        VTCdev
088400990427     C                   movel     DCTaae        VTCdev
088500050720     C                   Z-ADD     vtcdev        G02inv
088600050720     C                   movel     '3'           G02err
088700050720     C                   CALL      'XSRDA8'
088800050720     C                   PARM                    WLBDAT
088900050720     c                   z-add     g02dat        vtcdev
089000050720     c                   clear                   DTAD
089100050720      * tipo anomalia evento
089200050720     c                   clear                   TIBS02DS
089300050720     C                   MOVEL     'C'           t02mod
089400050720     C                   MOVEL     knsif         t02sif
089500050720     C                   MOVEL     'TAD'         t02cod
089600050720     C                   MOVEL(P)  DETtad        t02ke1
089700050720      *
089800050720     C                   CALL      'TIBS02R'
089900050720     C                   PARM                    KPJBA
090000050720     C                   PARM                    TIBS02DS
090100050720      *
090200050720     C                   if        t02err = *BLANKS
090300050720     C                   MOVEL     T02UNI        DTAD
090400050720     C                   endif
090500990427     C                   ENDIF
090600990427     C                   ENDIF
090700990427
090800990427      * NUMERO PRATICA ASSICURATIVA
090900990427     c                   IF        DCTprn > *zeros
091000990427     C                   movel     DCTdit        VTCdit
091100990427     C                   z-add     DCTprn        VTCprn
091200990427     C                   z-add     DCTpra        VTCpra
091300990427     C                   ENDIF
091400051117
091500051117      * VERIFICO SE HA MANDATO ASSICURATIVO
091600051117     c                   clear                   wmand
091700051117     c     kca           chain     Fndpt01l
091800060315     c                   if        %found(Fndpt01l) and dptatb <> 'A'
091900051117     c                   eval      Wmand = 'S'
092000051117     c                   endif
092100990426
092200990427      *          V I S I B I L I T A'   C A M P I
092300990427
092400990427     c                   eval      *IN01 = (VTCnev > *zeros)
092500990427     c                   eval      *IN02 = (VTCprn > *zeros)
092600990426
092700990426     C                   ENDSR
092800990426      ****************************************************************
092900990426      *  CARICAMENTO DATI DI DETTAGLIO
093000990426      ****************************************************************
093100990426     C     RIEDET        BEGSR
093200990427
093300990427     c                   movel     DCTfpr        V2Cfpr                         Tipo Gestione
093400990427     c                   movel     DCTptr        V2Cptr                         Beneficiario
093500990503
093600990503      * CONTROLLO SE HO RIVALSA
093700990503     c                   movel     'R'           Wtrc
093800990503     c     KDKA          CHAIN(N)  FNDKA000                           30
093900990909     c                   if        *in30 = *off  and  DKAatb = *blanks
094000990909     c                   eval      Wrivalsa = 'S'
094100990909     c                   endif
094200990427
094300990427      * BENEFICIARIO ALTERNATIVO
094400990427     c                   IF        DCTKSC > *ZEROS
094500990427     c                   z-add     dctksc        V2cksc
094600990427     c                   z-add     dctksc        WV2cksc
094700990427     c                   move      V2cksc        w0040
094800990427      *   se codificato decodifico da p.d.c.
094900990427     C                   IF        w0040 <> 8888
095000990427     c                   clear                   ACOrag
095100990428     C                   clear                   TIBS69DS
095200990428     c                   z-add     DCTksc        I69kac
095300990427     c                   exsr      CTR_anagra
095400990427     c  N30              movel     ACOrag        V2Dksc
095500990427     c                   ELSE
095600990427      *
095700990427      *   se non codificato decodifico da anagrafica clienti c.a.
095800990504     c                   movel     'S'           Wbenalt
095900990427     c                   clear                   FNDKA000
096000990427     c                   movel     'C'           Wtrc
096100990427     c     KDKA          CHAIN(N)  FNDKA000                           30
096200990427     c                   movel     dkarag        V2dksc
096300990427      *          imposto campi per video
096400990428     c                   movel     DKArag        W1Crag
096500990428     c                   movel     DKAvia        W1Cvia
096600990428     c                   movel     DKAcap        W1Ccap
096700990428     c                   movel     DKAloc        W1Cloc
096800990428     c                   movel     DKAprv        W1Cprv
096900990428     c                   movel     DKAnaz        W1Cnaz
097000990428     c                   eval      W1Civa = %subst(DKAiva:1:14)
097100990428     c                   eval      W1Cive = %subst(DKAiva:15:2)
097200990428     c                   movel     DKAtel        W1Ctel
097300990428     c                   movel     DKAfax        W1Cfax
097400990427     c                   ENDIF
097500990601      *
097600990601     c                   ELSE
097700990601     c                   clear                   W1Crag
097800990601     c                   clear                   W1Cvia
097900990601     c                   clear                   W1Ccap
098000990601     c                   clear                   W1Cloc
098100990601     c                   clear                   W1Cprv
098200990601     c                   clear                   W1Cnaz
098300990601     c                   clear                   W1Civa
098400990601     c                   clear                   W1Cive
098500990601     c                   clear                   W1Ctel
098600990601     c                   clear                   W1Cfax
098700990427     c                   ENDIF
098800990601     c                   movel     V2Dksc        WV2dksc
098900990427
099000990427      * INTERLOCUTORE
099100990427     c                   movel     'C'           ktpd
099200990427     c                   movel(P)  DS_numca      knkt
099300990427     c                   movel     'N'           kstd
099400990427     c                   clear                   knks
099500990427     c                   movel     'D'           ktrc
099600010912
099700990430     c     kdcs          setll     fndcs01l
099800011126     c     kdcs          reade     fndcs01l                               30
099900011126      *
100000990430     c     *in30         doweq     *off
100100990430     c     dcsatb        andne     *blanks
100200011126     c     kdcs          reade     fndcs01l                               30
100300990430     c                   enddo
100400990430      *
100500990430     c                   IF        *in30 = *off
100600990430     c                   eval      wfndcs = 'S'
100700990430     c                   movel     dcsnot        V2Dnoc
100800990430     c                   movel     dcsnot        WV2Dnoc
100900001010     c                   else
101000001010     c                   clear                   wfndcs
101100990430     c                   ENDIF
101200990427
101300990427      * IMPORTO MASSIMO RISARCIBILE
101400990427     c                   IF          DCTipv > *zeros
101500990427      *     Calcolo controvalore
101600010910     c                   IF        DCTvpv = §gedDBA
101700010906     c                   z-add     DCTipv        V2Cipv
101800010906     c                   ELSE
101900010906     c                   z-add     DCTipv        W0133
102000010912     C                   movel     DCTvpv        WVLT
102100010906     c                   exsr      srCMB
102200010906     c                   z-add     W0133         V2Cipv
102300010906     c                   ENDIF
102400010912     c                   movel     §gedDBA       V2Cvpv                           Valuta
102500990427      *
102600990427     c                   ENDIF
102700010911
102800010911      * Imposto la valuta per l'importo concordato
102900010911     C                   movel     §gedDBA       V2Cvlt
103000990426
103100990726      * IMPORTO CONCORDATO non considero se pratica
103200990426     c     kca           CHAIN     FNDCL000                           30
103300990430     c                   IF        *IN30 = *OFF
103400990504     c                   eval      Wfndcl = 'S'
103500990504      *
103600000721     c                   IF        DCLatb <> 'A'  and  DCTfpr <> 'P'
103700990504      *     Controllo la Valuta
103800990504     C                   movel     V2Cvlt        WVLT
103900990504     C                   z-add     V2Cipr        W0133
104000990504     C                   exsr      ctrvlt
104100990504
104200990504     C                   IF        *IN28 = *OFF  and  *IN29 = *OFF
104300010911      *     Calcolo il controvalore in divisa corrente
104400010910     c                   IF        V2Cvlt  = §gedDBA
104500010906     c                   z-add     DCLipr        V2Cipr
104600010906     c                   ELSE
104700010906     c                   z-add     DCLipr        W0133
104800010906     c                   exsr      srCMB
104900010906     c                   z-add     W0133         V2Cipr
105000010906     c                   ENDIF
105100010911     c                   movel     §gedDBA       V2Cvlt
105200010905     c                   z-add     V2Cipr        £DCLipr
105300990427     c                   ENDIF
105400990504      *
105500990430     c                   ENDIF
105600001010     c                   else
105700001010     c                   clear                   wfndcl
105800010912     c                   clear                   fndcl000
105900990504     c                   ENDIF
106000990426
106100990426     C                   ENDSR
106200990503      ****************************************************************
106300990503      * F7 = VISUALIZZA CA
106400990503      ****************************************************************
106500990503     C     SUB_F7        BEGSR
106600990503      *
106700990503     C                   clear                   FIDN00DS
106800990503     c                   z-add     DCTaac        I00aac
106900990503     c                   z-add     DCTfil        I00fil
107000990503     c                   z-add     DCTnca        I00nca
107100040802      *
107200040802     c                   z-add     Keyaas        I00aas
107300040802     c                   z-add     Keylnp        I00lnp
107400040802     c                   z-add     Keynrs        I00nrs
107500040802     c                   z-add     Keynsp        I00nsp
107600040802      *
107700990503     c                   z-add     DCTfca        I00fca
107800990503     c                   z-add     DCTgfc        I00fgs
107900990503     c                   movel     Wtpb          I00tpb
108000990503      *
108100990503     C                   movel(P)  FIDN00DS      KPJBU
108200990503     c                   call      'FIDN01R'
108300990503     c                   parm                    kpjba
108400990503      *
108500990503     C                   ENDSR
108600990426      ****************************************************************
108700990426      * F18 = GESTIONE NOTE
108800990426      ****************************************************************
108900990426     C     SUB_F18       BEGSR
109000990426      *
109100990426     C                   clear                   FIDN10DS
109200990427     c                   z-add     dataca        i10dta
109300990426     c                   movel     'N'           i10trc
109400990426     c                   movel     'C'           i10tpd
109500990426     c                   movel     'V'           i10flm
109600990426     c                   movel     dctfca        i10nks
109700990426     c                   movel     DS_numca      i10nkt
109800990426      *
109900990426     c                   movel(P)  FIDN10DS      kpjbu
110000990426     C                   CALL      'FIDN10R'
110100990426     C                   PARM                    kpjba
110200990426      *
110300990426     c                   ENDSR
110400990428      **********************************************************************
110500990428      *  CONTROLLI VIDEATA 2
110600990428      **********************************************************************
110700990428     C     CTRD02        BEGSR
110800990428
110900990503      * Azzero test per messaggio forzato
111000990503     c                   IF        V2Cptr <> WV2cptr
111100990503     c                   clear                   Wmsg18frz
111200990503     c                   clear                   Wmsg24frz
111300990503     c                   clear                   Wmsg30frz
111400990503     c                   movel     V2Cptr        WV2Cptr
111500990503     c                   ENDIF
111600990503      *
111700990503     c                   IF        V2Cfpr <> WV2Cfpr
111800990503     c                   clear                   Wmsg36frz
111900990503     c                   movel     V2Cfpr        WV2Cfpr
112000990503     c                   ENDIF
112100990503
112200990503
112300990428      * TIPO GESTIONE
112400990909      *      in filiale posso passare da 'P' a 'T' solo se non ho evento
112500990429      *      modifica possibile in 'T' se non ho importo trattenuto o rivalsa
112600990428     c                   SELECT
112700990429      *
112800990428     c                   WHEN      V2Cfpr = *blanks
112900990428     c                   movel     msg(8)        V2cmsg
113000990429      *
113100990429     c                   WHEN       V2Cfpr = 'P'
113200051117      * se modifica del 21/11/2005 è attiva non si può mettere in LT una pratica con :
113300051117      *                            evento fortuito senza mandato e senza importo d'assicurare
113400051117      *                          o porto franco import
113500060316     c                   WHEN      V2Cfpr = 'T' and DCTfpr <> 'T'
113600051117     c                   If          VTCnev > *zeros and §tadevfo = 'S' and     evento fortuito
113700051117     c                              wmand = ' ' and arbias = 0                  no mandato no assicu
113800051117     c                   movel     msg(31)       V2cmsg
113900051117     c                   endif
114000051117     c                   if          §DCTtisp = 'I'  and                        -import
114100051117     c                                   §DCTport = 'F'                          porto franco
114200081022      * se utente sede posso mettere in LT un Porto Franco Import
114300081022     c                             and simfel <> *zeros
114400051117     c                   movel     msg(33)       V2cmsg
114500051117     c                   endif
114600051117
114700060317     c                   If        DCLipt > *zeros                              * importo trattenuto
114800990428     c                   movel     msg(9)        V2cmsg
114900990428     c                   Else
115000990428     c                   movel     'R'           Wtrc
115100990428     c     KDKA          CHAIN(N)  FNDKA000                           30
115200990909     c                   if        *in30 = *off  and  DKAatb = *blanks
115300990909     c                   movel     msg(10)       V2cmsg
115400990428     c                   Endif
115500990909     c                   Endif
115600990429      *
115700990428     c                   ENDSL
115800990428      *
115900990428     c                   IF        V2Cmsg <> *blanks
116000990428     c                   eval      *in28 = *on
116100990428     c                   eval      *in40 = *on
116200990428     c                   goto      ectrd02
116300990428     c                   ENDIF
116400990428
116500990428      * BENEFICIARIO
116600990428     c                   SELECT
116700990429      *
116800990428     c                   WHEN      V2Cptr = *blanks
116900990428     c                   movel     msg(11)       V2cmsg
117000990430      *
117100120724     c                   WHEN      V2Cptr <> DCTptr  and  DCLipt > *zero
117200120724     c                   movel     Msg(12)       V2Cmsg
117300990429      *
117400990428     c                   ENDSL
117500990428      *
117600990428     c                   IF        V2Cmsg <> *blanks
117700990428     c                   eval      *in28 = *on
117800990428     c                   eval      *in41 = *on
117900990428     c                   goto      ectrd02
118000990428     c                   ENDIF
118100990428
118200990428      * RIMBORSO A CARICO
118300990504      *      se tipo gestione modificato in 'T' forzo 'V'
118400990504      *      se tipo gestione modificato in 'P' forzo 'V' se ho importo trattenuto o rivalsa, negli
118500990504      *       altri casi ricalcolo
118600990428     c                   SELECT
118700990429      *
118800990503     c                   WHEN      V2Cfpr = 'T'
118900990503      *
119000990504     c                   WHEN      DCTfpr <> 'P'  and  Wmsg36frz = *blanks
119100990428     C                   clear                   TIBS69DS
119200990428     C                   clear                   CLSflo
119300990428     c                   z-add     ARBksc        I69kcs
119400990428     c                   exsr      CTR_anagra
119500990504     c                   movel     'S'           Wmsg36frz
119600080307     c****               movel     msg(36)       V2cmsg
119700990429      *
119800990428     c                   ENDSL
119900990503      *
120000990428     c                   IF        V2Cmsg <> *blanks
120100990428     c                   eval      *in28 = *on
120200990430     c                   eval      *in42 = *on
120300990428     c                   goto      ectrd02
120400990428     c                   ENDIF
120500990428
120600990428      * BENEFICIARIO ALTERNATIVO
120700990601      *      emettere un messaggio informativo se modificato il beneficiario
120800990601     c                   If        DCTksc = V2Cksc  and  V2Cksc > *zeros  and
120900990601     c                             V2Cptr <> DCTptr  and  Wmsg18frz = *blanks
121000990504     c                   eval      Wmsg18frz = 'S'
121100990504     c                   movel     msg(18)       V2cmsg
121200990430     c                   ELSE
121300990430      *
121400990430     c                   move      V2Cksc        w0040
121500990504     c                   clear                   Wbenalt
121600990430      *
121700990430     c                   SELECT
121800990430      *    se immessa ragione sociale e non il codice richiamo il pgm di ricerca alfabetica
121900990430     c                   WHEN      V2Cksc = *zeros  and  V2Dksc <> *blanks
122000990430     c                   exsr      ricalf
122100990430     c                   eval      *in29 = *on
122200990430      *    Se codice 9999 segnalazione di errore
122300990430     c                   WHEN      w0040 = 9999
122400990430     c                   movel     msg(19)       V2cmsg
122500990430      *    se codice 8888 ed è inserito l'importo trattenuto segnalazione di errore
122600990430     c                   WHEN      w0040 = 8888  and  DCLipt > *zeros
122700990430     c                   movel     msg(20)       V2cmsg
122800990430      *    se codice 8888 controllo il P.O. ed emetto videata dati anagrafici se non inseriti
122900990430     c                   WHEN      w0040 = 8888
123000990430     c                   movel     V2Cksc        w0030
123100990430     C     W0030         chain(N)  AZORG                              31
123200990430     c                   If        *in31 = *on  or  ORGfva <> *blanks
123300990430     c                   movel     msg(21)       V2cmsg
123400990430     c                   Else
123500990504     c                   movel     'S'           Wbenalt
123600990601     c                   If        V2Dksc <> W1V2Dksc  or  W1Ccap = *blanks
123700990601     c                   movel     V2Dksc        W1Crag
123800990430     c                   exsr      mancli
123900990430     c                   eval      *in29 = *ON
124000990430     c                   endif
124100990512     c                   endif
124200990430      *    se codificato deve essere su P.d.C. e se ho imp. trattenuto deve essere di fatturazione
124300990430     c                   WHEN      V2Cksc <> *zeros
124400990430     c                   exsr      CTR_benalt
124500990430      *
124600990430     c                   ENDSL
124700990430     c                   ENDIF
124800990430      *
124900990430     c                   If        V2Cmsg <> *blanks
125000990430     c                   eval      *in28 = *on
125100990430     c                   eval      *in43 = *on
125200990430     C                   endif
125300990430      *
125400990430     c                   if        *in28 = *on  OR  *in29 = *on
125500990430     c                   goto      ectrd02
125600990430     C                   endif
125700990428
125800990430      * INTERLOCUTORE
125900990430      *      da verificare se modificato il beneficiario o il beneficiario alternativo
126000990504     c                   IF        Wmsg24frz = *blanks  and
126100990504     c                             V2Dnoc <> WV2Dnoc  and  V2Dnoc <> *blanks
126200990504     c                             and (V2Dksc <> WV2Dksc  or  DCTptr <> V2Cptr)
126300990430     c                   eval      Wmsg24frz = 'S'
126400990430     c                   movel     msg(24)       V2cmsg
126500990430     c                   eval      *in28 = *on
126600990430     c                   eval      *in44 = *on
126700990430     c                   goto      ectrd02
126800990430     C                   ENDIF
126900990430
127000990430      * IMPORTO CONCORDATO
127100990504      *      se tipo gestione è 'P' l'importo è da eliminare.
127200990503      *      se tipo gestione è 'T' ed è modificato il beneficiario o il beneficiario alternativo
127300990503      *       emetto un messaggio informativo
127400990609      *      se il tipo gestione è modificato in 'T' e non sono in sede controllo che l'importo
127500990609      *       richiesto rientri nei limiti tabellati
127600990430     c                   SELECT
127700990430      *
127800990504     c                   WHEN      V2Cfpr = 'P'  and  V2Cipr > *zeros
127900990430     c                   movel     msg(25)       V2Cmsg
128000990430      *
128100990430     c                   WHEN      V2Cfpr = 'P'
128200990430      *
128300990430      *
128400990504     c                   WHEN      Wmsg30frz = *blanks  and  V2Cipr = £DCLipr
128500990503     c                             and (V2Dksc <> WV2Dksc  or  DCTptr <> V2Cptr)
128600990430     c                   eval      Wmsg30frz = 'S'
128700990430     c                   movel     msg(30)       V2Cmsg
128800990503      *
128900990609     c                   WHEN      SIMFEL <> *zeros
129000080211      * tolgo il controllo dell'importo concordato
129100080211     c****               exsr      CTR_impcon
129200011009      *
129300011009      *
129400011009     c                   WHEN      V2Cipr <> *zeros
129500011009     c                   movel     V2Cvlt        WVLT
129600011009     c                   z-add     V2Cipr        W0133
129700011009     c                   exsr      ctrvlt
129800990430      *
129900990430     c                   ENDSL
130000990430      *
130100990430     c                   IF        V2Cmsg <> *blanks
130200990430     c                   eval      *in28 = *on
130300990430     c                   eval      *in45 = *on
130400990430     C                   ENDIF
130500990430     c                   IF        *in28 = *on  OR  *in29 = *on
130600990430     c                   goto      ectrd02
130700990430     C                   ENDIF
130800990430
130900990430     c     ECTRD02       ENDSR
131000990426      **********************************************************************
131100990430      *  CONTROLLO IMPORTO CONCORDATO
131200990426      **********************************************************************
131300990430     C     CTR_impcon    BEGSR
131400990428
131500990426      *     Controllo la Valuta
131600990430     C                   movel     V2Cvlt        WVLT
131700990426     C                   z-add     V2Cipr        W0133
131800990426     C                   exsr      ctrvlt
131900990426
132000990430     C                   IF        *IN28 = *OFF  and  *IN29 = *OFF
132100010906      *     Calcolo il controvalore in divisa corrente
132200010910     c                   IF        V2Cvlt  = §gedDBA
132300010906     c                   z-add     V2Cipr        £V2Cipr
132400010906     c                   ELSE
132500010906     c                   exsr      srCMB
132600010911     c                   z-add     W0133         £V2Cipr
132700010906     c                   ENDIF
132800990430      *
132900990609     c                   IF        £V2Cipr > §STDllt + (§STDllt * §STDptl)/ 100
133000080211     c                   movel     msg(32)       V2cmsg
133100990609     c                   ENDIF
133200990430      *
133300990426     c                   ENDIF
133400990426
133500990430     C                   ENDSR
133600990430      **************************************************************************
133700990430      * CONTROLLO IL BENEFICIARIO ALTERNATIVO
133800990430      **************************************************************************
133900990430     C     CTR_benalt    BEGSR
134000990430      *
134100990430      *    Aggancio anagrafico
134200990430     c                   clear                   ACOflg
134300990430     C                   clear                   TIBS69DS
134400990430     c                   z-add     V2Cksc        I69kac
134500990430     c                   exsr      CTR_anagra
134600990430      *    Aggancio particolarità se ho l'importo trattenuto
134700990430     c                   If          *in30 = *off  and  ACOflg = *blanks
134800990430     c                                             and  DCLipt > *zeros
134900990430     C                   clear                   TIBS69DS
135000990430     c                   z-add     V2Cksc        I69kcp
135100990430     c                   exsr      CTR_anagra
135200990430     c                   Endif
135300990430      *
135400990430     c                   SELECT
135500990430      *       non trovato o annullato errore
135600990430     c                   WHEN        *in30 = *on  or  ACOflg = '*'
135700990430     c                   movel     msg(22)       v2cmsg
135800990430      *
135900990430     c                   WHEN        DCLipt = *zeros
136000990430     c                   eval      *in29 = (V2Cksc <> WV2Cksc)
136100990430     c                   movel     ACOrag        V2Dksc
136200990503     c                   z-add     V2Cksc        WV2Cksc
136300990430      *
136400990430     c                   OTHER
136500990430      *    Controllo che sia il codice di fatturazione del beneficiario
136600990430     C                   clear                   TIBS69DS
136700990430     c                   If          V2Cptr = 'M'
136800990430     c                   z-add     VTCksm        I69kcp
136900990430     c                   Else
137000990430     c                   z-add     VTCksd        I69kcp
137100990430     c                   Endif
137200990430     c                   movel     I69kcp        W0040
137300990430      *
137400990430     c                   If        W0040 <> 9999  AND  W0040 <> 8888
137500990430     c                   exsr      CTR_anagra
137600990430     c                   If        *in30 = *on  or  ACOflg = '*'
137700990430     c                                          or  V2Cksc <> CLPscf
137800990430     c                   movel     msg(23)       v1cmsg
137900990430     c                   Else
138000990430     c                   eval      *in29 = (V2Cksc <> WV2Cksc)
138100990430     c                   movel     ACOrag        V2Dksc
138200990503     c                   z-add     V2Cksc        WV2Cksc
138300990430     c                   Endif
138400990430     c                   Endif
138500990430      *
138600990430     c                   ENDSL
138700990430      *
138800990430     C                   ENDSR
138900990426      *****************************************************************
139000990426      * CONTROLLO DIVISA
139100990426      *****************************************************************
139200990426     C     CTRVLT        BEGSR
139300990426      *
139400990426     C                   MOVEL     'CV'          KCOD
139500990426      * RICERCA
139600990426     C     '?'           SCAN      WVLT                                   30
139700990426      *
139800990426     C                   if        *IN30 = *ON
139900990426      *
140000990426     C                   CALL      'X§TABER'
140100990426     C                   PARM                    CODUT
140200990426     C                   PARM                    KCOD
140300990426     C                   PARM                    §KEY
140400990426     C                   PARM                    §DES
140500990426      *
140600990426     C                   MOVEL     §KEY          WVLT
140700990426     C                   eval      *in29 = *ON
140800990426     C                   GOTO      ENDVLT
140900990426     C                   endif
141000990426      *
141100990426      * CONTROLLO DIVISA
141200990426     C                   MOVEL(P)  WVLT          KKEY
141300990426     C     KTAB          CHAIN     TABEL00f                           30
141400990426      * ERRORE
141500990426     C                   if        *IN30 = *ON    OR
141600990426     C                              ( *IN30 = *OFF  and  TBLFLG <> *blanks)
141700990430     C                   MOVEL     msg(34)       V2CMSG
141800990426     C                   eval      *IN28 = *ON
141900990426     C                   GOTO      ENDVLT
142000990426     C                   endif
142100990426      *
142200990426     C                   MOVEL     TBLUNI        DSCV
142300990426      *
142400990426      * VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
142500990426     C                   IF        §CVFDC <> 'S'
142600990426     C                   MOVE      W0133         W0033
142700990426     C                   if        W0033 <> 0
142800990430     C                   movel     msg(35)       V2CMSG
142900990426     C                   eval      *IN28 = *ON
143000990426     C                   GOTO      ENDVLT
143100990426     C                   endif
143200010910     C                   else
143300010910      * controllo numero massimo di decimali impostabili per la valuta
143400010910     C     W0133         casNE     0             srCTRDEC
143500010910     C                   ENDCS
143600010910     C                   IF        *in28
143700010910     C                   MOVEL     MSG(40)       V2CMSG
143800010910     c                   move      §cvDEC        w002a
143900010914     c                   eval      %subst(V2CMSG:29:2) = w002a
144000010910     C                   GOTO      ENDVLT
144100010910     C                   ENDIF
144200990426     C                   ENDIF
144300990426      *
144400990426     C     ENDVLT        ENDSR
144500990426      *****************************************************************
144600990426      *  Ricerca alfabetica
144700990426      *****************************************************************
144800990426     c     ricalf        begsr
144900990426      *
145000990426     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
145100990426     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
145200990426     C                   Z-ADD     KCI           PAXccc                         Capoconto Clienti
145300990503     c                   movel(P)  V2Dksc        PAXdmt
145400000907     C                   z-add     1             PAXnum
145500990426      *
145600000907     C                   CALL      'XALFA3BR'
145700990426     C                   PARM                    PAXDUT
145800990426     C                   PARM                    CODUT
145900990426     C                   PARM                    PAXDMT
146000990426     C                   PARM                    PAXCCC
146100990426     C                   PARM                    PAXSTA
146200990426     C                   PARM      *blanks       PAXFLR
146300990426     C                   PARM      *blanks       PAXDIT
146400000907     C                   PARM                    PAXNUM            2 0
146500000907     C                   PARM                    PAXKCM           80
146600000907     C                   PARM                    PAXKSM          140
146700000907     C                   PARM                    PAXKDM           60
146800990426      *
146900990426      * NON TROVATO O PREMUTO CMD12
147000990426    2C     paxsta        IFGT      -1
147100000907     C                   MOVEL     paxksm        V2Cksc
147200990426    2C                   ENDIF
147300990426      *
147400990426     c                   endsr
147500990426      *****************************************************************
147600990426      *  MANUTENZIONE/INSERIMENTO ANAGRAFICA CLIENTI
147700990426      *****************************************************************
147800990426     C     MANCLI        BEGSR
147900990426      *
148000990426      * EMISSIONE
148100990428     c     emw01         tag
148200990428     c                   exfmt     FI32W01
148300990426      *
148400990428     c                   clear                   w1cmsg
148500990426     c                   eval      *in28 = *off
148600990426      *
148700990601     c   KL              goto      Emancli
148800990601      *
148900990426      * CONTROLLI
149000990428     c                   exsr      ctrw01
149100990428     c   28              goto      emw01
149200990601      *
149300990601     c     Emancli       tag
149400990601      *
149500990601     c                   movel     w1crag        V2dksc
149600990601     c                   movel     w1crag        W1V2dksc
149700990426      *
149800990601     C                   ENDSR
149900990426      *****************************************************************
150000990426      * CONTROLLI VIDEATA MANUTENZ./INSERIMENTO ANAGRAFICA CLIENTI
150100990426      *****************************************************************
150200990428     C     CTRW01        BEGSR
150300990426
150400990426     c     60            DO        69            XX
150500990426     c                   eval      *IN(XX) = *OFF
150600990426     c                   ENDDO
150700990426
150800990426      * Controllo Ragione Sociale, Indirizzo, Località, Provincia, CAP, Nazione e Partita IVA
150900990427     c                   CLEAR                   FNLV14DS
151000990430     c                   z-add     Wdatasp       I14dri
151100990428     c                   movel     W1Crag        I14rsc
151200990428     c                   movel     W1Cvia        I14ind
151300990428     c                   movel     W1Cloc        E14loc
151400990428     c                   movel     W1Cprv        E14prv
151500990428     c                   movel     W1Ccap        E14cap
151600990428     c                   movel     W1Cnaz        E14nar
151700990428     c                   eval      E14pip = W1Cive + W1Civa
151800990426      *
151900990427     c                   CALL      'FNLV14R'
152000990426     c                   PARM                    KPJBA
152100990426     c                   PARM                    FNLV14DS
152200990426      *
152300990428     c                   movel     I14ind        W1Cvia
152400990428     c                   movel     E14loc        W1Cloc
152500990428     c                   movel     E14prv        W1Cprv
152600990428     c                   movel     E14cap        W1Ccap
152700990428     c                   movel     E14nar        W1Cnaz
152800990428     c                   eval      W1Cive = %subst(E14pip:1:2)
152900990428     c                   eval      W1Civa = %subst(E14pip:3:14)
153000990426      *
153100990426     c                   eval      *in60 = (O14err = '1')                       Ragione Sociale
153200990426     c                   eval      *in61 = (O14err = '2')                       Indirizzo
153300990426     c                   eval      *in62 = (O14err = '5')                       CAP
153400990426     c                   eval      *in63 = (O14err = '3')                       Località
153500990426     c                   eval      *in64 = (O14err = '4')                       Provincia
153600990426     c                   eval      *in65 = (O14err = '6')                       Nazione
153700990426     c                   eval      *in68 = (O14err = '7')                       P.IVA
153800990426      *
153900990426     c                   IF        O14msg <> *blanks
154000990428     c                   movel     O14msg        W1Cmsg
154100990426     c                   eval      *IN28 = *ON
154200990426     c                   ELSE
154300990426      * Controllo il CAP
154400990426     C                   exsr      CAZIS0
154500990426     c                   ENDIF
154600990428     C   28              goto      ectW01
154700990426
154800990426      * CONTROLLO IL NUMERO FAX
154900990428     C                   IF        W1Cfax <> *BLANKS
155000990426     C                   clear                   TRUL42DS
155100990428     C                   movel     W1Cfax        D42FAX
155200990426     C                   CALL      'TRUL42R'
155300990426     C                   PARM                    TRUL42DS
155400990426     C                   IF        D42ERR = '1'
155500990426     C                   eval      *in66 = *on
155600990426     C                   eval      *in28 = *on
155700990428     C                   movel     D42MSG        W1cmsg
155800990426     C                   ENDIF
155900990426     C                   ENDIF
156000990428     C   28              goto      ectW01
156100990426
156200990426      * CONTROLLO IL NUMERO DI TELEFONO
156300990428     C                   IF        W1Ctel <> *BLANKS
156400990426     C                   clear                   TRUL42DS
156500990428     C                   movel     W1Ctel        D42FAX
156600990426     C                   CALL      'TRUL42R'
156700990426     C                   PARM                    TRUL42DS
156800990426     C                   IF        D42ERR = '1'
156900990426     C                   eval      *in67 = *on
157000990426     C                   eval      *in28 = *on
157100990428     C                   movel     D42MSG        W1cmsg
157200990426     C                   ENDIF
157300990426     C                   ENDIF
157400990428     C   28              goto      ectW01
157500990426
157600990428     c     ECTW01        ENDSR
157700990426      *---------------------------------------------------------------*
157800990426      * ROUTINE DI CONTROLLO ESISTENZA CAP                            *
157900990426      *---------------------------------------------------------------*
158000990426     C     CAZIS0        BEGSR
158100990426      *
158200990426      * CONTROLLO CAP PER INTERO
158300990426     C                   CLEAR                   TISI95DS
158400990426     C                   CLEAR                   FNLV13DS
158500990426     C                   MOVEL     ' '           I95TLA
158600990426     C                   MOVEL     '7'           I95TCN
158700990428     C                   MOVEL     W1cCAP        I95CAP
158800990428     C                   MOVEL     W1cNAZ        I95NAR
158900990428     C                   MOVEL     W1cvia        I95IND
159000990428     C                   MOVEL     W1cloc        I95LOC
159100990428     C                   MOVEL     W1cprv        I95PRV
159200990426     C                   MOVEL     'S'           I13AF0
159300990426     C                   MOVEL     'S'           I13AF1
159400990426     C                   MOVEL     'S'           I13SZ2
159500990426     C                   MOVEL     'S'           I13LA3
159600990426     C                   MOVEL     'S'           I13SZ3
159700990426     C                   MOVEL     'S'           I13CNV
159800990426     C                   MOVEL     'S'           I13SZV
159900990426     C* Gestione errori forzabili: se cambiato CAP/Località
160000990426      * riazzero i valori di errori forzabili
160100990428    2C     W1cCAP        IFNE      WSVCAP
160200990426     C                   CLEAR                   WFZV
160300990426     C                   CLEAR                   WFZ1
160400990426     C                   CLEAR                   WFZ2
160500990426     C                   CLEAR                   WFZ3
160600990428     C                   MOVEL     W1cCAP        WSVCAP
160700990426    2C                   ENDIF
160800990426      * Modifica località
160900990428    2C     W1cLOC        IFNE      WSVLOC
161000990428     C                   MOVEL     W1cLOC        WSVLOC
161100990426     C                   CLEAR                   WFZ1
161200990426     C                   CLEAR                   WFZ2
161300990426     C                   CLEAR                   WFZ3
161400990426    2C                   ENDIF
161500990426      * Modifica PROVINCIA
161600990428    2C     W1cPRv        IFNE      WSVPRv
161700990428     C                   MOVEL     W1cPRv        WSVPRv
161800990426     C                   CLEAR                   WFZ3
161900990426    2C                   ENDIF
162000990426      * CALL
162100990426     C                   MOVEL     WFZV          E13FZV
162200990426     C                   MOVEL     WFZ1          E13FZ1
162300990426     C                   MOVEL     WFZ2          E13FZ2
162400990426     C                   MOVEL     WFZ3          E13FZ3
162500990426     C                   CALL      'FNLV13R'
162600990426     C                   PARM                    KPJBA
162700990426     C                   PARM                    FNLV13DS
162800990426     C                   PARM                    TISI95DS
162900990426     C* Ricontrollo errori
163000990426     C                   MOVEL     O13ERR        WERR
163100990426     C                   MOVEL     O13MSG        WMSG
163200990426     C                   MOVEL     E13FZV        WFZV
163300990426     C                   MOVEL     E13FZ1        WFZ1
163400990426     C                   MOVEL     E13FZ2        WFZ2
163500990426     C                   MOVEL     E13FZ3        WFZ3
163600990426     C*
163700990426     C* Se il livello di affidabilità non è inferiore a 2 e
163800990426     C* il livello di reperimento dati non è inferiore a 3
163900990426     C* reimposto i dati da TISI95DS
164000990426     C     O13RON        IFEQ      'S'
164100990426     C     O13ROC        OREQ      'S'
164200990426     C     O13ROP        OREQ      'S'
164300990426     C     O13ROL        OREQ      'S'
164400990428     C                   MOVEL     O95LOC        W1cLOC
164500990428     C                   MOVEL     O95PRV        W1cprv
164600990428     C                   MOVEL     O95NAR        W1cnaz
164700990428     C                   MOVEL     O95CAP        W1cCAP
164800990426     C                   END
164900990426     C*
165000990426     C                   EXSR      CTRIND
165100990426     C* Se non ci sono errori proseguo
165200990426     C   28              GOTO      FINCTR
165300990426     C*
165400990426     C* Controllo se reimpostata località
165500990426     C     O13ROL        IFEQ      'S'
165600990426     c                   eval      *in63 = *on
165700990426     c                   eval      *in28 = *on
165800990428     C                   MOVEL     msg(26)       W1cmsg
165900990426     C                   GOTO      FINCTR
166000990426     C                   END
166100990426     C* Controllo se reimpostata Provincia
166200990426     C     O13ROP        IFEQ      'S'
166300990426     c                   eval      *in64 = *on
166400990426     c                   eval      *in28 = *on
166500990428     C                   MOVEL     msg(28)       W1cmsg
166600990426     C                   GOTO      FINCTR
166700990426     C                   END
166800990426     C* Controllo se reimpostato CAP
166900990426     C     O13ROC        IFEQ      'S'
167000990426     c                   eval      *in62 = *on
167100990426     c                   eval      *in28 = *on
167200990428     C                   MOVEL     msg(27)       W1cmsg
167300990426     C                   GOTO      FINCTR
167400990426     C                   END
167500990426     C* Controllo se reimpostata Nazione
167600990426     C     O13RON        IFEQ      'S'
167700990426     c                   eval      *in65 = *on
167800990426     c                   eval      *in28 = *on
167900990428     C                   MOVEL     msg(29)       W1cmsg
168000990426     C                   GOTO      FINCTR
168100990426     C                   END
168200990426     C*
168300990426     C     FINCTR        ENDSR
168400990426     C*------------------------------------------------------*
168500990426     C*  Controllo dati indirizzo
168600990426     C*------------------------------------------------------*
168700990426     C     CTRIND        BEGSR
168800990426     C*
168900990426     C* Errore !!!!
169000990426     C     WERR          IFNE      ' '
169100990426     C     WMSG          ANDNE     *BLANKS
169200990426     C* Manca la Ragione sociale
169300990426     C                   SELECT
169400990426     C* Errore su località/città
169500990426     C     WERR          WHENEQ    '3'
169600990426     c                   eval      *in63 = *on
169700990426     c                   eval      *in28 = *on
169800990426     C* Errore su Cap
169900990426     C     WERR          WHENEQ    '5'
170000990426     c                   eval      *in62 = *on
170100990426     c                   eval      *in28 = *on
170200990426     C* Errore su provincia
170300990426     C     WERR          WHENEQ    '4'
170400990426     c                   eval      *in64 = *on
170500990426     c                   eval      *in28 = *on
170600990426     C* Nazione  errata
170700990426     C     WERR          WHENEQ    '6'
170800990426     c                   eval      *in65 = *on
170900990426     c                   eval      *in28 = *on
171000990426    2C                   ENDSL
171100990426      *
171200990428     C                   MOVEL     WMSG          W1cmsg
171300990426     C                   END
171400990426     C*
171500990426     C                   ENDSR
171600990426      *****************************************************************
171700990426      *   CONFERMA DATI C. A.
171800990426      *****************************************************************
171900990426     C     AGGIORNA      BEGSR
172000990426
172100990430      * Descrizioni
172200990430      *     se modificato l'interlocutore
172300990430     c                   if        V2Dnoc <> WV2Dnoc
172400990430     C                   EXSR      WRTDCS
172500011126     C                   ELSE
172600011126     c                   IF        Wfndcs = 'S'
172700011126     c                   UNLOCK    fndcs01l
172800011126     c                   endif
172900990430     c                   endif
173000990430
173100990430      * Fasi
173200990503      *     se modificato il tipo gestione o il beneficiario
173300990503     c                   if        V2Cfpr <> DCTfpr  or  V2Cptr <> DCTptr
173400990504     c                                               or  V2Cipr <> £DCLipr
173500990430     C                   EXSR      WRTDCF
173600990430     c                   endif
173700990430
173800990430      * Liquidazione C.A.
173900990726      *     se modificato l'importo concordato o nuovo tipo gestione "T"
174000990927     c                   if        £DCLipr <> V2Cipr  or
174100990927     c                             (V2Cfpr <> DCTfpr  and  V2Cfpr = 'T')
174200990426     C                   EXSR      WRTDCL
174300990426     c                   endif
174400990430
174500990426      * Anagrafiche
174600990426     C                   EXSR      WRTDKA
174700990430
174800990426      * Testata C.A.
174900990426     C                   EXSR      WRTDCT
175000020130      *
175100020130     C                   IF        §DCTpfra <> wFRA
175200020130     C                   exsr      WRTDCS_2
175300020130     C                   ENDIF
175400990430
175500990426     C                   ENDSR
175600990430      *****************************************************************
175700990430      *  GESTIONE SCRITTURA DESCRIZIONI C.A.
175800990430      *****************************************************************
175900990430     C     WRTDCS        BEGSR
176000990430
176100990609     c                   IF        Wfndcs <> 'S'
176200990430     c                   clear                   fndcs000
176300990430     c                   movel     ktpd          dcstpd
176400990430     c                   movel     knkt          dcsnkt
176500990430     c                   movel     kstd          dcsstd
176600990430     c                   movel     knks          dcsnks
176700990430     c                   movel     kdim          dcsdim
176800990430     c                   movel     khim          dcshim
176900990430     c                   movel     ktrc          dcstrc
177000990430     c                   z-add     1             dcspno
177100990430     c                   movel     DCTgfc        dcspos
177200990430     c                   movel     knmus         dcspru
177300990609     c                   ENDIF
177400990430      *
177500990430     c                   movel     V2Dnoc        dcsnot
177600990430     c                   clear                   dcsatb
177700990430     c                   clear                   dcsft1
177800990430     c                   clear                   dcsdt1
177900990430      *
178000990430     c                   SELECT
178100990430     c                   WHEN      Wfndcs = 'S'
178200990430     c                   update    fndcs000
178300990430     c                   WHEN      V2Dnoc <> *blanks
178400990430     c                   write     fndcs000
178500990430     c                   ENDSL
178600990430
178700990430     C                   ENDSR
178800020130      *****************************************************************
178900020130      *  SCRITTURA NOTE AGGIUTIVE
179000020130      *****************************************************************
179100020130     C     WRTDCS_2      BEGSR
179200020130      *
179300020130      * cerco l'ultimo progressivo note della fase in corso
179400020130      * - campi strandard
179500020130     c                   eval      Ktpd     = 'C'
179600020130     c                   eval      Knkt     = DS_numca
179700020130     c                   eval      Kstd     = *blanks
179800020130     c                   eval      Kdim     = dateu
179900020130     c                   time                    W0140
180000020130     c                   movel     W0140         Khim
180100020130      * - dati relativi alla CA in manutenzione
180200020130     c                   movel(p)  DCTfca        Knks
180300020130     c                   eval      Ktrc     = 'N'
180400020130      * cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
180500020130     c     Kdcs          setgt     FNDCS01L
180600020130     c     Kdcs          readpe    FNDCS01L
180700020130      *
180800020130     c                   clear                   DCSatb
180900020130     c                   eval      DCStpd   = Ktpd
181000020130     c                   eval      DCSnkt   = Knkt
181100020130     c                   eval      DCSstd   = Kstd
181200020130     c                   eval      DCSdim   = Kdim
181300020130     c                   eval      DCShim   = Khim
181400020130     c                   eval      DCSnks   = Knks
181500020130     c                   eval      DCStrc   = Ktrc
181600020130     c                   IF        %eof(FNDCS01L)
181700020130     c                   eval      DCSpno   = 1
181800020130     c                   else
181900020130     c                   eval      DCSpno   = DCSpno + 1
182000020130     c                   ENDIF
182100020130     c                   IF        §DCTpfra =  'F'
182200020205     c                   eval      DCSnot   = Comm_Fra_I
182300020130     c                   else
182400020205     c                   eval      DCSnot   = Comm_Fra_E
182500020130     c                   ENDIF
182600020130     c                   eval      DCSpru   = KNMUS
182700020130     c                   eval      DCSpos   = DCTgfc
182800020130     c                   clear                   DCSstn
182900020130     c                   clear                   DCSft1
183000020130     c                   clear                   DCSdt1
183100020130      *
183200020130     c                   write     FNDCS000
183300020130      *
183400020130     c                   clear                   Khim
183500020130      *
183600020130     C                   ENDSR
183700990426      *****************************************************************
183800990426      *  SCRITTURA FASI C.A.
183900990426      *****************************************************************
184000990426     c     WRTDCF        BEGSR
184100990426      *
184200990426     c                   clear                   fndcf000
184300990426      *
184400990430     c                   z-add     DCTaac        DCFaac
184500990430     c                   z-add     DCTfil        DCFfil
184600990430     c                   z-add     DCTnca        DCFnca
184700990427     c                   movel     V2cptr        DCFptr
184800990430      * Imposto la fase
184900990426     c                   SELECT
185000990531     C                   WHEN      V2Cfpr = 'P'  and  DCTfpr = 'P'
185100990531     c                   z-add     DCTfca        DCFfca
185200990531      *
185300990503     C                   WHEN      V2Cfpr = 'P'
185400990503     c                   z-add     WFfff         DCFfca
185500990503      *
185600990503     C                   WHEN      V2Cipr = 0
185700990430     c                   z-add     WFfff         DCFfca
185800990503      *
185900990503     C                   WHEN      V2Cipr > 0
186000990503     c                   z-add     WFffs         DCFfca
186100990426     c                   ENDSL
186200020201      *
186300020201      * ripulisco il flag Franchigia se modificato il tipo gestione
186400020201      *  da "P" a "T"
186500020201     c                   IF        V2Cfpr = 'T'  and  DCTfpr = 'P'
186600020201     c                   clear                   §DCFpfra
186700020201     c                   else
186800020201     c                   movel     §DCTpfra      §DCFpfra
186900020201     c                   ENDIF
187000990426      *
187100990426     c                   z-add     dateu         DCFdfc
187200990426     c                   movel     w0140         DCFhfc
187300990503     c                   z-add     DCTgfc        DCFfev
187400990426     c                   movel     knmus         DCFpru
187500020201     c                   clear                   §DCFriti
187600020201     c                   movel     DDCF01        DCFlet
187700990426     c                   clear                   DCFftr
187800990426     c                   clear                   DCFdtr
187900990426      *
188000990426     c                   write     fndcf000
188100990426      *
188200990426     C                   ENDSR
188300990426      *****************************************************************
188400990426      *  SCRITTURA/AGGIORNAMENTO liquidazione C.A.
188500990426      *****************************************************************
188600990426     C     WRTDCL        BEGSR
188700990426      *
188800990426     c                   z-add     V2Cipr        DCLipr
188900010905     c                   IF        DCLipr <> 0
189000990430     c                   movel     V2Cvlt        DCLvpr
189100010905     c                   else
189200010905     c                   clear                   DCLvpr
189300010905     c                   ENDIF
189400990430     c                   clear                   DCLcpr
189500990426     c                   z-add     V2Cipr        DCLipl
189600010905     c                   IF        DCLipl <> 0
189700990430     c                   movel     V2Cvlt        DCLvpl
189800010905     c                   else
189900010905     c                   clear                   DCLvpl
190000010905     c                   ENDIF
190100990430     c                   clear                   DCLcpl
190200990426      *
190300990430     c                   z-add     DCTaac        DCLaac
190400990430     c                   z-add     DCTfil        DCLfil
190500990430     c                   z-add     DCTnca        DCLnca
190600990430     c                   clear                   DCLatb
190700990430     c                   clear                   DCLftr
190800990426      *
190900990426     c                   If        Wfndcl = 'S'
191000990426     c                   UPDATE    FNDCL000
191100990426     c                   Else
191200990426     c                   WRITE     FNDCL000
191300990426     c                   Endif
191400990426      *
191500990426     C                   ENDSR
191600990426      *****************************************************************
191700990426      *  SCRITTURA/AGGIORNAMENTO anagrafica clienti per danni
191800990426      *****************************************************************
191900990426     C     WRTDKA        BEGSR
192000990426
192100990426     c                   movel     'C'           Wtrc
192200990426     c     KDKA          chain     fndka01l                           30
192300990427     c                   move      V2Cksc        W0040
192400990426      *
192500990426      *     Se è codificato o non è impostato annullo FNDKA se esiste
192600990426     c                   IF        W0040 <> 8888
192700990426     c                   If          *IN30 = *OFF
192800990426     c                   movel     'A'           dkaatb
192900990426     c                   clear                   dkaftr
193000990426     c                   update    fndka000
193100990430     c                   Endif
193200990430      *
193300990426     c                   ELSE
193400990426      *
193500990426     c                   clear                   FNDKA000
193600990430     c                   z-add     DCTaac        dkaaac
193700990430     c                   z-add     DCTfil        dkafil
193800990430     c                   z-add     DCTnca        dkanca
193900990426      *
194000990427     c                   move      V2Cksc        DKAksc
194100990426     c                   movel     Wtrc          DKAtrc
194200990426     c                   movel     KCI           DKAkcc
194300990430     c                   movel     W1Crag        DKArag
194400990430     c                   movel     W1Cvia        DKAvia
194500990430     c                   movel     W1Ccap        DKAcap
194600990430     c                   movel     W1Cloc        DKAloc
194700990430     c                   movel     W1Cprv        DKAprv
194800990430     c                   movel     W1Cnaz        DKAnaz
194900990430     c                   eval      DKAiva = W1Civa + W1Cive
195000990430     c                   movel     W1Ctel        DKAtel
195100990430     c                   movel     W1Cfax        DKAfax
195200990426      *
195300990426      * Scrivo o aggiorno
195400990426     c                   If        *IN30 = *ON
195500990426     c                   write     fndka000
195600990426     c                   Else
195700990426     c                   update    fndka000
195800990426    2c                   Endif
195900990426      *
196000990426     c                   ENDIF
196100990426
196200990426     C                   ENDSR
196300990426      **********************************************************************
196400990426      *  SCRITTURA/AGGIORNAMENTO TESTATA C.A.
196500990426      **********************************************************************
196600990426     C     WRTDCT        BEGSR
196700990430
196800990430      * Imposto la fase e il P.O. gestione
196900990430     c                   SELECT
197000990503     C                   WHEN      V2Cfpr = DCTfpr  and  V2Cptr = DCTptr
197100990504     C                                              and  V2Cipr = £DCLipr
197200990503      *
197300990531     C                   WHEN      V2Cfpr = 'P'  and  DCTfpr = 'P'
197400990531      *
197500990503     C                   WHEN      V2Cfpr = 'P'
197600990503     c                   z-add     WFfff         DCTfca
197700990503     c                   if        V2Cptr = 'M'
197800990503     c                   z-add     Wlnp          DCTgfc
197900990503     c                   else
198000990503     c                   z-add     DCTlna        DCTgfc
198100990503     c                   endif
198200990503      *
198300990503     C                   WHEN      V2Cipr = *zeros
198400990430     c                   z-add     WFfff         DCTfca
198500990430     c                   if        V2Cptr = 'M'
198600990430     c                   z-add     Wlnp          DCTgfc
198700990430     c                   else
198800990430     c                   z-add     DCTlna        DCTgfc
198900990430     c                   endif
199000990503      *
199100990503     C                   WHEN      V2Cipr > *zeros
199200990430     c                   z-add     WFffs         DCTfca
199300990430     c                   z-add     046           DCTgfc
199400990430     c                   ENDSL
199500020201      *
199600020201      * ripulisco il flag Franchigia se modificato il tipo gestione
199700020201      *  da "P" a "T"
199800020201     c                   IF        V2Cfpr = 'T'  and  DCTfpr = 'P'
199900020201     c                   clear                   §DCTpfra
200000020201     c                   ENDIF
200100990430
200200990430      * Imposto altri campi
200300990430     c                   movel     V2Cfpr        dctfpr
200400990430     c                   movel     V2Cptr        dctptr
200500990430     c                   z-add     KCI           dctkcc
200600990430     c                   z-add     V2Cksc        dctksc
200700020130     c                   movel     DDCT01        DCTflo
200800990426
200900990426      * Imposto flag per trasmissione dati
201000990426     c                   clear                   dctft1
201100990426     c                   clear                   dctft2
201200990426
201300990426     c                   UPDATE    fndct000
201400990430
201500990426     C                   ENDSR
201600990426      *****************************************************************
201700990426      *  AGGANCIO ANAGRAFICHE
201800990426      *****************************************************************
201900990426     C     CTR_ANAGRA    BEGSR
202000990428      *
202100990426     C                   CALL      'TIBS69R'
202200990426     C                   PARM                    tibs69DS
202300990426     C                   PARM                    DS_cnaco
202400990426     C                   PARM                    DS_cnind
202500990426     C                   PARM                    DS_cnclp
202600990426     C                   PARM                    DS_fncls
202700990426      *
202800990426     C                   eval      *IN30 = (O69ERR =  '1')
202900990426      *
203000990426     C                   eval      wtibs69r = 'S'
203100990426      *
203200990426     C                   ENDSR
203300990427      **************************************************************************
203400990427      * CARICO SCHIERA L6 DA TABELLA £6
203500990427      **************************************************************************
203600990427     C     carl6         BEGSR
203700990427      *
203800990427     C                   CLEAR                   L6
203900990427     C                   CLEAR                   TRUL06DS
204000990427     C                   MOVE      '£6'          D06COD
204100990427     C                   MOVEL(P)  V1Cfgs        D06key
204200990427     C                   MOVEL(P)  TRUL06DS      KPJBU
204300990427      *
204400990427     C                   CALL      'TRUL06R'
204500990427     C                   PARM                    KPJBA
204600990427      *
204700990427     C                   MOVEL     KPJBU         TRUL06DS
204800990427     C                   movea     LIN           L6
204900990427      *
205000990427     c                   endsr
205100010910
205200010910      *****************************************************************
205300010910      * VERIFICA NUMERO DECIMALI IMPOSTATI
205400010910      *****************************************************************
205500010910     C     srCTRDEC      BEGSR
205600010910      *
205700011017     C                   reset                   XDECDS
205800010910      *
205900011017     C                   z-add     W0133         ICDnum
206000011017     C                   z-add     §cvDEC        ICDnrd
206100010910      *
206200011017     C                   call      'XDEC'
206300011017     C                   parm                    XDECDS
206400010910      *
206500011017     C                   IF        OCDesi  <> *off
206600010910     C                   eval      *IN28 = *ON
206700010910     C                   ENDIF
206800010910      *
206900010910     C                   ENDSR
207000010905
207100010905      *****************************************************************
207200010905      * CONVERSIONE IMPORTI IN MONETA CORRENTE
207300010905      *****************************************************************
207400010905     C     srCMB         BEGSR
207500010905      *
207600010905     C                   reset                   YEURCODS
207700010905      *
207800010905     C                   movel     WVLT          yecDVI
207900010905     C                   z-add     W0133         yecIMI
208000010910     C                   movel     §gedDBA       yecDVO
208100010905      *
208200010905     C                   call      'YEURCO'
208300010905     C                   parm                    YEURCODS
208400010905      *
208500010905     C                   IF        yecESI  = ' '
208600010905     C                   z-add     yecIMO        W0133
208700010905     C                   ENDIF
208800010905      *
208900010905     C                   ENDSR
209000010905
209100990426      *****************************************************************
209200990426      *  ROUTINE INIZIALE
209300990426      *****************************************************************
209400990426     C     *INZSR        BEGSR
209500990426
209600990426     C     *ENTRY        PLIST
209700990426     C                   PARM                    KPJBA
209800990426
209900990503     c                   movel     TIT           VTCtit
210000990503
210100990426      * Dati società
210200990426     C                   Z-ADD     1             CODUT
210300990426     C                   CALL      'X§PARUT'
210400990426     C                   PARM                    UT§DSE
210500990426      *
210600990426     C                   MOVEL     RAGUT         RSUT
210700990426     C                   MOVEL     REC80         CNCR80
210800990426      *
210900990426      *--- RICERCA CAPOCONTI
211000990426     C                   DO        50            XX
211100990426     C                   MOVE      TCU(XX)       TCUDS
211200990426     C     f56           CABNE     'CG'          END1
211300990426     C     f4            COMP      '1'                                    21
211400990426     C     f4            COMP      '2'                                    22
211500990426     C     f4            COMP      '3'                                    23
211600990426     C     f4            COMP      '6'                                    27
211700990426     C** 1 CLIENTI   21
211800990426     C** 2 FORNITORI 22
211900990426     C** 3 AGENTI    23
212000990426     C     f3            comp      '0'                                242425
212100990426     C     f3            comp      'I'                                    26
212200990426     C** 0 ITALIA   25
212300990426     C** 1 ESTERO   24
212400990426     ** I CAPO CONTO IVA
212500990426     c                   if        *in21 = *on
212600990426     C   24              Z-ADD     KCU(XX)       KCE               4 0
212700990426     C   25              Z-ADD     KCU(XX)       KCI               4 0
212800990426     c                   end
212900990426     c                   if        *in22 = *on
213000990426     C   24              Z-ADD     KCU(XX)       KFE               4 0
213100990426     C   25              Z-ADD     KCU(XX)       KFI               4 0
213200990426     c                   end
213300990426     c                   if        *in23 = *on
213400990426     C   24              Z-ADD     KCU(XX)       KAE               4 0
213500990426     C   25              Z-ADD     KCU(XX)       KAI               4 0
213600990426     c                   end
213700990426     C   26              Z-ADD     KCU(XX)       KIVA              4 0
213800990426     C   27              Z-ADD     KCU(XX)       KBNA              4 0
213900990426     C     end1          tag
214000990426     C                   end
214100990426     C                   SETOFF                                       212223
214200990426     C                   SETOFF                                       242526
214300990426     C                   SETOFF                                       27
214400990427      *
214500990427      * IMPOSTO IL P.O. IN GESTIONE
214600020502     c                   z-add     simpou        v1cfgs
214700990427     c                   IF        SIMFEL = *ZEROS
214800020502     C*                  z-add     46            V1Cfgs
214900990427     C                   eval      L6(1) = V1Cfgs
215000990427     C                   eval      *in06 = *on
215100990427     C                   eval      *in07 = *on
215200990427     c                   ELSE
215300990427      *
215400020502     C*                  If        rem = 'REM'  AND  remfil > *ZEROS
215500020502     C*                  movel     REMFIL        V1CFGS
215600020502     C                   If        simtpp = '2' or simtpp = *blanks
215700990427     C                   eval      *in06 = *on
215800020502   X2C*                  Else
215900020502     C*                  movel     SIMFEL        V1CFGS
216000990427    2C                   Endif
216100990427      *
216200990427     C     V1CFGS        CHAIN     AZORG01L                           31
216300990427     c                   IF        *IN31 = *OFF
216400990427     C                   MOVEL     orgDES        V1DFGS
216500990427    2C                   ENDIF
216600990427      *
216700990427     C                   EXSR      CARL6
216800990427    2C                   ENDIF
216900010905
217000010905      * aggancio tabella GED
217100010910     c                   clear                   DGEDDN
217200010905     c                   clear                   TIBS02DS
217300010905     C                   MOVEL     'C'           t02mod
217400010905     C                   MOVEL     knsif         t02sif
217500010905     C                   MOVEL     'GED'         t02cod
217600010905     C                   MOVEL(P)  'DANNI'       t02ke1
217700010905      *
217800010905     C                   CALL      'TIBS02R'
217900010905     C                   PARM                    KPJBA
218000010905     C                   PARM                    TIBS02DS
218100010905      *
218200010905     C                   if        t02err = *BLANKS
218300010910     C                   MOVEL     T02UNI        DGEDDN
218400010905     C                   endif
218500990427
218600990426      ***
218700990426      * leggo tabella standards danni
218800990426      ***
218900990426     c                   clear                   TIBS02DS
219000990426     C                   MOVEL     'C'           t02mod
219100990426     C                   MOVEL     knsif         t02sif
219200990426     C                   MOVEL     'STD'         t02cod
219300990426     C                   MOVEL(P)  1             t02ke1
219400010910     C                   MOVEL(P)  §gedDBA       t02ke2
219500990426      *
219600990426     C                   CALL      'TIBS02R'
219700990426     C                   PARM                    KPJBA
219800990426     C                   PARM                    TIBS02DS
219900990426      *
220000990426     C                   if        t02err = *BLANKS
220100990426     C                   MOVEL     T02UNI        DSTD
220200990426     C                   endif
220300990426      ***
220400990426      * leggo tabella variabili tariffe per limite risarcibile
220500990426      ***
220600990426      * ORA
220700990426     C                   TIME                    W0140
220800990426      * UDATE IN GGMMAAAA
220900990426     C                   MOVE      W0140         WDTGIO
221000990426      * UDATE IN AAAAMMGG
221100990426     C                   Z-ADD     WDTGIO        G02DAT
221200990426     C                   MOVEL     *BLANK        G02ERR
221300990426     C                   CALL      'XSRDA8'
221400990426     C                   PARM                    WLBDAT
221500990426     C                   MOVEL     G02INV        DATEU
221600990503     C                   MOVEL     G02INV        V1Caac
221700990426
221800990426      * APRO FILE SPECIFICI PER SEDE/FILIALE/MODALITA' PGM
221900990426     C                   IF        SIMFEL = *zeros
222000990809     C                   open      TITAS30C
222100990426     C                   open      TNTMD01L
222200990426     c                   ELSE
222300990426     C                   open      FNARB01L
222400990426     C                   open      FNBLP01L
222500991116     C                   open      FIAR601L
222600990426     c                   ENDIF
222700990426
222800990426     C     KTAB          KLIST
222900990426     C                   KFLD                    CODUT
223000990426     C                   KFLD                    KCOD
223100990426     C                   KFLD                    KKEY
223200990426      *
223300990426     c     kbol01        klist
223400040802     c                   kfld                    Keyaas
223500040802     c                   kfld                    Keylnp
223600040802     c                   kfld                    Keynrs
223700040802     c                   kfld                    Keynsp
223800991116      *
223900991116     c     kAR6          klist
224000040802     c                   kfld                    Keyaas
224100040802     c                   kfld                    Keylnp
224200040802     c                   kfld                    Keynrs
224300040802     c                   kfld                    Keynsp
224400991116     c                   kfld                    TRCar6
224500990426      *
224600990426     C     KDET          KLIST
224700990427     C                   KFLD                    DCTaae
224800990427     C                   KFLD                    DCTnev
224900990426      *
225000990426     C     KDCS          KLIST
225100990426     C                   KFLD                    KTPD
225200990426     C                   KFLD                    KNKT
225300990426     C                   KFLD                    KSTD
225400990426     C                   KFLD                    KDIM
225500990426     C                   KFLD                    KHIM
225600990426     C                   KFLD                    KNKS
225700990426     C                   KFLD                    KTRC
225800990426      *
225900990426     c     kca           klist
226000990503     c                   kfld                    v1caac
226100990503     c                   kfld                    v1cfil
226200990503     c                   kfld                    v1cnca
226300990426      *
226400990426     c     KDKA          klist
226500990430     c                   kfld                    dctaac
226600990430     c                   kfld                    dctfil
226700990430     c                   kfld                    dctnca
226800990426     c                   kfld                    Wtrc
226900990426      *
227000990426     C                   ENDSR
227100990426      *------------------------------------------------------------
227200990426** MSG  L78                                                                  *
227300990427Immesso numero C.A. non valido                                                 1
227400990427La Comunicazione Anomalia selezionata è inesistente                            2
227500990427La Comunicazione Anomalia selezionata è annullata                              3
227600990427La Comunicazione Anomalia selezionata è chiusa                                 4
227700990427Comunicazione Anomalia non manutenzionabile: in gestione ad altro P.O.         5
227800990428Non trovata spedizione relativa alla Comunicazione Anomalia                    6
227900990428Non consentite modifiche per la Comunicazione Anomalia selezionata             7
228000990428Tipo gestione: impostare un valore corretto                                    8
228100990428Non è possibile modificare in pratica il cliente ha trattenuto                 9
228200990428Non è possibile modificare in pratica presentata rivalsa                       10
228300990428Beneficiario: impostare un valore corretto                                     11
228400120724Beneficiario: la modifica è possibile solo per C.A. senza Importo Trattenuto   12
228500120724LIBERO - ex Rimborso a carico: impostare un valore corretto                                13
228600120724LIBERO - ex Rimborso a carico: impostato automaticamente "Vettore"                         14
228700120724Comunicazione Anomalia non manutenzionabile: ricevuti tutti i documenti        15
228800990428Comunicazione Anomalia non manutenzionabile: Liq. Transattiva contabilizzata   16
228900990428Comunicazione Anomalia non manutenzionabile con questo programma               17
229000990430Il Beneficiario è stato modificato, verificare il Beneficiario Alternativo     18
229100990430Codice 9999 non utilizzabile. Inserire 8888 o un codice esistente su P.d.C.    19
229200990430Il cliente trattiene, indicarne il codice nel beneficiario alternativo         20
229300990430P.O. inesistente o annullato                                                   21
229400990430Codice errato o annullato                                                      22
229500990430Il Beneficiario Alternativo non è il codice fatturazione del Beneficiario      23
229600990430Il Beneficiario è stato modificato, verificare se cambia l'Interlocutore       24
229700990504Eliminare l'importo concordato, non previsto per C.A. in Pratica Assicurativa  25
229800990430Attenzione !! Modificata Località                                              26
229900990430Attenzione !! Modificato CAP                                                   27
230000990430Attenzione !! Modificata Provincia                                             28
230100990430Attenzione !! Modificata Nazione                                               29
230200990503Il Beneficiario è stato modificato, verificare l'importo concordato            30
230300051117Non è possibile LT, CA su spedizione non assicurata con Evento FORTUITO        31
230400990430L'Importo Concordato è superiore all'Importo Limite per la Liq. Transattiva    32
230500051117Non è possibile modificare in LT, CA su spedizione Porto Franco Import         33
230600990430Divisa errata                                                                  34
230700990430Non ammessi importi con decimali per la divisa indicata                        35
230800120724LIBERO - ex Rimborso a carico: impostato automaticamente "Consuldanni"                     36
230900120724LIBERO - ex Rimborso a carico: impostato "Vettore", il cliente ha trattenuto               37
231000120724LIBERO - ex Rimborso a carico: impostato "Vettore", presentata rivalsa                     38
231100120724LIBERO - ex Non è possibile modificare in Liquidazione Transattiva, CA con evento.         39
231200010910Ammessi importi con massimo __ decimali per la divisa indicata                 40
231300050720Non è possibile modificare in Liq. Transattiva, CA con evento FORTUITO         41

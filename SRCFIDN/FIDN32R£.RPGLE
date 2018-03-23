000100990510      *------------------------------------------------------------------------*
000200990510      *                    MODIFICA TIPO GESTIONE C.A.                         *
000300990428      *------------------------------------------------------------------------*
000400990428      *  1)  da Pratica a Transattiva. Sede e Filiale                          *
000500990428      *  2)  da Transattiva a Pratica. Solo Sede                               *
000600990428      *  3)  beneficiario. Solo Sede                                           *
000700990504      *  4)  beneficiario alternativo, rimborso a carico, interlocutore ed     *
000800990504      *      importo concordato. Se abilitati a una delle precedenti variazioni*
000900980506      *------------------------------------------------------------------------*
001000990426
001100980803      ****************************************************************
001200980803      *  FASI C.A. FISSATE A PROGRAMMA  (ricercare con 0 davanti)
001300980803      ****************************************************************
001400990504      * 100 = Conferma C.A.                                 WFCON
001500990504      * 110 = Fittizia in Filiale                           WFFFF
001600990430      * 225 = Fittizia in SEDE per pagamento L.T.           WFFFS
001700990430      * 230 = Contabilizzazione L.T.                        WFCLT
001800990428      * 660 = Invio documenti a Consuldanni                 WFIDC
001900990428      ****************************************************************
002000990426
002100990426      ****************************************************************
002200990426      *  RIEPILOGO INDICATORI
002300990426      ***************************************************************
002400990427      * 01 - VISUALIZZA numero evento
002500990427      * 02 - VISUALIZZA numero pratica assicurativa
002600990428      * 03 - ABILITO F8 Manutenzione anagrafica clienti
002700990427      * 04 -
002800990427      * 05 - SPROTEGGO  "P.O. IN GESTIONE"
002900990427      * 06 - DISABILITO F18
003000990427      * 07 - NON VISUALIZZO "P.O. IN GESTIONE". Abbinare a IN06
003100990427      ***
003200990427      * 28 - ERRORE GENERICO DSPF
003300990427      * 29 - RIEMISSIONE FORMATO VIDEO
003400990427      * 30 - COMODO
003500990427      * 31 - COMODO
003600990427      * 32 - COMODO
003700990426      ***  Video 1
003800990427      * 35 - ERRORE inserimento P.O in gestione
003900990427      * 36 - ERRORE numero C.A.
004000990427      * 37 -
004100990427      * 38 -
004200990427      * 39 -
004300990428      ***  Video 2
004400990428      * 40 - ERRORE tipo gestione
004500990430      * 41 - ERRORE beneficiario
004600080307      * 42 - ERRORE rimborso a carico       NON LO FACCIO PIU'
004700990430      * 43 - ERRORE beneficiario alternativo
004800990430      * 44 - ERRORE interlocutore
004900990430      * 45 - ERRORE importo concordato
005000990430      * 46 -
005100990430      * 47 -
005200990427      * 48 -
005300990427      * 49 -
005400990427      * 50 -
005500990427      ***  Video per inserimento beneficiario alternativo
005600990427      * 60 - ERRORE ragione sociale
005700990427      * 61 - ERRORE indirizzo
005800990427      * 62 - ERRORE C.A.P.
005900990427      * 63 - ERRORE località
006000990427      * 64 - ERRORE provincia
006100990427      * 65 - ERRORE nazione
006200990427      * 66 - ERRORE fax
006300990427      * 67 - ERRORE telefono
006400990427      * 68 - ERRORE Partita IVA
006500990430      * 69 -
006600990426      ***************************************************************
006700990426
006800990426     H DECEDIT('0,') DATEDIT(*DMY.)
006900990426      * ---------------------------------------------------------
007000990426     fFIDN32D   CF   E             WORKSTN
007100990426     fTABEL00F  IF   E           K DISK
007200990426     fFNDCT01L  UF A E           K DISK
007300990426     fFNDCS01L  UF A E           K DISK
007400990426     fFNDET01L  IF   E           K DISK
007500051117     fFNDPT01L  IF   E           K DISK
007600990426     fFNDCF01L  IF A E           k DISK
007700990426     fFNDCL01L  UF A E           k DISK
007800990426     fFNDKA01L  UF A E           k DISK
007900040802     fFNDCK01L  IF   E           k DISK
008000990426     fAZORG01L  IF   E           K DISK
008100990426      * filiale
008200991116     fFIAR601L  IF   E           K DISK     usropn
008300990426     fFNBLP01L  IF   E           K DISK     usropn
008400990426     fFNARB01L  IF   E           K DISK     usropn
008500990426      * sede
008600990809     fTITAS30C  IF   E           K DISK     usropn
008700990426     fTNTMD01L  IF   E           K DISK     usropn
008800990426      * ---------------------------------------------------------
008900010910     D*** MSG             S             78    DIM(39) CTDATA PERRCD(1)             MSG VIDEO
009000050720     D MSG             S             78    DIM(41) CTDATA PERRCD(1)             MSG VIDEO
009100990426     D L6              S              3  0 DIM(30)                              P.O.GESTITI
009200990503      * ---------------------------------------------------------
009300990503     D FIDN00DS      E DS
009400990503     D FIDN05DS      E DS
009500990426     D FIDN10DS      E DS
009600990426     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
009700990426     D TIBS02DS      E DS
009800990428     d TIBS69DS      E DS
009900990426     d DS_cnaco      E DS                  extname(CNACO00F)
010000990426     d DS_cnind      E DS                  extname(CNIND00F)
010100990426     d DS_cnclp      E DS                  extname(CNCLP00F)
010200990426     d DS_fncls      E DS                  extname(FNCLS00F)
010300990426     d TISI95DS      E DS
010400990426     d TRUL42DS      E DS
010500990426     d FNLV13DS      E DS
010600990426     d FNLV14DS      E DS
010700990430     d FNLV50DS      E DS
010800990426     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010900990426     d  tcu                  398    697    DIM(50)
011000990426     d  kcu                  698    847P 0 DIM(50)  PACKEVEN
011100990426     D TRUL06DS      E DS
011200990426     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
011300990426     D CNCR80        E DS
011400990426     D KPJBA         E DS
011500020502     D* rem                    1      3
011600020502     D* remfil                 4      6
011700990426     D DTAD          E DS
011800010910     D DGEDDN        E DS                  inz
011900990426     D DSTD          E DS
012000990426     D DSTB          E DS
012100990426     D DSCV          E DS
012200990426     D DDCT01        E DS
012300020201     D DDCF01        E DS
012400010905     D YEURCODS      E DS                  inz
012500010905     D   YECTAR      E                     inz('H')
012600011017     D XDECDS        E DS                  inz
012700011017     D   OCDESI      E                     inz(*OFF)
012800990426
012900990428     d                 ds
013000990428     d  DS_numca               1     14
013100990426     d  dctaac                 1      4  0
013200990426     d  dctfil                 5      7  0
013300990426     d  dctnca                 8     14  0
013400990426
013500990426     d                 ds
013600990426     d  DS_dcdkey              1     14
013700990426     d  dcdaac                 1      4  0
013800990426     d  dcdfil                 5      7  0
013900990426     d  dcdnca                 8     14  0
014000990426
014100990426     D WLBDAT          DS                  INZ
014200990426     D  G02DAT                 1      8  0
014300990426     D  G02INV                 9     16  0
014400990426     D  G02ERR                17     17
014500990426     D  G02TGI                18     22  0
014600990426
014700990426     d TCUDS           DS
014800990426     d  f1                     1      1
014900990426     d  f3                     3      3
015000990426     d  f2                     2      2
015100990426     d  f4                     4      4
015200990426     d  f56                    5      6
015300990426
015400990426     D                SDS
015500990426     D  VTCPGM                 1     10
015600990426
015700990510     D TIT             C                   CONST('*   MODIFICA TIPO GESTIONE C.-
015800990510     D                                     A.  *')
015900020130      *
016000020205     d Comm_Fra_I      c                   const('Ins. Automatico Pratica Franc-
016100020205     d                                     higia ')
016200020205     d Comm_Fra_E      c                   const('Elim. Automatico Pratica Fran-
016300020205     d                                     chigia')
016400990426
016500990427     d Datasped        S               D   DATFMT(*eur)
016600990503     D Dataca          S                   LIKE(I10dta)
016700990503     D Wlnp            S                   LIKE(ARBlnp)
016800020130     D Wfra            S                   LIKE(§DCTpfra)
016900990504     D £V2Cipr         S                   LIKE(DCLipr)
017000990504     D £DCLipr         S                   LIKE(DCLipr)
017100990427     D wV2cptr         S                   LIKE(V2Cptr)
017200990430     D WV2Dnoc         S                   LIKE(V2Dnoc)
017300990430     D WV2Dksc         S                   LIKE(V2Dksc)
017400990601     D W1V2Dksc        S                   LIKE(V2Dksc)
017500990430     D Wv2cksc         s                   LIKE(V2Cksc)
017600990504     d WV2cfpr         s                   LIKE(V2Cfpr)
017700990426     D Wtrc            S                   LIKE(DKAtrc)
017800990426     D Kcod            S                   LIKE(TBLcod)
017900990426     D Kkey            S                   LIKE(TBLkey)
018000990426     D §key            S                   LIKE(TBLkey)
018100990426     D ktpd            s                   LIKE(DCStpd)
018200990426     D knkt            s                   LIKE(DCSnkt)
018300990426     D kstd            s                   LIKE(DCSstd)
018400990426     D wstd            s                   LIKE(DCSstd)
018500990426     D knks            s                   LIKE(DCSnks)
018600990426     D ktrc            s                   LIKE(DCStrc)
018700990426     D kdim            s                   LIKE(DCSdim)
018800990426     D khim            s                   LIKE(DCShim)
018900990428     D wsvcap          s                   LIKE(W1Ccap)
019000990428     D wsvloc          s                   LIKE(W1Cloc)
019100990428     D wsvprv          s                   LIKE(W1Cprv)
019200990426     D wmsg            s                   LIKE(O13msg)
019300990426     D wfzv            s                   LIKE(E13fzv)
019400990426     D wfz1            s                   LIKE(E13fz1)
019500990426     D wfz2            s                   LIKE(E13fz2)
019600990426     D wfz3            s                   LIKE(E13fz3)
019700990426     D paxccc          s                   LIKE(ACOkcc)
019800990426     D paxdmt          s                   LIKE(ACOrag)
019900990426     D wvlt            s                   LIKE(DCTvpv)
020000990427     d Wfpr            s                   LIKE(V2Cfpr)
020100990426     D W§dctport       S                   LIKE(§DCTport)
020200991116     D trcar6          S                   LIKE(ar6trc)
020300040802
020400040802     d keyaas          s                   like(dctaas)
020500040802     d keylnp          s                   like(dctlnp)
020600040802     d keynrs          s                   like(dctnrs)
020700040802     d keynsp          s                   like(dctnsp)
020800990426
020900990504     d WFcon           s                   LIKE(DCTfca)   inz(100)
021000990430     d WFfff           s                   LIKE(DCTfca)   inz(110)
021100990430     d WFffs           s                   LIKE(DCTfca)   inz(225)
021200990430     d WFclt           s                   LIKE(DCTfca)   inz(230)
021300990428     d WFidc           s                   LIKE(DCTfca)   inz(660)
021400990426
021500990503     d Wtpb            s              1
021600990503     d Wrivalsa        s              1
021700990430     d WF18frz         s              1
021800990430     d Wmsg18frz       s              1
021900990430     d Wmsg24frz       s              1
022000990430     d Wmsg30frz       s              1
022100990503     d Wmsg36frz       s              1
022200990426     D wfndcl          s              1
022300990430     D wfndcs          s              1
022400990504     d Wbenalt         s              1
022500990426     D §des            S             30
022600990426     D wtibs69r        s              1
022700990426     D werr            s              1
022800990426     D PAXdut          s             30
022900990426     D PAXdit          s              3
023000990426     D PAXflr          s             90
023100990504     D PAXsta          s              1  0
023200010910     D w002a           s              2
023300990426     D w0020           s              2  0
023400990426     D w0030           s              3  0
023500990426     D w0033           s              3  3
023600990426     D w0040           s              4  0
023700990426     D w0133           s             13  3
023800990426     D w0140           s             14  0
023900990426     D XX              s              2  0
024000990426     D wdtgio          s              8  0
024100990430     D wdatasp         s              8  0
024200011126     D wdataca         s               D   datfmt(*eur)
024300990426     D dateu           s              8  0
024400051117     D wmand           s              1
024500990426      * ---------------------------------------------------------
024600990426     IFNBLP000
024700990426     I              BLPATB                      ARBATB
024800990426     I              BLPAAS                      ARBAAS
024900990426     I              BLPLNP                      ARBLNP
025000990426     I              BLPNRS                      ARBNRS
025100990426     I              BLPNSP                      ARBNSP
025200990426     I              BLPCBO                      ARBCBO
025300990426     I              BLPMGS                      ARBMGS
025400990426     I              BLPFNS                      ARBFNS
025500990426     I              BLPSCL                      ARBSCL
025600990426     I              BLPFBR                      ARBFBR
025700990426     I              BLPFST                      WDDT
025800990426     I              BLPFDN                      ARBFDN
025900990426     I              BLPLNA                      ARBLNA
026000990426     I              BLPZNC                      ARBZNC
026100990426     I              BLPKSC                      ARBKSC
026200990426     I              BLPRSM                      ARBRSM
026300990426     I              BLPINM                      ARBINM
026400990426     I              BLPCAM                      ARBCAM
026500990426     I              BLPLOM                      ARBLOM
026600990426     I              BLPNZM                      ARBNZM
026700990426     I              BLPPRM                      ARBPRM
026800990426     I              BLPFAP                      ARBFAP
026900990426     I              BLPCPI                      ARBCPI
027000990426     I              BLPRSD                      ARBRSD
027100990426     I              BLPIND                      ARBIND
027200990426     I              BLPCAD                      ARBCAD
027300990426     I              BLPLOD                      ARBLOD
027400990426     I              BLPNZD                      ARBNZD
027500990426     I              BLPPRD                      ARBPRD
027600990426     I              BLPFIN                      ARBFIN
027700990426     I              BLPGC1                      ARBGC1
027800990426     I              BLPGC2                      ARBGC2
027900990426     I              BLPCTR                      ARBCTR
028000990426     I              BLPCTS                      ARBCTS
028100990426     I              BLPFTM                      ARBFTM
028200990426     I              BLPTSP                      ARBTSP
028300990426     I              BLPNAS                      ARBNAS
028400990426     I              BLPNCL                      ARBNCL
028500990426     I              BLPNCP                      ARBNCP
028600990426     I              BLPPKC                      ARBPKC
028700990426     I              BLPPKB                      ARBPKB
028800990426     I              BLPPKF                      ARBPKF
028900990426     I              BLPNCR                      ARBNCR
029000990426     I              BLPVLC                      ARBVLC
029100990426     I              BLPFVB                      ARBFVB
029200990426     I              BLPVLB                      ARBVLB
029300990426     I              BLPFVF                      ARBFVF
029400990426     I              BLPVLF                      ARBVLF
029500990426     I              BLPQFT                      ARBQFT
029600990426     I              BLPVMD                      ARBVMD
029700990426     I              BLPVAD                      ARBVAD
029800990426     I              BLPIAS                      ARBIAS
029900990426     I              BLPVAS                      ARBVAS
030000990426     I              BLPCCM                      ARBCCM
030100990426     I              BLPRMN                      ARBRMN
030200990426     I              BLPRMA                      ARBRMA
030300990426     I              BLPRMO                      ARBRMO
030400990426     I              BLPFLS                      ARBFLS
030500990426     I              BLPNCD                      ARBNCD
030600990426     I              BLPNCA                      ARBNCA
030700990426     I              BLPXCO                      ARBXCO
030800990426     I              BLPCTM                      ARBCTM
030900990426     I              BLPFFD                      ARBFFD
031000990426     I              BLPTCR                      ARBTCR
031100990426     I              BLPDCR                      ARBDCR
031200990426     I              BLPHCR                      ARBHCR
031300990426     I              BLPDTI                      ARBDTI
031400990426     I              BLPHTI                      ARBHTI
031500990426     I              BLPDCE                      ARBDCE
031600990426     I              BLPNPI                      ARBNPI
031700990426     I              BLPNCI                      ARBNCI
031800990426     I              BLPNRC                      ARBNRC
031900990426     I              BLPDBR                      ARBDBR
032000990426     I              BLPNFV                      ARBNFV
032100990426     I              BLPDPC                      ARBDPC
032200990426     I              BLPDUC                      ARBDUC
032300990426     I              BLPFLP                      ARBFLP
032400990426     I              BLPDET                      ARBDET
032500990426     I              BLPDUT                      ARBDUT
032600990426     I              BLPDAM                      ARBDAM
032700990426     I              BLPNDC                      ARBNDC
032800990426     I              BLPDDC                      ARBDDC
032900990426     I              BLPDCP                      ARBDCP
033000990426     I              BLPDCM                      ARBDCM
033100990426     I              BLPHMC                      ARBHMC
033200990426     I              BLPTC1                      ARBTC1
033300990426     I              BLPTC2                      ARBTC2
033400990426     I              BLPCCA                      ARBCCA
033500990426     I              BLPTMC                      ARBTMC
033600990426     I              BLPGGA                      ARBGGA
033700990426     I              BLPGMA                      ARBGMA
033800990426     I              BLPGVA                      ARBGVA
033900990426     I              BLPFLE                      ARBFLE
034000990426     I              BLPTFP                      ARBTFP
034100990426     I              BLPFEA                      ARBFEA
034200990426     I              BLPTFA                      ARBTFA
034300990426     I              BLPFT1                      ARBFT1
034400990426     I              BLPDT1                      ARBDT1
034500990426     I              BLPFT2                      ARBFT2
034600990426     I              BLPDT2                      ARBDT2
034700990426     I              BLPFT3                      ARBFT3
034800990426     I              BLPDT3                      ARBDT3
034900990426
035000990809     ITITAS000
035100990426     I              TASAAS                      ARBAAS
035200990426     I              TASLNP                      ARBLNP
035300990426     I              TASNRS                      ARBNRS
035400990426     I              TASNSP                      ARBNSP
035500990426     I              TASMGS                      ARBMGS
035600990426     I              TASNCL                      ARBNCL
035700990426     I              TASKSC                      ARBKSC
035800990426     I              TASCCM                      ARBCCM
035900990809     I              TASPKf                      ARBPKF
036000990426     I              TASFVF                      ARBFVF
036100990426     I              TASVLF                      ARBVLF
036200990426     I              TASLNA                      ARBLNA
036300990426     I              TASIAS                      ARBIAS
036400990426     I              TASVAS                      ARBVAS
036500990426     I              TASTSP                      ARBTSP
036600990426     I              TASCTR                      ARBCTR
036700990426     I              TASTFP                      ARBTFP
036800990426     I              TASTFA                      ARBTFA
036900990426     I              TASLL1                      WDDT
037000990426     I              TASCCA                      ARBCCA
037100990426
037200990809     ITITAS010
037300990426     I              TASAAS                      ARBAAS
037400990426     I              TASLNP                      ARBLNP
037500990426     I              TASNRS                      ARBNRS
037600990426     I              TASNSP                      ARBNSP
037700990426     I              TASMGS                      ARBMGS
037800990426     I              TASNCL                      ARBNCL
037900990426     I              TASKSC                      ARBKSC
038000990426     I              TASCCM                      ARBCCM
038100990809     I              TASPKf                      ARBPKF
038200990426     I              TASFVF                      ARBFVF
038300990426     I              TASVLF                      ARBVLF
038400990426     I              TASLNA                      ARBLNA
038500990426     I              TASIAS                      ARBIAS
038600990426     I              TASVAS                      ARBVAS
038700990426     I              TASTSP                      ARBTSP
038800990426     I              TASCTR                      ARBCTR
038900990426     I              TASTFP                      ARBTFP
039000990426     I              TASTFA                      ARBTFA
039100990426     I              TASLL1                      WDDT
039200990426     I              TASCCA                      ARBCCA
039300990426
039400990809     ITITASP00
039500990426     I              TASAAS                      ARBAAS
039600990426     I              TASLNP                      ARBLNP
039700990426     I              TASNRS                      ARBNRS
039800990426     I              TASNSP                      ARBNSP
039900990426     I              TASMGS                      ARBMGS
040000990426     I              TASNCL                      ARBNCL
040100990426     I              TASKSC                      ARBKSC
040200990426     I              TASCCM                      ARBCCM
040300990809     I              TASPKf                      ARBPKF
040400990426     I              TASFVF                      ARBFVF
040500990426     I              TASVLF                      ARBVLF
040600990426     I              TASLNA                      ARBLNA
040700990426     I              TASIAS                      ARBIAS
040800990426     I              TASVAS                      ARBVAS
040900990426     I              TASTSP                      ARBTSP
041000990426     I              TASCTR                      ARBCTR
041100990426     I              TASTFP                      ARBTFP
041200990426     I              TASTFA                      ARBTFA
041300990426     I              TASLL1                      WDDT
041400990426     I              TASCCA                      ARBCCA
041500990426
041600990426     ITNTMD000
041700990426     I              TMDAAS                      ARBAAS
041800990426     I              TMDLNP                      ARBLNP
041900990426     I              TMDNRS                      ARBNRS
042000990426     I              TMDNSP                      ARBNSP
042100990426     I              TMDRSM                      ARBRSM
042200990426     I              TMDINM                      ARBINM
042300990426     I              TMDCAM                      ARBCAM
042400990426     I              TMDLOM                      ARBLOM
042500990426     I              TMDPRM                      ARBPRM
042600990426     I              TMDNZM                      ARBNZM
042700990426     I              TMDRSD                      ARBRSD
042800990426     I              TMDIND                      ARBIND
042900990426     I              TMDCAD                      ARBCAD
043000990426     I              TMDLOD                      ARBLOD
043100990426     I              TMDPRD                      ARBPRD
043200990426     I              TMDNZD                      ARBNZD
043300990426      * ---------------------------------------------------------
043400990427
043500990427      *            G E S T I O N E   V I D E O  1
043600990427     C     EMD01         TAG
043700990427      *
043800990427     C                   WRITE     FI32T01
043900990427     C                   WRITE     FI32Z01
044000990427     C                   EXFMT     FI32D01
044100990427      *
044200990427     C                   CLEAR                   V1CMSG
044300990427     c                   eval      *in29 = *off
044400990427     c                   eval      *in28 = *off
044500990428     c     35            DO        39            XX
044600990427     c                   eval      *IN(XX) = *OFF
044700990427     c                   ENDDO
044800990427
044900990428      * F3  = FINE
045000990427     C                   IF        *INKC = *ON
045100990427     C                   goto      FINE
045200990427     C                   ENDIF
045300990427
045400990428      * F18 = Cambio P.O. in gestione
045500990427     c                   IF        *INKS = *ON
045600990427     c                   eval      *in05 = *on
045700990427     c                   goto      emd01
045800990427     C                   ENDIF
045900990427
046000990428      *     05 on --> controllo P.O. in gestione
046100990427     c                   IF        *in05 = *on
046200990427     C                   exsr      ctrfgs
046300990427     c   28              goto      emd01
046400990427     c                   eval      *in05 = *off
046500990427     C                   ENDIF
046600990427
046700990427     c                   EXSR      CTRD01
046800990427
046900990427     C                   IF        *IN28 = *ON  or  *IN29 = *ON
047000990427     C                   goto      EMD01
047100990427     C                   ENDIF
047200990426
047300990428      * Riempio testata  video 2
047400990426     C                   EXSR      RIETES
047500990426      *
047600990428      * Riempio dettaglio video 2
047700990426     C                   EXSR      RIEDET
047800990426
047900990427      *            G E S T I O N E   V I D E O  2
048000990427     C     EMD02         TAG
048100990426      *
048200990426      * Attivo F8
048300990504     c                   eval      *IN03 = (Wbenalt = 'S')
048400990426      *
048500990427     C                   WRITE     FI32T02
048600990427     C                   WRITE     FI32Z02
048700990427     C                   EXFMT     FI32D02
048800990426      *
048900990427     C                   CLEAR                   V2CMSG
049000990426     c                   eval      *in29 = *off
049100990426     c                   eval      *in28 = *off
049200990430     c     40            DO        50            XX
049300990428     c                   eval      *IN(XX) = *OFF
049400990428     c                   ENDDO
049500990426
049600990426      * F3 = FINE
049700990426     C                   IF        *INKC = *ON
049800990426     C                   goto      FINE
049900990426     C                   ENDIF
050000990426
050100990426      * F12 = RITORNO
050200990426     C                   IF        *INKL = *ON
050300990503     c                   clear                   fi32d02
050400990428     C                   goto      EMD01
050500990426     C                   ENDIF
050600990426
050700990503      * F7 = VISUALIZZA C.A.
050800990503     C                   IF        *INKG = *ON
050900990503     c                   exsr      SUB_F7
051000990503     C                   goto      EMD02
051100990503     c                   ENDIF
051200990503
051300990426      * F18 = NOTE
051400990428     C                   IF        *INKS = *ON
051500990430     c                   eval      WF18frz = 'S'
051600990426     c                   exsr      SUB_F18
051700990428     C                   goto      EMD02
051800990426     c                   ENDIF
051900990426
052000990428      * Controlli video 2
052100990428     C                   EXSR      CTRD02
052200990426      *
052300990426     C                   if        *IN28 = *ON  or  *IN29 = *ON
052400990428     C                   goto      EMD02
052500990426     C                   endif
052600990426
052700990426      * F08 = Manutenzione anagrafica Beneficiario alternativo
052800990428     c                   IF        *INKH = *ON
052900990426     c                   movel     'C'           Wtrc
053000990428     c                   exsr      mancli
053100990428     C                   goto      emd02
053200990428     C                   ENDIF
053300990426
053400990503     C                   IF        *INKF = *OFF
053500990430     c                   goto      EMD02
053600990430     C                   ENDIF
053700990430      * Aggiorno files
053800990430     c                   exsr      AGGIORNA
053900011126      * Obbligo l'inserimento delle note
054000011126     c                   IF        WF18frz = *blanks
054100011126     c                   exsr      SUB_F18
054200011126     C                   ENDIF
054300990430
054400990426      *------------------------
054500990426     C     FINE          TAG
054600990426      *------------------------
054700990426
054800990426      * Chiudo pgm di servizo
054900990426     c                   clear                   FIDN10DS
055000990426     c                   movel     'C'           i10tla
055100990426     c                   movel     FIDN10DS      kpjbu
055200990426     c                   call      'FIDN10R'
055300990426     c                   parm                    kpjba
055400990426      *
055500990426     C                   IF        Wtibs69r  = 'S'
055600990428     C                   clear                   TIBS69DS
055700990426     C                   EVAL      I69TLA  = 'C'
055800990426     C                   CALL      'TIBS69R'
055900990426     C                   PARM                    TIBS69DS
056000990426     C                   ENDIF
056100990426      *
056200990426     c                   clear                   tibs02ds
056300990426     c                   movel     'C'           t02tla
056400990426     c                   call      'TIBS02R'
056500990426     c                   parm                    kpjba
056600990426     c                   parm                    tibs02ds
056700990426      *
056800990426     C                   EVAL      *INLR = *ON
056900990427      **********************************************************************
057000990427      * CONTROLLO IL P.O. IN IN GESTIONE
057100990427      **********************************************************************
057200990427     C     CTRFGS        BEGSR
057300990427      *
057400990427     C                   eval      *in35 = *off
057500990427     c                   clear                   v1dfgs
057600990427     C                   clear                   FNLV50DS
057700990427     C                   MOVEL     KNMUS         D50PRU
057800990427     C                   MOVEL     V1cfgs        D50FGS
057900990427     C                   CALL      'FNLV50R'
058000990427     C                   PARM                    FNLV50DS
058100990427      *
058200990427     C                   IF        D50ERR <> *blanks
058300990427     C                   MOVEL     D50MSG        V1cMSG
058400990427     C                   eval      *in35 = *on
058500990427     C                   eval      *in28 = *on
058600990427     C                   ELSE
058700990427      *
058800990427     C                   EXSR      CARL6
058900990427     c     v1cfgs        CHAIN     azorg                              31
059000990427     c                   IF        *IN31 = *OFF
059100990427     C                   movel     orgDES        V1Dfgs
059200990428     C                   ENDIF
059300990427      *
059400990428     C                   ENDIF
059500990427      *
059600990427     c                   endsr
059700990427      **********************************************************************
059800990427      *  CONTROLLI VIDEATA 1
059900990427      **********************************************************************
060000990427     C     CTRD01        BEGSR
060100990427
060200990427      * SISTEMO ANNO DI DUE CIFRE
060300990427     c                   IF        V1Caac > *zeros  and  V1Caac < 100
060400990427     C                   if        V1Caac > 60
060500990427     C                   ADD       1900          V1Caac
060600990427     C                   else
060700990427     C                   ADD       2000          V1caac
060800990427     C                   endif
060900990427     c                   ENDIF
061000990427
061100990427      * CONTROLLO SE SELEZIONATI DATI VALIDI
061200990427     c                   IF        V1Cfil = *zeros  or  V1Cnca = *zeros
061300990427     c                                              or  V1Caac = *zeros
061400990427     c                   movel     msg(1)        V1Cmsg
061500990427     c                   eval      *in36 = *on
061600990427     c                   eval      *in28 = *on
061700990427     c                   ENDIF
061800990427     c   28              goto      ECTRD01
061900990427
062000990428      * CONTROLLO SE SELEZIONATA C.A. VALIDA
062100990430     c     kca           CHAIN     fndct000                           30
062200990427      *
062300990427     c                   SELECT
062400990427      *
062500990427     c                   WHEN      *IN30 = *ON
062600990427     c                   movel     msg(2)        V1Cmsg
062700990427      *
062800990427     c                   WHEN      DCTatb = 'A'
062900990427     c                   movel     msg(3)        V1Cmsg
063000990427      *
063100990427     c                   WHEN      DCTdch > *zeros
063200990427     c                   movel     msg(4)        V1Cmsg
063300990427      *
063400990427     c                   OTHER
063500990427     C                   if        DCTgfc > *zeros
063600990427     C     DCTgfc        LOOKUP    L6                                     31
063700990503     c  N31              movel     msg(5)        V1Cmsg
063800990427     C                   endif
063900990427      *
064000990427     c                   ENDSL
064100990427      *
064200990427     c                   IF        V1Cmsg <> *blanks
064300990427     c                   eval      *in36 = *on
064400990427     c                   eval      *in28 = *on
064500990427     c                   goto      ECTRD01
064600990427     c                   ENDIF
064700040802      *
064800040802      * se CA con legami e linea di arrivo delle CA presente nella l6 del P.O. gestione
064900040802      * recupero il numero spedizione da FNDCK
065000040802      *
065100040802     c                   eval      Keyaas = dctaas
065200040802     c                   eval      Keylnp = dctlnp
065300040802     c                   eval      Keynrs = dctnrs
065400040802     c                   eval      keynsp = dctnsp
065500040802      *
065600040802     c                   movel     dctflo        ddct01
065700040802     c                   If        §dctlega = 'S'
065800040802     C     DCTlna        LOOKUP    L6                                     31
065900040802     c                   If        *in31
066000040802     c     kca           chain     fndck01l
066100040802     c                   if        %found(fndck01l)
066200040802      * valorizzo la spedizione
066300040802     c                   eval      Keyaas = dckaas
066400040802     c                   eval      Keylnp = dcklnp
066500040802     c                   eval      Keynrs = dcknrs
066600040802     c                   eval      keynsp = dcknsp
066700040802     c                   endif
066800040802     c                   endif
066900040802     c                   endif
067000990427
067100990428      * AGGANCIO SPEDIZIONE
067200990427     c                   IF         SIMFEL = *zeros                             SEDE
067300990430     c                   eval       Wtpb = 'S'
067400990427     c     kbol01        chain(N)  tntmd000                           30
067500990809     c     kbol01        chain     TITAS30c                           30
067600990427      *  se tipo bolla non è valido cerco altro rcd
067700990427     C                   movel     'TB'          kCOD
067800990427     C                   movel(P)  TAStbl        kKEY
067900990427     C     ktab          chain(N)  TABEL                              31
068000990427     C                   movel     TBLUNI        DSTB
068100990427     c                   if        §TBrbl = 'R'
068200990809     c     kbol01        reade     TITAS30c                               30
068300990427     c                   endif
068400990427      *
068500990427     c                   ELSE
068600990427     c     kbol01        chain(N)  fnarb000                           30
068700990430     c                   If         *IN30 = *OFF
068800990430     c                   eval       Wtpb = 'A'
068900990430     c                   Else
069000990427     c     kbol01        chain(N)  fnblp000                           30
069100990430     c  N30              eval       Wtpb = 'P'
069200990427     c                   Endif
069300990427     c                   ENDIF
069400990427      *
069500990428     c                   IF          *IN30 = *off
069600990430     c                   z-add     arbmgs        WDATASP
069700990430     c                   movel     arbaas        WDATASP
069800990430     c     *iso          move      Wdatasp       datasped
069900990428     c                   ELSE
070000990428     c                   movel     msg(6)        V1Cmsg
070100990428     c                   eval      *in36 = *on
070200990428     c                   eval      *in28 = *on
070300990428     c                   goto      ECTRD01
070400990428     c                   ENDIF
070500990428
070600990428      * CONTROLLO SE C.A. ABILITATA ALLE MODIFICHE
070700990428     c                   SELECT
070800990504     c                   WHEN      DCTfca < WFCON
070900990504     c                   movel     msg(7)        V1Cmsg
071000990504     c                   WHEN      DCTfpr = 'P'  and  DCTfca >= WFIDC
071100990428     c                   movel     msg(15)       V1Cmsg
071200990428     c                   WHEN      DCTfpr = 'T'  and  DCTfca >= WFCLT
071300990428     c                   movel     msg(16)       V1Cmsg
071400990428     c                   WHEN      DCTfpr = 'T'  and  SIMFEL <> *zeros
071500990428     c                   movel     msg(17)       V1Cmsg
071600990428     c                   ENDSL
071700990428      *
071800990428     c                   IF        V1Cmsg <> *blanks
071900990428     c                   eval      *in36 = *on
072000990428     c                   eval      *in28 = *on
072100990428     c                   goto      ECTRD01
072200990428     c                   ENDIF
072300990427
072400990427     c     ECTRD01       ENDSR
072500990426      ****************************************************************
072600990427      *  CARICAMENTO DATI DI TESTATA
072700990426      ****************************************************************
072800990426     C     RIETES        BEGSR
072900990426
073000040802     c**""**             movel     dctflo        DDCT01
073100990426     c                   movel     §DCTlnpc      Wlnp
073200020130     c                   movel     §DCTpfra      Wfra
073300990427      *
073400990427     c                   z-add     V1Cfil        vtcfil
073500990427     c                   z-add     V1Cnca        vtcnca
073600990503     c                   z-add     DCTmgc        dataca
073700990503     c                   movel     DCTaac        dataca
073800011126     c     *ISO          movel     dataca        Wdataca
073900011126     c                   move      Wdataca       VTCdc1
074000011126     c****               move      dataca        VTCdc1
074100990426
074200990426     c                   move      datasped      VTCdsp
074300040802      *
074400040802     C**""**             move      DCTlnp        VTClnp
074500040802     C**""**             move      DCTnrs        VTCnrs
074600040802     C**""**             move      DCTnsp        VTCnsp
074700040802      *
074800040802     C                   move      Keylnp        VTClnp
074900040802     C                   move      Keynrs        VTCnrs
075000040802     C                   move      Keynsp        VTCnsp
075100990426
075200990427     C                   move      DCTlna        VTClna
075300990427     C     DCTlna        chain(N)  AZORG                              31
075400990426     C  N31              If        ORGDE5 <> *BLANKS
075500990426     C                   movel     ORGDE5        VTDlna
075600990426     C                   Else
075700990426     C                   movel     ORGDES        VTDlna
075800990426     C                   Endif
075900990426
076000990426      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con quello C.A. tranne se la
076100990426      *  sped. è "Mamma di cambio porto", in questo caso inverto il porto C.A.
076200990426     c                   SELECT
076300110503     c****               WHEN      ARBcca <> '9'
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
093500990427     c                   movel     DCTpgd        V2Cpgd                         Chi paga
093600990503
093700990503      * CONTROLLO SE HO RIVALSA
093800990503     c                   movel     'R'           Wtrc
093900990503     c     KDKA          CHAIN(N)  FNDKA000                           30
094000990909     c                   if        *in30 = *off  and  DKAatb = *blanks
094100990909     c                   eval      Wrivalsa = 'S'
094200990909     c                   endif
094300990427
094400990427      * BENEFICIARIO ALTERNATIVO
094500990427     c                   IF        DCTKSC > *ZEROS
094600990427     c                   z-add     dctksc        V2cksc
094700990427     c                   z-add     dctksc        WV2cksc
094800990427     c                   move      V2cksc        w0040
094900990427      *   se codificato decodifico da p.d.c.
095000990427     C                   IF        w0040 <> 8888
095100990427     c                   clear                   ACOrag
095200990428     C                   clear                   TIBS69DS
095300990428     c                   z-add     DCTksc        I69kac
095400990427     c                   exsr      CTR_anagra
095500990427     c  N30              movel     ACOrag        V2Dksc
095600990427     c                   ELSE
095700990427      *
095800990427      *   se non codificato decodifico da anagrafica clienti c.a.
095900990504     c                   movel     'S'           Wbenalt
096000990427     c                   clear                   FNDKA000
096100990427     c                   movel     'C'           Wtrc
096200990427     c     KDKA          CHAIN(N)  FNDKA000                           30
096300990427     c                   movel     dkarag        V2dksc
096400990427      *          imposto campi per video
096500990428     c                   movel     DKArag        W1Crag
096600990428     c                   movel     DKAvia        W1Cvia
096700990428     c                   movel     DKAcap        W1Ccap
096800990428     c                   movel     DKAloc        W1Cloc
096900990428     c                   movel     DKAprv        W1Cprv
097000990428     c                   movel     DKAnaz        W1Cnaz
097100990428     c                   eval      W1Civa = %subst(DKAiva:1:14)
097200990428     c                   eval      W1Cive = %subst(DKAiva:15:2)
097300990428     c                   movel     DKAtel        W1Ctel
097400990428     c                   movel     DKAfax        W1Cfax
097500990427     c                   ENDIF
097600990601      *
097700990601     c                   ELSE
097800990601     c                   clear                   W1Crag
097900990601     c                   clear                   W1Cvia
098000990601     c                   clear                   W1Ccap
098100990601     c                   clear                   W1Cloc
098200990601     c                   clear                   W1Cprv
098300990601     c                   clear                   W1Cnaz
098400990601     c                   clear                   W1Civa
098500990601     c                   clear                   W1Cive
098600990601     c                   clear                   W1Ctel
098700990601     c                   clear                   W1Cfax
098800990427     c                   ENDIF
098900990601     c                   movel     V2Dksc        WV2dksc
099000990427
099100990427      * INTERLOCUTORE
099200990427     c                   movel     'C'           ktpd
099300990427     c                   movel(P)  DS_numca      knkt
099400990427     c                   movel     'N'           kstd
099500990427     c                   clear                   knks
099600990427     c                   movel     'D'           ktrc
099700010912
099800990430     c     kdcs          setll     fndcs01l
099900011126     c     kdcs          reade     fndcs01l                               30
100000011126      *
100100990430     c     *in30         doweq     *off
100200990430     c     dcsatb        andne     *blanks
100300011126     c     kdcs          reade     fndcs01l                               30
100400990430     c                   enddo
100500990430      *
100600990430     c                   IF        *in30 = *off
100700990430     c                   eval      wfndcs = 'S'
100800990430     c                   movel     dcsnot        V2Dnoc
100900990430     c                   movel     dcsnot        WV2Dnoc
101000001010     c                   else
101100001010     c                   clear                   wfndcs
101200990430     c                   ENDIF
101300990427
101400990427      * IMPORTO MASSIMO RISARCIBILE
101500990427     c                   IF          DCTipv > *zeros
101600010912     C***                   z-add     DCTipv        W0133
101700010912     C***                   movel     DCTvpv        WVLT
101800010912     C***                   exsr      ctrvlt
101900010912     C***                   If        *IN28 = *OFF  and  *IN29 = *OFF
102000990427      *     Calcolo controvalore
102100010905     c***                   IF        DCTvpv = *blanks
102200010910     c                   IF        DCTvpv = §gedDBA
102300010906     c                   z-add     DCTipv        V2Cipv
102400010906     c                   ELSE
102500010905     c***     §CVcmb        mult      DCTipv        V2Cipv
102600010906     c                   z-add     DCTipv        W0133
102700010912     C                   movel     DCTvpv        WVLT
102800010906     c                   exsr      srCMB
102900010906     c                   z-add     W0133         V2Cipv
103000010906     c                   ENDIF
103100010905     c***                   clear                   V2Cvpv                           Valuta
103200010912     c                   movel     §gedDBA       V2Cvpv                           Valuta
103300990427      *
103400010912     c***                   ENDIF
103500990427     c                   ENDIF
103600010911
103700010911      * Imposto la valuta per l'importo concordato
103800010911     C                   movel     §gedDBA       V2Cvlt
103900990426
104000990726      * IMPORTO CONCORDATO non considero se pratica
104100990426     c     kca           CHAIN     FNDCL000                           30
104200990430     c                   IF        *IN30 = *OFF
104300990504     c                   eval      Wfndcl = 'S'
104400990504      *
104500000721     c                   IF        DCLatb <> 'A'  and  DCTfpr <> 'P'
104600990504      *     Controllo la Valuta
104700990504     C                   movel     V2Cvlt        WVLT
104800990504     C                   z-add     V2Cipr        W0133
104900990504     C                   exsr      ctrvlt
105000990504
105100990504     C                   IF        *IN28 = *OFF  and  *IN29 = *OFF
105200010905      ****     Calcolo il controvalore il LIRE
105300010905     c***                   IF        V2Cvlt  = *blanks
105400010911      *     Calcolo il controvalore in divisa corrente
105500010910     c                   IF        V2Cvlt  = §gedDBA
105600010906     c                   z-add     DCLipr        V2Cipr
105700010906     c                   ELSE
105800010905     c***     §CVcmb        mult      DCLipr        V2Cipr
105900010906     c                   z-add     DCLipr        W0133
106000010906     c                   exsr      srCMB
106100010906     c                   z-add     W0133         V2Cipr
106200010906     c                   ENDIF
106300010905     c***                   clear                   V2Cvlt
106400010911     c                   movel     §gedDBA       V2Cvlt
106500010905     c                   z-add     V2Cipr        £DCLipr
106600990427     c                   ENDIF
106700990504      *
106800990430     c                   ENDIF
106900001010     c                   else
107000001010     c                   clear                   wfndcl
107100010912     c                   clear                   fndcl000
107200990504     c                   ENDIF
107300990426
107400990426     C                   ENDSR
107500990503      ****************************************************************
107600990503      * F7 = VISUALIZZA CA
107700990503      ****************************************************************
107800990503     C     SUB_F7        BEGSR
107900990503      *
108000990503     C                   clear                   FIDN00DS
108100990503     c                   z-add     DCTaac        I00aac
108200990503     c                   z-add     DCTfil        I00fil
108300990503     c                   z-add     DCTnca        I00nca
108400040802     c**""**             z-add     DCTaas        I00aas
108500040802     c**""**             z-add     DCTlnp        I00lnp
108600040802     c**""**             z-add     DCTnrs        I00nrs
108700040802     c**""**             z-add     DCTnsp        I00nsp
108800040802      *
108900040802     c                   z-add     Keyaas        I00aas
109000040802     c                   z-add     Keylnp        I00lnp
109100040802     c                   z-add     Keynrs        I00nrs
109200040802     c                   z-add     Keynsp        I00nsp
109300040802      *
109400990503     c                   z-add     DCTfca        I00fca
109500990503     c                   z-add     DCTgfc        I00fgs
109600990503     c                   movel     Wtpb          I00tpb
109700990503      *
109800990503     C                   movel(P)  FIDN00DS      KPJBU
109900990503     c                   call      'FIDN01R'
110000990503     c                   parm                    kpjba
110100990503      *
110200990503     C                   ENDSR
110300990426      ****************************************************************
110400990426      * F18 = GESTIONE NOTE
110500990426      ****************************************************************
110600990426     C     SUB_F18       BEGSR
110700990426      *
110800990426     C                   clear                   FIDN10DS
110900990427     c                   z-add     dataca        i10dta
111000990426     c                   movel     'N'           i10trc
111100990426     c                   movel     'C'           i10tpd
111200990426     c                   movel     'V'           i10flm
111300990426     c                   movel     dctfca        i10nks
111400990426     c                   movel     DS_numca      i10nkt
111500990426      *
111600990426     c                   movel(P)  FIDN10DS      kpjbu
111700990426     C                   CALL      'FIDN10R'
111800990426     C                   PARM                    kpjba
111900990426      *
112000990426     c                   ENDSR
112100990428      **********************************************************************
112200990428      *  CONTROLLI VIDEATA 2
112300990428      **********************************************************************
112400990428     C     CTRD02        BEGSR
112500990428
112600990503      * Azzero test per messaggio forzato
112700990503     c                   IF        V2Cptr <> WV2cptr
112800990503     c                   clear                   Wmsg18frz
112900990503     c                   clear                   Wmsg24frz
113000990503     c                   clear                   Wmsg30frz
113100990503     c                   movel     V2Cptr        WV2Cptr
113200990503     c                   ENDIF
113300990503      *
113400990503     c                   IF        V2Cfpr <> WV2Cfpr
113500990503     c                   clear                   Wmsg36frz
113600990503     c                   movel     V2Cfpr        WV2Cfpr
113700990503     c                   ENDIF
113800990503
113900990503
114000990428      * TIPO GESTIONE
114100990909      *      in filiale posso passare da 'P' a 'T' solo se non ho evento
114200990429      *      modifica possibile in 'T' se non ho importo trattenuto o rivalsa
114300990428     c                   SELECT
114400990429      *
114500990428     c                   WHEN      V2Cfpr = *blanks
114600990428     c                   movel     msg(8)        V2cmsg
114700990429      *
114800990429     c                   WHEN       V2Cfpr = 'P'
114900051117      * se sono in filiale non posso impostare LT se c'è evento
115000060316     c****               WHEN       V2Cfpr = 'T'  and  DCTfpr <> 'T'  and
115100060316     c****                          SIMFEL <> *zeros  and  VTCnev > *zeros
115200060316     c*****                         and §STDmod = ' '
115300060316     c******             movel     msg(39)       V2cmsg
115400050720      * se evento fortuito non si cambia in LT
115500060316     c******             WHEN       V2Cfpr = 'T'  and  DCTfpr <> 'T' and
115600060316     c******                        VTCnev > *zeros and §tadevfo = 'S'
115700060316     c******                        and §STDmod = ' '
115800060316     c******             movel     msg(41)       v2cmsg
115900051117      * se modifica del 21/11/2005 è attiva non si può mettere in LT una pratica con :
116000051117      *                            evento fortuito senza mandato e senza importo d'assicurare
116100051117      *                          o porto franco import
116200060316     c******             WHEN      §STDmod = 'S' and
116300060316     c                   WHEN      V2Cfpr = 'T' and DCTfpr <> 'T'
116400051117     c                   If          VTCnev > *zeros and §tadevfo = 'S' and     evento fortuito
116500051117     c                              wmand = ' ' and arbias = 0                  no mandato no assicu
116600051117     c                   movel     msg(31)       V2cmsg
116700051117     c                   endif
116800051117     c                   if          §DCTtisp = 'I'  and                        -import
116900051117     c                                   §DCTport = 'F'                          porto franco
117000081022      * se utente sede posso mettere in LT un Porto Franco Import
117100081022     c                             and simfel <> *zeros
117200051117     c                   movel     msg(33)       V2cmsg
117300051117     c                   endif
117400051117
117500060317     c*******            WHEN       V2Cfpr = 'T'  and  DCTfpr <> 'T'            *ERRORE
117600060317     c                   If        DCLipt > *zeros                              * importo trattenuto
117700990428     c                   movel     msg(9)        V2cmsg
117800990428     c                   Else
117900990428     c                   movel     'R'           Wtrc
118000990428     c     KDKA          CHAIN(N)  FNDKA000                           30
118100990909     c                   if        *in30 = *off  and  DKAatb = *blanks
118200990909     c                   movel     msg(10)       V2cmsg
118300990428     c                   Endif
118400990909     c                   Endif
118500990429      *
118600990428     c                   ENDSL
118700990428      *
118800990428     c                   IF        V2Cmsg <> *blanks
118900990428     c                   eval      *in28 = *on
119000990428     c                   eval      *in40 = *on
119100990428     c                   goto      ectrd02
119200990428     c                   ENDIF
119300990428
119400990428      * BENEFICIARIO
119500990428     c                   SELECT
119600990429      *
119700990428     c                   WHEN      V2Cptr = *blanks
119800990428     c                   movel     msg(11)       V2cmsg
119900990430      *
120000990429      *
120100990428     c                   ENDSL
120200990428      *
120300990428     c                   IF        V2Cmsg <> *blanks
120400990428     c                   eval      *in28 = *on
120500990428     c                   eval      *in41 = *on
120600990428     c                   goto      ectrd02
120700990428     c                   ENDIF
120800990428
120900990428      * RIMBORSO A CARICO
121000990504      *      se tipo gestione modificato in 'T' forzo 'V'
121100990504      *      se tipo gestione modificato in 'P' forzo 'V' se ho importo trattenuto o rivalsa, negli
121200990504      *       altri casi ricalcolo
121300990428     c                   SELECT
121400990429      *
121500990429     c                   WHEN      V2Cpgd = *blanks
121600990428     c                   movel     msg(13)       V2cmsg
121700990429      *
121800990428     c                   WHEN      V2Cfpr = 'T'  and  V2Cpgd <> 'V'
121900990428     c                   movel     'V'           V2Cpgd
122000080307     c*                  movel     msg(14)       V2cmsg
122100990429      *
122200990503     c                   WHEN      V2Cfpr = 'T'
122300990503      *
122400990503     c                   WHEN      V2Cpgd <> 'V'  and  DCLipt > *zeros
122500990503     c                   movel     'V'           V2Cpgd
122600080307     c*                  movel     msg(37)       V2cmsg
122700990503      *
122800990503     c                   WHEN      V2Cpgd <> 'V'  and  Wrivalsa = 'S'
122900990503     c                   movel     'V'           V2Cpgd
123000080307     c*                  movel     msg(38)       V2cmsg
123100990503      *
123200990504     c                   WHEN      DCTfpr <> 'P'  and  Wmsg36frz = *blanks
123300990428     C                   clear                   TIBS69DS
123400990428     C                   clear                   CLSflo
123500990428     c                   z-add     ARBksc        I69kcs
123600990428     c                   exsr      CTR_anagra
123700990428     c                   If        %SUBST(CLSflo:3:1) <> *blanks
123800990428     c                   eval      V2Cpgd = %SUBST(CLSflo:3:1)
123900990428     c                   Else
124000990428     c                   eval      V2Cpgd = 'C'
124100990428     c                   Endif
124200990504     c                   movel     'S'           Wmsg36frz
124300080307     c****               movel     msg(36)       V2cmsg
124400990429      *
124500990428     c                   ENDSL
124600990503      *
124700990428     c                   IF        V2Cmsg <> *blanks
124800990428     c                   eval      *in28 = *on
124900990430     c                   eval      *in42 = *on
125000990428     c                   goto      ectrd02
125100990428     c                   ENDIF
125200990428
125300990428      * BENEFICIARIO ALTERNATIVO
125400990601      *      emettere un messaggio informativo se modificato il beneficiario
125500990601     c                   If        DCTksc = V2Cksc  and  V2Cksc > *zeros  and
125600990601     c                             V2Cptr <> DCTptr  and  Wmsg18frz = *blanks
125700990504     c                   eval      Wmsg18frz = 'S'
125800990504     c                   movel     msg(18)       V2cmsg
125900990430     c                   ELSE
126000990430      *
126100990430     c                   move      V2Cksc        w0040
126200990504     c                   clear                   Wbenalt
126300990430      *
126400990430     c                   SELECT
126500990430      *    se immessa ragione sociale e non il codice richiamo il pgm di ricerca alfabetica
126600990430     c                   WHEN      V2Cksc = *zeros  and  V2Dksc <> *blanks
126700990430     c                   exsr      ricalf
126800990430     c                   eval      *in29 = *on
126900990430      *    Se codice 9999 segnalazione di errore
127000990430     c                   WHEN      w0040 = 9999
127100990430     c                   movel     msg(19)       V2cmsg
127200990430      *    se codice 8888 ed è inserito l'importo trattenuto segnalazione di errore
127300990430     c                   WHEN      w0040 = 8888  and  DCLipt > *zeros
127400990430     c                   movel     msg(20)       V2cmsg
127500990430      *    se codice 8888 controllo il P.O. ed emetto videata dati anagrafici se non inseriti
127600990430     c                   WHEN      w0040 = 8888
127700990430     c                   movel     V2Cksc        w0030
127800990430     C     W0030         chain(N)  AZORG                              31
127900990430     c                   If        *in31 = *on  or  ORGfva <> *blanks
128000990430     c                   movel     msg(21)       V2cmsg
128100990430     c                   Else
128200990504     c                   movel     'S'           Wbenalt
128300990601     c                   If        V2Dksc <> W1V2Dksc  or  W1Ccap = *blanks
128400990601     c                   movel     V2Dksc        W1Crag
128500990430     c                   exsr      mancli
128600990430     c                   eval      *in29 = *ON
128700990430     c                   endif
128800990512     c                   endif
128900990430      *    se codificato deve essere su P.d.C. e se ho imp. trattenuto deve essere di fatturazione
129000990430     c                   WHEN      V2Cksc <> *zeros
129100990430     c                   exsr      CTR_benalt
129200990430      *
129300990430     c                   ENDSL
129400990430     c                   ENDIF
129500990430      *
129600990430     c                   If        V2Cmsg <> *blanks
129700990430     c                   eval      *in28 = *on
129800990430     c                   eval      *in43 = *on
129900990430     C                   endif
130000990430      *
130100990430     c                   if        *in28 = *on  OR  *in29 = *on
130200990430     c                   goto      ectrd02
130300990430     C                   endif
130400990428
130500990430      * INTERLOCUTORE
130600990430      *      da verificare se modificato il beneficiario o il beneficiario alternativo
130700990504     c                   IF        Wmsg24frz = *blanks  and
130800990504     c                             V2Dnoc <> WV2Dnoc  and  V2Dnoc <> *blanks
130900990504     c                             and (V2Dksc <> WV2Dksc  or  DCTptr <> V2Cptr)
131000990430     c                   eval      Wmsg24frz = 'S'
131100990430     c                   movel     msg(24)       V2cmsg
131200990430     c                   eval      *in28 = *on
131300990430     c                   eval      *in44 = *on
131400990430     c                   goto      ectrd02
131500990430     C                   ENDIF
131600990430
131700990430      * IMPORTO CONCORDATO
131800990504      *      se tipo gestione è 'P' l'importo è da eliminare.
131900990503      *      se tipo gestione è 'T' ed è modificato il beneficiario o il beneficiario alternativo
132000990503      *       emetto un messaggio informativo
132100990609      *      se il tipo gestione è modificato in 'T' e non sono in sede controllo che l'importo
132200990609      *       richiesto rientri nei limiti tabellati
132300990430     c                   SELECT
132400990430      *
132500990504     c                   WHEN      V2Cfpr = 'P'  and  V2Cipr > *zeros
132600990430     c                   movel     msg(25)       V2Cmsg
132700990430      *
132800990430     c                   WHEN      V2Cfpr = 'P'
132900990430      *
133000990430      *
133100990504     c                   WHEN      Wmsg30frz = *blanks  and  V2Cipr = £DCLipr
133200990503     c                             and (V2Dksc <> WV2Dksc  or  DCTptr <> V2Cptr)
133300990430     c                   eval      Wmsg30frz = 'S'
133400990430     c                   movel     msg(30)       V2Cmsg
133500990503      *
133600990609     c                   WHEN      SIMFEL <> *zeros
133700080211      * tolgo il controllo dell'importo concordato
133800080211     c****               exsr      CTR_impcon
133900011009      *
134000011009      *
134100011009     c                   WHEN      V2Cipr <> *zeros
134200011009     c                   movel     V2Cvlt        WVLT
134300011009     c                   z-add     V2Cipr        W0133
134400011009     c                   exsr      ctrvlt
134500990430      *
134600990430     c                   ENDSL
134700990430      *
134800990430     c                   IF        V2Cmsg <> *blanks
134900990430     c                   eval      *in28 = *on
135000990430     c                   eval      *in45 = *on
135100990430     C                   ENDIF
135200990430     c                   IF        *in28 = *on  OR  *in29 = *on
135300990430     c                   goto      ectrd02
135400990430     C                   ENDIF
135500990430
135600990430     c     ECTRD02       ENDSR
135700990426      **********************************************************************
135800990430      *  CONTROLLO IMPORTO CONCORDATO
135900990426      **********************************************************************
136000990430     C     CTR_impcon    BEGSR
136100990428
136200990426      *     Controllo la Valuta
136300990430     C                   movel     V2Cvlt        WVLT
136400990426     C                   z-add     V2Cipr        W0133
136500990426     C                   exsr      ctrvlt
136600990426
136700990430     C                   IF        *IN28 = *OFF  and  *IN29 = *OFF
136800010905      ****     Calcolo il controvalore il LIRE
136900010905     c***                   IF        V2Cvlt  = *blanks
137000010906      *     Calcolo il controvalore in divisa corrente
137100010910     c                   IF        V2Cvlt  = §gedDBA
137200010906     c                   z-add     V2Cipr        £V2Cipr
137300010906     c                   ELSE
137400010905     c***     §CVcmb        mult      V2Cipr        £V2Cipr
137500010906     c                   exsr      srCMB
137600010911     c                   z-add     W0133         £V2Cipr
137700010906     c                   ENDIF
137800990430      *
137900990609     c                   IF        £V2Cipr > §STDllt + (§STDllt * §STDptl)/ 100
138000080211     c                   movel     msg(32)       V2cmsg
138100990609     c                   ENDIF
138200990430      *
138300990426     c                   ENDIF
138400990426
138500990430     C                   ENDSR
138600990430      **************************************************************************
138700990430      * CONTROLLO IL BENEFICIARIO ALTERNATIVO
138800990430      **************************************************************************
138900990430     C     CTR_benalt    BEGSR
139000990430      *
139100990430      *    Aggancio anagrafico
139200990430     c                   clear                   ACOflg
139300990430     C                   clear                   TIBS69DS
139400990430     c                   z-add     V2Cksc        I69kac
139500990430     c                   exsr      CTR_anagra
139600990430      *    Aggancio particolarità se ho l'importo trattenuto
139700990430     c                   If          *in30 = *off  and  ACOflg = *blanks
139800990430     c                                             and  DCLipt > *zeros
139900990430     C                   clear                   TIBS69DS
140000990430     c                   z-add     V2Cksc        I69kcp
140100990430     c                   exsr      CTR_anagra
140200990430     c                   Endif
140300990430      *
140400990430     c                   SELECT
140500990430      *       non trovato o annullato errore
140600990430     c                   WHEN        *in30 = *on  or  ACOflg = '*'
140700990430     c                   movel     msg(22)       v2cmsg
140800990430      *
140900990430     c                   WHEN        DCLipt = *zeros
141000990430     c                   eval      *in29 = (V2Cksc <> WV2Cksc)
141100990430     c                   movel     ACOrag        V2Dksc
141200990503     c                   z-add     V2Cksc        WV2Cksc
141300990430      *
141400990430     c                   OTHER
141500990430      *    Controllo che sia il codice di fatturazione del beneficiario
141600990430     C                   clear                   TIBS69DS
141700990430     c                   If          V2Cptr = 'M'
141800990430     c                   z-add     VTCksm        I69kcp
141900990430     c                   Else
142000990430     c                   z-add     VTCksd        I69kcp
142100990430     c                   Endif
142200990430     c                   movel     I69kcp        W0040
142300990430      *
142400990430     c                   If        W0040 <> 9999  AND  W0040 <> 8888
142500990430     c                   exsr      CTR_anagra
142600990430     c                   If        *in30 = *on  or  ACOflg = '*'
142700990430     c                                          or  V2Cksc <> CLPscf
142800990430     c                   movel     msg(23)       v1cmsg
142900990430     c                   Else
143000990430     c                   eval      *in29 = (V2Cksc <> WV2Cksc)
143100990430     c                   movel     ACOrag        V2Dksc
143200990503     c                   z-add     V2Cksc        WV2Cksc
143300990430     c                   Endif
143400990430     c                   Endif
143500990430      *
143600990430     c                   ENDSL
143700990430      *
143800990430     C                   ENDSR
143900990426      *****************************************************************
144000990426      * CONTROLLO DIVISA
144100990426      *****************************************************************
144200990426     C     CTRVLT        BEGSR
144300990426      *
144400990426     C                   MOVEL     'CV'          KCOD
144500990426      * RICERCA
144600990426     C     '?'           SCAN      WVLT                                   30
144700990426      *
144800990426     C                   if        *IN30 = *ON
144900990426      *
145000990426     C                   CALL      'X§TABER'
145100990426     C                   PARM                    CODUT
145200990426     C                   PARM                    KCOD
145300990426     C                   PARM                    §KEY
145400990426     C                   PARM                    §DES
145500990426      *
145600990426     C                   MOVEL     §KEY          WVLT
145700990426     C                   eval      *in29 = *ON
145800990426     C                   GOTO      ENDVLT
145900990426     C                   endif
146000990426      *
146100990426      * CONTROLLO DIVISA
146200990426     C                   MOVEL(P)  WVLT          KKEY
146300990426     C     KTAB          CHAIN     TABEL00f                           30
146400990426      * ERRORE
146500990426     C                   if        *IN30 = *ON    OR
146600990426     C                              ( *IN30 = *OFF  and  TBLFLG <> *blanks)
146700990430     C                   MOVEL     msg(34)       V2CMSG
146800990426     C                   eval      *IN28 = *ON
146900990426     C                   GOTO      ENDVLT
147000990426     C                   endif
147100990426      *
147200990426     C                   MOVEL     TBLUNI        DSCV
147300990426      *
147400990426      * VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
147500990426     C                   IF        §CVFDC <> 'S'
147600990426     C                   MOVE      W0133         W0033
147700990426     C                   if        W0033 <> 0
147800990430     C                   movel     msg(35)       V2CMSG
147900990426     C                   eval      *IN28 = *ON
148000990426     C                   GOTO      ENDVLT
148100990426     C                   endif
148200010910     C                   else
148300010910      * controllo numero massimo di decimali impostabili per la valuta
148400010910     C     W0133         casNE     0             srCTRDEC
148500010910     C                   ENDCS
148600010910     C                   IF        *in28
148700010910     C                   MOVEL     MSG(40)       V2CMSG
148800010910     c                   move      §cvDEC        w002a
148900010914     c                   eval      %subst(V2CMSG:29:2) = w002a
149000010910     C                   GOTO      ENDVLT
149100010910     C                   ENDIF
149200990426     C                   ENDIF
149300990426      *
149400990426      * SE IMMESSA LA DIVISA E NON L'IMPORTO ERRORE
149500010911      * => NON PIU' !!!  CON L'EURO LA DIVISA SARA' SEMPRE VISUALIZZATA
149600010911      *    PER I CAMPI I/O
149700010911     c***                   if        WVLT <> *BLANKS  and  W0133 = *ZEROS
149800010911     C***                   MOVEL     msg(33)       V2CMSG
149900010911     C***                   eval      *IN28 = *ON
150000010911     C***                   GOTO      ENDVLT
150100010911     C***                   endif
150200990426      *
150300990426     C     ENDVLT        ENDSR
150400990426      *****************************************************************
150500990426      *  Ricerca alfabetica
150600990426      *****************************************************************
150700990426     c     ricalf        begsr
150800990426      *
150900990426     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
151000990426     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
151100990426     C                   Z-ADD     KCI           PAXccc                         Capoconto Clienti
151200990503     c                   movel(P)  V2Dksc        PAXdmt
151300000907     C                   z-add     1             PAXnum
151400990426      *
151500000907     C                   CALL      'XALFA3BR'
151600990426     C                   PARM                    PAXDUT
151700990426     C                   PARM                    CODUT
151800990426     C                   PARM                    PAXDMT
151900990426     C                   PARM                    PAXCCC
152000990426     C                   PARM                    PAXSTA
152100990426     C                   PARM      *blanks       PAXFLR
152200990426     C                   PARM      *blanks       PAXDIT
152300000907     C                   PARM                    PAXNUM            2 0
152400000907     C                   PARM                    PAXKCM           80
152500000907     C                   PARM                    PAXKSM          140
152600000907     C                   PARM                    PAXKDM           60
152700990426      *
152800990426      * NON TROVATO O PREMUTO CMD12
152900990426    2C     paxsta        IFGT      -1
153000000907     C                   MOVEL     paxksm        V2Cksc
153100990426    2C                   ENDIF
153200990426      *
153300990426     c                   endsr
153400990426      *****************************************************************
153500990426      *  MANUTENZIONE/INSERIMENTO ANAGRAFICA CLIENTI
153600990426      *****************************************************************
153700990426     C     MANCLI        BEGSR
153800990426      *
153900990426      * EMISSIONE
154000990428     c     emw01         tag
154100990428     c                   exfmt     FI32W01
154200990426      *
154300990428     c                   clear                   w1cmsg
154400990426     c                   eval      *in28 = *off
154500990426      *
154600990601     c   KL              goto      Emancli
154700990601      *
154800990426      * CONTROLLI
154900990428     c                   exsr      ctrw01
155000990428     c   28              goto      emw01
155100990601      *
155200990601     c     Emancli       tag
155300990601      *
155400990601     c                   movel     w1crag        V2dksc
155500990601     c                   movel     w1crag        W1V2dksc
155600990426      *
155700990601     C                   ENDSR
155800990426      *****************************************************************
155900990426      * CONTROLLI VIDEATA MANUTENZ./INSERIMENTO ANAGRAFICA CLIENTI
156000990426      *****************************************************************
156100990428     C     CTRW01        BEGSR
156200990426
156300990426     c     60            DO        69            XX
156400990426     c                   eval      *IN(XX) = *OFF
156500990426     c                   ENDDO
156600990426
156700990426      * Controllo Ragione Sociale, Indirizzo, Località, Provincia, CAP, Nazione e Partita IVA
156800990427     c                   CLEAR                   FNLV14DS
156900990430     c                   z-add     Wdatasp       I14dri
157000990428     c                   movel     W1Crag        I14rsc
157100990428     c                   movel     W1Cvia        I14ind
157200990428     c                   movel     W1Cloc        E14loc
157300990428     c                   movel     W1Cprv        E14prv
157400990428     c                   movel     W1Ccap        E14cap
157500990428     c                   movel     W1Cnaz        E14nar
157600990428     c                   eval      E14pip = W1Cive + W1Civa
157700990426      *
157800990427     c                   CALL      'FNLV14R'
157900990426     c                   PARM                    KPJBA
158000990426     c                   PARM                    FNLV14DS
158100990426      *
158200990428     c                   movel     I14ind        W1Cvia
158300990428     c                   movel     E14loc        W1Cloc
158400990428     c                   movel     E14prv        W1Cprv
158500990428     c                   movel     E14cap        W1Ccap
158600990428     c                   movel     E14nar        W1Cnaz
158700990428     c                   eval      W1Cive = %subst(E14pip:1:2)
158800990428     c                   eval      W1Civa = %subst(E14pip:3:14)
158900990426      *
159000990426     c                   eval      *in60 = (O14err = '1')                       Ragione Sociale
159100990426     c                   eval      *in61 = (O14err = '2')                       Indirizzo
159200990426     c                   eval      *in62 = (O14err = '5')                       CAP
159300990426     c                   eval      *in63 = (O14err = '3')                       Località
159400990426     c                   eval      *in64 = (O14err = '4')                       Provincia
159500990426     c                   eval      *in65 = (O14err = '6')                       Nazione
159600990426     c                   eval      *in68 = (O14err = '7')                       P.IVA
159700990426      *
159800990426     c                   IF        O14msg <> *blanks
159900990428     c                   movel     O14msg        W1Cmsg
160000990426     c                   eval      *IN28 = *ON
160100990426     c                   ELSE
160200990426      * Controllo il CAP
160300990426     C                   exsr      CAZIS0
160400990426     c                   ENDIF
160500990428     C   28              goto      ectW01
160600990426
160700990426      * CONTROLLO IL NUMERO FAX
160800990428     C                   IF        W1Cfax <> *BLANKS
160900990426     C                   clear                   TRUL42DS
161000990428     C                   movel     W1Cfax        D42FAX
161100990426     C                   CALL      'TRUL42R'
161200990426     C                   PARM                    TRUL42DS
161300990426     C                   IF        D42ERR = '1'
161400990426     C                   eval      *in66 = *on
161500990426     C                   eval      *in28 = *on
161600990428     C                   movel     D42MSG        W1cmsg
161700990426     C                   ENDIF
161800990426     C                   ENDIF
161900990428     C   28              goto      ectW01
162000990426
162100990426      * CONTROLLO IL NUMERO DI TELEFONO
162200990428     C                   IF        W1Ctel <> *BLANKS
162300990426     C                   clear                   TRUL42DS
162400990428     C                   movel     W1Ctel        D42FAX
162500990426     C                   CALL      'TRUL42R'
162600990426     C                   PARM                    TRUL42DS
162700990426     C                   IF        D42ERR = '1'
162800990426     C                   eval      *in67 = *on
162900990426     C                   eval      *in28 = *on
163000990428     C                   movel     D42MSG        W1cmsg
163100990426     C                   ENDIF
163200990426     C                   ENDIF
163300990428     C   28              goto      ectW01
163400990426
163500990428     c     ECTW01        ENDSR
163600990426      *---------------------------------------------------------------*
163700990426      * ROUTINE DI CONTROLLO ESISTENZA CAP                            *
163800990426      *---------------------------------------------------------------*
163900990426     C     CAZIS0        BEGSR
164000990426      *
164100990426      * CONTROLLO CAP PER INTERO
164200990426     C                   CLEAR                   TISI95DS
164300990426     C                   CLEAR                   FNLV13DS
164400990426     C                   MOVEL     ' '           I95TLA
164500990426     C                   MOVEL     '7'           I95TCN
164600990428     C                   MOVEL     W1cCAP        I95CAP
164700990428     C                   MOVEL     W1cNAZ        I95NAR
164800990428     C                   MOVEL     W1cvia        I95IND
164900990428     C                   MOVEL     W1cloc        I95LOC
165000990428     C                   MOVEL     W1cprv        I95PRV
165100990426     C                   MOVEL     'S'           I13AF0
165200990426     C                   MOVEL     'S'           I13AF1
165300990426     C                   MOVEL     'S'           I13SZ2
165400990426     C                   MOVEL     'S'           I13LA3
165500990426     C                   MOVEL     'S'           I13SZ3
165600990426     C                   MOVEL     'S'           I13CNV
165700990426     C                   MOVEL     'S'           I13SZV
165800990426     C* Gestione errori forzabili: se cambiato CAP/Località
165900990426      * riazzero i valori di errori forzabili
166000990428    2C     W1cCAP        IFNE      WSVCAP
166100990426     C                   CLEAR                   WFZV
166200990426     C                   CLEAR                   WFZ1
166300990426     C                   CLEAR                   WFZ2
166400990426     C                   CLEAR                   WFZ3
166500990428     C                   MOVEL     W1cCAP        WSVCAP
166600990426    2C                   ENDIF
166700990426      * Modifica località
166800990428    2C     W1cLOC        IFNE      WSVLOC
166900990428     C                   MOVEL     W1cLOC        WSVLOC
167000990426     C                   CLEAR                   WFZ1
167100990426     C                   CLEAR                   WFZ2
167200990426     C                   CLEAR                   WFZ3
167300990426    2C                   ENDIF
167400990426      * Modifica PROVINCIA
167500990428    2C     W1cPRv        IFNE      WSVPRv
167600990428     C                   MOVEL     W1cPRv        WSVPRv
167700990426     C                   CLEAR                   WFZ3
167800990426    2C                   ENDIF
167900990426      * CALL
168000990426     C                   MOVEL     WFZV          E13FZV
168100990426     C                   MOVEL     WFZ1          E13FZ1
168200990426     C                   MOVEL     WFZ2          E13FZ2
168300990426     C                   MOVEL     WFZ3          E13FZ3
168400990426     C                   CALL      'FNLV13R'
168500990426     C                   PARM                    KPJBA
168600990426     C                   PARM                    FNLV13DS
168700990426     C                   PARM                    TISI95DS
168800990426     C* Ricontrollo errori
168900990426     C                   MOVEL     O13ERR        WERR
169000990426     C                   MOVEL     O13MSG        WMSG
169100990426     C                   MOVEL     E13FZV        WFZV
169200990426     C                   MOVEL     E13FZ1        WFZ1
169300990426     C                   MOVEL     E13FZ2        WFZ2
169400990426     C                   MOVEL     E13FZ3        WFZ3
169500990426     C*
169600990426     C* Se il livello di affidabilità non è inferiore a 2 e
169700990426     C* il livello di reperimento dati non è inferiore a 3
169800990426     C* reimposto i dati da TISI95DS
169900990426     C     O13RON        IFEQ      'S'
170000990426     C     O13ROC        OREQ      'S'
170100990426     C     O13ROP        OREQ      'S'
170200990426     C     O13ROL        OREQ      'S'
170300990428     C                   MOVEL     O95LOC        W1cLOC
170400990428     C                   MOVEL     O95PRV        W1cprv
170500990428     C                   MOVEL     O95NAR        W1cnaz
170600990428     C                   MOVEL     O95CAP        W1cCAP
170700990426     C                   END
170800990426     C*
170900990426     C                   EXSR      CTRIND
171000990426     C* Se non ci sono errori proseguo
171100990426     C   28              GOTO      FINCTR
171200990426     C*
171300990426     C* Controllo se reimpostata località
171400990426     C     O13ROL        IFEQ      'S'
171500990426     c                   eval      *in63 = *on
171600990426     c                   eval      *in28 = *on
171700990428     C                   MOVEL     msg(26)       W1cmsg
171800990426     C                   GOTO      FINCTR
171900990426     C                   END
172000990426     C* Controllo se reimpostata Provincia
172100990426     C     O13ROP        IFEQ      'S'
172200990426     c                   eval      *in64 = *on
172300990426     c                   eval      *in28 = *on
172400990428     C                   MOVEL     msg(28)       W1cmsg
172500990426     C                   GOTO      FINCTR
172600990426     C                   END
172700990426     C* Controllo se reimpostato CAP
172800990426     C     O13ROC        IFEQ      'S'
172900990426     c                   eval      *in62 = *on
173000990426     c                   eval      *in28 = *on
173100990428     C                   MOVEL     msg(27)       W1cmsg
173200990426     C                   GOTO      FINCTR
173300990426     C                   END
173400990426     C* Controllo se reimpostata Nazione
173500990426     C     O13RON        IFEQ      'S'
173600990426     c                   eval      *in65 = *on
173700990426     c                   eval      *in28 = *on
173800990428     C                   MOVEL     msg(29)       W1cmsg
173900990426     C                   GOTO      FINCTR
174000990426     C                   END
174100990426     C*
174200990426     C     FINCTR        ENDSR
174300990426     C*------------------------------------------------------*
174400990426     C*  Controllo dati indirizzo
174500990426     C*------------------------------------------------------*
174600990426     C     CTRIND        BEGSR
174700990426     C*
174800990426     C* Errore !!!!
174900990426     C     WERR          IFNE      ' '
175000990426     C     WMSG          ANDNE     *BLANKS
175100990426     C* Manca la Ragione sociale
175200990426     C                   SELECT
175300990426     C* Errore su località/città
175400990426     C     WERR          WHENEQ    '3'
175500990426     c                   eval      *in63 = *on
175600990426     c                   eval      *in28 = *on
175700990426     C* Errore su Cap
175800990426     C     WERR          WHENEQ    '5'
175900990426     c                   eval      *in62 = *on
176000990426     c                   eval      *in28 = *on
176100990426     C* Errore su provincia
176200990426     C     WERR          WHENEQ    '4'
176300990426     c                   eval      *in64 = *on
176400990426     c                   eval      *in28 = *on
176500990426     C* Nazione  errata
176600990426     C     WERR          WHENEQ    '6'
176700990426     c                   eval      *in65 = *on
176800990426     c                   eval      *in28 = *on
176900990426    2C                   ENDSL
177000990426      *
177100990428     C                   MOVEL     WMSG          W1cmsg
177200990426     C                   END
177300990426     C*
177400990426     C                   ENDSR
177500990426      *****************************************************************
177600990426      *   CONFERMA DATI C. A.
177700990426      *****************************************************************
177800990426     C     AGGIORNA      BEGSR
177900990426
178000990430      * Descrizioni
178100990430      *     se modificato l'interlocutore
178200990430     c                   if        V2Dnoc <> WV2Dnoc
178300990430     C                   EXSR      WRTDCS
178400011126     C                   ELSE
178500011126     c                   IF        Wfndcs = 'S'
178600011126     c                   UNLOCK    fndcs01l
178700011126     c                   endif
178800990430     c                   endif
178900990430
179000990430      * Fasi
179100990503      *     se modificato il tipo gestione o il beneficiario
179200990503     c                   if        V2Cfpr <> DCTfpr  or  V2Cptr <> DCTptr
179300990504     c                                               or  V2Cipr <> £DCLipr
179400990430     C                   EXSR      WRTDCF
179500990430     c                   endif
179600990430
179700990430      * Liquidazione C.A.
179800990726      *     se modificato l'importo concordato o nuovo tipo gestione "T"
179900990927     c                   if        £DCLipr <> V2Cipr  or
180000990927     c                             (V2Cfpr <> DCTfpr  and  V2Cfpr = 'T')
180100990426     C                   EXSR      WRTDCL
180200990426     c                   endif
180300990430
180400990426      * Anagrafiche
180500990426     C                   EXSR      WRTDKA
180600990430
180700990426      * Testata C.A.
180800990426     C                   EXSR      WRTDCT
180900020130      *
181000020130     C                   IF        §DCTpfra <> wFRA
181100020130     C                   exsr      WRTDCS_2
181200020130     C                   ENDIF
181300990430
181400990426     C                   ENDSR
181500990430      *****************************************************************
181600990430      *  GESTIONE SCRITTURA DESCRIZIONI C.A.
181700990430      *****************************************************************
181800990430     C     WRTDCS        BEGSR
181900990430
182000990609     c                   IF        Wfndcs <> 'S'
182100990430     c                   clear                   fndcs000
182200990430     c                   movel     ktpd          dcstpd
182300990430     c                   movel     knkt          dcsnkt
182400990430     c                   movel     kstd          dcsstd
182500990430     c                   movel     knks          dcsnks
182600990430     c                   movel     kdim          dcsdim
182700990430     c                   movel     khim          dcshim
182800990430     c                   movel     ktrc          dcstrc
182900990430     c                   z-add     1             dcspno
183000990430     c                   movel     DCTgfc        dcspos
183100990430     c                   movel     knmus         dcspru
183200990609     c                   ENDIF
183300990430      *
183400990430     c                   movel     V2Dnoc        dcsnot
183500990430     c                   clear                   dcsatb
183600990430     c                   clear                   dcsft1
183700990430     c                   clear                   dcsdt1
183800990430      *
183900990430     c                   SELECT
184000990430     c                   WHEN      Wfndcs = 'S'
184100990430     c                   update    fndcs000
184200990430     c                   WHEN      V2Dnoc <> *blanks
184300990430     c                   write     fndcs000
184400990430     c                   ENDSL
184500990430
184600990430     C                   ENDSR
184700020130      *****************************************************************
184800020130      *  SCRITTURA NOTE AGGIUTIVE
184900020130      *****************************************************************
185000020130     C     WRTDCS_2      BEGSR
185100020130      *
185200020130      * cerco l'ultimo progressivo note della fase in corso
185300020130      * - campi strandard
185400020130     c                   eval      Ktpd     = 'C'
185500020130     c                   eval      Knkt     = DS_numca
185600020130     c                   eval      Kstd     = *blanks
185700020130     c                   eval      Kdim     = dateu
185800020130     c                   time                    W0140
185900020130     c                   movel     W0140         Khim
186000020130      * - dati relativi alla CA in manutenzione
186100020130     c                   movel(p)  DCTfca        Knks
186200020130     c                   eval      Ktrc     = 'N'
186300020130      * cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
186400020130     c     Kdcs          setgt     FNDCS01L
186500020130     c     Kdcs          readpe    FNDCS01L
186600020130      *
186700020130     c                   clear                   DCSatb
186800020130     c                   eval      DCStpd   = Ktpd
186900020130     c                   eval      DCSnkt   = Knkt
187000020130     c                   eval      DCSstd   = Kstd
187100020130     c                   eval      DCSdim   = Kdim
187200020130     c                   eval      DCShim   = Khim
187300020130     c                   eval      DCSnks   = Knks
187400020130     c                   eval      DCStrc   = Ktrc
187500020130     c                   IF        %eof(FNDCS01L)
187600020130     c                   eval      DCSpno   = 1
187700020130     c                   else
187800020130     c                   eval      DCSpno   = DCSpno + 1
187900020130     c                   ENDIF
188000020130     c                   IF        §DCTpfra =  'F'
188100020205     c                   eval      DCSnot   = Comm_Fra_I
188200020130     c                   else
188300020205     c                   eval      DCSnot   = Comm_Fra_E
188400020130     c                   ENDIF
188500020130     c                   eval      DCSpru   = KNMUS
188600020130     c***                IF        SimFel   = 0
188700020130     c                   eval      DCSpos   = DCTgfc
188800020130     c***                else
188900020130     c***                eval      DCSpos   = SimFel
189000020130     c***                endif
189100020130     c                   clear                   DCSstn
189200020130     c                   clear                   DCSft1
189300020130     c                   clear                   DCSdt1
189400020130      *
189500020130     c                   write     FNDCS000
189600020130      *
189700020130     c                   clear                   Khim
189800020130      *
189900020130     C                   ENDSR
190000990426      *****************************************************************
190100990426      *  SCRITTURA FASI C.A.
190200990426      *****************************************************************
190300990426     c     WRTDCF        BEGSR
190400990426      *
190500990426     c                   clear                   fndcf000
190600990426      *
190700990430     c                   z-add     DCTaac        DCFaac
190800990430     c                   z-add     DCTfil        DCFfil
190900990430     c                   z-add     DCTnca        DCFnca
191000990427     c                   movel     V2cptr        DCFptr
191100990430      * Imposto la fase
191200990426     c                   SELECT
191300990531     C                   WHEN      V2Cfpr = 'P'  and  DCTfpr = 'P'
191400990531     c                   z-add     DCTfca        DCFfca
191500990531      *
191600990503     C                   WHEN      V2Cfpr = 'P'
191700990503     c                   z-add     WFfff         DCFfca
191800990503      *
191900990503     C                   WHEN      V2Cipr = 0
192000990430     c                   z-add     WFfff         DCFfca
192100990503      *
192200990503     C                   WHEN      V2Cipr > 0
192300990503     c                   z-add     WFffs         DCFfca
192400990426     c                   ENDSL
192500020201      *
192600020201      * ripulisco il flag Franchigia se modificato il tipo gestione
192700020201      *  da "P" a "T"
192800020201     c                   IF        V2Cfpr = 'T'  and  DCTfpr = 'P'
192900020201     c                   clear                   §DCFpfra
193000020201     c                   else
193100020201     c                   movel     §DCTpfra      §DCFpfra
193200020201     c                   ENDIF
193300990426      *
193400990426     c                   z-add     dateu         DCFdfc
193500990426     c                   movel     w0140         DCFhfc
193600990503     c                   z-add     DCTgfc        DCFfev
193700990426     c                   movel     knmus         DCFpru
193800020201     c***                clear                   DCFlet
193900020201     c                   clear                   §DCFriti
194000020201     c                   movel     DDCF01        DCFlet
194100990426     c                   clear                   DCFftr
194200990426     c                   clear                   DCFdtr
194300990426      *
194400990426     c                   write     fndcf000
194500990426      *
194600990426     C                   ENDSR
194700990426      *****************************************************************
194800990426      *  SCRITTURA/AGGIORNAMENTO liquidazione C.A.
194900990426      *****************************************************************
195000990426     C     WRTDCL        BEGSR
195100990426      *
195200990426     c                   z-add     V2Cipr        DCLipr
195300010905     c                   IF        DCLipr <> 0
195400990430     c                   movel     V2Cvlt        DCLvpr
195500010905     c                   else
195600010905     c                   clear                   DCLvpr
195700010905     c                   ENDIF
195800990430     c                   clear                   DCLcpr
195900990426     c                   z-add     V2Cipr        DCLipl
196000010905     c                   IF        DCLipl <> 0
196100990430     c                   movel     V2Cvlt        DCLvpl
196200010905     c                   else
196300010905     c                   clear                   DCLvpl
196400010905     c                   ENDIF
196500990430     c                   clear                   DCLcpl
196600990426      *
196700990430     c                   z-add     DCTaac        DCLaac
196800990430     c                   z-add     DCTfil        DCLfil
196900990430     c                   z-add     DCTnca        DCLnca
197000990430     c                   clear                   DCLatb
197100990430     c                   clear                   DCLftr
197200990426      *
197300990426     c                   If        Wfndcl = 'S'
197400990426     c                   UPDATE    FNDCL000
197500990426     c                   Else
197600990426     c                   WRITE     FNDCL000
197700990426     c                   Endif
197800990426      *
197900990426     C                   ENDSR
198000990426      *****************************************************************
198100990426      *  SCRITTURA/AGGIORNAMENTO anagrafica clienti per danni
198200990426      *****************************************************************
198300990426     C     WRTDKA        BEGSR
198400990426
198500990426     c                   movel     'C'           Wtrc
198600990426     c     KDKA          chain     fndka01l                           30
198700990427     c                   move      V2Cksc        W0040
198800990426      *
198900990426      *     Se è codificato o non è impostato annullo FNDKA se esiste
199000990426     c                   IF        W0040 <> 8888
199100990426     c                   If          *IN30 = *OFF
199200990426     c                   movel     'A'           dkaatb
199300990426     c                   clear                   dkaftr
199400990426     c                   update    fndka000
199500990430     c                   Endif
199600990430      *
199700990426     c                   ELSE
199800990426      *
199900990426     c                   clear                   FNDKA000
200000990430     c                   z-add     DCTaac        dkaaac
200100990430     c                   z-add     DCTfil        dkafil
200200990430     c                   z-add     DCTnca        dkanca
200300990426      *
200400990427     c                   move      V2Cksc        DKAksc
200500990426     c                   movel     Wtrc          DKAtrc
200600990426     c                   movel     KCI           DKAkcc
200700990430     c                   movel     W1Crag        DKArag
200800990430     c                   movel     W1Cvia        DKAvia
200900990430     c                   movel     W1Ccap        DKAcap
201000990430     c                   movel     W1Cloc        DKAloc
201100990430     c                   movel     W1Cprv        DKAprv
201200990430     c                   movel     W1Cnaz        DKAnaz
201300990430     c                   eval      DKAiva = W1Civa + W1Cive
201400990430     c                   movel     W1Ctel        DKAtel
201500990430     c                   movel     W1Cfax        DKAfax
201600990426      *
201700990426      * Scrivo o aggiorno
201800990426     c                   If        *IN30 = *ON
201900990426     c                   write     fndka000
202000990426     c                   Else
202100990426     c                   update    fndka000
202200990426    2c                   Endif
202300990426      *
202400990426     c                   ENDIF
202500990426
202600990426     C                   ENDSR
202700990426      **********************************************************************
202800990426      *  SCRITTURA/AGGIORNAMENTO TESTATA C.A.
202900990426      **********************************************************************
203000990426     C     WRTDCT        BEGSR
203100990430
203200990430      * Imposto la fase e il P.O. gestione
203300990430     c                   SELECT
203400990503     C                   WHEN      V2Cfpr = DCTfpr  and  V2Cptr = DCTptr
203500990504     C                                              and  V2Cipr = £DCLipr
203600990503      *
203700990531     C                   WHEN      V2Cfpr = 'P'  and  DCTfpr = 'P'
203800990531      *
203900990503     C                   WHEN      V2Cfpr = 'P'
204000990503     c                   z-add     WFfff         DCTfca
204100990503     c                   if        V2Cptr = 'M'
204200990503     c                   z-add     Wlnp          DCTgfc
204300990503     c                   else
204400990503     c                   z-add     DCTlna        DCTgfc
204500990503     c                   endif
204600990503      *
204700990503     C                   WHEN      V2Cipr = *zeros
204800990430     c                   z-add     WFfff         DCTfca
204900990430     c                   if        V2Cptr = 'M'
205000990430     c                   z-add     Wlnp          DCTgfc
205100990430     c                   else
205200990430     c                   z-add     DCTlna        DCTgfc
205300990430     c                   endif
205400990503      *
205500990503     C                   WHEN      V2Cipr > *zeros
205600990430     c                   z-add     WFffs         DCTfca
205700990430     c                   z-add     046           DCTgfc
205800990430     c                   ENDSL
205900020201      *
206000020201      * ripulisco il flag Franchigia se modificato il tipo gestione
206100020201      *  da "P" a "T"
206200020201     c                   IF        V2Cfpr = 'T'  and  DCTfpr = 'P'
206300020201     c                   clear                   §DCTpfra
206400020201     c                   ENDIF
206500990430
206600990430      * Imposto altri campi
206700990430     c                   movel     V2Cfpr        dctfpr
206800990430     c                   movel     V2Cptr        dctptr
206900990430     c                   movel     V2Cpgd        dctpgd
207000990430     c                   z-add     KCI           dctkcc
207100990430     c                   z-add     V2Cksc        dctksc
207200020130     c                   movel     DDCT01        DCTflo
207300990426
207400990426      * Imposto flag per trasmissione dati
207500990426     c                   clear                   dctft1
207600990426     c                   clear                   dctft2
207700990426
207800990426     c                   UPDATE    fndct000
207900990430
208000990426     C                   ENDSR
208100990426      *****************************************************************
208200990426      *  AGGANCIO ANAGRAFICHE
208300990426      *****************************************************************
208400990426     C     CTR_ANAGRA    BEGSR
208500990428      *
208600990426     C                   CALL      'TIBS69R'
208700990426     C                   PARM                    tibs69DS
208800990426     C                   PARM                    DS_cnaco
208900990426     C                   PARM                    DS_cnind
209000990426     C                   PARM                    DS_cnclp
209100990426     C                   PARM                    DS_fncls
209200990426      *
209300990426     C                   eval      *IN30 = (O69ERR =  '1')
209400990426      *
209500990426     C                   eval      wtibs69r = 'S'
209600990426      *
209700990426     C                   ENDSR
209800990427      **************************************************************************
209900990427      * CARICO SCHIERA L6 DA TABELLA £6
210000990427      **************************************************************************
210100990427     C     carl6         BEGSR
210200990427      *
210300990427     C                   CLEAR                   L6
210400990427     C                   CLEAR                   TRUL06DS
210500990427     C                   MOVE      '£6'          D06COD
210600990427     C                   MOVEL(P)  V1Cfgs        D06key
210700990427     C                   MOVEL(P)  TRUL06DS      KPJBU
210800990427      *
210900990427     C                   CALL      'TRUL06R'
211000990427     C                   PARM                    KPJBA
211100990427      *
211200990427     C                   MOVEL     KPJBU         TRUL06DS
211300990427     C                   movea     LIN           L6
211400990427      *
211500990427     c                   endsr
211600010910
211700010910      *****************************************************************
211800010910      * VERIFICA NUMERO DECIMALI IMPOSTATI
211900010910      *****************************************************************
212000010910     C     srCTRDEC      BEGSR
212100010910      *
212200011017     C                   reset                   XDECDS
212300010910      *
212400011017     C                   z-add     W0133         ICDnum
212500011017     C                   z-add     §cvDEC        ICDnrd
212600010910      *
212700011017     C                   call      'XDEC'
212800011017     C                   parm                    XDECDS
212900010910      *
213000011017     C                   IF        OCDesi  <> *off
213100010910     C                   eval      *IN28 = *ON
213200010910     C                   ENDIF
213300010910      *
213400010910     C                   ENDSR
213500010905
213600010905      *****************************************************************
213700010905      * CONVERSIONE IMPORTI IN MONETA CORRENTE
213800010905      *****************************************************************
213900010905     C     srCMB         BEGSR
214000010905      *
214100010905     C                   reset                   YEURCODS
214200010905      *
214300010905     C                   movel     WVLT          yecDVI
214400010905     C                   z-add     W0133         yecIMI
214500010910     C                   movel     §gedDBA       yecDVO
214600010905      *
214700010905     C                   call      'YEURCO'
214800010905     C                   parm                    YEURCODS
214900010905      *
215000010905     C                   IF        yecESI  = ' '
215100010905     C                   z-add     yecIMO        W0133
215200010905     C                   ENDIF
215300010905      *
215400010905     C                   ENDSR
215500010905
215600990426      *****************************************************************
215700990426      *  ROUTINE INIZIALE
215800990426      *****************************************************************
215900990426     C     *INZSR        BEGSR
216000990426
216100990426     C     *ENTRY        PLIST
216200990426     C                   PARM                    KPJBA
216300990426
216400990503     c                   movel     TIT           VTCtit
216500990503
216600990426      * Dati società
216700990426     C                   Z-ADD     1             CODUT
216800990426     C                   CALL      'X§PARUT'
216900990426     C                   PARM                    UT§DSE
217000990426      *
217100990426     C                   MOVEL     RAGUT         RSUT
217200990426     C                   MOVEL     REC80         CNCR80
217300990426      *
217400990426      *--- RICERCA CAPOCONTI
217500990426     C                   DO        50            XX
217600990426     C                   MOVE      TCU(XX)       TCUDS
217700990426     C     f56           CABNE     'CG'          END1
217800990426     C     f4            COMP      '1'                                    21
217900990426     C     f4            COMP      '2'                                    22
218000990426     C     f4            COMP      '3'                                    23
218100990426     C     f4            COMP      '6'                                    27
218200990426     C** 1 CLIENTI   21
218300990426     C** 2 FORNITORI 22
218400990426     C** 3 AGENTI    23
218500990426     C     f3            comp      '0'                                242425
218600990426     C     f3            comp      'I'                                    26
218700990426     C** 0 ITALIA   25
218800990426     C** 1 ESTERO   24
218900990426     ** I CAPO CONTO IVA
219000990426     c                   if        *in21 = *on
219100990426     C   24              Z-ADD     KCU(XX)       KCE               4 0
219200990426     C   25              Z-ADD     KCU(XX)       KCI               4 0
219300990426     c                   end
219400990426     c                   if        *in22 = *on
219500990426     C   24              Z-ADD     KCU(XX)       KFE               4 0
219600990426     C   25              Z-ADD     KCU(XX)       KFI               4 0
219700990426     c                   end
219800990426     c                   if        *in23 = *on
219900990426     C   24              Z-ADD     KCU(XX)       KAE               4 0
220000990426     C   25              Z-ADD     KCU(XX)       KAI               4 0
220100990426     c                   end
220200990426     C   26              Z-ADD     KCU(XX)       KIVA              4 0
220300990426     C   27              Z-ADD     KCU(XX)       KBNA              4 0
220400990426     C     end1          tag
220500990426     C                   end
220600990426     C                   SETOFF                                       212223
220700990426     C                   SETOFF                                       242526
220800990426     C                   SETOFF                                       27
220900990427      *
221000990427      * IMPOSTO IL P.O. IN GESTIONE
221100020502     c                   z-add     simpou        v1cfgs
221200990427     c                   IF        SIMFEL = *ZEROS
221300020502     C*                  z-add     46            V1Cfgs
221400990427     C                   eval      L6(1) = V1Cfgs
221500990427     C                   eval      *in06 = *on
221600990427     C                   eval      *in07 = *on
221700990427     c                   ELSE
221800990427      *
221900020502     C*                  If        rem = 'REM'  AND  remfil > *ZEROS
222000020502     C*                  movel     REMFIL        V1CFGS
222100020502     C                   If        simtpp = '2' or simtpp = *blanks
222200990427     C                   eval      *in06 = *on
222300020502   X2C*                  Else
222400020502     C*                  movel     SIMFEL        V1CFGS
222500990427    2C                   Endif
222600990427      *
222700990427     C     V1CFGS        CHAIN     AZORG01L                           31
222800990427     c                   IF        *IN31 = *OFF
222900990427     C                   MOVEL     orgDES        V1DFGS
223000990427    2C                   ENDIF
223100990427      *
223200990427     C                   EXSR      CARL6
223300990427    2C                   ENDIF
223400010905
223500010905      * aggancio tabella GED
223600010910     c                   clear                   DGEDDN
223700010905     c                   clear                   TIBS02DS
223800010905     C                   MOVEL     'C'           t02mod
223900010905     C                   MOVEL     knsif         t02sif
224000010905     C                   MOVEL     'GED'         t02cod
224100010905     C                   MOVEL(P)  'DANNI'       t02ke1
224200010905      *
224300010905     C                   CALL      'TIBS02R'
224400010905     C                   PARM                    KPJBA
224500010905     C                   PARM                    TIBS02DS
224600010905      *
224700010905     C                   if        t02err = *BLANKS
224800010910     C                   MOVEL     T02UNI        DGEDDN
224900010905     C                   endif
225000990427
225100990426      ***
225200990426      * leggo tabella standards danni
225300990426      ***
225400990426     c                   clear                   TIBS02DS
225500990426     C                   MOVEL     'C'           t02mod
225600990426     C                   MOVEL     knsif         t02sif
225700990426     C                   MOVEL     'STD'         t02cod
225800990426     C                   MOVEL(P)  1             t02ke1
225900010910     C                   MOVEL(P)  §gedDBA       t02ke2
226000990426      *
226100990426     C                   CALL      'TIBS02R'
226200990426     C                   PARM                    KPJBA
226300990426     C                   PARM                    TIBS02DS
226400990426      *
226500990426     C                   if        t02err = *BLANKS
226600990426     C                   MOVEL     T02UNI        DSTD
226700990426     C                   endif
226800990426      ***
226900990426      * leggo tabella variabili tariffe per limite risarcibile
227000990426      ***
227100990426      * ORA
227200990426     C                   TIME                    W0140
227300990426      * UDATE IN GGMMAAAA
227400990426     C                   MOVE      W0140         WDTGIO
227500990426      * UDATE IN AAAAMMGG
227600990426     C                   Z-ADD     WDTGIO        G02DAT
227700990426     C                   MOVEL     *BLANK        G02ERR
227800990426     C                   CALL      'XSRDA8'
227900990426     C                   PARM                    WLBDAT
228000990426     C                   MOVEL     G02INV        DATEU
228100990503     C                   MOVEL     G02INV        V1Caac
228200990426
228300990426      * APRO FILE SPECIFICI PER SEDE/FILIALE/MODALITA' PGM
228400990426     C                   IF        SIMFEL = *zeros
228500990809     C                   open      TITAS30C
228600990426     C                   open      TNTMD01L
228700990426     c                   ELSE
228800990426     C                   open      FNARB01L
228900990426     C                   open      FNBLP01L
229000991116     C                   open      FIAR601L
229100990426     c                   ENDIF
229200990426
229300990426     C     KTAB          KLIST
229400990426     C                   KFLD                    CODUT
229500990426     C                   KFLD                    KCOD
229600990426     C                   KFLD                    KKEY
229700990426      *
229800990426     c     kbol01        klist
229900040802     c                   kfld                    Keyaas
230000040802     c                   kfld                    Keylnp
230100040802     c                   kfld                    Keynrs
230200040802     c                   kfld                    Keynsp
230300991116      *
230400991116     c     kAR6          klist
230500040802     c                   kfld                    Keyaas
230600040802     c                   kfld                    Keylnp
230700040802     c                   kfld                    Keynrs
230800040802     c                   kfld                    Keynsp
230900991116     c                   kfld                    TRCar6
231000990426      *
231100990426     C     KDET          KLIST
231200990427     C                   KFLD                    DCTaae
231300990427     C                   KFLD                    DCTnev
231400990426      *
231500990426     C     KDCS          KLIST
231600990426     C                   KFLD                    KTPD
231700990426     C                   KFLD                    KNKT
231800990426     C                   KFLD                    KSTD
231900990426     C                   KFLD                    KDIM
232000990426     C                   KFLD                    KHIM
232100990426     C                   KFLD                    KNKS
232200990426     C                   KFLD                    KTRC
232300990426      *
232400990426     c     kca           klist
232500990503     c                   kfld                    v1caac
232600990503     c                   kfld                    v1cfil
232700990503     c                   kfld                    v1cnca
232800990426      *
232900990426     c     KDKA          klist
233000990430     c                   kfld                    dctaac
233100990430     c                   kfld                    dctfil
233200990430     c                   kfld                    dctnca
233300990426     c                   kfld                    Wtrc
233400990426      *
233500990426     C                   ENDSR
233600990426      *------------------------------------------------------------
233700990426** MSG  L78                                                                  *
233800990427Immesso numero C.A. non valido                                                 1
233900990427La Comunicazione Anomalia selezionata è inesistente                            2
234000990427La Comunicazione Anomalia selezionata è annullata                              3
234100990427La Comunicazione Anomalia selezionata è chiusa                                 4
234200990427Comunicazione Anomalia non manutenzionabile: in gestione ad altro P.O.         5
234300990428Non trovata spedizione relativa alla Comunicazione Anomalia                    6
234400990428Non consentite modifiche per la Comunicazione Anomalia selezionata             7
234500990428Tipo gestione: impostare un valore corretto                                    8
234600990428Non è possibile modificare in pratica il cliente ha trattenuto                 9
234700990428Non è possibile modificare in pratica presentata rivalsa                       10
234800990428Beneficiario: impostare un valore corretto                                     11
234900010911LIBERO - ex Beneficiario: la modifica è possibile solo per CA in Liquidazione Transattiva  12
235000990428Rimborso a carico: impostare un valore corretto                                13
235100990503Rimborso a carico: impostato automaticamente "Vettore"                         14
235200990428Comunicazione Anomalia non manutenzionabile: inviati documenti a Consuldanni   15
235300990428Comunicazione Anomalia non manutenzionabile: Liq. Transattiva contabilizzata   16
235400990428Comunicazione Anomalia non manutenzionabile con questo programma               17
235500990430Il Beneficiario è stato modificato, verificare il Beneficiario Alternativo     18
235600990430Codice 9999 non utilizzabile. Inserire 8888 o un codice esistente su P.d.C.    19
235700990430Il cliente trattiene, indicarne il codice nel beneficiario alternativo         20
235800990430P.O. inesistente o annullato                                                   21
235900990430Codice errato o annullato                                                      22
236000990430Il Beneficiario Alternativo non è il codice fatturazione del Beneficiario      23
236100990430Il Beneficiario è stato modificato, verificare se cambia l'Interlocutore       24
236200990504Eliminare l'importo concordato, non previsto per C.A. in Pratica Assicurativa  25
236300990430Attenzione !! Modificata Località                                              26
236400990430Attenzione !! Modificato CAP                                                   27
236500990430Attenzione !! Modificata Provincia                                             28
236600990430Attenzione !! Modificata Nazione                                               29
236700990503Il Beneficiario è stato modificato, verificare l'importo concordato            30
236800051117Non è possibile LT, CA su spedizione non assicurata con Evento FORTUITO        31
236900990430L'Importo Concordato è superiore all'Importo Limite per la Liq. Transattiva    32
237000051117Non è possibile modificare in LT, CA su spedizione Porto Franco Import         33
237100990430Divisa errata                                                                  34
237200990430Non ammessi importi con decimali per la divisa indicata                        35
237300990503Rimborso a carico: impostato automaticamente "Consuldanni"                     36
237400990503Rimborso a carico: impostato "Vettore", il cliente ha trattenuto               37
237500990503Rimborso a carico: impostato "Vettore", presentata rivalsa                     38
237600990909Non è possibile modificare in Liquidazione Transattiva, CA con evento.         39
237700010910Ammessi importi con massimo __ decimali per la divisa indicata                 40
237800050720Non è possibile modificare in Liq. Transattiva, CA con evento FORTUITO         41

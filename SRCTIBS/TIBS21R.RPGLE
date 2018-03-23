000100000406      *--------------------------------------------------------------------------------------------*
000200000321      * Trasmissione bolle rigenerate da SEDE a P.O.
000300000321      *--------------------------------------------------------------------------------------------*
000400000321      * Se non riesco ad allocare un membro di un P.O. non trasmetto
000500000321      *--------------------------------------------------------------------------------------------*
000600020903      * A T T E N Z I O N E:
000700020903      *---------------------
000800090423      * VALORI DI RIGCAU
000900020903      * D = DANNI
001000020903      * C = C/A OMESSI
001100020903      * F = BOLLE FIRMATE
001200040511      * S = BOLLE FIRMATE DA SCANNER
001300021127
001400021127      * Azione messa in KGIORNO2 in modo da non avere 2 lavori che vanno in contemporanea
001500021127      * nel caso di richiesta rigenerazione bolla x danni (utente di filiale su as unico) e
001600021127      * nel caso di richiesta rigenerazione bolla da int. bolle di sede
001700000406
001800980929      ****************************************************************
001900980929      *  RIEPILOGO INDICATORI
002000980929      ***************************************************************
002100980929      * 20   - errore generico qcmdexc
002200980930      * 91   - Mancata allocazione file
002300000419      * 92   - Non trovato TITAS
002400000322      ***************************************************************
002500000322
002600000322     H DECEDIT('0,') DATEDIT(*DMY.)
002700000322
002800000321      *--------------------------------------------------------------------------------------------*
002900090423     FFnRIG01L  UF   E           K DISK
003000000322     FTITAS30C  IF   E           K DISK
003100000322     FTITAA30C  IF   E           K DISK
003200000322     FTITA430C  IF   E           K DISK
003300000322     FTITAT30C  IF   E           K DISK
003400000322     FTITA730C  IF   E           K DISK
003500040531     FFIAR531C  IF   E           K DISK
003600041122     FFNLBL01L  IF   E           K DISK    rename(fnlbl000:fnlbl001)
003700041122     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl002)
003800000322     FTNCSB03L  IF   E           K DISK
003900000322     FAZORG01L  IF   E           K DISK
004000000404     FTABEL00F  IF   E           K DISK
004100030110     FTNTBE01L  IF   E           K DISK
004200060221      *
004300000412     FFNBLP00T  UF A E             DISK    usropn
004400000412     FFNBLT00T  UF A E             DISK    usropn
004500030110     F                                     prefix(ART:3)
004600000322      *
004700000412     FFNARB00T  UF A E             DISK    usropn
004800060221     FFIAR400T  UF A E             DISK    usropn
004900051116     FFiAR900T  UF A E             DISK    usropn
005000000412     FFNART00T  UF A E             DISK    usropn
005100000404      *
005200030110     FFIAR500T  UF A E             DISK    usropn
005300030110     F                                     rename(FIAR5000:FIAR5T)
005400000412     FFIAR600T  UF A E             DISK    usropn
005500000412     FFIAR700T  UF A E             DISK    usropn
005600020304
005700020925     ffivabbft  o    e             disk    usropn
005800020304
005900000322      *----------------
006000120607     D cmpCHK          S             60    DIM(7) CTDATA PERRCD(1)
006100120607     D cmaCHK          S             60    DIM(7) CTDATA PERRCD(1)
006200120607     D cmpADD          S             60    DIM(7) CTDATA PERRCD(1)
006300120607     D cmaADD          S             60    DIM(7) CTDATA PERRCD(1)
006400120607     D cmpOVR          S             60    DIM(7) CTDATA PERRCD(1)
006500120607     D cmaOVR          S             60    DIM(7) CTDATA PERRCD(1)
006600120607     D cmpALC          S             60    DIM(7) CTDATA PERRCD(1)
006700120607     D cmaALC          S             60    DIM(7) CTDATA PERRCD(1)
006800120607     D cmpDLC          S             60    DIM(7) CTDATA PERRCD(1)
006900120607     D cmaDLC          S             60    DIM(7) CTDATA PERRCD(1)
007000020304
007100020827     d cmdvab          s             62    dim(5) ctdata perrcd(1)
007200000327      *
007300000410     D filet           S             10    DIM(20)
007400000410     D rcdt            S              7  0 DIM(20)
007500000412     D filer           S             10    DIM(20)
007600000404      *
007700000417     D T3aK            S              2    DIM(50)
007800000417     D T3aD            S             89    DIM(50)
007900000417     D TblK            S              8    DIM(50)
008000000417     D TblD            S             89    DIM(50)
008100030110     d AR5k            s                   dim(50) like(AR5trd)
008200000322      *----------------
008300000327     D kpjba         E DS
008400000412     D DTASV         E DS                  prefix(D)
008500000412     D FNLV16DS      E DS
008600000327     D dscmz         E DS
008700020827     D TIBS55DS      E DS
008800980915     D cncr80        E DS
008900000412     D UT§DSE0F      E DS
009000000327     D FNLV55DS      E DS
009100020204     D DSBL4A        E DS
009200000327     D DSBL4E        E DS
009300020204     D DSBL49        E DS
009400000327     D DTA4A         E DS
009500000413     D DSTB          E DS
009600000413     D DS3A          E DS
009700100604     D DS7R          E DS
009800030110     d dAR5          e ds                  inz
009900000412     D DSQT1         E DS
010000000412     D DS_TITAS      E DS                  extname(TITAS00F)
010100000412     D DS_TITAS_2    E DS                  extname(TITAS00F) prefix($)
010200000412     D DS_TITAA      E DS                  extname(TITAA00F)
010300000412     D DS_TITAA_O    E DS                  extname(TITAA00F) prefix(O)
010400000412     D DS_TITAA_M    E DS                  extname(TITAA00F) prefix(M)
010500000328     d TIBS69DS      E DS                  INZ
010600000328     d DS_cnaco      E DS                  extname(CNACO00F)
010700000328     d DS_cnind      E DS                  extname(CNIND00F)
010800000328     d DS_cnclp      E DS                  extname(CNCLP00F)
010900000328     d DS_fncls      E DS                  extname(FNCLS00F)
011000020304
011100000322      *----------------
011200020304     d keyorg          S                   LIKE(orgfil)
011300000330     d ktrc            S                   LIKE(TA4trc)
011400000404     d Wsegna          S                   LIKE(TASnca)
011500000404     D KCOD            S                   LIKE(TBLcod)
011600000404     D KKEY            S                   LIKE(TBLkey)
011700000412     D WAaas           S                   LIKE(TASaas)
011800000412     D Waas            S                   LIKE(TASaas)
011900000412     D Wlnp            S                   LIKE(TASlnp)
012000000412     D Wnrs            S                   LIKE(TASnrs)
012100000412     D Wnsp            S                   LIKE(TASnsp)
012200000412     D WAlnp           S                   LIKE(TASlnp)
012300000412     D WAnrs           S                   LIKE(TASnrs)
012400000412     D WAnsp           S                   LIKE(TASnsp)
012500000412     D WPaas           S                   LIKE(TASaas)
012600000412     D WPlnp           S                   LIKE(TASlnp)
012700000412     D WPnrs           S                   LIKE(TASnrs)
012800000412     D Wpnsp           S                   LIKE(TASnsp)
012900120530     D WLaas           S                   LIKE(TASaas)
013000120530     D WLlnp           S                   LIKE(TASlnp)
013100120530     D WLnrs           S                   LIKE(TASnrs)
013200120530     D WLnsp           S                   LIKE(TASnsp)
013300090423     D Wrigdue         S                   LIKE(RIGdela)  INZ
013400030508     D Wvabrmo         S                   LIKE(VABrmo)
013500030508     D Wvabrma         S                   LIKE(VABrma)
013600000322      *----------------
013700000322      * Variabili a programma
013800000322      *----------------
013900000413     D Wnumele         S              5P 0
014000000413     D WfileR          S             10
014100000412     D WfileT          S             10
014200020211     D Wlibfile        S             10
014300000412     D Wassegno        S              1
014400000419     D Wfirig          S              1
014500000412     D Wtipodata       S              1
014600000412     D Wtiporcd        S              1
014700000412     D Wazione         S              2  0
014800000412     D xx              S              2  0
014900000327     D yy              S              2  0
015000000327     D lenght          S             15  5 INZ(80)
015100000327     D comman          S             80
015200000322     D Wdata           S               D   DATFMT(*eur)
015300000327     D w0140           S             14  0
015400000327     D w0080           S              8  0
015500000412     D w0030           S              3  0
015600000327     D datcor          S              8  0
015700000412     d Wlegame         s              1
015800000413     D W008a           S              8
015900020304
016000020304     d rcdvab          s              7  0
016100020827     d wmbrvab         s              6
016200020827     d wmbrt           s             10
016300020304
016400020304      * work dati mamma originale
016500020304     d wpaasv          s              4
016600020304     d wplnpv          s              3
016700020304     d wpnrsv          s              2
016800020304     d wpnspv          s              7
016900020304     d wpkscv          s              7
017000020304     d wplnav          s              3
017100020305     d wpragv          s             35
017200020304     d wpdspv          s              8
017300020304      * work dati ultima figlia
017400020304     d waaasv          s              4
017500020304     d walnpv          s              3
017600020304     d wanrsv          s              2
017700020304     d wanspv          s              7
017800020304     d walnav          s              3
017900020304     d wapdcv          s              7
018000020304     d wandcv          s              7
018100020304     d waddcv          s              8
018200020304     d wadspv          s              8
018300020304
018400020304     d wrigpor         s              3
018500020304
018600020304     d wdatav          ds
018700020304     d  wgg                    1      2
018800020304     d  wmm                    3      4
018900020304     d  waa                    5      8
019000020304
019100020304     d wnumbolla       ds
019200020304     d  wvabaas                1      2
019300020304     d  wvablnp                3      5
019400020304     d  wvabnrs                6      7
019500020304     d  wvabnsp                8     14
019600020304
019700020304     D WLBDAT          DS                  INZ
019800020304     D  G02DAT                 1      8  0
019900020304     D  G02INV                 9     16  0
020000020304     D  G02ERR                17     17
020100020304     D  G02TGI                18     22  0
020200020304
020300020304      * Costanti per messaggi si P.E.I.
020400020408     d pe_testa1       c                   'Conferma - Richiesta Bolla Firmata'
020500020304     d pe_riga1        c                   'Il p.o. xxx richiede l''invio della-
020600020305     d                                      lettera di vettura firmata'
020700020305     d pe_riga2        c                   'relativa alla spedizione n. xxx-xx--
020800020305     d                                     xxxxxxx del xx/xx/xxxx'
020900020304     d pe_riga3        c                   'cliente xxxxxxx xxxxxxxxxxxxxxxxxxx-
021000020304     d                                     xxxxxxxxxxxxxxxx'
021100020305     d pe_riga4        c                   'spedizione consegnata n. xxx-xx-xxx-
021200020305     d                                     xxxx del xx/xx/xxxx'
021300020305     d pe_riga5        c                   'legata a quella richiesta'
021400020305     d pe_riga6        c                   'distinta di consegna n. xxxxxxx del-
021500020304     d                                      xx/xx/xxxx autotrasp. xxxxxxx'
021600980928      * ---------------------------------------------------------
021700980928     IFNBLP000
021800980928     I              BLPATB                      ARBATB
021900000324     I              BLPFLB                      ARBFLB
022000000324     I              BLPDIM                      ARBDIM
022100000324     I              BLPAAS                      ARBAAS
022200980928     I              BLPLNP                      ARBLNP
022300980928     I              BLPNRS                      ARBNRS
022400980928     I              BLPNSP                      ARBNSP
022500980928     I              BLPCBO                      ARBCBO
022600980928     I              BLPMGS                      ARBMGS
022700980928     I              BLPFNS                      ARBFNS
022800980928     I              BLPSCL                      ARBSCL
022900980928     I              BLPFBR                      ARBFBR
023000000324     I              BLPFST                      ARBFST
023100980928     I              BLPFDN                      ARBFDN
023200980928     I              BLPLNA                      ARBLNA
023300980928     I              BLPZNC                      ARBZNC
023400980928     I              BLPKSC                      ARBKSC
023500980928     I              BLPRSM                      ARBRSM
023600980928     I              BLPINM                      ARBINM
023700980928     I              BLPCAM                      ARBCAM
023800980928     I              BLPLOM                      ARBLOM
023900980928     I              BLPNZM                      ARBNZM
024000980928     I              BLPPRM                      ARBPRM
024100980928     I              BLPFAP                      ARBFAP
024200980928     I              BLPCPI                      ARBCPI
024300980928     I              BLPRSD                      ARBRSD
024400980928     I              BLPIND                      ARBIND
024500980928     I              BLPCAD                      ARBCAD
024600980928     I              BLPLOD                      ARBLOD
024700980928     I              BLPNZD                      ARBNZD
024800980928     I              BLPPRD                      ARBPRD
024900980928     I              BLPFIN                      ARBFIN
025000980928     I              BLPGC1                      ARBGC1
025100980928     I              BLPGC2                      ARBGC2
025200980928     I              BLPCTR                      ARBCTR
025300980928     I              BLPCTS                      ARBCTS
025400980928     I              BLPFTM                      ARBFTM
025500980928     I              BLPTSP                      ARBTSP
025600980928     I              BLPNAS                      ARBNAS
025700980928     I              BLPNCL                      ARBNCL
025800980928     I              BLPNCP                      ARBNCP
025900980928     I              BLPPKC                      ARBPKC
026000980928     I              BLPPKB                      ARBPKB
026100980928     I              BLPPKF                      ARBPKF
026200980928     I              BLPNCR                      ARBNCR
026300980928     I              BLPVLC                      ARBVLC
026400980928     I              BLPFVB                      ARBFVB
026500980928     I              BLPVLB                      ARBVLB
026600980928     I              BLPFVF                      ARBFVF
026700980928     I              BLPVLF                      ARBVLF
026800980928     I              BLPQFT                      ARBQFT
026900980928     I              BLPVMD                      ARBVMD
027000980928     I              BLPVAD                      ARBVAD
027100980928     I              BLPIAS                      ARBIAS
027200980928     I              BLPVAS                      ARBVAS
027300980928     I              BLPCCM                      ARBCCM
027400980928     I              BLPRMN                      ARBRMN
027500980928     I              BLPRMA                      ARBRMA
027600980928     I              BLPRMO                      ARBRMO
027700980928     I              BLPFLS                      ARBFLS
027800980928     I              BLPNCD                      ARBNCD
027900980928     I              BLPNCA                      ARBNCA
028000980928     I              BLPXCO                      ARBXCO
028100980928     I              BLPCTM                      ARBCTM
028200980928     I              BLPFFD                      ARBFFD
028300000324     I              BLPDRT                      ARBDRT
028400000324     I              BLPNRT                      ARBNRT
028500000324     I              BLPFPP                      ARBFPP
028600000324     I              BLPPDR                      ARBPDR
028700000324     I              BLPDSE                      ARBDSE
028800980928     I              BLPTCR                      ARBTCR
028900980928     I              BLPDCR                      ARBDCR
029000980928     I              BLPHCR                      ARBHCR
029100980928     I              BLPDTI                      ARBDTI
029200980928     I              BLPHTI                      ARBHTI
029300980928     I              BLPDCE                      ARBDCE
029400980928     I              BLPNPI                      ARBNPI
029500980928     I              BLPNCI                      ARBNCI
029600980928     I              BLPNRC                      ARBNRC
029700980928     I              BLPDBR                      ARBDBR
029800980928     I              BLPNFV                      ARBNFV
029900980928     I              BLPDPC                      ARBDPC
030000980928     I              BLPDUC                      ARBDUC
030100980928     I              BLPFLP                      ARBFLP
030200980928     I              BLPDET                      ARBDET
030300980928     I              BLPDUT                      ARBDUT
030400980928     I              BLPDAM                      ARBDAM
030500980928     I              BLPNDC                      ARBNDC
030600980928     I              BLPDDC                      ARBDDC
030700980928     I              BLPDCP                      ARBDCP
030800980928     I              BLPDCM                      ARBDCM
030900980928     I              BLPHMC                      ARBHMC
031000980928     I              BLPTC1                      ARBTC1
031100980928     I              BLPTC2                      ARBTC2
031200980928     I              BLPCCA                      ARBCCA
031300980928     I              BLPTMC                      ARBTMC
031400980928     I              BLPGGA                      ARBGGA
031500980928     I              BLPGMA                      ARBGMA
031600980928     I              BLPGVA                      ARBGVA
031700980928     I              BLPFLE                      ARBFLE
031800980928     I              BLPTFP                      ARBTFP
031900980928     I              BLPFEA                      ARBFEA
032000980928     I              BLPTFA                      ARBTFA
032100980928     I              BLPFT1                      ARBFT1
032200980928     I              BLPDT1                      ARBDT1
032300980928     I              BLPFT2                      ARBFT2
032400980928     I              BLPDT2                      ARBDT2
032500980928     I              BLPFT3                      ARBFT3
032600980928     I              BLPDT3                      ARBDT3
032700030110
032800000322      *--------------------------------------------------------------------------------------------*
032900000322      * Main lines
033000000322      *--------------------------------------------------------------------------------------------*
033100090423     C     Wrigdue       setll     FnRIG000
033200090423     C     Wrigdue       READE     FnRIG000
033300000419      *
033400000419      * Elaboro il file richiesto fino alla fine se i file di trasmissione
033500000419      * sono allocabili
033600090423     C                   DOW       not %EOF(fnrig01L)  and  *IN91 = *off
033700040507
033800090423      * Elaboro solo i record 'F' 'C' 'D'
033900121023if  2c                   If        RigCau = 'F' or RigCau = 'D' or
034000121023     c                             RigCau = '3'
034100000418      *
034200000419     c                   clear                   Wfirig
034300000419     c                   clear                   ds_titas
034400000412     c                   clear                   ds_titas_2
034500000327      *
034600000419      * aggancio TITAS, se non lo trovo accendo *in92                        interessato
034700000419     c                   eval      Waas = RIGaas
034800000419     c                   eval      Wlnp = RIGlnp
034900000419     c                   eval      Wnrs = RIGnrs
035000000419     c                   eval      Wnsp = RIGnsp
035100000419     C                   eval      *in92 = *off
035200000419     C                   exsr      AGGTAS
035300000419      *
035400000419     c                   IF        *in92 = *off
035500000322      * legge il file fisico e scrive il membro di trasmissione
035600000322     C                   EXSR      letscr
035700000418     C                   ENDIF
035800040507
035900040507e   2c                   EndIf
036000000418      *
036100090423     C     Wrigdue       READE     FnRIG000
036200000418     C                   ENDDO
036300000322      *
036400000418     C                   Exsr      chiusure
036500000418     C                   eval      *inlr = *on
036600000322      *--------------------------------------------------------------------------------------------*
036700000419      * Aggancio file TITAS se cambiato x legami
036800000322      *--------------------------------------------------------------------------------------------*
036900000419     C     AGGTAS        BEGSR
037000000322      *
037100000419      * verifico l'esistenza della spedizione
037200000419     c     KBOLw         chain     TITAS30c
037300000419      *
037400000419     C                   IF        NOT %FOUND(titas30c)
037500000419     C                   eval      *in92 = *on
037600000419     c                   ELSE
037700000419      *
037800000419      *   se tipo bolla non è valido cerco altro rcd
037900000419     C                   clear                   DSTB
038000000419     C                   eval      XX = 1
038100000419     C                   eval      W008a = TAStbl
038200000419     C     W008a         lookup    TblK(xx)                               31
038300000419     C                   movel     TblD(xx)      DSTB
038400000419     c                   if        §TBrbl = 'R'
038500000419     c                   movel     ds_titas      ds_titas_2
038600000419     c                   clear                   ds_titas
038700000509     c     KBOLw         reade     TITAS30c                               92
038800000419     c                   endif
038900000419      *
039000000419     C                   ENDIF
039100000419      *
039200000419     C                   ENDSR
039300000419      *--------------------------------------------------------------------------------------------*
039400000419      * letscr - legge il file fisico e scrive il membro di trasmissione
039500000419      *--------------------------------------------------------------------------------------------*
039600000419     C     letscr        BEGSR
039700000419      *
039800000412     c                   clear                   Wlegame
039900000322      *
040000120601      * BOLLE LEGATE:
040100120601      * se rigenerazione per bolla firmata trasmetto:
040200120601      *                                       in partenza la Mamma
040300000322      *                                       in arrivo l'Ultima Figlia
040400120601      * In tutti gli altri casi trasmetto tutte le bolle sia a partenza che ad
040500120601      * arrivo
040600000418     c                   exsr      legbol
040700000322      *
040800120605     c                   select
040900000322      * Imposto key sped se non ho bolla legata
041000120605    3c                   when      Wlegame = *blanks                            * bolla di reso
041100000322     c                   z-add     rigaas        WPaas
041200000322     c                   z-add     riglnp        WPlnp
041300000322     c                   z-add     rignrs        WPnrs
041400000322     c                   z-add     rignsp        WPnsp
041500000322     c                   z-add     rigaas        WAaas
041600000322     c                   z-add     riglnp        WAlnp
041700000322     c                   z-add     rignrs        WAnrs
041800000322     c                   z-add     rignsp        WAnsp
041900120605     c                   exsr      elatra
042000120605     g* Rigenerazione per bolla firmata
042100120605     c                   when      rigcau='F'
042200120605     c                   exsr      elatra
042300120605     c                   other
042400120601     c* se presente legame bolla devo trasmettere tutte le bolle legate ad eccezione
042500120601     c* della rigenerazione per bolle firmate che in questo caso serve solo la mamma e l'ultima
042600120601     c* figlia
042700120530     c                   eval      wlaas=lblaao
042800120530     c                   eval      wllnp=LBLLPO
042900120530     c                   eval      wlnrs=LBLNRO
043000120530     c                   eval      wlnsp=LBLNSO
043100120530     c     klbl2         setll     fnlbl02l
043200120530     c     klbl2         reade     fnlbl02l
043300120530     c                   z-add     lblAAO        WPaas
043400120530     c                   z-add     lblLPO        WPlnp
043500120530     c                   z-add     lblNRO        WPnrs
043600120530     c                   z-add     lblNSO        WPnsp
043700120530     c                   z-add     lblAAO        WAaas
043800120530     c                   z-add     lblLPO        WAlnp
043900120530     c                   z-add     lblNRO        WAnrs
044000120530     c                   z-add     lblNSO        WAnsp
044100120530     c                   exsr      ELATRA
044200120605     c                   dow       not %eof(fnlbl02l)
044300120530     c                   z-add     lblAAN        WPaas
044400120530     c                   z-add     lblLPN        WPlnp
044500120530     c                   z-add     lblNRN        WPnrs
044600120530     c                   z-add     lblNSN        WPnsp
044700120530     c                   z-add     lblAAN        WAaas
044800120530     c                   z-add     lblLPN        WAlnp
044900120530     c                   z-add     lblNRN        WAnrs
045000120530     c                   z-add     lblNSN        WAnsp
045100000322      *
045200120530     C                   EXSR      ELATRA
045300020304
045400120601     c                   eval      Wlaas=LBLAAN
045500120601     c                   eval      Wllnp=LBLLPN
045600120601     c                   eval      Wlnrs=LBLNRN
045700120601     c                   eval      Wlnsp=LBLNSN
045800120605     c     klbl2         setll     fnlbl02l
045900120605     c     klbl2         reade     fnlbl02l
046000120530     c                   enddo
046100120605     c                   endsl
046200120530     c*
046300000323      *
046400000323     c                   ENDSR
046500120530      *--------------------------------------------------------------------------------------------*
046600120530      * Elaboro per trasmissione                                            to
046700120530      *--------------------------------------------------------------------------------------------*
046800120530     C     ELATRA        BEGSR
046900120530      * Elaboro per trasmissione alla partenza, all'arrivo o entrambe
047000120530      * solo se la causale è Danni o C/A Omessi
047100120530      * Se la causale è F= Bolle Firmate devo trasmettere
047200120530      * 1 msg di PEI alla Partenza e 1 all'Arrivo e scrivere
047300120530      * il file FIVABBFT da inviare all'arrivo
047400120530     C                   SELECT
047500120530     c                   WHEN      rigCAU = 'F'
047600120530     c                   exsr      trasvab
047700120530     C                   WHEN      rigTRA = 'P'
047800120530     c                   exsr      traspar
047900120530     C                   WHEN      rigTRA = 'A'
048000120530     c                   exsr      trasarr
048100120530     C                   WHEN      rigTRA = 'E'
048200120530     c                   exsr      traspar
048300120530     c                   If        *in91 = *off  and  *in92 = *off
048400120530     c                   exsr      trasarr
048500120530     c                   Endif
048600120530     c                   ENDSL
048700120530     c                   endsr
048800000323      *--------------------------------------------------------------------------------------------*
048900000404      * Trasmissione in partenza                                            to
049000000323      *--------------------------------------------------------------------------------------------*
049100000323     C     TRASPAR       BEGSR
049200000323      *
049300000404     C                   clear                   filet
049400000404     C                   clear                   rcdt
049500000404      *
049600000405      * Gestione file per trasmissione in partenza
049700000405     c                   exsr      GESfiltrpa
049800000323      *
049900000322      * se 91 acceso vuol dire che non e' stato possibile allocare il membro interessato
050000000324     c                   IF        *in91 = *off
050100000324      *
050200000419      * Verifico se devo riagganciare TITAS
050300000419     C                   IF        WPaas <> TASaas  or  WPlnp <> TASlnp  or
050400000419     C                             WPnrs <> TASnrs  or  WPnsp <> TASnsp
050500000419     c                   clear                   ds_titas
050600000419     c                   clear                   ds_titas_2
050700000419     c                   eval      Waas = WPaas
050800000419     c                   eval      Wlnp = WPlnp
050900000419     c                   eval      Wnrs = WPnrs
051000000419     c                   eval      Wnsp = WPnsp
051100000419     c                   eval      *in92 = *off
051200000419      * se non trovo TITAS accendo *in92                                     interessato
051300000419     C                   exsr      AGGTAS
051400000419     C                   Endif
051500000419      *
051600000419     c                   IF        *in92 = *off
051700000419      *
051800000419      * Scrivo i rcd da trasmettere
051900000328     c                   eval      Wtiporcd = 'P'
052000000328     c                   exsr      wrtrcd
052100000322      *
052200000322      * chiude il membro di trasmissione
052300000404     C                   exsr      closepfpar
052400000404      *
052500000404      * TRASMETTO
052600000404     C                   exsr      trasmi
052700000324      *
052800000324      * Aggiorno i record del fisico trasmessi
052900000419     C                   IF        Wfirig <> 'S'
053000090423     C                   z-add     datcor        RIGdela                        *data trasmissione
053100090423     C                   UPDATE    FnRIG000
053200000419     c                   eval      Wfirig = 'S'
053300000419     c                   ENDIF
053400000404      *
053500000404     c                   ENDIF
053600000419     c                   ENDIF
053700000324      *
053800000322     C                   ENDSR
053900000405      *--------------------------------------------------------------------------------------------*
054000000405      * Trasmissione in arrivo                                              to
054100000405      *--------------------------------------------------------------------------------------------*
054200000405     C     TRASARR       BEGSR
054300000405      *
054400000405     C                   clear                   filet
054500000405     C                   clear                   rcdt
054600000405      *
054700000405      * Gestione file per trasmissione in arrivo
054800000405     c                   exsr      GESfiltrar
054900000405      *
055000000405      * se 91 acceso vuol dire che non e' stato possibile allocare il membro interessato
055100000405     c                   IF        *in91 = *off
055200000419      *
055300000419      * Verifico se devo riagganciare TITAS
055400000419     C                   IF        WAaas <> TASaas  or  WAlnp <> TASlnp  or
055500000419     C                             WAnrs <> TASnrs  or  WAnsp <> TASnsp
055600000419     c                   clear                   ds_titas
055700000419     c                   clear                   ds_titas_2
055800000419     c                   eval      Waas = WAaas
055900000419     c                   eval      Wlnp = WAlnp
056000000419     c                   eval      Wnrs = WAnrs
056100000419     c                   eval      Wnsp = WAnsp
056200000419     c                   eval      *in92 = *off
056300000419      * se non trovo TITAS accendo *in92                                     interessato
056400000419     C                   exsr      AGGTAS
056500000509      * se 92 acceso vuol dire che non ha trovato il record giusto
056600000509      * o la bolla legata è una bolla di recupero da non trasmettere
056700000509     c                   if        *in92
056800000509     c                   clear                   ds_titas
056900000509     c                   clear                   ds_titas_2
057000000509     c                   eval      Waas = WPaas
057100000509     c                   eval      Wlnp = WPlnp
057200000509     c                   eval      Wnrs = WPnrs
057300000509     c                   eval      Wnsp = WPnsp
057400000509     c                   eval      *in92 = *off
057500000509      * se non trovo TITAS accendo *in92                                     interessato
057600000509     C                   exsr      AGGTAS
057700000509     c                   Endif
057800000509      *
057900000419     C                   Endif
058000000419      *
058100000419     c                   IF        *in92 = *off
058200000405      *
058300000405      * Scrivo i rcd da trasmettere
058400131017      * non scrivo ARB se di tratta di bolle con cons.anomala "1-dirottata"
058500131017     c                   if        tascca<>'1'
058600000405     c                   eval      Wtiporcd = 'A'
058700000405     c                   exsr      wrtrcd
058800000405      *
058900000405      * chiude il membro di trasmissione
059000000405     C                   exsr      closepfarr
059100000405      *
059200000405      * TRASMETTO
059300000405     C                   exsr      trasmi
059400131017     c                   endif
059500000419      *
059600000419      * Aggiorno i record del fisico trasmessi
059700000419     C                   IF        Wfirig <> 'S'
059800090423     C                   z-add     datcor        RIGdela                        *data trasmissione
059900090423     C                   UPDATE    FnRIG000
060000000419     c                   eval      Wfirig = 'S'
060100000419     c                   ENDIF
060200000405      *
060300000405     c                   ENDIF
060400000419     c                   ENDIF
060500000405      *
060600000405     C                   ENDSR
060700020304      *--------------------------------------------------------------------------------------------*
060800020925      * Trasmissione msg di P.E.I. e FIVABBFT
060900020304      *--------------------------------------------------------------------------------------------*
061000020304     c     trasvab       begsr
061100020304
061200020925      * recupero dati dal titas da scrivere nei messaggi di P.E.I. e nel FIVAB
061300020304     c                   exsr      recdati
061400020304
061500020304     c                   clear                   rcdvab
061600020827     c                   Movel     '046'         Wmbrvab
061700020304     c                   move      walnav        wmbrvab
061800020304
061900020925      * Gestione file per trasmissione msg e fivab
062000020304     c                   exsr      gesfilvab
062100020304
062200020304      * se 91 spento vuol dire che e' stato possibile allocare il membro interessato
062300020304     c                   IF        *in91 = *off
062400020304
062500020304      * Invio i messaggi di P.E.I.
062600020304     c                   exsr      crtposta
062700020304
062800020304      * Scrivo il rcd da trasmettere
062900020304     c                   exsr      wrtvab
063000020304
063100020304      * chiude il membro di trasmissione
063200020304     c                   exsr      closepfvab
063300020304
063400020304      * Trasmetto
063500020304     c                   exsr      trasmivab
063600020304
063700020304      * Aggiorno i record del fisico trasmessi
063800020304     c                   if        wfirig <> 'S'
063900090423     c                   z-add     datcor        rigdela                        *data trasmissione
064000090423     c                   update    fnrig000
064100020304     c                   eval      wfirig = 'S'
064200020304     c                   endif
064300020304
064400020304     c                   endif
064500020304
064600020304     c                   endsr
064700020304      *
064800000322      *--------------------------------------------------------------------------------------------*
064900000405      * Controllo se la sped. da rigenerare ha dei legami bolla
065000000322      *--------------------------------------------------------------------------------------------*
065100981008     C     legbol        BEGSR
065200000418      *
065300000418     c                   eval      *in81 = *on
065400000418     c                   eval      *in82 = *on
065500000509     c                   eval      *in32 = *off
065600000418      *
065700000418      * controllo se c'è un legame bolla
065800000418     c     klbl          chain     fnlbl01l                           82
065900000418     c   82klbl          chain     fnlbl02l                           81
066000000418      *
066100000418      * n81 oppure n82  c'è legame bolla
066200000418    2c                   if        *in81 = *off  or  *in82 = *off
066300000418      *
066400000418     c                   eval      Wlegame = 'S'
066500000418      *
066600000418      * DEFINISCO LE SPEDIZIONE DA TRASMETTERE
066700000418      *
066800120601   a c                   if        rigcau = 'F'
066900000418      * Mamma originale - PARTENZA
067000000418     c                   eval      WPaas = lblaao
067100000418     c                   eval      WPlnp = lbllpo
067200000418     c                   eval      WPnrs = lblnro
067300000418     c                   eval      WPnsp = lblnso
067400000418      *
067500000418      * Ultima figlia - ARRIVO
067600000509    4c                   DOW       not *in32
067700000418     c                   eval      WAaas = lblaan
067800000418     c                   eval      WAlnp = lbllpn
067900000418     c                   eval      WAnrs = lblnrn
068000000418     c                   eval      WAnsp = lblnsn
068100000509     c     klblA         chain     fnlbl02l                           32
068200000418    4c                   ENDDO
068300020701
068400170710      * controllo l'ultima figlia
068500020701     c                   eval      waas = waaas
068600020701     c                   eval      wlnp = walnp
068700020701     c                   eval      wnrs = wanrs
068800020701     c                   eval      wnsp = wansp
068900020701     c     kbolw         chain     titas30c
069000020701   b c                   if        %found(titas30c)
069100020701     c                   clear                   Dstb
069200020701     c                   eval      W008a = TAStbl
069300020701     c                   eval      XX = 1
069400020701     c     W008a         lookup    TblK(xx)                               31
069500020701     c                   movel     TblD(xx)      Dstb
069600020701      * se bolla recupero prendo la penultima figlia
069700020701   c c                   if        §TBrbl = 'R'
069800020701     c                   eval      WAaas = lblaap
069900020701     c                   eval      WAlnp = lbllpp
070000020701     c                   eval      WAnrs = lblnrp
070100020701     c                   eval      WAnsp = lblnsp
070200020701   c c                   endif
070300020701   b c                   endif
070400020701   a c                   endif
070500000418      *
070600000418    1c                   endif
070700000322      *
070800000323     c                   ENDSR
070900000405      *--------------------------------------------------------------------------------------------*
071000000405      * trasmi - trasmette il membro di trasmissione
071100000405      *--------------------------------------------------------------------------------------------*
071200980828     C     trasmi        BEGSR
071300000405      *
071400120607     c                   if        Wtiporcd='P'
071500000405     C                   MOVEL(P)  'SEDE'        cmzmbd                          *dal membro
071600000405     C                   MOVEL(P)  'SEDE'        cmzmba                          *al membro
071700120607     c                   else
071800120607     C                   MOVEL(P)  'SEDEA'       cmzmbd                          *dal membro
071900120607     C                   MOVEL(P)  'SEDEA'       cmzmba                          *al membro
072000120607     c                   endif
072100000405     C                   CLEAR                   cmzprr                          *prerequisito
072200981222     C                   MOVE      '0000'        cmzori                          *ora invio
072300981222     C                   MOVE      '0000000'     cmzdti                          *data invio
072400990208     C                   MOVEL     'N'           cmzurg                          *urgenza notte
072500981222     C                   MOVEL     'A'           cmzadd                          *record in aggiunta
072600981222     C                   MOVEL     'S'           cmzclr                          *no pulizia file
072700981222     C                   MOVEL     'S'           cmzftr                          *invio da transito
072800000405     C                   CLEAR                   cmzerr                          *flag errore('1')
072900000509     C                   CLEAR                   cmzazn                          *azione
073000000410      *
073100000410      * Destinatario
073200000412     c                   IF        Wtiporcd = 'P'
073300000412     c     TASlnp        chain     azorg
073400000412     C                   ELSE
073500000412     c     TASlna        chain     azorg
073600000412     C                   ENDIF
073700000412     c                   movel     orgfel        cmzdst                          *destinatario
073800000412      *
073900000412      * Cerco il terminal
074000000412     c                   IF        Wtiporcd = 'P'
074100000412     c                   eval      W0030 = TASlnp
074200000412     C                   ELSE
074300000412     c                   eval      W0030 = TASlna
074400000412     C                   ENDIF
074500000412     c                   eval      Wtipodata = 'T'
074600000412     c                   exsr      carter
074700000405      * cerco le librerie e i sistemi infomativi di arrivo
074800020827     C                   CLEAR                   TIBS55DS
074900981218     C                   MOVEL     knsif         i50psi
075000981218     C                   MOVEL     simfel        i50ppo
075100000412     C                   MOVEL     D55tfp        i50apo
075200020827     C                   CALL      'TIBS55R'
075300020827     C                   PARM                    TIBS55DS
075400981222     c                   movel     o50lcl        cmzlcl            1             *locale
075500000419      *
075600000419      * parto dalla libreria di gruppo
075700980928     c                   movel     o50plg        cmzlbd                         *dalla libreria
075800981218     c                   movel     o50asi        cmzsyf                         *sist.informativo
075900000404      *
076000000410     C                   eval      xx = 1
076100000410     C                   DOW       filet(xx) <> *blanks                         *se scritti dei rec
076200000404      *
076300000410     C                   MOVEL     FileT(xx)     cmzfld                          *dal file -trasm-
076400000410     C                   MOVEL     FileR(xx)     cmzfla                          *al file  -ricez-
076500000410     C                   MOVE      Rcdt(xx)      cmznrr                          *record inviati
076600000419      *
076700000419      * trasmetto nella libreria aziendale tranne per i legami (FNLBL00T)
076800000419      * che trasmetto nella libreria di gruppo
076900000419     C                   IF        FileT(xx) = 'FNLBL00T'                        *dal file -trasm-
077000000419     c                   movel     o50alg        cmzlba                         *alla  libreria
077100000419     c                   ELSE
077200000419     c                   movel     o50ala        cmzlba                         *alla  libreria
077300000419     c                   ENDIF
077400000412      * alla trasmissione di BLP/ARB aggancio l'esecuzione della ricezione
077500000413     C                   SELECT
077600000413     c                   WHEN      Wazione <> xx
077700000413     c                   WHEN      Wtiporcd = 'P'
077800000413     C                   movel     'BS22'        cmzazn                          *azione ricezione
077900000413     c                   OTHER
078000000413     C                   movel     'BS23'        cmzazn                          *azione ricezione
078100000413     c                   ENDSL
078200000412      *
078300020827     C                   CALL      'TIBS53C'
078400980911     C                   PARM                    cmzlcl
078500980828     C                   PARM                    cmzdst
078600980828     C                   PARM                    cmzfld
078700980828     C                   PARM                    cmzmbd
078800980828     C                   PARM                    cmzlbd
078900980828     C                   PARM                    cmzfla
079000980828     C                   PARM                    cmzmba
079100980828     C                   PARM                    cmzlba
079200980828     C                   PARM                    cmzori
079300980828     C                   PARM                    cmzdti
079400980828     C                   PARM                    cmzurg
079500980828     C                   PARM                    cmzadd
079600980828     C                   PARM                    cmzclr
079700980828     C                   PARM                    cmzftr
079800980828     C                   PARM                    cmzerr
079900980828     C                   PARM                    cmzazn
080000980828     C                   PARM                    cmzsyf
080100980828     C                   PARM                    cmznrr
080200980828     C                   PARM                    cmzprr
080300980911     C                   PARM                    w236            236
080400000410      *
080500000410     C                   add       1             xx
080600000410     c                   ENDDO
080700000405      *
080800980914     c                   exsr      finsr
080900000405      *
081000000419     C                   clear                   Wazione
081100000419      *
081200980828     C                   ENDSR
081300020304      *--------------------------------------------------------------------------------------------*
081400020925      * Trasmette il file FIVABBFT
081500020304      *--------------------------------------------------------------------------------------------*
081600020304     c     trasmivab     begsr
081700020304
081800020827     c                   Clear                   Wmbrt
081900020827     c                   Eval      Wmbrt = 'M' + Wmbrvab
082000020304     c                   movel(p)  wmbrt         cmzmbd                          *dal membro
082100020925     c                   movel(p)  'FIVAB00F'    cmzmba                          *al membro
082200020304     c                   clear                   cmzprr                          *prerequisito
082300020304     c                   move      '0000'        cmzori                          *ora invio
082400020304     c                   move      '0000000'     cmzdti                          *data invio
082500020304     c                   movel     'N'           cmzurg                          *urgenza notte
082600020304     c                   movel     'A'           cmzadd                          *record in aggiunta
082700020304     c                   movel     'S'           cmzclr                          *no pulizia file
082800020304     c                   movel     'S'           cmzftr                          *invio da transito
082900020304     c                   clear                   cmzerr                          *flag errore('1')
083000020304     c                   clear                   cmzazn                          *azione
083100020304
083200020304      * Destinatario
083300020304     c                   move      walnav        keyorg
083400020304     c     keyorg        chain     azorg
083500020304     c                   movel     orgfel        cmzdst                          *destinatario
083600020304
083700020304      * Cerco il terminal
083800020618     c                   movel     walnav        w0030
083900020304     c                   eval      Wtipodata = 'T'
084000020304     c                   exsr      carter
084100020304      * cerco le librerie e i sistemi infomativi di arrivo
084200020827     c                   clear                   tibs55ds
084300020304     c                   movel     knsif         i50psi
084400020304     c                   movel     simfel        i50ppo
084500020304     c                   movel     d55tfp        i50apo
084600020827     c                   call      'TIBS55R'
084700020827     c                   parm                    tibs55ds
084800020304     c                   movel     o50lcl        cmzlcl            1             *locale
084900020304
085000020304      * parto dalla libreria di gruppo
085100020304     c                   movel     o50plg        cmzlbd                         *dalla libreria
085200020304     c                   movel     o50asi        cmzsyf                         *sist.informativo
085300020304
085400020925     c                   movel     'FIVABBFT'    cmzfld                          *dal file -trasm-
085500020925     c                   movel     'FIVAB00F'    cmzfla                          *al file  -ricez-
085600020304     c                   move      rcdvab        cmznrr                          *record inviati
085700020304
085800020304      * trasmetto nella libreria aziendale
085900021009     c                   movel     o50alg        cmzlba                         *alla  libreria
086000020304
086100020827     C                   CALL      'TIBS53C'
086200020304     C                   PARM                    cmzlcl
086300020304     C                   PARM                    cmzdst
086400020304     C                   PARM                    cmzfld
086500020304     C                   PARM                    cmzmbd
086600020304     C                   PARM                    cmzlbd
086700020304     C                   PARM                    cmzfla
086800020304     C                   PARM                    cmzmba
086900020304     C                   PARM                    cmzlba
087000020304     C                   PARM                    cmzori
087100020304     C                   PARM                    cmzdti
087200020304     C                   PARM                    cmzurg
087300020304     C                   PARM                    cmzadd
087400020304     C                   PARM                    cmzclr
087500020304     C                   PARM                    cmzftr
087600020304     C                   PARM                    cmzerr
087700020304     C                   PARM                    cmzazn
087800020304     C                   PARM                    cmzsyf
087900020304     C                   PARM                    cmznrr
088000020304     C                   PARM                    cmzprr
088100020304     C                   PARM                    w236            236
088200020304
088300020304     c                   exsr      finsrvab
088400020304
088500020304     c                   endsr
088600000412      *--------------------------------------------------------------------------------------------*
088700000412      * carfil - carica filiali mper trasmissione dati
088800000412      *--------------------------------------------------------------------------------------------*
088900000412     C     carter        BEGSR
089000000412      *
089100000412     c                   clear                   FNLV55DS
089200000412     c                   movel     'P'           d55tpt
089300000412     c                   movel     W0030         d55lin
089400000412     c                   IF        Wtipodata = 'T'
089500000412     c                   movel     datcor        d55drf
089600000412     C                   ELSE
089700000412     c                   z-add     TASmgs        d55drf
089800000412     c                   movel     TASaas        d55drf
089900000412     c                   ENDIF
090000000412     c                   call      'FNLV55R'
090100000412     c                   parm                    FNLV55DS
090200000412      *
090300000412     c                   if        d55err <> *blanks
090400000412     c                   movel     W0030         d55tfp
090500000412     c                   endif
090600000412      *
090700000412     c                   endsr
090800000405      *--------------------------------------------------------------------------------------------*
090900000405      * finsr - disalloco membri di trasmissione
091000000405      *--------------------------------------------------------------------------------------------*
091100000322     C     finsr         BEGSR
091200000405      *
091300000413     c                   Do        wnumele       XX
091400000405      *
091500000405     c                   If        Wtiporcd = 'P'
091600000405     C                   MOVEL(P)  cmpDLC(xx)    comman
091700000405     c                   Else
091800000405     C                   MOVEL(P)  cmaDLC(xx)    comman
091900000405     c                   Endif
092000000405      *
092100000405     C                   CALL      'QCMDEXC'                            20
092200000405     C                   PARM                    comman
092300000405     C                   PARM                    lenght
092400000406      *
092500000405     c                   Enddo
092600000405      *
092700000322     C                   ENDSR
092800020304      *--------------------------------------------------------------------------------------------*
092900020925      * Disalloco membri di trasmissione FIVABBFT
093000020304      *--------------------------------------------------------------------------------------------*
093100020304     c     finsrvab      begsr
093200020304
093300020304     c                   movel(p)  cmdvab(5)     comman
093400020827     c                   eval      %subst(comman:54:6) = wmbrvab
093500020304     c                   call      'QCMDEXC'                            20
093600020304     c                   parm                    comman
093700020304     c                   parm                    lenght
093800020304
093900020304     c                   endsr
094000000404      *--------------------------------------------------------------------------------------------*
094100000404      * Scrittura archivi da trasmettere in partenza
094200000404      *--------------------------------------------------------------------------------------------*
094300000404     C     wrtrcd        BEGSR
094400000404      *
094500020204     C                   clear                   DSBL4A
094600000404     C                   clear                   DSBL4E
094700020204     C                   clear                   DSBL49
094800000404     C                   clear                   DTA4A
094900000404     C                   clear                   DS_titaa_o
095000000404     C                   clear                   DS_titaa_m
095100000404     C                   clear                   Wassegno
095200000413     C                   clear                   FIAR7000
095300000413     C                   clear                   FIAR6000
095400000413     C                   clear                   FNARB000
095500060221     C                   clear                   FIAR4000
095600051116     C                   clear                   FiAR9000
095700000413     C                   clear                   FNART000
095800000413     C                   clear                   FNBLP000
095900000413     C                   clear                   FNBLT000
096000000404      *
096100000419      * Leggo file clienti non codificati
096200000404     C     kspe          setll     TITAA30C
096300000404     C     kspe          reade     TITAA30C
096400000404     C                   DOW       not %EOF
096500000412     C                   SELECT
096600000404     c                   WHEN       TAAtrc = 'O'
096700000412     c                   movel     DS_titaa      DS_TITAA_O
096800000404     c                   WHEN       TAAtrc = 'M'
096900000412     c                   movel     DS_titaa      DS_TITAA_M
097000000404     C                   ENDSL
097100000404     C     kspe          reade     TITAA30C
097200000404     C                   ENDDO
097300000404      *
097400000404      * Leggo file C/assegno
097500000404     C     kspe          chain     TNCSB000
097600000407     c                   SELECT
097700000407     C                   WHEN      not %found
097800000413     C                   WHEN      CSBsta = 9
097900000407     C                   eval      Wassegno = 'A'
098000000407     C                   OTHER
098100000407     C                   eval      Wassegno = 'S'
098200000407     c                   ENDSL
098300000404      *
098400000404      * Leggo file riferimenti
098500000404     C                   eval      Ktrc = 'A'
098600000404     C     kta4          chain     TITA430C
098700000404     C                   If          %found
098800000404     C                   movel     TA4not        Dta4A
098900000404     c                   Endif
099000070927      * Leggo file riferimenti
099100070927     C                   eval      Ktrc = 'E'
099200070927     C     kta4          chain     TITA430C
099300070927     C                   If          %found
099400070927     C                   movel     TA4not        dsbl4E
099500070927     c                   Endif
099600000404      *
099700000404      * FNARB/FNBLP
099800000404     C                   eval      ARBaas = TASaas
099900000404     C                   eval      ARBlnp = TASlnp
100000000404     C                   eval      ARBnrs = TASnrs
100100000404     C                   eval      ARBnsp = TASnsp
100200000404     C                   eval      ARBmgs = TASmgs
100300000404     C                   eval      ARBfns = TASfns
100400000404     C                   eval      ARBfbr = TASfbr
100500000404     C                   eval      ARBfdn = TASfdn
100600000404     C                   eval      ARBlna = TASlna
100700000404     C                   eval      ARBznc = TASznc
100800000404     C                   eval      ARBksc = TASksc
100900000404     C                   eval      ARBfap = TASfap
101000000404     C                   eval      ARBrsd = TASrsd
101100000404     C                   eval      ARBind = TASind
101200000404     C                   eval      ARBcad = TAScad
101300000404     C                   eval      ARBlod = TASlod
101400000404     C                   eval      ARBprd = TASprd
101500000404     C                   eval      ARBnzd = TASnzd
101600000404     C                   eval      ARBfin = TASfin
101700000404     C                   eval      ARBgc1 = TASgc1
101800000404     C                   eval      ARBgc2 = TASgc2
101900000404     C                   eval      ARBctr = TASctr
102000000404     C                   eval      ARBcts = TAScts
102100000404     C                   eval      ARBftm = TASftm
102200000404     C                   eval      ARBtsp = TAStsp
102300160122     C                   eval      ARBnas = §TA4anas
102400000404     C                   eval      ARBncl = TASncl
102500000404     C                   eval      ARBncp = TASncp
102600000404     C                   eval      ARBpkc = TASpkc
102700000404     C                   eval      ARBpkb = TASpkb
102800000404     C                   eval      ARBpkf = TASpkf
102900000404     C                   eval      ARBncr = TASncr
103000000404     C                   eval      ARBvlc = TASvlc
103100000404     C                   eval      ARBfvb = TASfvb
103200000404     C                   eval      ARBvlb = TASvlb
103300000404     C                   eval      ARBfvf = TASfvf
103400000404     C                   eval      ARBvlf = TASvlf
103500000404     C                   eval      ARBqft = TASqft
103600001120     C                   If        TASfcm <> 'F'
103700000404     C                   eval      ARBias = TASias
103800000404     C                   eval      ARBvas = TASvas
103900001120     c                   EndIf
104000000404     C                   eval      ARBccm = TASccm
104100000404     C                   eval      ARBrmn = TASrmn
104200000404     C                   eval      ARBrma = §TA4Arma
104300000404     C                   eval      ARBrmo = OTAArsc
104400000404     C                   eval      ARBfls = TASfls
104500000404     C                   eval      ARBncd = TASncd
104600000404     C                   eval      ARBnca = TASnca
104700050113     C                   clear                   ARBxco
104800000404     C                   eval      ARBffd = TASffd
104900000404     C                   eval      ARBtcr = TAStcr
105000000404     C                   eval      ARBdcr = TASdcr
105100000404     C                   eval      ARBhcr = TAShcr
105200000404     C                   eval      ARBdti = TASdti
105300000404     C                   eval      ARBhti = TAShti
105400000404     C                   eval      ARBdce = TASdce
105500000404     C                   eval      ARBnpi = TASnpi
105600000404     C                   eval      ARBnci = TASnci
105700000404     C                   eval      ARBnrc = TASnrc
105800000404     C                   eval      ARBdbr = TASdbr
105900000404     C                   eval      ARBduc = TASduc
106000040528     c                   Clear                   ArbFlp
106100000404     C                   eval      ARBpdc = TASpdc
106200000404     C                   eval      ARBndc = TASndc
106300000404     C                   eval      ARBddc = TASddc
106400000404     C                   eval      ARBdcp = TASdcp
106500000404     C                   eval      ARBdcm = TASdcm
106600000404     C                   eval      ARBhmc = TAShmc
106700000404     C                   eval      ARBtc1 = TASftc
106800000404     C                   eval      ARBtc2 = TAStc2
106900000404     C                   eval      ARBcca = TAScca
107000000404     C                   eval      ARBgga = TASgga
107100000404     C                   eval      ARBgma = TASgma
107200100604     c                   clear                   ds7R
107300100604     c                   If        ArbGma <>*blanks
107400100604     C                   eval      KCOD = '7R'
107500100604     C                   movel(P)  arbgma        KKEY
107600100604     C     keytab        chain     TABEL
107700100604     c                   IF        %found
107800100604     C                   movel     TBLUNI        ds7R
107900100604     c                   ENDIF
108000100604     c                   ENDIF
108100100604
108200100604     c                   If        §7rRC='S'
108300030801     c                   Clear                   ArbGma
108400030801     c                   EndIf
108500100604     c
108600000404     C                   eval      ARBgva = TASgva
108700000623     C                   Z-ADD     TASSTP        ARBSTP
108800000404     C                   eval      ARBctm = TASctm
108900000404     C                   eval      ARBdim = TASdim
109000000404     C                   eval      ARBdrt = TASdrt
109100000404     C                   eval      ARBflb = TASflb
109200000404     C                   eval      ARBfpp = TASfpp
109300000404     c                   eval      ARBnrt = TASnrt
109400040528     C                   eval      ARBtfp = TAStfp
109500000404     C                   eval      ARBtfa = TAStfa
109600000404     C                   eval      ARBdse = TASduc
109700000404     C                   eval      ARBdpc = TASduc
109800000404     C                   eval      ARBdam = TASdti
109900000404     C                   eval      ARBpdr = TASpdr
110000000404     C                   eval      ARBfst = TASll1
110100000404     C                   eval      ARBnfv = 99998
110200000404     C                   eval      ARBntc = 1
110300000404     C                   eval      ARBft3 = 'T'
110400000404     C                   eval      ARBdt3 = TASdti
110500000404     C                   eval      ARBamp = 1
110600000404     C                   eval      ARBama = 3
110700000404     C                   eval      ARBacc = 'S'
110800000404     C                   eval      ARBaca = 'S'
110900000404     C                   eval      ARBicc = 'Y'
111000000404     C                   eval      ARBica = 'Y'
111100020726     C                   eval      ARBifp = *zeros
111200000404     C                   eval      ARBfdc = 'S'
111300000428     C                   eval      ARBnmi = 989898
111400121023     c                   if        rigcau = '3'
111500020204     C                   eval      ARBnmi = 979797
111600020204     c                   end
111700000428     C                   eval      ARBdmi = datcor
111800000407      *
111900000404     C                   IF        MTAArsc <> *blanks
112000010726     C                   eval      ARBrsm = mTAArsc
112100010726     C                   eval      ARBinm = mTAAind
112200010726     C                   eval      ARBcam = mTAAcap
112300010726     C                   eval      ARBlom = mTAAloc
112400010726     C                   eval      ARBprm = mTAAprv
112500010726     C                   eval      ARBnzm = mTAAnaz
112600000404     C                   ELSE
112700000419     c                   z-add     TASccm        I69kac
112800000419     c                   z-add     TASccm        I69kin
112900000404     c                   EXSR      CTR_ANAGRA
113000000404     C                   eval      ARBrsm = ACOrag
113100000404     C                   eval      ARBinm = INDvia
113200000404     C                   eval      ARBcam = INDcae
113300000404     C                   eval      ARBlom = INDcit
113400000404     C                   eval      ARBprm = INDprv
113500000404     C                   eval      ARBnzm = INDsta
113600000404     C                   ENDIF
113700000404      *
113800020204     C                   eval      ARBcbo = TAScbo
113900020204      * Per contrassegni omessi
114000121023     C                   IF        RIGcau = '3'
114100020204     c                   select
114200020204     c                   when      %subst(tastbl: 1: 1) = 'F'
114300020204     C                   movel(p)  '4'           ARBcbo
114400020204     c                   when      %subst(tastbl: 1: 1) = 'A'
114500020204     C                   movel(p)  '6'           ARBcbo
114600020204     c                   endsl
114700020204     C                   clear                   ARBndc
114800000404     C                   eval      ARBicc = 'R'
114900000404     C                   ENDIF
115000000404      *
115100000404     c                   eval      W0030 = TASlnp
115200000412     c                   clear                   Wtipodata
115300000412     c                   exsr      carter
115400000404     c                   eval      ARBfle = d55tfp
115500000404      *
115600000404     c                   eval      W0030 = TASlna
115700000412     c                   clear                   Wtipodata
115800000412     c                   exsr      carter
115900000404     c                   eval      ARBfea = d55tfp
116000120601     c* recupero capofila della linea di arrivo per arbifp/Ipp
116100120606     c                   if        tasddc>0
116200120601     c                   clear                   fnlv55ds
116300120601     c                   eval      d55lin=taslna
116400120601     c                   eval      d55tpt='6'
116500120601     c                   eval      d55drf=tasddc
116600120601     c                   call      'FNLV55R'
116700120601     c                   parm                    FNLV55DS
116800120601     c                   eval      arbifp=d55tfa
116900120601     c                   eval      arbipp=arbifp
117000120606     c                   endif
117100000410      *
117200000414      * FIAR6-7 1a BOLLA
117300000414     c                   exsr      AGGfatt1
117400000414      *
117500000414      * Verifico se il tipo porto è tra quelli convertibili
117600000417     c                   clear                   DS3a
117700000417     c                   eval      xx = 1
117800000417     C     TAScbo        lookup    T3aK(xx)                               31
117900000417      *
118000000417     C                   if        *in31 = *on
118100000417     c                   movel     T3aD(xx)      DS3a
118200000414     C                   SELECT
118300000417      *  non convertibile
118400000417     C                   WHEN      §3aCfr = *blanks
118500000414      *  se il C/a è annullato converto il tipo bolla
118600000414     C                   WHEN      Wassegno = 'A'
118700000417     C                   movel     §3aCfr        ARBcbo
118800000417      *  FIAR6-7  2a BOLLA se esiste 2a bolla
118900000417     c                   WHEN      $TASaas > *zeros
119000000414     c                   exsr      AGGfatt2
119100000414     c                   ENDSL
119200000417     c                   endif
119300000404      *
119400051116      * FIAR9
119500000404     C                   IF        Wassegno = 'S'
119600000404     C                   eval      AR9aas = TASaas
119700000404     C                   eval      AR9lnp = TASlnp
119800000404     C                   eval      AR9nrs = TASnrs
119900000404     C                   eval      AR9nsp = TASnsp
120000000404     C                   eval      AR9cas = CSBcas
120100000404     C                   eval      AR9vca = CSBvca
120200000404     C                   eval      AR9cmb = CSBcmb
120300000404     C                   eval      AR9gca = CSBgca
120400151202     C                   If        RIGcau = '3' or
120500151202     C                             RIGcau = '8'
120600121023     c                   movel     rigflo        ar9tic
120700151202     c                   Else
120800151202     C                   If        RIGcau <> 'A'
120900151202     c                   eval      AR9tic = CSBtpa + CSBtpi
121000151202     c                   end
121100151202     c                   end
121200051116      *
121300051116     C                   WRITE     FiAR9000
121400051116     C                   eval      WfileT = 'FiAR900T'
121500051116     C                   eval      WfileR = 'FiAR900R'
121600000412     C                   exsr      AGGnrec
121700000404      *
121800000404     C                   ENDIF
121900000404      *
122000060221      * FIAR4
122100000404     C                   eval      AR4aas = TASaas
122200000404     C                   eval      AR4lnp = TASlnp
122300000404     C                   eval      AR4nrs = TASnrs
122400000404     C                   eval      AR4nsp = TASnsp
122500000404      *   Tipo Record "A"
122600031029     c                   Select
122700031029     c                   When      TasLl1 = 'C'
122800031029     c                   Eval      §B4Abm = 'Q'
122900031029     c                   When      TasLl1 = 'S'
123000031029     c                   Eval      §B4Abm = 'P'
123100050301     c                   When      TasLl1 = 'N' or Tasll1 = 'Y'
123200031029     c                   Eval      §B4Abm = 'J'
123300031113     c                   When      TasLl1 = 'K'
123400031113     c                   Eval      §B4Abm = 'W'
123500031029     c                   Other
123600000404     C                   eval      §B4Abm = TASll1
123700031029     c                   EndSl
123800000404     C                   eval      §B4Aso = '-'
123900060221     C                   movel(p)  DSbl4A        AR4not
124000000404     C                   eval      AR4trc = 'A'
124100060221     C                   WRITE     FIAR4000
124200060221     C                   eval      WfileT = 'FIAR400T'
124300060221     C                   eval      WfileR = 'FIAR400R'
124400000410     C                   exsr      AGGnrec
124500000404      *   Tipo Record "F"
124600000404     C                   eval      Ktrc = 'F'
124700000404     C     kta4          chain     TITA430C
124800000404     C                   If          %found
124900060221     c                   clear                   ar4not
125000000404     C                   eval      AR4not = TA4not
125100000404     C                   eval      AR4trc = TA4trc
125200000404      *
125300060221     C                   WRITE     FIAR4000
125400060221     C                   eval      WfileT = 'FIAR400T'
125500060221     C                   eval      WfileR = 'FIAR400R'
125600000412     C                   exsr      AGGnrec
125700000404      *
125800000404     C                   Endif
125900060530      *   Tipo Record "I"
126000060530     C                   eval      Ktrc = 'I'
126100060530     C     kta4          chain     TITA430C
126200060530     C                   If          %found
126300060530     c                   clear                   ar4not
126400060530     C                   eval      AR4not = TA4not
126500060530     C                   eval      AR4trc = TA4trc
126600060530      *
126700060530     C                   WRITE     FIAR4000
126800060530     C                   eval      WfileT = 'FIAR400T'
126900060530     C                   eval      WfileR = 'FIAR400R'
127000060530     C                   exsr      AGGnrec
127100060530      *
127200060530     C                   Endif
127300000404      *   Tipo Record "E"
127400070927     c
127500070927     c* La dsble$ è già impostata per cui non devo scrivere nulla
127600070927     c                   if        §b4erp<>*blanks
127700000404      *
127800000404     C     TASlna        chain     AZORG
127900000404     C                   If        ORGdit = 'SDI'  and  ORGfl1 = 'E'
128000000404     C                   eval      §B4Ema = 'A'
128100000404     C                   eval      §B4Eat = 'P'
128200000404     C                   eval      §B4Est = 'S'
128300000404     C                   Endif
128400060221     C                   movel(P)  DSbl4E        AR4not
128500000404     C                   eval      AR4trc = 'E'
128600000404      *
128700060221     C                   WRITE     FIAR4000
128800060221     C                   eval      WfileT = 'FIAR400T'
128900060221     C                   eval      WfileR = 'FIAR400R'
129000000412     C                   exsr      AGGnrec
129100000412      *
129200000412     C                   Endif
129300000404      *
129400000404      *   Tipo Record "8"
129500000404     C                   eval      AR4trc = '8'
129600060221     c                   clear                   ar4not
129700000404     C                   eval      AR4not = 'BOLLA RIGENERATA DALLA SEDE'
129800000404      *
129900060221     C                   WRITE     FIAR4000
130000060221     C                   eval      WfileT = 'FIAR400T'
130100060221     C                   eval      WfileR = 'FIAR400R'
130200000412     C                   exsr      AGGnrec
130300020204      *   Tipo Record "9" per contrassegni omessi
130400121023     c                   if        rigcau = '3'
130500020204     C                   eval      AR4trc = '9'
130600020204     C                   eval      §b4ds9 = 'PER C/ASSEGNO OMESSO'
130700020206     C                   eval      §b4dt9 = datcor
130800060221     c                   clear                   ar4not
130900020204     C                   eval      AR4not = dsbl49
131000020204      *
131100060221     C                   WRITE     FIAR4000
131200060221     C                   eval      WfileT = 'FIAR400T'
131300060221     C                   eval      WfileR = 'FIAR400R'
131400020204     C                   exsr      AGGnrec
131500020204     C                   Endif
131600111011      *   Tipo Record "D"
131700111011     C                   eval      Ktrc = 'D'
131800111011     C     kta4          chain     TITA430C
131900111011     C                   If          %found
132000111011     c                   clear                   ar4not
132100111011     C                   eval      AR4not = TA4not
132200111011     C                   eval      AR4trc = TA4trc
132300111011      *
132400111011     C                   WRITE     FIAR4000
132500111011     C                   eval      WfileT = 'FIAR400T'
132600111011     C                   eval      WfileR = 'FIAR400R'
132700111011     C                   exsr      AGGnrec
132800111011      *
132900111011     C                   Endif
133000020204      *                                                                   CC
133100000404      * FNBLT/FNART
133200000404     C                   eval      ARTaas = TASaas
133300000404     C                   eval      ARTlnp = TASlnp
133400000404     C                   eval      ARTnrs = TASnrs
133500000404     C                   eval      ARTnsp = TASnsp
133600000419     C                   eval      ARTfls = TASfls
133700000419     C                   eval      ARTlna = TASlna
133800000404     C                   eval      ARTdfv = ARBduc
133900000518     C                   eval      ARTnpp = 86
134000000404     C                   eval      ARTnfv = 99998
134100000404     C                   eval      ARTdse = ARBdse
134200000518     C                   eval      ARTnpe = 86
134300000404     C                   eval      ARTdam = ARBdam
134400000404     C                   eval      ARTdcm = ARBdcm
134500000518     C                   eval      ARTnap = 86
134600000404     C                   eval      ARTabn = 'S'
134700000404      *   Numero segnacollo: se sequenziale sviluppo da titas
134800000404      *                      se non sequenziale sviluppo da titat
134900000404     C                   IF         TASFNS = 'S'
135000000404     c     kspe          setll     TITAT30c
135100000404     c     kspe          reade     TITAT30c
135200000404     c                   DOW       not %EOF
135300000404     c                   eval       ARTnsc = TATnsc
135400000404     c                   If        Wtiporcd = 'P'
135500000404     C                   WRITE     FNBLT000
135600000412     C                   eval      WfileT = 'FNBLT00T'
135700000412     C                   eval      WfileR = 'FNBLT00R'
135800000404     c                   Else
135900000404     C                   WRITE     FNART000
136000000412     C                   eval      WfileT = 'FNART00T'
136100000412     C                   eval      WfileR = 'FNART00R'
136200000404     C                   Endif
136300000412     C                   exsr      AGGnrec
136400000404     c     kspe          reade     TITAT30c
136500000404     C                   ENDDO
136600000404      *
136700000404     C                   ELSE
136800000404      *
136900000404     C                   If        TASncd > 0
137000000404      *
137100000404     C                   If          TASnca = 0
137200000404     C                   eval      TASnca = TASncd
137300000404     C                   Endif
137400000404      *
137500000404     C     TASNCD        DO        TASNCA        WSEGNA
137600000404     c                   eval       ARTnsc = Wsegna
137700000404     c                   If        Wtiporcd = 'P'
137800000404     C                   WRITE     FNBLT000
137900000412     C                   eval      WfileT = 'FNBLT00T'
138000000412     C                   eval      WfileR = 'FNBLT00R'
138100000404     c                   Else
138200000419     C                   WRITE     FNART000
138300000419     C                   eval      WfileT = 'FNART00T'
138400000419     C                   eval      WfileR = 'FNART00R'
138500000404     C                   Endif
138600000412     C                   exsr      AGGnrec
138700000404     C                   ENDDO
138800000404      *
138900000404     C                   Endif
139000000404     C                   ENDIF
139100030110      *
139200030110      * FIAR5
139300030110     c                   exsr      srFIAR5
139400000412      *
139500000412      * Scrivo ARB/BLP. Devono essere gli ultimi file perchè a questi
139600000412      *                 è legata la call all'azione di ricezione.
139700000412     c                   If        Wtiporcd = 'P'
139800000412     C                   eval      BLPsop = '-'
139900000412     C                   WRITE     FNBLP000
140000000412     C                   eval      WfileT = 'FNBLP00T'
140100000412     C                   eval      WfileR = 'FNBLP00R'
140200000412     c                   Else
140300000412     C                   WRITE     FNARB000
140400000412     C                   eval      WfileT = 'FNARB00T'
140500000412     C                   eval      WfileR = 'FNARB00R'
140600000412     c                   Endif
140700000412     C                   exsr      AGGnrec
140800000412     c                   z-add     XX            Wazione
140900000404      *
141000000404     c                   endsr
141100020304      *--------------------------------------------------------------------------------------------*
141200020304      * Recupero dati dal titas
141300020304      *--------------------------------------------------------------------------------------------*
141400020304     c     recdati       begsr
141500020304
141600020304      * Verifico se devo riagganciare TITAS per recuperare i dati che servono
141700020304      * mamma originale
141800020304     c                   if        wpaas <> tasaas  or  wplnp <> taslnp  or
141900020304     c                             wpnrs <> tasnrs  or  wpnsp <> tasnsp
142000020304     c                   clear                   ds_titas
142100020304     c                   clear                   ds_titas_2
142200020304     c                   eval      waas = wpaas
142300020304     c                   eval      wlnp = wplnp
142400020304     c                   eval      wnrs = wpnrs
142500020304     c                   eval      wnsp = wpnsp
142600020304     c                   eval      *in92 = *off
142700020304      * se non trovo TITAS accendo *in92                                     interessato
142800020304     c                   exsr      AGGTAS
142900020304     c                   endif
143000020304
143100020304     c                   if        *in92 = *off
143200020304
143300020304     c                   movel     tasaas        wpaasv
143400020304     c                   movel     taslnp        wplnpv
143500020304     c                   movel     tasnrs        wpnrsv
143600020304     c                   movel     tasnsp        wpnspv
143700020304
143800090423      * Se tipo richiesta 'F'
143900090423     c                   If        Rigcau = 'F'
144000090423      * addebito mittente
144100090423     c                   if        rigadd = 'M'
144200020903     c                   movel     tasccm        wpkscv
144300040511      * ragione sociale mittente originale  valorizzato con la rag.sociale destinatario
144400040511     c                   movel     tasrsd        Wvabrmo
144500020903     c                   EndIf
144600090423      * addebito al destinatario
144700090423     c                   If        rigadd = 'D'
144800040517     c                   movel     tasksc        wpkscv
144900040511      * ragione sociale mittente originale  valorizzato con la rag.sociale mittente
145000040513     c                   Eval      ktrc = 'M'
145100040513     c     kta4          Chain     Titaa30c
145200040513     c                   If        %Found(Titaa30c)
145300040513     c                   eval      Wvabrmo = TaaRsc
145400040513     c                   EndIf
145500090423     c                   endif
145600030514      * riferimento mittente alfanumerico   valorizzato con rif.mittente numerico
145700030515     c                   eval      WVABrma= %editc(tasrmn:'Z')
145800030508     c                   endif
145900020304
146000020304     c                   movel     taslna        wplnav
146100030508
146200020304     c                   clear                   wlbdat
146300020304     c                   movel     tasaas        g02inv
146400020304     c                   move      tasmgs        g02inv
146500020304     c                   movel     '3'           g02err
146600020304     c                   call      'XSRDA8'
146700020304     c                   parm                    wlbdat
146800020304     c                   move      g02dat        wpdspv
146900020304      * ragione sociale
147000020304     c                   move      wpkscv        I69kac
147100020304     c                   EXSR      CTR_ANAGRA
147200020304     c                   eval      wpragv = acorag
147300020304
147400020304     c                   endif
147500020304
147600020304      * Verifico se devo riagganciare TITAS per recuperare i dati che servono
147700020304      * ultima figlia
147800020304     c                   if        waaas <> tasaas  or  walnp <> taslnp  or
147900020304     c                             wanrs <> tasnrs  or  wansp <> tasnsp
148000020304     c                   clear                   ds_titas
148100020304     c                   clear                   ds_titas_2
148200020304     c                   eval      waas = waaas
148300020304     c                   eval      wlnp = walnp
148400020304     c                   eval      wnrs = wanrs
148500020304     c                   eval      wnsp = wansp
148600020304     c                   eval      *in92 = *off
148700020304      * se non trovo TITAS accendo *in92                                     interessato
148800020304     c                   exsr      AGGTAS
148900020304     c                   endif
149000020304
149100020304     c                   if        *in92 = *off
149200020304
149300020304     c                   movel     tasaas        waaasv
149400020304     c                   movel     taslnp        walnpv
149500020304     c                   movel     tasnrs        wanrsv
149600020304     c                   movel     tasnsp        wanspv
149700020304
149800020304     c                   movel     taslna        walnav
149900020304     c                   movel     taspdc        wapdcv
150000020304     c                   movel     tasndc        wandcv
150100020304
150200020304     c                   clear                   wlbdat
150300020304     c                   movel     tasddc        g02inv
150400020304     c                   movel     '3'           g02err
150500020304     c                   call      'XSRDA8'
150600020304     c                   parm                    wlbdat
150700020304     c                   move      g02dat        waddcv
150800020304
150900020304     c                   clear                   wlbdat
151000020304     c                   movel     tasaas        g02inv
151100020304     c                   move      tasmgs        g02inv
151200020304     c                   movel     '3'           g02err
151300020304     c                   call      'XSRDA8'
151400020304     c                   parm                    wlbdat
151500020304     c                   move      g02dat        wadspv
151600020304
151700020304     c                   endif
151800020304
151900020304     c                   endsr
152000020304      *--------------------------------------------------------------------------------------------*
152100020304      * Scrittura messaggi P.E.I.
152200020304      *--------------------------------------------------------------------------------------------*
152300020304     c     crtposta      begsr
152400020304
152500090423     c                   movel     rigfil        wrigpor
152600020304
152700130409     C* Inizializzo variabili
152800130409     C                   movel     *blanks       wrkEmlTo        253
152900130409     C                   movel     *blanks       wrkEmlCc        253
153000130409     C                   movel     *blanks       wrkMsg         5000
153100130409     C                   movel     *blanks       wrkOgg           44
153200130409     C                   movel     *blanks       wrkesito          1
153300130409     c                   clear                   ym5t01           70
153400130409     c                   clear                   ym5t02           70
153500130409     c                   clear                   ym5t03           70
153600130409     c                   clear                   ym5t04           70
153700130409     c                   clear                   ym5t05           70
153800130409     c                   clear                   ym5t06           70
153900130411     c                   clear                   vuoto85          85
154000020304
154100130409     c                   eval      wrkogg = PE_testa1
154200020304
154300020304      * Preparo il messaggio uguale sia per la partenza che per l'arrivo
154400020304     c                   eval      ym5t01 = PE_riga1
154500020304     c                   eval      %subst(ym5t01:9:3) = wrigpor
154600020304
154700020304     c                   eval      ym5t02 = PE_riga2
154800020305     c                   eval      %subst(ym5t02:29:3) = wplnpv
154900020305     c                   eval      %subst(ym5t02:33:2) = wpnrsv
155000020305     c                   eval      %subst(ym5t02:36:7) = wpnspv
155100020304     c                   movel     wpdspv        wdatav
155200020305     c                   eval      %subst(ym5t02:48:2) = wgg
155300020305     c                   eval      %subst(ym5t02:51:2) = wmm
155400020305     c                   eval      %subst(ym5t02:54:4) = waa
155500020304
155600020304     c                   eval      ym5t03 = PE_riga3
155700020304     c                   eval      %subst(ym5t03:9:7) = wpkscv
155800020305     c                   eval      %subst(ym5t03:17:35) = wpragv
155900020305
156000020305      * se devo scrivere anche la spedizione figlia
156100020305     c                   if        wpaasv <> waaasv or  wplnpv <> walnpv or
156200020305     c                             wpnrsv <> wanrsv or  wpnspv <> wanspv
156300020305      * aggiungo righe della spedizione figlia
156400020305     c                   eval      ym5t04 = PE_riga4
156500020305     c                   eval      %subst(ym5t04:26:3) = walnpv
156600020305     c                   eval      %subst(ym5t04:30:2) = wanrsv
156700020305     c                   eval      %subst(ym5t04:33:7) = wanspv
156800020305     c                   movel     wadspv        wdatav
156900020305     c                   eval      %subst(ym5t04:45:2) = wgg
157000020305     c                   eval      %subst(ym5t04:48:2) = wmm
157100020305     c                   eval      %subst(ym5t04:51:4) = waa
157200020305
157300020305     c                   eval      ym5t05 = pe_riga5
157400020304
157500020305     c                   eval      ym5t06 = pe_riga6
157600020305     c                   eval      %subst(ym5t06:25:7) = wandcv
157700020304     c                   movel     waddcv        wdatav
157800020305     c                   eval      %subst(ym5t06:37:2) = wgg
157900020305     c                   eval      %subst(ym5t06:40:2) = wmm
158000020305     c                   eval      %subst(ym5t06:43:4) = waa
158100020305     c                   eval      %subst(ym5t06:59:7) = wapdcv
158200020305
158300130409     c                   eval      wrkmsg=ym5t01+ym5t02+ym5t03+ym5t04+
158400130409     c                                    ym5t05+ym5t06
158500020305     c                   else
158600020305
158700020305      * scrivo solo righe delle distinta di consegna
158800020305     c                   eval      ym5t04 = pe_riga6
158900020305     c                   eval      %subst(ym5t04:25:7) = wandcv
159000020305     c                   movel     waddcv        wdatav
159100020305     c                   eval      %subst(ym5t04:37:2) = wgg
159200020305     c                   eval      %subst(ym5t04:40:2) = wmm
159300020305     c                   eval      %subst(ym5t04:43:4) = waa
159400020305     c                   eval      %subst(ym5t04:59:7) = wapdcv
159500020305
159600130411     c                   eval      wrkmsg=%trim(ym5t01)+ ' ' +%trim(ym5t02)+
159700130411     c                              vuoto85+%trim(ym5t03)+'  ' +ym5t04
159800130411     c
159900020305     c                   endif
160000020617
160100020617      * Utente + p.o. CHE EFFETTUA LA RICHIESTA
160200130409     C                   eval      wrkEmlTo='INFO'+ %editc(rigfil:'X')+
160300130409     c                                             '@brt.it'
160400130409     C*
160500130409     C                   call(e)   'TIS701C1'
160600130409     C                   parm                    wrkEmlto
160700130409     C                   parm                    wrkEmlCc
160800130409     C                   parm                    wrkOgg
160900130409     C                   parm                    wrkMsg
161000130409     C                   parm                    wrkesito
161100020304
161200020617
161300020617      * Utente + p.o. CHE DEVE RICERCARE LA BOLLA
161400130409     c
161500130409     C                   eval      wrkEmlTo='INFO'+ walnav  +
161600130409     c                                             '@brt.it'
161700130409     C                   call(e)   'TIS701C1'
161800130409     C                   parm                    wrkEmlto
161900130409     C                   parm                    wrkEmlCc
162000130409     C                   parm                    wrkOgg
162100130409     C                   parm                    wrkMsg
162200130409     C                   parm                    wrkesito
162300020304
162400020304     c                   endsr
162500020304      *--------------------------------------------------------------------------------------------*
162600020925      * Scrittura FIVABBFT
162700020304      *--------------------------------------------------------------------------------------------*
162800020304     c     wrtvab        begsr
162900020304
163000020925     c                   clear                   fivab000
163100020715
163200020715      * Imposto data generazione bolla
163300020715     c                   movel     datcor        vabaas
163400020715     c                   move      datcor        vabmgs
163500020304
163600020304      * cap - località- provincia destinatario: del p.o. che ha fatto la richiesta
163700090423     c     rigfil        chain     azorg01l
163800020304     c                   if        not %found(azorg01l)
163900020304     c                   clear                   orgcpf
164000020304     c                   clear                   orgloc
164100020304     c                   clear                   orgpro
164200020304     c                   endif
164300020304
164400020304      * p.o. arrivo: il p.o. che ha fatto la richiesta
164500090423     c                   eval      vablna = rigfil
164600020304      * p.o. partenza: la linea di arrivo della bolla ultima figlia
164700020304     c                   movel     walnav        vablnp
164800020304      * cliente: linea di arrivo ultima figlia + 9999
164900020304     c                   movel     walnav        vabccm
165000020304     c                   move      9999          vabccm
165100020315      * tipo bolla: 'F'
165200020510     c                   eval      vabcbo ='FO'
165300020315      * codice tratamento merce: '4F'
165400020315     c                   eval      vabctm ='4F'
165500020304      * destinatario
165600020530     c                   eval      vabrsd = 'RAC Cli. ' + wpkscv + ' ' +
165700020508     c                             '- Bolla firmata'
165800020304     c                   eval      vabind = wpragv
165900020304     c                   movel     orgcpf        vabcad
166000020304     c                   eval      vablod = orgloc
166100020304     c                   eval      vabprd = orgpro
166200020304      * natura merce
166300020304     c                   eval      vabnas = 'BOLLA FIRMATA'
166400020304      * colli
166500020304     c                   eval      vabncl = 1
166600020506      * peso
166700020506     c                   eval      vabpkb = 1
166800020304      * riferimento mittente numerico: aa-lnp-nrs-nsp della spedizione mamma originale
166900020304     c                   move      wpaasv        wvabaas
167000020304     c                   movel     wplnpv        wvablnp
167100020304     c                   movel     wpnrsv        wvabnrs
167200020304     c                   movel     wpnspv        wvabnsp
167300020304     c                   move      wnumbolla     vabrmn
167400020304      * note
167500020304     c                   movel     waddcv        wdatav
167600020304     c                   eval      vabnot = 'Distinta n.' + ' ' + wandcv + ' ' +
167700020304     c                             'del' + ' ' + wgg + '/' + wmm + '/' + waa
167800020304
167900020304     c                   add       1             rcdvab
168000020925      *
168100020925      * P.O di gestione
168200020925     c                   move      walnav        vabfgs
168300030508      *
168400030508      * RMO + RMA
168500030508     c                   eval      vabrmo = wvabrmo
168600030508     c                   eval      vabrma = wvabrma
168700030508      *
168800020925      *
168900020925     c                   write     fivab000
169000020304
169100030508     c                   clear                   wvabrmo
169200030508     c                   clear                   wvabrma
169300030508
169400020304     c                   endsr
169500000414      *****************************************************************
169600000414      *  Creo file fatturazione per prima bolla
169700000414      *****************************************************************
169800000414     C     AGGfatt1      BEGSR
169900000414      *
170000000414      * ricavo il tipo porto
170100000414     C                   clear                   DSTB
170200000414     C                   eval      XX = 1
170300000414     C                   eval      W008a = TAStbl
170400000417     C     W008a         lookup    TblK(xx)                               31
170500000417     C                   movel     TblD(xx)      DSTB
170600000414      *
170700000414     C                   SELECT
170800000414      * Non elaboro fatture non emesse dal P.O.
170900000414     c                   WHEN      TASfiv = *zeros  or
171000170710     c                             TASfiv >= 900
171100000414     c                   OTHER
171200000414     C                   eval      AR6div = TASdiv
171300000414     C                   eval      AR6por = TASpor
171400000414     C                   eval      AR6sv1 = TASsv1
171500000414     C                   eval      AR6va1 = TASva1
171600000414     C                   eval      AR6sv2 = TASsv2
171700000414     C                   eval      AR6va2 = TASva2
171800000414     C                   eval      AR6sv3 = TASsv3
171900000414     C                   eval      AR6va3 = TASva3
172000000414     C                   eval      AR6imv = TASimv
172100120103     c                   if        tasnft>0
172200120103     c                   if        tasdft>§qtdsa
172300000414     C                   eval      AR6ali = §QTali
172400120103     c                   else
172500120103     C                   eval      AR6ali = §QTalo
172600120103     c                   endif
172700120103     c                   endif
172800000414     C                   eval      AR6cei = TASfei
172900000414     C                   eval      AR6fiv = TASfiv
173000000414     C                   eval      AR6nft = TASnft
173100000414     C                   eval      AR6dft = TASdft
173200020916
173300020916     C                   eval      AR6aas = TASaas
173400020916     C                   eval      AR6lnp = TASlnp
173500020916     C                   eval      AR6nrs = TASnrs
173600020916     C                   eval      AR6nsp = TASnsp
173700020916     C                   eval      AR6trc = '1'
173800020916     C                   eval      AR6ksc = TASksc
173900020916     C                   eval      AR6ctr = TASctr
174000020916
174100000414      *   calcolo l'importo
174200000426     C                   clear                   FNLV16DS
174300000414     c                   IF         §TBTPO = 'F'
174400000414     c                   eval      D17PRP = 'S'
174500000414     c                   ENDIF
174600000414     c                   eval      D17tla = 'L'
174700000414     c                   eval      D17tbo = '0'
174800000414     c                   eval      D17crb = 'S'
174900000414     c                   eval      D17aas = TASaas
175000000414     c                   eval      D17lns = TASlnp
175100000414     c                   eval      D17nrs = TASnrs
175200000414     c                   eval      D17nsp = TASnsp
175300000414     c                   eval      D17tbs = TAStbl
175400000414     c                   eval      D17fim = 'F'
175500000414     c                   CALL      'FNLV16R'
175600000414     c                   PARM                    FNLV16DS
175700000414     c                   PARM                    DTASV
175800000414      *
175900000414     C                   eval      AR6ift = O17ift
176000000414      *
176100000414     C                   write     FIAR6000
176200000414     C                   eval      WfileT = 'FIAR600T'
176300000414     C                   eval      WfileR = 'FIAR600R'
176400000414     C                   exsr      AGGnrec
176500000414      *
176600000414      * FIAR7
176700000414     c     KSPE          setll     TITA7000
176800000414     c     KSPE          reade     TITA7000
176900000414     c                   DOW       not %EOF
177000000414     c                   eval      AR7aas = TA7aas
177100000414     c                   eval      AR7lnp = TA7lnp
177200000414     c                   eval      AR7nrs = TA7nrs
177300000414     c                   eval      AR7nsp = TA7nsp
177400000414     c                   eval      AR7trc = '1'
177500000414     c                   eval      AR7svn = TA7svn
177600000414     c                   eval      AR7van = TA7van
177700000414     c                   write     FIAR7000
177800000414     C                   eval      WfileT = 'FIAR700T'
177900000414     C                   eval      WfileR = 'FIAR700R'
178000000414     C                   exsr      AGGnrec
178100000414      *
178200000414     c     KSPE          reade     TITA7000
178300000414     c                   ENDDO
178400000414      *
178500000414     c                   ENDSL
178600000414      *
178700000414     c                   ENDSR
178800000414      *****************************************************************
178900000414      *  Creo file fatturazione per seconda bolla
179000000414      *****************************************************************
179100000414     C     AGGfatt2      BEGSR
179200000414      *
179300000414     C                   clear                   DSTB
179400000414     C                   eval      XX = 1
179500000414     C                   eval      W008a = $TAStbl
179600000417     C     W008a         lookup    TblK(xx)                               31
179700000417     C                   movel     TblD(xx)      DSTB
179800000414      *
179900000414     C                   SELECT
180000000417      *
180100000417      * Non elaboro se non è prevista la 2a bolla
180200000417     c                   WHEN      §3Atb2 = *blanks
180300000417      *
180400000417      * Non elaboro se la 2a bolla è uno storno
180500000417     c                   WHEN      §TBfcb = '0'
180600000417      *
180700000414      * Non elaboro fatture non emesse dal P.O.
180800000417     c                   WHEN      $TASfiv = *zeros  or
180900170710     c                             $TASfiv >= 900
181000000417      *
181100120607     c* Sia per partenza che per arrivo trasmetto alla stessa maniera
181200120607     c                   WHEN      §TBtpo = 'A'
181300000414     C                   eval      AR6aas = $TASaas
181400000414     C                   eval      AR6lnp = $TASlnp
181500000414     C                   eval      AR6nrs = $TASnrs
181600000414     C                   eval      AR6nsp = $TASnsp
181700000414     C                   eval      AR6trc = '2'
181800000414     C                   eval      AR6ksc = $TASksc
181900000414     C                   eval      AR6ctr = $TASctr
182000000414     C                   eval      AR6div = $TASdiv
182100000414     C                   eval      AR6por = $TASpor
182200000414     C                   eval      AR6sv1 = $TASsv1
182300000414     C                   eval      AR6va1 = $TASva1
182400000414     C                   eval      AR6sv2 = $TASsv2
182500000414     C                   eval      AR6va2 = $TASva2
182600000414     C                   eval      AR6sv3 = $TASsv3
182700000414     C                   eval      AR6va3 = $TASva3
182800000414     C                   eval      AR6imv = $TASimv
182900120103     c                   if        $tasnft>0
183000120103     c                   if        $tasdft>§qtdsa
183100000414     C                   eval      AR6ali = §QTali
183200120103     c                   else
183300120103     C                   eval      AR6ali = §QTalo
183400120103     c                   endif
183500120103     c                   endif
183600000414     C                   eval      AR6cei = $TASfei
183700000414     C                   eval      AR6fiv = $TASfiv
183800000414     C                   eval      AR6nft = $TASnft
183900000414     C                   eval      AR6dft = $TASdft
184000000414      *   calcolo l'importo
184100000426     C                   clear                   FNLV16DS
184200000414     c                   IF        §TBTPO = 'F'
184300000414     c                   eval      D17PRP = 'S'
184400000414     c                   ENDIF
184500000414     c                   eval      D17tla = 'L'
184600000414     c                   eval      D17tbo = '0'
184700000414     c                   eval      D17crb = 'S'
184800000414     c                   eval      D17aas = $TASaas
184900000414     c                   eval      D17lns = $TASlnp
185000000414     c                   eval      D17nrs = $TASnrs
185100000414     c                   eval      D17nsp = $TASnsp
185200000414     c                   eval      D17tbs = $TAStbl
185300000414     c                   eval      D17fim = 'F'
185400000414     c                   CALL      'FNLV16R'
185500000414     c                   PARM                    FNLV16DS
185600000414     c                   PARM                    DTASV
185700000414     C                   eval      AR6ift = O17ift
185800000414      *
185900000414     C                   write     FIAR6000
186000000414     C                   eval      WfileT = 'FIAR600T'
186100000414     C                   eval      WfileR = 'FIAR600R'
186200000414     C                   exsr      AGGnrec
186300000414      *
186400000414     c     KSPE$         setll     TITA7000
186500000414     c     KSPE$         reade     TITA7000
186600000414     c                   DOW       not %EOF
186700000414     c                   eval      AR7aas = TA7aas
186800000414     c                   eval      AR7lnp = TA7lnp
186900000414     c                   eval      AR7nrs = TA7nrs
187000000414     c                   eval      AR7nsp = TA7nsp
187100000414     c                   eval      AR7trc = '2'
187200000414     c                   eval      AR7svn = TA7svn
187300000414     c                   eval      AR7van = TA7van
187400000414     C                   write     FIAR7000
187500000414     C                   eval      WfileT = 'FIAR700T'
187600000414     C                   eval      WfileR = 'FIAR700R'
187700000414     C                   exsr      AGGnrec
187800000414      *
187900000414     c     KSPE$         reade     TITA7000
188000000414     c                   ENDDO
188100000414      *
188200000414     c                   ENDSL
188300000414      *
188400000414     c                   endsr
188500030110      *---------------------------------------------------------------*
188600030110      *  Aggiorno file "estensione testata bolla" da trasmettere      *
188700030110      *---------------------------------------------------------------*
188800030110     c     srFIAR5       BEGSR
188900030110      *
189000030110      * Lettura file FIAR5
189100040531     c     kspe          setll     FIAR531C
189200040531     c     kspe          reade     FIAR531C
189300030110      *
189400040531do  1c                   DOW       not %EOF(FIAR531C)
189500030110      *
189600030110      * controllo il tipo record per verificare che sia da tresmettere
189700030110     c                   clear                   dAR5
189800030110     c                   eval      XX = 1
189900030110     c     AR5trd        lookup    AR5k(xx)                               31
190000030110      *
190100030110      * se il record è da trasmettere: scrivo FIAR500T
190200030110if  2c                   If        *in31
190300030110     c                   write     FIAR5T
190400030110     c                   eval      WfileT = 'FIAR500T'
190500030110     c                   eval      WfileR = 'FIAR500R'
190600030110     c                   exsr      AGGnrec
190700030110e   2c                   Endif
190800030110      *
190900040531     c     kspe          reade     FIAR531C
191000030110      *
191100030110e   1c                   ENDDO
191200030110      *
191300030110     c                   ENDSR
191400000404      *****************************************************************
191500000404      *  AGGANCIO ANAGRAFICHE
191600000404      *****************************************************************
191700000410     C     AGGnrec       BEGSR
191800000404      *
191900000410     c                   eval      xx = 1
192000000412     C     WfileT        lookup    filet(xx)                              31
192100000410      *
192200000410     C                   IF         *in31 = *on
192300000410     c                   add       1             rcdt(xx)
192400000410     c                   ELSE
192500000410     C     *blanks       lookup    filet(xx)                              31
192600000410     C                   IF         *in31 = *on
192700000412     c                   movel     WfileT        fileT(xx)
192800000412     c                   movel     WfileR        fileR(xx)
192900000412     c                   add       1             rcdT(xx)
193000000410     C                   ENDIF
193100000410     C                   ENDIF
193200000410      *
193300000410     c                   endsr
193400000410      *****************************************************************
193500000410      *  AGGANCIO ANAGRAFICHE
193600000410      *****************************************************************
193700000410     C     CTR_ANAGRA    BEGSR
193800000410      *
193900000404     C                   CALL      'TIBS69R'
194000000404     C                   PARM                    tibs69DS
194100000404     C                   PARM                    DS_cnaco
194200000404     C                   PARM                    DS_cnind
194300000404     C                   PARM                    DS_cnclp
194400000404     C                   PARM                    DS_fncls
194500000404      *
194600000404     C                   eval      *IN30 = (O69ERR =  '1')
194700000404      *
194800000404      * per il 1° giro è inizializzata nelle specifiche "D"
194900000404     C                   clear                   TIBS69DS
195000000404      *
195100000404     C                   ENDSR
195200000404      *--------------------------------------------------------------------------------------------*
195300000404      * Gestione file di trasmissione in partenza
195400000404      *--------------------------------------------------------------------------------------------*
195500000405     C     GESfiltrpa    BEGSR
195600000404      *
195700000404      * chiudo i files
195800000404     C                   exsr      closepfpar
195900000404      *
196000000413     c                   Do        Wnumele       XX
196100000404      *
196200000404      * controlla se esiste il membro
196300000404     C                   MOVEL(P)  cmpCHK(xx)    comman
196400000404     C                   CALL      'QCMDEXC'                            20
196500000404     C                   PARM                    comman
196600000404     C                   PARM                    lenght
196700000404      *
196800000404      * inesistente, aggiunge un membro nuovo
196900000404     C                   if        *in20
197000000404     C                   MOVEL(P)  cmpADD(xx)    comman
197100000404     C                   CALL      'QCMDEXC'
197200000404     C                   PARM                    comman
197300000404     C                   PARM                    lenght
197400000404     C                   endif
197500000404      *
197600000404      * in ogni caso, esegue l'ovrdbf
197700000404     C                   MOVEL(P)  cmpOVR(xx)    comman
197800000404     C                   CALL      'QCMDEXC'
197900000404     C                   PARM                    comman
198000000404     C                   PARM                    lenght
198100000404      *
198200000404      * alloco il file
198300000404     C                   MOVEL(P)  cmpALC(xx)    comman
198400000404     C                   CALL      'QCMDEXC'                            91
198500000404     C                   PARM                    comman
198600000404     C                   PARM                    lenght
198700000404      *
198800000404      * Se file già allocato disalloco quelli allocati e vado a fine pgm
198900000404     c                   If        *in91 =*on
199000000404      *
199100000404     c                   If        XX > 1
199200000404     c                   sub       1             XX
199300000404     c                   Do        XX            YY
199400000404     C                   MOVEL(P)  cmpDLC(xx)    comman
199500000404     C                   CALL      'QCMDEXC'                            20
199600000404     C                   PARM                    comman
199700000404     C                   PARM                    lenght
199800000404     c                   Enddo
199900000404     c                   Endif
200000000404      *
200100000412     c                   LEAVE
200200000404     c                   Endif
200300000404      *
200400000412     C                   ENDDO
200500000412      *
200600000404      * Apro i files
200700000412     c  N91              exsr      OPENpfpar
200800000404      *
200900000404     C     endtrpa       ENDSR
201000000404      *--------------------------------------------------------------------------------------------*
201100000404      * Gestione file di trasmissione in arrivo
201200000404      *--------------------------------------------------------------------------------------------*
201300000405     C     GESfiltrar    BEGSR
201400000404      *
201500000404      * chiudo i files
201600000404     C                   exsr      closepfarr
201700000404      *
201800000413     c                   Do        Wnumele       XX
201900000404      *
202000000404      * controlla se esiste il membro
202100000404     C                   MOVEL(P)  cmaCHK(xx)    comman
202200000404     C                   CALL      'QCMDEXC'                            20
202300000404     C                   PARM                    comman
202400000404     C                   PARM                    lenght
202500000404      *
202600000404      * inesistente, aggiunge un membro nuovo
202700000404     C                   if        *in20
202800000404     C                   MOVEL(P)  cmaADD(xx)    comman
202900000404     C                   CALL      'QCMDEXC'
203000000404     C                   PARM                    comman
203100000404     C                   PARM                    lenght
203200000404     C                   endif
203300000404      *
203400000404      * in ogni caso, esegue l'ovrdbf
203500000404     C                   MOVEL(P)  cmaOVR(xx)    comman
203600000404     C                   CALL      'QCMDEXC'
203700000404     C                   PARM                    comman
203800000404     C                   PARM                    lenght
203900000404      *
204000000404      * alloco il file
204100000404     C                   MOVEL(P)  cmaALC(xx)    comman
204200000404     C                   CALL      'QCMDEXC'                            91
204300000404     C                   PARM                    comman
204400000404     C                   PARM                    lenght
204500000404      *
204600000404      * Se file già allocato disalloco quelli allocati e vado a fine pgm
204700000404     c                   If        *in91 =*on
204800000404      *
204900000404     c                   If        XX > 1
205000000404     c                   sub       1             XX
205100000404     c                   Do        XX            YY
205200000404     C                   MOVEL(P)  cmaDLC(xx)    comman
205300000404     C                   CALL      'QCMDEXC'                            20
205400000404     C                   PARM                    comman
205500000404     C                   PARM                    lenght
205600000404     c                   Enddo
205700000404     c                   Endif
205800000404      *
205900000412     c                   LEAVE
206000000404     c                   Endif
206100000412      *
206200000412     C                   ENDDO
206300000404      *
206400000404      * Apro i files
206500000412     c  N91              exsr      openpfarr
206600000404      *
206700000404     C     endtrar       ENDSR
206800020304      *--------------------------------------------------------------------------------------------*
206900020925      * Gestione file di trasmissione msg e fivab
207000020304      *--------------------------------------------------------------------------------------------*
207100020304     c     gesfilvab     begsr
207200020304
207300020304      * chiudo il file
207400020304     c                   exsr      closepfvab
207500020304      *
207600020304
207700020304      * controlla se esiste il membro
207800020304     c                   movel(p)  cmdvab(1)     comman
207900020827     c                   eval      %subst(comman:56:6) = wmbrvab
208000020304     c                   call      'QCMDEXC'                            20
208100020304     c                   parm                    comman
208200020304     c                   parm                    lenght
208300020304
208400020304      * inesistente, aggiunge un membro nuovo
208500020304     c                   if        *in20
208600020304     c                   movel(p)  cmdvab(2)     comman
208700020827     c                   eval      %subst(comman:56:6) = wmbrvab
208800020304     c                   call      'QCMDEXC'
208900020304     c                   parm                    comman
209000020304     c                   parm                    lenght
209100020304     c                   endif
209200020304
209300020304      * in ogni caso, esegue l'ovrdbf
209400020304     c                   movel(p)  cmdvab(3)     comman
209500020827     c                   eval      %subst(comman:56:6) = wmbrvab
209600020304     c                   call      'QCMDEXC'
209700020304     c                   parm                    comman
209800020304     c                   parm                    lenght
209900020304
210000020304      * alloco il file
210100020304     c                   movel(p)  cmdvab(4)     comman
210200020827     c                   eval      %subst(comman:54:6) = wmbrvab
210300020304     c                   call      'QCMDEXC'                            91
210400020304     c                   parm                    comman
210500020304     c                   parm                    lenght
210600020304
210700020304      * Se file già allocato disalloco ed esco dal programma
210800020304     c                   if        *in91
210900020304     c                   movel(p)  cmdvab(5)     comman
211000020827     c                   eval      %subst(comman:54:6) = wmbrvab
211100020304     c                   call      'QCMDEXC'                            20
211200020304     c                   parm                    comman
211300020304     c                   parm                    lenght
211400020304     c                   endif
211500020304
211600020304      * Apro il file
211700020304     c  n91              exsr      openpfvab
211800020304
211900020304     c                   endsr
212000000404      *--------------------------------------------------------------------------------------------*
212100000404      * Chiudo file per partenza
212200000404      *--------------------------------------------------------------------------------------------*
212300000404     c     CLOSEpfpar    BEGSR
212400000404      *
212500000404     c                   close     fnblp00t
212600060221     c                   close     fiar400t
212700051116     c                   close     fIar900t
212800000404     c                   close     fnblt00t
212900000404     c                   close     fiar600t
213000000404     c                   close     fiar700t
213100030110     c                   close     fiar500t
213200000404      *
213300000404     c                   ENDSR
213400000404      *--------------------------------------------------------------------------------------------*
213500000404      * apro file per partenza
213600000404      *--------------------------------------------------------------------------------------------*
213700000404     c     OPENpfpar     BEGSR
213800000404      *
213900000404     c                   open      fnblp00t
214000060221     c                   open      fiar400t
214100051116     c                   open      fIAR900t
214200000404     c                   open      fnblt00t
214300000404     c                   open      fiar600t
214400000404     c                   open      fiar700t
214500030110     c                   open      fiar500t
214600000404      *
214700000404     c                   ENDSR
214800000404      *--------------------------------------------------------------------------------------------*
214900000404      * Chiudo file per arrivo
215000000404      *--------------------------------------------------------------------------------------------*
215100000404     c     CLOSEpfarr    BEGSR
215200000404      *
215300000404     c                   close     fnarb00t
215400060221     c                   close     fiar400t
215500051116     c                   close     fiar900t
215600000404     c                   close     fnart00t
215700000404     c                   close     fiar600t
215800000404     c                   close     fiar700t
215900030110     c                   close     fiar500t
216000000404      *
216100000404     c                   ENDSR
216200000404      *--------------------------------------------------------------------------------------------*
216300000404      * apro file per arrivo
216400000404      *--------------------------------------------------------------------------------------------*
216500000404     c     OPENpfarr     BEGSR
216600000404      *
216700000404     c                   open      fnarb00t
216800060221     c                   open      fiar400t
216900051116     c                   open      fiar900t
217000000404     c                   open      fnart00t
217100000404     c                   open      fiar600t
217200000404     c                   open      fiar700t
217300030110     c                   open      fiar500t
217400000404      *
217500000404     c                   ENDSR
217600020304      *--------------------------------------------------------------------------------------------*
217700020925      * Chiudo file fivab
217800020304      *--------------------------------------------------------------------------------------------*
217900020304     c     closepfvab    begsr
218000020304
218100020925     c                   close     fivabbft
218200020304
218300020304     c                   endsr
218400020304      *--------------------------------------------------------------------------------------------*
218500020925      * apro file fivab
218600020304      *--------------------------------------------------------------------------------------------*
218700020304     c     openpfvab     begsr
218800020304
218900020925     c                   open      fivabbft
219000020304
219100020304     c                   endsr
219200000323      *--------------------------------------------------------------------------------------------*
219300000323      * Imposto la librerie in tutte le schiere
219400000323      *--------------------------------------------------------------------------------------------*
219500000323     c     impolib       BEGSR
219600020301      *Se il nome della libreria è lungo 10 OK se lungo 9 mettiamo
219700020301      *il ' ' nella 1° posizione a sinis.
219800020301     c                   if        %subst(wlibfile: 10: 1) =' '
219900020301     c                   movel     wlibfile      com09             9
220000020301     c                   clear                   wlibfile
220100020301     c                   move      com09         wlibfile
220200020301     c                   end
220300000323     c                   eval      XX = 1
220400000413     c                   DO        Wnumele       xx
220500020211     c                   eval      %subst(cmpCHK(xx):15:10) = wlibfile
220600020211     c                   eval      %subst(cmpADD(xx):15:10) = wlibfile
220700020211     c                   eval      %subst(cmpOVR(xx):15:10) = wlibfile
220800020211     c                   eval      %subst(cmpALC(xx):15:10) = wlibfile
220900020211     c                   eval      %subst(cmpDLC(xx):15:10) = wlibfile
221000020211     c                   eval      %subst(cmaCHK(xx):15:10) = wlibfile
221100020211     c                   eval      %subst(cmaADD(xx):15:10) = wlibfile
221200020211     c                   eval      %subst(cmaOVR(xx):15:10) = wlibfile
221300020211     c                   eval      %subst(cmaALC(xx):15:10) = wlibfile
221400020211     c                   eval      %subst(cmaDLC(xx):15:10) = wlibfile
221500000412     c                   ENDDO
221600020304
221700020304     c                   DO        5             xx
221800020304     c                   eval      %subst(cmdvab(xx):15:10) = wlibfile
221900020304     c                   ENDDO
222000020304
222100000323      *
222200000323     C                   ENDSR
222300000328      *--------------------------------------------------------------------------------------------*
222400000328      * Operazioni finali
222500000328      *--------------------------------------------------------------------------------------------*
222600000412     C     Chiusure      BEGSR
222700000328      *
222800020827     C                   clear                   TIBS55DS
222900000328     C                   MOVEL     'C'           i50tla
223000020827     C                   CALL      'TIBS55R'
223100020827     C                   PARM                    TIBS55DS
223200000328      *
223300000328     C                   clear                   TIBS69DS
223400000328     C                   eval      I69TLA  = 'C'
223500000328     C                   CALL      'TIBS69R'
223600000328     C                   PARM                    TIBS69DS
223700000328      *
223800000328     C                   ENDSR
223900000322      *--------------------------------------------------------------------------------------------*
224000000407      * Routine iniziale
224100000322      *--------------------------------------------------------------------------------------------*
224200980828     C     *inzsr        BEGSR
224300000322      *
224400980828     C     *entry        PLIST
224500980828     C                   PARM                    kpjba
224600000322      *
224700000322     C                   Z-ADD     1             CODUT
224800980915     C                   CALL      'X§PARUT'
224900000412     C                   PARM                    UT§DSE0F
225000980915     C                   MOVEL     REC80         CNCR80
225100000330      * spedizione
225200000330     c     kspe          klist
225300000327     c                   kfld                    TASaas
225400000327     c                   kfld                    TASlnp
225500000327     c                   kfld                    TASnrs
225600000327     c                   kfld                    TASnsp
225700000410      * spedizione 2a bolla
225800000412     c     kspe$         klist
225900000410     c                   kfld                    $TASaas
226000000410     c                   kfld                    $TASlnp
226100000410     c                   kfld                    $TASnrs
226200000410     c                   kfld                    $TASnsp
226300000330      * Tita4
226400000330     c     kta4          klist
226500000330     c                   kfld                    TASaas
226600000330     c                   kfld                    TASlnp
226700000330     c                   kfld                    TASnrs
226800000330     c                   kfld                    TASnsp
226900000330     c                   kfld                    Ktrc
227000981008      * legame bolla spedizione
227100981008     c     klbl          klist
227200000322     c                   kfld                    rigaas
227300000322     c                   kfld                    riglnp
227400000322     c                   kfld                    rignrs
227500000322     c                   kfld                    rignsp
227600120530      *
227700120530     c     klbl2         klist
227800120530     c                   kfld                    WLaas
227900120530     c                   kfld                    WLlnp
228000120530     c                   kfld                    WLnrs
228100120530     c                   kfld                    WLnsp
228200000323      *
228300000323     c     klblA         klist
228400000323     c                   kfld                    WAaas
228500000323     c                   kfld                    WAlnp
228600000323     c                   kfld                    WAnrs
228700000323     c                   kfld                    WAnsp
228800000412      *
228900000419     c     KBOLw         klist
229000000412     c                   kfld                    Waas
229100000412     c                   kfld                    Wlnp
229200000412     c                   kfld                    Wnrs
229300000412     c                   kfld                    Wnsp
229400000412      *
229500000407     C     kTAB          KLIST
229600000407     C                   KFLD                    CODUT
229700000407     C                   KFLD                    KCOD
229800000322      *
229900000404     C     keytab        KLIST
230000000404     C                   KFLD                    CODUT
230100000404     C                   KFLD                    KCOD
230200000404     C                   KFLD                    KKEY
230300000322      *
230400000413      * Imposto numero elementi in schiera
230500000413     C                   eval      Wnumele = %elem(cmpchk)
230600000413      *
230700000322      * reperimento librerie job
230800020827     C                   CLEAR                   TIBS55DS
230900980828     C                   MOVEL     knsif         i50psi
231000020827     C                   CALL      'TIBS55R'
231100020827     C                   PARM                    TIBS55DS
231200000322      *
231300000323      * La libreria dei file di trasmissione è quella di gruppo, la imposto
231400000323      *  in tutte le schiere
231500981222     c                   movel     o50plg        wlibfile
231600000323     c                   exsr      impolib
231700000322      *
231800000322      * calcola la data corrente
231900000323     C                   TIME                    w0140                          *ora(6) + data(8)
232000000322     C                   MOVE      w0140         w0080                          *data(8) in g/m/aa
232100000322      * UDATE IN AAAAMMGG
232200000323     C     *eur          MOVEL     W0080         Wdata
232300000322     C     *iso          MOVEL     Wdata         datcor
232400000322      *
232500000407      * Carico codici bolla da convertire
232600000407     c                   clear                   xx
232700000407     C                   eval      kcod = '3A'
232800000407     c     KTAB          setll     tabel
232900000407     c     KTAB          reade     tabel
233000000407     C                   DOW       not %EOF
233100000412     c                   If        TBLflg = *blanks
233200000407     C                   add       1             xx
233300000417     c                   movel     TBlkey        T3aK(xx)
233400000417     c                   movel     TBLuni        T3aD(xx)
233500000407     c                   Endif
233600000412     c     KTAB          reade     tabel
233700000407     C                   ENDDO
233800000413      *
233900000413      * Carico tipi bolla
234000000413     c                   clear                   xx
234100000413     C                   eval      kcod = 'TB'
234200000413     c     KTAB          setll     tabel
234300000413     c     KTAB          reade     tabel
234400000413     C                   DOW       not %EOF
234500000413     c                   If        TBLflg = *blanks
234600000413     C                   add       1             xx
234700000417     c                   movel     TBlkey        TblK(xx)
234800000417     c                   movel     TBLuni        TblD(xx)
234900000413     c                   Endif
235000000413     c     KTAB          reade     tabel
235100000413     C                   ENDDO
235200000412      *
235300000412      * Aggancio tabella campi di tassazione standard
235400000419     C                   clear                   dsqt1
235500000412     C                   eval      KCOD = 'QT'
235600000412     C                   movel(P)  '1'           KKEY
235700000412     C     keytab        chain     TABEL
235800000412     c                   IF        %found
235900000412     C                   movel     TBLUNI        DSQT1
236000000412     c                   ENDIF
236100030110      *
236200030110      * Carico codici Tipi Record di FIAR500F
236300030110      * (solo se prevedono la trasmissione in partenza e/o in arrivo)
236400030110     c                   clear                   xx
236500030110     c                   eval      TBEcod = 'AR5'
236600030110     c     TBEcod        setll     TNTBE000
236700030110     c     TBEcod        reade     TNTBE000
236800030110     c                   DOW       not %EOF(TNTBE01L)
236900030110     c                   If        TBEatb = *blanks
237000030110     c                   movel     TBEuni        dAR5
237100030110     c                   If        §AR5rigb = 'S'
237200030110     c                   add       1             xx
237300030110     c                   movel     TBEke1        AR5k(xx)
237400030110     c                   Endif
237500030110     c                   Endif
237600030110     c     TBEcod        reade     TNTBE000
237700030110     c                   ENDDO
237800000322      *
237900980828     C                   ENDSR
238000000323      *--------------------------------------------------------------------------------------------*
238100000324**   cmpCHK - comandi
238200020211CHKOBJ    OBJ(xxxxxxxxxx/FNBLP00T) OBJTYPE(*FILE) MBR(SEDE)                         1
238300060221CHKOBJ    OBJ(xxxxxxxxxx/FIAR400T) OBJTYPE(*FILE) MBR(SEDE)                         1
238400051116CHKOBJ    OBJ(xxxxxxxxxx/FIAR900T) OBJTYPE(*FILE) MBR(SEDE)                         1
238500020211CHKOBJ    OBJ(xxxxxxxxxx/FNBLT00T) OBJTYPE(*FILE) MBR(SEDE)                         1
238600030110CHKOBJ    OBJ(xxxxxxxxxx/FIAR500T) OBJTYPE(*FILE) MBR(SEDE)                         1
238700020211CHKOBJ    OBJ(xxxxxxxxxx/FIAR600T) OBJTYPE(*FILE) MBR(SEDE)                         1
238800020211CHKOBJ    OBJ(xxxxxxxxxx/FIAR700T) OBJTYPE(*FILE) MBR(SEDE)                         1
238900000324**   cmaCHK - comandi
239000120607CHKOBJ    OBJ(xxxxxxxxxx/FNARB00T) OBJTYPE(*FILE) MBR(SEDEA)                        1
239100120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR400T) OBJTYPE(*FILE) MBR(SEDEA)                        1
239200120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR900T) OBJTYPE(*FILE) MBR(SEDEA)                        1
239300120607CHKOBJ    OBJ(xxxxxxxxxx/FNART00T) OBJTYPE(*FILE) MBR(SEDEA)                        1
239400120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR500T) OBJTYPE(*FILE) MBR(SEDEA)                        1
239500120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR600T) OBJTYPE(*FILE) MBR(SEDEA)                        1
239600120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR700T) OBJTYPE(*FILE) MBR(SEDEA)                        1
239700000324**   cmpADD - comandi
239800020211ADDPFM   FILE(xxxxxxxxxx/FNBLP00T)                MBR(SEDE)                         2
239900060221ADDPFM   FILE(xxxxxxxxxx/FIAR400T)                MBR(SEDE)                         2
240000051116ADDPFM   FILE(xxxxxxxxxx/FIAR900T)                MBR(SEDE)                         2
240100020211ADDPFM   FILE(xxxxxxxxxx/FNBLT00T)                MBR(SEDE)                         2
240200030110ADDPFM   FILE(xxxxxxxxxx/FIAR500T)                MBR(SEDE)                         2
240300020211ADDPFM   FILE(xxxxxxxxxx/FIAR600T)                MBR(SEDE)                         2
240400020211ADDPFM   FILE(xxxxxxxxxx/FIAR700T)                MBR(SEDE)                         2
240500000324**   cmaADD - comandi
240600120607ADDPFM   FILE(xxxxxxxxxx/FNARB00T)                MBR(SEDEA)                        2
240700120607ADDPFM   FILE(xxxxxxxxxx/FIAR400T)                MBR(SEDEA)                        2
240800120607ADDPFM   FILE(xxxxxxxxxx/FIAR900T)                MBR(SEDEA)                        2
240900120607ADDPFM   FILE(xxxxxxxxxx/FNART00T)                MBR(SEDEA)                        2
241000120607ADDPFM   FILE(xxxxxxxxxx/FIAR500T)                MBR(SEDEA)                        2
241100120607ADDPFM   FILE(xxxxxxxxxx/FIAR600T)                MBR(SEDEA)                        2
241200120607ADDPFM   FILE(xxxxxxxxxx/FIAR700T)                MBR(SEDEA)                        2
241300000324**   cmpOVR - comandi
241400020211OVRDBF TOFILE(xxxxxxxxxx/FNBLP00T) FILE(FNBLP00T) MBR(SEDE)                         3
241500060221OVRDBF TOFILE(xxxxxxxxxx/FIAR400T) FILE(FIAR400T) MBR(SEDE)                         3
241600051116OVRDBF TOFILE(xxxxxxxxxx/FIAR900T) FILE(FIAR900T) MBR(SEDE)                         3
241700020211OVRDBF TOFILE(xxxxxxxxxx/FNBLT00T) FILE(FNBLT00T) MBR(SEDE)                         3
241800030110OVRDBF TOFILE(xxxxxxxxxx/FIAR500T) FILE(FIAR500T) MBR(SEDE)                         3
241900020211OVRDBF TOFILE(xxxxxxxxxx/FIAR600T) FILE(FIAR600T) MBR(SEDE)                         3
242000020211OVRDBF TOFILE(xxxxxxxxxx/FIAR700T) FILE(FIAR700T) MBR(SEDE)                         3
242100000324**   cmaOVR - comandi
242200120607OVRDBF TOFILE(xxxxxxxxxx/FNARB00T) FILE(FNARB00T) MBR(SEDEA)                        3
242300120607OVRDBF TOFILE(xxxxxxxxxx/FIAR400T) FILE(FIAR400T) MBR(SEDEA)                        3
242400120607OVRDBF TOFILE(xxxxxxxxxx/FIAR900T) FILE(FIAR900T) MBR(SEDEA)                        3
242500120607OVRDBF TOFILE(xxxxxxxxxx/FNART00T) FILE(FNART00T) MBR(SEDEA)                        3
242600120607OVRDBF TOFILE(xxxxxxxxxx/FIAR500T) FILE(FIAR500T) MBR(SEDEA)                        3
242700120607OVRDBF TOFILE(xxxxxxxxxx/FIAR600T) FILE(FIAR600T) MBR(SEDEA)                        3
242800120607OVRDBF TOFILE(xxxxxxxxxx/FIAR700T) FILE(FIAR700T) MBR(SEDEA)                        3
242900000324**   cmpALC - comandi
243000020211ALCOBJ   OBJ((xxxxxxxxxx/FNBLP00T  *FILE   *EXCL   (SEDE)))                         5
243100060221ALCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDE)))                         5
243200051116ALCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDE)))                         5
243300020211ALCOBJ   OBJ((xxxxxxxxxx/FNBLT00T  *FILE   *EXCL   (SEDE)))                         5
243400030110ALCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDE)))                         5
243500020211ALCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDE)))                         5
243600020211ALCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDE)))                         5
243700000324**   cmaALC - comandi
243800120607ALCOBJ   OBJ((xxxxxxxxxx/FNARB00T  *FILE   *EXCL   (SEDEA)))                        5
243900120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDEA)))                        5
244000120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDEA)))                        5
244100120607ALCOBJ   OBJ((xxxxxxxxxx/FNART00T  *FILE   *EXCL   (SEDEA)))                        5
244200120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDEA)))                        5
244300120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDEA)))                        5
244400120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDEA)))                        5
244500000324**   cmpDLC - comandi
244600020211DLCOBJ   OBJ((xxxxxxxxxx/FNBLP00T  *FILE   *EXCL   (SEDE)))                         6
244700060221DLCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDE)))                         6
244800051116DLCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDE)))                         6
244900020211DLCOBJ   OBJ((xxxxxxxxxx/FNBLT00T  *FILE   *EXCL   (SEDE)))                         6
245000030110DLCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDE)))                         6
245100020211DLCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDE)))                         6
245200020211DLCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDE)))                         6
245300000324**   cmaDLC - comandi
245400120607DLCOBJ   OBJ((xxxxxxxxxx/FNARB00T  *FILE   *EXCL   (SEDEA)))                        6
245500120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDEA)))                        6
245600120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDEA)))                        6
245700120607DLCOBJ   OBJ((xxxxxxxxxx/FNART00T  *FILE   *EXCL   (SEDEA)))                        6
245800120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDEA)))                        6
245900120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDEA)))                        6
246000120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDEA)))                        6
246100020304**   cmdvab - comandi
246200020925CHKOBJ    OBJ(xxxxxxxxxx/FIVABBFT) OBJTYPE(*FILE) MBR(Mxxxxxx)                      1
246300020925ADDPFM   FILE(xxxxxxxxxx/FIVABBFT)                MBR(Mxxxxxx)                      2
246400021009OVRDBF TOFILE(xxxxxxxxxx/FIVABBFT) FILE(FIVABBFT) MBR(Mxxxxxx)                      3
246500020925ALCOBJ   OBJ((xxxxxxxxxx/FIVABBFT  *FILE   *EXCL   (Mxxxxxx)))                      4
246600020925DLCOBJ   OBJ((xxxxxxxxxx/FIVABBFT  *FILE   *EXCL   (Mxxxxxx)))                      5

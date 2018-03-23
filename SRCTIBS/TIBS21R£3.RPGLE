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
009900000412     D DSQC2         E DS
010000000412     D DSQT1         E DS
010100000412     D DS_TITAS      E DS                  extname(TITAS00F)
010200000412     D DS_TITAS_2    E DS                  extname(TITAS00F) prefix($)
010300000412     D DS_TITAA      E DS                  extname(TITAA00F)
010400000412     D DS_TITAA_O    E DS                  extname(TITAA00F) prefix(O)
010500000412     D DS_TITAA_M    E DS                  extname(TITAA00F) prefix(M)
010600000328     d TIBS69DS      E DS                  INZ
010700000328     d DS_cnaco      E DS                  extname(CNACO00F)
010800000328     d DS_cnind      E DS                  extname(CNIND00F)
010900000328     d DS_cnclp      E DS                  extname(CNCLP00F)
011000000328     d DS_fncls      E DS                  extname(FNCLS00F)
011100020304
011200000322      *----------------
011300020304     d keyorg          S                   LIKE(orgfil)
011400000330     d ktrc            S                   LIKE(TA4trc)
011500000404     d Wsegna          S                   LIKE(TASnca)
011600000404     D KCOD            S                   LIKE(TBLcod)
011700000404     D KKEY            S                   LIKE(TBLkey)
011800000412     D WAaas           S                   LIKE(TASaas)
011900000412     D Waas            S                   LIKE(TASaas)
012000000412     D Wlnp            S                   LIKE(TASlnp)
012100000412     D Wnrs            S                   LIKE(TASnrs)
012200000412     D Wnsp            S                   LIKE(TASnsp)
012300000412     D WAlnp           S                   LIKE(TASlnp)
012400000412     D WAnrs           S                   LIKE(TASnrs)
012500000412     D WAnsp           S                   LIKE(TASnsp)
012600000412     D WPaas           S                   LIKE(TASaas)
012700000412     D WPlnp           S                   LIKE(TASlnp)
012800000412     D WPnrs           S                   LIKE(TASnrs)
012900000412     D Wpnsp           S                   LIKE(TASnsp)
013000120530     D WLaas           S                   LIKE(TASaas)
013100120530     D WLlnp           S                   LIKE(TASlnp)
013200120530     D WLnrs           S                   LIKE(TASnrs)
013300120530     D WLnsp           S                   LIKE(TASnsp)
013400090423     D Wrigdue         S                   LIKE(RIGdela)  INZ
013500030508     D Wvabrmo         S                   LIKE(VABrmo)
013600030508     D Wvabrma         S                   LIKE(VABrma)
013700000322      *----------------
013800000322      * Variabili a programma
013900000322      *----------------
014000000413     D Wnumele         S              5P 0
014100000413     D WfileR          S             10
014200000412     D WfileT          S             10
014300020211     D Wlibfile        S             10
014400000412     D Wassegno        S              1
014500000419     D Wfirig          S              1
014600000412     D Wtipodata       S              1
014700000412     D Wtiporcd        S              1
014800000412     D Wazione         S              2  0
014900000412     D xx              S              2  0
015000000327     D yy              S              2  0
015100000327     D lenght          S             15  5 INZ(80)
015200000327     D comman          S             80
015300000322     D Wdata           S               D   DATFMT(*eur)
015400000327     D w0140           S             14  0
015500000327     D w0080           S              8  0
015600000412     D w0030           S              3  0
015700000327     D datcor          S              8  0
015800000412     d Wlegame         s              1
015900000413     D W008a           S              8
016000020304
016100020304     d rcdvab          s              7  0
016200020827     d wmbrvab         s              6
016300020827     d wmbrt           s             10
016400020304
016500020304      * work dati mamma originale
016600020304     d wpaasv          s              4
016700020304     d wplnpv          s              3
016800020304     d wpnrsv          s              2
016900020304     d wpnspv          s              7
017000020304     d wpkscv          s              7
017100020304     d wplnav          s              3
017200020305     d wpragv          s             35
017300020304     d wpdspv          s              8
017400020304      * work dati ultima figlia
017500020304     d waaasv          s              4
017600020304     d walnpv          s              3
017700020304     d wanrsv          s              2
017800020304     d wanspv          s              7
017900020304     d walnav          s              3
018000020304     d wapdcv          s              7
018100020304     d wandcv          s              7
018200020304     d waddcv          s              8
018300020304     d wadspv          s              8
018400020304
018500020304     d wrigpor         s              3
018600020304
018700020304     d wdatav          ds
018800020304     d  wgg                    1      2
018900020304     d  wmm                    3      4
019000020304     d  waa                    5      8
019100020304
019200020304     d wnumbolla       ds
019300020304     d  wvabaas                1      2
019400020304     d  wvablnp                3      5
019500020304     d  wvabnrs                6      7
019600020304     d  wvabnsp                8     14
019700020304
019800020304     D WLBDAT          DS                  INZ
019900020304     D  G02DAT                 1      8  0
020000020304     D  G02INV                 9     16  0
020100020304     D  G02ERR                17     17
020200020304     D  G02TGI                18     22  0
020300020304
020400020304      * Costanti per messaggi si P.E.I.
020500020408     d pe_testa1       c                   'Conferma - Richiesta Bolla Firmata'
020600130409     d*** pe_testa2       c                   'Ricerca - Bolla Firmata'
020700020304     d pe_riga1        c                   'Il p.o. xxx richiede l''invio della-
020800020305     d                                      lettera di vettura firmata'
020900020305     d pe_riga2        c                   'relativa alla spedizione n. xxx-xx--
021000020305     d                                     xxxxxxx del xx/xx/xxxx'
021100020304     d pe_riga3        c                   'cliente xxxxxxx xxxxxxxxxxxxxxxxxxx-
021200020304     d                                     xxxxxxxxxxxxxxxx'
021300020305     d pe_riga4        c                   'spedizione consegnata n. xxx-xx-xxx-
021400020305     d                                     xxxx del xx/xx/xxxx'
021500020305     d pe_riga5        c                   'legata a quella richiesta'
021600020305     d pe_riga6        c                   'distinta di consegna n. xxxxxxx del-
021700020304     d                                      xx/xx/xxxx autotrasp. xxxxxxx'
021800980928      * ---------------------------------------------------------
021900980928     IFNBLP000
022000980928     I              BLPATB                      ARBATB
022100000324     I              BLPFLB                      ARBFLB
022200000324     I              BLPDIM                      ARBDIM
022300000324     I              BLPAAS                      ARBAAS
022400980928     I              BLPLNP                      ARBLNP
022500980928     I              BLPNRS                      ARBNRS
022600980928     I              BLPNSP                      ARBNSP
022700980928     I              BLPCBO                      ARBCBO
022800980928     I              BLPMGS                      ARBMGS
022900980928     I              BLPFNS                      ARBFNS
023000980928     I              BLPSCL                      ARBSCL
023100980928     I              BLPFBR                      ARBFBR
023200000324     I              BLPFST                      ARBFST
023300980928     I              BLPFDN                      ARBFDN
023400980928     I              BLPLNA                      ARBLNA
023500980928     I              BLPZNC                      ARBZNC
023600980928     I              BLPKSC                      ARBKSC
023700980928     I              BLPRSM                      ARBRSM
023800980928     I              BLPINM                      ARBINM
023900980928     I              BLPCAM                      ARBCAM
024000980928     I              BLPLOM                      ARBLOM
024100980928     I              BLPNZM                      ARBNZM
024200980928     I              BLPPRM                      ARBPRM
024300980928     I              BLPFAP                      ARBFAP
024400980928     I              BLPCPI                      ARBCPI
024500980928     I              BLPRSD                      ARBRSD
024600980928     I              BLPIND                      ARBIND
024700980928     I              BLPCAD                      ARBCAD
024800980928     I              BLPLOD                      ARBLOD
024900980928     I              BLPNZD                      ARBNZD
025000980928     I              BLPPRD                      ARBPRD
025100980928     I              BLPFIN                      ARBFIN
025200980928     I              BLPGC1                      ARBGC1
025300980928     I              BLPGC2                      ARBGC2
025400980928     I              BLPCTR                      ARBCTR
025500980928     I              BLPCTS                      ARBCTS
025600980928     I              BLPFTM                      ARBFTM
025700980928     I              BLPTSP                      ARBTSP
025800980928     I              BLPNAS                      ARBNAS
025900980928     I              BLPNCL                      ARBNCL
026000980928     I              BLPNCP                      ARBNCP
026100980928     I              BLPPKC                      ARBPKC
026200980928     I              BLPPKB                      ARBPKB
026300980928     I              BLPPKF                      ARBPKF
026400980928     I              BLPNCR                      ARBNCR
026500980928     I              BLPVLC                      ARBVLC
026600980928     I              BLPFVB                      ARBFVB
026700980928     I              BLPVLB                      ARBVLB
026800980928     I              BLPFVF                      ARBFVF
026900980928     I              BLPVLF                      ARBVLF
027000980928     I              BLPQFT                      ARBQFT
027100980928     I              BLPVMD                      ARBVMD
027200980928     I              BLPVAD                      ARBVAD
027300980928     I              BLPIAS                      ARBIAS
027400980928     I              BLPVAS                      ARBVAS
027500980928     I              BLPCCM                      ARBCCM
027600980928     I              BLPRMN                      ARBRMN
027700980928     I              BLPRMA                      ARBRMA
027800980928     I              BLPRMO                      ARBRMO
027900980928     I              BLPFLS                      ARBFLS
028000980928     I              BLPNCD                      ARBNCD
028100980928     I              BLPNCA                      ARBNCA
028200980928     I              BLPXCO                      ARBXCO
028300980928     I              BLPCTM                      ARBCTM
028400980928     I              BLPFFD                      ARBFFD
028500000324     I              BLPDRT                      ARBDRT
028600000324     I              BLPNRT                      ARBNRT
028700000324     I              BLPFPP                      ARBFPP
028800000324     I              BLPPDR                      ARBPDR
028900000324     I              BLPDSE                      ARBDSE
029000980928     I              BLPTCR                      ARBTCR
029100980928     I              BLPDCR                      ARBDCR
029200980928     I              BLPHCR                      ARBHCR
029300980928     I              BLPDTI                      ARBDTI
029400980928     I              BLPHTI                      ARBHTI
029500980928     I              BLPDCE                      ARBDCE
029600980928     I              BLPNPI                      ARBNPI
029700980928     I              BLPNCI                      ARBNCI
029800980928     I              BLPNRC                      ARBNRC
029900980928     I              BLPDBR                      ARBDBR
030000980928     I              BLPNFV                      ARBNFV
030100980928     I              BLPDPC                      ARBDPC
030200980928     I              BLPDUC                      ARBDUC
030300980928     I              BLPFLP                      ARBFLP
030400980928     I              BLPDET                      ARBDET
030500980928     I              BLPDUT                      ARBDUT
030600980928     I              BLPDAM                      ARBDAM
030700980928     I              BLPNDC                      ARBNDC
030800980928     I              BLPDDC                      ARBDDC
030900980928     I              BLPDCP                      ARBDCP
031000980928     I              BLPDCM                      ARBDCM
031100980928     I              BLPHMC                      ARBHMC
031200980928     I              BLPTC1                      ARBTC1
031300980928     I              BLPTC2                      ARBTC2
031400980928     I              BLPCCA                      ARBCCA
031500980928     I              BLPTMC                      ARBTMC
031600980928     I              BLPGGA                      ARBGGA
031700980928     I              BLPGMA                      ARBGMA
031800980928     I              BLPGVA                      ARBGVA
031900980928     I              BLPFLE                      ARBFLE
032000980928     I              BLPTFP                      ARBTFP
032100980928     I              BLPFEA                      ARBFEA
032200980928     I              BLPTFA                      ARBTFA
032300980928     I              BLPFT1                      ARBFT1
032400980928     I              BLPDT1                      ARBDT1
032500980928     I              BLPFT2                      ARBFT2
032600980928     I              BLPDT2                      ARBDT2
032700980928     I              BLPFT3                      ARBFT3
032800980928     I              BLPDT3                      ARBDT3
032900030110
033000000322      *--------------------------------------------------------------------------------------------*
033100000322      * Main lines
033200000322      *--------------------------------------------------------------------------------------------*
033300090423     C     Wrigdue       setll     FnRIG000
033400090423     C     Wrigdue       READE     FnRIG000
033500000419      *
033600000419      * Elaboro il file richiesto fino alla fine se i file di trasmissione
033700000419      * sono allocabili
033800090423     C                   DOW       not %EOF(fnrig01L)  and  *IN91 = *off
033900040507
034000090423      * Elaboro solo i record 'F' 'C' 'D'
034100121023if  2c                   If        RigCau = 'F' or RigCau = 'D' or
034200121023     c                             RigCau = '3'
034300000418      *
034400000419     c                   clear                   Wfirig
034500000419     c                   clear                   ds_titas
034600000412     c                   clear                   ds_titas_2
034700000327      *
034800000419      * aggancio TITAS, se non lo trovo accendo *in92                        interessato
034900000419     c                   eval      Waas = RIGaas
035000000419     c                   eval      Wlnp = RIGlnp
035100000419     c                   eval      Wnrs = RIGnrs
035200000419     c                   eval      Wnsp = RIGnsp
035300000419     C                   eval      *in92 = *off
035400000419     C                   exsr      AGGTAS
035500000419      *
035600000419     c                   IF        *in92 = *off
035700000322      * legge il file fisico e scrive il membro di trasmissione
035800000322     C                   EXSR      letscr
035900000418     C                   ENDIF
036000040507
036100040507e   2c                   EndIf
036200000418      *
036300090423     C     Wrigdue       READE     FnRIG000
036400000418     C                   ENDDO
036500000322      *
036600000418     C                   Exsr      chiusure
036700000418     C                   eval      *inlr = *on
036800000322      *--------------------------------------------------------------------------------------------*
036900000419      * Aggancio file TITAS se cambiato x legami
037000000322      *--------------------------------------------------------------------------------------------*
037100000419     C     AGGTAS        BEGSR
037200000322      *
037300000419      * verifico l'esistenza della spedizione
037400000419     c     KBOLw         chain     TITAS30c
037500000419      *
037600000419     C                   IF        NOT %FOUND(titas30c)
037700000419     C                   eval      *in92 = *on
037800000419     c                   ELSE
037900000419      *
038000000419      *   se tipo bolla non è valido cerco altro rcd
038100000419     C                   clear                   DSTB
038200000419     C                   eval      XX = 1
038300000419     C                   eval      W008a = TAStbl
038400000419     C     W008a         lookup    TblK(xx)                               31
038500000419     C                   movel     TblD(xx)      DSTB
038600000419     c                   if        §TBrbl = 'R'
038700000419     c                   movel     ds_titas      ds_titas_2
038800000419     c                   clear                   ds_titas
038900000509     c     KBOLw         reade     TITAS30c                               92
039000000419     c                   endif
039100000419      *
039200000419     C                   ENDIF
039300000419      *
039400000419     C                   ENDSR
039500000419      *--------------------------------------------------------------------------------------------*
039600000419      * letscr - legge il file fisico e scrive il membro di trasmissione
039700000419      *--------------------------------------------------------------------------------------------*
039800000419     C     letscr        BEGSR
039900000419      *
040000000412     c                   clear                   Wlegame
040100000322      *
040200120601      * BOLLE LEGATE:
040300120601      * se rigenerazione per bolla firmata trasmetto:
040400120601      *                                       in partenza la Mamma
040500000322      *                                       in arrivo l'Ultima Figlia
040600120601      * In tutti gli altri casi trasmetto tutte le bolle sia a partenza che ad
040700120601      * arrivo
040800000418     c                   exsr      legbol
040900000322      *
041000120605     c                   select
041100000322      * Imposto key sped se non ho bolla legata
041200120605    3c                   when      Wlegame = *blanks                            * bolla di reso
041300000322     c                   z-add     rigaas        WPaas
041400000322     c                   z-add     riglnp        WPlnp
041500000322     c                   z-add     rignrs        WPnrs
041600000322     c                   z-add     rignsp        WPnsp
041700000322     c                   z-add     rigaas        WAaas
041800000322     c                   z-add     riglnp        WAlnp
041900000322     c                   z-add     rignrs        WAnrs
042000000322     c                   z-add     rignsp        WAnsp
042100120605     c                   exsr      elatra
042200120605     g* Rigenerazione per bolla firmata
042300120605     c                   when      rigcau='F'
042400120605     c                   exsr      elatra
042500120605     c                   other
042600120601     c* se presente legame bolla devo trasmettere tutte le bolle legate ad eccezione
042700120601     c* della rigenerazione per bolle firmate che in questo caso serve solo la mamma e l'ultima
042800120601     c* figlia
042900120530     c                   eval      wlaas=lblaao
043000120530     c                   eval      wllnp=LBLLPO
043100120530     c                   eval      wlnrs=LBLNRO
043200120530     c                   eval      wlnsp=LBLNSO
043300120530     c     klbl2         setll     fnlbl02l
043400120530     c     klbl2         reade     fnlbl02l
043500120530     c                   z-add     lblAAO        WPaas
043600120530     c                   z-add     lblLPO        WPlnp
043700120530     c                   z-add     lblNRO        WPnrs
043800120530     c                   z-add     lblNSO        WPnsp
043900120530     c                   z-add     lblAAO        WAaas
044000120530     c                   z-add     lblLPO        WAlnp
044100120530     c                   z-add     lblNRO        WAnrs
044200120530     c                   z-add     lblNSO        WAnsp
044300120530     c                   exsr      ELATRA
044400120605     c                   dow       not %eof(fnlbl02l)
044500120530     c                   z-add     lblAAN        WPaas
044600120530     c                   z-add     lblLPN        WPlnp
044700120530     c                   z-add     lblNRN        WPnrs
044800120530     c                   z-add     lblNSN        WPnsp
044900120530     c                   z-add     lblAAN        WAaas
045000120530     c                   z-add     lblLPN        WAlnp
045100120530     c                   z-add     lblNRN        WAnrs
045200120530     c                   z-add     lblNSN        WAnsp
045300000322      *
045400120530     C                   EXSR      ELATRA
045500020304
045600120601     c                   eval      Wlaas=LBLAAN
045700120601     c                   eval      Wllnp=LBLLPN
045800120601     c                   eval      Wlnrs=LBLNRN
045900120601     c                   eval      Wlnsp=LBLNSN
046000120605     c     klbl2         setll     fnlbl02l
046100120605     c     klbl2         reade     fnlbl02l
046200120530     c                   enddo
046300120605     c                   endsl
046400120530     c*
046500000323      *
046600000323     c                   ENDSR
046700120530      *--------------------------------------------------------------------------------------------*
046800120530      * Elaboro per trasmissione                                            to
046900120530      *--------------------------------------------------------------------------------------------*
047000120530     C     ELATRA        BEGSR
047100120530      * Elaboro per trasmissione alla partenza, all'arrivo o entrambe
047200120530      * solo se la causale è Danni o C/A Omessi
047300120530      * Se la causale è F= Bolle Firmate devo trasmettere
047400120530      * 1 msg di PEI alla Partenza e 1 all'Arrivo e scrivere
047500120530      * il file FIVABBFT da inviare all'arrivo
047600120530     C                   SELECT
047700120530     c                   WHEN      rigCAU = 'F'
047800120530     c                   exsr      trasvab
047900120530     C                   WHEN      rigTRA = 'P'
048000120530     c                   exsr      traspar
048100120530     C                   WHEN      rigTRA = 'A'
048200120530     c                   exsr      trasarr
048300120530     C                   WHEN      rigTRA = 'E'
048400120530     c                   exsr      traspar
048500120530     c                   If        *in91 = *off  and  *in92 = *off
048600120530     c                   exsr      trasarr
048700120530     c                   Endif
048800120530     c                   ENDSL
048900120530     c                   endsr
049000000323      *--------------------------------------------------------------------------------------------*
049100000404      * Trasmissione in partenza                                            to
049200000323      *--------------------------------------------------------------------------------------------*
049300000323     C     TRASPAR       BEGSR
049400000323      *
049500000404     C                   clear                   filet
049600000404     C                   clear                   rcdt
049700000404      *
049800000405      * Gestione file per trasmissione in partenza
049900000405     c                   exsr      GESfiltrpa
050000000323      *
050100000322      * se 91 acceso vuol dire che non e' stato possibile allocare il membro interessato
050200000324     c                   IF        *in91 = *off
050300000324      *
050400000419      * Verifico se devo riagganciare TITAS
050500000419     C                   IF        WPaas <> TASaas  or  WPlnp <> TASlnp  or
050600000419     C                             WPnrs <> TASnrs  or  WPnsp <> TASnsp
050700000419     c                   clear                   ds_titas
050800000419     c                   clear                   ds_titas_2
050900000419     c                   eval      Waas = WPaas
051000000419     c                   eval      Wlnp = WPlnp
051100000419     c                   eval      Wnrs = WPnrs
051200000419     c                   eval      Wnsp = WPnsp
051300000419     c                   eval      *in92 = *off
051400000419      * se non trovo TITAS accendo *in92                                     interessato
051500000419     C                   exsr      AGGTAS
051600000419     C                   Endif
051700000419      *
051800000419     c                   IF        *in92 = *off
051900000419      *
052000000419      * Scrivo i rcd da trasmettere
052100000328     c                   eval      Wtiporcd = 'P'
052200000328     c                   exsr      wrtrcd
052300000322      *
052400000322      * chiude il membro di trasmissione
052500000404     C                   exsr      closepfpar
052600000404      *
052700000404      * TRASMETTO
052800000404     C                   exsr      trasmi
052900000324      *
053000000324      * Aggiorno i record del fisico trasmessi
053100000419     C                   IF        Wfirig <> 'S'
053200090423     C                   z-add     datcor        RIGdela                        *data trasmissione
053300090423     C                   UPDATE    FnRIG000
053400000419     c                   eval      Wfirig = 'S'
053500000419     c                   ENDIF
053600000404      *
053700000404     c                   ENDIF
053800000419     c                   ENDIF
053900000324      *
054000000322     C                   ENDSR
054100000405      *--------------------------------------------------------------------------------------------*
054200000405      * Trasmissione in arrivo                                              to
054300000405      *--------------------------------------------------------------------------------------------*
054400000405     C     TRASARR       BEGSR
054500000405      *
054600000405     C                   clear                   filet
054700000405     C                   clear                   rcdt
054800000405      *
054900000405      * Gestione file per trasmissione in arrivo
055000000405     c                   exsr      GESfiltrar
055100000405      *
055200000405      * se 91 acceso vuol dire che non e' stato possibile allocare il membro interessato
055300000405     c                   IF        *in91 = *off
055400000419      *
055500000419      * Verifico se devo riagganciare TITAS
055600000419     C                   IF        WAaas <> TASaas  or  WAlnp <> TASlnp  or
055700000419     C                             WAnrs <> TASnrs  or  WAnsp <> TASnsp
055800000419     c                   clear                   ds_titas
055900000419     c                   clear                   ds_titas_2
056000000419     c                   eval      Waas = WAaas
056100000419     c                   eval      Wlnp = WAlnp
056200000419     c                   eval      Wnrs = WAnrs
056300000419     c                   eval      Wnsp = WAnsp
056400000419     c                   eval      *in92 = *off
056500000419      * se non trovo TITAS accendo *in92                                     interessato
056600000419     C                   exsr      AGGTAS
056700000509      * se 92 acceso vuol dire che non ha trovato il record giusto
056800000509      * o la bolla legata è una bolla di recupero da non trasmettere
056900000509     c                   if        *in92
057000000509     c                   clear                   ds_titas
057100000509     c                   clear                   ds_titas_2
057200000509     c                   eval      Waas = WPaas
057300000509     c                   eval      Wlnp = WPlnp
057400000509     c                   eval      Wnrs = WPnrs
057500000509     c                   eval      Wnsp = WPnsp
057600000509     c                   eval      *in92 = *off
057700000509      * se non trovo TITAS accendo *in92                                     interessato
057800000509     C                   exsr      AGGTAS
057900000509     c                   Endif
058000000509      *
058100000419     C                   Endif
058200000419      *
058300000419     c                   IF        *in92 = *off
058400000405      *
058500000405      * Scrivo i rcd da trasmettere
058600000405     c                   eval      Wtiporcd = 'A'
058700000405     c                   exsr      wrtrcd
058800000405      *
058900000405      * chiude il membro di trasmissione
059000000405     C                   exsr      closepfarr
059100000405      *
059200000405      * TRASMETTO
059300000405     C                   exsr      trasmi
059400000419      *
059500000419      * Aggiorno i record del fisico trasmessi
059600000419     C                   IF        Wfirig <> 'S'
059700090423     C                   z-add     datcor        RIGdela                        *data trasmissione
059800090423     C                   UPDATE    FnRIG000
059900000419     c                   eval      Wfirig = 'S'
060000000419     c                   ENDIF
060100000405      *
060200000405     c                   ENDIF
060300000419     c                   ENDIF
060400000405      *
060500000405     C                   ENDSR
060600020304      *--------------------------------------------------------------------------------------------*
060700020925      * Trasmissione msg di P.E.I. e FIVABBFT
060800020304      *--------------------------------------------------------------------------------------------*
060900020304     c     trasvab       begsr
061000020304
061100020925      * recupero dati dal titas da scrivere nei messaggi di P.E.I. e nel FIVAB
061200020304     c                   exsr      recdati
061300020304
061400020304     c                   clear                   rcdvab
061500020827     c                   Movel     '046'         Wmbrvab
061600020304     c                   move      walnav        wmbrvab
061700020304
061800020925      * Gestione file per trasmissione msg e fivab
061900020304     c                   exsr      gesfilvab
062000020304
062100020304      * se 91 spento vuol dire che e' stato possibile allocare il membro interessato
062200020304     c                   IF        *in91 = *off
062300020304
062400020304      * Invio i messaggi di P.E.I.
062500020304     c                   exsr      crtposta
062600020304
062700020304      * Scrivo il rcd da trasmettere
062800020304     c                   exsr      wrtvab
062900020304
063000020304      * chiude il membro di trasmissione
063100020304     c                   exsr      closepfvab
063200020304
063300020304      * Trasmetto
063400020304     c                   exsr      trasmivab
063500020304
063600020304      * Aggiorno i record del fisico trasmessi
063700020304     c                   if        wfirig <> 'S'
063800090423     c                   z-add     datcor        rigdela                        *data trasmissione
063900090423     c                   update    fnrig000
064000020304     c                   eval      wfirig = 'S'
064100020304     c                   endif
064200020304
064300020304     c                   endif
064400020304
064500020304     c                   endsr
064600020304      *
064700000322      *--------------------------------------------------------------------------------------------*
064800000405      * Controllo se la sped. da rigenerare ha dei legami bolla
064900000322      *--------------------------------------------------------------------------------------------*
065000981008     C     legbol        BEGSR
065100000418      *
065200000418      * Non considero i resi come bolle legate
065300020304      * se causale richiesta è DANNI
065400120530    3c****               IF        TASfbr = *blanks  or                         * bolla di reso
065500120530    3c****                         (TASfbr = 'S' and rigcau <> 'D')
065600000418      *
065700000418     c                   eval      *in81 = *on
065800000418     c                   eval      *in82 = *on
065900000509     c                   eval      *in32 = *off
066000000418      *
066100000418      * controllo se c'è un legame bolla
066200000418     c     klbl          chain     fnlbl01l                           82
066300000418     c   82klbl          chain     fnlbl02l                           81
066400000418      *
066500000418      * n81 oppure n82  c'è legame bolla
066600000418    2c                   if        *in81 = *off  or  *in82 = *off
066700000418      *
066800000418     c                   eval      Wlegame = 'S'
066900000418      *
067000000418      * DEFINISCO LE SPEDIZIONE DA TRASMETTERE
067100000418      *
067200120601   a c                   if        rigcau = 'F'
067300000418      * Mamma originale - PARTENZA
067400000418     c                   eval      WPaas = lblaao
067500000418     c                   eval      WPlnp = lbllpo
067600000418     c                   eval      WPnrs = lblnro
067700000418     c                   eval      WPnsp = lblnso
067800000418      *
067900000418      * Ultima figlia - ARRIVO
068000000509    4c                   DOW       not *in32
068100000418     c                   eval      WAaas = lblaan
068200000418     c                   eval      WAlnp = lbllpn
068300000418     c                   eval      WAnrs = lblnrn
068400000418     c                   eval      WAnsp = lblnsn
068500000509     c     klblA         chain     fnlbl02l                           32
068600000418    4c                   ENDDO
068700020701
068800020701      * se richiesta bolla firmata controllo l'ultima figlia
068900120601   a c***                if        rigcau = 'F'
069000020701     c                   eval      waas = waaas
069100020701     c                   eval      wlnp = walnp
069200020701     c                   eval      wnrs = wanrs
069300020701     c                   eval      wnsp = wansp
069400020701     c     kbolw         chain     titas30c
069500020701   b c                   if        %found(titas30c)
069600020701     c                   clear                   Dstb
069700020701     c                   eval      W008a = TAStbl
069800020701     c                   eval      XX = 1
069900020701     c     W008a         lookup    TblK(xx)                               31
070000020701     c                   movel     TblD(xx)      Dstb
070100020701      * se bolla recupero prendo la penultima figlia
070200020701   c c                   if        §TBrbl = 'R'
070300020701     c                   eval      WAaas = lblaap
070400020701     c                   eval      WAlnp = lbllpp
070500020701     c                   eval      WAnrs = lblnrp
070600020701     c                   eval      WAnsp = lblnsp
070700020701   c c                   endif
070800020701   b c                   endif
070900020701   a c                   endif
071000000418      *
071100000418    1c                   endif
071200000418      *
071300120530     C*****              ENDIF
071400000322      *
071500000323     c                   ENDSR
071600000405      *--------------------------------------------------------------------------------------------*
071700000405      * trasmi - trasmette il membro di trasmissione
071800000405      *--------------------------------------------------------------------------------------------*
071900980828     C     trasmi        BEGSR
072000000405      *
072100120607     c                   if        Wtiporcd='P'
072200000405     C                   MOVEL(P)  'SEDE'        cmzmbd                          *dal membro
072300000405     C                   MOVEL(P)  'SEDE'        cmzmba                          *al membro
072400120607     c                   else
072500120607     C                   MOVEL(P)  'SEDEA'       cmzmbd                          *dal membro
072600120607     C                   MOVEL(P)  'SEDEA'       cmzmba                          *al membro
072700120607     c                   endif
072800000405     C                   CLEAR                   cmzprr                          *prerequisito
072900981222     C                   MOVE      '0000'        cmzori                          *ora invio
073000981222     C                   MOVE      '0000000'     cmzdti                          *data invio
073100990208     C                   MOVEL     'N'           cmzurg                          *urgenza notte
073200981222     C                   MOVEL     'A'           cmzadd                          *record in aggiunta
073300981222     C                   MOVEL     'S'           cmzclr                          *no pulizia file
073400981222     C                   MOVEL     'S'           cmzftr                          *invio da transito
073500000405     C                   CLEAR                   cmzerr                          *flag errore('1')
073600000509     C                   CLEAR                   cmzazn                          *azione
073700000410      *
073800000410      * Destinatario
073900000412     c                   IF        Wtiporcd = 'P'
074000000412     c     TASlnp        chain     azorg
074100000412     C                   ELSE
074200000412     c     TASlna        chain     azorg
074300000412     C                   ENDIF
074400000412     c                   movel     orgfel        cmzdst                          *destinatario
074500000412      *
074600000412      * Cerco il terminal
074700000412     c                   IF        Wtiporcd = 'P'
074800000412     c                   eval      W0030 = TASlnp
074900000412     C                   ELSE
075000000412     c                   eval      W0030 = TASlna
075100000412     C                   ENDIF
075200000412     c                   eval      Wtipodata = 'T'
075300000412     c                   exsr      carter
075400000405      * cerco le librerie e i sistemi infomativi di arrivo
075500020827     C                   CLEAR                   TIBS55DS
075600981218     C                   MOVEL     knsif         i50psi
075700981218     C                   MOVEL     simfel        i50ppo
075800000412     C                   MOVEL     D55tfp        i50apo
075900020827     C                   CALL      'TIBS55R'
076000020827     C                   PARM                    TIBS55DS
076100981222     c                   movel     o50lcl        cmzlcl            1             *locale
076200000419      *
076300000419      * parto dalla libreria di gruppo
076400980928     c                   movel     o50plg        cmzlbd                         *dalla libreria
076500981218     c                   movel     o50asi        cmzsyf                         *sist.informativo
076600000404      *
076700000410     C                   eval      xx = 1
076800000410     C                   DOW       filet(xx) <> *blanks                         *se scritti dei rec
076900000404      *
077000000410     C                   MOVEL     FileT(xx)     cmzfld                          *dal file -trasm-
077100000410     C                   MOVEL     FileR(xx)     cmzfla                          *al file  -ricez-
077200000410     C                   MOVE      Rcdt(xx)      cmznrr                          *record inviati
077300000419      *
077400000419      * trasmetto nella libreria aziendale tranne per i legami (FNLBL00T)
077500000419      * che trasmetto nella libreria di gruppo
077600000419     C                   IF        FileT(xx) = 'FNLBL00T'                        *dal file -trasm-
077700000419     c                   movel     o50alg        cmzlba                         *alla  libreria
077800000419     c                   ELSE
077900000419     c                   movel     o50ala        cmzlba                         *alla  libreria
078000000419     c                   ENDIF
078100000412      * alla trasmissione di BLP/ARB aggancio l'esecuzione della ricezione
078200000413     C                   SELECT
078300000413     c                   WHEN      Wazione <> xx
078400000413     c                   WHEN      Wtiporcd = 'P'
078500000413     C                   movel     'BS22'        cmzazn                          *azione ricezione
078600000413     c                   OTHER
078700000413     C                   movel     'BS23'        cmzazn                          *azione ricezione
078800000413     c                   ENDSL
078900000412      *
079000020827     C                   CALL      'TIBS53C'
079100980911     C                   PARM                    cmzlcl
079200980828     C                   PARM                    cmzdst
079300980828     C                   PARM                    cmzfld
079400980828     C                   PARM                    cmzmbd
079500980828     C                   PARM                    cmzlbd
079600980828     C                   PARM                    cmzfla
079700980828     C                   PARM                    cmzmba
079800980828     C                   PARM                    cmzlba
079900980828     C                   PARM                    cmzori
080000980828     C                   PARM                    cmzdti
080100980828     C                   PARM                    cmzurg
080200980828     C                   PARM                    cmzadd
080300980828     C                   PARM                    cmzclr
080400980828     C                   PARM                    cmzftr
080500980828     C                   PARM                    cmzerr
080600980828     C                   PARM                    cmzazn
080700980828     C                   PARM                    cmzsyf
080800980828     C                   PARM                    cmznrr
080900980828     C                   PARM                    cmzprr
081000980911     C                   PARM                    w236            236
081100000410      *
081200000410     C                   add       1             xx
081300000410     c                   ENDDO
081400000405      *
081500980914     c                   exsr      finsr
081600000405      *
081700000419     C                   clear                   Wazione
081800000419      *
081900980828     C                   ENDSR
082000020304      *--------------------------------------------------------------------------------------------*
082100020925      * Trasmette il file FIVABBFT
082200020304      *--------------------------------------------------------------------------------------------*
082300020304     c     trasmivab     begsr
082400020304
082500020827     c                   Clear                   Wmbrt
082600020827     c                   Eval      Wmbrt = 'M' + Wmbrvab
082700020304     c                   movel(p)  wmbrt         cmzmbd                          *dal membro
082800020925     c                   movel(p)  'FIVAB00F'    cmzmba                          *al membro
082900020304     c                   clear                   cmzprr                          *prerequisito
083000020304     c                   move      '0000'        cmzori                          *ora invio
083100020304     c                   move      '0000000'     cmzdti                          *data invio
083200020304     c                   movel     'N'           cmzurg                          *urgenza notte
083300020304     c                   movel     'A'           cmzadd                          *record in aggiunta
083400020304     c                   movel     'S'           cmzclr                          *no pulizia file
083500020304     c                   movel     'S'           cmzftr                          *invio da transito
083600020304     c                   clear                   cmzerr                          *flag errore('1')
083700020304     c                   clear                   cmzazn                          *azione
083800020304
083900020304      * Destinatario
084000020304     c                   move      walnav        keyorg
084100020304     c     keyorg        chain     azorg
084200020304     c                   movel     orgfel        cmzdst                          *destinatario
084300020304
084400020304      * Cerco il terminal
084500020618     c                   movel     walnav        w0030
084600020304     c                   eval      Wtipodata = 'T'
084700020304     c                   exsr      carter
084800020304      * cerco le librerie e i sistemi infomativi di arrivo
084900020827     c                   clear                   tibs55ds
085000020304     c                   movel     knsif         i50psi
085100020304     c                   movel     simfel        i50ppo
085200020304     c                   movel     d55tfp        i50apo
085300020827     c                   call      'TIBS55R'
085400020827     c                   parm                    tibs55ds
085500020304     c                   movel     o50lcl        cmzlcl            1             *locale
085600020304
085700020304      * parto dalla libreria di gruppo
085800020304     c                   movel     o50plg        cmzlbd                         *dalla libreria
085900020304     c                   movel     o50asi        cmzsyf                         *sist.informativo
086000020304
086100020925     c                   movel     'FIVABBFT'    cmzfld                          *dal file -trasm-
086200020925     c                   movel     'FIVAB00F'    cmzfla                          *al file  -ricez-
086300020304     c                   move      rcdvab        cmznrr                          *record inviati
086400020304
086500020304      * trasmetto nella libreria aziendale
086600021009     c                   movel     o50alg        cmzlba                         *alla  libreria
086700020304
086800020827     C                   CALL      'TIBS53C'
086900020304     C                   PARM                    cmzlcl
087000020304     C                   PARM                    cmzdst
087100020304     C                   PARM                    cmzfld
087200020304     C                   PARM                    cmzmbd
087300020304     C                   PARM                    cmzlbd
087400020304     C                   PARM                    cmzfla
087500020304     C                   PARM                    cmzmba
087600020304     C                   PARM                    cmzlba
087700020304     C                   PARM                    cmzori
087800020304     C                   PARM                    cmzdti
087900020304     C                   PARM                    cmzurg
088000020304     C                   PARM                    cmzadd
088100020304     C                   PARM                    cmzclr
088200020304     C                   PARM                    cmzftr
088300020304     C                   PARM                    cmzerr
088400020304     C                   PARM                    cmzazn
088500020304     C                   PARM                    cmzsyf
088600020304     C                   PARM                    cmznrr
088700020304     C                   PARM                    cmzprr
088800020304     C                   PARM                    w236            236
088900020304
089000020304     c                   exsr      finsrvab
089100020304
089200020304     c                   endsr
089300000412      *--------------------------------------------------------------------------------------------*
089400000412      * carfil - carica filiali mper trasmissione dati
089500000412      *--------------------------------------------------------------------------------------------*
089600000412     C     carter        BEGSR
089700000412      *
089800000412     c                   clear                   FNLV55DS
089900000412     c                   movel     'P'           d55tpt
090000000412     c                   movel     W0030         d55lin
090100000412     c                   IF        Wtipodata = 'T'
090200000412     c                   movel     datcor        d55drf
090300000412     C                   ELSE
090400000412     c                   z-add     TASmgs        d55drf
090500000412     c                   movel     TASaas        d55drf
090600000412     c                   ENDIF
090700000412     c                   call      'FNLV55R'
090800000412     c                   parm                    FNLV55DS
090900000412      *
091000000412     c                   if        d55err <> *blanks
091100000412     c                   movel     W0030         d55tfp
091200000412     c                   endif
091300000412      *
091400000412     c                   endsr
091500000405      *--------------------------------------------------------------------------------------------*
091600000405      * finsr - disalloco membri di trasmissione
091700000405      *--------------------------------------------------------------------------------------------*
091800000322     C     finsr         BEGSR
091900000405      *
092000000413     c                   Do        wnumele       XX
092100000405      *
092200000405     c                   If        Wtiporcd = 'P'
092300000405     C                   MOVEL(P)  cmpDLC(xx)    comman
092400000405     c                   Else
092500000405     C                   MOVEL(P)  cmaDLC(xx)    comman
092600000405     c                   Endif
092700000405      *
092800000405     C                   CALL      'QCMDEXC'                            20
092900000405     C                   PARM                    comman
093000000405     C                   PARM                    lenght
093100000406      *
093200000405     c                   Enddo
093300000405      *
093400000322     C                   ENDSR
093500020304      *--------------------------------------------------------------------------------------------*
093600020925      * Disalloco membri di trasmissione FIVABBFT
093700020304      *--------------------------------------------------------------------------------------------*
093800020304     c     finsrvab      begsr
093900020304
094000020304     c                   movel(p)  cmdvab(5)     comman
094100020827     c                   eval      %subst(comman:54:6) = wmbrvab
094200020304     c                   call      'QCMDEXC'                            20
094300020304     c                   parm                    comman
094400020304     c                   parm                    lenght
094500020304
094600020304     c                   endsr
094700000404      *--------------------------------------------------------------------------------------------*
094800000404      * Scrittura archivi da trasmettere in partenza
094900000404      *--------------------------------------------------------------------------------------------*
095000000404     C     wrtrcd        BEGSR
095100000404      *
095200020204     C                   clear                   DSBL4A
095300000404     C                   clear                   DSBL4E
095400020204     C                   clear                   DSBL49
095500000404     C                   clear                   DTA4A
095600000404     C                   clear                   DS_titaa_o
095700000404     C                   clear                   DS_titaa_m
095800000404     C                   clear                   Wassegno
095900000413     C                   clear                   FIAR7000
096000000413     C                   clear                   FIAR6000
096100000413     C                   clear                   FNARB000
096200060221     C                   clear                   FIAR4000
096300051116     C                   clear                   FiAR9000
096400000413     C                   clear                   FNART000
096500041122     C****               clear                   FNLBL000
096600040831     C****               clear                   FNEVB000
096700000413     C                   clear                   FNBLP000
096800000413     C                   clear                   FNBLT000
096900000404      *
097000000419      * Leggo file clienti non codificati
097100000404     C     kspe          setll     TITAA30C
097200000404     C     kspe          reade     TITAA30C
097300000404     C                   DOW       not %EOF
097400000412     C                   SELECT
097500000404     c                   WHEN       TAAtrc = 'O'
097600000412     c                   movel     DS_titaa      DS_TITAA_O
097700000404     c                   WHEN       TAAtrc = 'M'
097800000412     c                   movel     DS_titaa      DS_TITAA_M
097900000404     C                   ENDSL
098000000404     C     kspe          reade     TITAA30C
098100000404     C                   ENDDO
098200000404      *
098300000404      * Leggo file C/assegno
098400000404     C     kspe          chain     TNCSB000
098500000407     c                   SELECT
098600000407     C                   WHEN      not %found
098700000413     C                   WHEN      CSBsta = 9
098800000407     C                   eval      Wassegno = 'A'
098900000407     C                   OTHER
099000000407     C                   eval      Wassegno = 'S'
099100000407     c                   ENDSL
099200000404      *
099300000404      * Leggo file riferimenti
099400000404     C                   eval      Ktrc = 'A'
099500000404     C     kta4          chain     TITA430C
099600000404     C                   If          %found
099700000404     C                   movel     TA4not        Dta4A
099800000404     c                   Endif
099900070927      * Leggo file riferimenti
100000070927     C                   eval      Ktrc = 'E'
100100070927     C     kta4          chain     TITA430C
100200070927     C                   If          %found
100300070927     C                   movel     TA4not        dsbl4E
100400070927     c                   Endif
100500000404      *
100600000404      * FNARB/FNBLP
100700000404     C                   eval      ARBaas = TASaas
100800000404     C                   eval      ARBlnp = TASlnp
100900000404     C                   eval      ARBnrs = TASnrs
101000000404     C                   eval      ARBnsp = TASnsp
101100000404     C                   eval      ARBmgs = TASmgs
101200000404     C                   eval      ARBfns = TASfns
101300060221     C****               eval      ARBscl = TASscl
101400000404     C                   eval      ARBfbr = TASfbr
101500000404     C                   eval      ARBfdn = TASfdn
101600000404     C                   eval      ARBlna = TASlna
101700000404     C                   eval      ARBznc = TASznc
101800000404     C                   eval      ARBksc = TASksc
101900000404     C                   eval      ARBfap = TASfap
102000000404     C                   eval      ARBrsd = TASrsd
102100000404     C                   eval      ARBind = TASind
102200000404     C                   eval      ARBcad = TAScad
102300000404     C                   eval      ARBlod = TASlod
102400000404     C                   eval      ARBprd = TASprd
102500000404     C                   eval      ARBnzd = TASnzd
102600000404     C                   eval      ARBfin = TASfin
102700000404     C                   eval      ARBgc1 = TASgc1
102800000404     C                   eval      ARBgc2 = TASgc2
102900000404     C                   eval      ARBctr = TASctr
103000000404     C                   eval      ARBcts = TAScts
103100000404     C                   eval      ARBftm = TASftm
103200000404     C                   eval      ARBtsp = TAStsp
103300000404     C                   eval      ARBnas = TASnas
103400000404     C                   eval      ARBncl = TASncl
103500000404     C                   eval      ARBncp = TASncp
103600000404     C                   eval      ARBpkc = TASpkc
103700000404     C                   eval      ARBpkb = TASpkb
103800000404     C                   eval      ARBpkf = TASpkf
103900000404     C                   eval      ARBncr = TASncr
104000000404     C                   eval      ARBvlc = TASvlc
104100000404     C                   eval      ARBfvb = TASfvb
104200000404     C                   eval      ARBvlb = TASvlb
104300000404     C                   eval      ARBfvf = TASfvf
104400000404     C                   eval      ARBvlf = TASvlf
104500000404     C                   eval      ARBqft = TASqft
104600001120     C                   If        TASfcm <> 'F'
104700000404     C                   eval      ARBias = TASias
104800000404     C                   eval      ARBvas = TASvas
104900001120     c                   EndIf
105000000404     C                   eval      ARBccm = TASccm
105100000404     C                   eval      ARBrmn = TASrmn
105200000404     C                   eval      ARBrma = §TA4Arma
105300000404     C                   eval      ARBrmo = OTAArsc
105400000404     C                   eval      ARBfls = TASfls
105500000404     C                   eval      ARBncd = TASncd
105600000404     C                   eval      ARBnca = TASnca
105700050113     C                   clear                   ARBxco
105800000404     C                   eval      ARBffd = TASffd
105900000404     C                   eval      ARBtcr = TAStcr
106000000404     C                   eval      ARBdcr = TASdcr
106100000404     C                   eval      ARBhcr = TAShcr
106200000404     C                   eval      ARBdti = TASdti
106300000404     C                   eval      ARBhti = TAShti
106400000404     C                   eval      ARBdce = TASdce
106500000404     C                   eval      ARBnpi = TASnpi
106600000404     C                   eval      ARBnci = TASnci
106700000404     C                   eval      ARBnrc = TASnrc
106800000404     C                   eval      ARBdbr = TASdbr
106900000404     C                   eval      ARBduc = TASduc
107000040528     C**!!!              eval      ARBflp = TASflp
107100040528     c                   Clear                   ArbFlp
107200000404     C                   eval      ARBpdc = TASpdc
107300000404     C                   eval      ARBndc = TASndc
107400000404     C                   eval      ARBddc = TASddc
107500000404     C                   eval      ARBdcp = TASdcp
107600000404     C                   eval      ARBdcm = TASdcm
107700000404     C                   eval      ARBhmc = TAShmc
107800000404     C                   eval      ARBtc1 = TASftc
107900000404     C                   eval      ARBtc2 = TAStc2
108000000404     C                   eval      ARBcca = TAScca
108100000404     C                   eval      ARBgga = TASgga
108200000404     C                   eval      ARBgma = TASgma
108300100604     c                   clear                   ds7R
108400100604     c                   If        ArbGma <>*blanks
108500100604     C                   eval      KCOD = '7R'
108600100604     C                   movel(P)  arbgma        KKEY
108700100604     C     keytab        chain     TABEL
108800100604     c                   IF        %found
108900100604     C                   movel     TBLUNI        ds7R
109000100604     c                   ENDIF
109100100604     c                   ENDIF
109200100604
109300100604     c                   If        §7rRC='S'
109400030801     c                   Clear                   ArbGma
109500030801     c                   EndIf
109600100604     c
109700000404     C                   eval      ARBgva = TASgva
109800000623     C                   Z-ADD     TASSTP        ARBSTP
109900000404     C                   eval      ARBctm = TASctm
110000000404     C                   eval      ARBdim = TASdim
110100000404     C                   eval      ARBdrt = TASdrt
110200000404     C                   eval      ARBflb = TASflb
110300000404     C                   eval      ARBfpp = TASfpp
110400000404     c                   eval      ARBnrt = TASnrt
110500040528     C                   eval      ARBtfp = TAStfp
110600000404     C                   eval      ARBtfa = TAStfa
110700000404     C                   eval      ARBdse = TASduc
110800000404     C                   eval      ARBdpc = TASduc
110900000404     C                   eval      ARBdam = TASdti
111000000404     C                   eval      ARBpdr = TASpdr
111100000404     C                   eval      ARBfst = TASll1
111200000404     C                   eval      ARBnfv = 99998
111300000404     C                   eval      ARBntc = 1
111400000404     C                   eval      ARBft3 = 'T'
111500000404     C                   eval      ARBdt3 = TASdti
111600000404     C                   eval      ARBamp = 1
111700000404     C                   eval      ARBama = 3
111800000404     C                   eval      ARBacc = 'S'
111900000404     C                   eval      ARBaca = 'S'
112000000404     C                   eval      ARBicc = 'Y'
112100000404     C                   eval      ARBica = 'Y'
112200020726     C                   eval      ARBifp = *zeros
112300000404     C                   eval      ARBfdc = 'S'
112400000428     C                   eval      ARBnmi = 989898
112500121023     c                   if        rigcau = '3'
112600020204     C                   eval      ARBnmi = 979797
112700020204     c                   end
112800000428     C                   eval      ARBdmi = datcor
112900000407      *
113000000404     C                   IF        MTAArsc <> *blanks
113100010726     C                   eval      ARBrsm = mTAArsc
113200010726     C                   eval      ARBinm = mTAAind
113300010726     C                   eval      ARBcam = mTAAcap
113400010726     C                   eval      ARBlom = mTAAloc
113500010726     C                   eval      ARBprm = mTAAprv
113600010726     C                   eval      ARBnzm = mTAAnaz
113700000404     C                   ELSE
113800000419     c                   z-add     TASccm        I69kac
113900000419     c                   z-add     TASccm        I69kin
114000000404     c                   EXSR      CTR_ANAGRA
114100000404     C                   eval      ARBrsm = ACOrag
114200000404     C                   eval      ARBinm = INDvia
114300000404     C                   eval      ARBcam = INDcae
114400000404     C                   eval      ARBlom = INDcit
114500000404     C                   eval      ARBprm = INDprv
114600000404     C                   eval      ARBnzm = INDsta
114700000404     C                   ENDIF
114800000404      *
114900020204     C                   eval      ARBcbo = TAScbo
115000020204      * Per contrassegni omessi
115100121023     C                   IF        RIGcau = '3'
115200020204     c                   select
115300020204     c                   when      %subst(tastbl: 1: 1) = 'F'
115400020204     C                   movel(p)  '4'           ARBcbo
115500020204     c                   when      %subst(tastbl: 1: 1) = 'A'
115600020204     C                   movel(p)  '6'           ARBcbo
115700020204     c                   endsl
115800020204     C                   clear                   ARBndc
115900000404     C                   eval      ARBicc = 'R'
116000000404     C                   ENDIF
116100000404      *
116200000404     c                   eval      W0030 = TASlnp
116300000412     c                   clear                   Wtipodata
116400000412     c                   exsr      carter
116500000404     c                   eval      ARBfle = d55tfp
116600000404      *
116700000404     c                   eval      W0030 = TASlna
116800000412     c                   clear                   Wtipodata
116900000412     c                   exsr      carter
117000000404     c                   eval      ARBfea = d55tfp
117100120601     c* recupero capofila della linea di arrivo per arbifp/Ipp
117200120606     c                   if        tasddc>0
117300120601     c                   clear                   fnlv55ds
117400120601     c                   eval      d55lin=taslna
117500120601     c                   eval      d55tpt='6'
117600120601     c                   eval      d55drf=tasddc
117700120601     c                   call      'FNLV55R'
117800120601     c                   parm                    FNLV55DS
117900120601     c                   eval      arbifp=d55tfa
118000120601     c                   eval      arbipp=arbifp
118100120606     c                   endif
118200000410      *
118300000414      * FIAR6-7 1a BOLLA
118400000414     c                   exsr      AGGfatt1
118500000414      *
118600000414      * Verifico se il tipo porto è tra quelli convertibili
118700000417     c                   clear                   DS3a
118800000417     c                   eval      xx = 1
118900000417     C     TAScbo        lookup    T3aK(xx)                               31
119000000417      *
119100000417     C                   if        *in31 = *on
119200000417     c                   movel     T3aD(xx)      DS3a
119300000414     C                   SELECT
119400000417      *  non convertibile
119500000417     C                   WHEN      §3aCfr = *blanks
119600000414      *  se il C/a è annullato converto il tipo bolla
119700000414     C                   WHEN      Wassegno = 'A'
119800000417     C                   movel     §3aCfr        ARBcbo
119900000417      *  FIAR6-7  2a BOLLA se esiste 2a bolla
120000000417     c                   WHEN      $TASaas > *zeros
120100000414     c                   exsr      AGGfatt2
120200000414     c                   ENDSL
120300000417     c                   endif
120400000404      *
120500051116      * FIAR9
120600000404     C                   IF        Wassegno = 'S'
120700000404     C                   eval      AR9aas = TASaas
120800000404     C                   eval      AR9lnp = TASlnp
120900000404     C                   eval      AR9nrs = TASnrs
121000000404     C                   eval      AR9nsp = TASnsp
121100000404     C                   eval      AR9cas = CSBcas
121200000404     C                   eval      AR9vca = CSBvca
121300000404     C                   eval      AR9cmb = CSBcmb
121400000404     C                   eval      AR9gca = CSBgca
121500090423     C                   If          RIGcau <> 'A'
121600121023     c*                  eval      AR9tic = CSBtpa + CSBtpi
121700121023     c                   movel     rigflo        ar9tic
121800000404     c                   Endif
121900051116      *
122000051116     C                   WRITE     FiAR9000
122100051116     C                   eval      WfileT = 'FiAR900T'
122200051116     C                   eval      WfileR = 'FiAR900R'
122300000412     C                   exsr      AGGnrec
122400000404      *
122500000404     C                   ENDIF
122600000404      *
122700060221      * FIAR4
122800000404     C                   eval      AR4aas = TASaas
122900000404     C                   eval      AR4lnp = TASlnp
123000000404     C                   eval      AR4nrs = TASnrs
123100000404     C                   eval      AR4nsp = TASnsp
123200000404      *   Tipo Record "A"
123300120607     c*****              If        Wtiporcd <> 'P'
123400031029     c                   Select
123500031029     c                   When      TasLl1 = 'C'
123600031029     c                   Eval      §B4Abm = 'Q'
123700031029     c                   When      TasLl1 = 'S'
123800031029     c                   Eval      §B4Abm = 'P'
123900050301     c                   When      TasLl1 = 'N' or Tasll1 = 'Y'
124000031029     c                   Eval      §B4Abm = 'J'
124100031113     c                   When      TasLl1 = 'K'
124200031113     c                   Eval      §B4Abm = 'W'
124300031029     c                   Other
124400000404     C                   eval      §B4Abm = TASll1
124500031029     c                   EndSl
124600000404     C                   eval      §B4Aso = '-'
124700060221     C                   movel(p)  DSbl4A        AR4not
124800000404     C                   eval      AR4trc = 'A'
124900060221     C                   WRITE     FIAR4000
125000060221     C                   eval      WfileT = 'FIAR400T'
125100060221     C                   eval      WfileR = 'FIAR400R'
125200000410     C                   exsr      AGGnrec
125300120607     C*****              Endif
125400000404      *   Tipo Record "F"
125500000404     C                   eval      Ktrc = 'F'
125600000404     C     kta4          chain     TITA430C
125700000404     C                   If          %found
125800060221     c                   clear                   ar4not
125900000404     C                   eval      AR4not = TA4not
126000000404     C                   eval      AR4trc = TA4trc
126100000404      *
126200060221     C                   WRITE     FIAR4000
126300060221     C                   eval      WfileT = 'FIAR400T'
126400060221     C                   eval      WfileR = 'FIAR400R'
126500000412     C                   exsr      AGGnrec
126600000404      *
126700000404     C                   Endif
126800060530      *   Tipo Record "I"
126900060530     C                   eval      Ktrc = 'I'
127000060530     C     kta4          chain     TITA430C
127100060530     C                   If          %found
127200060530     c                   clear                   ar4not
127300060530     C                   eval      AR4not = TA4not
127400060530     C                   eval      AR4trc = TA4trc
127500060530      *
127600060530     C                   WRITE     FIAR4000
127700060530     C                   eval      WfileT = 'FIAR400T'
127800060530     C                   eval      WfileR = 'FIAR400R'
127900060530     C                   exsr      AGGnrec
128000060530      *
128100060530     C                   Endif
128200000404      *   Tipo Record "E"
128300070927     C***                If        §TA4Arpt <> *blanks
128400070927     C***                eval      §B4Erp = §TA4Arpt
128500070927     c
128600070927     c* La dsble$ è già impostata per cui non devo scrivere nulla
128700070927     c                   if        §b4erp<>*blanks
128800000404      *
128900000404     C     TASlna        chain     AZORG
129000000404     C                   If        ORGdit = 'SDI'  and  ORGfl1 = 'E'
129100000404     C                   eval      §B4Ema = 'A'
129200000404     C                   eval      §B4Eat = 'P'
129300000404     C                   eval      §B4Est = 'S'
129400000404     C                   Endif
129500060221     C                   movel(P)  DSbl4E        AR4not
129600000404     C                   eval      AR4trc = 'E'
129700000404      *
129800060221     C                   WRITE     FIAR4000
129900060221     C                   eval      WfileT = 'FIAR400T'
130000060221     C                   eval      WfileR = 'FIAR400R'
130100000412     C                   exsr      AGGnrec
130200000412      *
130300000412     C                   Endif
130400000404      *
130500000404      *   Tipo Record "8"
130600000404     C                   eval      AR4trc = '8'
130700060221     c                   clear                   ar4not
130800000404     C                   eval      AR4not = 'BOLLA RIGENERATA DALLA SEDE'
130900000404      *
131000060221     C                   WRITE     FIAR4000
131100060221     C                   eval      WfileT = 'FIAR400T'
131200060221     C                   eval      WfileR = 'FIAR400R'
131300000412     C                   exsr      AGGnrec
131400020204      *   Tipo Record "9" per contrassegni omessi
131500121023     c                   if        rigcau = '3'
131600020204     C                   eval      AR4trc = '9'
131700020204     C                   eval      §b4ds9 = 'PER C/ASSEGNO OMESSO'
131800020206     C                   eval      §b4dt9 = datcor
131900060221     c                   clear                   ar4not
132000020204     C                   eval      AR4not = dsbl49
132100020204      *
132200060221     C                   WRITE     FIAR4000
132300060221     C                   eval      WfileT = 'FIAR400T'
132400060221     C                   eval      WfileR = 'FIAR400R'
132500020204     C                   exsr      AGGnrec
132600020204     C                   Endif
132700111011      *   Tipo Record "D"
132800111011     C                   eval      Ktrc = 'D'
132900111011     C     kta4          chain     TITA430C
133000111011     C                   If          %found
133100111011     c                   clear                   ar4not
133200111011     C                   eval      AR4not = TA4not
133300111011     C                   eval      AR4trc = TA4trc
133400111011      *
133500111011     C                   WRITE     FIAR4000
133600111011     C                   eval      WfileT = 'FIAR400T'
133700111011     C                   eval      WfileR = 'FIAR400R'
133800111011     C                   exsr      AGGnrec
133900111011      *
134000111011     C                   Endif
134100020204      *                                                                   CC
134200000404      * FNBLT/FNART
134300000404     C                   eval      ARTaas = TASaas
134400000404     C                   eval      ARTlnp = TASlnp
134500000404     C                   eval      ARTnrs = TASnrs
134600000404     C                   eval      ARTnsp = TASnsp
134700000419     C                   eval      ARTfls = TASfls
134800000419     C                   eval      ARTlna = TASlna
134900000404     C                   eval      ARTdfv = ARBduc
135000000518     C                   eval      ARTnpp = 86
135100000404     C                   eval      ARTnfv = 99998
135200000404     C                   eval      ARTdse = ARBdse
135300000518     C                   eval      ARTnpe = 86
135400000404     C                   eval      ARTdam = ARBdam
135500000404     C                   eval      ARTdcm = ARBdcm
135600000518     C                   eval      ARTnap = 86
135700000404     C                   eval      ARTabn = 'S'
135800000404      *   Numero segnacollo: se sequenziale sviluppo da titas
135900000404      *                      se non sequenziale sviluppo da titat
136000000404     C                   IF         TASFNS = 'S'
136100000404     c     kspe          setll     TITAT30c
136200000404     c     kspe          reade     TITAT30c
136300000404     c                   DOW       not %EOF
136400000404     c                   eval       ARTnsc = TATnsc
136500000404     c                   If        Wtiporcd = 'P'
136600000404     C                   WRITE     FNBLT000
136700000412     C                   eval      WfileT = 'FNBLT00T'
136800000412     C                   eval      WfileR = 'FNBLT00R'
136900000404     c                   Else
137000000404     C                   WRITE     FNART000
137100000412     C                   eval      WfileT = 'FNART00T'
137200000412     C                   eval      WfileR = 'FNART00R'
137300000404     C                   Endif
137400000412     C                   exsr      AGGnrec
137500000404     c     kspe          reade     TITAT30c
137600000404     C                   ENDDO
137700000404      *
137800000404     C                   ELSE
137900000404      *
138000000404     C                   If        TASncd > 0
138100000404      *
138200000404     C                   If          TASnca = 0
138300000404     C                   eval      TASnca = TASncd
138400000404     C                   Endif
138500000404      *
138600000404     C     TASNCD        DO        TASNCA        WSEGNA
138700000404     c                   eval       ARTnsc = Wsegna
138800000404     c                   If        Wtiporcd = 'P'
138900000404     C                   WRITE     FNBLT000
139000000412     C                   eval      WfileT = 'FNBLT00T'
139100000412     C                   eval      WfileR = 'FNBLT00R'
139200000404     c                   Else
139300000419     C                   WRITE     FNART000
139400000419     C                   eval      WfileT = 'FNART00T'
139500000419     C                   eval      WfileR = 'FNART00R'
139600000404     C                   Endif
139700000412     C                   exsr      AGGnrec
139800000404     C                   ENDDO
139900000404      *
140000000404     C                   Endif
140100000404     C                   ENDIF
140200000412      *
140300000412      * FNLBL
140400041122     c**                 IF        Wlegame = 'S'
140500041122     c**                 eval      Waas = WPaas
140600041122     c**                 eval      Wlnp = WPlnp
140700041122     c**                 eval      Wnrs = WPnrs
140800041122     c**                 eval      Wnsp = WPnsp
140900041122     c**   KBOLw         chain     fnlbl02l
141000000412      *
141100041122    4c**                 DOW       %FOUND
141200041122     c**                 eval      LBLft2 = 'T'
141300041122     C**                 write     FNLBL000
141400041122     C**                 eval      WfileT = 'FNLBL00T'
141500041122     C**                 eval      WfileR = 'FNLBL00R'
141600041122     C**                 exsr      AGGnrec
141700041122     c**                 eval      Waas = lblaan
141800041122     c**                 eval      Wlnp = lbllpn
141900041122     c**                 eval      Wnrs = lblnrn
142000041122     c**                 eval      Wnsp = lblnsn
142100041122     c**   KBOLw         chain     fnlbl02l
142200041122    4c**                 ENDDO
142300000412      *
142400041122    3c**                 ENDIF
142500000412      *
142600000412      * FNEVB
142700040831     C**** kspe          setll     FNEVB01L
142800040831     C**** kspe          READE     FNEVB01L
142900040831    4c****               DOW       not %EOF
143000000412      *
143100040831     C****               IF        EVBcev = 'R  '
143200040831     C****               write     FNEVB000
143300040831     C****               eval      WfileT = 'FNEVB00T'
143400040831     C****               eval      WfileR = 'FNEVB00R'
143500040831     C****               exsr      AGGnrec
143600040831     c****               ENDIF
143700000412      *
143800040831     C**** kspe          READE     FNEVB01L
143900040831    4c****               ENDDO
144000030110      *
144100030110      * FIAR5
144200030110     c                   exsr      srFIAR5
144300000412      *
144400000412      * Scrivo ARB/BLP. Devono essere gli ultimi file perchè a questi
144500000412      *                 è legata la call all'azione di ricezione.
144600000412     c                   If        Wtiporcd = 'P'
144700000412     C                   eval      BLPsop = '-'
144800000412     C                   WRITE     FNBLP000
144900000412     C                   eval      WfileT = 'FNBLP00T'
145000000412     C                   eval      WfileR = 'FNBLP00R'
145100000412     c                   Else
145200000412     C                   WRITE     FNARB000
145300000412     C                   eval      WfileT = 'FNARB00T'
145400000412     C                   eval      WfileR = 'FNARB00R'
145500000412     c                   Endif
145600000412     C                   exsr      AGGnrec
145700000412     c                   z-add     XX            Wazione
145800000404      *
145900000404     c                   endsr
146000020304      *--------------------------------------------------------------------------------------------*
146100020304      * Recupero dati dal titas
146200020304      *--------------------------------------------------------------------------------------------*
146300020304     c     recdati       begsr
146400020304
146500020304      * Verifico se devo riagganciare TITAS per recuperare i dati che servono
146600020304      * mamma originale
146700020304     c                   if        wpaas <> tasaas  or  wplnp <> taslnp  or
146800020304     c                             wpnrs <> tasnrs  or  wpnsp <> tasnsp
146900020304     c                   clear                   ds_titas
147000020304     c                   clear                   ds_titas_2
147100020304     c                   eval      waas = wpaas
147200020304     c                   eval      wlnp = wplnp
147300020304     c                   eval      wnrs = wpnrs
147400020304     c                   eval      wnsp = wpnsp
147500020304     c                   eval      *in92 = *off
147600020304      * se non trovo TITAS accendo *in92                                     interessato
147700020304     c                   exsr      AGGTAS
147800020304     c                   endif
147900020304
148000020304     c                   if        *in92 = *off
148100020304
148200020304     c                   movel     tasaas        wpaasv
148300020304     c                   movel     taslnp        wplnpv
148400020304     c                   movel     tasnrs        wpnrsv
148500020304     c                   movel     tasnsp        wpnspv
148600020304
148700090423      * Se tipo richiesta 'F'
148800090423     c                   If        Rigcau = 'F'
148900090423      * addebito mittente
149000090423     c                   if        rigadd = 'M'
149100020903     c                   movel     tasccm        wpkscv
149200040511      * ragione sociale mittente originale  valorizzato con la rag.sociale destinatario
149300040511     c                   movel     tasrsd        Wvabrmo
149400020903     c                   EndIf
149500090423      * addebito al destinatario
149600090423     c                   If        rigadd = 'D'
149700040517     c                   movel     tasksc        wpkscv
149800040511      * ragione sociale mittente originale  valorizzato con la rag.sociale mittente
149900040513     c                   Eval      ktrc = 'M'
150000040513     c     kta4          Chain     Titaa30c
150100040513     c                   If        %Found(Titaa30c)
150200040513     c                   eval      Wvabrmo = TaaRsc
150300040513     c                   EndIf
150400090423     c                   endif
150500030514      * riferimento mittente alfanumerico   valorizzato con rif.mittente numerico
150600030515     c                   eval      WVABrma= %editc(tasrmn:'Z')
150700030508     c                   endif
150800020304
150900020304     c                   movel     taslna        wplnav
151000030508
151100020304     c                   clear                   wlbdat
151200020304     c                   movel     tasaas        g02inv
151300020304     c                   move      tasmgs        g02inv
151400020304     c                   movel     '3'           g02err
151500020304     c                   call      'XSRDA8'
151600020304     c                   parm                    wlbdat
151700020304     c                   move      g02dat        wpdspv
151800020304      * ragione sociale
151900020304     c                   move      wpkscv        I69kac
152000020304     c                   EXSR      CTR_ANAGRA
152100020304     c                   eval      wpragv = acorag
152200020304
152300020304     c                   endif
152400020304
152500020304      * Verifico se devo riagganciare TITAS per recuperare i dati che servono
152600020304      * ultima figlia
152700020304     c                   if        waaas <> tasaas  or  walnp <> taslnp  or
152800020304     c                             wanrs <> tasnrs  or  wansp <> tasnsp
152900020304     c                   clear                   ds_titas
153000020304     c                   clear                   ds_titas_2
153100020304     c                   eval      waas = waaas
153200020304     c                   eval      wlnp = walnp
153300020304     c                   eval      wnrs = wanrs
153400020304     c                   eval      wnsp = wansp
153500020304     c                   eval      *in92 = *off
153600020304      * se non trovo TITAS accendo *in92                                     interessato
153700020304     c                   exsr      AGGTAS
153800020304     c                   endif
153900020304
154000020304     c                   if        *in92 = *off
154100020304
154200020304     c                   movel     tasaas        waaasv
154300020304     c                   movel     taslnp        walnpv
154400020304     c                   movel     tasnrs        wanrsv
154500020304     c                   movel     tasnsp        wanspv
154600020304
154700020304     c                   movel     taslna        walnav
154800020304     c                   movel     taspdc        wapdcv
154900020304     c                   movel     tasndc        wandcv
155000020304
155100020304     c                   clear                   wlbdat
155200020304     c                   movel     tasddc        g02inv
155300020304     c                   movel     '3'           g02err
155400020304     c                   call      'XSRDA8'
155500020304     c                   parm                    wlbdat
155600020304     c                   move      g02dat        waddcv
155700020304
155800020304     c                   clear                   wlbdat
155900020304     c                   movel     tasaas        g02inv
156000020304     c                   move      tasmgs        g02inv
156100020304     c                   movel     '3'           g02err
156200020304     c                   call      'XSRDA8'
156300020304     c                   parm                    wlbdat
156400020304     c                   move      g02dat        wadspv
156500020304
156600020304     c                   endif
156700020304
156800020304     c                   endsr
156900020304      *--------------------------------------------------------------------------------------------*
157000020304      * Scrittura messaggi P.E.I.
157100020304      *--------------------------------------------------------------------------------------------*
157200020304     c     crtposta      begsr
157300020304
157400090423     c                   movel     rigfil        wrigpor
157500020304
157600130409     C* Inizializzo variabili
157700130409     C                   movel     *blanks       wrkEmlTo        253
157800130409     C                   movel     *blanks       wrkEmlCc        253
157900130409     C                   movel     *blanks       wrkMsg         5000
158000130409     C                   movel     *blanks       wrkOgg           44
158100130409     C                   movel     *blanks       wrkesito          1
158200130409     c                   clear                   ym5t01           70
158300130409     c                   clear                   ym5t02           70
158400130409     c                   clear                   ym5t03           70
158500130409     c                   clear                   ym5t04           70
158600130409     c                   clear                   ym5t05           70
158700130409     c                   clear                   ym5t06           70
158800130411     c                   clear                   vuoto85          85
158900020304
159000130409     c                   eval      wrkogg = PE_testa1
159100020304
159200020304      * Preparo il messaggio uguale sia per la partenza che per l'arrivo
159300020304     c                   eval      ym5t01 = PE_riga1
159400020304     c                   eval      %subst(ym5t01:9:3) = wrigpor
159500020304
159600020304     c                   eval      ym5t02 = PE_riga2
159700020305     c                   eval      %subst(ym5t02:29:3) = wplnpv
159800020305     c                   eval      %subst(ym5t02:33:2) = wpnrsv
159900020305     c                   eval      %subst(ym5t02:36:7) = wpnspv
160000020304     c                   movel     wpdspv        wdatav
160100020305     c                   eval      %subst(ym5t02:48:2) = wgg
160200020305     c                   eval      %subst(ym5t02:51:2) = wmm
160300020305     c                   eval      %subst(ym5t02:54:4) = waa
160400020304
160500020304     c                   eval      ym5t03 = PE_riga3
160600020304     c                   eval      %subst(ym5t03:9:7) = wpkscv
160700020305     c                   eval      %subst(ym5t03:17:35) = wpragv
160800020305
160900020305      * se devo scrivere anche la spedizione figlia
161000020305     c                   if        wpaasv <> waaasv or  wplnpv <> walnpv or
161100020305     c                             wpnrsv <> wanrsv or  wpnspv <> wanspv
161200020305      * aggiungo righe della spedizione figlia
161300020305     c                   eval      ym5t04 = PE_riga4
161400020305     c                   eval      %subst(ym5t04:26:3) = walnpv
161500020305     c                   eval      %subst(ym5t04:30:2) = wanrsv
161600020305     c                   eval      %subst(ym5t04:33:7) = wanspv
161700020305     c                   movel     wadspv        wdatav
161800020305     c                   eval      %subst(ym5t04:45:2) = wgg
161900020305     c                   eval      %subst(ym5t04:48:2) = wmm
162000020305     c                   eval      %subst(ym5t04:51:4) = waa
162100020305
162200020305     c                   eval      ym5t05 = pe_riga5
162300020304
162400020305     c                   eval      ym5t06 = pe_riga6
162500020305     c                   eval      %subst(ym5t06:25:7) = wandcv
162600020304     c                   movel     waddcv        wdatav
162700020305     c                   eval      %subst(ym5t06:37:2) = wgg
162800020305     c                   eval      %subst(ym5t06:40:2) = wmm
162900020305     c                   eval      %subst(ym5t06:43:4) = waa
163000020305     c                   eval      %subst(ym5t06:59:7) = wapdcv
163100020305
163200130409     c                   eval      wrkmsg=ym5t01+ym5t02+ym5t03+ym5t04+
163300130409     c                                    ym5t05+ym5t06
163400020305     c                   else
163500020305
163600020305      * scrivo solo righe delle distinta di consegna
163700020305     c                   eval      ym5t04 = pe_riga6
163800020305     c                   eval      %subst(ym5t04:25:7) = wandcv
163900020305     c                   movel     waddcv        wdatav
164000020305     c                   eval      %subst(ym5t04:37:2) = wgg
164100020305     c                   eval      %subst(ym5t04:40:2) = wmm
164200020305     c                   eval      %subst(ym5t04:43:4) = waa
164300020305     c                   eval      %subst(ym5t04:59:7) = wapdcv
164400020305
164500130411     c                   eval      wrkmsg=%trim(ym5t01)+ ' ' +%trim(ym5t02)+
164600130411     c                              vuoto85+%trim(ym5t03)+'  ' +ym5t04
164700130411     c
164800020305     c                   endif
164900020617
165000020617      * Utente + p.o. CHE EFFETTUA LA RICHIESTA
165100130409     c***                z-add     rigfil        ym5fde
165200130409     c***                eval      ym5ude = 'RAC'
165300130409     C                   eval      wrkEmlTo='INFO'+ %editc(rigfil:'X')+
165400130409     c                                             '@brt.it'
165500130409     C*
165600130409     C                   call(e)   'TIS701C1'
165700130409     C                   parm                    wrkEmlto
165800130409     C                   parm                    wrkEmlCc
165900130409     C                   parm                    wrkOgg
166000130409     C                   parm                    wrkMsg
166100130409     C                   parm                    wrkesito
166200020304
166300020617
166400020617      * Utente + p.o. CHE DEVE RICERCARE LA BOLLA
166500130409     c***                movel     walnav        ym5fde
166600130409     c***                eval      ym5ude = 'RAM'
166700130409     c
166800130409     C                   eval      wrkEmlTo='INFO'+ walnav  +
166900130409     c                                             '@brt.it'
167000130409     C*
167100130409     C                   call(e)   'TIS701C1'
167200130409     C                   parm                    wrkEmlto
167300130409     C                   parm                    wrkEmlCc
167400130409     C                   parm                    wrkOgg
167500130409     C                   parm                    wrkMsg
167600130409     C                   parm                    wrkesito
167700020304
167800020304     c                   endsr
167900020304      *--------------------------------------------------------------------------------------------*
168000020925      * Scrittura FIVABBFT
168100020304      *--------------------------------------------------------------------------------------------*
168200020304     c     wrtvab        begsr
168300020304
168400020925     c                   clear                   fivab000
168500020715
168600020715      * Imposto data generazione bolla
168700020715     c                   movel     datcor        vabaas
168800020715     c                   move      datcor        vabmgs
168900020304
169000020304      * cap - località- provincia destinatario: del p.o. che ha fatto la richiesta
169100090423     c     rigfil        chain     azorg01l
169200020304     c                   if        not %found(azorg01l)
169300020304     c                   clear                   orgcpf
169400020304     c                   clear                   orgloc
169500020304     c                   clear                   orgpro
169600020304     c                   endif
169700020304
169800020304      * p.o. arrivo: il p.o. che ha fatto la richiesta
169900090423     c                   eval      vablna = rigfil
170000020304      * p.o. partenza: la linea di arrivo della bolla ultima figlia
170100020304     c                   movel     walnav        vablnp
170200020304      * cliente: linea di arrivo ultima figlia + 9999
170300020304     c                   movel     walnav        vabccm
170400020304     c                   move      9999          vabccm
170500020315      * tipo bolla: 'F'
170600020510     c                   eval      vabcbo ='FO'
170700020315      * codice tratamento merce: '4F'
170800020315     c                   eval      vabctm ='4F'
170900020304      * destinatario
171000020530     c                   eval      vabrsd = 'RAC Cli. ' + wpkscv + ' ' +
171100020508     c                             '- Bolla firmata'
171200020304     c                   eval      vabind = wpragv
171300020304     c                   movel     orgcpf        vabcad
171400020304     c                   eval      vablod = orgloc
171500020304     c                   eval      vabprd = orgpro
171600020304      * natura merce
171700020304     c                   eval      vabnas = 'BOLLA FIRMATA'
171800020304      * colli
171900020304     c                   eval      vabncl = 1
172000020506      * peso
172100020506     c                   eval      vabpkb = 1
172200020304      * riferimento mittente numerico: aa-lnp-nrs-nsp della spedizione mamma originale
172300020304     c                   move      wpaasv        wvabaas
172400020304     c                   movel     wplnpv        wvablnp
172500020304     c                   movel     wpnrsv        wvabnrs
172600020304     c                   movel     wpnspv        wvabnsp
172700020304     c                   move      wnumbolla     vabrmn
172800020304      * note
172900020304     c                   movel     waddcv        wdatav
173000020304     c                   eval      vabnot = 'Distinta n.' + ' ' + wandcv + ' ' +
173100020304     c                             'del' + ' ' + wgg + '/' + wmm + '/' + waa
173200020304
173300020304     c                   add       1             rcdvab
173400020925      *
173500020925      * P.O di gestione
173600020925     c                   move      walnav        vabfgs
173700030508      *
173800030508      * RMO + RMA
173900030508     c                   eval      vabrmo = wvabrmo
174000030508     c                   eval      vabrma = wvabrma
174100030508      *
174200020925      *
174300020925     c                   write     fivab000
174400020304
174500030508     c                   clear                   wvabrmo
174600030508     c                   clear                   wvabrma
174700030508
174800020304     c                   endsr
174900000414      *****************************************************************
175000000414      *  Creo file fatturazione per prima bolla
175100000414      *****************************************************************
175200000414     C     AGGfatt1      BEGSR
175300000414      *
175400000414      * ricavo il tipo porto
175500000414     C                   clear                   DSTB
175600000414     C                   eval      XX = 1
175700000414     C                   eval      W008a = TAStbl
175800000417     C     W008a         lookup    TblK(xx)                               31
175900000417     C                   movel     TblD(xx)      DSTB
176000000414      *
176100000414     C                   SELECT
176200000414      * Non elaboro fatture non emesse dal P.O.
176300000414     c                   WHEN      TASfiv = *zeros  or
176400000414     c                             TASfiv = §QCfiv  or TASfiv = §QCfi2  or
176500000414     c                             TASfiv = §QCfi3  or TASfiv = §QCfi4  or
176600000414     c                             TASfiv = §QCfi5  or TASfiv = §QCfi6
176700000414      * Non elaboro i porti franchi se trasmetto in arrivo
176800120607     c******             WHEN      §TBtpo = 'F' and Wtiporcd <> 'P'
176900000414      * Se porto assegnato da trasmettere in partenza invio solo KSC e CTR
177000120607     c*****              WHEN      §TBtpo = 'A' and Wtiporcd = 'P'
177100120607     C******             eval      AR6aas = TASaas
177200120607     C******             eval      AR6lnp = TASlnp
177300120607     C*****              eval      AR6nrs = TASnrs
177400120607     C******             eval      AR6nsp = TASnsp
177500120607     C********           eval      AR6trc = '1'
177600120607     C*********          eval      AR6ksc = TASksc
177700120607     C********           eval      AR6ctr = TASctr
177800120607     C********           WRITE     FIAR6000
177900120607     C********           eval      WfileT = 'FIAR600T'
178000120607     C********           eval      WfileR = 'FIAR600R'
178100120607     C********           exsr      AGGnrec
178200000414      *
178300000414     c                   OTHER
178400000414     C                   eval      AR6div = TASdiv
178500000414     C                   eval      AR6por = TASpor
178600000414     C                   eval      AR6sv1 = TASsv1
178700000414     C                   eval      AR6va1 = TASva1
178800000414     C                   eval      AR6sv2 = TASsv2
178900000414     C                   eval      AR6va2 = TASva2
179000000414     C                   eval      AR6sv3 = TASsv3
179100000414     C                   eval      AR6va3 = TASva3
179200000414     C                   eval      AR6imv = TASimv
179300120103     c                   if        tasnft>0
179400120103     c                   if        tasdft>§qtdsa
179500000414     C                   eval      AR6ali = §QTali
179600120103     c                   else
179700120103     C                   eval      AR6ali = §QTalo
179800120103     c                   endif
179900120103     c                   endif
180000000414     C                   eval      AR6cei = TASfei
180100000414     C                   eval      AR6fiv = TASfiv
180200000414     C                   eval      AR6nft = TASnft
180300000414     C                   eval      AR6dft = TASdft
180400020916
180500020916     C                   eval      AR6aas = TASaas
180600020916     C                   eval      AR6lnp = TASlnp
180700020916     C                   eval      AR6nrs = TASnrs
180800020916     C                   eval      AR6nsp = TASnsp
180900020916     C                   eval      AR6trc = '1'
181000020916     C                   eval      AR6ksc = TASksc
181100020916     C                   eval      AR6ctr = TASctr
181200020916
181300000414      *   calcolo l'importo
181400000426     C                   clear                   FNLV16DS
181500000414     c                   IF         §TBTPO = 'F'
181600000414     c                   eval      D17PRP = 'S'
181700000414     c                   ENDIF
181800000414     c                   eval      D17tla = 'L'
181900000414     c                   eval      D17tbo = '0'
182000000414     c                   eval      D17crb = 'S'
182100000414     c                   eval      D17aas = TASaas
182200000414     c                   eval      D17lns = TASlnp
182300000414     c                   eval      D17nrs = TASnrs
182400000414     c                   eval      D17nsp = TASnsp
182500000414     c                   eval      D17tbs = TAStbl
182600000414     c                   eval      D17fim = 'F'
182700000414     c                   CALL      'FNLV16R'
182800000414     c                   PARM                    FNLV16DS
182900000414     c                   PARM                    DTASV
183000000414      *
183100000414     C                   eval      AR6ift = O17ift
183200000414      *
183300000414     C                   write     FIAR6000
183400000414     C                   eval      WfileT = 'FIAR600T'
183500000414     C                   eval      WfileR = 'FIAR600R'
183600000414     C                   exsr      AGGnrec
183700000414      *
183800000414      * FIAR7
183900000414     c     KSPE          setll     TITA7000
184000000414     c     KSPE          reade     TITA7000
184100000414     c                   DOW       not %EOF
184200000414     c                   eval      AR7aas = TA7aas
184300000414     c                   eval      AR7lnp = TA7lnp
184400000414     c                   eval      AR7nrs = TA7nrs
184500000414     c                   eval      AR7nsp = TA7nsp
184600000414     c                   eval      AR7trc = '1'
184700000414     c                   eval      AR7svn = TA7svn
184800000414     c                   eval      AR7van = TA7van
184900000414     c                   write     FIAR7000
185000000414     C                   eval      WfileT = 'FIAR700T'
185100000414     C                   eval      WfileR = 'FIAR700R'
185200000414     C                   exsr      AGGnrec
185300000414      *
185400000414     c     KSPE          reade     TITA7000
185500000414     c                   ENDDO
185600000414      *
185700000414     c                   ENDSL
185800000414      *
185900000414     c                   ENDSR
186000000414      *****************************************************************
186100000414      *  Creo file fatturazione per seconda bolla
186200000414      *****************************************************************
186300000414     C     AGGfatt2      BEGSR
186400000414      *
186500000414     C                   clear                   DSTB
186600000414     C                   eval      XX = 1
186700000414     C                   eval      W008a = $TAStbl
186800000417     C     W008a         lookup    TblK(xx)                               31
186900000417     C                   movel     TblD(xx)      DSTB
187000000414      *
187100000414     C                   SELECT
187200000417      *
187300000417      * Non elaboro se non è prevista la 2a bolla
187400000417     c                   WHEN      §3Atb2 = *blanks
187500000417      *
187600000417      * Non elaboro se la 2a bolla è uno storno
187700000417     c                   WHEN      §TBfcb = '0'
187800000417      *
187900000414      * Non elaboro fatture non emesse dal P.O.
188000000417     c                   WHEN      $TASfiv = *zeros  or
188100000417     c                             $TASfiv = §QCfiv  or $TASfiv = §QCfi2  or
188200000417     c                             $TASfiv = §QCfi3  or $TASfiv = §QCfi4  or
188300000417     c                             $TASfiv = §QCfi5  or $TASfiv = §QCfi6
188400000417      *
188500000414      * Porto assegnato da trasmettere in partenza invio solo KSC e CTR
188600120607     c********           WHEN      §TBtpo = 'A' and Wtiporcd = 'P'
188700120607     C*******            eval      AR6aas = $TASaas
188800120607     C********           eval      AR6lnp = $TASlnp
188900120607     C*********          eval      AR6nrs = $TASnrs
189000120607     C*********          eval      AR6nsp = $TASnsp
189100120607     C**********         eval      AR6trc = '2'
189200120607     C**********         eval      AR6ksc = $TASksc
189300120607     C***********        eval      AR6ctr = $TASctr
189400000417      *
189500120607     C**********         write     FIAR6000
189600120607     C********           eval      WfileT = 'FIAR600T'
189700120607     C**********         eval      WfileR = 'FIAR600R'
189800120607     C**********         exsr      AGGnrec
189900000417      *
190000000414      * Porto assegnato da trasmettere in arrivo
190100120607     c*********          WHEN      §TBtpo = 'A' and Wtiporcd <> 'P'
190200120607     c* Sia per partenza che per arrivo trasmetto alla stessa maniera
190300120607     c                   WHEN      §TBtpo = 'A'
190400000414     C                   eval      AR6aas = $TASaas
190500000414     C                   eval      AR6lnp = $TASlnp
190600000414     C                   eval      AR6nrs = $TASnrs
190700000414     C                   eval      AR6nsp = $TASnsp
190800000414     C                   eval      AR6trc = '2'
190900000414     C                   eval      AR6ksc = $TASksc
191000000414     C                   eval      AR6ctr = $TASctr
191100000414     C                   eval      AR6div = $TASdiv
191200000414     C                   eval      AR6por = $TASpor
191300000414     C                   eval      AR6sv1 = $TASsv1
191400000414     C                   eval      AR6va1 = $TASva1
191500000414     C                   eval      AR6sv2 = $TASsv2
191600000414     C                   eval      AR6va2 = $TASva2
191700000414     C                   eval      AR6sv3 = $TASsv3
191800000414     C                   eval      AR6va3 = $TASva3
191900000414     C                   eval      AR6imv = $TASimv
192000120103     c                   if        $tasnft>0
192100120103     c                   if        $tasdft>§qtdsa
192200000414     C                   eval      AR6ali = §QTali
192300120103     c                   else
192400120103     C                   eval      AR6ali = §QTalo
192500120103     c                   endif
192600120103     c                   endif
192700000414     C                   eval      AR6cei = $TASfei
192800000414     C                   eval      AR6fiv = $TASfiv
192900000414     C                   eval      AR6nft = $TASnft
193000000414     C                   eval      AR6dft = $TASdft
193100000414      *   calcolo l'importo
193200000426     C                   clear                   FNLV16DS
193300000414     c                   IF        §TBTPO = 'F'
193400000414     c                   eval      D17PRP = 'S'
193500000414     c                   ENDIF
193600000414     c                   eval      D17tla = 'L'
193700000414     c                   eval      D17tbo = '0'
193800000414     c                   eval      D17crb = 'S'
193900000414     c                   eval      D17aas = $TASaas
194000000414     c                   eval      D17lns = $TASlnp
194100000414     c                   eval      D17nrs = $TASnrs
194200000414     c                   eval      D17nsp = $TASnsp
194300000414     c                   eval      D17tbs = $TAStbl
194400000414     c                   eval      D17fim = 'F'
194500000414     c                   CALL      'FNLV16R'
194600000414     c                   PARM                    FNLV16DS
194700000414     c                   PARM                    DTASV
194800000414     C                   eval      AR6ift = O17ift
194900000414      *
195000000414     C                   write     FIAR6000
195100000414     C                   eval      WfileT = 'FIAR600T'
195200000414     C                   eval      WfileR = 'FIAR600R'
195300000414     C                   exsr      AGGnrec
195400000414      *
195500000414     c     KSPE$         setll     TITA7000
195600000414     c     KSPE$         reade     TITA7000
195700000414     c                   DOW       not %EOF
195800000414     c                   eval      AR7aas = TA7aas
195900000414     c                   eval      AR7lnp = TA7lnp
196000000414     c                   eval      AR7nrs = TA7nrs
196100000414     c                   eval      AR7nsp = TA7nsp
196200000414     c                   eval      AR7trc = '2'
196300000414     c                   eval      AR7svn = TA7svn
196400000414     c                   eval      AR7van = TA7van
196500000414     C                   write     FIAR7000
196600000414     C                   eval      WfileT = 'FIAR700T'
196700000414     C                   eval      WfileR = 'FIAR700R'
196800000414     C                   exsr      AGGnrec
196900000414      *
197000000414     c     KSPE$         reade     TITA7000
197100000414     c                   ENDDO
197200000414      *
197300000414     c                   ENDSL
197400000414      *
197500000414     c                   endsr
197600030110      *---------------------------------------------------------------*
197700030110      *  Aggiorno file "estensione testata bolla" da trasmettere      *
197800030110      *---------------------------------------------------------------*
197900030110     c     srFIAR5       BEGSR
198000030110      *
198100030110      * Lettura file FIAR5
198200040531     c     kspe          setll     FIAR531C
198300040531     c     kspe          reade     FIAR531C
198400030110      *
198500040531do  1c                   DOW       not %EOF(FIAR531C)
198600030110      *
198700030110      * controllo il tipo record per verificare che sia da tresmettere
198800030110     c                   clear                   dAR5
198900030110     c                   eval      XX = 1
199000030110     c     AR5trd        lookup    AR5k(xx)                               31
199100030110      *
199200030110      * se il record è da trasmettere: scrivo FIAR500T
199300030110if  2c                   If        *in31
199400030110     c                   write     FIAR5T
199500030110     c                   eval      WfileT = 'FIAR500T'
199600030110     c                   eval      WfileR = 'FIAR500R'
199700030110     c                   exsr      AGGnrec
199800030110e   2c                   Endif
199900030110      *
200000040531     c     kspe          reade     FIAR531C
200100030110      *
200200030110e   1c                   ENDDO
200300030110      *
200400030110     c                   ENDSR
200500000404      *****************************************************************
200600000404      *  AGGANCIO ANAGRAFICHE
200700000404      *****************************************************************
200800000410     C     AGGnrec       BEGSR
200900000404      *
201000000410     c                   eval      xx = 1
201100000412     C     WfileT        lookup    filet(xx)                              31
201200000410      *
201300000410     C                   IF         *in31 = *on
201400000410     c                   add       1             rcdt(xx)
201500000410     c                   ELSE
201600000410     C     *blanks       lookup    filet(xx)                              31
201700000410     C                   IF         *in31 = *on
201800000412     c                   movel     WfileT        fileT(xx)
201900000412     c                   movel     WfileR        fileR(xx)
202000000412     c                   add       1             rcdT(xx)
202100000410     C                   ENDIF
202200000410     C                   ENDIF
202300000410      *
202400000410     c                   endsr
202500000410      *****************************************************************
202600000410      *  AGGANCIO ANAGRAFICHE
202700000410      *****************************************************************
202800000410     C     CTR_ANAGRA    BEGSR
202900000410      *
203000000404     C                   CALL      'TIBS69R'
203100000404     C                   PARM                    tibs69DS
203200000404     C                   PARM                    DS_cnaco
203300000404     C                   PARM                    DS_cnind
203400000404     C                   PARM                    DS_cnclp
203500000404     C                   PARM                    DS_fncls
203600000404      *
203700000404     C                   eval      *IN30 = (O69ERR =  '1')
203800000404      *
203900000404      * per il 1° giro è inizializzata nelle specifiche "D"
204000000404     C                   clear                   TIBS69DS
204100000404      *
204200000404     C                   ENDSR
204300000404      *--------------------------------------------------------------------------------------------*
204400000404      * Gestione file di trasmissione in partenza
204500000404      *--------------------------------------------------------------------------------------------*
204600000405     C     GESfiltrpa    BEGSR
204700000404      *
204800000404      * chiudo i files
204900000404     C                   exsr      closepfpar
205000000404      *
205100000413     c                   Do        Wnumele       XX
205200000404      *
205300000404      * controlla se esiste il membro
205400000404     C                   MOVEL(P)  cmpCHK(xx)    comman
205500000404     C                   CALL      'QCMDEXC'                            20
205600000404     C                   PARM                    comman
205700000404     C                   PARM                    lenght
205800000404      *
205900000404      * inesistente, aggiunge un membro nuovo
206000000404     C                   if        *in20
206100000404     C                   MOVEL(P)  cmpADD(xx)    comman
206200000404     C                   CALL      'QCMDEXC'
206300000404     C                   PARM                    comman
206400000404     C                   PARM                    lenght
206500000404     C                   endif
206600000404      *
206700000404      * in ogni caso, esegue l'ovrdbf
206800000404     C                   MOVEL(P)  cmpOVR(xx)    comman
206900000404     C                   CALL      'QCMDEXC'
207000000404     C                   PARM                    comman
207100000404     C                   PARM                    lenght
207200000404      *
207300000404      * alloco il file
207400000404     C                   MOVEL(P)  cmpALC(xx)    comman
207500000404     C                   CALL      'QCMDEXC'                            91
207600000404     C                   PARM                    comman
207700000404     C                   PARM                    lenght
207800000404      *
207900000404      * Se file già allocato disalloco quelli allocati e vado a fine pgm
208000000404     c                   If        *in91 =*on
208100000404      *
208200000404     c                   If        XX > 1
208300000404     c                   sub       1             XX
208400000404     c                   Do        XX            YY
208500000404     C                   MOVEL(P)  cmpDLC(xx)    comman
208600000404     C                   CALL      'QCMDEXC'                            20
208700000404     C                   PARM                    comman
208800000404     C                   PARM                    lenght
208900000404     c                   Enddo
209000000404     c                   Endif
209100000404      *
209200000412     c                   LEAVE
209300000404     c                   Endif
209400000404      *
209500000412     C                   ENDDO
209600000412      *
209700000404      * Apro i files
209800000412     c  N91              exsr      OPENpfpar
209900000404      *
210000000404     C     endtrpa       ENDSR
210100000404      *--------------------------------------------------------------------------------------------*
210200000404      * Gestione file di trasmissione in arrivo
210300000404      *--------------------------------------------------------------------------------------------*
210400000405     C     GESfiltrar    BEGSR
210500000404      *
210600000404      * chiudo i files
210700000404     C                   exsr      closepfarr
210800000404      *
210900000413     c                   Do        Wnumele       XX
211000000404      *
211100000404      * controlla se esiste il membro
211200000404     C                   MOVEL(P)  cmaCHK(xx)    comman
211300000404     C                   CALL      'QCMDEXC'                            20
211400000404     C                   PARM                    comman
211500000404     C                   PARM                    lenght
211600000404      *
211700000404      * inesistente, aggiunge un membro nuovo
211800000404     C                   if        *in20
211900000404     C                   MOVEL(P)  cmaADD(xx)    comman
212000000404     C                   CALL      'QCMDEXC'
212100000404     C                   PARM                    comman
212200000404     C                   PARM                    lenght
212300000404     C                   endif
212400000404      *
212500000404      * in ogni caso, esegue l'ovrdbf
212600000404     C                   MOVEL(P)  cmaOVR(xx)    comman
212700000404     C                   CALL      'QCMDEXC'
212800000404     C                   PARM                    comman
212900000404     C                   PARM                    lenght
213000000404      *
213100000404      * alloco il file
213200000404     C                   MOVEL(P)  cmaALC(xx)    comman
213300000404     C                   CALL      'QCMDEXC'                            91
213400000404     C                   PARM                    comman
213500000404     C                   PARM                    lenght
213600000404      *
213700000404      * Se file già allocato disalloco quelli allocati e vado a fine pgm
213800000404     c                   If        *in91 =*on
213900000404      *
214000000404     c                   If        XX > 1
214100000404     c                   sub       1             XX
214200000404     c                   Do        XX            YY
214300000404     C                   MOVEL(P)  cmaDLC(xx)    comman
214400000404     C                   CALL      'QCMDEXC'                            20
214500000404     C                   PARM                    comman
214600000404     C                   PARM                    lenght
214700000404     c                   Enddo
214800000404     c                   Endif
214900000404      *
215000000412     c                   LEAVE
215100000404     c                   Endif
215200000412      *
215300000412     C                   ENDDO
215400000404      *
215500000404      * Apro i files
215600000412     c  N91              exsr      openpfarr
215700000404      *
215800000404     C     endtrar       ENDSR
215900020304      *--------------------------------------------------------------------------------------------*
216000020925      * Gestione file di trasmissione msg e fivab
216100020304      *--------------------------------------------------------------------------------------------*
216200020304     c     gesfilvab     begsr
216300020304
216400020304      * chiudo il file
216500020304     c                   exsr      closepfvab
216600020304      *
216700020304
216800020304      * controlla se esiste il membro
216900020304     c                   movel(p)  cmdvab(1)     comman
217000020827     c                   eval      %subst(comman:56:6) = wmbrvab
217100020304     c                   call      'QCMDEXC'                            20
217200020304     c                   parm                    comman
217300020304     c                   parm                    lenght
217400020304
217500020304      * inesistente, aggiunge un membro nuovo
217600020304     c                   if        *in20
217700020304     c                   movel(p)  cmdvab(2)     comman
217800020827     c                   eval      %subst(comman:56:6) = wmbrvab
217900020304     c                   call      'QCMDEXC'
218000020304     c                   parm                    comman
218100020304     c                   parm                    lenght
218200020304     c                   endif
218300020304
218400020304      * in ogni caso, esegue l'ovrdbf
218500020304     c                   movel(p)  cmdvab(3)     comman
218600020827     c                   eval      %subst(comman:56:6) = wmbrvab
218700020304     c                   call      'QCMDEXC'
218800020304     c                   parm                    comman
218900020304     c                   parm                    lenght
219000020304
219100020304      * alloco il file
219200020304     c                   movel(p)  cmdvab(4)     comman
219300020827     c                   eval      %subst(comman:54:6) = wmbrvab
219400020304     c                   call      'QCMDEXC'                            91
219500020304     c                   parm                    comman
219600020304     c                   parm                    lenght
219700020304
219800020304      * Se file già allocato disalloco ed esco dal programma
219900020304     c                   if        *in91
220000020304     c                   movel(p)  cmdvab(5)     comman
220100020827     c                   eval      %subst(comman:54:6) = wmbrvab
220200020304     c                   call      'QCMDEXC'                            20
220300020304     c                   parm                    comman
220400020304     c                   parm                    lenght
220500020304     c                   endif
220600020304
220700020304      * Apro il file
220800020304     c  n91              exsr      openpfvab
220900020304
221000020304     c                   endsr
221100000404      *--------------------------------------------------------------------------------------------*
221200000404      * Chiudo file per partenza
221300000404      *--------------------------------------------------------------------------------------------*
221400000404     c     CLOSEpfpar    BEGSR
221500000404      *
221600000404     c                   close     fnblp00t
221700060221     c                   close     fiar400t
221800051116     c                   close     fIar900t
221900000404     c                   close     fnblt00t
222000000404     c                   close     fiar600t
222100000404     c                   close     fiar700t
222200040831     c****               close     fnevb00t
222300041122     c****               close     fnlbl00t
222400030110     c                   close     fiar500t
222500000404      *
222600000404     c                   ENDSR
222700000404      *--------------------------------------------------------------------------------------------*
222800000404      * apro file per partenza
222900000404      *--------------------------------------------------------------------------------------------*
223000000404     c     OPENpfpar     BEGSR
223100000404      *
223200000404     c                   open      fnblp00t
223300060221     c                   open      fiar400t
223400051116     c                   open      fIAR900t
223500000404     c                   open      fnblt00t
223600000404     c                   open      fiar600t
223700000404     c                   open      fiar700t
223800040831     c****               open      fnevb00t
223900041122     c****               open      fnlbl00t
224000030110     c                   open      fiar500t
224100000404      *
224200000404     c                   ENDSR
224300000404      *--------------------------------------------------------------------------------------------*
224400000404      * Chiudo file per arrivo
224500000404      *--------------------------------------------------------------------------------------------*
224600000404     c     CLOSEpfarr    BEGSR
224700000404      *
224800000404     c                   close     fnarb00t
224900060221     c                   close     fiar400t
225000051116     c                   close     fiar900t
225100000404     c                   close     fnart00t
225200000404     c                   close     fiar600t
225300000404     c                   close     fiar700t
225400040831     c*****              close     fnevb00t
225500041122     c*****              close     fnlbl00t
225600030110     c                   close     fiar500t
225700000404      *
225800000404     c                   ENDSR
225900000404      *--------------------------------------------------------------------------------------------*
226000000404      * apro file per arrivo
226100000404      *--------------------------------------------------------------------------------------------*
226200000404     c     OPENpfarr     BEGSR
226300000404      *
226400000404     c                   open      fnarb00t
226500060221     c                   open      fiar400t
226600051116     c                   open      fiar900t
226700000404     c                   open      fnart00t
226800000404     c                   open      fiar600t
226900000404     c                   open      fiar700t
227000040831     c****               open      fnevb00t
227100041122     c****               open      fnlbl00t
227200030110     c                   open      fiar500t
227300000404      *
227400000404     c                   ENDSR
227500020304      *--------------------------------------------------------------------------------------------*
227600020925      * Chiudo file fivab
227700020304      *--------------------------------------------------------------------------------------------*
227800020304     c     closepfvab    begsr
227900020304
228000020925     c                   close     fivabbft
228100020304
228200020304     c                   endsr
228300020304      *--------------------------------------------------------------------------------------------*
228400020925      * apro file fivab
228500020304      *--------------------------------------------------------------------------------------------*
228600020304     c     openpfvab     begsr
228700020304
228800020925     c                   open      fivabbft
228900020304
229000020304     c                   endsr
229100000323      *--------------------------------------------------------------------------------------------*
229200000323      * Imposto la librerie in tutte le schiere
229300000323      *--------------------------------------------------------------------------------------------*
229400000323     c     impolib       BEGSR
229500020301      *Se il nome della libreria è lungo 10 OK se lungo 9 mettiamo
229600020301      *il ' ' nella 1° posizione a sinis.
229700020301     c                   if        %subst(wlibfile: 10: 1) =' '
229800020301     c                   movel     wlibfile      com09             9
229900020301     c                   clear                   wlibfile
230000020301     c                   move      com09         wlibfile
230100020301     c                   end
230200000323     c                   eval      XX = 1
230300000413     c                   DO        Wnumele       xx
230400020211     c                   eval      %subst(cmpCHK(xx):15:10) = wlibfile
230500020211     c                   eval      %subst(cmpADD(xx):15:10) = wlibfile
230600020211     c                   eval      %subst(cmpOVR(xx):15:10) = wlibfile
230700020211     c                   eval      %subst(cmpALC(xx):15:10) = wlibfile
230800020211     c                   eval      %subst(cmpDLC(xx):15:10) = wlibfile
230900020211     c                   eval      %subst(cmaCHK(xx):15:10) = wlibfile
231000020211     c                   eval      %subst(cmaADD(xx):15:10) = wlibfile
231100020211     c                   eval      %subst(cmaOVR(xx):15:10) = wlibfile
231200020211     c                   eval      %subst(cmaALC(xx):15:10) = wlibfile
231300020211     c                   eval      %subst(cmaDLC(xx):15:10) = wlibfile
231400000412     c                   ENDDO
231500020304
231600020304     c                   DO        5             xx
231700020304     c                   eval      %subst(cmdvab(xx):15:10) = wlibfile
231800020304     c                   ENDDO
231900020304
232000000323      *
232100000323     C                   ENDSR
232200000328      *--------------------------------------------------------------------------------------------*
232300000328      * Operazioni finali
232400000328      *--------------------------------------------------------------------------------------------*
232500000412     C     Chiusure      BEGSR
232600000328      *
232700020827     C                   clear                   TIBS55DS
232800000328     C                   MOVEL     'C'           i50tla
232900020827     C                   CALL      'TIBS55R'
233000020827     C                   PARM                    TIBS55DS
233100000328      *
233200000328     C                   clear                   TIBS69DS
233300000328     C                   eval      I69TLA  = 'C'
233400000328     C                   CALL      'TIBS69R'
233500000328     C                   PARM                    TIBS69DS
233600000328      *
233700000328     C                   ENDSR
233800000322      *--------------------------------------------------------------------------------------------*
233900000407      * Routine iniziale
234000000322      *--------------------------------------------------------------------------------------------*
234100980828     C     *inzsr        BEGSR
234200000322      *
234300980828     C     *entry        PLIST
234400980828     C                   PARM                    kpjba
234500000322      *
234600000322     C                   Z-ADD     1             CODUT
234700980915     C                   CALL      'X§PARUT'
234800000412     C                   PARM                    UT§DSE0F
234900980915     C                   MOVEL     REC80         CNCR80
235000000330      * spedizione
235100000330     c     kspe          klist
235200000327     c                   kfld                    TASaas
235300000327     c                   kfld                    TASlnp
235400000327     c                   kfld                    TASnrs
235500000327     c                   kfld                    TASnsp
235600000410      * spedizione 2a bolla
235700000412     c     kspe$         klist
235800000410     c                   kfld                    $TASaas
235900000410     c                   kfld                    $TASlnp
236000000410     c                   kfld                    $TASnrs
236100000410     c                   kfld                    $TASnsp
236200000330      * Tita4
236300000330     c     kta4          klist
236400000330     c                   kfld                    TASaas
236500000330     c                   kfld                    TASlnp
236600000330     c                   kfld                    TASnrs
236700000330     c                   kfld                    TASnsp
236800000330     c                   kfld                    Ktrc
236900981008      * legame bolla spedizione
237000981008     c     klbl          klist
237100000322     c                   kfld                    rigaas
237200000322     c                   kfld                    riglnp
237300000322     c                   kfld                    rignrs
237400000322     c                   kfld                    rignsp
237500120530      *
237600120530     c     klbl2         klist
237700120530     c                   kfld                    WLaas
237800120530     c                   kfld                    WLlnp
237900120530     c                   kfld                    WLnrs
238000120530     c                   kfld                    WLnsp
238100000323      *
238200000323     c     klblA         klist
238300000323     c                   kfld                    WAaas
238400000323     c                   kfld                    WAlnp
238500000323     c                   kfld                    WAnrs
238600000323     c                   kfld                    WAnsp
238700000412      *
238800000419     c     KBOLw         klist
238900000412     c                   kfld                    Waas
239000000412     c                   kfld                    Wlnp
239100000412     c                   kfld                    Wnrs
239200000412     c                   kfld                    Wnsp
239300000412      *
239400000407     C     kTAB          KLIST
239500000407     C                   KFLD                    CODUT
239600000407     C                   KFLD                    KCOD
239700000322      *
239800000404     C     keytab        KLIST
239900000404     C                   KFLD                    CODUT
240000000404     C                   KFLD                    KCOD
240100000404     C                   KFLD                    KKEY
240200000322      *
240300000413      * Imposto numero elementi in schiera
240400000413     C                   eval      Wnumele = %elem(cmpchk)
240500000413      *
240600000322      * reperimento librerie job
240700020827     C                   CLEAR                   TIBS55DS
240800980828     C                   MOVEL     knsif         i50psi
240900020827     C                   CALL      'TIBS55R'
241000020827     C                   PARM                    TIBS55DS
241100000322      *
241200000323      * La libreria dei file di trasmissione è quella di gruppo, la imposto
241300000323      *  in tutte le schiere
241400981222     c                   movel     o50plg        wlibfile
241500000323     c                   exsr      impolib
241600000322      *
241700000322      * calcola la data corrente
241800000323     C                   TIME                    w0140                          *ora(6) + data(8)
241900000322     C                   MOVE      w0140         w0080                          *data(8) in g/m/aa
242000000322      * UDATE IN AAAAMMGG
242100000323     C     *eur          MOVEL     W0080         Wdata
242200000322     C     *iso          MOVEL     Wdata         datcor
242300000322      *
242400000407      * Carico codici bolla da convertire
242500000407     c                   clear                   xx
242600000407     C                   eval      kcod = '3A'
242700000407     c     KTAB          setll     tabel
242800000407     c     KTAB          reade     tabel
242900000407     C                   DOW       not %EOF
243000000412     c                   If        TBLflg = *blanks
243100000407     C                   add       1             xx
243200000417     c                   movel     TBlkey        T3aK(xx)
243300000417     c                   movel     TBLuni        T3aD(xx)
243400000407     c                   Endif
243500000412     c     KTAB          reade     tabel
243600000407     C                   ENDDO
243700000413      *
243800000413      * Carico tipi bolla
243900000413     c                   clear                   xx
244000000413     C                   eval      kcod = 'TB'
244100000413     c     KTAB          setll     tabel
244200000413     c     KTAB          reade     tabel
244300000413     C                   DOW       not %EOF
244400000413     c                   If        TBLflg = *blanks
244500000413     C                   add       1             xx
244600000417     c                   movel     TBlkey        TblK(xx)
244700000417     c                   movel     TBLuni        TblD(xx)
244800000413     c                   Endif
244900000413     c     KTAB          reade     tabel
245000000413     C                   ENDDO
245100000412      *
245200000412      * Aggancio tabella filiali IVA di sede
245300000412     C                   clear                   dsqc2
245400000412     C                   eval      KCOD = 'QC'
245500000412     C                   movel(P)  '2'           KKEY
245600000412     C     keytab        chain     TABEL
245700000412     c                   IF        %found
245800000412     C                   movel     TBLUNI        DSQC2
245900000412     c                   ENDIF
246000000412      *
246100000412      * Aggancio tabella campi di tassazione standard
246200000419     C                   clear                   dsqt1
246300000412     C                   eval      KCOD = 'QT'
246400000412     C                   movel(P)  '1'           KKEY
246500000412     C     keytab        chain     TABEL
246600000412     c                   IF        %found
246700000412     C                   movel     TBLUNI        DSQT1
246800000412     c                   ENDIF
246900030110      *
247000030110      * Carico codici Tipi Record di FIAR500F
247100030110      * (solo se prevedono la trasmissione in partenza e/o in arrivo)
247200030110     c                   clear                   xx
247300030110     c                   eval      TBEcod = 'AR5'
247400030110     c     TBEcod        setll     TNTBE000
247500030110     c     TBEcod        reade     TNTBE000
247600030110     c                   DOW       not %EOF(TNTBE01L)
247700030110     c                   If        TBEatb = *blanks
247800030110     c                   movel     TBEuni        dAR5
247900030110     c                   If        §AR5rigb = 'S'
248000030110     c                   add       1             xx
248100030110     c                   movel     TBEke1        AR5k(xx)
248200030110     c                   Endif
248300030110     c                   Endif
248400030110     c     TBEcod        reade     TNTBE000
248500030110     c                   ENDDO
248600000322      *
248700980828     C                   ENDSR
248800000323      *--------------------------------------------------------------------------------------------*
248900000324**   cmpCHK - comandi
249000020211CHKOBJ    OBJ(xxxxxxxxxx/FNBLP00T) OBJTYPE(*FILE) MBR(SEDE)                         1
249100060221CHKOBJ    OBJ(xxxxxxxxxx/FIAR400T) OBJTYPE(*FILE) MBR(SEDE)                         1
249200051116CHKOBJ    OBJ(xxxxxxxxxx/FIAR900T) OBJTYPE(*FILE) MBR(SEDE)                         1
249300020211CHKOBJ    OBJ(xxxxxxxxxx/FNBLT00T) OBJTYPE(*FILE) MBR(SEDE)                         1
249400030110CHKOBJ    OBJ(xxxxxxxxxx/FIAR500T) OBJTYPE(*FILE) MBR(SEDE)                         1
249500020211CHKOBJ    OBJ(xxxxxxxxxx/FIAR600T) OBJTYPE(*FILE) MBR(SEDE)                         1
249600020211CHKOBJ    OBJ(xxxxxxxxxx/FIAR700T) OBJTYPE(*FILE) MBR(SEDE)                         1
249700000324**   cmaCHK - comandi
249800120607CHKOBJ    OBJ(xxxxxxxxxx/FNARB00T) OBJTYPE(*FILE) MBR(SEDEA)                        1
249900120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR400T) OBJTYPE(*FILE) MBR(SEDEA)                        1
250000120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR900T) OBJTYPE(*FILE) MBR(SEDEA)                        1
250100120607CHKOBJ    OBJ(xxxxxxxxxx/FNART00T) OBJTYPE(*FILE) MBR(SEDEA)                        1
250200120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR500T) OBJTYPE(*FILE) MBR(SEDEA)                        1
250300120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR600T) OBJTYPE(*FILE) MBR(SEDEA)                        1
250400120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR700T) OBJTYPE(*FILE) MBR(SEDEA)                        1
250500000324**   cmpADD - comandi
250600020211ADDPFM   FILE(xxxxxxxxxx/FNBLP00T)                MBR(SEDE)                         2
250700060221ADDPFM   FILE(xxxxxxxxxx/FIAR400T)                MBR(SEDE)                         2
250800051116ADDPFM   FILE(xxxxxxxxxx/FIAR900T)                MBR(SEDE)                         2
250900020211ADDPFM   FILE(xxxxxxxxxx/FNBLT00T)                MBR(SEDE)                         2
251000030110ADDPFM   FILE(xxxxxxxxxx/FIAR500T)                MBR(SEDE)                         2
251100020211ADDPFM   FILE(xxxxxxxxxx/FIAR600T)                MBR(SEDE)                         2
251200020211ADDPFM   FILE(xxxxxxxxxx/FIAR700T)                MBR(SEDE)                         2
251300000324**   cmaADD - comandi
251400120607ADDPFM   FILE(xxxxxxxxxx/FNARB00T)                MBR(SEDEA)                        2
251500120607ADDPFM   FILE(xxxxxxxxxx/FIAR400T)                MBR(SEDEA)                        2
251600120607ADDPFM   FILE(xxxxxxxxxx/FIAR900T)                MBR(SEDEA)                        2
251700120607ADDPFM   FILE(xxxxxxxxxx/FNART00T)                MBR(SEDEA)                        2
251800120607ADDPFM   FILE(xxxxxxxxxx/FIAR500T)                MBR(SEDEA)                        2
251900120607ADDPFM   FILE(xxxxxxxxxx/FIAR600T)                MBR(SEDEA)                        2
252000120607ADDPFM   FILE(xxxxxxxxxx/FIAR700T)                MBR(SEDEA)                        2
252100000324**   cmpOVR - comandi
252200020211OVRDBF TOFILE(xxxxxxxxxx/FNBLP00T) FILE(FNBLP00T) MBR(SEDE)                         3
252300060221OVRDBF TOFILE(xxxxxxxxxx/FIAR400T) FILE(FIAR400T) MBR(SEDE)                         3
252400051116OVRDBF TOFILE(xxxxxxxxxx/FIAR900T) FILE(FIAR900T) MBR(SEDE)                         3
252500020211OVRDBF TOFILE(xxxxxxxxxx/FNBLT00T) FILE(FNBLT00T) MBR(SEDE)                         3
252600030110OVRDBF TOFILE(xxxxxxxxxx/FIAR500T) FILE(FIAR500T) MBR(SEDE)                         3
252700020211OVRDBF TOFILE(xxxxxxxxxx/FIAR600T) FILE(FIAR600T) MBR(SEDE)                         3
252800020211OVRDBF TOFILE(xxxxxxxxxx/FIAR700T) FILE(FIAR700T) MBR(SEDE)                         3
252900000324**   cmaOVR - comandi
253000120607OVRDBF TOFILE(xxxxxxxxxx/FNARB00T) FILE(FNARB00T) MBR(SEDEA)                        3
253100120607OVRDBF TOFILE(xxxxxxxxxx/FIAR400T) FILE(FIAR400T) MBR(SEDEA)                        3
253200120607OVRDBF TOFILE(xxxxxxxxxx/FIAR900T) FILE(FIAR900T) MBR(SEDEA)                        3
253300120607OVRDBF TOFILE(xxxxxxxxxx/FNART00T) FILE(FNART00T) MBR(SEDEA)                        3
253400120607OVRDBF TOFILE(xxxxxxxxxx/FIAR500T) FILE(FIAR500T) MBR(SEDEA)                        3
253500120607OVRDBF TOFILE(xxxxxxxxxx/FIAR600T) FILE(FIAR600T) MBR(SEDEA)                        3
253600120607OVRDBF TOFILE(xxxxxxxxxx/FIAR700T) FILE(FIAR700T) MBR(SEDEA)                        3
253700000324**   cmpALC - comandi
253800020211ALCOBJ   OBJ((xxxxxxxxxx/FNBLP00T  *FILE   *EXCL   (SEDE)))                         5
253900060221ALCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDE)))                         5
254000051116ALCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDE)))                         5
254100020211ALCOBJ   OBJ((xxxxxxxxxx/FNBLT00T  *FILE   *EXCL   (SEDE)))                         5
254200030110ALCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDE)))                         5
254300020211ALCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDE)))                         5
254400020211ALCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDE)))                         5
254500000324**   cmaALC - comandi
254600120607ALCOBJ   OBJ((xxxxxxxxxx/FNARB00T  *FILE   *EXCL   (SEDEA)))                        5
254700120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDEA)))                        5
254800120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDEA)))                        5
254900120607ALCOBJ   OBJ((xxxxxxxxxx/FNART00T  *FILE   *EXCL   (SEDEA)))                        5
255000120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDEA)))                        5
255100120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDEA)))                        5
255200120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDEA)))                        5
255300000324**   cmpDLC - comandi
255400020211DLCOBJ   OBJ((xxxxxxxxxx/FNBLP00T  *FILE   *EXCL   (SEDE)))                         6
255500060221DLCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDE)))                         6
255600051116DLCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDE)))                         6
255700020211DLCOBJ   OBJ((xxxxxxxxxx/FNBLT00T  *FILE   *EXCL   (SEDE)))                         6
255800030110DLCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDE)))                         6
255900020211DLCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDE)))                         6
256000020211DLCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDE)))                         6
256100000324**   cmaDLC - comandi
256200120607DLCOBJ   OBJ((xxxxxxxxxx/FNARB00T  *FILE   *EXCL   (SEDEA)))                        6
256300120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDEA)))                        6
256400120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDEA)))                        6
256500120607DLCOBJ   OBJ((xxxxxxxxxx/FNART00T  *FILE   *EXCL   (SEDEA)))                        6
256600120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDEA)))                        6
256700120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDEA)))                        6
256800120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDEA)))                        6
256900020304**   cmdvab - comandi
257000020925CHKOBJ    OBJ(xxxxxxxxxx/FIVABBFT) OBJTYPE(*FILE) MBR(Mxxxxxx)                      1
257100020925ADDPFM   FILE(xxxxxxxxxx/FIVABBFT)                MBR(Mxxxxxx)                      2
257200021009OVRDBF TOFILE(xxxxxxxxxx/FIVABBFT) FILE(FIVABBFT) MBR(Mxxxxxx)                      3
257300020925ALCOBJ   OBJ((xxxxxxxxxx/FIVABBFT  *FILE   *EXCL   (Mxxxxxx)))                      4
257400020925DLCOBJ   OBJ((xxxxxxxxxx/FIVABBFT  *FILE   *EXCL   (Mxxxxxx)))                      5

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
003600040831     F***FNEVB01L  IF   E           K DISK    rename(fnevb000:fnevb001)
003700041122     FFNLBL01L  IF   E           K DISK    rename(fnlbl000:fnlbl001)
003800041122     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl002)
003900000322     FTNCSB03L  IF   E           K DISK
004000000322     FAZORG01L  IF   E           K DISK
004100000404     FTABEL00F  IF   E           K DISK
004200030110     FTNTBE01L  IF   E           K DISK
004300060221      *
004400000412     FFNBLP00T  UF A E             DISK    usropn
004500000412     FFNBLT00T  UF A E             DISK    usropn
004600030110     F                                     prefix(ART:3)
004700000322      *
004800000412     FFNARB00T  UF A E             DISK    usropn
004900060221     FFIAR400T  UF A E             DISK    usropn
005000051116     FFiAR900T  UF A E             DISK    usropn
005100000412     FFNART00T  UF A E             DISK    usropn
005200000404      *
005300030110     FFIAR500T  UF A E             DISK    usropn
005400030110     F                                     rename(FIAR5000:FIAR5T)
005500000412     FFIAR600T  UF A E             DISK    usropn
005600000412     FFIAR700T  UF A E             DISK    usropn
005700040831     F***FNEVB00T  UF A E             DISK    usropn
005800041122     F***FNLBL00T  UF A E             DISK    usropn
005900020304
006000020925     ffivabbft  o    e             disk    usropn
006100020304
006200000322      *----------------
006300120607     D cmpCHK          S             60    DIM(7) CTDATA PERRCD(1)
006400120607     D cmaCHK          S             60    DIM(7) CTDATA PERRCD(1)
006500120607     D cmpADD          S             60    DIM(7) CTDATA PERRCD(1)
006600120607     D cmaADD          S             60    DIM(7) CTDATA PERRCD(1)
006700120607     D cmpOVR          S             60    DIM(7) CTDATA PERRCD(1)
006800120607     D cmaOVR          S             60    DIM(7) CTDATA PERRCD(1)
006900120607     D cmpALC          S             60    DIM(7) CTDATA PERRCD(1)
007000120607     D cmaALC          S             60    DIM(7) CTDATA PERRCD(1)
007100120607     D cmpDLC          S             60    DIM(7) CTDATA PERRCD(1)
007200120607     D cmaDLC          S             60    DIM(7) CTDATA PERRCD(1)
007300020304
007400020827     d cmdvab          s             62    dim(5) ctdata perrcd(1)
007500000327      *
007600000410     D filet           S             10    DIM(20)
007700000410     D rcdt            S              7  0 DIM(20)
007800000412     D filer           S             10    DIM(20)
007900000404      *
008000000417     D T3aK            S              2    DIM(50)
008100000417     D T3aD            S             89    DIM(50)
008200000417     D TblK            S              8    DIM(50)
008300000417     D TblD            S             89    DIM(50)
008400030110     d AR5k            s                   dim(50) like(AR5trd)
008500000322      *----------------
008600000327     D kpjba         E DS
008700000412     D DTASV         E DS                  prefix(D)
008800000412     D FNLV16DS      E DS
008900000327     D dscmz         E DS
009000020827     D TIBS55DS      E DS
009100980915     D cncr80        E DS
009200000412     D UT§DSE0F      E DS
009300000327     D FNLV55DS      E DS
009400020204     D DSBL4A        E DS
009500000327     D DSBL4E        E DS
009600020204     D DSBL49        E DS
009700000327     D DTA4A         E DS
009800000413     D DSTB          E DS
009900000413     D DS3A          E DS
010000100604     D DS7R          E DS
010100030110     d dAR5          e ds                  inz
010200000412     D DSQC2         E DS
010300000412     D DSQT1         E DS
010400000412     D DS_TITAS      E DS                  extname(TITAS00F)
010500000412     D DS_TITAS_2    E DS                  extname(TITAS00F) prefix($)
010600000412     D DS_TITAA      E DS                  extname(TITAA00F)
010700000412     D DS_TITAA_O    E DS                  extname(TITAA00F) prefix(O)
010800000412     D DS_TITAA_M    E DS                  extname(TITAA00F) prefix(M)
010900000328     d TIBS69DS      E DS                  INZ
011000000328     d DS_cnaco      E DS                  extname(CNACO00F)
011100000328     d DS_cnind      E DS                  extname(CNIND00F)
011200000328     d DS_cnclp      E DS                  extname(CNCLP00F)
011300000328     d DS_fncls      E DS                  extname(FNCLS00F)
011400020304
011500020304     d tnsym5cds     e ds
011600020304
011700000322      *----------------
011800020304     d keyorg          S                   LIKE(orgfil)
011900000330     d ktrc            S                   LIKE(TA4trc)
012000000404     d Wsegna          S                   LIKE(TASnca)
012100000404     D KCOD            S                   LIKE(TBLcod)
012200000404     D KKEY            S                   LIKE(TBLkey)
012300000412     D WAaas           S                   LIKE(TASaas)
012400000412     D Waas            S                   LIKE(TASaas)
012500000412     D Wlnp            S                   LIKE(TASlnp)
012600000412     D Wnrs            S                   LIKE(TASnrs)
012700000412     D Wnsp            S                   LIKE(TASnsp)
012800000412     D WAlnp           S                   LIKE(TASlnp)
012900000412     D WAnrs           S                   LIKE(TASnrs)
013000000412     D WAnsp           S                   LIKE(TASnsp)
013100000412     D WPaas           S                   LIKE(TASaas)
013200000412     D WPlnp           S                   LIKE(TASlnp)
013300000412     D WPnrs           S                   LIKE(TASnrs)
013400000412     D Wpnsp           S                   LIKE(TASnsp)
013401120530     D WLaas           S                   LIKE(TASaas)
013402120530     D WLlnp           S                   LIKE(TASlnp)
013403120530     D WLnrs           S                   LIKE(TASnrs)
013404120530     D WLnsp           S                   LIKE(TASnsp)
013500090423     D Wrigdue         S                   LIKE(RIGdela)  INZ
013600030508     D Wvabrmo         S                   LIKE(VABrmo)
013700030508     D Wvabrma         S                   LIKE(VABrma)
013800000322      *----------------
013900000322      * Variabili a programma
014000000322      *----------------
014100000413     D Wnumele         S              5P 0
014200000413     D WfileR          S             10
014300000412     D WfileT          S             10
014400020211     D Wlibfile        S             10
014500000412     D Wassegno        S              1
014600000419     D Wfirig          S              1
014700000412     D Wtipodata       S              1
014800000412     D Wtiporcd        S              1
014900000412     D Wazione         S              2  0
015000000412     D xx              S              2  0
015100000327     D yy              S              2  0
015200000327     D lenght          S             15  5 INZ(80)
015300000327     D comman          S             80
015400000322     D Wdata           S               D   DATFMT(*eur)
015500000327     D w0140           S             14  0
015600000327     D w0080           S              8  0
015700000412     D w0030           S              3  0
015800000327     D datcor          S              8  0
015900000412     d Wlegame         s              1
016000000413     D W008a           S              8
016100020304
016200020304     d rcdvab          s              7  0
016300020827     d wmbrvab         s              6
016400020827     d wmbrt           s             10
016500020304
016600020304      * work dati mamma originale
016700020304     d wpaasv          s              4
016800020304     d wplnpv          s              3
016900020304     d wpnrsv          s              2
017000020304     d wpnspv          s              7
017100020304     d wpkscv          s              7
017200020304     d wplnav          s              3
017300020305     d wpragv          s             35
017400020304     d wpdspv          s              8
017500020304      * work dati ultima figlia
017600020304     d waaasv          s              4
017700020304     d walnpv          s              3
017800020304     d wanrsv          s              2
017900020304     d wanspv          s              7
018000020304     d walnav          s              3
018100020304     d wapdcv          s              7
018200020304     d wandcv          s              7
018300020304     d waddcv          s              8
018400020304     d wadspv          s              8
018500020304
018600020304     d wrigpor         s              3
018700020304
018800020304     d wdatav          ds
018900020304     d  wgg                    1      2
019000020304     d  wmm                    3      4
019100020304     d  waa                    5      8
019200020304
019300020304     d wnumbolla       ds
019400020304     d  wvabaas                1      2
019500020304     d  wvablnp                3      5
019600020304     d  wvabnrs                6      7
019700020304     d  wvabnsp                8     14
019800020304
019900020304     D WLBDAT          DS                  INZ
020000020304     D  G02DAT                 1      8  0
020100020304     D  G02INV                 9     16  0
020200020304     D  G02ERR                17     17
020300020304     D  G02TGI                18     22  0
020400020304
020500020304      * Costanti per messaggi si P.E.I.
020600020408     d pe_testa1       c                   'Conferma - Richiesta Bolla Firmata'
020700020408     d pe_testa2       c                   'Ricerca - Bolla Firmata'
020800020304     d pe_riga1        c                   'Il p.o. xxx richiede l''invio della-
020900020305     d                                      lettera di vettura firmata'
021000020305     d pe_riga2        c                   'relativa alla spedizione n. xxx-xx--
021100020305     d                                     xxxxxxx del xx/xx/xxxx'
021200020304     d pe_riga3        c                   'cliente xxxxxxx xxxxxxxxxxxxxxxxxxx-
021300020304     d                                     xxxxxxxxxxxxxxxx'
021400020305     d pe_riga4        c                   'spedizione consegnata n. xxx-xx-xxx-
021500020305     d                                     xxxx del xx/xx/xxxx'
021600020305     d pe_riga5        c                   'legata a quella richiesta'
021700020305     d pe_riga6        c                   'distinta di consegna n. xxxxxxx del-
021800020304     d                                      xx/xx/xxxx autotrasp. xxxxxxx'
021900980928      * ---------------------------------------------------------
022000980928     IFNBLP000
022100980928     I              BLPATB                      ARBATB
022200000324     I              BLPFLB                      ARBFLB
022300000324     I              BLPDIM                      ARBDIM
022400000324     I              BLPAAS                      ARBAAS
022500980928     I              BLPLNP                      ARBLNP
022600980928     I              BLPNRS                      ARBNRS
022700980928     I              BLPNSP                      ARBNSP
022800980928     I              BLPCBO                      ARBCBO
022900980928     I              BLPMGS                      ARBMGS
023000980928     I              BLPFNS                      ARBFNS
023100980928     I              BLPSCL                      ARBSCL
023200980928     I              BLPFBR                      ARBFBR
023300000324     I              BLPFST                      ARBFST
023400980928     I              BLPFDN                      ARBFDN
023500980928     I              BLPLNA                      ARBLNA
023600980928     I              BLPZNC                      ARBZNC
023700980928     I              BLPKSC                      ARBKSC
023800980928     I              BLPRSM                      ARBRSM
023900980928     I              BLPINM                      ARBINM
024000980928     I              BLPCAM                      ARBCAM
024100980928     I              BLPLOM                      ARBLOM
024200980928     I              BLPNZM                      ARBNZM
024300980928     I              BLPPRM                      ARBPRM
024400980928     I              BLPFAP                      ARBFAP
024500980928     I              BLPCPI                      ARBCPI
024600980928     I              BLPRSD                      ARBRSD
024700980928     I              BLPIND                      ARBIND
024800980928     I              BLPCAD                      ARBCAD
024900980928     I              BLPLOD                      ARBLOD
025000980928     I              BLPNZD                      ARBNZD
025100980928     I              BLPPRD                      ARBPRD
025200980928     I              BLPFIN                      ARBFIN
025300980928     I              BLPGC1                      ARBGC1
025400980928     I              BLPGC2                      ARBGC2
025500980928     I              BLPCTR                      ARBCTR
025600980928     I              BLPCTS                      ARBCTS
025700980928     I              BLPFTM                      ARBFTM
025800980928     I              BLPTSP                      ARBTSP
025900980928     I              BLPNAS                      ARBNAS
026000980928     I              BLPNCL                      ARBNCL
026100980928     I              BLPNCP                      ARBNCP
026200980928     I              BLPPKC                      ARBPKC
026300980928     I              BLPPKB                      ARBPKB
026400980928     I              BLPPKF                      ARBPKF
026500980928     I              BLPNCR                      ARBNCR
026600980928     I              BLPVLC                      ARBVLC
026700980928     I              BLPFVB                      ARBFVB
026800980928     I              BLPVLB                      ARBVLB
026900980928     I              BLPFVF                      ARBFVF
027000980928     I              BLPVLF                      ARBVLF
027100980928     I              BLPQFT                      ARBQFT
027200980928     I              BLPVMD                      ARBVMD
027300980928     I              BLPVAD                      ARBVAD
027400980928     I              BLPIAS                      ARBIAS
027500980928     I              BLPVAS                      ARBVAS
027600980928     I              BLPCCM                      ARBCCM
027700980928     I              BLPRMN                      ARBRMN
027800980928     I              BLPRMA                      ARBRMA
027900980928     I              BLPRMO                      ARBRMO
028000980928     I              BLPFLS                      ARBFLS
028100980928     I              BLPNCD                      ARBNCD
028200980928     I              BLPNCA                      ARBNCA
028300980928     I              BLPXCO                      ARBXCO
028400980928     I              BLPCTM                      ARBCTM
028500980928     I              BLPFFD                      ARBFFD
028600000324     I              BLPDRT                      ARBDRT
028700000324     I              BLPNRT                      ARBNRT
028800000324     I              BLPFPP                      ARBFPP
028900000324     I              BLPPDR                      ARBPDR
029000000324     I              BLPDSE                      ARBDSE
029100980928     I              BLPTCR                      ARBTCR
029200980928     I              BLPDCR                      ARBDCR
029300980928     I              BLPHCR                      ARBHCR
029400980928     I              BLPDTI                      ARBDTI
029500980928     I              BLPHTI                      ARBHTI
029600980928     I              BLPDCE                      ARBDCE
029700980928     I              BLPNPI                      ARBNPI
029800980928     I              BLPNCI                      ARBNCI
029900980928     I              BLPNRC                      ARBNRC
030000980928     I              BLPDBR                      ARBDBR
030100980928     I              BLPNFV                      ARBNFV
030200980928     I              BLPDPC                      ARBDPC
030300980928     I              BLPDUC                      ARBDUC
030400980928     I              BLPFLP                      ARBFLP
030500980928     I              BLPDET                      ARBDET
030600980928     I              BLPDUT                      ARBDUT
030700980928     I              BLPDAM                      ARBDAM
030800980928     I              BLPNDC                      ARBNDC
030900980928     I              BLPDDC                      ARBDDC
031000980928     I              BLPDCP                      ARBDCP
031100980928     I              BLPDCM                      ARBDCM
031200980928     I              BLPHMC                      ARBHMC
031300980928     I              BLPTC1                      ARBTC1
031400980928     I              BLPTC2                      ARBTC2
031500980928     I              BLPCCA                      ARBCCA
031600980928     I              BLPTMC                      ARBTMC
031700980928     I              BLPGGA                      ARBGGA
031800980928     I              BLPGMA                      ARBGMA
031900980928     I              BLPGVA                      ARBGVA
032000980928     I              BLPFLE                      ARBFLE
032100980928     I              BLPTFP                      ARBTFP
032200980928     I              BLPFEA                      ARBFEA
032300980928     I              BLPTFA                      ARBTFA
032400980928     I              BLPFT1                      ARBFT1
032500980928     I              BLPDT1                      ARBDT1
032600980928     I              BLPFT2                      ARBFT2
032700980928     I              BLPDT2                      ARBDT2
032800980928     I              BLPFT3                      ARBFT3
032900980928     I              BLPDT3                      ARBDT3
033000030110
033100000322      *--------------------------------------------------------------------------------------------*
033200000322      * Main lines
033300000322      *--------------------------------------------------------------------------------------------*
033400090423     C     Wrigdue       setll     FnRIG000
033500090423     C     Wrigdue       READE     FnRIG000
033600000419      *
033700000419      * Elaboro il file richiesto fino alla fine se i file di trasmissione
033800000419      * sono allocabili
033900090423     C                   DOW       not %EOF(fnrig01L)  and  *IN91 = *off
034000040507
034100090423      * Elaboro solo i record 'F' 'C' 'D'
034200090423if  2c                   If        RigCau = 'F' or
034300090423     c                             RigCau = 'C' or RigCau = 'D'
034400000418      *
034500000419     c                   clear                   Wfirig
034600000419     c                   clear                   ds_titas
034700000412     c                   clear                   ds_titas_2
034800000327      *
034900000419      * aggancio TITAS, se non lo trovo accendo *in92                        interessato
035000000419     c                   eval      Waas = RIGaas
035100000419     c                   eval      Wlnp = RIGlnp
035200000419     c                   eval      Wnrs = RIGnrs
035300000419     c                   eval      Wnsp = RIGnsp
035400000419     C                   eval      *in92 = *off
035500000419     C                   exsr      AGGTAS
035600000419      *
035700000419     c                   IF        *in92 = *off
035800000322      * legge il file fisico e scrive il membro di trasmissione
035900000322     C                   EXSR      letscr
036000000418     C                   ENDIF
036100040507
036200040507e   2c                   EndIf
036300000418      *
036400090423     C     Wrigdue       READE     FnRIG000
036500000418     C                   ENDDO
036600000322      *
036700000418     C                   Exsr      chiusure
036800000418     C                   eval      *inlr = *on
036900000322      *--------------------------------------------------------------------------------------------*
037000000419      * Aggancio file TITAS se cambiato x legami
037100000322      *--------------------------------------------------------------------------------------------*
037200000419     C     AGGTAS        BEGSR
037300000322      *
037400000419      * verifico l'esistenza della spedizione
037500000419     c     KBOLw         chain     TITAS30c
037600000419      *
037700000419     C                   IF        NOT %FOUND(titas30c)
037800000419     C                   eval      *in92 = *on
037900000419     c                   ELSE
038000000419      *
038100000419      *   se tipo bolla non è valido cerco altro rcd
038200000419     C                   clear                   DSTB
038300000419     C                   eval      XX = 1
038400000419     C                   eval      W008a = TAStbl
038500000419     C     W008a         lookup    TblK(xx)                               31
038600000419     C                   movel     TblD(xx)      DSTB
038700000419     c                   if        §TBrbl = 'R'
038800000419     c                   movel     ds_titas      ds_titas_2
038900000419     c                   clear                   ds_titas
039000000509     c     KBOLw         reade     TITAS30c                               92
039100000419     c                   endif
039200000419      *
039300000419     C                   ENDIF
039400000419      *
039500000419     C                   ENDSR
039600000419      *--------------------------------------------------------------------------------------------*
039700000419      * letscr - legge il file fisico e scrive il membro di trasmissione
039800000419      *--------------------------------------------------------------------------------------------*
039900000419     C     letscr        BEGSR
040000000419      *
040100000412     c                   clear                   Wlegame
040200000322      *
040201120601      * BOLLE LEGATE:
040202120601      * se rigenerazione per bolla firmata trasmetto:
040300120601      *                                       in partenza la Mamma
040400000322      *                                       in arrivo l'Ultima Figlia
040401120601      * In tutti gli altri casi trasmetto tutte le bolle sia a partenza che ad
040402120601      * arrivo
040500000418     c                   exsr      legbol
040600000322      *
040601120605     c                   select
040700000322      * Imposto key sped se non ho bolla legata
040800120605    3c                   when      Wlegame = *blanks                            * bolla di reso
040900000322     c                   z-add     rigaas        WPaas
041000000322     c                   z-add     riglnp        WPlnp
041100000322     c                   z-add     rignrs        WPnrs
041200000322     c                   z-add     rignsp        WPnsp
041300000322     c                   z-add     rigaas        WAaas
041400000322     c                   z-add     riglnp        WAlnp
041500000322     c                   z-add     rignrs        WAnrs
041600000322     c                   z-add     rignsp        WAnsp
041601120605     c                   exsr      elatra
041602120605     g* Rigenerazione per bolla firmata
041603120605     c                   when      rigcau='F'
041604120605     c                   exsr      elatra
041605120605     c                   other
041606120601     c* se presente legame bolla devo trasmettere tutte le bolle legate ad eccezione
041607120601     c* della rigenerazione per bolle firmate che in questo caso serve solo la mamma e l'ultima
041608120601     c* figlia
041609120530     c                   eval      wlaas=lblaao
041610120530     c                   eval      wllnp=LBLLPO
041611120530     c                   eval      wlnrs=LBLNRO
041612120530     c                   eval      wlnsp=LBLNSO
041613120530     c     klbl2         setll     fnlbl02l
041614120530     c     klbl2         reade     fnlbl02l
041615120530     c                   z-add     lblAAO        WPaas
041616120530     c                   z-add     lblLPO        WPlnp
041617120530     c                   z-add     lblNRO        WPnrs
041618120530     c                   z-add     lblNSO        WPnsp
041619120530     c                   z-add     lblAAO        WAaas
041620120530     c                   z-add     lblLPO        WAlnp
041621120530     c                   z-add     lblNRO        WAnrs
041622120530     c                   z-add     lblNSO        WAnsp
041623120530     c                   exsr      ELATRA
041713120605     c                   dow       not %eof(fnlbl02l)
041717120530     c                   z-add     lblAAN        WPaas
041718120530     c                   z-add     lblLPN        WPlnp
041719120530     c                   z-add     lblNRN        WPnrs
041720120530     c                   z-add     lblNSN        WPnsp
041721120530     c                   z-add     lblAAN        WAaas
041722120530     c                   z-add     lblLPN        WAlnp
041723120530     c                   z-add     lblNRN        WAnrs
041724120530     c                   z-add     lblNSN        WAnsp
041800000322      *
041801120530     C                   EXSR      ELATRA
043700020304
043702120601     c                   eval      Wlaas=LBLAAN
043703120601     c                   eval      Wllnp=LBLLPN
043704120601     c                   eval      Wlnrs=LBLNRN
043705120601     c                   eval      Wlnsp=LBLNSN
043706120605     c     klbl2         setll     fnlbl02l
043707120605     c     klbl2         reade     fnlbl02l
043711120530     c                   enddo
043712120605     c                   endsl
043713120530     c*
043800000323      *
043900000323     c                   ENDSR
043901120530      *--------------------------------------------------------------------------------------------*
043902120530      * Elaboro per trasmissione                                            to
043903120530      *--------------------------------------------------------------------------------------------*
043904120530     C     ELATRA        BEGSR
043905120530      * Elaboro per trasmissione alla partenza, all'arrivo o entrambe
043906120530      * solo se la causale è Danni o C/A Omessi
043907120530      * Se la causale è F= Bolle Firmate devo trasmettere
043908120530      * 1 msg di PEI alla Partenza e 1 all'Arrivo e scrivere
043909120530      * il file FIVABBFT da inviare all'arrivo
043910120530     C                   SELECT
043911120530     c                   WHEN      rigCAU = 'F'
043912120530     c                   exsr      trasvab
043913120530     C                   WHEN      rigTRA = 'P'
043914120530     c                   exsr      traspar
043915120530     C                   WHEN      rigTRA = 'A'
043916120530     c                   exsr      trasarr
043917120530     C                   WHEN      rigTRA = 'E'
043918120530     c                   exsr      traspar
043919120530     c                   If        *in91 = *off  and  *in92 = *off
043920120530     c                   exsr      trasarr
043921120530     c                   Endif
043922120530     c                   ENDSL
043923120530     c                   endsr
044000000323      *--------------------------------------------------------------------------------------------*
044100000404      * Trasmissione in partenza                                            to
044200000323      *--------------------------------------------------------------------------------------------*
044300000323     C     TRASPAR       BEGSR
044400000323      *
044500000404     C                   clear                   filet
044600000404     C                   clear                   rcdt
044700000404      *
044800000405      * Gestione file per trasmissione in partenza
044900000405     c                   exsr      GESfiltrpa
045000000323      *
045100000322      * se 91 acceso vuol dire che non e' stato possibile allocare il membro interessato
045200000324     c                   IF        *in91 = *off
045300000324      *
045400000419      * Verifico se devo riagganciare TITAS
045500000419     C                   IF        WPaas <> TASaas  or  WPlnp <> TASlnp  or
045600000419     C                             WPnrs <> TASnrs  or  WPnsp <> TASnsp
045700000419     c                   clear                   ds_titas
045800000419     c                   clear                   ds_titas_2
045900000419     c                   eval      Waas = WPaas
046000000419     c                   eval      Wlnp = WPlnp
046100000419     c                   eval      Wnrs = WPnrs
046200000419     c                   eval      Wnsp = WPnsp
046300000419     c                   eval      *in92 = *off
046400000419      * se non trovo TITAS accendo *in92                                     interessato
046500000419     C                   exsr      AGGTAS
046600000419     C                   Endif
046700000419      *
046800000419     c                   IF        *in92 = *off
046900000419      *
047000000419      * Scrivo i rcd da trasmettere
047100000328     c                   eval      Wtiporcd = 'P'
047200000328     c                   exsr      wrtrcd
047300000322      *
047400000322      * chiude il membro di trasmissione
047500000404     C                   exsr      closepfpar
047600000404      *
047700000404      * TRASMETTO
047800000404     C                   exsr      trasmi
047900000324      *
048000000324      * Aggiorno i record del fisico trasmessi
048100000419     C                   IF        Wfirig <> 'S'
048200090423     C                   z-add     datcor        RIGdela                        *data trasmissione
048300090423     C                   UPDATE    FnRIG000
048400000419     c                   eval      Wfirig = 'S'
048500000419     c                   ENDIF
048600000404      *
048700000404     c                   ENDIF
048800000419     c                   ENDIF
048900000324      *
049000000322     C                   ENDSR
049100000405      *--------------------------------------------------------------------------------------------*
049200000405      * Trasmissione in arrivo                                              to
049300000405      *--------------------------------------------------------------------------------------------*
049400000405     C     TRASARR       BEGSR
049500000405      *
049600000405     C                   clear                   filet
049700000405     C                   clear                   rcdt
049800000405      *
049900000405      * Gestione file per trasmissione in arrivo
050000000405     c                   exsr      GESfiltrar
050100000405      *
050200000405      * se 91 acceso vuol dire che non e' stato possibile allocare il membro interessato
050300000405     c                   IF        *in91 = *off
050400000419      *
050500000419      * Verifico se devo riagganciare TITAS
050600000419     C                   IF        WAaas <> TASaas  or  WAlnp <> TASlnp  or
050700000419     C                             WAnrs <> TASnrs  or  WAnsp <> TASnsp
050800000419     c                   clear                   ds_titas
050900000419     c                   clear                   ds_titas_2
051000000419     c                   eval      Waas = WAaas
051100000419     c                   eval      Wlnp = WAlnp
051200000419     c                   eval      Wnrs = WAnrs
051300000419     c                   eval      Wnsp = WAnsp
051400000419     c                   eval      *in92 = *off
051500000419      * se non trovo TITAS accendo *in92                                     interessato
051600000419     C                   exsr      AGGTAS
051700000509      * se 92 acceso vuol dire che non ha trovato il record giusto
051800000509      * o la bolla legata è una bolla di recupero da non trasmettere
051900000509     c                   if        *in92
052000000509     c                   clear                   ds_titas
052100000509     c                   clear                   ds_titas_2
052200000509     c                   eval      Waas = WPaas
052300000509     c                   eval      Wlnp = WPlnp
052400000509     c                   eval      Wnrs = WPnrs
052500000509     c                   eval      Wnsp = WPnsp
052600000509     c                   eval      *in92 = *off
052700000509      * se non trovo TITAS accendo *in92                                     interessato
052800000509     C                   exsr      AGGTAS
052900000509     c                   Endif
053000000509      *
053100000419     C                   Endif
053200000419      *
053300000419     c                   IF        *in92 = *off
053400000405      *
053500000405      * Scrivo i rcd da trasmettere
053600000405     c                   eval      Wtiporcd = 'A'
053700000405     c                   exsr      wrtrcd
053800000405      *
053900000405      * chiude il membro di trasmissione
054000000405     C                   exsr      closepfarr
054100000405      *
054200000405      * TRASMETTO
054300000405     C                   exsr      trasmi
054400000419      *
054500000419      * Aggiorno i record del fisico trasmessi
054600000419     C                   IF        Wfirig <> 'S'
054700090423     C                   z-add     datcor        RIGdela                        *data trasmissione
054800090423     C                   UPDATE    FnRIG000
054900000419     c                   eval      Wfirig = 'S'
055000000419     c                   ENDIF
055100000405      *
055200000405     c                   ENDIF
055300000419     c                   ENDIF
055400000405      *
055500000405     C                   ENDSR
055600020304      *--------------------------------------------------------------------------------------------*
055700020925      * Trasmissione msg di P.E.I. e FIVABBFT
055800020304      *--------------------------------------------------------------------------------------------*
055900020304     c     trasvab       begsr
056000020304
056100020925      * recupero dati dal titas da scrivere nei messaggi di P.E.I. e nel FIVAB
056200020304     c                   exsr      recdati
056300020304
056400020304     c                   clear                   rcdvab
056500020827     c                   Movel     '046'         Wmbrvab
056600020304     c                   move      walnav        wmbrvab
056700020304
056800020925      * Gestione file per trasmissione msg e fivab
056900020304     c                   exsr      gesfilvab
057000020304
057100020304      * se 91 spento vuol dire che e' stato possibile allocare il membro interessato
057200020304     c                   IF        *in91 = *off
057300020304
057400020304      * Invio i messaggi di P.E.I.
057500020304     c                   exsr      crtposta
057600020304
057700020304      * Scrivo il rcd da trasmettere
057800020304     c                   exsr      wrtvab
057900020304
058000020304      * chiude il membro di trasmissione
058100020304     c                   exsr      closepfvab
058200020304
058300020304      * Trasmetto
058400020304     c                   exsr      trasmivab
058500020304
058600020304      * Aggiorno i record del fisico trasmessi
058700020304     c                   if        wfirig <> 'S'
058800090423     c                   z-add     datcor        rigdela                        *data trasmissione
058900090423     c                   update    fnrig000
059000020304     c                   eval      wfirig = 'S'
059100020304     c                   endif
059200020304
059300020304     c                   endif
059400020304
059500020304     c                   endsr
059600020304      *
059700000322      *--------------------------------------------------------------------------------------------*
059800000405      * Controllo se la sped. da rigenerare ha dei legami bolla
059900000322      *--------------------------------------------------------------------------------------------*
060000981008     C     legbol        BEGSR
060100000418      *
060200000418      * Non considero i resi come bolle legate
060300020304      * se causale richiesta è DANNI
060400120530    3c****               IF        TASfbr = *blanks  or                         * bolla di reso
060500120530    3c****                         (TASfbr = 'S' and rigcau <> 'D')
060600000418      *
060700000418     c                   eval      *in81 = *on
060800000418     c                   eval      *in82 = *on
060900000509     c                   eval      *in32 = *off
061000000418      *
061100000418      * controllo se c'è un legame bolla
061200000418     c     klbl          chain     fnlbl01l                           82
061300000418     c   82klbl          chain     fnlbl02l                           81
061400000418      *
061500000418      * n81 oppure n82  c'è legame bolla
061600000418    2c                   if        *in81 = *off  or  *in82 = *off
061700000418      *
061800000418     c                   eval      Wlegame = 'S'
061900000418      *
062000000418      * DEFINISCO LE SPEDIZIONE DA TRASMETTERE
062100000418      *
062101120601   a c                   if        rigcau = 'F'
062200000418      * Mamma originale - PARTENZA
062300000418     c                   eval      WPaas = lblaao
062400000418     c                   eval      WPlnp = lbllpo
062500000418     c                   eval      WPnrs = lblnro
062600000418     c                   eval      WPnsp = lblnso
062700000418      *
062800000418      * Ultima figlia - ARRIVO
062900000509    4c                   DOW       not *in32
063000000418     c                   eval      WAaas = lblaan
063100000418     c                   eval      WAlnp = lbllpn
063200000418     c                   eval      WAnrs = lblnrn
063300000418     c                   eval      WAnsp = lblnsn
063400000509     c     klblA         chain     fnlbl02l                           32
063500000418    4c                   ENDDO
063600020701
063700020701      * se richiesta bolla firmata controllo l'ultima figlia
063800120601   a c***                if        rigcau = 'F'
063900020701     c                   eval      waas = waaas
064000020701     c                   eval      wlnp = walnp
064100020701     c                   eval      wnrs = wanrs
064200020701     c                   eval      wnsp = wansp
064300020701     c     kbolw         chain     titas30c
064400020701   b c                   if        %found(titas30c)
064500020701     c                   clear                   Dstb
064600020701     c                   eval      W008a = TAStbl
064700020701     c                   eval      XX = 1
064800020701     c     W008a         lookup    TblK(xx)                               31
064900020701     c                   movel     TblD(xx)      Dstb
065000020701      * se bolla recupero prendo la penultima figlia
065100020701   c c                   if        §TBrbl = 'R'
065200020701     c                   eval      WAaas = lblaap
065300020701     c                   eval      WAlnp = lbllpp
065400020701     c                   eval      WAnrs = lblnrp
065500020701     c                   eval      WAnsp = lblnsp
065600020701   c c                   endif
065700020701   b c                   endif
065800020701   a c                   endif
065900000418      *
066000000418    1c                   endif
066100000418      *
066200120530     C*****              ENDIF
066300000322      *
066400000323     c                   ENDSR
066500000405      *--------------------------------------------------------------------------------------------*
066600000405      * trasmi - trasmette il membro di trasmissione
066700000405      *--------------------------------------------------------------------------------------------*
066800980828     C     trasmi        BEGSR
066900000405      *
066901120607     c                   if        Wtiporcd='P'
067000000405     C                   MOVEL(P)  'SEDE'        cmzmbd                          *dal membro
067100000405     C                   MOVEL(P)  'SEDE'        cmzmba                          *al membro
067101120607     c                   else
067102120607     C                   MOVEL(P)  'SEDEA'       cmzmbd                          *dal membro
067103120607     C                   MOVEL(P)  'SEDEA'       cmzmba                          *al membro
067104120607     c                   endif
067200000405     C                   CLEAR                   cmzprr                          *prerequisito
067300981222     C                   MOVE      '0000'        cmzori                          *ora invio
067400981222     C                   MOVE      '0000000'     cmzdti                          *data invio
067500990208     C                   MOVEL     'N'           cmzurg                          *urgenza notte
067600981222     C                   MOVEL     'A'           cmzadd                          *record in aggiunta
067700981222     C                   MOVEL     'S'           cmzclr                          *no pulizia file
067800981222     C                   MOVEL     'S'           cmzftr                          *invio da transito
067900000405     C                   CLEAR                   cmzerr                          *flag errore('1')
068000000509     C                   CLEAR                   cmzazn                          *azione
068100000410      *
068200000410      * Destinatario
068300000412     c                   IF        Wtiporcd = 'P'
068400000412     c     TASlnp        chain     azorg
068500000412     C                   ELSE
068600000412     c     TASlna        chain     azorg
068700000412     C                   ENDIF
068800000412     c                   movel     orgfel        cmzdst                          *destinatario
068900000412      *
069000000412      * Cerco il terminal
069100000412     c                   IF        Wtiporcd = 'P'
069200000412     c                   eval      W0030 = TASlnp
069300000412     C                   ELSE
069400000412     c                   eval      W0030 = TASlna
069500000412     C                   ENDIF
069600000412     c                   eval      Wtipodata = 'T'
069700000412     c                   exsr      carter
069800000405      * cerco le librerie e i sistemi infomativi di arrivo
069900020827     C                   CLEAR                   TIBS55DS
070000981218     C                   MOVEL     knsif         i50psi
070100981218     C                   MOVEL     simfel        i50ppo
070200000412     C                   MOVEL     D55tfp        i50apo
070300020827     C                   CALL      'TIBS55R'
070400020827     C                   PARM                    TIBS55DS
070500981222     c                   movel     o50lcl        cmzlcl            1             *locale
070600000419      *
070700000419      * parto dalla libreria di gruppo
070800980928     c                   movel     o50plg        cmzlbd                         *dalla libreria
070900981218     c                   movel     o50asi        cmzsyf                         *sist.informativo
071000000404      *
071100000410     C                   eval      xx = 1
071200000410     C                   DOW       filet(xx) <> *blanks                         *se scritti dei rec
071300000404      *
071400000410     C                   MOVEL     FileT(xx)     cmzfld                          *dal file -trasm-
071500000410     C                   MOVEL     FileR(xx)     cmzfla                          *al file  -ricez-
071600000410     C                   MOVE      Rcdt(xx)      cmznrr                          *record inviati
071700000419      *
071800000419      * trasmetto nella libreria aziendale tranne per i legami (FNLBL00T)
071900000419      * che trasmetto nella libreria di gruppo
072000000419     C                   IF        FileT(xx) = 'FNLBL00T'                        *dal file -trasm-
072100000419     c                   movel     o50alg        cmzlba                         *alla  libreria
072200000419     c                   ELSE
072300000419     c                   movel     o50ala        cmzlba                         *alla  libreria
072400000419     c                   ENDIF
072500000412      * alla trasmissione di BLP/ARB aggancio l'esecuzione della ricezione
072600000413     C                   SELECT
072700000413     c                   WHEN      Wazione <> xx
072800000413     c                   WHEN      Wtiporcd = 'P'
072900000413     C                   movel     'BS22'        cmzazn                          *azione ricezione
073000000413     c                   OTHER
073100000413     C                   movel     'BS23'        cmzazn                          *azione ricezione
073200000413     c                   ENDSL
073300000412      *
073400020827     C                   CALL      'TIBS53C'
073500980911     C                   PARM                    cmzlcl
073600980828     C                   PARM                    cmzdst
073700980828     C                   PARM                    cmzfld
073800980828     C                   PARM                    cmzmbd
073900980828     C                   PARM                    cmzlbd
074000980828     C                   PARM                    cmzfla
074100980828     C                   PARM                    cmzmba
074200980828     C                   PARM                    cmzlba
074300980828     C                   PARM                    cmzori
074400980828     C                   PARM                    cmzdti
074500980828     C                   PARM                    cmzurg
074600980828     C                   PARM                    cmzadd
074700980828     C                   PARM                    cmzclr
074800980828     C                   PARM                    cmzftr
074900980828     C                   PARM                    cmzerr
075000980828     C                   PARM                    cmzazn
075100980828     C                   PARM                    cmzsyf
075200980828     C                   PARM                    cmznrr
075300980828     C                   PARM                    cmzprr
075400980911     C                   PARM                    w236            236
075500000410      *
075600000410     C                   add       1             xx
075700000410     c                   ENDDO
075800000405      *
075900980914     c                   exsr      finsr
076000000405      *
076100000419     C                   clear                   Wazione
076200000419      *
076300980828     C                   ENDSR
076400020304      *--------------------------------------------------------------------------------------------*
076500020925      * Trasmette il file FIVABBFT
076600020304      *--------------------------------------------------------------------------------------------*
076700020304     c     trasmivab     begsr
076800020304
076900020827     c                   Clear                   Wmbrt
077000020827     c                   Eval      Wmbrt = 'M' + Wmbrvab
077100020304     c                   movel(p)  wmbrt         cmzmbd                          *dal membro
077200020925     c                   movel(p)  'FIVAB00F'    cmzmba                          *al membro
077300020304     c                   clear                   cmzprr                          *prerequisito
077400020304     c                   move      '0000'        cmzori                          *ora invio
077500020304     c                   move      '0000000'     cmzdti                          *data invio
077600020304     c                   movel     'N'           cmzurg                          *urgenza notte
077700020304     c                   movel     'A'           cmzadd                          *record in aggiunta
077800020304     c                   movel     'S'           cmzclr                          *no pulizia file
077900020304     c                   movel     'S'           cmzftr                          *invio da transito
078000020304     c                   clear                   cmzerr                          *flag errore('1')
078100020304     c                   clear                   cmzazn                          *azione
078200020304
078300020304      * Destinatario
078400020304     c                   move      walnav        keyorg
078500020304     c     keyorg        chain     azorg
078600020304     c                   movel     orgfel        cmzdst                          *destinatario
078700020304
078800020304      * Cerco il terminal
078900020618     c                   movel     walnav        w0030
079000020304     c                   eval      Wtipodata = 'T'
079100020304     c                   exsr      carter
079200020304      * cerco le librerie e i sistemi infomativi di arrivo
079300020827     c                   clear                   tibs55ds
079400020304     c                   movel     knsif         i50psi
079500020304     c                   movel     simfel        i50ppo
079600020304     c                   movel     d55tfp        i50apo
079700020827     c                   call      'TIBS55R'
079800020827     c                   parm                    tibs55ds
079900020304     c                   movel     o50lcl        cmzlcl            1             *locale
080000020304
080100020304      * parto dalla libreria di gruppo
080200020304     c                   movel     o50plg        cmzlbd                         *dalla libreria
080300020304     c                   movel     o50asi        cmzsyf                         *sist.informativo
080400020304
080500020925     c                   movel     'FIVABBFT'    cmzfld                          *dal file -trasm-
080600020925     c                   movel     'FIVAB00F'    cmzfla                          *al file  -ricez-
080700020304     c                   move      rcdvab        cmznrr                          *record inviati
080800020304
080900020304      * trasmetto nella libreria aziendale
081000021009     c                   movel     o50alg        cmzlba                         *alla  libreria
081100020304
081200020827     C                   CALL      'TIBS53C'
081300020304     C                   PARM                    cmzlcl
081400020304     C                   PARM                    cmzdst
081500020304     C                   PARM                    cmzfld
081600020304     C                   PARM                    cmzmbd
081700020304     C                   PARM                    cmzlbd
081800020304     C                   PARM                    cmzfla
081900020304     C                   PARM                    cmzmba
082000020304     C                   PARM                    cmzlba
082100020304     C                   PARM                    cmzori
082200020304     C                   PARM                    cmzdti
082300020304     C                   PARM                    cmzurg
082400020304     C                   PARM                    cmzadd
082500020304     C                   PARM                    cmzclr
082600020304     C                   PARM                    cmzftr
082700020304     C                   PARM                    cmzerr
082800020304     C                   PARM                    cmzazn
082900020304     C                   PARM                    cmzsyf
083000020304     C                   PARM                    cmznrr
083100020304     C                   PARM                    cmzprr
083200020304     C                   PARM                    w236            236
083300020304
083400020304     c                   exsr      finsrvab
083500020304
083600020304     c                   endsr
083700000412      *--------------------------------------------------------------------------------------------*
083800000412      * carfil - carica filiali mper trasmissione dati
083900000412      *--------------------------------------------------------------------------------------------*
084000000412     C     carter        BEGSR
084100000412      *
084200000412     c                   clear                   FNLV55DS
084300000412     c                   movel     'P'           d55tpt
084400000412     c                   movel     W0030         d55lin
084500000412     c                   IF        Wtipodata = 'T'
084600000412     c                   movel     datcor        d55drf
084700000412     C                   ELSE
084800000412     c                   z-add     TASmgs        d55drf
084900000412     c                   movel     TASaas        d55drf
085000000412     c                   ENDIF
085100000412     c                   call      'FNLV55R'
085200000412     c                   parm                    FNLV55DS
085300000412      *
085400000412     c                   if        d55err <> *blanks
085500000412     c                   movel     W0030         d55tfp
085600000412     c                   endif
085700000412      *
085800000412     c                   endsr
085900000405      *--------------------------------------------------------------------------------------------*
086000000405      * finsr - disalloco membri di trasmissione
086100000405      *--------------------------------------------------------------------------------------------*
086200000322     C     finsr         BEGSR
086300000405      *
086400000413     c                   Do        wnumele       XX
086500000405      *
086600000405     c                   If        Wtiporcd = 'P'
086700000405     C                   MOVEL(P)  cmpDLC(xx)    comman
086800000405     c                   Else
086900000405     C                   MOVEL(P)  cmaDLC(xx)    comman
087000000405     c                   Endif
087100000405      *
087200000405     C                   CALL      'QCMDEXC'                            20
087300000405     C                   PARM                    comman
087400000405     C                   PARM                    lenght
087500000406      *
087600000405     c                   Enddo
087700000405      *
087800000322     C                   ENDSR
087900020304      *--------------------------------------------------------------------------------------------*
088000020925      * Disalloco membri di trasmissione FIVABBFT
088100020304      *--------------------------------------------------------------------------------------------*
088200020304     c     finsrvab      begsr
088300020304
088400020304     c                   movel(p)  cmdvab(5)     comman
088500020827     c                   eval      %subst(comman:54:6) = wmbrvab
088600020304     c                   call      'QCMDEXC'                            20
088700020304     c                   parm                    comman
088800020304     c                   parm                    lenght
088900020304
089000020304     c                   endsr
089100000404      *--------------------------------------------------------------------------------------------*
089200000404      * Scrittura archivi da trasmettere in partenza
089300000404      *--------------------------------------------------------------------------------------------*
089400000404     C     wrtrcd        BEGSR
089500000404      *
089600020204     C                   clear                   DSBL4A
089700000404     C                   clear                   DSBL4E
089800020204     C                   clear                   DSBL49
089900000404     C                   clear                   DTA4A
090000000404     C                   clear                   DS_titaa_o
090100000404     C                   clear                   DS_titaa_m
090200000404     C                   clear                   Wassegno
090300000413     C                   clear                   FIAR7000
090400000413     C                   clear                   FIAR6000
090500000413     C                   clear                   FNARB000
090600060221     C                   clear                   FIAR4000
090700051116     C                   clear                   FiAR9000
090800000413     C                   clear                   FNART000
090900041122     C****               clear                   FNLBL000
091000040831     C****               clear                   FNEVB000
091100000413     C                   clear                   FNBLP000
091200000413     C                   clear                   FNBLT000
091300000404      *
091400000419      * Leggo file clienti non codificati
091500000404     C     kspe          setll     TITAA30C
091600000404     C     kspe          reade     TITAA30C
091700000404     C                   DOW       not %EOF
091800000412     C                   SELECT
091900000404     c                   WHEN       TAAtrc = 'O'
092000000412     c                   movel     DS_titaa      DS_TITAA_O
092100000404     c                   WHEN       TAAtrc = 'M'
092200000412     c                   movel     DS_titaa      DS_TITAA_M
092300000404     C                   ENDSL
092400000404     C     kspe          reade     TITAA30C
092500000404     C                   ENDDO
092600000404      *
092700000404      * Leggo file C/assegno
092800000404     C     kspe          chain     TNCSB000
092900000407     c                   SELECT
093000000407     C                   WHEN      not %found
093100000413     C                   WHEN      CSBsta = 9
093200000407     C                   eval      Wassegno = 'A'
093300000407     C                   OTHER
093400000407     C                   eval      Wassegno = 'S'
093500000407     c                   ENDSL
093600000404      *
093700000404      * Leggo file riferimenti
093800000404     C                   eval      Ktrc = 'A'
093900000404     C     kta4          chain     TITA430C
094000000404     C                   If          %found
094100000404     C                   movel     TA4not        Dta4A
094200000404     c                   Endif
094300070927      * Leggo file riferimenti
094400070927     C                   eval      Ktrc = 'E'
094500070927     C     kta4          chain     TITA430C
094600070927     C                   If          %found
094700070927     C                   movel     TA4not        dsbl4E
094800070927     c                   Endif
094900000404      *
095000000404      * FNARB/FNBLP
095100000404     C                   eval      ARBaas = TASaas
095200000404     C                   eval      ARBlnp = TASlnp
095300000404     C                   eval      ARBnrs = TASnrs
095400000404     C                   eval      ARBnsp = TASnsp
095500000404     C                   eval      ARBmgs = TASmgs
095600000404     C                   eval      ARBfns = TASfns
095700060221     C****               eval      ARBscl = TASscl
095800000404     C                   eval      ARBfbr = TASfbr
095900000404     C                   eval      ARBfdn = TASfdn
096000000404     C                   eval      ARBlna = TASlna
096100000404     C                   eval      ARBznc = TASznc
096200000404     C                   eval      ARBksc = TASksc
096300000404     C                   eval      ARBfap = TASfap
096400000404     C                   eval      ARBrsd = TASrsd
096500000404     C                   eval      ARBind = TASind
096600000404     C                   eval      ARBcad = TAScad
096700000404     C                   eval      ARBlod = TASlod
096800000404     C                   eval      ARBprd = TASprd
096900000404     C                   eval      ARBnzd = TASnzd
097000000404     C                   eval      ARBfin = TASfin
097100000404     C                   eval      ARBgc1 = TASgc1
097200000404     C                   eval      ARBgc2 = TASgc2
097300000404     C                   eval      ARBctr = TASctr
097400000404     C                   eval      ARBcts = TAScts
097500000404     C                   eval      ARBftm = TASftm
097600000404     C                   eval      ARBtsp = TAStsp
097700000404     C                   eval      ARBnas = TASnas
097800000404     C                   eval      ARBncl = TASncl
097900000404     C                   eval      ARBncp = TASncp
098000000404     C                   eval      ARBpkc = TASpkc
098100000404     C                   eval      ARBpkb = TASpkb
098200000404     C                   eval      ARBpkf = TASpkf
098300000404     C                   eval      ARBncr = TASncr
098400000404     C                   eval      ARBvlc = TASvlc
098500000404     C                   eval      ARBfvb = TASfvb
098600000404     C                   eval      ARBvlb = TASvlb
098700000404     C                   eval      ARBfvf = TASfvf
098800000404     C                   eval      ARBvlf = TASvlf
098900000404     C                   eval      ARBqft = TASqft
099000001120     C                   If        TASfcm <> 'F'
099100000404     C                   eval      ARBias = TASias
099200000404     C                   eval      ARBvas = TASvas
099300001120     c                   EndIf
099400000404     C                   eval      ARBccm = TASccm
099500000404     C                   eval      ARBrmn = TASrmn
099600000404     C                   eval      ARBrma = §TA4Arma
099700000404     C                   eval      ARBrmo = OTAArsc
099800000404     C                   eval      ARBfls = TASfls
099900000404     C                   eval      ARBncd = TASncd
100000000404     C                   eval      ARBnca = TASnca
100100050113     C                   clear                   ARBxco
100200000404     C                   eval      ARBffd = TASffd
100300000404     C                   eval      ARBtcr = TAStcr
100400000404     C                   eval      ARBdcr = TASdcr
100500000404     C                   eval      ARBhcr = TAShcr
100600000404     C                   eval      ARBdti = TASdti
100700000404     C                   eval      ARBhti = TAShti
100800000404     C                   eval      ARBdce = TASdce
100900000404     C                   eval      ARBnpi = TASnpi
101000000404     C                   eval      ARBnci = TASnci
101100000404     C                   eval      ARBnrc = TASnrc
101200000404     C                   eval      ARBdbr = TASdbr
101300000404     C                   eval      ARBduc = TASduc
101400040528     C**!!!              eval      ARBflp = TASflp
101500040528     c                   Clear                   ArbFlp
101600000404     C                   eval      ARBpdc = TASpdc
101700000404     C                   eval      ARBndc = TASndc
101800000404     C                   eval      ARBddc = TASddc
101900000404     C                   eval      ARBdcp = TASdcp
102000000404     C                   eval      ARBdcm = TASdcm
102100000404     C                   eval      ARBhmc = TAShmc
102200000404     C                   eval      ARBtc1 = TASftc
102300000404     C                   eval      ARBtc2 = TAStc2
102400000404     C                   eval      ARBcca = TAScca
102500000404     C                   eval      ARBgga = TASgga
102600000404     C                   eval      ARBgma = TASgma
102700100604     c                   clear                   ds7R
102800100604     c                   If        ArbGma <>*blanks
102900100604     C                   eval      KCOD = '7R'
103000100604     C                   movel(P)  arbgma        KKEY
103100100604     C     keytab        chain     TABEL
103200100604     c                   IF        %found
103300100604     C                   movel     TBLUNI        ds7R
103400100604     c                   ENDIF
103500100604     c                   ENDIF
103600100604
103700100604     c                   If        §7rRC='S'
103800030801     c                   Clear                   ArbGma
103900030801     c                   EndIf
104000100604     c
104100000404     C                   eval      ARBgva = TASgva
104200000623     C                   Z-ADD     TASSTP        ARBSTP
104300000404     C                   eval      ARBctm = TASctm
104400000404     C                   eval      ARBdim = TASdim
104500000404     C                   eval      ARBdrt = TASdrt
104600000404     C                   eval      ARBflb = TASflb
104700000404     C                   eval      ARBfpp = TASfpp
104800000404     c                   eval      ARBnrt = TASnrt
104900040528     C                   eval      ARBtfp = TAStfp
105000000404     C                   eval      ARBtfa = TAStfa
105100000404     C                   eval      ARBdse = TASduc
105200000404     C                   eval      ARBdpc = TASduc
105300000404     C                   eval      ARBdam = TASdti
105400000404     C                   eval      ARBpdr = TASpdr
105500000404     C                   eval      ARBfst = TASll1
105600000404     C                   eval      ARBnfv = 99998
105700000404     C                   eval      ARBntc = 1
105800000404     C                   eval      ARBft3 = 'T'
105900000404     C                   eval      ARBdt3 = TASdti
106000000404     C                   eval      ARBamp = 1
106100000404     C                   eval      ARBama = 3
106200000404     C                   eval      ARBacc = 'S'
106300000404     C                   eval      ARBaca = 'S'
106400000404     C                   eval      ARBicc = 'Y'
106500000404     C                   eval      ARBica = 'Y'
106600020726     C                   eval      ARBifp = *zeros
106700000404     C                   eval      ARBfdc = 'S'
106800000428     C                   eval      ARBnmi = 989898
106900090423     c                   if        rigcau = 'C'
107000020204     C                   eval      ARBnmi = 979797
107100020204     c                   end
107200000428     C                   eval      ARBdmi = datcor
107300000407      *
107400000404     C                   IF        MTAArsc <> *blanks
107500010726     C                   eval      ARBrsm = mTAArsc
107600010726     C                   eval      ARBinm = mTAAind
107700010726     C                   eval      ARBcam = mTAAcap
107800010726     C                   eval      ARBlom = mTAAloc
107900010726     C                   eval      ARBprm = mTAAprv
108000010726     C                   eval      ARBnzm = mTAAnaz
108100000404     C                   ELSE
108200000419     c                   z-add     TASccm        I69kac
108300000419     c                   z-add     TASccm        I69kin
108400000404     c                   EXSR      CTR_ANAGRA
108500000404     C                   eval      ARBrsm = ACOrag
108600000404     C                   eval      ARBinm = INDvia
108700000404     C                   eval      ARBcam = INDcae
108800000404     C                   eval      ARBlom = INDcit
108900000404     C                   eval      ARBprm = INDprv
109000000404     C                   eval      ARBnzm = INDsta
109100000404     C                   ENDIF
109200000404      *
109300020204     C                   eval      ARBcbo = TAScbo
109400020204      * Per contrassegni omessi
109500090423     C                   IF        RIGcau = 'C'
109600020204     c                   select
109700020204     c                   when      %subst(tastbl: 1: 1) = 'F'
109800020204     C                   movel(p)  '4'           ARBcbo
109900020204     c                   when      %subst(tastbl: 1: 1) = 'A'
110000020204     C                   movel(p)  '6'           ARBcbo
110100020204     c                   endsl
110200020204     C                   clear                   ARBndc
110300000404     C                   eval      ARBicc = 'R'
110400000404     C                   ENDIF
110500000404      *
110600000404     c                   eval      W0030 = TASlnp
110700000412     c                   clear                   Wtipodata
110800000412     c                   exsr      carter
110900000404     c                   eval      ARBfle = d55tfp
111000000404      *
111100000404     c                   eval      W0030 = TASlna
111200000412     c                   clear                   Wtipodata
111300000412     c                   exsr      carter
111400000404     c                   eval      ARBfea = d55tfp
111401120601     c* recupero capofila della linea di arrivo per arbifp/Ipp
111402120606     c                   if        tasddc>0
111403120601     c                   clear                   fnlv55ds
111404120601     c                   eval      d55lin=taslna
111405120601     c                   eval      d55tpt='6'
111406120601     c                   eval      d55drf=tasddc
111407120601     c                   call      'FNLV55R'
111408120601     c                   parm                    FNLV55DS
111409120601     c                   eval      arbifp=d55tfa
111410120601     c                   eval      arbipp=arbifp
111411120606     c                   endif
111500000410      *
111600000414      * FIAR6-7 1a BOLLA
111700000414     c                   exsr      AGGfatt1
111800000414      *
111900000414      * Verifico se il tipo porto è tra quelli convertibili
112000000417     c                   clear                   DS3a
112100000417     c                   eval      xx = 1
112200000417     C     TAScbo        lookup    T3aK(xx)                               31
112300000417      *
112400000417     C                   if        *in31 = *on
112500000417     c                   movel     T3aD(xx)      DS3a
112600000414     C                   SELECT
112700000417      *  non convertibile
112800000417     C                   WHEN      §3aCfr = *blanks
112900000414      *  se il C/a è annullato converto il tipo bolla
113000000414     C                   WHEN      Wassegno = 'A'
113100000417     C                   movel     §3aCfr        ARBcbo
113200000417      *  FIAR6-7  2a BOLLA se esiste 2a bolla
113300000417     c                   WHEN      $TASaas > *zeros
113400000414     c                   exsr      AGGfatt2
113500000414     c                   ENDSL
113600000417     c                   endif
113700000404      *
113800051116      * FIAR9
113900000404     C                   IF        Wassegno = 'S'
114000000404     C                   eval      AR9aas = TASaas
114100000404     C                   eval      AR9lnp = TASlnp
114200000404     C                   eval      AR9nrs = TASnrs
114300000404     C                   eval      AR9nsp = TASnsp
114400000404     C                   eval      AR9cas = CSBcas
114500000404     C                   eval      AR9vca = CSBvca
114600000404     C                   eval      AR9cmb = CSBcmb
114700000404     C                   eval      AR9gca = CSBgca
114800090423     C                   If          RIGcau <> 'A'
114900000404     c                   eval      AR9tic = CSBtpa + CSBtpi
115000000404     c                   Endif
115100051116      *
115200051116     C                   WRITE     FiAR9000
115300051116     C                   eval      WfileT = 'FiAR900T'
115400051116     C                   eval      WfileR = 'FiAR900R'
115500000412     C                   exsr      AGGnrec
115600000404      *
115700000404     C                   ENDIF
115800000404      *
115900060221      * FIAR4
116000000404     C                   eval      AR4aas = TASaas
116100000404     C                   eval      AR4lnp = TASlnp
116200000404     C                   eval      AR4nrs = TASnrs
116300000404     C                   eval      AR4nsp = TASnsp
116400000404      *   Tipo Record "A"
116500120607     c*****              If        Wtiporcd <> 'P'
116600031029     c                   Select
116700031029     c                   When      TasLl1 = 'C'
116800031029     c                   Eval      §B4Abm = 'Q'
116900031029     c                   When      TasLl1 = 'S'
117000031029     c                   Eval      §B4Abm = 'P'
117100050301     c                   When      TasLl1 = 'N' or Tasll1 = 'Y'
117200031029     c                   Eval      §B4Abm = 'J'
117300031113     c                   When      TasLl1 = 'K'
117400031113     c                   Eval      §B4Abm = 'W'
117500031029     c                   Other
117600000404     C                   eval      §B4Abm = TASll1
117700031029     c                   EndSl
117800000404     C                   eval      §B4Aso = '-'
117900060221     C                   movel(p)  DSbl4A        AR4not
118000000404     C                   eval      AR4trc = 'A'
118100060221     C                   WRITE     FIAR4000
118200060221     C                   eval      WfileT = 'FIAR400T'
118300060221     C                   eval      WfileR = 'FIAR400R'
118400000410     C                   exsr      AGGnrec
118500120607     C*****              Endif
118600000404      *   Tipo Record "F"
118700000404     C                   eval      Ktrc = 'F'
118800000404     C     kta4          chain     TITA430C
118900000404     C                   If          %found
119000060221     c                   clear                   ar4not
119100000404     C                   eval      AR4not = TA4not
119200000404     C                   eval      AR4trc = TA4trc
119300000404      *
119400060221     C                   WRITE     FIAR4000
119500060221     C                   eval      WfileT = 'FIAR400T'
119600060221     C                   eval      WfileR = 'FIAR400R'
119700000412     C                   exsr      AGGnrec
119800000404      *
119900000404     C                   Endif
120000060530      *   Tipo Record "I"
120100060530     C                   eval      Ktrc = 'I'
120200060530     C     kta4          chain     TITA430C
120300060530     C                   If          %found
120400060530     c                   clear                   ar4not
120500060530     C                   eval      AR4not = TA4not
120600060530     C                   eval      AR4trc = TA4trc
120700060530      *
120800060530     C                   WRITE     FIAR4000
120900060530     C                   eval      WfileT = 'FIAR400T'
121000060530     C                   eval      WfileR = 'FIAR400R'
121100060530     C                   exsr      AGGnrec
121200060530      *
121300060530     C                   Endif
121400000404      *   Tipo Record "E"
121500070927     C***                If        §TA4Arpt <> *blanks
121600070927     C***                eval      §B4Erp = §TA4Arpt
121700070927     c
121800070927     c* La dsble$ è già impostata per cui non devo scrivere nulla
121900070927     c                   if        §b4erp<>*blanks
122000000404      *
122100000404     C     TASlna        chain     AZORG
122200000404     C                   If        ORGdit = 'SDI'  and  ORGfl1 = 'E'
122300000404     C                   eval      §B4Ema = 'A'
122400000404     C                   eval      §B4Eat = 'P'
122500000404     C                   eval      §B4Est = 'S'
122600000404     C                   Endif
122700060221     C                   movel(P)  DSbl4E        AR4not
122800000404     C                   eval      AR4trc = 'E'
122900000404      *
123000060221     C                   WRITE     FIAR4000
123100060221     C                   eval      WfileT = 'FIAR400T'
123200060221     C                   eval      WfileR = 'FIAR400R'
123300000412     C                   exsr      AGGnrec
123400000412      *
123500000412     C                   Endif
123600000404      *
123700000404      *   Tipo Record "8"
123800000404     C                   eval      AR4trc = '8'
123900060221     c                   clear                   ar4not
124000000404     C                   eval      AR4not = 'BOLLA RIGENERATA DALLA SEDE'
124100000404      *
124200060221     C                   WRITE     FIAR4000
124300060221     C                   eval      WfileT = 'FIAR400T'
124400060221     C                   eval      WfileR = 'FIAR400R'
124500000412     C                   exsr      AGGnrec
124600020204      *   Tipo Record "9" per contrassegni omessi
124700090423     c                   if        rigcau = 'C'
124800020204     C                   eval      AR4trc = '9'
124900020204     C                   eval      §b4ds9 = 'PER C/ASSEGNO OMESSO'
125000020206     C                   eval      §b4dt9 = datcor
125100060221     c                   clear                   ar4not
125200020204     C                   eval      AR4not = dsbl49
125300020204      *
125400060221     C                   WRITE     FIAR4000
125500060221     C                   eval      WfileT = 'FIAR400T'
125600060221     C                   eval      WfileR = 'FIAR400R'
125700020204     C                   exsr      AGGnrec
125800020204     C                   Endif
125900111011      *   Tipo Record "D"
126000111011     C                   eval      Ktrc = 'D'
126100111011     C     kta4          chain     TITA430C
126200111011     C                   If          %found
126300111011     c                   clear                   ar4not
126400111011     C                   eval      AR4not = TA4not
126500111011     C                   eval      AR4trc = TA4trc
126600111011      *
126700111011     C                   WRITE     FIAR4000
126800111011     C                   eval      WfileT = 'FIAR400T'
126900111011     C                   eval      WfileR = 'FIAR400R'
127000111011     C                   exsr      AGGnrec
127100111011      *
127200111011     C                   Endif
127300020204      *                                                                   CC
127400000404      * FNBLT/FNART
127500000404     C                   eval      ARTaas = TASaas
127600000404     C                   eval      ARTlnp = TASlnp
127700000404     C                   eval      ARTnrs = TASnrs
127800000404     C                   eval      ARTnsp = TASnsp
127900000419     C                   eval      ARTfls = TASfls
128000000419     C                   eval      ARTlna = TASlna
128100000404     C                   eval      ARTdfv = ARBduc
128200000518     C                   eval      ARTnpp = 86
128300000404     C                   eval      ARTnfv = 99998
128400000404     C                   eval      ARTdse = ARBdse
128500000518     C                   eval      ARTnpe = 86
128600000404     C                   eval      ARTdam = ARBdam
128700000404     C                   eval      ARTdcm = ARBdcm
128800000518     C                   eval      ARTnap = 86
128900000404     C                   eval      ARTabn = 'S'
129000000404      *   Numero segnacollo: se sequenziale sviluppo da titas
129100000404      *                      se non sequenziale sviluppo da titat
129200000404     C                   IF         TASFNS = 'S'
129300000404     c     kspe          setll     TITAT30c
129400000404     c     kspe          reade     TITAT30c
129500000404     c                   DOW       not %EOF
129600000404     c                   eval       ARTnsc = TATnsc
129700000404     c                   If        Wtiporcd = 'P'
129800000404     C                   WRITE     FNBLT000
129900000412     C                   eval      WfileT = 'FNBLT00T'
130000000412     C                   eval      WfileR = 'FNBLT00R'
130100000404     c                   Else
130200000404     C                   WRITE     FNART000
130300000412     C                   eval      WfileT = 'FNART00T'
130400000412     C                   eval      WfileR = 'FNART00R'
130500000404     C                   Endif
130600000412     C                   exsr      AGGnrec
130700000404     c     kspe          reade     TITAT30c
130800000404     C                   ENDDO
130900000404      *
131000000404     C                   ELSE
131100000404      *
131200000404     C                   If        TASncd > 0
131300000404      *
131400000404     C                   If          TASnca = 0
131500000404     C                   eval      TASnca = TASncd
131600000404     C                   Endif
131700000404      *
131800000404     C     TASNCD        DO        TASNCA        WSEGNA
131900000404     c                   eval       ARTnsc = Wsegna
132000000404     c                   If        Wtiporcd = 'P'
132100000404     C                   WRITE     FNBLT000
132200000412     C                   eval      WfileT = 'FNBLT00T'
132300000412     C                   eval      WfileR = 'FNBLT00R'
132400000404     c                   Else
132500000419     C                   WRITE     FNART000
132600000419     C                   eval      WfileT = 'FNART00T'
132700000419     C                   eval      WfileR = 'FNART00R'
132800000404     C                   Endif
132900000412     C                   exsr      AGGnrec
133000000404     C                   ENDDO
133100000404      *
133200000404     C                   Endif
133300000404     C                   ENDIF
133400000412      *
133500000412      * FNLBL
133600041122     c**                 IF        Wlegame = 'S'
133700041122     c**                 eval      Waas = WPaas
133800041122     c**                 eval      Wlnp = WPlnp
133900041122     c**                 eval      Wnrs = WPnrs
134000041122     c**                 eval      Wnsp = WPnsp
134100041122     c**   KBOLw         chain     fnlbl02l
134200000412      *
134300041122    4c**                 DOW       %FOUND
134400041122     c**                 eval      LBLft2 = 'T'
134500041122     C**                 write     FNLBL000
134600041122     C**                 eval      WfileT = 'FNLBL00T'
134700041122     C**                 eval      WfileR = 'FNLBL00R'
134800041122     C**                 exsr      AGGnrec
134900041122     c**                 eval      Waas = lblaan
135000041122     c**                 eval      Wlnp = lbllpn
135100041122     c**                 eval      Wnrs = lblnrn
135200041122     c**                 eval      Wnsp = lblnsn
135300041122     c**   KBOLw         chain     fnlbl02l
135400041122    4c**                 ENDDO
135500000412      *
135600041122    3c**                 ENDIF
135700000412      *
135800000412      * FNEVB
135900040831     C**** kspe          setll     FNEVB01L
136000040831     C**** kspe          READE     FNEVB01L
136100040831    4c****               DOW       not %EOF
136200000412      *
136300040831     C****               IF        EVBcev = 'R  '
136400040831     C****               write     FNEVB000
136500040831     C****               eval      WfileT = 'FNEVB00T'
136600040831     C****               eval      WfileR = 'FNEVB00R'
136700040831     C****               exsr      AGGnrec
136800040831     c****               ENDIF
136900000412      *
137000040831     C**** kspe          READE     FNEVB01L
137100040831    4c****               ENDDO
137200030110      *
137300030110      * FIAR5
137400030110     c                   exsr      srFIAR5
137500000412      *
137600000412      * Scrivo ARB/BLP. Devono essere gli ultimi file perchè a questi
137700000412      *                 è legata la call all'azione di ricezione.
137800000412     c                   If        Wtiporcd = 'P'
137900000412     C                   eval      BLPsop = '-'
138000000412     C                   WRITE     FNBLP000
138100000412     C                   eval      WfileT = 'FNBLP00T'
138200000412     C                   eval      WfileR = 'FNBLP00R'
138300000412     c                   Else
138400000412     C                   WRITE     FNARB000
138500000412     C                   eval      WfileT = 'FNARB00T'
138600000412     C                   eval      WfileR = 'FNARB00R'
138700000412     c                   Endif
138800000412     C                   exsr      AGGnrec
138900000412     c                   z-add     XX            Wazione
139000000404      *
139100000404     c                   endsr
139200020304      *--------------------------------------------------------------------------------------------*
139300020304      * Recupero dati dal titas
139400020304      *--------------------------------------------------------------------------------------------*
139500020304     c     recdati       begsr
139600020304
139700020304      * Verifico se devo riagganciare TITAS per recuperare i dati che servono
139800020304      * mamma originale
139900020304     c                   if        wpaas <> tasaas  or  wplnp <> taslnp  or
140000020304     c                             wpnrs <> tasnrs  or  wpnsp <> tasnsp
140100020304     c                   clear                   ds_titas
140200020304     c                   clear                   ds_titas_2
140300020304     c                   eval      waas = wpaas
140400020304     c                   eval      wlnp = wplnp
140500020304     c                   eval      wnrs = wpnrs
140600020304     c                   eval      wnsp = wpnsp
140700020304     c                   eval      *in92 = *off
140800020304      * se non trovo TITAS accendo *in92                                     interessato
140900020304     c                   exsr      AGGTAS
141000020304     c                   endif
141100020304
141200020304     c                   if        *in92 = *off
141300020304
141400020304     c                   movel     tasaas        wpaasv
141500020304     c                   movel     taslnp        wplnpv
141600020304     c                   movel     tasnrs        wpnrsv
141700020304     c                   movel     tasnsp        wpnspv
141800020304
141900090423      * Se tipo richiesta 'F'
142000090423     c                   If        Rigcau = 'F'
142100090423      * addebito mittente
142200090423     c                   if        rigadd = 'M'
142300020903     c                   movel     tasccm        wpkscv
142400040511      * ragione sociale mittente originale  valorizzato con la rag.sociale destinatario
142500040511     c                   movel     tasrsd        Wvabrmo
142600020903     c                   EndIf
142700090423      * addebito al destinatario
142800090423     c                   If        rigadd = 'D'
142900040517     c                   movel     tasksc        wpkscv
143000040511      * ragione sociale mittente originale  valorizzato con la rag.sociale mittente
143100040513     c                   Eval      ktrc = 'M'
143200040513     c     kta4          Chain     Titaa30c
143300040513     c                   If        %Found(Titaa30c)
143400040513     c                   eval      Wvabrmo = TaaRsc
143500040513     c                   EndIf
143600090423     c                   endif
143700030514      * riferimento mittente alfanumerico   valorizzato con rif.mittente numerico
143800030515     c                   eval      WVABrma= %editc(tasrmn:'Z')
143900030508     c                   endif
144000020304
144100020304     c                   movel     taslna        wplnav
144200030508
144300020304     c                   clear                   wlbdat
144400020304     c                   movel     tasaas        g02inv
144500020304     c                   move      tasmgs        g02inv
144600020304     c                   movel     '3'           g02err
144700020304     c                   call      'XSRDA8'
144800020304     c                   parm                    wlbdat
144900020304     c                   move      g02dat        wpdspv
145000020304      * ragione sociale
145100020304     c                   move      wpkscv        I69kac
145200020304     c                   EXSR      CTR_ANAGRA
145300020304     c                   eval      wpragv = acorag
145400020304
145500020304     c                   endif
145600020304
145700020304      * Verifico se devo riagganciare TITAS per recuperare i dati che servono
145800020304      * ultima figlia
145900020304     c                   if        waaas <> tasaas  or  walnp <> taslnp  or
146000020304     c                             wanrs <> tasnrs  or  wansp <> tasnsp
146100020304     c                   clear                   ds_titas
146200020304     c                   clear                   ds_titas_2
146300020304     c                   eval      waas = waaas
146400020304     c                   eval      wlnp = walnp
146500020304     c                   eval      wnrs = wanrs
146600020304     c                   eval      wnsp = wansp
146700020304     c                   eval      *in92 = *off
146800020304      * se non trovo TITAS accendo *in92                                     interessato
146900020304     c                   exsr      AGGTAS
147000020304     c                   endif
147100020304
147200020304     c                   if        *in92 = *off
147300020304
147400020304     c                   movel     tasaas        waaasv
147500020304     c                   movel     taslnp        walnpv
147600020304     c                   movel     tasnrs        wanrsv
147700020304     c                   movel     tasnsp        wanspv
147800020304
147900020304     c                   movel     taslna        walnav
148000020304     c                   movel     taspdc        wapdcv
148100020304     c                   movel     tasndc        wandcv
148200020304
148300020304     c                   clear                   wlbdat
148400020304     c                   movel     tasddc        g02inv
148500020304     c                   movel     '3'           g02err
148600020304     c                   call      'XSRDA8'
148700020304     c                   parm                    wlbdat
148800020304     c                   move      g02dat        waddcv
148900020304
149000020304     c                   clear                   wlbdat
149100020304     c                   movel     tasaas        g02inv
149200020304     c                   move      tasmgs        g02inv
149300020304     c                   movel     '3'           g02err
149400020304     c                   call      'XSRDA8'
149500020304     c                   parm                    wlbdat
149600020304     c                   move      g02dat        wadspv
149700020304
149800020304     c                   endif
149900020304
150000020304     c                   endsr
150100020304      *--------------------------------------------------------------------------------------------*
150200020304      * Scrittura messaggi P.E.I.
150300020304      *--------------------------------------------------------------------------------------------*
150400020304     c     crtposta      begsr
150500020304
150600090423     c                   movel     rigfil        wrigpor
150700020304
150800020304     c                   clear                   tnsym5cds
150900020304
151000020403     c                   eval      ym5tes = PE_testa1
151100020304
151200020304      * Preparo il messaggio uguale sia per la partenza che per l'arrivo
151300020304     c                   eval      ym5t01 = PE_riga1
151400020304     c                   eval      %subst(ym5t01:9:3) = wrigpor
151500020304
151600020304     c                   eval      ym5t02 = PE_riga2
151700020305     c                   eval      %subst(ym5t02:29:3) = wplnpv
151800020305     c                   eval      %subst(ym5t02:33:2) = wpnrsv
151900020305     c                   eval      %subst(ym5t02:36:7) = wpnspv
152000020304     c                   movel     wpdspv        wdatav
152100020305     c                   eval      %subst(ym5t02:48:2) = wgg
152200020305     c                   eval      %subst(ym5t02:51:2) = wmm
152300020305     c                   eval      %subst(ym5t02:54:4) = waa
152400020304
152500020304     c                   eval      ym5t03 = PE_riga3
152600020304     c                   eval      %subst(ym5t03:9:7) = wpkscv
152700020305     c                   eval      %subst(ym5t03:17:35) = wpragv
152800020305
152900020305      * se devo scrivere anche la spedizione figlia
153000020305     c                   if        wpaasv <> waaasv or  wplnpv <> walnpv or
153100020305     c                             wpnrsv <> wanrsv or  wpnspv <> wanspv
153200020305      * aggiungo righe della spedizione figlia
153300020305     c                   eval      ym5t04 = PE_riga4
153400020305     c                   eval      %subst(ym5t04:26:3) = walnpv
153500020305     c                   eval      %subst(ym5t04:30:2) = wanrsv
153600020305     c                   eval      %subst(ym5t04:33:7) = wanspv
153700020305     c                   movel     wadspv        wdatav
153800020305     c                   eval      %subst(ym5t04:45:2) = wgg
153900020305     c                   eval      %subst(ym5t04:48:2) = wmm
154000020305     c                   eval      %subst(ym5t04:51:4) = waa
154100020305
154200020305     c                   eval      ym5t05 = pe_riga5
154300020304
154400020305     c                   eval      ym5t06 = pe_riga6
154500020305     c                   eval      %subst(ym5t06:25:7) = wandcv
154600020304     c                   movel     waddcv        wdatav
154700020305     c                   eval      %subst(ym5t06:37:2) = wgg
154800020305     c                   eval      %subst(ym5t06:40:2) = wmm
154900020305     c                   eval      %subst(ym5t06:43:4) = waa
155000020305     c                   eval      %subst(ym5t06:59:7) = wapdcv
155100020304      * righe da inviare
155200020305     c                   z-add     6             ym5rig
155300020305
155400020305     c                   else
155500020305
155600020305      * scrivo solo righe delle distinta di consegna
155700020305     c                   eval      ym5t04 = pe_riga6
155800020305     c                   eval      %subst(ym5t04:25:7) = wandcv
155900020305     c                   movel     waddcv        wdatav
156000020305     c                   eval      %subst(ym5t04:37:2) = wgg
156100020305     c                   eval      %subst(ym5t04:40:2) = wmm
156200020305     c                   eval      %subst(ym5t04:43:4) = waa
156300020305     c                   eval      %subst(ym5t04:59:7) = wapdcv
156400020305      * righe da inviare
156500020305     c                   z-add     4             ym5rig
156600020305
156700020305     c                   endif
156800020617
156900020617      * Utente + p.o. CHE EFFETTUA LA RICHIESTA
157000090423     c                   z-add     rigfil        ym5fde
157100020617     c                   eval      ym5ude = 'RAC'
157200020304
157300020304     c                   call      'TNSYM5C'
157400020304     c                   parm                    tnsym5cds
157500020617
157600020617      * Utente + p.o. CHE DEVE RICERCARE LA BOLLA
157700020617     c                   movel     walnav        ym5fde
157800020617     c                   eval      ym5ude = 'RAM'
157900020304
158000020304     c                   call      'TNSYM5C'
158100020304     c                   parm                    tnsym5cds
158200020304
158300020304     c                   endsr
158400020304      *--------------------------------------------------------------------------------------------*
158500020925      * Scrittura FIVABBFT
158600020304      *--------------------------------------------------------------------------------------------*
158700020304     c     wrtvab        begsr
158800020304
158900020925     c                   clear                   fivab000
159000020715
159100020715      * Imposto data generazione bolla
159200020715     c                   movel     datcor        vabaas
159300020715     c                   move      datcor        vabmgs
159400020304
159500020304      * cap - località- provincia destinatario: del p.o. che ha fatto la richiesta
159600090423     c     rigfil        chain     azorg01l
159700020304     c                   if        not %found(azorg01l)
159800020304     c                   clear                   orgcpf
159900020304     c                   clear                   orgloc
160000020304     c                   clear                   orgpro
160100020304     c                   endif
160200020304
160300020304      * p.o. arrivo: il p.o. che ha fatto la richiesta
160400090423     c                   eval      vablna = rigfil
160500020304      * p.o. partenza: la linea di arrivo della bolla ultima figlia
160600020304     c                   movel     walnav        vablnp
160700020304      * cliente: linea di arrivo ultima figlia + 9999
160800020304     c                   movel     walnav        vabccm
160900020304     c                   move      9999          vabccm
161000020315      * tipo bolla: 'F'
161100020510     c                   eval      vabcbo ='FO'
161200020315      * codice tratamento merce: '4F'
161300020315     c                   eval      vabctm ='4F'
161400020304      * destinatario
161500020530     c                   eval      vabrsd = 'RAC Cli. ' + wpkscv + ' ' +
161600020508     c                             '- Bolla firmata'
161700020304     c                   eval      vabind = wpragv
161800020304     c                   movel     orgcpf        vabcad
161900020304     c                   eval      vablod = orgloc
162000020304     c                   eval      vabprd = orgpro
162100020304      * natura merce
162200020304     c                   eval      vabnas = 'BOLLA FIRMATA'
162300020304      * colli
162400020304     c                   eval      vabncl = 1
162500020506      * peso
162600020506     c                   eval      vabpkb = 1
162700020304      * riferimento mittente numerico: aa-lnp-nrs-nsp della spedizione mamma originale
162800020304     c                   move      wpaasv        wvabaas
162900020304     c                   movel     wplnpv        wvablnp
163000020304     c                   movel     wpnrsv        wvabnrs
163100020304     c                   movel     wpnspv        wvabnsp
163200020304     c                   move      wnumbolla     vabrmn
163300020304      * note
163400020304     c                   movel     waddcv        wdatav
163500020304     c                   eval      vabnot = 'Distinta n.' + ' ' + wandcv + ' ' +
163600020304     c                             'del' + ' ' + wgg + '/' + wmm + '/' + waa
163700020304
163800020304     c                   add       1             rcdvab
163900020925      *
164000020925      * P.O di gestione
164100020925     c                   move      walnav        vabfgs
164200030508      *
164300030508      * RMO + RMA
164400030508     c                   eval      vabrmo = wvabrmo
164500030508     c                   eval      vabrma = wvabrma
164600030508      *
164700020925      *
164800020925     c                   write     fivab000
164900020304
165000030508     c                   clear                   wvabrmo
165100030508     c                   clear                   wvabrma
165200030508
165300020304     c                   endsr
165400000414      *****************************************************************
165500000414      *  Creo file fatturazione per prima bolla
165600000414      *****************************************************************
165700000414     C     AGGfatt1      BEGSR
165800000414      *
165900000414      * ricavo il tipo porto
166000000414     C                   clear                   DSTB
166100000414     C                   eval      XX = 1
166200000414     C                   eval      W008a = TAStbl
166300000417     C     W008a         lookup    TblK(xx)                               31
166400000417     C                   movel     TblD(xx)      DSTB
166500000414      *
166600000414     C                   SELECT
166700000414      * Non elaboro fatture non emesse dal P.O.
166800000414     c                   WHEN      TASfiv = *zeros  or
166900000414     c                             TASfiv = §QCfiv  or TASfiv = §QCfi2  or
167000000414     c                             TASfiv = §QCfi3  or TASfiv = §QCfi4  or
167100000414     c                             TASfiv = §QCfi5  or TASfiv = §QCfi6
167200000414      * Non elaboro i porti franchi se trasmetto in arrivo
167300120607     c******             WHEN      §TBtpo = 'F' and Wtiporcd <> 'P'
167400000414      * Se porto assegnato da trasmettere in partenza invio solo KSC e CTR
167500120607     c*****              WHEN      §TBtpo = 'A' and Wtiporcd = 'P'
167600120607     C******             eval      AR6aas = TASaas
167700120607     C******             eval      AR6lnp = TASlnp
167800120607     C*****              eval      AR6nrs = TASnrs
167900120607     C******             eval      AR6nsp = TASnsp
168000120607     C********           eval      AR6trc = '1'
168100120607     C*********          eval      AR6ksc = TASksc
168200120607     C********           eval      AR6ctr = TASctr
168300120607     C********           WRITE     FIAR6000
168400120607     C********           eval      WfileT = 'FIAR600T'
168500120607     C********           eval      WfileR = 'FIAR600R'
168600120607     C********           exsr      AGGnrec
168700000414      *
168800000414     c                   OTHER
168900000414     C                   eval      AR6div = TASdiv
169000000414     C                   eval      AR6por = TASpor
169100000414     C                   eval      AR6sv1 = TASsv1
169200000414     C                   eval      AR6va1 = TASva1
169300000414     C                   eval      AR6sv2 = TASsv2
169400000414     C                   eval      AR6va2 = TASva2
169500000414     C                   eval      AR6sv3 = TASsv3
169600000414     C                   eval      AR6va3 = TASva3
169700000414     C                   eval      AR6imv = TASimv
169701120103     c                   if        tasnft>0
169702120103     c                   if        tasdft>§qtdsa
169800000414     C                   eval      AR6ali = §QTali
169801120103     c                   else
169802120103     C                   eval      AR6ali = §QTalo
169803120103     c                   endif
169804120103     c                   endif
169900000414     C                   eval      AR6cei = TASfei
170000000414     C                   eval      AR6fiv = TASfiv
170100000414     C                   eval      AR6nft = TASnft
170200000414     C                   eval      AR6dft = TASdft
170300020916
170400020916     C                   eval      AR6aas = TASaas
170500020916     C                   eval      AR6lnp = TASlnp
170600020916     C                   eval      AR6nrs = TASnrs
170700020916     C                   eval      AR6nsp = TASnsp
170800020916     C                   eval      AR6trc = '1'
170900020916     C                   eval      AR6ksc = TASksc
171000020916     C                   eval      AR6ctr = TASctr
171100020916
171200000414      *   calcolo l'importo
171300000426     C                   clear                   FNLV16DS
171400000414     c                   IF         §TBTPO = 'F'
171500000414     c                   eval      D17PRP = 'S'
171600000414     c                   ENDIF
171700000414     c                   eval      D17tla = 'L'
171800000414     c                   eval      D17tbo = '0'
171900000414     c                   eval      D17crb = 'S'
172000000414     c                   eval      D17aas = TASaas
172100000414     c                   eval      D17lns = TASlnp
172200000414     c                   eval      D17nrs = TASnrs
172300000414     c                   eval      D17nsp = TASnsp
172400000414     c                   eval      D17tbs = TAStbl
172500000414     c                   eval      D17fim = 'F'
172600000414     c                   CALL      'FNLV16R'
172700000414     c                   PARM                    FNLV16DS
172800000414     c                   PARM                    DTASV
172900000414      *
173000000414     C                   eval      AR6ift = O17ift
173100000414      *
173200000414     C                   write     FIAR6000
173300000414     C                   eval      WfileT = 'FIAR600T'
173400000414     C                   eval      WfileR = 'FIAR600R'
173500000414     C                   exsr      AGGnrec
173600000414      *
173700000414      * FIAR7
173800000414     c     KSPE          setll     TITA7000
173900000414     c     KSPE          reade     TITA7000
174000000414     c                   DOW       not %EOF
174100000414     c                   eval      AR7aas = TA7aas
174200000414     c                   eval      AR7lnp = TA7lnp
174300000414     c                   eval      AR7nrs = TA7nrs
174400000414     c                   eval      AR7nsp = TA7nsp
174500000414     c                   eval      AR7trc = '1'
174600000414     c                   eval      AR7svn = TA7svn
174700000414     c                   eval      AR7van = TA7van
174800000414     c                   write     FIAR7000
174900000414     C                   eval      WfileT = 'FIAR700T'
175000000414     C                   eval      WfileR = 'FIAR700R'
175100000414     C                   exsr      AGGnrec
175200000414      *
175300000414     c     KSPE          reade     TITA7000
175400000414     c                   ENDDO
175500000414      *
175600000414     c                   ENDSL
175700000414      *
175800000414     c                   ENDSR
175900000414      *****************************************************************
176000000414      *  Creo file fatturazione per seconda bolla
176100000414      *****************************************************************
176200000414     C     AGGfatt2      BEGSR
176300000414      *
176400000414     C                   clear                   DSTB
176500000414     C                   eval      XX = 1
176600000414     C                   eval      W008a = $TAStbl
176700000417     C     W008a         lookup    TblK(xx)                               31
176800000417     C                   movel     TblD(xx)      DSTB
176900000414      *
177000000414     C                   SELECT
177100000417      *
177200000417      * Non elaboro se non è prevista la 2a bolla
177300000417     c                   WHEN      §3Atb2 = *blanks
177400000417      *
177500000417      * Non elaboro se la 2a bolla è uno storno
177600000417     c                   WHEN      §TBfcb = '0'
177700000417      *
177800000414      * Non elaboro fatture non emesse dal P.O.
177900000417     c                   WHEN      $TASfiv = *zeros  or
178000000417     c                             $TASfiv = §QCfiv  or $TASfiv = §QCfi2  or
178100000417     c                             $TASfiv = §QCfi3  or $TASfiv = §QCfi4  or
178200000417     c                             $TASfiv = §QCfi5  or $TASfiv = §QCfi6
178300000417      *
178400000414      * Porto assegnato da trasmettere in partenza invio solo KSC e CTR
178500120607     c********           WHEN      §TBtpo = 'A' and Wtiporcd = 'P'
178600120607     C*******            eval      AR6aas = $TASaas
178700120607     C********           eval      AR6lnp = $TASlnp
178800120607     C*********          eval      AR6nrs = $TASnrs
178900120607     C*********          eval      AR6nsp = $TASnsp
179000120607     C**********         eval      AR6trc = '2'
179100120607     C**********         eval      AR6ksc = $TASksc
179200120607     C***********        eval      AR6ctr = $TASctr
179300000417      *
179400120607     C**********         write     FIAR6000
179500120607     C********           eval      WfileT = 'FIAR600T'
179600120607     C**********         eval      WfileR = 'FIAR600R'
179700120607     C**********         exsr      AGGnrec
179800000417      *
179900000414      * Porto assegnato da trasmettere in arrivo
180000120607     c*********          WHEN      §TBtpo = 'A' and Wtiporcd <> 'P'
180001120607     c* Sia per partenza che per arrivo trasmetto alla stessa maniera
180003120607     c                   WHEN      §TBtpo = 'A'
180100000414     C                   eval      AR6aas = $TASaas
180200000414     C                   eval      AR6lnp = $TASlnp
180300000414     C                   eval      AR6nrs = $TASnrs
180400000414     C                   eval      AR6nsp = $TASnsp
180500000414     C                   eval      AR6trc = '2'
180600000414     C                   eval      AR6ksc = $TASksc
180700000414     C                   eval      AR6ctr = $TASctr
180800000414     C                   eval      AR6div = $TASdiv
180900000414     C                   eval      AR6por = $TASpor
181000000414     C                   eval      AR6sv1 = $TASsv1
181100000414     C                   eval      AR6va1 = $TASva1
181200000414     C                   eval      AR6sv2 = $TASsv2
181300000414     C                   eval      AR6va2 = $TASva2
181400000414     C                   eval      AR6sv3 = $TASsv3
181500000414     C                   eval      AR6va3 = $TASva3
181600000414     C                   eval      AR6imv = $TASimv
181601120103     c                   if        $tasnft>0
181602120103     c                   if        $tasdft>§qtdsa
181700000414     C                   eval      AR6ali = §QTali
181701120103     c                   else
181702120103     C                   eval      AR6ali = §QTalo
181703120103     c                   endif
181704120103     c                   endif
181800000414     C                   eval      AR6cei = $TASfei
181900000414     C                   eval      AR6fiv = $TASfiv
182000000414     C                   eval      AR6nft = $TASnft
182100000414     C                   eval      AR6dft = $TASdft
182200000414      *   calcolo l'importo
182300000426     C                   clear                   FNLV16DS
182400000414     c                   IF        §TBTPO = 'F'
182500000414     c                   eval      D17PRP = 'S'
182600000414     c                   ENDIF
182700000414     c                   eval      D17tla = 'L'
182800000414     c                   eval      D17tbo = '0'
182900000414     c                   eval      D17crb = 'S'
183000000414     c                   eval      D17aas = $TASaas
183100000414     c                   eval      D17lns = $TASlnp
183200000414     c                   eval      D17nrs = $TASnrs
183300000414     c                   eval      D17nsp = $TASnsp
183400000414     c                   eval      D17tbs = $TAStbl
183500000414     c                   eval      D17fim = 'F'
183600000414     c                   CALL      'FNLV16R'
183700000414     c                   PARM                    FNLV16DS
183800000414     c                   PARM                    DTASV
183900000414     C                   eval      AR6ift = O17ift
184000000414      *
184100000414     C                   write     FIAR6000
184200000414     C                   eval      WfileT = 'FIAR600T'
184300000414     C                   eval      WfileR = 'FIAR600R'
184400000414     C                   exsr      AGGnrec
184500000414      *
184600000414     c     KSPE$         setll     TITA7000
184700000414     c     KSPE$         reade     TITA7000
184800000414     c                   DOW       not %EOF
184900000414     c                   eval      AR7aas = TA7aas
185000000414     c                   eval      AR7lnp = TA7lnp
185100000414     c                   eval      AR7nrs = TA7nrs
185200000414     c                   eval      AR7nsp = TA7nsp
185300000414     c                   eval      AR7trc = '2'
185400000414     c                   eval      AR7svn = TA7svn
185500000414     c                   eval      AR7van = TA7van
185600000414     C                   write     FIAR7000
185700000414     C                   eval      WfileT = 'FIAR700T'
185800000414     C                   eval      WfileR = 'FIAR700R'
185900000414     C                   exsr      AGGnrec
186000000414      *
186100000414     c     KSPE$         reade     TITA7000
186200000414     c                   ENDDO
186300000414      *
186400000414     c                   ENDSL
186500000414      *
186600000414     c                   endsr
186700030110      *---------------------------------------------------------------*
186800030110      *  Aggiorno file "estensione testata bolla" da trasmettere      *
186900030110      *---------------------------------------------------------------*
187000030110     c     srFIAR5       BEGSR
187100030110      *
187200030110      * Lettura file FIAR5
187300040531     c     kspe          setll     FIAR531C
187400040531     c     kspe          reade     FIAR531C
187500030110      *
187600040531do  1c                   DOW       not %EOF(FIAR531C)
187700030110      *
187800030110      * controllo il tipo record per verificare che sia da tresmettere
187900030110     c                   clear                   dAR5
188000030110     c                   eval      XX = 1
188100030110     c     AR5trd        lookup    AR5k(xx)                               31
188200030110      *
188300030110      * se il record è da trasmettere: scrivo FIAR500T
188400030110if  2c                   If        *in31
188500030110     c                   write     FIAR5T
188600030110     c                   eval      WfileT = 'FIAR500T'
188700030110     c                   eval      WfileR = 'FIAR500R'
188800030110     c                   exsr      AGGnrec
188900030110e   2c                   Endif
189000030110      *
189100040531     c     kspe          reade     FIAR531C
189200030110      *
189300030110e   1c                   ENDDO
189400030110      *
189500030110     c                   ENDSR
189600000404      *****************************************************************
189700000404      *  AGGANCIO ANAGRAFICHE
189800000404      *****************************************************************
189900000410     C     AGGnrec       BEGSR
190000000404      *
190100000410     c                   eval      xx = 1
190200000412     C     WfileT        lookup    filet(xx)                              31
190300000410      *
190400000410     C                   IF         *in31 = *on
190500000410     c                   add       1             rcdt(xx)
190600000410     c                   ELSE
190700000410     C     *blanks       lookup    filet(xx)                              31
190800000410     C                   IF         *in31 = *on
190900000412     c                   movel     WfileT        fileT(xx)
191000000412     c                   movel     WfileR        fileR(xx)
191100000412     c                   add       1             rcdT(xx)
191200000410     C                   ENDIF
191300000410     C                   ENDIF
191400000410      *
191500000410     c                   endsr
191600000410      *****************************************************************
191700000410      *  AGGANCIO ANAGRAFICHE
191800000410      *****************************************************************
191900000410     C     CTR_ANAGRA    BEGSR
192000000410      *
192100000404     C                   CALL      'TIBS69R'
192200000404     C                   PARM                    tibs69DS
192300000404     C                   PARM                    DS_cnaco
192400000404     C                   PARM                    DS_cnind
192500000404     C                   PARM                    DS_cnclp
192600000404     C                   PARM                    DS_fncls
192700000404      *
192800000404     C                   eval      *IN30 = (O69ERR =  '1')
192900000404      *
193000000404      * per il 1° giro è inizializzata nelle specifiche "D"
193100000404     C                   clear                   TIBS69DS
193200000404      *
193300000404     C                   ENDSR
193400000404      *--------------------------------------------------------------------------------------------*
193500000404      * Gestione file di trasmissione in partenza
193600000404      *--------------------------------------------------------------------------------------------*
193700000405     C     GESfiltrpa    BEGSR
193800000404      *
193900000404      * chiudo i files
194000000404     C                   exsr      closepfpar
194100000404      *
194200000413     c                   Do        Wnumele       XX
194300000404      *
194400000404      * controlla se esiste il membro
194500000404     C                   MOVEL(P)  cmpCHK(xx)    comman
194600000404     C                   CALL      'QCMDEXC'                            20
194700000404     C                   PARM                    comman
194800000404     C                   PARM                    lenght
194900000404      *
195000000404      * inesistente, aggiunge un membro nuovo
195100000404     C                   if        *in20
195200000404     C                   MOVEL(P)  cmpADD(xx)    comman
195300000404     C                   CALL      'QCMDEXC'
195400000404     C                   PARM                    comman
195500000404     C                   PARM                    lenght
195600000404     C                   endif
195700000404      *
195800000404      * in ogni caso, esegue l'ovrdbf
195900000404     C                   MOVEL(P)  cmpOVR(xx)    comman
196000000404     C                   CALL      'QCMDEXC'
196100000404     C                   PARM                    comman
196200000404     C                   PARM                    lenght
196300000404      *
196400000404      * alloco il file
196500000404     C                   MOVEL(P)  cmpALC(xx)    comman
196600000404     C                   CALL      'QCMDEXC'                            91
196700000404     C                   PARM                    comman
196800000404     C                   PARM                    lenght
196900000404      *
197000000404      * Se file già allocato disalloco quelli allocati e vado a fine pgm
197100000404     c                   If        *in91 =*on
197200000404      *
197300000404     c                   If        XX > 1
197400000404     c                   sub       1             XX
197500000404     c                   Do        XX            YY
197600000404     C                   MOVEL(P)  cmpDLC(xx)    comman
197700000404     C                   CALL      'QCMDEXC'                            20
197800000404     C                   PARM                    comman
197900000404     C                   PARM                    lenght
198000000404     c                   Enddo
198100000404     c                   Endif
198200000404      *
198300000412     c                   LEAVE
198400000404     c                   Endif
198500000404      *
198600000412     C                   ENDDO
198700000412      *
198800000404      * Apro i files
198900000412     c  N91              exsr      OPENpfpar
199000000404      *
199100000404     C     endtrpa       ENDSR
199200000404      *--------------------------------------------------------------------------------------------*
199300000404      * Gestione file di trasmissione in arrivo
199400000404      *--------------------------------------------------------------------------------------------*
199500000405     C     GESfiltrar    BEGSR
199600000404      *
199700000404      * chiudo i files
199800000404     C                   exsr      closepfarr
199900000404      *
200000000413     c                   Do        Wnumele       XX
200100000404      *
200200000404      * controlla se esiste il membro
200300000404     C                   MOVEL(P)  cmaCHK(xx)    comman
200400000404     C                   CALL      'QCMDEXC'                            20
200500000404     C                   PARM                    comman
200600000404     C                   PARM                    lenght
200700000404      *
200800000404      * inesistente, aggiunge un membro nuovo
200900000404     C                   if        *in20
201000000404     C                   MOVEL(P)  cmaADD(xx)    comman
201100000404     C                   CALL      'QCMDEXC'
201200000404     C                   PARM                    comman
201300000404     C                   PARM                    lenght
201400000404     C                   endif
201500000404      *
201600000404      * in ogni caso, esegue l'ovrdbf
201700000404     C                   MOVEL(P)  cmaOVR(xx)    comman
201800000404     C                   CALL      'QCMDEXC'
201900000404     C                   PARM                    comman
202000000404     C                   PARM                    lenght
202100000404      *
202200000404      * alloco il file
202300000404     C                   MOVEL(P)  cmaALC(xx)    comman
202400000404     C                   CALL      'QCMDEXC'                            91
202500000404     C                   PARM                    comman
202600000404     C                   PARM                    lenght
202700000404      *
202800000404      * Se file già allocato disalloco quelli allocati e vado a fine pgm
202900000404     c                   If        *in91 =*on
203000000404      *
203100000404     c                   If        XX > 1
203200000404     c                   sub       1             XX
203300000404     c                   Do        XX            YY
203400000404     C                   MOVEL(P)  cmaDLC(xx)    comman
203500000404     C                   CALL      'QCMDEXC'                            20
203600000404     C                   PARM                    comman
203700000404     C                   PARM                    lenght
203800000404     c                   Enddo
203900000404     c                   Endif
204000000404      *
204100000412     c                   LEAVE
204200000404     c                   Endif
204300000412      *
204400000412     C                   ENDDO
204500000404      *
204600000404      * Apro i files
204700000412     c  N91              exsr      openpfarr
204800000404      *
204900000404     C     endtrar       ENDSR
205000020304      *--------------------------------------------------------------------------------------------*
205100020925      * Gestione file di trasmissione msg e fivab
205200020304      *--------------------------------------------------------------------------------------------*
205300020304     c     gesfilvab     begsr
205400020304
205500020304      * chiudo il file
205600020304     c                   exsr      closepfvab
205700020304      *
205800020304
205900020304      * controlla se esiste il membro
206000020304     c                   movel(p)  cmdvab(1)     comman
206100020827     c                   eval      %subst(comman:56:6) = wmbrvab
206200020304     c                   call      'QCMDEXC'                            20
206300020304     c                   parm                    comman
206400020304     c                   parm                    lenght
206500020304
206600020304      * inesistente, aggiunge un membro nuovo
206700020304     c                   if        *in20
206800020304     c                   movel(p)  cmdvab(2)     comman
206900020827     c                   eval      %subst(comman:56:6) = wmbrvab
207000020304     c                   call      'QCMDEXC'
207100020304     c                   parm                    comman
207200020304     c                   parm                    lenght
207300020304     c                   endif
207400020304
207500020304      * in ogni caso, esegue l'ovrdbf
207600020304     c                   movel(p)  cmdvab(3)     comman
207700020827     c                   eval      %subst(comman:56:6) = wmbrvab
207800020304     c                   call      'QCMDEXC'
207900020304     c                   parm                    comman
208000020304     c                   parm                    lenght
208100020304
208200020304      * alloco il file
208300020304     c                   movel(p)  cmdvab(4)     comman
208400020827     c                   eval      %subst(comman:54:6) = wmbrvab
208500020304     c                   call      'QCMDEXC'                            91
208600020304     c                   parm                    comman
208700020304     c                   parm                    lenght
208800020304
208900020304      * Se file già allocato disalloco ed esco dal programma
209000020304     c                   if        *in91
209100020304     c                   movel(p)  cmdvab(5)     comman
209200020827     c                   eval      %subst(comman:54:6) = wmbrvab
209300020304     c                   call      'QCMDEXC'                            20
209400020304     c                   parm                    comman
209500020304     c                   parm                    lenght
209600020304     c                   endif
209700020304
209800020304      * Apro il file
209900020304     c  n91              exsr      openpfvab
210000020304
210100020304     c                   endsr
210200000404      *--------------------------------------------------------------------------------------------*
210300000404      * Chiudo file per partenza
210400000404      *--------------------------------------------------------------------------------------------*
210500000404     c     CLOSEpfpar    BEGSR
210600000404      *
210700000404     c                   close     fnblp00t
210800060221     c                   close     fiar400t
210900051116     c                   close     fIar900t
211000000404     c                   close     fnblt00t
211100000404     c                   close     fiar600t
211200000404     c                   close     fiar700t
211300040831     c****               close     fnevb00t
211400041122     c****               close     fnlbl00t
211500030110     c                   close     fiar500t
211600000404      *
211700000404     c                   ENDSR
211800000404      *--------------------------------------------------------------------------------------------*
211900000404      * apro file per partenza
212000000404      *--------------------------------------------------------------------------------------------*
212100000404     c     OPENpfpar     BEGSR
212200000404      *
212300000404     c                   open      fnblp00t
212400060221     c                   open      fiar400t
212500051116     c                   open      fIAR900t
212600000404     c                   open      fnblt00t
212700000404     c                   open      fiar600t
212800000404     c                   open      fiar700t
212900040831     c****               open      fnevb00t
213000041122     c****               open      fnlbl00t
213100030110     c                   open      fiar500t
213200000404      *
213300000404     c                   ENDSR
213400000404      *--------------------------------------------------------------------------------------------*
213500000404      * Chiudo file per arrivo
213600000404      *--------------------------------------------------------------------------------------------*
213700000404     c     CLOSEpfarr    BEGSR
213800000404      *
213900000404     c                   close     fnarb00t
214000060221     c                   close     fiar400t
214100051116     c                   close     fiar900t
214200000404     c                   close     fnart00t
214300000404     c                   close     fiar600t
214400000404     c                   close     fiar700t
214500040831     c*****              close     fnevb00t
214600041122     c*****              close     fnlbl00t
214700030110     c                   close     fiar500t
214800000404      *
214900000404     c                   ENDSR
215000000404      *--------------------------------------------------------------------------------------------*
215100000404      * apro file per arrivo
215200000404      *--------------------------------------------------------------------------------------------*
215300000404     c     OPENpfarr     BEGSR
215400000404      *
215500000404     c                   open      fnarb00t
215600060221     c                   open      fiar400t
215700051116     c                   open      fiar900t
215800000404     c                   open      fnart00t
215900000404     c                   open      fiar600t
216000000404     c                   open      fiar700t
216100040831     c****               open      fnevb00t
216200041122     c****               open      fnlbl00t
216300030110     c                   open      fiar500t
216400000404      *
216500000404     c                   ENDSR
216600020304      *--------------------------------------------------------------------------------------------*
216700020925      * Chiudo file fivab
216800020304      *--------------------------------------------------------------------------------------------*
216900020304     c     closepfvab    begsr
217000020304
217100020925     c                   close     fivabbft
217200020304
217300020304     c                   endsr
217400020304      *--------------------------------------------------------------------------------------------*
217500020925      * apro file fivab
217600020304      *--------------------------------------------------------------------------------------------*
217700020304     c     openpfvab     begsr
217800020304
217900020925     c                   open      fivabbft
218000020304
218100020304     c                   endsr
218200000323      *--------------------------------------------------------------------------------------------*
218300000323      * Imposto la librerie in tutte le schiere
218400000323      *--------------------------------------------------------------------------------------------*
218500000323     c     impolib       BEGSR
218600020301      *Se il nome della libreria è lungo 10 OK se lungo 9 mettiamo
218700020301      *il ' ' nella 1° posizione a sinis.
218800020301     c                   if        %subst(wlibfile: 10: 1) =' '
218900020301     c                   movel     wlibfile      com09             9
219000020301     c                   clear                   wlibfile
219100020301     c                   move      com09         wlibfile
219200020301     c                   end
219300000323     c                   eval      XX = 1
219400000413     c                   DO        Wnumele       xx
219500020211     c                   eval      %subst(cmpCHK(xx):15:10) = wlibfile
219600020211     c                   eval      %subst(cmpADD(xx):15:10) = wlibfile
219700020211     c                   eval      %subst(cmpOVR(xx):15:10) = wlibfile
219800020211     c                   eval      %subst(cmpALC(xx):15:10) = wlibfile
219900020211     c                   eval      %subst(cmpDLC(xx):15:10) = wlibfile
220000020211     c                   eval      %subst(cmaCHK(xx):15:10) = wlibfile
220100020211     c                   eval      %subst(cmaADD(xx):15:10) = wlibfile
220200020211     c                   eval      %subst(cmaOVR(xx):15:10) = wlibfile
220300020211     c                   eval      %subst(cmaALC(xx):15:10) = wlibfile
220400020211     c                   eval      %subst(cmaDLC(xx):15:10) = wlibfile
220500000412     c                   ENDDO
220600020304
220700020304     c                   DO        5             xx
220800020304     c                   eval      %subst(cmdvab(xx):15:10) = wlibfile
220900020304     c                   ENDDO
221000020304
221100000323      *
221200000323     C                   ENDSR
221300000328      *--------------------------------------------------------------------------------------------*
221400000328      * Operazioni finali
221500000328      *--------------------------------------------------------------------------------------------*
221600000412     C     Chiusure      BEGSR
221700000328      *
221800020827     C                   clear                   TIBS55DS
221900000328     C                   MOVEL     'C'           i50tla
222000020827     C                   CALL      'TIBS55R'
222100020827     C                   PARM                    TIBS55DS
222200000328      *
222300000328     C                   clear                   TIBS69DS
222400000328     C                   eval      I69TLA  = 'C'
222500000328     C                   CALL      'TIBS69R'
222600000328     C                   PARM                    TIBS69DS
222700000328      *
222800000328     C                   ENDSR
222900000322      *--------------------------------------------------------------------------------------------*
223000000407      * Routine iniziale
223100000322      *--------------------------------------------------------------------------------------------*
223200980828     C     *inzsr        BEGSR
223300000322      *
223400980828     C     *entry        PLIST
223500980828     C                   PARM                    kpjba
223600000322      *
223700000322     C                   Z-ADD     1             CODUT
223800980915     C                   CALL      'X§PARUT'
223900000412     C                   PARM                    UT§DSE0F
224000980915     C                   MOVEL     REC80         CNCR80
224100000330      * spedizione
224200000330     c     kspe          klist
224300000327     c                   kfld                    TASaas
224400000327     c                   kfld                    TASlnp
224500000327     c                   kfld                    TASnrs
224600000327     c                   kfld                    TASnsp
224700000410      * spedizione 2a bolla
224800000412     c     kspe$         klist
224900000410     c                   kfld                    $TASaas
225000000410     c                   kfld                    $TASlnp
225100000410     c                   kfld                    $TASnrs
225200000410     c                   kfld                    $TASnsp
225300000330      * Tita4
225400000330     c     kta4          klist
225500000330     c                   kfld                    TASaas
225600000330     c                   kfld                    TASlnp
225700000330     c                   kfld                    TASnrs
225800000330     c                   kfld                    TASnsp
225900000330     c                   kfld                    Ktrc
226000981008      * legame bolla spedizione
226100981008     c     klbl          klist
226200000322     c                   kfld                    rigaas
226300000322     c                   kfld                    riglnp
226400000322     c                   kfld                    rignrs
226500000322     c                   kfld                    rignsp
226501120530      *
226502120530     c     klbl2         klist
226503120530     c                   kfld                    WLaas
226504120530     c                   kfld                    WLlnp
226505120530     c                   kfld                    WLnrs
226506120530     c                   kfld                    WLnsp
226600000323      *
226700000323     c     klblA         klist
226800000323     c                   kfld                    WAaas
226900000323     c                   kfld                    WAlnp
227000000323     c                   kfld                    WAnrs
227100000323     c                   kfld                    WAnsp
227200000412      *
227300000419     c     KBOLw         klist
227400000412     c                   kfld                    Waas
227500000412     c                   kfld                    Wlnp
227600000412     c                   kfld                    Wnrs
227700000412     c                   kfld                    Wnsp
227800000412      *
227900000407     C     kTAB          KLIST
228000000407     C                   KFLD                    CODUT
228100000407     C                   KFLD                    KCOD
228200000322      *
228300000404     C     keytab        KLIST
228400000404     C                   KFLD                    CODUT
228500000404     C                   KFLD                    KCOD
228600000404     C                   KFLD                    KKEY
228700000322      *
228800000413      * Imposto numero elementi in schiera
228900000413     C                   eval      Wnumele = %elem(cmpchk)
229000000413      *
229100000322      * reperimento librerie job
229200020827     C                   CLEAR                   TIBS55DS
229300980828     C                   MOVEL     knsif         i50psi
229400020827     C                   CALL      'TIBS55R'
229500020827     C                   PARM                    TIBS55DS
229600000322      *
229700000323      * La libreria dei file di trasmissione è quella di gruppo, la imposto
229800000323      *  in tutte le schiere
229900981222     c                   movel     o50plg        wlibfile
230000000323     c                   exsr      impolib
230100000322      *
230200000322      * calcola la data corrente
230300000323     C                   TIME                    w0140                          *ora(6) + data(8)
230400000322     C                   MOVE      w0140         w0080                          *data(8) in g/m/aa
230500000322      * UDATE IN AAAAMMGG
230600000323     C     *eur          MOVEL     W0080         Wdata
230700000322     C     *iso          MOVEL     Wdata         datcor
230800000322      *
230900000407      * Carico codici bolla da convertire
231000000407     c                   clear                   xx
231100000407     C                   eval      kcod = '3A'
231200000407     c     KTAB          setll     tabel
231300000407     c     KTAB          reade     tabel
231400000407     C                   DOW       not %EOF
231500000412     c                   If        TBLflg = *blanks
231600000407     C                   add       1             xx
231700000417     c                   movel     TBlkey        T3aK(xx)
231800000417     c                   movel     TBLuni        T3aD(xx)
231900000407     c                   Endif
232000000412     c     KTAB          reade     tabel
232100000407     C                   ENDDO
232200000413      *
232300000413      * Carico tipi bolla
232400000413     c                   clear                   xx
232500000413     C                   eval      kcod = 'TB'
232600000413     c     KTAB          setll     tabel
232700000413     c     KTAB          reade     tabel
232800000413     C                   DOW       not %EOF
232900000413     c                   If        TBLflg = *blanks
233000000413     C                   add       1             xx
233100000417     c                   movel     TBlkey        TblK(xx)
233200000417     c                   movel     TBLuni        TblD(xx)
233300000413     c                   Endif
233400000413     c     KTAB          reade     tabel
233500000413     C                   ENDDO
233600000412      *
233700000412      * Aggancio tabella filiali IVA di sede
233800000412     C                   clear                   dsqc2
233900000412     C                   eval      KCOD = 'QC'
234000000412     C                   movel(P)  '2'           KKEY
234100000412     C     keytab        chain     TABEL
234200000412     c                   IF        %found
234300000412     C                   movel     TBLUNI        DSQC2
234400000412     c                   ENDIF
234500000412      *
234600000412      * Aggancio tabella campi di tassazione standard
234700000419     C                   clear                   dsqt1
234800000412     C                   eval      KCOD = 'QT'
234900000412     C                   movel(P)  '1'           KKEY
235000000412     C     keytab        chain     TABEL
235100000412     c                   IF        %found
235200000412     C                   movel     TBLUNI        DSQT1
235300000412     c                   ENDIF
235400030110      *
235500030110      * Carico codici Tipi Record di FIAR500F
235600030110      * (solo se prevedono la trasmissione in partenza e/o in arrivo)
235700030110     c                   clear                   xx
235800030110     c                   eval      TBEcod = 'AR5'
235900030110     c     TBEcod        setll     TNTBE000
236000030110     c     TBEcod        reade     TNTBE000
236100030110     c                   DOW       not %EOF(TNTBE01L)
236200030110     c                   If        TBEatb = *blanks
236300030110     c                   movel     TBEuni        dAR5
236400030110     c                   If        §AR5rigb = 'S'
236500030110     c                   add       1             xx
236600030110     c                   movel     TBEke1        AR5k(xx)
236700030110     c                   Endif
236800030110     c                   Endif
236900030110     c     TBEcod        reade     TNTBE000
237000030110     c                   ENDDO
237100000322      *
237200980828     C                   ENDSR
237300000323      *--------------------------------------------------------------------------------------------*
237400000324**   cmpCHK - comandi
237500020211CHKOBJ    OBJ(xxxxxxxxxx/FNBLP00T) OBJTYPE(*FILE) MBR(SEDE)                         1
237600060221CHKOBJ    OBJ(xxxxxxxxxx/FIAR400T) OBJTYPE(*FILE) MBR(SEDE)                         1
237700051116CHKOBJ    OBJ(xxxxxxxxxx/FIAR900T) OBJTYPE(*FILE) MBR(SEDE)                         1
237800020211CHKOBJ    OBJ(xxxxxxxxxx/FNBLT00T) OBJTYPE(*FILE) MBR(SEDE)                         1
237900030110CHKOBJ    OBJ(xxxxxxxxxx/FIAR500T) OBJTYPE(*FILE) MBR(SEDE)                         1
238000020211CHKOBJ    OBJ(xxxxxxxxxx/FIAR600T) OBJTYPE(*FILE) MBR(SEDE)                         1
238100020211CHKOBJ    OBJ(xxxxxxxxxx/FIAR700T) OBJTYPE(*FILE) MBR(SEDE)                         1
238200000324**   cmaCHK - comandi
238300120607CHKOBJ    OBJ(xxxxxxxxxx/FNARB00T) OBJTYPE(*FILE) MBR(SEDEA)                        1
238400120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR400T) OBJTYPE(*FILE) MBR(SEDEA)                        1
238500120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR900T) OBJTYPE(*FILE) MBR(SEDEA)                        1
238600120607CHKOBJ    OBJ(xxxxxxxxxx/FNART00T) OBJTYPE(*FILE) MBR(SEDEA)                        1
238700120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR500T) OBJTYPE(*FILE) MBR(SEDEA)                        1
238800120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR600T) OBJTYPE(*FILE) MBR(SEDEA)                        1
238900120607CHKOBJ    OBJ(xxxxxxxxxx/FIAR700T) OBJTYPE(*FILE) MBR(SEDEA)                        1
239000000324**   cmpADD - comandi
239100020211ADDPFM   FILE(xxxxxxxxxx/FNBLP00T)                MBR(SEDE)                         2
239200060221ADDPFM   FILE(xxxxxxxxxx/FIAR400T)                MBR(SEDE)                         2
239300051116ADDPFM   FILE(xxxxxxxxxx/FIAR900T)                MBR(SEDE)                         2
239400020211ADDPFM   FILE(xxxxxxxxxx/FNBLT00T)                MBR(SEDE)                         2
239500030110ADDPFM   FILE(xxxxxxxxxx/FIAR500T)                MBR(SEDE)                         2
239600020211ADDPFM   FILE(xxxxxxxxxx/FIAR600T)                MBR(SEDE)                         2
239700020211ADDPFM   FILE(xxxxxxxxxx/FIAR700T)                MBR(SEDE)                         2
239800000324**   cmaADD - comandi
239900120607ADDPFM   FILE(xxxxxxxxxx/FNARB00T)                MBR(SEDEA)                        2
240000120607ADDPFM   FILE(xxxxxxxxxx/FIAR400T)                MBR(SEDEA)                        2
240100120607ADDPFM   FILE(xxxxxxxxxx/FIAR900T)                MBR(SEDEA)                        2
240200120607ADDPFM   FILE(xxxxxxxxxx/FNART00T)                MBR(SEDEA)                        2
240300120607ADDPFM   FILE(xxxxxxxxxx/FIAR500T)                MBR(SEDEA)                        2
240400120607ADDPFM   FILE(xxxxxxxxxx/FIAR600T)                MBR(SEDEA)                        2
240500120607ADDPFM   FILE(xxxxxxxxxx/FIAR700T)                MBR(SEDEA)                        2
240600000324**   cmpOVR - comandi
240700020211OVRDBF TOFILE(xxxxxxxxxx/FNBLP00T) FILE(FNBLP00T) MBR(SEDE)                         3
240800060221OVRDBF TOFILE(xxxxxxxxxx/FIAR400T) FILE(FIAR400T) MBR(SEDE)                         3
240900051116OVRDBF TOFILE(xxxxxxxxxx/FIAR900T) FILE(FIAR900T) MBR(SEDE)                         3
241000020211OVRDBF TOFILE(xxxxxxxxxx/FNBLT00T) FILE(FNBLT00T) MBR(SEDE)                         3
241100030110OVRDBF TOFILE(xxxxxxxxxx/FIAR500T) FILE(FIAR500T) MBR(SEDE)                         3
241200020211OVRDBF TOFILE(xxxxxxxxxx/FIAR600T) FILE(FIAR600T) MBR(SEDE)                         3
241300020211OVRDBF TOFILE(xxxxxxxxxx/FIAR700T) FILE(FIAR700T) MBR(SEDE)                         3
241400000324**   cmaOVR - comandi
241500120607OVRDBF TOFILE(xxxxxxxxxx/FNARB00T) FILE(FNARB00T) MBR(SEDEA)                        3
241600120607OVRDBF TOFILE(xxxxxxxxxx/FIAR400T) FILE(FIAR400T) MBR(SEDEA)                        3
241700120607OVRDBF TOFILE(xxxxxxxxxx/FIAR900T) FILE(FIAR900T) MBR(SEDEA)                        3
241800120607OVRDBF TOFILE(xxxxxxxxxx/FNART00T) FILE(FNART00T) MBR(SEDEA)                        3
241900120607OVRDBF TOFILE(xxxxxxxxxx/FIAR500T) FILE(FIAR500T) MBR(SEDEA)                        3
242000120607OVRDBF TOFILE(xxxxxxxxxx/FIAR600T) FILE(FIAR600T) MBR(SEDEA)                        3
242100120607OVRDBF TOFILE(xxxxxxxxxx/FIAR700T) FILE(FIAR700T) MBR(SEDEA)                        3
242200000324**   cmpALC - comandi
242300020211ALCOBJ   OBJ((xxxxxxxxxx/FNBLP00T  *FILE   *EXCL   (SEDE)))                         5
242400060221ALCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDE)))                         5
242500051116ALCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDE)))                         5
242600020211ALCOBJ   OBJ((xxxxxxxxxx/FNBLT00T  *FILE   *EXCL   (SEDE)))                         5
242700030110ALCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDE)))                         5
242800020211ALCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDE)))                         5
242900020211ALCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDE)))                         5
243000000324**   cmaALC - comandi
243100120607ALCOBJ   OBJ((xxxxxxxxxx/FNARB00T  *FILE   *EXCL   (SEDEA)))                        5
243200120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDEA)))                        5
243300120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDEA)))                        5
243400120607ALCOBJ   OBJ((xxxxxxxxxx/FNART00T  *FILE   *EXCL   (SEDEA)))                        5
243500120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDEA)))                        5
243600120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDEA)))                        5
243700120607ALCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDEA)))                        5
243800000324**   cmpDLC - comandi
243900020211DLCOBJ   OBJ((xxxxxxxxxx/FNBLP00T  *FILE   *EXCL   (SEDE)))                         6
244000060221DLCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDE)))                         6
244100051116DLCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDE)))                         6
244200020211DLCOBJ   OBJ((xxxxxxxxxx/FNBLT00T  *FILE   *EXCL   (SEDE)))                         6
244300030110DLCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDE)))                         6
244400020211DLCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDE)))                         6
244500020211DLCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDE)))                         6
244600000324**   cmaDLC - comandi
244700120607DLCOBJ   OBJ((xxxxxxxxxx/FNARB00T  *FILE   *EXCL   (SEDEA)))                        6
244800120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR400T  *FILE   *EXCL   (SEDEA)))                        6
244900120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR900T  *FILE   *EXCL   (SEDEA)))                        6
245000120607DLCOBJ   OBJ((xxxxxxxxxx/FNART00T  *FILE   *EXCL   (SEDEA)))                        6
245100120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR500T  *FILE   *EXCL   (SEDEA)))                        6
245200120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR600T  *FILE   *EXCL   (SEDEA)))                        6
245300120607DLCOBJ   OBJ((xxxxxxxxxx/FIAR700T  *FILE   *EXCL   (SEDEA)))                        6
245400020304**   cmdvab - comandi
245500020925CHKOBJ    OBJ(xxxxxxxxxx/FIVABBFT) OBJTYPE(*FILE) MBR(Mxxxxxx)                      1
245600020925ADDPFM   FILE(xxxxxxxxxx/FIVABBFT)                MBR(Mxxxxxx)                      2
245700021009OVRDBF TOFILE(xxxxxxxxxx/FIVABBFT) FILE(FIVABBFT) MBR(Mxxxxxx)                      3
245800020925ALCOBJ   OBJ((xxxxxxxxxx/FIVABBFT  *FILE   *EXCL   (Mxxxxxx)))                      4
245900020925DLCOBJ   OBJ((xxxxxxxxxx/FIVABBFT  *FILE   *EXCL   (Mxxxxxx)))                      5

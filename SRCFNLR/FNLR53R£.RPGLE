000100120123     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200920914     H DECEDIT('0,') DATEDIT(*DMY.)
000300950321?     *--------------------------------------------------------------*
000400131112?     *      --------  I M P O R T A N T I S S I M O -------         *
000500070517?     *--------------------------------------------------------------*
000600941115      * FNLR53R *                                                    *
000700930217      *---------*                                                    *
000800930308      *         - GESTIONE CHIUSURA DISTINTA -                       *
000900911028      *--------------------------------------------------------------*
001000941115     FFNLR53D   CF   E             WORKSTN
001100941123     F                                     SFILE(LR53S02:NRR2)
001200941123     F                                     SFILE(LR53S03:NRR3)
001300941123     F                                     SFILE(LR53S06:NRR6)
001400941123     F                                     SFILE(LR53S07:NRR7)
001500911028     FTABEL00F  IF   E           K DISK
001600990923     FTNTBE01L  IF   E           K DISK
001700070319     FAZORG01L  IF   e           k DISK
001800050406     Ftigcp21L  IF   E           K DISK
001900060412     FyfQIG01L  IF   E           K DISK
002000071026     Ffiarp01L  uF a E           K DISK    usropn
002100070313     F*****MM                              COMMIT
002200941123     FFNARB01L  UF   E           K DISK    USROPN
002300930420     F                                     COMMIT
002400941123     FFNART01L  UF   E           K DISK    USROPN
002500930402     F                                     COMMIT
002600060214     FFiAR401L  UF A E           K DISK    USROPN
002700941115     F                                     COMMIT
002800990924     FFIARI01L  UF A E           K DISK    USROPN
002900990924     F                                     COMMIT
003000051115     FFIAR901L  UF   E           K DISK    USROPN
003100941123     F                                     COMMIT
003200911029     FCNABI01L  IF   E           K DISK
003300020823     FFNFVV01L  UF   E           K DISK    USROPN
003400930402     F                                     COMMIT
003500070410     Ffidst09L  UF   E           K DISK    USROPN
003600070316     F                                     COMMIT
003700990519     FFNANM02L  IF   E           K DISK    USROPN
003800960418     F                                     INFDS(FLAN02)
003900990519     FFNANM01L  IF   E           K DISK    USROPN
004000990519     F                                     RENAME(FNANM000:FNANM1)
004100960418     F                                     INFDS(FLAN01)
004200941115     FCNASS00F  O  A E             DISK    USROPN
004300930617     F                                     COMMIT
004400060323     Fcnass01L  uF   E           K DISK    prefix(£)
004500060323     F                                     RENAME(cnass000:cnass1)
004600060323     F                                     COMMIT
004700990803     FFNEVB01L  UF A E           K DISK    USROPN
004800931029     F                                     COMMIT
004900070312     FFNEVB20f  UF A E           K DISK    USROPN
005000070312     F                                     COMMIT
005100011121     Ffiftt01L  UF A E           K DISK    USROPN
005200940414     F                                     COMMIT
005300060203     Ffiftd22L  UF A E           K DISK    USROPN
005400940414     F                                     COMMIT
005500980402     FFNCDS02L  IF   E           K DISK    USROPN
005600980402     F                                     COMMIT
005700051027     FFIARS01L  IF   E           K DISK    USROPN
005800980402     F                                     COMMIT
005900970926     FFNSTP10F  UF A E             DISK    USROPN
006000040311     F******* ES/AB del 11/03              COMMIT
006100960116     FFNARBX0F  O  A E             DISK    USROPN
006200960115     F                                     COMMIT
006300050516     Ffiapd01L  IF   E           K DISK
006400080715     Ffidsf02L  IF   E           K DISK
006500120418     Ffidsf00f  o    E             DISK    commit
006600120418     f                                     rename(fidsf000:fidsf00)
006700080714     Ffnbrv01L  IF   E           K DISK
006800080805     Ffipcd03L  IF   E           K DISK
006900080806     Ffipct02L  IF   E           K DISK
007000140609     Ffipdc03t  IF   E           K DISK
007100130722     ffiar501l  uf a e           k disk    commit
007200060323     Ffiinc04l  uF   E           K DISK    commit
007300110208     Ffiilv00f  o    E             DISK    commit
007400071026     Fwfvarb1l  uF a E           K DISK
007500131014     Ffiarbf2c  iF   E           K DISK    ignore(FNARBD00)
007600071026     f                                     ignore(FNARBK00)
007700071026     f                                     ignore(FIARBT00)
007800071026     f                                     ignore(FNARBP00)
007900071026     f                                     ignore(FNARBM00)
008000071026     f                                     ignore(FNARBV00)
008100131014     f                                     ignore(FNARBN00)
008200071026     f                                     prefix($)
008300120518     Ffncss00f  uf a E             DISK    commit
008400120518     Ffncss01l  iF   E           K DISK    prefix(£) infds(infdscss)
008500120302     F                                     RENAME(fncss000:fncss1)
008600120305     Ffncss04l  iF   E           K DISK    prefix($)
008700120305     F                                     RENAME(fncss000:fncss4)
008800120302     Ftncsm03l  iF   E           K DISK
008900131121     Ffnevb04l  iF   E           K DISK    rename(fnevb000:fnevb4)
009000141212ab    * Controllo RA aperte Testata e Dettaglio
009100141212ab   Ffitgd11l  iF   E           K DISK
009200141212ab   Ffitgr01l  iF   E           K DISK
009300141212      *
009400941123     FQSYSPRT   O    F  132        PRINTER INFDS(STP1)
009500941123     F                                     OFLIND(*INOF)
009600941123     F                                     USROPN
009700940317      *----------------------------------------------------------------
009800931029     D SNT             S              7  0 DIM(5)                               NUM.SPED.NON CONS.
009900961212     D STP             S              3  0 DIM(999)                             STOP
010000950918     D STT             S             66    DIM(6) CTDATA PERRCD(1)              STAMPA
010100940415     D CMDP            S              1    DIM(65) CTDATA PERRCD(65)
010200130416     D ERR             S             70    DIM(109) CTDATA PERRCD(1)
010300000211     D MSGT            S             70    DIM(1) CTDATA PERRCD(1)
010400000211     D MSG             S             66    DIM(3) CTDATA PERRCD(1)
010500941123      *----------------------------------------------------------------
010600080903     D FLV             S              1    DIM(200)                             SKI TAB. CV
010700080903     D CCV             S              3    DIM(200)                             SKI CHIAVI CV
010800130416     D D1A             S             89    DIM(200)                             SKI TAB. 1A
010900080903     D C1A             S              2    DIM(200)                             SKI CHIAVI 1A
011000080903     D D2A             S             89    DIM(200)                             SKI TAB. 2A
011100080903     D D2Z             S             89    DIM(200)                             SKI TAB. 2Z
011200080903     D C2A             S              3    DIM(200)                             SKI CHIAVI 2A
011300141128     D D3A             S             89    DIM(200)                             SKI TAB. 3A
011400080903     D C3A             S              2    DIM(200)                             SKI CHIAVI 3A
011500141128     D D7R             S             89    DIM(200)                             SKI TAB. 7R
011600080903     D C7R             S              2    DIM(200)                             SKI CHIAVI 7R
011700141128     D D7O             S             89    DIM(200)                             SKI TAB. 7O
011800080903     D C7O             S              1    DIM(200)                             SKI CHIAVI 7O
011900080903     D C7M             S              1    DIM(200)                             SKI CHIAVI 7O
012000141128     D D1P             S             89    DIM(200)                             SKI TAB. 1P
012100080903     D C1P             S              1    DIM(200)                             SKI CHIAVI 1P
012200000211     D C7U             S              7  0 DIM(500)                             COD.CLIENTI 7U
012300000211     D F7U             S              1    DIM(500)                             SKI CHIAVI 7U
012400080903     D NST             S              3  0 DIM(200)                             N.STOP X DIST.
012500080903     D SPS             S              7  0 DIM(200)                             N.SPED X STOP
012600120518     D infdscss        DS
012700120518     D  nrrcss               397    400B 0
012800140609     D ddoc          E DS
012900140609     d  comflg                41     46
013000950619     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
013100950619     D  DTU                  848    895P 0
013200950619     D                                     DIM(12)                              DATE PARAM
013300950619     D                                     PACKEVEN
013400071026     D fiarpds       E DS                  EXTNAME(Fiarp00f)
013500071026     D LR533         E DS                  EXTNAME(FNLR53DS3)
013600061107     D fnlrz1ds      E DS                  inz
013700141212ab   D fnlrq0ds      E DS                  inz
013800120228     D tnsc30ds      E DS                  inz
013900120528     D dilvlibero    E DS                  inz
014000970120     D FNLR53        E DS                  EXTNAME(FNLR53DS)
014100030624     D  ND1                    1     35  0 DIM(5)                               NUM.DISTINTA
014200030624     D  DTD                   36     75  0 DIM(5)                               DATA DISTINTA
014300030624     D  DDC                   76    115  0 DIM(5)                               DATA CHIUSURA
014400030624     D  HRC                  116    135  0 DIM(5)                               ORA CONSEGNA
014500030624     D  FPP                  136    140    DIM(5)                               FLAG PRESTAZ.
014600030624     D  PEP                  141    145    DIM(5)                               PICKING
014700030624     D  SET                  146    180  0 DIM(5)                               NUM.SPED.DIST.
014800030624     D  CAR                  247    251    DIM(5)                               Carico S/N
014900941123     D STP1            DS
015000941123     D  STPLI1               367    368B 0
015100951205     D* Parametri passati al pgm di stampa bolla
015200030624     D fnlsb5ds      E DS
015300941115      * DS per richiamo pgm FNLR36R
015400941123     D DSLR36          DS
015500941115     D  P36AAS                14     17  0
015600941115     D  P36LNP                18     20  0
015700941115     D  P36NRS                21     22  0
015800941115     D  P36NSP                23     29  0
015900941115     D  P36F03                30     30
016000941115     D  P36FLG                31     31
016100911028     D KPJBA         E DS
016200911028     D  LIBSYS                92    100
016300911028     D  MSGERR               453    502
016400030718     D trul90ds      E DS
016500071030     D KPJBAs        E DS                  extname(kpjba) prefix(s)
016600911028      *
016700141212ab   D dTGR01        E DS
016800941123     D DSCV          E DS
016900941123     D DSTM          E DS
017000941123     D DS1A          E DS
017100941123     D DS2A          E DS
017200131120     D DS2Acep       E DS                  extname(ds2a) prefix(w)
017300051215     D DS2Atat       E DS                  extname(ds2a) prefix(x)
017400101123     D DS2Atur       E DS                  extname(ds2a) prefix(y)
017500941123     D DS2Z          E DS
017600941123     D DS3A          E DS
017700921015     D DS1P          E DS
017800941123     D DS7O          E DS
017900960227     D DS7U          E DS
018000110908     D DS7v          E DS
018100941123     D DS7R          E DS
018200030624     D  TPG                   22     26    DIM(5)                               TIP. GIACENZA
018300030624     D  NTC                   27     31  0 DIM(5)                               NR.TENTAVI CONS.
018400030624     D  EVE                   32     46    DIM(5)                               COD.EVENTO
018500030624     D  TMC                   47     51    DIM(5)                               TIP.NO CONSE.
018600030624     D  VOL                   52     56    DIM(5)                               VOLTE
018700990923     D DGEI          E DS
018800990924     D DGED          E DS
018900020418     D Ddgd          E DS
019000040330     D Dvpo          E DS
019100011121     d dgedcn        e ds
019200011011     D DSCW          E DS
019300060310     D ycow0120s     E DS
019400140207     D trtcp2ds      E DS
019500140207     D fnevb2ds      E DS                  EXTNAME(fnevb20f)
019600140207     D YEURDS        E DS                  EXTNAME(YEURCODS)
019700941115     D FNLR48        E DS                  EXTNAME(FNLR48DS)
019800961120     D FNLR63        E DS                  EXTNAME(FNLR63DS)
019900020822     D ddatiute      e ds
020000020822     D azuteds       e ds                  extname(AZUTE00F)
020100020822     D tibs34ds      E DS                  inz
020200140210     D fnlrn6ds      E DS
020300070319     D fidn12ds      E DS
020400080604     D truldesds     E DS                  inz
020500070319     D dsbl4l        E DS
020600941115     D*
020700941115     D DSARBD        E DS
020800941115     D DSARBG        E DS
020900991129     D DARBT         E DS
021000941115     D DSARBK        E DS
021100950112     D DSTB70        E DS                  EXTNAME(DS7PQRS)
021200030127     d ds7l          e ds
021300080709     d dy4o          e ds
021400050225     d fnlrf8ds      e ds
021500080723     d fipcdcedds    e ds
021600080806     d fipctcetds    e ds
021700080808     d dar5ban       e ds
021800130723     d dar5cog       e ds
021900140306     d dar5Gen       e ds
022000060322     D cnc0wa00s     e ds                  prefix(£)
022100120814     d darbflrx      e ds
022200131203     d fidg46ds      e ds
022300140225     d dftt01        e ds
022400140404     d fidna6ds      e ds
022500140404      *
022600140404     d TRULVPODS     e ds
022700140404     d   sk_PDA               16    765    inz   dim(250)
022800911028      *
022900941115     D WLBDA8          DS
023000941115     D  G02DAT                 1      8  0
023100941115     D  G02INV                 9     16  0
023200941115     D  G02ERR                17     17
023300941115     D  G02TGI                18     22  0
023400930407      *
023500930407     D WLBGIO          DS
023600941115     D  GIODAT                 1      8  0
023700941115     D  GIOINV                 9     16  0
023800930407     D  GIOTGI                13     17  0
023900020418      *
024000020418     d Wdata8          DS
024100020418     d  dadata                 1      8  0
024200020418     d  adata                  9     16  0
024300020418     d  GioLav                17     21  0
024400911030      *
024500930202     D                 DS
024600030624     D  NDC                    1     35  0 DIM(5)                               NUM.DISTINTA
024700941115     D  V2CND1                 1      7  0
024800941115     D  V2CND2                 8     14  0
024900941115     D  V2CND3                15     21  0
025000941115     D  V2CND4                22     28  0
025100941115     D  V2CND5                29     35  0
025200930331     D                 DS
025300000211     D  W1GC                   1      2
025400930331     D  DSG1                   1      1
025500930331     D  DSMP1                  2      2
025600911028      *
025700930331     D                 DS
025800930331     D  W2GC                   1      2
025900930331     D  DSG2                   1      1
026000930331     D  DSMP2                  2      2
026100000211     D DSMSG           DS
026200000211     D  KCODM                 25     26
026300960418     D DSLR33        E DS                  EXTNAME(FNLR33DS)
026400000821      * UTILIZZO LA DSLR01 PER PARAMETRI A "FNLR96R" (INVIO SPUNTE 4/8)
026500000821     D DSLR01        E DS
026600020717     D OG143         E DS
026700070319     D OG148         E DS
026800070405     D OG146         E DS
026900000526     D FVVDS         E DS                  EXTNAME(FNFVV00F)
027000960418     D FLAN02          DS
027100960418     D  ANMNR2               397    400B 0
027200960418     D FLAN01          DS
027300960418     D  ANMNR1               397    400B 0
027400131106     c*
027500131106     d dEVB01        e ds
027600131106     c*
027700941124     D                SDS
027800941124     D  NOMPGM                 1     10
027900070412     d dataeur         s               d   datfmt(*eur)
028000040330     d dataiso         s               d   datfmt(*iso)
028100131108     d woramin         s               t   timfmt(*iso)
028200060320     d savtce          s                   like(v05tce)
028300060320     d savtae          s                   like(v05tae)
028400060320     d savpos          s                   like(v05pos)
028500060321     d comresto        s                   like(v05resto)
028600071030     d timesav         s                   like(times)
028700071031     d oggisav         s                   like(oggi)
028800120228     d exopz           s                   like(csskey)
028900120308     d comaas          s                   like(cssaas)
029000120308     d comlnp          s                   like(csslnp)
029100120308     d comnrs          s                   like(cssnrs)
029200120308     d comnsp          s                   like(cssnsp)
029300120308     d comkey          s                   like(csskey)
029400120308     d comnrr          s                   like(nrr3)
029500071030     d com$arb         s             14  0
029600071030     d comvarb         s             14  0
029700120418     d com04           s              4  0
029800140326     d wAR5PDACO       s                   like(§AR5PDACO)
029900101123      * - Parametri per pgm. XGIOSE1
030000101123     d Wggmmaa         s              6    inz
030100101123     d Wgioset         s              1    inz
030200101123      *
030300101123     d wData_6         s              6  0 inz
030400101123     d wData_Dmy       s               d   inz  datfmt(*dmy)
030500101123      *
030600060302     D F3              C                   X'33'
030700060302     D F6              C                   X'36'
030800060302     D F12             C                   X'3C'
030900941227      *---------------------------------------------------------------*
031000941227     D CMAT            C                   CONST('MATTINO   ')
031100941227     D CPOM            C                   CONST('POMERIGGIO')
031200941115      *----------------------------------------------------------------
031300970926     IFNSTP100
031400970926     I              STPATB                      FTPATB
031500970926     I              STPTSR                      FTPTSR
031600970926     I              STPAAS                      FTPAAS
031700970926     I              STPLNP                      FTPLNP
031800970926     I              STPNRS                      FTPNRS
031900970926     I              STPNSP                      FTPNSP
032000970926     I              STPSET                      FTPSET
032100970926     I              STPINV                      FTPINV
032200911028      *---------------------------------------------------------------*
032300911028      * FLUSSO PRINCIPALE                                             *
032400911028      *---------------------------------------------------------------*
032500941123      *  Operazioni iniziali
032600941124     C     *ENTRY        PLIST
032700941124     C                   PARM                    KPJBA
032800030718     C                   PARM                    trul90ds
032900941124     C                   MOVEL     KPJBU         FNLR53
033000050516     c                   move      d53dsf        w53pdc
033100080715     c                   move      d53pda        wpda              1
033200140404     c* ricerca P.O. abilitati da tabella VPO per step di test
033300140516     c*                  clear                   trulvpods
033400140516     c*                  eval      ivpoke1 = 'DECOFI816F17'
033500140516     c*                  call      'TRULVPOR'
033600140516     c*                  parm                    trulvpods
033700140516     c*                  move      '999'         d53fgsa
033800140516     c*    d53fgsa       lookup    sk_pda                                 77
033900140516     c* n77              move      d53fgs        d53fgsa           3
034000140516     c* n77d53fgsa       lookup    sk_pda                                 77
034100140516     c*  77              move      '1'           fi816f17          1
034200140516     c* n77              clear                   fi816f17
034300941124      *  Se pgm richiamato per l'ultima volta chiudo in LR
034400941124     C     D53TRI        IFNE      'U'
034500941124     C                   EXSR      DEFCAM
034600941116      *  Inizializzo Subfile hidden x dettaglio segnacolli
034700941116     C                   EXSR      INZS07
034800941115      *  Gestione videate
034900941115     C     WFINE         DOUEQ     'S'
035000941124     C     WTPVID        CASEQ     '2'           GESS02                         Subfile Sped.
035100060303     C     WTPVID        CASEQ     'P'           srprepagati                    Subfile Impo.
035200060303     C     WTPVID        CASEQ     '3'           GESS03                         Subfile Impo.
035300941124     C     WTPVID        CASEQ     '5'           GESD05                         Totale
035400941115     C                   END
035500941115     C                   END
035600941115     C*
035700941115     C*  controllo se devo eseguire  aggiornamento archivi + Stampa
035800941115     C     WAGGIO        IFEQ      'S'
035900941115     C                   EXSR      AGGIOR
036000960228     C                   END
036100031008     c                   clear                   pretes
036200941115     C*
036300941123     C     FINE          TAG
036400970926      *  SE HO APERTO FNSTP10T LO CHIUDO
036500950221     C     CNTPDR        IFEQ      'N'
036600970926     C                   CLOSE     FNSTP10F
036700950221     C                   ENDIF
036800950221      *
036900941115     C                   COMMIT
037000941115     C                   MOVEL     FNLR53        KPJBU
037100950221     C                   SETON                                        RT
037200941124     C                   ELSE
037300040505     C***  WRICH1        IFEQ      'S'
037400040505     C***                MOVEL(P)  'KSDIFFE'     KQIEB
037500040505     C***                MOVEL     'LR27'        KCOAZ
037600040505     C***                CALL      'BCH10'
037700040505     C***                PARM                    KPJBA
037800040505     C***                CLEAR                   KQIEB
037900040505     C***                END
038000960418     C                   MOVEL     'C'           D33TLA
038100960418     C                   CALL      'FNLR33R'
038200960418     C                   PARM                    DSLR33
038300970227     C     WRISTA        IFEQ      'S'
038400970227     C                   MOVEL     'C'           SA2TLA            1
038500970227     C                   MOVEL     'C'           SA2TRC            1
038600970228     C                   Z-ADD     CONDIS        SA2DTS            8 0
038700970227     C                   Z-ADD     D53LNA        SA2LNA            3 0
038800970227     C                   CALL      'FNLSA2R3'
038900970227     C                   PARM                    SA2TLA            1
039000970227     C                   PARM                    SA2TRC            1
039100970227     C                   PARM                    SA2DTS            8 0
039200970227     C                   PARM                    SA2LNA            3 0
039300970227     C                   END
039400941124     C                   SETON                                        LR
039500941124     C                   END
039600941116     C*-----------------------------------------------------------*
039700941116     C* Inizilzzazipone Subfile dettaglio segnacolli              *
039800941116     C*-----------------------------------------------------------*
039900941123?    C     INZS07        BEGSR
040000941116     C*
040100941116     C                   SETON                                        23
040200941116     C                   WRITE     LR53C07
040300941116     C                   Z-ADD     0             NRR7
040400941116     C                   Z-ADD     0             TOTSFH
040500941116     C                   SETOFF                                       23
040600941116     C*
040700941116?    C                   ENDSR
040800941115      *---------------------------------------------------------*
040900941115      *  Gestione della prima videata                           *
041000941115      *---------------------------------------------------------*
041100941116     C     GESS02        BEGSR
041200941115      *
041300030902     c     esci          tag
041400941124     C                   WRITE     LR53Z02
041500941115     C                   EXFMT     LR53C02
041600941115     C                   SETOFF                                       28
041700941115     C*  Ritorno
041800941115     C     *INKL         IFEQ      '1'
041900030902     c                   if        kcdaz = 'XR53' and verifica = *blank
042000030902     c                   eval      $msg = 'Premendo F12 andranno perse le -
042100030902     c                             letture effettuate con la pistola'
042200030902     c                   move      '1'           verifica          1
042300030902     c                   seton                                        28
042400030902     C                   GOTO      esci
042500030902     c                   endif
042600030902     c                   move      ' '           verifica
042700941115     C                   MOVEL     'S'           WFINE
042800950321     C     D53ITA        IFEQ      'E'
042900950321     C                   MOVEL     '1'           D53F03
043000950322     C                   COMMIT
043100950321     C                   END
043200120308     c* se premuto f12 verifico se devo cancellare i multiassegni
043300120615     c                   do        5             i
043400120615     c                   if        ndc(i) > 0
043500120615     c                   exsr      delassdis
043600120615     c                   endif
043700120615     c                   enddo
043800120308     c* se premuto f12 verifico se ci sono state variazioni bolle per
043900071026     c* riportare indietro la situazione
044000071026     c                   exsr      srback
044100941116     C                   GOTO      FINVD2
044200941115     C                   END
044300131203     C*  Interrogazione eventi PDA
044400131203     C     *INKE         IFEQ      '1'
044500131203     C                   CLEAR                   fidg46ds
044600131203     c                   if        c2csrrrn > 0
044700131203     c     c2csrrrn      chain     lr53s02
044800131203     c                   move      v1clna        DG46FGSI
044900131203     c                   z-add     Vnmdoc        DG46NDCI
045000131203     c                   move      Vh2pdr        DG46CODAUT
045100131203     c                   move      v2caas        DG46AASI
045200131203     c                   move      V2CLNP        DG46LNPI
045300131203     c                   move      V2CNRS        DG46NRSI
045400131203     c                   move      V2CNSP        DG46NSPI
045500131203     c                   move      'S'           dg46canc
045600131203     c                   endif
045700131203     C                   MOVEL     FIDG46DS      KPJBU
045800131203     C                   CALL      'FIDG49R'
045900131203     C                   PARM                    KPJBA
046000131206     C                   MOVEL     kpjbu         FIDG46DS
046100131206     c                   if        dg46canc = 'C'
046200131206     c                   z-add     0             vh2hco
046300131206     c                   z-add     0             vh2mco
046400131206     c                   clear                   VH2EVCEP
046500131206     c                   update    lr53s02
046600131206     c                   endif
046700131203     C                   GOTO      FINVD2
046800131203     C                   END
046900941115     C*  Interrogazione bolle
047000941115     C     *INKG         IFEQ      '1'
047100941115     C                   CLEAR                   DSLR36
047200131203     c                   if        c2csrrrn > 0
047300131203     c     c2csrrrn      chain     lr53s02
047400131203     c                   move      v2caas        p36aas
047500131203     c                   move      V2CLNP        p36LNP
047600131203     c                   move      V2CNRS        p36NRS
047700131203     c                   move      V2CNSP        p36NSP
047800131203     c                   move      '1'           p36flg
047900131203     c                   endif
048000941115     C                   MOVEL     DSLR36        KPJBU
048100941115     C                   CALL      'FNLR36R'
048200941115     C                   PARM                    KPJBA
048300941116     C                   GOTO      FINVD2
048400941115     C                   END
048500140909      * verifico se per l'aut non sono stati fissati dei giorni limite
048600140909      * fuori standard primi 2 byte del campo APDFLR
048700140909     c     kapd          chain     fiapd01l
048800140909     c                   clear                   como2n
048900140909     c                   clear                   como2
049000140909     c                   if        apdflr <> *blank
049100140909     c                   movel     apdflr        como2             2
049200140909     c                   endif
049300140909     C                   TESTN                   como2                95
049400140909     C   95              MOVE      como2         como2n            2 0
049500941115     C*  Controlli
049600941116     C                   EXSR      CTR02
049700941115     C*  Conferma
049800941115     C     *INKF         IFEQ      '1'
049900941115     C     *IN28         ANDEQ     '0'
050000941130     C                   XFOOT     SET           V05TSC
050100941130     C                   XFOOT     SNT           V05TSN
050200941130     C                   SUB       V05TSN        V05TSC
050300060303     C*  chiamo il pgm d'incasso prepagati nel caso ce ne siano
050400060303     C                   MOVEL     'P'           WTPVID
050500060303     C                   END
050600941115     C*
050700941116     C     FINVD2        ENDSR
050800941115      *---------------------------------------------------------*
050900941115      *  Gestione della prima videata                           *
051000941115      *---------------------------------------------------------*
051100941116     C     GESS03        BEGSR
051200941115      *
051300941124     C                   WRITE     LR53Z03
051400941115     C                   EXFMT     LR53C03
051500941115     C                   SETOFF                                       28
051600941115     C*  Ritorno
051700941115     C     *INKL         IFEQ      '1'
051800060303     c                   if        wtrpre = 'S'
051900060303     C                   MOVEL     'P'           WTPVID
052000060323     c                   exsr      srdelpre
052100060303     c                   else
052200941124     C                   MOVEL     '2'           WTPVID
052300060303     c                   end
052400941116     C                   GOTO      FINVD3
052500941115     C                   END
052600941115     C*  Interrogazione bolle
052700941115     C     *INKG         IFEQ      '1'
052800941115     C                   CLEAR                   DSLR36
052900941115     C                   MOVEL     DSLR36        KPJBU
053000941115     C                   CALL      'FNLR36R'
053100941115     C                   PARM                    KPJBA
053200941116     C                   GOTO      FINVD3
053300941115     C                   END
053400941115     C*  Controlli
053500941116     C                   EXSR      CTR03
053600941115     C*  Conferma
053700941115     C     *INKF         IFEQ      '1'
053800941205     C     *IN28         ANDEQ     '0'
053900120302     C*  richiamo pgm gestione scrittura file per multiassegno e multispedizione
054000120615     c                   do        5             i
054100120615     c                   if        ndc(i) > 0
054200120615     C                   EXSR      srinsert
054300120615     C   28              LEAVE
054400120615     c                   endif
054500120615     c                   enddo
054600120306     c                   if        not *in28
054700060303     C                   MOVE      '5'           WTPVID
054800060302     C                   END
054900120306     C                   END
055000941115     C*
055100941116     C     FINVD3        ENDSR
055200941115      *---------------------------------------------------------*
055300060302      *  Gestione dei prepagati                                 *
055400941115      *---------------------------------------------------------*
055500060302     C     srprepagati   BEGSR
055600060302     c*
055700060302     c                   clear                   ycow0120s
055800060302     c                   eval      OPCODE = 'DACHIUDIST'
055900060302     c                   eval      FGS  = d53fgs
056000100128     c                   eval      POR  = d53fgs
056100060302     c                   eval      NDC1 = nd1(1)
056200060302     c                   eval      NDC2 = nd1(2)
056300060302     c                   eval      NDC3 = nd1(3)
056400060302     c                   eval      NDC4 = nd1(4)
056500060302     c                   eval      NDC5 = nd1(5)
056600060320     c                   eval      CMTCTL = *on
056700060324     c                   eval      setonlr= *on
056800060320     c                   eval      STRCMTCTL =*off
056900060320     c                   eval      COMMIT = *off
057000060302     c                   movel(p)  ycow0120s     kpjbu
057100060302     c                   call      'YCOW0120R'
057200060302     c                   parm                    kpjba
057300060320     c                   movel     kpjbu         ycow0120s
057400060303     c* premuto conferma o non esistono prepagati quindi procedo
057500060303     c                   if        RTNCMD = F6 or rtncode ='NODATA'
057600060320     c                   if        rtncmd = F6
057700060320     c                   eval      wtrpre = 'S'
057800060320     c                   else
057900060320     c                   eval      wtrpre = 'N'
058000060320     c                   end
058100060303     C*  Se ci sono dei C/Assegni o degli assegnati vado al
058200060303     C*  subfile degli importi
058300060303     C     WTRIMP        IFEQ      'S'
058400060303     C                   MOVEL     '3'           WTPVID
058500060303     C                   ELSE
058600060303     C                   EXSR      AZZTOT
058700060303     C                   MOVE      '5'           WTPVID
058800060303     C                   END
058900060303     c                   end
059000060303     c* premuto ritorno
059100060320     c                   if        RTNCMD = F12 or rtncmd = F3
059200060303     C                   MOVEL     '2'           WTPVID
059300060303     C                   END
059400060302     C*
059500060302     C                   ENDSR
059600060302      *---------------------------------------------------------*
059700060323      *  Gestione f12: cancello gli incassi dei prepagati       *
059800060302      *---------------------------------------------------------*
059900060323     C     srdelpre      BEGSR
060000060323     C*
060100060323     c                   if        sessione <> 0
060200060323     c     sessione      setll     fiinc04l
060300060323     c                   do        *hival
060400060323     c     sessione      reade     fiinc04l
060500060323     c                   if        %eof(fiinc04l)
060600060323     c                   leave
060700060323     c                   end
060800060323     c     kass          chain     cnass01l
060900060323     c                   if        %found(cnass01l)
061000060323     c                   delete    cnass1
061100060323     c                   end
061200060323     c                   delete    fiinc000
061300060323     c                   enddo
061400060323     c                   end
061500060323     C*
061600060323     C                   ENDSR
061700120305      *---------------------------------------------------------*
061800120305      *  Gestione f12: cancello le spedizioni dei multiassegni  *
061900120305      *---------------------------------------------------------*
062000120308     C     delassdis     BEGSR
062100120305     C*
062200120316     c                   clear                   okagg             1
062300120305     c                   if        d53fgs = 0
062400120305     c                   z-add     d53lna        $cssfgs
062500120305     c                   else
062600120305     c                   z-add     d53fgs        $cssfgs
062700120305     c                   end
062800120615     c                   z-add     ndc(i)        NDCI              7 0
062900120305     c     kdst4         setll     fncss04l
063000120305     c                   do        *hival
063100120305     c     kdst4         reade     fncss04l
063200120305     c                   if        %eof(fncss04l)
063300120305     c                   leave
063400120305     c                   end
063500120316     c                   eval      okagg = '1'
063600120307     c* se ho già scritto anche assegni chiamo il pgm per annullare
063700120305     c                   clear                   tnsc30ds
063800120307     c                   eval      sc30cmti = 'F'
063900120305     c                   eval      sc30AASI = $cssaao
064000120305     c                   eval      sc30LNPI = $csslno
064100120305     c                   eval      sc30NRSI = $cssnro
064200120305     c                   eval      sc30NSPI = $cssnso
064300120305     c                   eval      sc30AANI = $cssaas
064400120305     c                   eval      sc30LPNI = $csslnp
064500120305     c                   eval      sc30NRNI = $cssnrs
064600120305     c                   eval      sc30NSNI = $cssnsp
064700120305     c                   eval      SC30TPCI = 'A'
064800120305     c                   eval      kpjbu  = tnsc30ds
064900120305     c                   call      'TNSC30R'
065000120305     c                   parm                    kpjba
065100120305     c                   eval      kpjbu = tnsc30ds
065200120305     c                   if        sc30msgo <> ' '
065300120305     C                   SETON                                        40  28
065400120306     C                   MOVEL     sc30msgo      $MSG
065500120305     c                   leavesr
065600120305     c                   end
065700120305     c                   enddo
065800120313     c* aggiorno tutto il sfl dove presenti dei legami
065900120316     c                   if        okagg = '1'
066000120308     c                   clear                   comaas
066100120308     c                   clear                   comlnp
066200120308     c                   eval      comnrs = *hival
066300120308     c                   clear                   comnsp
066400120308     c                   eval      comkey = *hival
066500120308     c                   exsr      sraggsfl
066600120316     c                   end
066700120308     C*
066800120308     C                   ENDSR
066900080715     c*________________________________________________________________
067000080715     c     insbancali    begsr
067100080715     c*________________________________________________________________
067200100118     c* se la distinta è pda verifico se la fase CRI è stata inserita
067300100118     c* da AS 400, in quel caso la procedura sarà manuale
067400100118     c                   if        wpda = 'S'
067500080715     c     kdsf          chain     fidsf02l
067600080715     c                   if        %found(fidsf02l) and DSFPDAAS4 = 'A'
067700100118     c                   else
067800100118     c                   leavesr
067900100118     c                   endif
068000100118     c                   endif
068100100118     c*
068200080715     c                   move      'E'           rf8mod
068300100118     c                   z-add     d53fgs        rf8fgs
068400080715     c                   z-add     wdsfndc       rf8dis
068500080715     C                   movel     fnlrf8ds      KPJBU
068600080715     C                   CALL      'FNLRF8R'
068700080715     C                   PARM                    KPJBA
068800080715     c                   endsr
068900060323      *---------------------------------------------------------*
069000060323      *  Gestione della prima videata                           *
069100060323      *---------------------------------------------------------*
069200060323     C     GESD05        BEGSR
069300060302      *
069400060303     c* aggiungo importi prepagati
069500060303     c                   if        wtrpre = 'S'
069600060303     c                   exsr      caltotpre
069700060303     c                   end
069800060303     c*
069900941115     C                   EXFMT     LR53D05
070000991008     C                   SETOFF                                       284041
070100941115     C*  Ritorno
070200941115     C     *INKL         IFEQ      '1'
070300941124     C     WTRIMP        IFEQ      'S'
070400941124     C                   MOVEL     '3'           WTPVID
070500941124     C                   ELSE
070600060303     c* se ci sono prepagati
070700060303     c     wtrpre        ifeq      'S'
070800060303     C                   MOVEL     'P'           WTPVID
070900060323     c                   exsr      srdelpre
071000060303     C                   ELSE
071100060303     C                   MOVEL     '2'           WTPVID
071200941124     C                   END
071300060323     C                   END
071400941116     C                   GOTO      FINVD5
071500060320     C                   END
071600941115     C*  Prima videata
071700941115     C     *INKS         IFEQ      '1'
071800941115     C                   MOVEL     'S'           WFINE
071900060502     c     wtrpre        ifeq      'S'
072000060502     c                   exsr      srdelpre
072100060502     C                   END
072200120305     c     wtrimp        ifeq      'S'
072300120615     c                   do        5             i
072400120615     c                   if        ndc(i) > 0
072500120615     c                   exsr      delassdis
072600120615     c                   endif
072700120615     c                   enddo
072800120305     C                   END
072900950323     C     D53ITA        IFEQ      'E'
073000950323     C                   MOVEL     '2'           D53F03
073100950323     C                   COMMIT
073200950323     C                   END
073300941116     C                   GOTO      FINVD5
073400941115     C                   END
073500060322     c* dettaglio contante
073600060322     c                   if        *inki
073700060322     c                   clear                   cnc0wa00s
073800060426     c                   eval      £IMPTOTALE = v05tce
073900060322     c                   eval      £DIVISA ='EUR'
074000060322     c                   eval      kpjbu = cnc0wa00s
074100060322     c                   call      'CNC0WA00R'
074200060322     c                   parm                    kpjba
074300060322     c                   eval      cnc0wa00s = kpjbu
074400060322     c                   if        £RTNCMD = F6 and
074500060322     c                             £RTNCODE ='DONE' and
074600060322     c                             £IMPIMMESSO > 0
074700060322     c                   eval      v05tec = £IMPIMMESSO
074800060322     c                   end
074900060322     c                   end
075000991007     C* Controllo se modificati totali
075100991007     C                   EXSR      CTR05
075200951205     C*  Conferma
075300941115     C     *INKF         IFEQ      '1'
075400941115     C     *IN28         ANDEQ     '0'
075500941115     C                   MOVE      'S'           WFINE
075600941115     C                   MOVE      'S'           WAGGIO
075700100121     c* no  bancali per estero
075800100121     c                   if        d53ita <> 'E'
075900100118     C                   clear                   fnlrf8ds
076000050225     c*richiamo pgm per inserimento bancali a fine chiusura
076100050225      *viene chiamato una volta per ogni distinta che si sta chiudendo
076200050225     c                   if        v2cnd1 > 0
076300080715     c                   z-add     v2cnd1        wdsfndc
076400080715     c                   exsr      insbancali
076500050225     c                   endif
076600050225     c                   if        v2cnd2 > 0
076700080715     c                   z-add     v2cnd2        wdsfndc
076800080715     c                   exsr      insbancali
076900050225     c                   endif
077000050225     c                   if        v2cnd3 > 0
077100080715     c                   z-add     v2cnd3        wdsfndc
077200080715     c                   exsr      insbancali
077300050225     c                   endif
077400050225     c                   if        v2cnd4 > 0
077500080715     c                   z-add     v2cnd4        wdsfndc
077600080715     c                   exsr      insbancali
077700050225     c                   endif
077800050225     c                   if        v2cnd5 > 0
077900080715     c                   z-add     v2cnd5        wdsfndc
078000080715     c                   exsr      insbancali
078100050225     c                   endif
078200100121     c                   endif
078300080714
078400941115     C                   END
078500941115     C*
078600941116     C     FINVD5        ENDSR
078700941116      *---------------------------------------------------------*
078800941116      *  Controlli sulla prima videata                          *
078900941116      *---------------------------------------------------------*
079000941116     C     CTR02         BEGSR
079100071026     c                   clear                   fiarpds
079200941116      *  Se ho chiesto la conferma leggo tutti i record e calcolo
079300941116      *  i totali
079400941116     C     *INKF         IFEQ      '1'
079500941207     C                   Z-ADD     *ZEROS        STP
079600941207     C                   Z-ADD     *ZEROS        SNT
079700941123     C                   Z-ADD     *ZEROS        F                 4 0
079800941117     C                   Z-ADD     *ZEROS        V05STP
079900000505     C                   Z-ADD     *ZEROS        V05ADR
080000000505     C                   Z-ADD     *ZEROS        V05NDR
080100110909     C     1             CHAIN     LR53S02                            96
080200941116     C                   ELSE
080300941116     C                   READC     LR53S02                                96
080400941116     C                   END
080500941116     C*  Loop letture
080600941117     C                   Z-ADD     1             NR2KEY
080700941117     C     *IN96         DOWEQ     '0'                                          --- 01 ---
080800941116     C     *IN28         ANDEQ     '0'
080900941116     C                   SETOFF                                       404142
081000941116     C                   SETOFF                                       434445
081100941117     C                   SETOFF                                       464748
081200970311     C                   SETOFF                                       757902
081300110419     C                   SETOFF                                       51
081400030305      *
081500030305      *  controlla la LNA per capire se è un estero oppure no
081600110908     C*  reperisco AZORG della linea di arrivo   1
081700030305     c                   move      'N'           flg_Estero        1
081800030305     C     v2cLNA        CHAIN     AZORG01L                           74
081900030305     C  N74              MOVEL     ORGDE3        OG143
082000030305      * se tratta di LNA estera.
082100030305     C     §OgNTW        ifeq      'FED'
082200030305     C     §OgNTW        oreq      'EEX'
082300030305     C     §OgNTW        oreq      'EUP'
082400030305     c                   move      'S'           flg_Estero
082500030305     C                   End
082600030305      *
082700960214     C     VNMDOC        IFNE      WSVDOC
082800960214     C                   Z-ADD     0             STP
082900960214     C                   Z-ADD     0             F
083000960214     C                   Z-ADD     VNMDOC        WSVDOC            7 0
083100960214     C                   END
083200950112     C     V2CINT        IFEQ      '?'
083300950112     C                   CLEAR                   DSTB70
083400950112     C                   MOVEL     'S'           DS7RIC
083500950112     C                   MOVEL     V2CGMA        DS7COD
083600950112     C                   MOVEL     'V'           DS7GES
083700950112     C                   MOVEL     DSTB70        KPJBU
083800950112     C                   CALL      'TRTB70R'
083900950112     C                   PARM                    KPJBA
084000950112     C                   MOVEL     ' '           V2CINT
084100950112     C                   END
084200110208      * interrogazione bolla su riga
084300110207     C     V2CINT        IFEQ      'I'
084400110207     c                   exsr      richbl
084500110208     C                   MOVEL     ' '           V2CINT
084600110207     C                   END
084700950112     C*
084800941214     C     V2CGMA        COMP      *BLANKS                                08
084900100223     C     V2CCON        comp      'SI'                                   79    --- 02 ---
085000100205     C*  Controllo se immesso codice particolarità
085100100205     C                   CLEAR                   DS7R
085200100205     C     V2CGMA        IFNE      *BLANKS                                      --- 03 ---
085300100205     C                   Z-ADD     1             X
085400100205     C     V2CGMA        LOOKUP    C7R(X)                                 95
085500100205     C     *IN95         IFEQ      '1'
085600100205     C                   MOVEL     D7R(X)        DS7R
085700100205     C                   END
085800100205     C                   END                                                    --- 03 ---
085900141202ab   c******************
086000141202ab   c*   se PDA ='S' **
086100141202ab   c* prende il CET **
086200141202ab   c******************
086300141202ab   c*  aggancia l'ultimo CET presente sulla spedizione se con PDA
086400141202ab   c*   servirà in più punti testare i campi della DS (fipctcetds)
086500141202ab   c                   if           wpda = 'S'
086600141202ab   c                   exsr      Found_CET
086700141202ab   c                   end
086800141202ab   c*
086900100205     c******************
087000100205     c* note
087100100205     c******************
087200100205     C     V02SEL        IFEQ      'V'
087300100205     C                   EXSR      GESRMB
087400100205     C                   MOVEL     ' '           V02SEL
087500100205     C                   Z-ADD     NRR2          NR2KEY
087600100205     C                   END
087700100205     C     V02SEL        IFEQ      'E'
087800100205     C                   Z-ADD     V2CAAS        D63AAS
087900100205     C                   Z-ADD     V2CLNP        D63LNP
088000100205     C                   Z-ADD     V2CNSP        D63NSP
088100100205     C                   Z-ADD     V2CNRS        D63NRS
088200100205     C                   MOVEL     KPJBU         WSVKPJ          256
088300100205     C                   MOVEL     FNLR63        KPJBU
088400100205     C                   CALL      'FNLR63R4'                           98
088500100205     C                   PARM                    KPJBA
088600100205     C                   MOVEL     KPJBU         FNLR63
088700100205     C                   MOVEL     WSVKPJ        KPJBU
088800100205     C                   Z-ADD     NRR2          NR2KEY
088900100205     C                   MOVEL     ' '           V02SEL
089000100205     C     D63TRN        IFNE      'S'
089100100205     C                   EXSR      GESRMB
089200100205     C                   END
089300100205     C                   END
089400100205     c*
089500100205     C                   if        v02sel = 'N'
089600100205     c                   if        §oggeot = 'S'
089700140210     c                   clear                   fnlrn6ds
089800100205     C                   movel     kpjbu         WSVKPJ          256
089900140210     C                   move      'I'           in6mod
090000140210     C                   z-add     V2CAAS        in6AAS
090100140210     C                   z-add     V2CLNP        in6LNP
090200140210     C                   z-add     V2CNRS        in6NRS
090300140210     C                   z-add     V2CNSP        in6NSP
090400140210     C                   movel     'FNLR53R'     in6pgm
090500140210     C                   movel     'S'           IN6CHDIST
090600140210     C                   call      'FNLRN6R'                            98
090700100205     C                   parm                    kpjba
090800140210     C                   parm                    fnlrn6ds
090900100205     C                   movel     WSVKPJ        kpjbu
091000100205     C                   MOVEL     ' '           V02SEL
091100100205     C                   Z-ADD     NRR2          NR2KEY
091200100205     c                   else
091300100205     C                   SETON                                        4228
091400100205     C                   MOVEL     ERR(97)       $MSG
091500100205     C                   GOTO      AGGREC
091600100205     c                   endif
091700100205     c                   endif
091800100205      * nr stop
091900100205     c     v02stp        IFeq      0
092000100205     C                   SETON                                        4128
092100100205     C                   MOVEL     ERR(87)       $MSG
092200100205     C                   GOTO      AGGREC
092300100205     C                   END
092400100205  408 ** il controllo della nota NON deve essere eseguito x la chiusura distinta ESTERO
092500100205  408 ** poichè i partners mandano una infinità di note inutili che bloccano l'operatività
092600100205  408 ** in filiale. (esempio AZKAR a Genova)
092700100205     c                   IF        V02SELB = *BLANK AND v2cnt1 <> *blank
092800100205  408c                             and d53ita <>'E'
092900100205     C                   SETON                                          28
093000100205     C                   MOVEL     ERR(105)      $MSG
093100100205     C                   GOTO      AGGREC
093200100205     C                   END
093300120426  408 ** verifica se particolarità con richiesta di visualizzare la         pre videata con
093400110908      * descrizione della particolarità per dare al cassiere la possibilità di verificare
093500120426      * se l'autista ha fatto quanto richiesto
093600120426     c                   IF        V02SELd = *BLANK AND v2cvpcd ='S'
093700110912  408c                             and d53ita <>'E' and v02ccn = *blank
093800110908     C                   exsr      gesvid7
093900110908     C                   GOTO      AGGREC
094000110908     C                   END
094100100223     c*******************
094200100223     C* RITORNO ALL'INCASSO
094300100223     c*******************
094400100223     C     V2CCON        IFEQ      'SI'                                         --- 02 ---
094500100223     C     V02SEL        IFEQ      'W'
094600100223     C                   SETON                                        4228
094700100223     C                   MOVEL     ERR(65)       $MSG
094800100223     C                   GOTO      AGGREC
094900100223     C                   END
095000100223     C   KF              EXSR      AGGTO2
095100100223     c                   goto      aggrec
095200100223     c                   end
095300950113     C*
095400950113     C     V02CCN        IFNE      V2CSEV
095500950113     C                   MOVEL     V02CCN        V2CSEV
095600950113     C                   MOVEL     *BLANKS       V2CEVP
095700951228     C                   MOVEL     *BLANKS       V2CF08
095800950113     C                   END
095900100205     C*  Controllo la validità della data consegna
096000100205     C                   Z-ADD     VH2DDC        DTCONS
096100100205     C                   MOVEL     VH2DDC        ANNO
096200100205     C                   MOVE      VH2DDC        MMGG              4 0
096300100205     C                   MOVEL     MMGG          MM1               2 0
096400100205     C                   MOVE      V02DTC        MM2               2 0
096500100205     C     MM1           IFEQ      12
096600100205     C     MM2           ANDEQ     1
096700100205     C                   ADD       1             ANNO
096800100205     C                   END
096900100205     C     MM1           IFEQ      1
097000100205     C     MM2           ANDEQ     12
097100100205     C                   SUB       1             ANNO
097200100205     C                   END
097300100205     C     V02DTC        IFNE      0
097400100205     C                   MOVEL     V02DTC        G02DAT
097500100205     C                   MOVE      ANNO          G02DAT
097600100205     C                   MOVEL     *BLANKS       G02ERR
097700100205     C                   CALL      'XSRDA8'
097800100205     C                   PARM                    WLBDA8
097900100205     C     G02ERR        IFEQ      '1'
098000100205     C                   SETON                                        4628
098100100205     C                   MOVEL     ERR(18)       $MSG
098200100205     C                   GOTO      AGGREC
098300100205     C                   END
098400100205     C                   Z-ADD     G02INV        DTCONS
098500100205     C                   END
098600100205     C* Controllo che data consegna non sia inferiore alla data sped.
098700100205     C                   MOVEL     V2CGMS        GG
098800100205     C                   MOVE      V2CGMS        MM
098900100205     C                   MOVE      GG            DAT4
099000100205     C                   MOVEL     MM            DAT4
099100100205     C                   MOVE      DAT4          DTSPED
099200100205     C                   MOVEL     V2CAAS        DTSPED
099300100205     C     DTSPED        IFGT      DTCONS
099400100205     C                   SETON                                        4628
099500100205     C                   MOVEL     ERR(17)       $MSG
099600100205     C                   GOTO      AGGREC
099700100205     C                   END
099800100205     C* controllo se esistono spedizioni con data consegna
099900100205     C* inferiore di un max. di 15 gg. dalla data distinta
100000100205     C     DTCONS        IFLT      VH2DDT
100100100205     C     D53ITA        ANDne     'E'
100200100205     C                   MOVE      DTCONS        VH2DDC
100300100205     C                   SETON                                        4628
100400100205     C                   MOVEL     ERR(19)       $MSG
100500100205     C                   GOTO      AGGREC
100600100205     C                   END
100700100205     C* controllo se esistono date consegna con data maggiore odierna
100800100205     C     DTCONS        IFGT      OGGI
100900100205     C                   SETON                                        4628
101000100205     C                   MOVEL     ERR(21)       $MSG
101100100205     C                   GOTO      AGGREC
101200100205     C                   END
101300100205     C* controllo limiti minimi da tabella dgd per data consegna non per estero
101400100205     c                   if        d53ita = 'I'
101500100205     c                   eval      dadata = dtcons
101600100205     c                   eval      adata  = oggi
101700100205     c                   CALL      'XSRLAV8'
101800100205     c                   PARM                    Wdata8
101900100205      * se l'autista ha una personalizzazione in anagrafico per i giorni
102000100205      * limite da controllare prevale sui valori standard della tabella
102100100205     c                   if        como2n > 0
102200100205     c                   if        giolav > como2n
102300100205     C                   SETON                                        4628
102400100205     C                   MOVEL     ERR(20)       $MSG
102500100205     C                   GOTO      aggrec
102600100205     c                   end
102700100205     c                   else
102800100205      * limiti standard tabellati
102900100205     c                   if        giolav > §dgdgbar
103000100205     C                   SETON                                        4628
103100100205     C                   MOVEL     ERR(20)       $MSG
103200100205     C                   GOTO      AGGREC
103300100205     c                   end
103400100205     c                   end
103500140909     c                   endif
103600100205     C* Controllo primo codice consegna particolare
103700100205     C     '?'           SCAN      V2CCP1                                 95
103800100205     C     *IN95         IFEQ      '1'
103900100205     C                   MOVEL     KKUT          §KUT
104000100205     C                   MOVEL     '1P'          §COD
104100100205     C                   MOVE      *BLANKS       V2CCP1
104200100205     C                   CALL      'X§TABER'
104300100205     C                   PARM                    §KUT
104400100205     C                   PARM                    §COD
104500100205     C                   PARM                    §KEY
104600100205     C                   PARM                    §DES             30
104700100205     C                   MOVEL     §KEY          V2CCP1
104800100205     C                   END
104900100205     C* Controllo nella tabella 1P se esiste e è utilizzabile
105000100205     C     V2CCP1        IFNE      *BLANKS
105100100205     C     V2CCP1        ANDNE     V2HCP1
105200100205     C                   Z-ADD     1             X
105300100205     C                   CLEAR                   DS1P
105400100205     C                   Z-ADD     1             X
105500100205     C     V2CCP1        LOOKUP    C1P(X)                                 95
105600100205     C     *IN95         IFEQ      '1'
105700100205     C                   MOVEL     D1P(X)        DS1P
105800100205     C                   ELSE
105900100205     C                   SETON                                        2844
106000100205     C                   MOVEL     ERR(23)       $MSG
106100100205     C                   GOTO      AGGREC
106200100205     C                   END
106300100205     C                   END
106400100205     C* Controllo secondo codice consegna particolare
106500100205     C     '?'           SCAN      V2CCP2                                 95
106600100205     C     *IN95         IFEQ      '1'
106700100205     C                   MOVEL     KKUT          §KUT
106800100205     C                   MOVEL     '1P'          §COD
106900100205     C                   MOVE      *BLANKS       V2CCP2
107000100205     C                   CALL      'X§TABER'
107100100205     C                   PARM                    §KUT
107200100205     C                   PARM                    §COD
107300100205     C                   PARM                    §KEY
107400100205     C                   PARM                    §DES             30
107500100205     C                   MOVEL     §KEY          V2CCP2
107600100205     C                   END
107700100205     C* Controllo nella tabella 1P se esiste e è utilizzabile
107800100205     C     V2CCP2        IFNE      *BLANKS
107900100205     C     V2CCP2        ANDNE     V2HCP2
108000100205     C                   Z-ADD     1             X
108100100205     C                   CLEAR                   DS1P
108200100205     C                   Z-ADD     1             X
108300100205     C     V2CCP2        LOOKUP    C1P(X)                                 95
108400100205     C     *IN95         IFEQ      '1'
108500100205     C                   MOVEL     D1P(X)        DS1P
108600100205     C                   ELSE
108700100205     C                   SETON                                        2848
108800100205     C                   MOVEL     ERR(23)       $MSG
108900100205     C                   GOTO      AGGREC
109000100205     C                   END
109100100205     C                   END
109200970117     C*
109300941117     C                   MOVEL     V021GC        W1GC
109400941117     C                   MOVEL     V022GC        W2GC
109500101015     C                   z-add     V2cdcr        w13dcrg           8 0
109600100211     C                   move      V2ctcr        w13doil           1
109700941116     C*  Controllo codice di mancata consegna
109800941116     C     '?'           SCAN      V02CCN                                 95
109900941117     C     *IN95         IFEQ      '1'                                          --- 03 ---
110000941116     C                   MOVEL     'GNP*****'    §TIP
110100941116     C                   MOVEL     *BLANKS       V02CCN
110200941116     C                   CALL      'TRUL40R'
110300941116     C                   PARM                    §TIP
110400941116     C                   PARM                    §KEY
110500941116     C                   PARM                    §DES
110600941116     C                   MOVEL     §KEY          V02CCN
110700970901     C                   GOTO      AGGREC
110800941117     C                   END                                                    --- 03 ---
110900970311     C*  Se indicata consegna anomala 5 obbligatorio evento IDD
111000970805     C     V02CCA        IFEQ      '5'
111100970805     C     V2CIDD        ANDEQ     *BLANKS
111200970805     C     V02CCN        ANDEQ     *BLANKS
111300970805     C                   SETON                                        4028
111400970805     C                   MOVEL     ERR(70)       $MSG
111500970805     C                   GOTO      AGGREC
111600970805     C                   END                                                    --- 04 ---
111700941116     C                   CLEAR                   DS2A
111800941123     C                   CLEAR                   DS2Z
111900941123     C                   MOVEL     *BLANKS       WTPGIA
112000100205     C                   clear                   W2zmdr            3
112100100205     c*********************
112200100205     C* MANCATA CONSEGNA
112300100205     c*********************
112400941117     C     V02CCN        IFNE      *BLANKS                                      --- 03 ---
112500000505     C     V2CCP1        COMP      'D'                                    31
112600000505     C  N31V2CCP2        COMP      'D'                                    31
112700000505     C   31              ADD       1             V05NDR
112800941123     C                   Z-ADD     1             Y                 3 0
112900941116     C     V02CCN        LOOKUP    C2A(Y)                                 95
113000941117     C     *IN95         IFEQ      '1'                                          --- 04 ---
113100941116     C                   MOVEL     D2A(Y)        DS2A
113200941116     C                   MOVEL     D2Z(Y)        DS2Z
113300941123     C                   MOVEL     §2ZTIC        WTPGIA
113400040213     C                   MOVEL     §2Zmdr        W2zmdr            3
113500941117     C                   ELSE                                                   --- 04 ---
113600941116     C                   SETON                                        4028
113700941116     C                   MOVEL     ERR(1)        $MSG
113800941117     C                   GOTO      AGGREC
113900941117     C                   END                                                    --- 04 ---
114000131111      *
114100131111 2   C     §2ZUTP        IFEQ      'S'
114200131111     C     C2A(Y)        ANDEQ     V02CCN
114300131111     C                   SETON                                        2840
114400131111     C                   MOVEL     ERR(55)       $MSG
114500131111     C                   GOTO      aggrec
114600131111 e2  C                   END
114700141127ab    *
114800141127ab   C*  Se particolarità di consegna non prevede il PINCODE
114900141127ab   C*   NON si deve dare come Mancata CONSEGNA la Mancanza del PINCODE (044)
115000141127ab   C                   if        §7RpinCODE <> 'S' and
115100141201ab   C                                 V02CCN =  '044'
115200141127ab   C                   SETON                                        4028
115300141127ab   C                   MOVEL     ERR(78)       $MSG
115400141127ab   C                   GOTO      AGGREC
115500141127ab   C                   END
115600141127ab   C*
115700080604      *
115800080604      * verifica se destinatario e indirizzo già frequentati x segnalazione
115900080604      * motivo di non trovato ecc.ecc.
116000080605     c                   if        Vcontrdes = *blank
116100080604     c
116200080604     c                   if        v02ccn = '021'  or
116300080604     c                             v02ccn = '022'
116400080604     c                   clear                   truldesds
116500080604     c                   move      V2CAAS        idsAAS
116600080604     c                   move      V2CLNP        idSLNP
116700080604     c                   move      V2CNRS        idSNRS
116800080604     c                   move      V2CNSP        idSNSP
116900080604     c                   call      'TRULDESR'
117000080604     c                   parm                    truldesds
117100080604     c                   if        odsesito = 'S'
117200080604     c                   eval      $msg = 'Esistono già -
117300080604     c                             spedizioni -
117400080604     c                             consegnate a questo indirizzo ' +
117500080604     c                             odsbolla
117600080605     c                   eval      vcontrdes = 'S'
117700080604     C                   SETON                                        4028
117800080604     C                   GOTO      aggrec
117900080604     c                   endif
118000080604     c                   endif
118100080604     c                   endif
118200080604     c
118300941116     C*  Codice non utilizzabile in chiusura distinta
118400941116     C     §2ATIP        IFNE      'N'
118500941116     C     §2ATIP        ANDNE     'P'
118600941116     C     §2ATIP        ANDNE     'G'
118700941116     C                   SETON                                        4028
118800941116     C                   MOVEL     ERR(2)        $MSG
118900941116     C                   GOTO      AGGREC
119000941116     C                   END
119100950113     C*---------------------------------------------------------------*
119200070405      * se il valore è il medesimo fra immesso e proposto accendo
119300070405      * indicatore per no display essendo una forzatura per gestire
119400070405      * estensione eventi non si deve vedere
119500070405     c                   if        v2cevp = v02ccn and v2cevp <> *blank
119600070405     c                   seton                                        70
119700070405     c                   else
119800070405     c                   setoff                                       70
119900070405     c                   endif
120000070405     C*  Se previsto codice particolarità richiamo routine x relativa
120100070405     C*  gestione forzata riempimento campo evento proposto
120200950113     C                   EXSR      GESPCO
120300051215?     * verifico se bolla con data cons. rich. TASSATIVA
120400051215?     * nel caso non posso immettere un evento di lasciato avviso nemmeno
120500051215?     * come evento proposto
120600051215     c                   clear                   ds2atat
120700051215     c                   z-add     1             tat               3 0
120800170224     c                   if        v2ctat = 'S' and
120900170224     c                             (V2hCp1 = 'S' or V2hCp2 = 'S')
121000051215     c                   select
121100051215?     * evento proposto da particolarità
121200051215     c                   when      v2cevp <> *blank
121300051215     c     v2cevp        lookup    c2a(tat)                               85
121400051215     c   85              movel     d2a(tat)      ds2atat
121500051215     c                   if        x§2aftc = 'A'
121600051215     C                   seton                                        4028
121700051215     C                   movel     err(93)       $msg
121800051215     c                   goto      aggrec
121900051215     c                   endif
122000051215?     * evento forzato legato in tabella 2z
122100051215     c                   when      w2zmdr <> *blank
122200051215     c     w2zmdr        lookup    c2a(tat)                               85
122300051215     c   85              movel     d2a(tat)      ds2atat
122400051215     c                   if        x§2aftc = 'A'
122500051215     C                   seton                                        4028
122600051215     C                   movel     err(93)       $msg
122700051215     c                   goto      aggrec
122800051215     c                   endif
122900051215?     * evento inserito a video
123000051215     c                   other
123100051215     c                   if        §2aftc = 'A'
123200051215     C                   seton                                        4028
123300051215     C                   movel     err(93)       $msg
123400051215     c                   goto      aggrec
123500051215     c                   endif
123600051215     c                   endsl
123700051215     c                   endif
123800051215?     *
123900040213      *data consegna richiesta forza evento se non c'è particolarità
124000100208     c                   if        w2zmdr <> *blank
124100040224     c                             and d53ita <>'E'
124200100208     c                   if        v2cevp = *blank
124300040213     c                   movel     w2zmdr        v2cevp
124400140423     c                   else
124500140423     c                   if        v02ccn = '016' and v2cevp <> 'A16' or
124600140423     c                             v02ccn = '023' and v2cevp <> 'A23'
124700140423     C                   seton                                        4028
124800140423     C                   movel     err(72)       $msg
124900140423     c                   goto      aggrec
125000140423     c                   endif
125100100208     c                   end
125200101015     c                   if        v2cevp = 'A16'
125300120810     c                   move      ' '           w13doil
125400101015     c                   move      ' '           v2ctcr
125500101015     c                   end
125600101015     c                   if        v2cevp = 'A23'
125700120810     c                   move      'D'           w13doil
125800101015     c                   move      'D'           v2ctcr
125900101015     c                   end
126000100308     c                   else
126100100308     c                   clear                   w2zmdr
126200100211     c                   endif
126300070405      * se dopo la particolarità o eventuali forzature eventi proposti per
126400070405      * consegna richiesta il codice evento proposto è ancora a blank forzo
126500070405      * il codice evento immesso a video per agganciare gestione estensione eventi
126600070405     c                   if        v2cevp = *blank
126700070405     c                   move      v02ccn        v2cevp
126800070405     c                   seton                                        70
126900070517      * abblenco anche campo per pilotare segnalazione eventuale FIARP
127000070517     c                   clear                   segnale           1
127100070405     c                   endif
127200101027mm+  c* se non consegnata e l'evento immesso prevede il tentativo di conseg.
127300101027mm+  c                   exsr      srctrora
127400101027mm+  C   47              GOTO      AGGREC
127500101015     c* se pda verifico se immesso evento con data cons. ric. solo la 1°
127600101015     c* volta
127700101015     c                   if        wpda = 'S' and w2zmdr <> ' '
127800101018     c                             and v2hevp = ' '
127900101015     c                   z-add     §PCTDCR       V2CDCR
128000170123     c                   movel     §PCTHCR       V2CHCR
128100101015     c                   if        §pctcmc = '016'
128200101015     c                   move      ' '           v2ctcr
128300101015     c                   end
128400101015     c                   if        §pctcmc = '023'
128500101015     c                   move      'D'           v2ctcr
128600101015     c                   end
128700101015     c                   end
128800101015      * verifico se evento proposto abilita scrittura estensione evento
128900101015      * oppure se evento proposto forzato abilita reperimento DCR da FIARP o
129000101015      * da PDA
129100101015     C                   z-add     1             Yz                3 0
129200101015     C     V2cevp        lookup    c2a(Yz)                                95
129300101015     C                   if        *in95                                        --- 04 ---
129400101015     C                   movel     d2z(Yz)       ds2z
129500101015     C                   MOVEL     §2Zadr        W2zadr            1
129600101015     c                   else
129700101015     C                   clear                   W2zadr
129800101015     C                   endif                                                  --- 04 ---
129900070309      *data consegna richiesta da file FIARP x eventi abilitati
130000101015     c*                  clear                   v13tcrd
130100070518      *segnala se immesso evento che non gestisce fiarp ma esiste un record
130200070518      *riferito alla bolla
130300101015     c                   clear                   fiarpds
130400101015     c                   setoff                                       66
130500070313     c     karp          chain(n)  fiarp01l
130600070517     c                   if        %found(fiarp01l) and w2zadr = *blank
130700070517     c                             and d53ita <>'E' and segnale = *blank
130800070518     c                             and arpdcr > oggi
130900070517     c                   move      'X'           segnale
131000070517     c                   movel     err(101)      $msg
131100070517     C                   seton                                        4028
131200070517     c                   goto      aggrec
131300070517     c                   endif
131400070309     c                   if        %found(fiarp01l) and w2zadr <> *blank
131500070309     c                             and d53ita <>'E'
131600070309     c                   move      arpdcr        v2cdcr
131700101015     c                   move      arptcr        v2ctcr
131800101015     c                   move      arphcr        v2chcr
131900101015     c*                  move      arptcr        v13tcr
132000101015     c*                  move      arphcr        v13hcr
132100101015     c                   seton                                        66
132200071003     c                   eval      v2harp = '1'
132300070405      * imposta descrizione del flad x data consegna richiesta
132400101015     c*                  if        v13tcr = *blank
132500101015     c*                  move      '  Il'        v13tcrd
132600101015     c*                  else
132700101015     c*                  move      'Dopo'        v13tcrd
132800101015     c*                  endif
132900070405      *
133000070309     c                   else
133100101015     c*                  clear                   v13tcr
133200101015     c*                  clear                   v13hcr
133300070309     c                   clear                   w2zadr
133400070309     c                   endif
133500950113     C   28              GOTO      AGGREC
133600970122     C*  Se richiesto richiamo pgm x invio dei motivi di
133700970122     C*  mancata consegna
133800970122     C     V02SEL        IFEQ      'W'
133900970122     C                   MOVEL     'S'           D53VIS
134000970122     C                   EXSR      CALLR3
134100970122     C                   MOVEL     *BLANKS       V02SEL
134200970122     C     D53ERR        IFEQ      '1'
134300970122     C                   MOVEL     D53MSG        $MSG
134400970122     C                   SETON                                        28
134500970122     C                   GOTO      AGGREC
134600970122     C                   END
134700970122     C                   END
134800100205     c* se non consegnata e l'evento immesso prevede il tentativo di conseg.
134900101022     c*m                 exsr      srctrora
135000101022     C*m 47              GOTO      AGGREC
135100100205     c*********************************
135200100205     C*  MANCATA CONSEGNA, NO PARZIALE
135300100205     c*********************************
135400941123     C     §2ATIP        IFNE      'P'                                          --- 04 ---
135500941116     C*  Se c'è precedente consegna parziale non si può mettere giacenza
135600941116     C     §2AFTG        IFEQ      'S'
135700941116     C     V02PRZ        ANDEQ     'S'
135800941116     C                   SETON                                        4028
135900941116     C                   MOVEL     ERR(3)        $MSG
136000941116     C                   GOTO      AGGREC
136100941116     C                   END
136200941116     C*  Se non consegnata impossibile mettere codice consegna anomala
136300941116     C     V02CCA        IFNE      *BLANKS
136400941116     C                   SETON                                        404528
136500941116     C                   MOVEL     ERR(4)        $MSG
136600941116     C                   GOTO      AGGREC
136700941116     C                   END
136800941116     C*  Se non consegnata impossibile mettere riserva
136900941116     C     V02RIS        IFNE      *BLANKS
137000941116     C                   SETON                                        404328
137100941116     C                   MOVEL     ERR(5)        $MSG
137200941116     C                   GOTO      AGGREC
137300941116     C                   END
137400031202     C*  Se tentativi di consegna < a 2 non si può inserire l'evento 017
137500031202     C     V02ccn        IFeq      '017'
137600031202     C     V2cntc        andlt     2
137700031202     C                   SETON                                        4028
137800031202     C                   MOVEL     ERR(89)       $MSG
137900031202     C                   GOTO      AGGREC
138000031202     C                   END
138100941122     C*  Se immesso codice di lasc.avviso controllo che
138200941122     C*  non sia già stato immesso un codice analogo
138300960119     C*  a meno che non sia previsto dalla particol.
138400091026     c     d53ita        ifne      'E'
138500960119     C     §7R4TM        IFNE      §2AFTC
138600960205     C     §7R4NT        ORNE      V2CNTC
138700941222     C     §2AFTC        IFEQ      'A'
138800941122     C                   EXSR      GESAVV
138900941122     C   28              GOTO      AGGREC
139000941122     C                   END
139100960119     C                   END
139200091026     C                   END
139300100311     C*  Se mancata consegna impossibile immettere firmatario
139400100311     C     V2CFIR        IFNE      *BLANKS
139500100311     C                   SETON                                        404228
139600100311     C                   MOVEL     ERR(16)       $MSG
139700100311     C                   GOTO      AGGREC
139800100311     C                   END
139900941116     C*  Se mancata consegna vado a bloccare gli incassi sulla
140000941116     C*  relativa videata
140100941125     C     *INKF         IFEQ      '1'
140200941125     C     V2CNRD        IFNE      0                                            --- 05 ---
140300941117     C                   Z-ADD     NRR2          SAVREC
140400941207     C                   SETOFF                                       03
140500941116     C     V2CNRD        DO        V2CNRA        NRR3
140600941116     C     NRR3          CHAIN     LR53S03                            95
140700941116     C     *IN95         IFEQ      '0'
140800991007     C     V3DTPI        IFNE      'Ass'
140900991007     C     V3DTPI        ANDNE     'C/A'
141000991007     C                   SETON                                        05
141100991007     C                   ELSE
141200120302     C                   SETOFF                                       05
141300991007     C                   END
141400941116     C                   SETON                                        2282
141500941129     C                   MOVEL     'SI'          V3CNCO
141600941116     C                   MOVEL     'N'           V02SN1
141700120308     c     v3hkey        comp      ' '                                8787
141800941116     C                   UPDATE    LR53S03
141900941116     C                   SETOFF                                       22
142000941207     C                   SETON                                        03
142100941116     C                   END
142200941116     C                   END
142300941116     C                   Z-ADD     SAVREC        NRR2
142400100311     C*    NRR2          CHAIN     LR53S02                            95
142500941207     C                   MOVEL     'SI'          V2CBLO
142600941117     C                   END                                                    --- 05 ---
142700941116     C*
142800941116     C*  Se richiesta conferma aggiorno numero spedizioni non consegnate
142900941116     C                   Z-ADD     1             X
143000941116     C     VNMDOC        LOOKUP    NDC(X)                                 95
143100941129     C   95              ADD       1             SNT(X)
143200941116     C                   END
143300100205     c* Gestione data consegna richiesta
143400100211     c                   if        v2cevp <> v2hevp and
143500100211     c                             (w2zmdr <> *blank or w2zadr <> *blank)
143600100205     c                   setoff                                       32
143700100205     c                   exsr      vidrich
143800100211     c                   endif
143900100205     C*  Gestione turno
144000100205     C     V02CCN        IFEQ      'T  '
144100140313     C     V2cgc1        IFEQ      *BLANKS
144200140313     C     V2cgc2        andeq     *BLANKS
144300100205     C*
144400140313     C     V021gc        IFNE      *BLANKS
144500140313     C     V022gc        ORNE      *BLANKS
144600140313     C                   MOVEL     V021gc        W1GC
144700140313     C                   MOVEL     V022gc        W2GC
144800100205     C                   END
144900100205     C*
145000100205     C                   EXSR      TURNO
145100100205     C                   END
145200100205     C                   END
145300941117     C                   ELSE                                                   --- 04 ---
145400100205     c*****************************************
145500120109     C*  MANCATA CONSEGNA,  CONSEGNA PARZIALE
145600100205     c*****************************************
145700970805     C     V02CCA        IFEQ      '5'
145800970805     C     §2AIDD        ANDEQ     'P'
145900970805     C                   SETON                                        4528
146000970805     C                   MOVEL     ERR(73)       $MSG
146100970805     C                   GOTO      AGGREC
146200970805     C                   END
146300970805     C     §2AIDD        IFEQ      *BLANKS
146400941116     C                   EXSR      GESPAR
146500970805     C                   ELSE
146600970805     C                   MOVEL     '5'           V02CCA
146700970805     C                   MOVEL     V02CCN        V2CEVD
146800970805     C                   MOVEL     *BLANKS       V02CCN
146900970313     C* Se immesso codice consegna anomala '5' --> IDD apro finestra
147000970313     C* dettaglio segnacolli per richiedere all'utente quali sono i
147100970313     C* colli mancanti
147200970805     C                   EXSR      GESIDD
147300970805     C   28              GOTO      AGGREC
147400970805     C                   END
147500110117     C  nkl              MOVEL     *BLANKS       V02CCN
147600941116     C   28              GOTO      AGGREC
147700941117     C                   END                                                    --- 04 ---
147800950113     C*
147900941125     C                   END                                                    --- 03 ---
148000100205     c*********************
148100100205     c* CONSEGNA
148200100205     c*********************
148300941125     C     V02CCN        IFEQ      *BLANKS                                      --- 03 ---
148400100205     c* totalizzo ADR
148500000508     C     V2CCP1        IFEQ      'D'
148600000508     C     V2CCP2        OREQ      'D'
148700000508     C                   ADD       1             V05ADR
148800000508     C                   END
148900970123     C*  Se richiesto richiamo pgm x invio dei motivi di
149000970123     C*  mancata consegna
149100970123     C     V02SEL        IFEQ      'W'
149200970123     C                   MOVEL     'S'           D53VIS
149300970123     C                   EXSR      CALLR3
149400970123     C                   MOVEL     *BLANKS       V02SEL
149500970123     C     D53ERR        IFEQ      '1'
149600970123     C                   MOVEL     D53MSG        $MSG
149700970123     C                   SETON                                        28
149800970123     C                   GOTO      AGGREC
149900970123     C                   END
150000970123     C                   END
150100941116     C*  Gestisco i controlli da effettuare solo se ho consegnato
150200941124     C     V02CCA        IFNE      *BLANKS                                      --- 04 ---
150300941117     C*  Controllo codice consegna anomala
150400941117     C     '?'           SCAN      V02CCA                                 95
150500941117     C     *IN95         IFEQ      '1'
150600941123     C                   MOVEL     KKUT          §KUT
150700941117     C                   MOVEL     '7O'          §COD
150800941117     C                   MOVE      *BLANKS       V02CCA
150900941117     C                   CALL      'X§TABER'
151000941117     C                   PARM                    §KUT
151100941117     C                   PARM                    §COD
151200941117     C                   PARM                    §KEY
151300941117     C                   PARM                    §DES             30
151400941117     C                   MOVEL     §KEY          V02CCA
151500941117     C                   END
151600941124     C                   END
151700941117     C*
151800941124     C     V02CCA        IFNE      *BLANK                                       --- 04 ---
151900000505     C     V02CCA        LOOKUP    C7O                                    95
152000941128     C     *IN95         IFEQ      '0'
152100941116     C                   SETON                                        4528
152200941116     C                   MOVEL     ERR(10)       $MSG
152300941116     C                   GOTO      AGGREC
152400941124     C                   END
152500941124     C                   END                                                    --- 04 ---
152600941116     C*  Controllo codice consegna con riserva
152700941117     C     '?'           SCAN      V02RIS                                 95
152800941117     C     *IN95         IFEQ      '1'
152900941117     C                   MOVE      *BLANKS       V02RIS
153000941117     C                   MOVEL     *BLANKS       §TIP
153100941117     C                   MOVEL     'R*******'    §TIP
153200941117     C                   CALL      'TRUL40R'
153300941123     C                   PARM                    §TIP              8
153400941123     C                   PARM                    §KEY              8
153500941117     C                   PARM                    §DES             30
153600941117     C                   MOVEL     §KEY          V02RIS
153700941117     C                   END
153800941117     C*
153900941117     C     V02RIS        IFNE      *BLANK                                       --- 04 ---
154000941116     C                   Z-ADD     1             X
154100941116     C     V02RIS        LOOKUP    C2A(X)                                 95
154200941128     C     *IN95         IFEQ      '0'
154300941116     C                   SETON                                        4328
154400941116     C                   MOVEL     ERR(11)       $MSG
154500941116     C                   GOTO      AGGREC
154600941116     C                   ELSE
154700941116     C                   MOVEL     D2A(X)        DS2A
154800941116     C     §2ATIP        IFNE      'R'
154900941116     C                   SETON                                        4328
155000941116     C                   MOVEL     ERR(11)       $MSG
155100941116     C                   GOTO      AGGREC
155200941116     C                   END
155300941116     C                   END
155400941117     C                   END                                                    --- 04 ---
155500141127ab    *
155600141201ab   C*  La segnalazione la deve fare solo una volta poi
155700141201ab    *    è come se forzasse il controllo
155800141201ab   C*  SOLO se particolarità di consegna con PINCODE
155900141202ab   C                   if        §7RpinCODE ='S'
156000141202ab   c                              and v2hForza = *blank
156100141201ab    * ERRORE :
156200141201ab   C*   se si vuole mettere in consegna su una Distinta senza PDA
156300141201ab    *    oppure:
156400141201ab   C*   se la distinta è con PDA e la consegna non l'ha esitata l'Autista
156500141201ab   c                   if        wpda <>'S'
156600141201ab   c                              or
156700141201ab   c                             ecco_il_CET ='S' and
156800141212ab   c                             (§PCTFLGSPC='S' or PCTPDAAS4='A')
156900141202ab   c                              or
157000141202ab   c                             (vh2CCN <> *blank and vh2CCN <> 'P')
157100141211ab    *
157200141212ab    *  Controlla presenza di una RA (60) per verificare se Autorizzata
157300141212ab    *   la consegna oppure NO
157400141212ab   c                   exsr      Found_RA_60
157500141212ab    *
157600141212ab    * Se Presente la RA (60) ed è autorizzata
157700141212ab    *   deve INFORMARE e RICORDARE di apporre il timbro o la dicitura sulla LDV
157800141212ab   c                   IF         ecco_la_RA_60 = 'S' and Autorizzata ='S'
157900141212ab    *
158000141212ab    *  Emette una Finestra informativa
158100141212ab    *   e flagga comunque il record per non ripetere l'emissione
158200141212ab   c                   exsr      WIN_INFO
158300141212ab   c                   eval           v2hForza = 'I'
158400141212ab    *
158500141212ab    * Se NON presente la RA (60) oppure è presente ma non è autorizzata,
158600141212ab    *  deve proporre una WINDOW di Forzatura
158700141212ab    *    e se NON Forzato emettere l'errore sulla Consegna
158800141212ab    *
158900141212ab   c                   ElseIf     ecco_la_RA_60 <> 'S'
159000141212ab   c                                or
159100141212ab   c                              ecco_la_RA_60 = 'S' and Autorizzata <>'S'
159200141212ab    *
159300141211ab    *  Emette una Finestra di Forzatura
159400141211ab   c                   exsr      WIN_Forza
159500141202ab    *
159600141202ab    *   Se non si è forzato emissione dell'errore
159700141209ab   c                   if        lrq0errO = '0' and lrq0frzIO <> 'F'
159800141127ab   C                   SETON                                        4028
159900141127ab   C                   MOVEL     ERR(77)       $MSG
160000141127ab   C                   GOTO      AGGREC
160100141211ab    *
160200141209ab   c                   elseIf        lrq0frzIO = 'F'
160300141211ab    *
160400141212ab    *  x Pro-memoria di aver Forzato
160500141212ab    *    serve "F" per scrivere una RA (61) quando fa la AGGIOR
160600141209ab   c                   eval      v2hForza = lrq0frzIO
160700141202ab   c                   end
160800141202ab    *
160900141212ab   c                   endIF
161000141212ab    *
161100141201ab   C                   END
161200141127ab   C                   END
161300141127ab   C*
161400950113     C*  Controllo se obbligatorio il firmatario
161500950113     C     §7R2FI        IFEQ      'S'
161600950113     C     V2CFIR        ANDEQ     *BLANKS
161700950113     C                   SETON                                        4228
161800950113     C                   MOVEL     ERR(12)       $MSG
161900950113     C                   GOTO      AGGREC
162000950113     C                   END
162100970623     C*  Se non consegnata impossibile mettere codice consegna anomala
162200970623     C     V02CCA        IFEQ      '5'
162300970623     C     V02CPZ        ANDEQ     'P'
162400970623     C                   SETON                                        404528
162500970623     C                   MOVEL     ERR(73)       $MSG
162600970623     C                   GOTO      AGGREC
162700970623     C                   END
162800100205     c* controllo ora consegna
162900100205     c                   exsr      srctrora
163000100205     C   47              GOTO      AGGREC
163100941118     C*
163200941123     C   KF              EXSR      AGGTO2
163300941124     C                   END
163400941205     C*
163500941205     C     V2CTUR        IFEQ      'T'
163600941205     C                   EXSR      TURNO
163700941205     C                   MOVEL     ' '           V2CTUR
163800941205     C                   END
163900100205     c*************
164000080806     c* richiamo pgm variazione bolle x inserire bancali causale BR
164100150224     c*m                 if        v02ccn = *blank or v02ccn ='P  '
164200141202ab    ****
164300150224ab   c*m                 if        ecco_il_CET = 'S'
164400141202ab    ****
164500150224     c*m                 CLEAR                   darendere         2 0
164600150224     c*m                 CLEAR                   dar5ban
164700150224     c*m                 move      'BAN'         trek5
164800150224     c*m   kar5          chain(n)  fiar501l
164900150224     c*m                 if        %found(fiar501l)
165000150224     c*m                 movel     ar5uni        dar5ban
165100150224     c*m                 eval      darendere =  §ar5nba + §ar5nb2
165200150224     c*m                 endif
165300141202      ****
165400150224     c*m                 if        wnspbr <> v2cnsp and darendere > 0
165500150224     c*m                 z-add     v2cnsp        wnspbr            7 0
165600150224     C*m                 CLEAR                   FNLR48
165700150224     C*m                 CLEAR                   DSARBD
165800150224     C*m                 CLEAR                   DSARBG
165900150224     C*m                 CLEAR                   DSARBk
166000150224     C*m                 CLEAR                   DARBt
166100080806      *
166200150224     C*m                 Z-ADD     V2CLNP        D48LNP
166300150224     C*m                 Z-ADD     V2CAAS        D48AAS
166400150224     C*m                 Z-ADD     V2CNRS        D48NRS
166500150224     C*m                 Z-ADD     V2CNSP        D48NSP
166600150224     C*m                 MOVEL     'E'           D48FFR
166700150224     C*m                 MOVEL     V2CCBO        D48CBO
166800150224     C*m                 MOVEL     'BR'          D48CVB
166900150224     C*m                 MOVEL     'D'           D48TRC
167000150224     c*m                 z-add     §PCTNRBANR    §BDPKF
167100150224     c*m                 if        §PCTNOMFIR <> *blank
167200150224     c*m                 movel     §PCTNOMFIR    §BDRSD
167300150224     c*m                 else
167400150224     c*m                 eval      §BDRSD  = 'Nominativo non inserito'
167500150224     c*m                 endif
167600150224     c*m                 move      'PDA'         §BDRD2
167700150224     C*m                 MOVEL     fnlr48        kpjbu
167800150224     C*m                 CALL      'FNLR48R'
167900150224     C*m                 PARM                    KPJBA
168000150224     C*m                 PARM                    DSARBD
168100150224     C*m                 PARM                    DSARBK
168200150224     C*m                 PARM                    DARBT
168300150224     C*m                 PARM                    DSARBG
168400150224     C*m                 PARM                    trul90ds
168500150224     C*m                 MOVEL     KPJBU         FNLR48
168600150224     c*m                 endif
168700150224     c*m                 endif
168800150224     c*m                 endif
168900150224     C*
169000941117     C*  Controllo se variati codici consegna particolare o gg.
169100941117     C*  di chiusura x evetuale richiamo pgm. variazione bolle
169200941130     C     V2CCP1        IFNE      V2HCP1
169300950206     C     V2CCP1        ANDNE     *BLANKS
169400941130     C     V2CCP2        ORNE      V2HCP2
169500950206     C     V2CCP2        ANDNE     *BLANKS
169600941117     C     W1GC          ORNE      V021GC
169700941117     C     W2GC          ORNE      V022GC
169800950322     C     V2CGC1        ORNE      *BLANKS
169900950322     C     V2CGC1        ANDNE     V021GC
170000950322     C     V2CGC2        ORNE      *BLANKS
170100950322     C     V2CGC2        ANDNE     V022GC
170200040223     c     w13dcrg       orne      v2cdcr
170300051010     c     w13doil       orne      v2ctcr
170400101214     c     w2zadr        orne      *blank
170500941117     C                   EXSR      VARBOL
170600941117     C                   END
170700020610      *
170800941117     C*  Eseguo update subfile
170900941117     C     AGGREC        TAG
171000990922     C     V2CNT1        IFNE      *BLANKS
171100090220     C     V02selb       IFeq      *BLANKS
171200941214     C                   SETON                                        02
171300090220     c                   else
171400090220     c                   setoff                                       02
171500090220     C                   endif
171600941214     C                   END
171700941128     C     V02CPZ        COMP      'P'                                    07
171800970805     C  N07V2CIDD        COMP      'P'                                    07
171900941117     C                   SETON                                        22
172000970218     C                   SETOFF                                       35
172100970218     C     *IN28         IFEQ      '0'
172200970218     C     *IN40         OREQ      '0'
172300970218     C     V2CEXC        COMP      *BLANKS                            3535
172400970218     C                   ELSE
172500970218     C                   MOVEL     *BLANKS       V2CEXC
172600970218     C                   MOVEL     *BLANKS       V2CNM1
172700970218     C                   MOVEL     *BLANKS       V2CNM2
172800970218     C                   MOVEL     *BLANKS       V2CTR1
172900970218     C                   MOVEL     *BLANKS       V2CTR2
173000970218     C                   END
173100970805     C     V2CIDD        COMP      ' '                                7575
173200131205     C     Vh2hco        COMP      0                                  52
173300990423     C                   MOVEL     V2IN09        *IN09
173400000522     C                   MOVEL     V2IN24        *IN24
173500941117     C                   UPDATE    LR53S02
173600941117     C                   SETOFF                                       22
173700941117     C     *IN28         IFEQ      '0'
173800941118     C     *INKF         IFEQ      '1'
173900941118     C                   ADD       1             NRR2
174000941118     C     NRR2          CHAIN     LR53S02                            96
174100941118     C                   ELSE
174200941117     C                   READC     LR53S02                                96
174300941123     C                   END
174400941117     C                   ELSE
174500941117     C                   MOVE      NRR2          NR2KEY
174600941117     C                   END
174700941117     C*
174800941117     C                   END                                                    --- 01 ---
174900941117     C*
175000941117     C                   ENDSR
175100091216     C*----------------------------------------------------*
175200091216     C*  controllo ora consegna per h10,30 e priority
175300091216     C*----------------------------------------------------*
175400091216     C     srctrora      BEGSR
175500091216     C     V02CCN        IFne      *BLANKS                                      --- 03 ---
175600091216     C     V2CEVP        IFEQ      *BLANKS
175700091216     C                   MOVEL     V02CCN        V2CEVP
175800091216     C                   END
175900091216     C                   Z-ADD     1             X
176000091216     C     V2cevp        LOOKUP    C2A(X)                                 95
176100091216     C                   MOVEL     D2A(X)        DS2A
176200091216     c                   if        §2antc <> 'S'
176300091216     c                   leavesr
176400091216     c                   end
176500100205     C                   MOVEL     D2Z(X)        DS2Z
176600091216     c                   end
176700100205     c* obbligo ora se H1030, priority, se richiesto nella part.cons.
176800100205     c* oppure se estensione eventi
176900091216     C                   if        v02hco = 0 and v02mco = 0
177000091216     C                             and (v02tsp = 'H'
177100100203     c                             or v02tsp = 'E'
177200100112     c                             or §7R2OR = 'S' or
177300140221     c                             §2ztre = 'S')
177400091221     c                   movel     v02tsp        v02fhob
177500091216     C                   SETON                                        4728
177600091216     C                   MOVEL     ERR(13)       $MSG
177700091218     c                   leavesr
177800091216     C                   END
177900091216     C*  Ora consegna errata  per H10,30
178000091216     C     v02fhob       IFeq      'H'
178100131203      *se impostata ora da esistenza esito CEP utilizzo quella per calcolare affidabilità
178200131203     C     Vh2HCO        IFGT      0
178300131203     C     Vh2HCO        IFGT      10
178400131203     C     Vh2hco        OReq      10
178500131203     C     Vh2MCO        ANDGT     30
178600131203     C                   SETON                                        4728
178700131203     C                   MOVEL     ERR(107)      $MSG
178800131203     c                   movel     'X'           v02fhob
178900131203     c                   leavesr
179000131203     C                   Endif
179100131203      *cenza CEP uso campi a video normalmente
179200131203     C                   Else
179300091216     C     V02HCO        IFGT      10
179400091216     C     V02hco        OReq      10
179500091216     C     V02MCO        ANDGT     30
179600091216     C                   SETON                                        4728
179700091216     C                   MOVEL     ERR(107)      $MSG
179800091216     c                   movel     'X'           v02fhob
179900091218     c                   leavesr
180000091216     C                   END
180100131203     C                   endif
180200091216     C                   END
180300091216     C*  Ora consegna errata  per priority
180400091216     C     v02fhob       IFeq      'E'
180500131203     c                   if        vh2hco > 0
180600131203     c                   movel     vh2hco        comora            4 0
180700131203     c                   move      vh2mco        comora            4 0
180800131203     c                   else
180900091216     c                   movel     v02hco        comora            4 0
181000091216     c                   move      v02mco        comora            4 0
181100131203     c                   endif
181200091218     c                   if        vh2ddc > v2hdci or
181300091218     c                             (vh2ddc = v2hdci and
181400091216     c                             comora > v2hhci )
181500091216     C                   SETON                                        4728
181600091216     C                   MOVEL     ERR(108)      $MSG
181700091216     c                   movel     'X'           v02fhob
181800091218     c                   leavesr
181900091216     C                   END
182000091216     C                   END
182100131204     c* controllo  se presente ora primo evento l'ora a video non può essere inferiore
182200131204     c                   if        vh2hco > 0
182300131204     c                   movel     vh2hco        comorap           4 0
182400131204     c                   move      vh2mco        comorap
182500131204     c                   movel     v02hco        comorav           4 0
182600131204     c                   move      v02mco        comorav
182700131204     c                   if        comorav < comorap
182800131204     c                   seton                                        4728
182900131204     c                   movel     err(83)       $msg
183000131204     c                   leavesr
183100131204     c                   endif
183200131204     c                   endif
183300100112     C*  Controllo L'ora di consegna SE DISTINTA DEL POMERIGGIO L'ORARIO
183400100112      *  DEVE ESSERE MAGGIORE UGUALE ALLE 13
183500100112     C     V02HCO        IFNE      0
183600100112     C                   Z-ADD     1             XY                3 0
183700100112     C     VNMDOC        LOOKUP    ND1(XY)                                99
183800100112     C   99FPP(XY)       IFEQ      'P'
183900120814     C     wpda          ANDne     'S'
184000100112     C     V02HCO        ANDLE     12
184100100112     C                   SETON                                        4728
184200100112     C                   MOVEL     ERR(84)       $MSG
184300100112     c                   leavesr
184400100112     C                   END
184500100112     C                   ENDIF
184600100112     C*  Ora consegna errata
184700100112     C     D53ITA        IFNE      'E'
184800100112     C     V02HCO        IFGT      21
184900100112     C     V02HCO        ORLT      7
185000100112     C     V02HCO        ANDGT     0
185100100112     C     V02MCO        ORNE      0
185200100112     C     V02MCO        ANDGT     59
185300100112     C                   SETON                                        4728
185400100112     C                   MOVEL     ERR(14)       $MSG
185500100112     c                   leavesr
185600100112     C                   END
185700100112     C                   END
185800091216     C                   ENDSR
185900941116     C*----------------------------------------------------*
186000941116     C*  GESTIONE PARTICOLARITA' CONSEGNA
186100941116     C*----------------------------------------------------*
186200941116     C     GESPCO        BEGSR
186300970617      *
186400040213 1   C     V2CGMA        IFNE      *BLANKS
186500970211     C*  Se esiste particolarità che prevede l'invio dei
186600970211     C*  motivi di mancata consegna ed ho immesso G o GEN
186700970211     C*  richiamo pgm x immissione decodifica
186800040213 2   C     §2ATIP        IFNE      'P'
186900970211     C                   MOVEA     TMC           WTMC              5
187000040213 3   C     WTMC          IFNE      *BLANKS
187100970211     C     'T'           LOOKUP    TMC                                    32
187200970211     C  N32'G'           LOOKUP    TMC                                    32
187300040213 4   C   32V02CCN        IFEQ      'G  '
187400970211     C     V02CCN        OREQ      'GEN'
187500040213 5   C     V2CEXC        IFEQ      *BLANKS
187600970211     C                   MOVEL     'S'           D53VIS
187700970211     C                   EXSR      CALLR3
187800040213 6   C     D53ERR        IFEQ      '1'
187900970211     C                   MOVEL     D53MSG        $MSG
188000970211     C                   SETON                                        28
188100040213 x6  C                   ELSE
188200040213 7   C     D53NT1        IFEQ      *BLANKS
188300970220     C     D53MSG        ANDEQ     *BLANKS
188400970211     C                   SETON                                        28
188500970211     C                   MOVEL     ERR(64)       $MSG
188600040213 e7  C                   END
188700040213 e6  C                   END
188800040213 e5  C                   END
188900970211     C*
189000040213 x4  C                   ELSE
189100970211     C                   MOVEL     'N'           D53VIS
189200970211     C                   EXSR      CALLR3
189300040213 5   C     D53ERR        IFEQ      '1'
189400970211     C                   MOVEL     D53MSG        $MSG
189500970211     C                   SETON                                        28
189600040213 e5  C                   END
189700040213 e4  C                   END
189800040213 e3  C                   END
189900941116     C*  Se cod.particolarità indica di ritentare consegna x n.volte
190000000421     C*  per lasciato avviso sostiutuisco all'evento di lasciato avv.
190100000421     C*  quello che la particolarità mi dice di proporre fino a che
190200000421     C*  non ho raggiunto gli x tentativi
190300040213 3   C     §2AFTC        IFEQ      §7R4TM
190400941123     C     V2CNTC        ANDLT     §7R4NT
190500951228     C     V02CCN        ANDNE     §7R4EV
190600951228     C     V2CF08        ANDEQ     *BLANKS
190700040213 4   C     V2CEVP        IFNE      §7R4EV
190800030829     C*                  SETON                                        2810
190900030829     C                   SETON                                        28
191000941116     C                   MOVEL     §7R4EV        V2CEVP
191100941116     C                   MOVEL     ERR(7)        $MSG
191200941116     C                   GOTO      FINPAR
191300040213 x4  C                   ELSE
191400951228     C                   MOVEL     *INKH         V2CF08
191500040213 6   C     *INKH         IFEQ      '1'
191600040213 7   C     §7R3EV        IFEQ      *BLANKS
191700990419     C                   MOVEL     V02CCN        V2CEVP
191800040213 x7  C                   ELSE
191900990419     C                   MOVEL     §7R3EV        V2CEVP
192000040213 e7  C                   END
192100040213 e5  C                   END
192200951228     C                   SETOFF                                       10
192300040213 e4  C                   END
192400040213 e3  C                   END
192500941125     C*  Lasciato avviso diverso da standard
192600040213 3   C     WTMC          IFNE      *BLANKS
192700970211     C     D53TRV        ANDEQ     'S'
192800970211     C     §2AFTC        ANDEQ     §7R3TM
192900970211     C     V02CCN        ANDNE     §7R3EV
193000970211     C     V2CEVP        ANDNE     §7R3EV
193100970214     C                   MOVEL     'AVV'         §7R3EV
193200040213 e3  C                   END
193300970211     C*
193400040213 3   C     §2AFTC        IFEQ      §7R3TM
193500941116     C     V02CCN        ANDNE     §7R3EV
193600941116     C     V2CEVP        ANDNE     §7R3EV
193700040213 4   C     §7R4NT        IFLE      V2CNTC
193800990419     C     §7R4NT        OREQ      *ZEROS
193900970211     C*  Se previsto invio e già inviato x l.avviso non do più attesa
194000941116     C                   SETON                                        28
194100941116     C                   MOVEL     §7R3EV        V2CEVP
194200941116     C                   MOVEL     ERR(7)        $MSG
194300941116     C                   GOTO      FINPAR
194400040213 e4  C                   END
194500040213 e3  C                   END
194600941116     C*  Se sono impostati dei codici tipologie giacenze controllo
194700941116     C*  attendibilità con codice mancata consegna
194800960119     C                   MOVEA     TPG           WTPG              5
194900040213 3   C     §2AFTC        IFEQ      'G'
195000961129     C*
195100040213 4   C     WTPG          IFNE      *BLANKS
195200040213 5   C     V02CCN        IFEQ      'G  '
195300961205     C     WTPG          ANDNE     'T'
195400960119     C                   SETON                                        2840
195500960119     C                   MOVEL     ERR(9)        $MSG
195600960119     C                   GOTO      FINPAR
195700040213 e5  C                   END
195800960116     C                   SETOFF                                       95
195900040213 5   C     WTPGIA        IFNE      *BLANKS
196000941116     C                   Z-ADD     1             X
196100950113     C     WTPGIA        LOOKUP    TPG(X)                                 95
196200040213 e5  C                   END
196300040213 5   C     *IN95         IFEQ      '0'
196400941116     C                   Z-ADD     1             X
196500941116     C     'T'           LOOKUP    TPG(X)                                 95
196600040213 e5  C                   END
196700040213 5   C     *IN95         IFEQ      '1'
196800941116     C     V2CNTC        ANDLT     NTC(X)
196900941116     C     V2CEVP        ANDNE     EVE(X)
197000941116     C     V02CCN        ANDNE     EVE(X)
197100941116     C                   SETON                                        28
197200941116     C                   MOVEL     EVE(X)        V2CEVP
197300941116     C                   MOVEL     ERR(7)        $MSG
197400941116     C                   GOTO      FINPAR
197500040213 e5  C                   END
197600040213 e4  C                   END
197700941125     C*  Apertura giacenza dopo x giorni
197800040213 4   C     §2AFTC        IFEQ      §7R5TM
197900941116     C     V2CEVP        ANDNE     §7R5EV
198000941116     C     V02CCN        ANDNE     §7R5EV
198100941116     C                   SETON                                        28
198200941116     C                   MOVEL     §7R5EV        V2CEVP
198300941116     C                   MOVEL     ERR(7)        $MSG
198400941116     C                   GOTO      FINPAR
198500040213 e4  C                   END
198600040213 e3  C                   END
198700961129     C*
198800040213 e2  C                   END
198900941116     C*  Se immesso codice particolarità da usare solo se cliente
199000941116     C*  abilitato controllo se = codice particolarità da propore
199100131111 2   C*m   §2ZUTP        IFEQ      'S'
199200131111     C*m   C2A(Y)        ANDEQ     V02CCN
199300131111     C*m                 SETON                                        2840
199400131111     C*m                 MOVEL     ERR(55)       $MSG
199500131111     C*m                 GOTO      FINPAR
199600131111 e2  C*m                 END
199700040213 e1  C                   END
199800941116     C*----------------------------------------------------*
199900941116     C*  FINE GESTIONE PARTICOLARITA' CONSEGNA
200000941116     C*----------------------------------------------------*
200100941123     C     FINPAR        ENDSR
200200970121     C*----------------------------------------------------*
200300970121     C*  RICHIAMO PGM FNLR53R3
200400970121     C*----------------------------------------------------*
200500970121     C     CALLR3        BEGSR
200600970121     C*
200700970121     C                   MOVEL     D53VIS        WSVVIS            1
200800970121     C                   CLEAR                   LR533
200900970220     C                   MOVEL     V02SEL        D53RIC
201000970220     C                   MOVEL     WSVVIS        D53VIS
201100970121     C                   Z-ADD     V2CAAS        D53AAS
201200970121     C                   Z-ADD     V2CLNP        D53LNP
201300970121     C                   Z-ADD     V2CNRS        D53NRS
201400970121     C                   Z-ADD     V2CNSP        D53NSP
201500970121     C                   MOVEL     §2AFTC        D53TMC
201600061030      * passo evento proposto altrimenti passo evento inserito a video
201700090507      * se evento di giacenza passo sempre quello del video da riportare sul tivgd
201800090507     c                   if        §2aftc = 'G'
201900090507     c                   movel     v02ccn        d53cmc
202000090507     c                   else
202100090507     c                   if        v2cevp <> *blank
202200090507     c                   movel     v2cevp        d53cmc
202300090507     c                   else
202400090507     C                   MOVEL     V02CCN        D53CMC
202500090507     c                   endif
202600090507     c                   endif
202700970121     C                   MOVEL     §2ADES        D53DMC
202800970121     C                   MOVEL     V2CGMA        D53GMA
202900970121     C                   MOVEL     V2CEXC        D53EXC
203000970121     C                   MOVEL     V2CNM1        D53NT1
203100970121     C                   MOVEL     V2CNM2        D53NT2
203200970121     C                   MOVEL     V2CTR1        D53TR1
203300970121     C                   MOVEL     V2CTR2        D53TR2
203400970121     C                   MOVEL     LR533         KPJBU
203500970121     C                   CALL      'FNLR53R3'
203600970121     C                   PARM                    KPJBA
203700970121     C                   MOVEL     KPJBU         LR533
203800970121     C*  Se non ci sono errori aggiorno campi subfile
203900970121     C     D53ERR        IFEQ      *BLANKS
204000970121     C     D53F12        ANDEQ     *BLANKS
204100970121     C                   MOVEL     D53EXC        V2CEXC
204200970121     C                   MOVEL     D53NT1        V2CNM1
204300970121     C                   MOVEL     D53NT2        V2CNM2
204400970121     C                   MOVEL     D53TR1        V2CTR1
204500970121     C                   MOVEL     D53TR2        V2CTR2
204600970121     C*  Se ho immesso note aggiorno EXC con CMC
204700970121     C     D53NT1        IFNE      *BLANKS
204800970121     C                   MOVEL     D53CMC        V2CEXC
204900970121     C                   END
205000970121     C                   END
205100970121     C*
205200970121     C                   ENDSR
205300941122     C*----------------------------------------------------*
205400941122     C*  GESTIONE LASCIATO AVVISO
205500941122     C*----------------------------------------------------*
205600941122     C     GESAVV        BEGSR
205700941122     C*
205800941222     C                   Z-ADD     V2CLNP        KLNP
205900941222     C                   Z-ADD     V2CNRS        KNRS
206000941222     C                   Z-ADD     V2CNSP        KNSP
206100990803     C                   Z-ADD     V2CAAS        KAAS
206200941122     C                   MOVE      *HIVAL        KDEV
206300941122     C                   MOVE      *HIVAL        KHEV
206400941123     C                   MOVE      'N'           WTRAVV            1
206500941123     C                   MOVE      'N'           WTRGIA            1
206600000421     C* A fronte di un immissione di un evento di lasciato
206700000421     C* avviso in distinta verifico se ne ho immesso un altro
206800990803     C     KEVB1         SETGT     FNEVB01L
206900990803     C     KEVB2         READPE    FNEVB01L                               95
207000941122     C     *IN95         DOWEQ     '0'
207100941222     C     WTRAVV        ANDEQ     'N'
207200941122     C                   Z-ADD     1             X
207300941122     C     EVBCEV        LOOKUP    C2A(X)                                 94
207400941122     C     *IN94         IFEQ      '1'
207500941122     C                   MOVEL     D2A(X)        DS2A
207600070406     C                   MOVEL     D2z(X)        DS2z
207700941122     C     §2AFTC        IFEQ      'A'
207800941122     C                   MOVEL     'S'           WTRAVV
207900941123     C                   Z-ADD     EVBDEV        WDTAVV
208000941122     C                   END
208100941122     C                   END
208200990803     C     KEVB2         READPE    FNEVB01L                               95
208300941122     C                   END
208400941122     C*
208500000421     C* ... se ho trovato un evento di lasciato avviso verifico
208600000421     C* se dopo l'immissione di quest'ultimo ho avuto una apert.
208700000421     C* di giacenza
208800941222     C                   MOVE      V2CAAS        KAAS
208900941125     C     WTRAVV        IFEQ      'S'
209000050406     C     KGCA          CHAIN     tigcp21L                           95
209100941125     C     *IN95         IFEQ      '0'
209200941122     C                   MOVE      GCPDUR        WDTGIA
209300941122     C     WDTGIA        IFGT      WDTAVV
209400941122     C                   MOVE      'S'           WTRGIA
209500941122     C                   END
209600941122     C                   END
209700941122     C*  Se trovato lasciato avviso e non trovata giacenza errore
209800000421     C*  (non posso fare + lasciati avvisi senza giacenza a meno
209900000421     C*   che non sia previsto dalla perticolarità consegne)
210000941125     C     WTRGIA        IFNE      'S'
210100941122     C                   SETON                                        4028
210200941122     C                   MOVEL     ERR(53)       $MSG
210300941122     C                   END
210400941122     C                   END
210500941122     C*
210600941122     C                   ENDSR
210700941117     C*----------------------------------------------------*
210800941117     C*  GESTIONE RICHIAMO PGM VARIAZIONE BOLLE
210900941117     C*----------------------------------------------------*
211000941117     C     VARBOL        BEGSR
211100941117      *
211200941117     C                   Z-ADD     1             X                 3 0
211300941117     C                   CLEAR                   DS3A
211400941125     C                   Z-ADD     1             X
211500941117     C     V2CCBO        LOOKUP    C3A(X)                                 95
211600941117     C     *IN95         IFEQ      '1'
211700941117     C                   MOVEL     D3A(X)        DS3A
211800941117     C                   END
211900941117     C                   MOVEL     §3ATB1        PORTO             1
212000941117     C*
212100941117     C* Carico la variazione sulla bolla a video tranne nei casi
212200000421     C* di bolla figlia non Assegnato
212300941222     C                   CLEAR                   FNLR48
212400941222     C                   CLEAR                   DSARBD
212500941117     C                   CLEAR                   DSARBG
212600941117     C     VH2LPP        IFEQ      0
212700941117     C     PORTO         OREQ      'A'
212800941117     C                   Z-ADD     V2CLNP        D48LNP
212900941117     C                   Z-ADD     V2CAAS        D48AAS
213000941117     C                   Z-ADD     V2CNRS        D48NRS
213100941117     C                   Z-ADD     V2CNSP        D48NSP
213200941117     C                   ELSE
213300941117     C                   Z-ADD     VH2LPP        D48LNP
213400941117     C                   Z-ADD     VH2AAP        D48AAS
213500941117     C                   Z-ADD     VH2NRP        D48NRS
213600941117     C                   Z-ADD     VH2NSP        D48NSP
213700941117     C                   END
213800941117     C*
213900941215     C                   MOVEL     'E'           D48FFR
214000941117     C                   MOVEL     V2CCBO        D48CBO
214100941117     C                   MOVEL     'CR'          D48CVB
214200941117     C                   MOVEL     'G'           D48TRC
214300941222     C                   MOVEL     'A'           D48TBO
214400040220      * variata data consegna richiesta
214500040220      *  in questo caso viene forzata la causale 'CD'
214600101214     c*                  if        w13dcrg > 0 and
214700101214     c*                            (w13dcrg <> v2cdcr or w13doil <> v2ctcr)
214800101214     c*                            or w2zadr <> *blank
214900101214     c                   if        (w13dcrg <> v2cdcr or w13doil <> v2ctcr)
215000101214     c                             or w2zadr <> *blank
215100101015     c*                  z-add     w13dcrg       v2cdcr
215200101015     c*                  move      w13doil       v2ctcr
215300101015     c*                  z-add     v13hcr        v2chcr
215400101015     c                   eval      w13dcrg   =   v2cdcr
215500101015     c                   eval      w13doil   =   v2ctcr
215600040220     C                   MOVEL     'CD'          D48CVB
215700040220     c                   endif
215800040220
215900030127      * Se la bolla prevede la consegna particolare supermercato
216000030127      * oppure è stata inserita la consegna particolare supermercato
216100030127      * verifico se devo richiamare il pgm FNLR48R con la causale 'CS'
216200040220      * al posto della causale 'CR' o 'CD'
216300030127     c                   If        V2cCp1 = 'S' or V2cCp2 = 'S' or
216400030127     c                             V2hCp1 = 'S' or V2hCp2 = 'S'
216500030127     c                   Clear                   Ds7l
216600030127     c                   Movel     '7L'          kCod
216700030127     c                   Movel(p)  'CS'          kKey
216800030127     c     kTab2         Chain     Tabel00f                           96
216900030127     c                   If        Not *In96 and TblFlg <> ' '
217000030127     c                   Eval      *In96 = *On
217100030127     c                   EndIf
217200030127     c  n96              Movel     TblUni        Ds7l
217300030127     c                   If        Not *In96 and §7LSup <> 'N'
217400030127     c                   Movel     'CS'          D48Cvb
217500050105     c                   Movel     'V'           D48Ffr
217600050105     c                   Movel     'S'           D48F12
217700030127     c                   EndIf
217800030127     c                   EndIf
217900040216
218000941117     C                   Z-ADD     V2CHCR        §BGHCR
218100941117     C                   Z-ADD     V2CDCR        §BGDCR
218200941117     C                   MOVEL     V2CTCR        §BGTCR
218300941117     C                   MOVEL     W1GC          §BGGC1
218400941117     C                   MOVEL     W2GC          §BGGC2
218500950322     C     V2CGC1        IFNE      *BLANKS
218600950322     C     V2CGC2        ORNE      *BLANKS
218700950321     C                   MOVEL     V2CGC1        §BGGC1
218800950321     C                   MOVEL     V2CGC2        §BGGC2
218900950321     C                   END
219000941117     C     V2CCP1        IFNE      *BLANKS
219100941117     C                   MOVEL     V2CCP1        §BGTC1
219200941117     C                   ELSE
219300941123     C                   MOVEL     V2HCP1        §BGTC1
219400941117     C                   END
219500941117     C     V2CCP2        IFNE      *BLANKS
219600941117     C                   MOVEL     V2CCP2        §BGTC2
219700941117     C                   ELSE
219800941123     C                   MOVEL     V2HCP2        §BGTC2
219900941117     C                   END
220000941117     C                   MOVEL     FNLR48        KPJBU
220100071031     C                   TIME                    WHHDAT
220200071031     C                   MOVEL     WHHDAT        TIMESav
220300071031     C                   MOVE      WHHDAT        G02DAT
220400071031     C                   MOVEL     *BLANK        G02ERR
220500071031     C                   CALL      'XSRDA8'
220600071031     C                   PARM                    WLBDA8
220700071031     C                   Z-ADD     G02INV        OGGIsav
220800941117     C                   CALL      'FNLR48R'
220900941117     C                   PARM                    KPJBA
221000941117     C                   PARM                    DSARBD
221100941117     C                   PARM                    DSARBK
221200991129     C                   PARM                    DARBT
221300941117     C                   PARM                    DSARBG
221400030718     C                   PARM                    trul90ds
221500941117     C                   MOVEL     KPJBU         FNLR48
221600941117     C     D48ERR        IFEQ      *BLANKS
221700050105     C     D48f12        andne     '1'
221800071026     c* memorizzo la spia per le spedioni che vengono variate perchè in caso
221900071026     c* di non conferma devo riportare indietro la variazione
222000071026     c                   exsr      srmemo
222100941125     C                   MOVEL     §BGTC1        V2CCP1
222200941125     C                   MOVEL     §BGTC2        V2CCP2
222300941123     C                   MOVEL     V2CCP1        V2HCP1
222400941123     C                   MOVEL     V2CCP2        V2HCP2
222500941123     C                   MOVE      W1GC          V021GC
222600941123     C                   MOVE      W2GC          V022GC
222700101015     C*                  MOVE      W13dcrg       V2cdcr
222800101015     C*                  MOVE      W13doil       V2ctcr
222900100211     C                   MOVEl     v2cevp        V2hevp
223000950322     C     V2CGC1        IFNE      *BLANKS
223100950322     C     V2CGC2        ORNE      *BLANKS
223200950322     C                   MOVE      V2CGC1        V021GC
223300950322     C                   MOVE      V2CGC2        V022GC
223400140313     C*mm                MOVEL     *BLANKS       V2CGC1
223500140313     C*mm                MOVEL     *BLANKS       V2CGC2
223600950322     C                   END
223700941117     C                   ELSE
223800941117     C                   MOVEL     *BLANKS       V2CCP1
223900941117     C                   MOVEL     *BLANKS       V2CCP2
224000101201     C*mm                MOVEL     '§  '         V02ccn
224100101201     C                   MOVEL     '£££'         V02ccn
224200941117     C                   SETON                                        28
224300941117     C                   MOVEL     ERR(24)       $MSG
224400941215     C     D48MSG        IFNE      *BLANKS
224500941215     C                   MOVEL     D48MSG        $MSG
224600941117     C                   END
224700941215     C                   END
224800941117     C*
224900941117     C                   ENDSR
225000941117     C*----------------------------------------------------*
225100071026     C*  memorizzo spia x bolla variata
225200941117     C*----------------------------------------------------*
225300071026     C     srmemo        BEGSR
225400071026     C*
225500071026     c                   clear                   varbtrd
225600071026     c     kvarb         chain     wfvarb1l
225700071026     c                   if        not %found(wfvarb1l)
225800090731      * se motivo 16 o 23 date consegna richiesta imposto il motivo per verificare
225900090731      * se quando vado ad aggiornare il rekord è stato cambiato ma è stata fatta
226000090731      * una variazione bolla con data consegna richiesta
226100090731     c                   if        v02ccn = '016' or v02ccn = '023'
226200090731     c                   move      v02ccn        varbtrd
226300090731     c                   endif
226400071026     C                   eval      varblnp =     D48LNP
226500071026     C                   eval      varbaas =     D48AAS
226600071026     C                   eval      varbnrs =     D48NRS
226700071026     C                   eval      varbnsp =     D48NSP
226800071026     C                   eval      varbpru =     knmus
226900071031     C                   Z-ADD     oggisav       vARBdim
227000071030     C                   eval      vARBoim = timesav
227100110418     c                   if        arpnsp =  d48nsp
227200110418     c*                  if        arpnsp <> 0
227300071026     c                   eval      varbflr = fiarpds
227400071029     c                   else
227500071029     c                   clear                   varbflr
227600071029     c                   end
227700071026     c                   write     wfvarb00
227800090731     c                   else
227900090731     c                   if        v02ccn = '016' or v02ccn = '023'
228000090731     c                   move      v02ccn        varbtrd
228100090731     c                   update    wfvarb00
228200090731     c                   endif
228300071026     c                   end
228400071026     C*
228500071026     C                   ENDSR
228600090731     C*----------------------------------------------------*
228700090731     C*  richiamo la variazione bolle per riportare la situazione iniziale
228800090731     C*----------------------------------------------------*
228900090731     C     srback        BEGSR
229000090731     c* trucco il profilo per scartare quelle automatiche che saranno con il
229100090731     C* profilo EDPCED
229200090731     c                   eval      kpjbas = kpjba
229300090731     c                   eval      sknmus = 'EDPCED'
229400090731     c     *loval        setll     wfvarb1l
229500090731     c                   do        *hival
229600090731     c                   read      wfvarb1l
229700090731     c                   if        %eof(wfvarb1l)
229800090731     c                   leave
229900090731     c                   end
230000131014     c     kwf           setgt     FIARBF2C
230100090731     c                   do        *hival
230200131014     c     kwf           readpe    FIARBF2C
230300131014     c                   if        %eof(fiarbf2c)
230400090731     c                   leave
230500090731     c                   end
230600090731     c* prendo le variazioni bolle fatte con il profilo fino alla data e ora
230700090731     c* della chiusura che sto facendo (escludo l'ultimo bite perchè
230800090731     c* c'è A x bolle arrivi e P per bolle in partenza)
230900090731     c                   if        %subst($ARBPRU: 1: 9) <>
231000090731     c                             %subst(knmus: 1: 9)
231100090731     c                   iter
231200090731     c                   end
231300090731     c                   movel     $arbdtv       com$arb
231400090731     c                   move      $arborv       com$arb
231500090731     c                   movel     varbdim       comvarb
231600090731     c                   move      varboim       comvarb
231700090731     c                   if        com$arb <comvarb
231800090731     c                   leave
231900090731     c                   end
232000090731     C                   CLEAR                   FNLR48
232100090731     C                   CLEAR                   DSARBD
232200090731     C                   CLEAR                   DSARBG
232300090731     C                   Z-ADD     $arbLNP       D48LNP
232400090731     C                   Z-ADD     $arbAAS       D48AAS
232500090731     C                   Z-ADD     $arbNRS       D48NRS
232600090731     C                   Z-ADD     $arbNSP       D48NSP
232700090731     C                   MOVEL     $arbcbo       D48CBO
232800090731     C                   MOVEL     $arbcvb       D48CVB
232900090731     C                   MOVEL     'E'           D48FFR
233000090731     C                   MOVEL     'G'           D48TRC
233100090731     C                   MOVEL     'A'           D48TBO
233200090731     C                   Z-ADD     $arbhcr       §BGHCR
233300090731     C                   Z-ADD     $arbdcr       §BGDCR
233400090731     C                   MOVEL     $arbtcr       §BGTCR
233500090731     C                   MOVEL     $arbgc1       §BGGC1
233600090731     C                   MOVEL     $arbgc2       §BGGC2
233700090731     C                   MOVEL     $arbtc1       §BGTC1
233800090731     C                   MOVEL     $arbtc2       §BGTC2
233900090731     C                   MOVEL     FNLR48        sKPJBU
234000090731     C                   CALL      'FNLR48R'
234100090731     C                   PARM                    KPJBAs
234200090731     C                   PARM                    DSARBD
234300090731     C                   PARM                    DSARBK
234400090731     C                   PARM                    DARBT
234500090731     C                   PARM                    DSARBG
234600090731     C                   PARM                    trul90ds
234700090731     C                   MOVEL     sKPJBU        FNLR48
234800090731     C     D48ERR        IFEQ      *BLANKS
234900090731     c* verifico se devo ricreare il fiarp00f
235000090731     c                   if        varbflr <> ' '
235100090731     c     kvar          chain(n)  fiarp01l
235200090731     c                   if        not %found(fiarp01l)
235300090731     c                   movel     varbflr       fiarpds
235400090731     c                   write     fiarp000
235500090731     c                   end
235600090731     c                   end
235700090731     c                   end
235800090731     c                   enddo
235900090731     c                   delete    wfvarb1l
236000090731     c                   enddo
236100090731     C*
236200090731     C                   ENDSR
236300090731     C*----------------------------------------------------*
236400090731     C*  richiamo la variazione bolle se è stato cambiato il motivo dopo variazione
236500090731     C*----------------------------------------------------*
236600090731     C     srback2       BEGSR
236700090731     c* trucco il profilo per scartare quelle automatiche che saranno con il
236800090731     C* profilo EDPCED
236900090731     c                   eval      kpjbas = kpjba
237000090731     c                   eval      sknmus = 'EDPCED'
237100090804     c     *loval        setll     wfvarb1l
237200090731     c                   do        *hival
237300090804     c                   read      wfvarb1l
237400090731     c                   if        %eof(wfvarb1l)
237500090731     c                   leave
237600090731     c                   end
237700110418     c*                  if        varbtrd = '016' or varbtrd = '023'
237800110418     c                   if        varbflr = *blank and
237900110418     c                             (varbtrd = '016' or varbtrd = '023')
238000090804     c     kwf1          chain(n)  fnarb01l
238100090804     c                   if        %found(fnarb01l) and
238200090804     c                             (arbcmc <> 'A16' and arbcmc <> 'A23')
238300131014     c     kwf           setgt     FIARBF2C
238400090731     c                   do        *hival
238500131014     c     kwf           readpe    FIARBF2C
238600131014     c                   if        %eof(fiarbf2c)
238700090731     c                   leave
238800090731     c                   end
238900090731     c* prendo le variazioni bolle fatte con il profilo fino alla data e ora
239000090731     c* della chiusura che sto facendo (escludo l'ultimo bite perchè
239100090731     c* c'è A x bolle arrivi e P per bolle in partenza)
239200090731     c                   if        %subst($ARBPRU: 1: 9) <>
239300090731     c                             %subst(knmus: 1: 9)
239400090731     c                   iter
239500090731     c                   end
239600090731     c                   movel     $arbdtv       com$arb
239700090731     c                   move      $arborv       com$arb
239800090731     c                   movel     varbdim       comvarb
239900090731     c                   move      varboim       comvarb
240000090731     c                   if        com$arb <comvarb
240100090731     c                   leave
240200090731     c                   end
240300090731     C                   CLEAR                   FNLR48
240400090731     C                   CLEAR                   DSARBD
240500090731     C                   CLEAR                   DSARBG
240600090731     C                   Z-ADD     $arbLNP       D48LNP
240700090731     C                   Z-ADD     $arbAAS       D48AAS
240800090731     C                   Z-ADD     $arbNRS       D48NRS
240900090731     C                   Z-ADD     $arbNSP       D48NSP
241000090731     C                   MOVEL     $arbcbo       D48CBO
241100090731     C                   MOVEL     $arbcvb       D48CVB
241200090731     C                   MOVEL     'E'           D48FFR
241300090731     C                   MOVEL     'G'           D48TRC
241400090731     C                   MOVEL     'A'           D48TBO
241500090731     C                   Z-ADD     $arbhcr       §BGHCR
241600090731     C                   Z-ADD     $arbdcr       §BGDCR
241700090731     C                   MOVEL     $arbtcr       §BGTCR
241800090731     C                   MOVEL     $arbgc1       §BGGC1
241900090731     C                   MOVEL     $arbgc2       §BGGC2
242000090731     C                   MOVEL     $arbtc1       §BGTC1
242100090731     C                   MOVEL     $arbtc2       §BGTC2
242200090731     C                   MOVEL     FNLR48        sKPJBU
242300090731     C                   CALL      'FNLR48R'
242400090731     C                   PARM                    KPJBAs
242500090731     C                   PARM                    DSARBD
242600090731     C                   PARM                    DSARBK
242700090731     C                   PARM                    DARBT
242800090731     C                   PARM                    DSARBG
242900090731     C                   PARM                    trul90ds
243000090731     C                   MOVEL     sKPJBU        FNLR48
243100090731     C     D48ERR        IFEQ      *BLANKS
243200090731     c                   end
243300090731     c                   enddo
243400090731     c                   endif
243500090804     c                   endif
243600090731     c                   enddo
243700090731     C*
243800090731     C                   ENDSR
243900071026     C*----------------------------------------------------*
244000071026     C*  AGGIORNO TOTALI
244100071026     C*----------------------------------------------------*
244200071026     C     AGGTO2        BEGSR
244300941117      *
244400941123     C     V2CNRD        IFGT      *ZEROS
244500941123     C                   MOVEL     'S'           WTRIMP
244600941123     C                   END
244700941117     C     V02STP        IFEQ      *ZEROS
244800941117     C                   ADD       1             V05STP
244900941117     C                   ELSE
245000941117     C     V02STP        LOOKUP    STP                                    95
245100941117     C     *IN95         IFEQ      '0'
245200941117     C                   ADD       1             V05STP
245300941117     C                   ADD       1             F
245400941117     C                   MOVE      V02STP        STP(F)
245500941117     C                   END
245600941117     C                   END
245700941117     C*
245800941118     C*  Se i record degli incassi erano stati in precedenza bloccati
245900941118     C*  provvedo a sbloccarli
246000941118     C     V2CBLO        IFEQ      'SI'
246100941118     C                   Z-ADD     NRR2          SAVREC
246200941207     C                   SETOFF                                       03
246300941118     C     V2CNRD        DO        V2CNRA        NRR3
246400120228     C     NRR3          CHAIN     lr53S03                            95
246500941118     C     *IN95         IFEQ      '0'
246600991007     C     V3DTPI        IFNE      'Ass'
246700991007     C     V3DTPI        ANDNE     'C/A'
246800991007     C                   SETON                                        05
246900991007     C                   ELSE
247000991007     C                   SETOFF                                       05
247100991007     C                   END
247200941118     C                   SETON                                        22
247300120308     C                   SETOFF                                       82
247400941129     C                   MOVEL     '  '          V3CNCO
247500941118     C                   MOVEL     ' '           V02SN1
247600941118     C                   MOVEL     '  '          V2CBLO
247700120308     c     v3hkey        comp      ' '                                8787
247800941118     C                   UPDATE    LR53S03
247900941118     C                   SETOFF                                       22
248000941207     C                   SETON                                        03
248100941118     C                   END
248200941118     C                   END
248300941118     C                   Z-ADD     SAVREC        NRR2
248400941124     C     NRR2          CHAIN     LR53S02                            95
248500941118     C                   END
248600941129     C*
248700941129     C                   MOVE      D53DSF        VIDDES
248800941118     C*
248900941117     C                   ENDSR
249000141201ab   C*------------------------------------------------------*
249100141201ab   C* cerca il CET per verificare la consegna
249200141201ab   C*------------------------------------------------------*
249300141201ab   C     Found_CET     BEGSR
249400141201ab   C*
249500141202ab   c                   clear                   ecco_il_CET       1
249600141201ab   C*  solo se PDA
249700141201ab   c                   clear                   fipctcetds
249800141201ab   c                   z-add     vnmdoc        vnmdoc6
249900141201ab   c                   move      'CET'         ttrd
250000141201ab   C*
250100141201ab   c     kcet          setgt     fipct02l
250200141201ab   c     kcet          readpe    fipct02l
250300141201ab   C*
250400141201ab   c                   if        not %eof(fipct02l)
250500141202ab   c                   eval         ecco_il_CET = 'S'
250600141201ab   c                   movel     pctdati       fipctcetds
250700141201ab   c                   endif
250800141201ab   C*
250900141201?ab  C                   ENDSR
251000141210ab   C*------------------------------------------------------*
251100141210ab   C* cerca la presenza di una R.A.(60)
251200141210ab   C*------------------------------------------------------*
251300141211ab   C     Found_RA_60   BEGSR
251400141210ab   C*
251500141210ab   c                   clear                   ecco_la_RA_60     1
251600141212ab   c                   clear                   autorizzata       1
251700141212ab   C*
251800141212ab   c                   eval      tgdTOR = 'S'
251900141212ab   c                   eval      tgdOGG = %editc(v2clnp:'X') +
252000141212ab   c                                      %editc(v2cnrs:'X') +
252100141212ab   c                                      %editc(v2cnsp:'X') +
252200141212ab   c                                      %editc(v2caas:'X')
252300141212ab   c                   eval      tgdMAD = ' 60'
252400141212ab    *
252500141212ab    * CERCA l'ultima inserita
252600141212ab    *  cronologicamente
252700141212ab   c     Key_TGD11     setgt     fitgd11l
252800141212ab   c     Key_TGD11     readpe    fitgd11l
252900141212ab   c                   dow       not %EoF(fitgd11l)
253000141212ab    *
253100141212ab   c                   eval        tgRANT = tgDANT
253200141212ab   c                   eval        tgRNUT = tgDNUT
253300141212ab   c                   eval        tgRPDT = tgDPDT
253400141212ab   c                   eval        tgRTRI = 'D'
253500141212ab   c     Key_TGR01     chain     fitgr01l
253600141212ab    *
253700141212ab    * se l'ha trovata ed è della distinta che sta trattando
253800141212ab    *  allora è proprio quello che stava cercando.
253900141212ab   c                   if        %Found(fitgr01l)
254000141212ab   c                   eval      dTGR01 = tgRRIF
254100141212ab    *
254200141212ab    *  Deve essere della stessa Distinta
254300141212ab    *      per poter verificare se Autorizzato
254400141212ab    *   altrimenti dovrà leggere una nuova testata.
254500141212ab   c                   if        §TGRNFV =  vnmdoc
254600141212ab   c                   eval       ecco_la_RA_60 = 'S'
254700141212ab    *
254800141212ab    *  se l'esito è Positivo
254900141212ab   c                   if        §TGRESITO = 'P'
255000141212ab    *  allora è autorizzata la consegna
255100141212ab   c                   eval       autorizzata   = 'S'
255200141212ab   c                   end
255300141212ab    * ed
255400141212ab    *  ESCE con i flag impostati
255500141212ab   c                   leave
255600141212ab   c                   end
255700141212ab    *
255800141212ab   c                   end
255900141212ab    *
256000141212ab    *  altrimenti dev provare con un'altra testata se c'è
256100141212ab   c     Key_TGD11     readpe    fitgd11l
256200141212ab   c                   enddo
256300141212ab    *
256400141210?ab  C                   ENDSR
256500141210ab   C*------------------------------------------------------*
256600141211ab   C* FINESTRA di alert e Forzatura
256700141210ab   C*------------------------------------------------------*
256800141211ab   C     WIN_FORZA     BEGSR
256900141212ab    **
257000141211ab   c                   clear                   fnlrq0ds
257100141211ab   c                   eval      lrq0aasI =  v2caas
257200141211ab   c                   eval      lrq0lnpI =  v2clnp
257300141211ab   c                   eval      lrq0nrsI =  v2cnrs
257400141211ab   c                   eval      lrq0nspI =  v2cnsp
257500141211ab   c                   eval      lrq0txtI =
257600141215ab   c                             'La spedizione è stata consegnata senza l''i-
257700141215ab   c                             mmisione del PINCODE.  F7 per procedere'
257800141211ab   c                   eval      lrq0tipI = 'PIN'
257900141211ab   c                   eval      lrq0pgmI = 'FNLR53R'
258000141211ab   c                   eval      lrq0frzIO= 'F'
258100141211ab   c                   call      'FNLRQ0R'
258200141211ab   c                   parm                    fnlrq0ds
258300141212ab    **
258400141211?ab  C                   ENDSR
258500141211ab   C*------------------------------------------------------*
258600141212ab   C* FINESTRA di INFO x apporre la dicitura o il Timbro sulla LDV
258700141211ab   C*------------------------------------------------------*
258800141211ab   C     WIN_INFO      BEGSR
258900141212ab    **
259000141211ab   c                   clear                   fnlrq0ds
259100141211ab   c                   eval      lrq0aasI =  v2caas
259200141211ab   c                   eval      lrq0lnpI =  v2clnp
259300141211ab   c                   eval      lrq0nrsI =  v2cnrs
259400141211ab   c                   eval      lrq0nspI =  v2cnsp
259500141211ab   c                   eval      %subst(lrq0txtI:1:50) =
259600141215ab   c                             'Apporre sulla LDV la scritta:'
259700141211ab   c                   eval      %subst(lrq0txtI:51:50) =
259800141211ab   c                             '"PINCODE verificato telefonicamente"'
259900141211ab   c                   eval      lrq0tipI = 'PIN'
260000141211ab   c                   eval      lrq0pgmI = 'FNLR53R'
260100141211ab   c                   eval      lrq0frzIO= 'I'
260200141211ab   c                   call      'FNLRQ0R'
260300141211ab   c                   parm                    fnlrq0ds
260400141212ab    **
260500141211?ab  C                   ENDSR
260600141211ab   C*------------------------------------------------------*
260700141211ab   C* scrive R.A.(61) dopo aver Forzato la Consegna (con PC PINCode)
260800141211ab   C*------------------------------------------------------*
260900141211ab   C     Write_RA_61   BEGSR
261000141212ab    **
261100141212ab    **    Data Distinta in *EUR (ddmmyyyy)
261200141212ab   c                   move      vh2ddt        dataiso
261300141212ab   c                   move      dataiso       dataeur
261400141212ab   c                   move      dataeur       gmaDDT            8 0
261500141212ab    **  Filiale
261600141212ab   c                   if        d53fgs = 0
261700141212ab   c                   z-add     d53lna        Filiale           3 0
261800141212ab   c                   else
261900141212ab   c                   z-add     d53fgs        Filiale
262000141212ab   c                   end
262100141212ab    **
262200141211ab    *    imposta la DS di scrittura
262300141212ab   c                   clear                   fidna6ds
262400141212ab   c                   eval      ida06mad = ' 61'
262500141212ab   c                   eval      ida06tor = 'S'
262600141212ab   c                   eval      ida06ogg = %editc(v2clnp:'X') +
262700141212ab   c                                        %editc(v2cnrs:'X') +
262800141212ab   c                                        %editc(v2cnsp:'X') +
262900141212ab   c                                        %editc(v2caas:'X')
263000141212ab   c                   eval      ida06ute = uteute
263100141212ab   c                   eval      ida06fil = utefil
263200141212ab   c                   eval      ida06no2 =
263300141218ab   c                             'DIST: ' + %editc(Filiale:'X')+ ' '  +
263400141218ab   c                                  %trim(%editc(vnmdoc:'Z'))+
263500141218ab   c                             ' DEL ' + %editw(gmaDDT:'  /  /    ') +
263600141218ab   c                             '  AUT: ' + %editc(vh2pdr:'X')
263700141212ab   c                   eval      ida06rsc = 'Automatica da Chius.Dist.'
263800141212ab   c                   call      'FIDNA6R'
263900141212ab   c                   parm                    fidna6ds
264000141210ab   C*
264100141210?ab  C                   ENDSR
264200941117     C*------------------------------------------------------*
264300941117     C* GESTIONE FINESTRA PER GIORNI CHIUSURA DESTINATARIO
264400941117     C*------------------------------------------------------*
264500941117?    C     TURNO         BEGSR
264600941117     C*
264700101020     C                   SETOFF                                       32
264800941128     C                   MOVE      V2CAAS        V9CAAS
264900941128     C                   MOVE      V2CLNP        V9CLNP
265000941128     C                   MOVE      V2CNRS        V9CNRS
265100941128     C                   MOVE      V2CNSP        V9CNSP
265200941117     C                   MOVEL     *BLANKS       WINMSG
265300141202      ***
265400101123      * se distinta pda imposta eventuali turni rilevati dall'autista su pda
265500101123     c                   if        wpda = 'S'
265600141202      ***
265700101123     c                   exsr      impturno
265800141202      ***
265900101123      * turno trovato da impostare verifico fra primo e secondo quale è libero se
266000101123      * pieni entrambi copro il primo
266100101123     c                   if        wtur <> *blank
266200130709      * verifica che il turno inserito dal PDA non sia già presente
266300130709     c                   if        wtur <> w1gc and
266400130709     c                             wtur <> w2gc
266500101123     c                   if        w1gc = *blank
266600101123     c                   move      wtur          w1gc
266700101123     c                   else
266800101123     c                   if        w2gc = *blank
266900101123     c                   move      wtur          w2gc
267000101123     c                   else
267100101123     c                   move      wtur          w1gc
267200101123     c                   endif
267300130709     c                   endif
267400130709      * se inserito il turno verifica il primo bite dei campi se è uguale
267500130709      * pulisce il secondo campo e il secondo byte del primo
267600130709      * esempio se ci trovassimo con 1M e 1P vengono trasformati in '1 ' e '  '
267700130709     c                   if        %subst(w1gc:1:1) = %subst(w2gc:1:1)
267800130709     c                   clear                   w2gc
267900130709     c                   move      ' '           w1gc
268000130709     c                   endif
268100130709     c                   endif
268200101123     c                   endif
268300101123     c                   endif
268400101123      *
268500941117     C     *IN32         DOUEQ     '0'
268600941128     C   32              EXFMT     LR53W08
268700941125     C                   EXFMT     LR53W09
268800941117     C                   SETOFF                                       32
268900941117     C                   MOVEL     *BLANKS       WINMSG
269000941117     C  NKL              DO
269100941117     C     W1GC          IFEQ      *BLANKS
269200941117     C     W2GC          ANDEQ     *BLANKS
269300941117     C     V021GC        ANDEQ     *BLANKS
269400941117     C     V022GC        ANDEQ     *BLANKS
269500941117     C                   SETON                                        32
269600941125     C                   MOVEL     ERR(54)       WINMSG
269700941117     C                   END
269800941117     C     DSG1          IFGT      '7'
269900941117     C     DSG2          ORGT      '7'
270000941117     C     DSG1          ORLT      '1'
270100941117     C     DSG1          ANDNE     *BLANKS
270200941117     C     DSG2          ORLT      '1'
270300941117     C     DSG2          ANDNE     *BLANKS
270400941117     C                   SETON                                        32
270500941125     C                   MOVEL     ERR(54)       WINMSG
270600941117     C                   END
270700941117     C     DSMP1         IFNE      *BLANKS
270800941117     C     DSMP1         ANDNE     'P'
270900941117     C     DSMP1         ANDNE     'M'
271000941117     C     DSMP2         ORNE      *BLANKS
271100941117     C     DSMP2         ANDNE     'P'
271200941117     C     DSMP2         ANDNE     'M'
271300941117     C                   SETON                                        32
271400941125     C                   MOVEL     ERR(54)       WINMSG
271500941117     C                   END
271600941117     C     W1GC          IFEQ      W2GC
271700941117     C     W1GC          ANDNE     *BLANKS
271800941117     C                   SETON                                        32
271900941125     C                   MOVEL     ERR(54)       WINMSG
272000941117     C                   END
272100941117     C                   END
272200941117     C                   END
272300941117     C*
272400941117     C* Memorizzo numero relativo di record dove avevo richiesto la finestra
272500140313     c                   move      w1gc          v2cgc1
272600140313     c                   move      w2gc          v2cgc2
272700941117     C                   Z-ADD     NRR2          NR2KEY
272800941117     C*
272900941117?    C                   ENDSR
273000101123     C*------------------------------------------------------*
273100101123     C* reperisce turni chiusura rilevati da autista su pda
273200101123     C*------------------------------------------------------*
273300101123     C     impturno      BEGSR
273400101123     c                   clear                   wtur              2
273500101123      * non consegnato
273600140219     c                   z-add     1             tur               3 0
273700101123     c     §pctcmc       lookup    c2a(tur)                               92
273800101123     c                   if        *in92
273900101123     c                   movel     d2a(tur)      ds2atur
274000101124     c                   if        y§2acpt = 'S'
274100101123     c                   exsr      impgior
274200101123     c                   movel     §pctcmc       wtur
274300101123     c                   else
274400101123      * tentativo precedente
274500140219     c                   z-add     1             tur               3 0
274600101123     c     §pctcmcp      lookup    c2a(tur)                               92
274700101123     c                   if        *in92
274800101123     c                   movel     d2a(tur)      ds2atur
274900101124     c                   if        y§2acpt = 'S'
275000101123     c                   exsr      impgior
275100101123     c                   movel     §pctcmcp      wtur
275200101123     c                   endif
275300101123     c                   endif
275400101123     c                   endif
275500101123     c                   endif
275600101123     c                   movel     Wgioset       wtur
275700101123     C*
275800101123     c                   endsr
275900101123     C*------------------------------------------------------*
276000101123     C* ricerca giorno della settimana
276100101123     C*------------------------------------------------------*
276200101123     C     impgior       BEGSR
276300101123     c     *iso          move      vh2ddc        wData_Dmy
276400101123      * Reperimento del giorno della settimana da stampare
276500101123     c                   move      wData_Dmy     wData_6
276600101123     c                   move      wData_6       Wggmmaa
276700101123     c                   call      'XGIOSE1'
276800101123     c                   parm                    Wggmmaa           6
276900101123     c                   parm      *blanks       Wgioset           1
277000101123      *
277100101123     c                   endsr
277200040216     C*------------------------------------------------------*
277300040216     C* GESTIONE FINESTRA PER data consegna richiesta
277400040216     C*------------------------------------------------------*
277500040216     C     vidrich       BEGSR
277600040216     C*
277700040216     C                   MOVE      V2CAAS        V9CAAS
277800040216     C                   MOVE      V2CLNP        V9CLNP
277900040216     C                   MOVE      V2CNRS        V9CNRS
278000040216     C                   MOVE      V2CNSP        V9CNSP
278100040216     C                   MOVEL     *BLANKS       WINMSG
278200101015     c
278300101015     c*                  if        *in66
278400101015     c*                  move      v13tcr        w13doil
278500101015     c*                  endif
278600101021     c                   movel     v2ctcr        v13tcr
278700101021     c                   if        v13tcr = *blank
278800101021     c                   move      '  Il'        v13tcrd
278900101021     c                   else
279000101021     c                   move      'Dopo'        v13tcrd
279100101021     c                   endif
279200101021     c*
279300040220     c                   if        v2cdcr > 0
279400040220     C                   z-add     0             G02DAT
279500040220     C                   Z-ADD     v2cdcr        G02INV
279600040220     C                   MOVEL     '3'           G02ERR
279700040220     C                   CALL      'XSRDA8'
279800040220     C                   PARM                    WLBDA8
279900040220     C                   z-add     G02DAT        v13dcr
280000101214     C*                  z-add     G02inv        w13dcrg
280100101015     c                   movel     v2chcr        v13hcr
280200040220     c                   else
280300040217     c                   clear                   v13dcr
280400101214     c*                  clear                   w13dcrg
280500101015     c                   clear                   v13hcr
280600101015     c                   setoff                                       66
280700040220     c                   endif
280800101021     c*
280900040216     C                   DO        *hival
281000040216     C   32              EXFMT     LR53W08
281100040216     C                   EXFMT     LR53W13
281200040330     c                   setoff                                       32
281300040330     C                   MOVEL     *BLANKS       WINMSG
281400040216
281500040216     C     v13dcr        IFEQ      0
281600040216     C                   SETON                                        32
281700040216     C                   MOVEL     ERR(90)       WINMSG
281800040216     c                   iter
281900040216     C                   END
282000040216     C     v13dcr        IFGT      0
282100040216     C                   MOVE      v13dcr        G02DAT
282200040216     C                   Z-ADD     0             G02INV
282300040216     C                   MOVEL     *BLANKS       G02ERR
282400040216     C                   CALL      'XSRDA8'
282500040216     C                   PARM                    WLBDA8
282600040216     C     G02ERR        IFEQ      '1'
282700040216     C                   SETON                                          32
282800040216     C                   MOVEL     ERR(90)       winmsg
282900040216     C                   iter
283000040216     C                   ELSE
283100101214     C*                  Z-ADD     G02INV        W13dcrg           8 0
283200040216     C                   MOVEL     G02DAT        V13dcr
283300040216     C                   END
283400101015     C*                  IF        w13dcrg< oggi
283500101015     C                   IF        g02inv < oggi
283600040216     C                   SETON                                        32
283700040216     C                   MOVEL     ERR(91)       WINMSG
283800040216     c                   iter
283900040216     C                   END
284000101015     C*                  IF        w13dcrg > wvpodcr
284100101015     C                   IF        g02inv  > wvpodcr
284200040330     C                   SETON                                        32
284300040330     C                   MOVEL     ERR(92)       WINMSG
284400040330     c                   iter
284500040330     C                   END
284600040216     C                   END
284700040702      *uscita solo con data valorizzata
284800101015     c*                  if        *inkj and w13dcrg > 0 and not *in32
284900101015     c                   if        *inkj and g02inv > 0 and not *in32
285000101021     c                   eval      v2cdcr = g02inv
285100040702     c                   leave
285200040702     c                   endif
285300040216     c                   enddo
285400040216     C*
285500040216     C* Memorizzo numero relativo di record dove avevo richiesto la finestra
285600040216     C                   Z-ADD     NRR2          NR2KEY
285700040216     C*
285800040216?    C                   ENDSR
285900941117     C*------------------------------------------------------*
286000941117     C* GESTIONE DEL FORMATO DEI RIMBORSI E NOTE
286100941117     C*------------------------------------------------------*
286200941117?    C     GESRMB        BEGSR
286300941117     C*
286400941117     C* Valorizzo i campi dal subile
286500941117     C                   MOVEL     V2CFIR        V4CFIR
286600941117     C                   MOVEL     V2CNT1        V4CNT1
286700101111     C                   MOVEL     V2Clnp        V4Clnp
286800101111     C                   MOVEL     V2Cnrs        V4Cnrs
286900101111     C                   MOVEL     V2Cnsp        V4Cnsp
287000101111     C                   MOVEL     V02des        V4Cdes
287100990923     C                   SETOFF                                       28
287200941117     C* Gestione videata
287300941117     C                   EXSR      GESD04
287400941117     C*
287500941117     C* VALORIZZO I CAMPI DEL SUBFILE
287600941125     C                   SETOFF                                       28
287700941117     C     *INKL         IFEQ      '0'
287800941117     C                   MOVE      V4CFIR        V2CFIR
287900941117     C                   MOVE      V4CNT1        V2CNT1
288000941117     C                   END
288100941117     C*
288200941117?    C                   ENDSR
288300941117     C*------------------------------------------------------*
288400941117     C* GESTIONE VIDEATA RIMBORSI/NOTE
288500941117     C*------------------------------------------------------*
288600941117?    C     GESD04        BEGSR
288700941117     C*
288800941123     C                   EXFMT     LR53D04
288900990923     C                   SETOFF                                       28
289000941125     C* Fine Rimborsi
289100941125     C   KL              GOTO      FINVD4
289200090216     C                   if        *inkr
289300140210     c                   clear                   fnlrn6ds
289400090216     C                   movel     kpjbu         WSVKPJ          256
289500140210     C                   move      'I'           in6mod
289600140210     C                   z-add     V2CAAS        in6AAS
289700140210     C                   z-add     V2CLNP        in6LNP
289800140210     C                   z-add     V2CNRS        in6NRS
289900140210     C                   z-add     V2CNSP        in6NSP
290000140210     C                   movel     v4cnt1        IN6NOT1
290100140210     C                   movel     'FNLR53R'     in6pgm
290200140210     C                   movel     'S'           IN6CHDIST
290300140210     C                   call      'FNLRN6R'                            98
290400090216     C                   parm                    kpjba
290500140210     C                   parm                    fnlrn6ds
290600090216     C                   movel     WSVKPJ        kpjbu
290700090216     C                   MOVEL     ' '           V02SEL
290800090216     C                   Z-ADD     NRR2          NR2KEY
290900090216     c                   endif
291000100223     C                   MOVE      'X'           V02SELb
291100941117     C*
291200941117     C     FINVD4        ENDSR
291300110908     C*------------------------------------------------------*
291400110908     C* GESTIONE DEL FORMATO 7 visualizza particolarità consegna documenti
291500110908     C*------------------------------------------------------*
291600110908?    C     GESvid7       BEGSR
291700110908     C*
291800110908     C* Valorizzo i campi dal subile
291900110908     C                   MOVEL     V2Clnp        V7Clnp
292000110908     C                   MOVEL     V2Cnrs        V7Cnrs
292100110908     C                   MOVEL     V2Cnsp        V7Cnsp
292200110908     C                   MOVEL     V02des        V7Cdes
292300110908     c                   movel     vh2rsd        v7dest
292400110908     c                   movel     vh2rsm        v7mitt
292500110908     c                   exsr      RepDesPar
292600110908     C* Gestione videata
292700110908     C                   EXSR      GESvid7b
292800110908     C*
292900110908     C*
293000110908?    C                   ENDSR
293100110908     C*------------------------------------------------------*
293200110908     C* GESTIONE VIDEATA con particolarità consegna x documenti
293300110908     C*------------------------------------------------------*
293400110908?    C     GESvid7b      BEGSR
293500110908     C*
293600110908     C                   EXFMT     LR53D07
293700110908     C                   SETOFF                                       28
293800110908     C                   Z-ADD     NRR2          NR2KEY
293900110909     C                   MOVE      'D'           V02SELd
294000110908     C*
294100110914     C                   ENDSR
294200110908     C*------------------------------------------------------*
294300110908?    C* reperimento descrizione particolarità
294400110908     C*------------------------------------------------------*
294500110908?    C     repdespar     BEGSR
294600110908     c                   clear                   v7despa
294700110908     C                   MOVEL     '7V'          KCOD
294800110908     C                   eval      KKEY = 'R' + v2cgma + 'V0001'
294900110908     C     KTAB2         setll     TABEL00F
295000110908     c                   do        *hival
295100110908     c                   read      tabel00f
295200110908     c                   if        %eof(tabel00f) or
295300110908     c                             %subst(tblkey:2:2) <> v2cgma
295400110908     c                   leave
295500110908     c                   endif
295600110908     c                   movel     tbluni        ds7v
295700110908     c                   eval      v7despa =%trim(v7despa)+ §7vnot
295800110908     C                   enddo
295900110908
296000110908     C                   endsr
296100941117     C*------------------------------------------------------*
296200941117     C* GESTIONE CONSEGNA PARZIALE
296300941117     C*------------------------------------------------------*
296400941117?    C     GESPAR        BEGSR
296500941117     C*
296600970304     C                   SETON                                        6301
296700941123     C                   WRITE     LR53C06
296800941117     C                   Z-ADD     0             NRR6
296900941128     C                   SETOFF                                       6388
297000941117     C*
297100941117     C     V2CPDS        IFEQ      0
297200941117?    C                   EXSR      CARS06
297300941117     C                   ELSE
297400941117?    C                   EXSR      RCRS06
297500941117     C                   END
297600941117     C*
297700941117     C     NRR6          IFNE      0
297800941205     C     *INKA         DOUNE     '0'
297900941205     C     *INKL         ORNE      '0'
298000941205     C     *INKG         ORNE      '0'
298100941205     C     *INKQ         ORNE      '0'
298200941205     C     *IN88         DOUEQ     '0'
298300941117?    C                   EXSR      GESS06
298400941117     C                   END
298500941205     C                   END
298600941117     C                   ELSE
298700941117     C                   SETON                                        2840
298800941205     C                   MOVEL     ERR(62)       $MSG
298900941117     C                   END
299000941117     C*
299100941117     C* Memorizzo numero relativo di record dove avevo richiesto la finestra
299200941117     C                   Z-ADD     NRR2          NR2KEY
299300941123     C     NRR2          CHAIN     LR53S02                            89
299400941117     C     *INKL         IFEQ      '0'
299500941117     C     CONT          IFGT      0
299600941117     C     NRR6          ANDGT     0
299700941117     C     *INKL         ANDEQ     '0'
299800941117     C     *INKQ         ANDEQ     '0'
299900941117     C                   MOVEL     'P'           V02CPZ
300000941128     C                   ELSE
300100941128     C     *INKQ         IFEQ      '1'
300200941128     C                   MOVEL     ' '           V02CPZ
300300941128     C                   END
300400941128     C                   END
300500941117     C     V2CPDS        IFEQ      0
300600941117     C                   Z-ADD     POSIZ         V2CPDS
300700941117     C                   END
300800941117     C                   END
300900110117     C  nkl              MOVEL     *BLANKS       V02CCN
301000941117     C*
301100941117?    C                   ENDSR
301200970304     C*------------------------------------------------------*
301300970304     C* Colli persi definitivamente --> chiudo bolla con IDD
301400970304     C*------------------------------------------------------*
301500970620?    C     GESIDD        BEGSR
301600970304     C*
301700970304     C                   SETON                                        63
301800970304     C                   SETOFF                                       01
301900970304     C                   WRITE     LR53C06
302000970304     C                   Z-ADD     0             NRR6
302100970304     C                   SETOFF                                       6388
302200970304     C*
302300970304     C     V2CPDS        IFEQ      0
302400970304?    C                   EXSR      CARS06
302500970304     C                   ELSE
302600970304?    C                   EXSR      RCRS06
302700970304     C                   END
302800970304     C*
302900970304     C     NRR6          IFNE      0
303000970304     C     *INKA         DOUNE     '0'
303100970304     C     *INKL         ORNE      '0'
303200970304     C     *INKG         ORNE      '0'
303300970304     C     *INKQ         ORNE      '0'
303400970304     C     *IN88         DOUEQ     '0'
303500970304?    C                   EXSR      GESS06
303600970304     C                   END
303700970304     C                   END
303800970311     C                   ELSE
303900970304     C                   SETON                                        2840
304000970304     C                   MOVEL     ERR(67)       $MSG
304100970304     C                   END
304200970304     C*
304300970304     C* Memorizzo numero relativo di record dove avevo richiesto la finestra
304400970304     C                   Z-ADD     NRR2          NR2KEY
304500970304     C     NRR2          CHAIN     LR53S02                            89
304600970304     C     *INKL         IFEQ      '0'
304700970305     C     CONT          IFNE      TOTCAR
304800970304     C     NRR6          ANDGT     0
304900970304     C     *INKL         ANDEQ     '0'
305000970304     C     *INKQ         ANDEQ     '0'
305100970311     C                   MOVEL     §2AIDD        V2CIDD
305200970311     C                   MOVEL     '5'           V02CCA
305300970311     C                   MOVEL     V02CCN        V2CEVD
305400970304     C                   ELSE
305500970304     C     *INKQ         IFEQ      '1'
305600970311     C                   MOVEL     *BLANKS       V2CEVD
305700970311     C                   MOVEL     ' '           V2CIDD
305800970311     C                   MOVEL     ' '           V02CCA
305900970311     C                   SETOFF                                       75
306000970304     C                   END
306100970304     C                   END
306200970304     C     V2CPDS        IFEQ      0
306300970304     C                   Z-ADD     POSIZ         V2CPDS
306400970304     C                   END
306500970304     C                   END
306600970304     C                   MOVEL     *BLANKS       V02CCN
306700970304     C*
306800970311?    C                   ENDSR
306900941117     C*------------------------------------------------------*
307000941117     C* CARICAMENTO DETTAGLIO SEGNACOLLI DA ARCHIVIO
307100941117     C*------------------------------------------------------*
307200941118?    C     CARS06        BEGSR
307300980402      *
307400980402      * Se c'è legame utilizzo il legame per agganciare il dettaglio
307500941117     C                   SETOFF                                       15
307600980402     C                   Z-ADD     V2CAAS        V6CAAS
307700980402     C                   Z-ADD     V2CLNP        V6CLNP
307800980402     C                   Z-ADD     V2CNRS        V6CNRS
307900980402     C                   Z-ADD     V2CNSP        V6CNSP
308000980402     C                   Z-ADD     V2CFLS        SFWFLS
308100980402      *
308200941129     C                   Z-ADD     HARAAS        KAAS1
308300941129     C                   Z-ADD     HARLNP        KLNP1
308400941129     C                   Z-ADD     HARNRS        KNRS1
308500941129     C                   Z-ADD     HARNSP        KNSP1
308600941117     C     KART1         CHAIN     FNART000                           26
308700980402      *
308800941117     C     *IN26         DOWEQ     '0'
308900941128     C     ARTDAM        COMP      0                                      27
309000980402      *
309100941128     C     ARTDCM        IFEQ      0
309200941123     C                   Z-ADD     ARTNSC        SFWSCN
309300941117     C     ARTDAM        IFEQ      0
309400941117     C                   MOVEL     'N'           SFWARR
309500941117     C                   ELSE
309600941117     C                   MOVEL     *BLANKS       SFWARR
309700941117     C                   END
309800980402      * Per estero verifico se preimpostare la scelta
309900980402     C                   MOVEL     *BLANKS       SELEEX            1
310000980402     C     D53ITA        IFEQ      'E'
310100980402     C                   EXSR      SCES06
310200980402     C                   END
310300980402     C                   MOVEL     SELEEX        SFWSEL
310400080723      * distinte PDA preimposta scelta nel SFL
310500080723     c                   if        wpda = 'S'
310600081111     c                   clear                   SFWSEL
310700080723      * imposta scelta nel sfl Se: trova spunta 8
310800080723      * oppure non trova la spunta verifica dettaglio ritorno da pda
310900080723      * imposta scelta nel sfl Se: non trova dettaglio
311000080723      *                            trova il dettaglio ed è spuntato al rientro
311100080723      *                            trova il dettaglio e non è spuntato in consegna
311200080714     c                   z-add     vnmdoc        wdisbrv
311300080714     c     kbrv          chain     fnbrv01l
311400080714     c                   if        %found(fnbrv01l)
311500080714     c                   eval      SFWSEL= '1'
311600080723     c                   else
311700080805     c     kced          chain     fipcd03l
311800080805     c                   if        %found(fipcd03l)
311900080723     c                   movel     pcddati       fipcdcedds
312000080723     c                   if        §PCDSPUSPC <> *blank or
312100080723     c                             §PCDSPUcon <> 'S'
312200080723     c                   eval      SFWSEL= '1'
312300080723     c                   endif
312400101115     c                   else
312500101115     c                   eval      SFWSEL= '1'
312600080723     c                   endif
312700080714     c                   endif
312800080723     c                   endif
312900980402      *
313000941117     C                   Z-ADD     0             SFWNRH
313100941117     C                   ADD       1             NRR6
313200941118     C                   WRITE     LR53S06
313300941117     C                   END
313400980402      *
313500941117     C     KART1         READE     FNART000                               26
313600941117     C                   END
313700980402      *
313800941123     C                   Z-ADD     NRR6          TOTCAR                         TOT.COLLI CARICATI
313900980402      *
314000941117?    C                   ENDSR
314100980402      *------------------------------------------------------------------------*
314200980402      * PER ESTERO DETERMINO SE PREIMPOSTARE LA SELEZIONE PER DETTAGLIO SEGNAC.
314300980402      *------------------------------------------------------------------------*
314400980402?    C     SCES06        BEGSR
314500980402      *
314600980402      * Ricerco se esiste il segnacollo Euroexpress
314700980402     C                   Z-ADD     ARTFLS        KFLS
314800980402     C                   Z-ADD     ARTLNA        KLNA
314900980402     C                   Z-ADD     ARTNSC        KNSC
315000980402     C                   MOVEL     'E'           KTRC
315100051027     C     KARS0         CHAIN     FIARS000                           33
315200980402      *
315300980402     C     *IN33         IFEQ      '0'
315400980402      *
315500980402      * Ricerco se è fra quelli selezionati
315600980402     C                   Z-ADD     NDC(1)        KNDC
315700980402     C     KCDS0         CHAIN     FNCDS000                           33
315800980402      *
315900980402     C     *IN33         DOWEQ     '0'
316000980402     C     CDSSEE        IFEQ      ARSNOT
316100980402     C                   SETON                                        33
316200980402     C                   MOVEL     1             SELEEX
316300980402     C                   END
316400980402     C  N33KCDS0         READE     FNCDS000                               33
316500980402     C                   END
316600980402      *
316700980402     C                   END
316800980402      *
316900980402?    C                   ENDSR
317000980402     C*------------------------------------------------------*
317100980402     C* CARICAMENTO DETTAGLIO SEGNACOLLI DA SUB-FILE HIDDEN
317200980402     C*------------------------------------------------------*
317300980402?    C     RCRS06        BEGSR
317400980402     C*
317500941123     C                   Z-ADD     V2CAAS        V6CAAS
317600941123     C                   Z-ADD     V2CLNP        V6CLNP
317700941123     C                   Z-ADD     V2CNRS        V6CNRS
317800941123     C                   Z-ADD     V2CNSP        V6CNSP
317900941117     C                   Z-ADD     V2CFLS        SFWFLS
318000941117     C*
318100941123     C                   Z-ADD     V2CPDS        XX                4 0
318200941123     C     XX            CHAIN     LR53S07                            94
318300941117     C*
318400941117     C     XX            DOWLE     TOTSFH
318500941123     C     *IN94         ANDEQ     '0'
318600941117     C     HARAAS        ANDEQ     SFHAAS
318700941117     C     HARLNP        ANDEQ     SFHLNP
318800941117     C     HARNRS        ANDEQ     SFHNRS
318900941117     C     HARNSP        ANDEQ     SFHNSP
319000941117     C                   MOVEL     SFHSEL        SFWSEL
319100941117     C                   Z-ADD     SFHSCN        SFWSCN
319200941117     C                   MOVEL     SFHARR        SFWARR
319300941117     C     SFWARR        COMP      'N'                                    27
319400941117     C                   Z-ADD     XX            SFWNRH
319500941117     C                   ADD       1             NRR6
319600941117     C                   WRITE     LR53S06
319700941117     C                   ADD       1             XX
319800941123     C     XX            CHAIN     LR53S07                            94
319900941117     C                   END
320000941117     C*
320100941117     C                   Z-ADD     NRR6          TOTCAR                         TOT.COLLI CARICATI
320200941117     C*
320300941117?    C                   ENDSR
320400941117     C*------------------------------------------------------*
320500941117     C* AGGIORNAMENTO SUB-FILE HIDDEN DETTAGLIO SEGNACOLLI
320600000421     C*-------------------------
320700941118?    C     AGGS07        BEGSR
320800941117     C*
320900941117     C                   Z-ADD     0             POSIZ
321000941117     C                   Z-ADD     HARAAS        SFHAAS
321100941117     C                   Z-ADD     HARLNP        SFHLNP
321200941117     C                   Z-ADD     HARNRS        SFHNRS
321300941117     C                   Z-ADD     HARNSP        SFHNSP
321400941117     C                   Z-ADD     V2CFLS        SFHFLS
321500941123     C                   DO        TOTCAR        KJ
321600941117     C     KJ            CHAIN     LR53S06                            89
321700941117     C  N89V2CPDS        IFEQ      0
321800941117     C                   ADD       1             NRR7
321900941123     C                   ADD       1             TOTSFH
322000941117     C     POSIZ         IFEQ      0
322100941123     C                   Z-ADD     NRR7          POSIZ
322200941117     C                   END
322300941128     C                   MOVEL     SFWSEL        SFHSEL
322400941128     C                   Z-ADD     SFWSCN        SFHSCN
322500941128     C                   MOVEL     SFWARR        SFHARR
322600941117     C                   WRITE     LR53S07
322700941117     C                   ELSE
322800941123     C     SFWNRH        CHAIN     LR53S07                            89
322900941117     C   KQ              MOVEL     *BLANKS       SFWSEL
323000941117     C                   MOVEL     SFWSEL        SFHSEL
323100941117     C                   Z-ADD     SFWSCN        SFHSCN
323200941117     C                   MOVEL     SFWARR        SFHARR
323300941117     C  N89              UPDATE    LR53S07
323400941117     C                   END
323500941117     C                   END
323600941117     C*
323700941117?    C                   ENDSR
323800941117     C*------------------------------------------------------*
323900941117     C* GESTIONE SUB-FILE DETTAGLIO SEGNACOLLI
324000941117     C*------------------------------------------------------*
324100941117?    C     GESS06        BEGSR
324200941117     C*
324300941128     C   88              EXFMT     LR53W08
324400941117     C                   WRITE     LR53Z06
324500941117     C                   EXFMT     LR53C06
324600941117     C                   SETOFF                                       272888
324700941117     C*
324800941117     C   KL              GOTO      FINVD6
324900970305     C* Tutti: seleziono tutti i record del subfile
325000970805     C     *INKB         IFEQ      '1'
325100970805     C                   Z-ADD     1             NRR6
325200970805     C     NRR6          CHAIN     LR53S06                            89
325300970805     C     *IN89         DOWEQ     '0'
325400970805     C                   MOVEL     '1'           SFWSEL
325500970805     C                   UPDATE    LR53S06
325600970805     C                   ADD       1             NRR6
325700970805     C     NRR6          CHAIN     LR53S06                            89
325800970805     C                   END
325900970805     C                   GOTO      FINVD6
326000970805     C                   END
326100970305     C*
326200941123     C                   Z-ADD     0             CONT
326300941123     C                   Z-ADD     1             JX                4 0
326400941118     C     JX            CHAIN     LR53S06                            89
326500941117     C     *IN89         DOWEQ     '0'
326600941123     C     *IN88         ANDEQ     '0'
326700941117     C     JX            ANDLE     TOTCAR
326800941128     C     SFWSEL        IFNE      '1'
326900941117     C                   ADD       1             CONT
327000941117     C     SFWARR        IFEQ      'N'
327100970805     C     §2AIDD        ANDNE     'P'
327200941117     C  NKG
327300941117     CANNKQ              SETON                                        282788
327400941128     C  NKG
327500941128     CANNKQ              MOVEL     ERR(56)       WINMSG
327600941117     C                   END
327700941118     C                   UPDATE    LR53S06
327800941117     C                   END
327900941117     C                   ADD       1             JX
328000941118     C     JX            CHAIN     LR53S06                            89
328100941117     C                   END
328200941117     C*
328300970620     C  NKQ
328400970620     CANN88
328500970620     CAN 01CONT          IFEQ      TOTCAR
328600941117     C     CONT          OREQ      0
328700941117     C                   SETON                                            88    SE CONS. TUTTI ERROR
328800941128     C                   MOVEL     ERR(57)       WINMSG
328900941117     C                   END
329000970805     C  NKQ
329100970805     CANN88
329200970805     CANN01CONT          IFEQ      TOTCAR
329300970805     C                   SETON                                            88    SE NESSUNO ERRORE
329400970805     C                   MOVEL     ERR(69)       WINMSG
329500970805     C                   END
329600970805     C  NKQ
329700970805     CANN88
329800970805     CANN01CONT          IFEQ      0
329900970805     C     §2AIDD        ANDEQ     'P'
330000970805     C                   SETON                                            88    SE NESSUNO ERRORE
330100970805     C                   MOVEL     ERR(71)       WINMSG
330200970805     C                   END
330300941117     C* Se richiesto annullamento consegna parziale controllo che lo fosse
330400970304     C   KQ
330500970304     CAN 01V02CPZ        IFNE      'P'
330600941117     C                   SETON                                          88
330700941128     C                   MOVEL     ERR(58)       WINMSG
330800941117     C                   END
330900970304     C* Se richiesto annullamento pratica IDD controllo di averla immessa
331000970805     C   KQ
331100970805     CANN01V2CIDD        IFEQ      ' '
331200970805     C                   SETON                                          88
331300970805     C                   MOVEL     ERR(68)       WINMSG
331400970805     C                   END
331500941117     C* Se ci sono errori vado a fine pgm
331600941117     C   88              GOTO      FINVD6
331700941117     C*
331800941128     C     *INKA         IFEQ      '1'
331900941117     C     *INKQ         OREQ      '1'
332000941117     C     *INKG         OREQ      '1'
332100941118?    C                   EXSR      AGGS07
332200941117     C                   END
332300941117     C*
332400941118     C     FINVD6        ENDSR
332500941118      *---------------------------------------------------------*
332600941118      *  Controlli sulla terza videata                          *
332700941118      *---------------------------------------------------------*
332800941118     C     CTR03         BEGSR
332900941118      *  Se ho chiesto la conferma leggo tutti i record e calcolo
333000941118      *  i totali
333100941207     C                   Z-ADD     0             WSVAAS
333200941207     C                   Z-ADD     0             WSVLNP
333300941207     C                   Z-ADD     0             WSVNRS
333400941207     C                   Z-ADD     0             WSVNSP
333500991007     C                   Z-ADD     0             WTOT1
333600941212     C                   MOVEL     *BLANKS       WINTOT
333700941118     C     *INKF         IFEQ      '1'
333800941118     C                   EXSR      AZZTOT
333900941118     C     1             CHAIN     LR53S03                            96
334000941118     C                   ELSE
334100941118     C                   READC     LR53S03                                96
334200941118     C                   END
334300941118     C*  Loop letture
334400941118     C                   Z-ADD     1             NR3KEY
334500991008     C     *IN96         DOWEQ     '0'                                          -- 01 --
334600941118     C     *IN28         ANDEQ     '0'
334700941118     C                   SETOFF                                       404142
334800941118     C                   SETOFF                                       434445
334900941207     C                   SETOFF                                       820503
335000120308     C                   SETOFF                                       4951
335100941121     C                   MOVEL     *BLANKS       AGGI              1
335200120302     c************ NO CONSEGNA ****************
335300991008     C     V3CNCO        IFEQ      'SI'                                         -- 02 --
335400941129     C                   SETON                                        82
335500991007     C     V3DTPI        IFNE      'C/A'
335600991007     C     V3DTPI        ANDNE     'Ass'
335700991007     C                   SETON                                        05
335800991007     C                   ELSE
335900991007     C                   SETOFF                                       05
336000991007     C                   END
336100991130     C* Azzero dati spedizione se modificata
336200991130     C     V3CAAS        IFNE      WSVAAS
336300991130     C     V3CLNP        ORNE      WSVLNP
336400991130     C     V3CNRS        ORNE      WSVNRS
336500991130     C     V3CNSP        ORNE      WSVNSP
336600991130     C                   EXSR      AZZSPE
336700991130     C                   END
336800120302     c************ CONSEGNA ****************
336900991008     C                   ELSE                                                   -- 02 --
337000990924     C                   MOVEL     'S'           WFLV              1
337100941128     C                   Z-ADD     1             X
337200941128     C     VNMDOC        LOOKUP    NDC(X)                                 95
337300950102     C                   MOVE      DTD(X)        ANNO1             4 0
337400941222     C                   MOVEL     DTD(X)        MMGG
337500941222     C                   MOVE      MMGG          MM1
337600120302     c************ INCASSO TOTALE **********
337700941118     C*  Controllo se devo gestire incassi sulla riga di totale
337800991008     C     V3DTPI        IFNE      'C/A'                                        -- 03 --
337900991007     C     V3DTPI        ANDNE     'Ass'
338000990928     C                   SETON                                        05
338100991007     C                   Z-ADD     WTOT1         V3CIMP
338200941212     C                   SUB       RESTO         V3CIMP
338300991008     C     WINTOT        IFEQ      'NO'                                         -- 04 --
338400000421     C* Se devo incassare sia il c/assegno che l'assegnato ed ho
338500000421     C* scritto la riga per l'incasso totale,nell'ipotesi in cui
338600000421     C* abbia proposto e non modificato la prima riga di incasso
338700000421     C* C/Assegno con un tipo incasso non congruente con la div.
338800000421     C* (es: C/Assegno in Euro in contanti) prima di dare l'errore
338900000421     C* sulla riga di incasso C/Assegno debbo verificare di non
339000000421     C* voler gestire l'incasso sulla riga di totale. Se mi accorgo
339100000421     C* che non posso gestire l'incasso sulla riga di totale xchè
339200000421     C* ho dato per non incassata la seconda riga di incasso
339300000421     C* quando leggo la terza riga emetto l'errore: nelle righe
339400000421     C* precedenti presente incasso non congruente con divisa.
339500000421     C* Per sistemare il tutto l'utente deve o aggiornare la
339600000421     C* prima riga modificando il tipo incasso oppure deve abblencare
339700000421     C* il flag di non incasso sulla seconda riga e gestire l'incasso
339800000421     C* sulla terza
339900991008     C     WRIGTO        IFEQ      'S'
340000991008     C                   SETON                                        4028
340100991008     C                   MOVEL     ERR(8)        $MSG
340200991008     C                   GOTO      UPDRE1
340300991008     C                   END
340400941123     C                   MOVEL     'N'           V02SN1
340500941207     C                   MOVEL     *BLANKS       WINTOT            2
340600000421     C*  Se non posso gestire l'incasso sulla terza riga in
340700000421     C*  quanto ho modificato una delle precedenti dandole x
340800000421     C*  non incassate o specificando dei dati di incasso a
340900000421     C*  livello di dettaglio su almeno una di esse controllo
341000000421     C*  che non siano stati impostati dati di incasso sulla
341100000421     C*  stessa
341200941125     C     V3CTIC        IFNE      *BLANKS
341300941227     C     V02AS1        ORNE      *BLANKS
341400941125     C     V02AB1        ORNE      *ZEROS
341500941125     C     V02CA1        ORNE      *ZEROS
341600941125     C     V02DT1        ORNE      *ZEROS
341700990928     C     V02ASP        ORNE      *ZEROS
341800990928     C     V3CVCA        ORNE      VH3DIV
341900941125     C                   SETON                                        404142
342000941125     C                   SETON                                        4328
342100941125     C                   MOVEL     ERR(34)       $MSG
342200941125     C                   GOTO      UPDRE1
342300941125     C                   END
342400991008     C                   ELSE                                                   --  04 --
342500941207     C                   MOVEL     ' '           V02SN1
342600991008     C                   END                                                    -- 04 --
342700991008     C                   ELSE                                                   -- 03 --
342800991008     C     V3CTOT        IFEQ      'SI'                                         -- 04 --
342900941118     C*  Se flag di effettuato incasso = 'N' non devo incassare
343000941123     C     V02SN1        IFEQ      'N'
343100941118     C     V3CTIC        ORNE      *BLANKS
343200990928     C     V3CVCA        ORNE      VH3DIV
343300941125     C                   MOVEL     'NO'          WINTOT            2
343400941125     C                   END
343500991008     C                   END                                                    -- 04 --
343600991008     C                   END                                                    -- 03 --
343700991007     C* Azzero dati spedizione se modificata
343800991007     C     V3CAAS        IFNE      WSVAAS
343900991007     C     V3CLNP        ORNE      WSVLNP
344000991007     C     V3CNRS        ORNE      WSVNRS
344100991007     C     V3CNSP        ORNE      WSVNSP
344200991007     C                   EXSR      AZZSPE
344300991007     C                   END
344400991007     C* Azzero sempre resto
344500991007     C                   Z-ADD     0             RESTO
344600120302     C*
344700120302     C*se opzione in riga incasso non effettuato impossibile inserire opzione
344800120302     C     V3copz        IFNE      *blank
344900120302     C     v02sn1        ANDNE     *blank
345000120302     C                   SETON                                        40  28
345100120302     C                   MOVEL     ERR(76)       $MSG
345200120302     C                   GOTO      UPDRE1
345300120302     C                   END
345400120302     C************* INCASSATO ****************
345500941118     C*  Se flag incasso impostato a ' ' controllo attendbilità dati
345600991008     C     V02SN1        IFEQ      ' '                                          --- 03 ---
345700950314     C*  Controllo se richiesta ricerca tipo incasso
345800950314     C     '?'           SCAN      V3CTIC                                 15
345900950314     C     *IN15         IFEQ      '1'
346000950314     C                   MOVEL     KKUT          §KUT
346100950314     C                   MOVEL     '1A'          §COD
346200950314     C                   MOVE      *BLANKS       V3CTIC
346300950314     C                   CALL      'X§TABER'
346400950314     C                   PARM                    §KUT
346500950314     C                   PARM                    §COD
346600950314     C                   PARM                    §KEY
346700950314     C                   PARM                    §DES             30
346800950314     C                   MOVEL     §KEY          V3CTIC
346900950314     C                   END
347000060320     c* per il momento il tipo incasso POS è ammesso solo per aut. fittizio
347100060320     c* consegne a magazzino
347200060403     c                   if        v3ctic = 'P'
347300060403     c                   if        apdpdd = ' '
347400060320     C                   SETON                                        4028
347500060320     C                   MOVEL     ERR(94)       $MSG
347600060320     C                   GOTO      UPDRE1
347700060320     c                   end
347800080709     c* Verifica se possibile incasso POS tramite tabella Y4O
347900080709     c                   clear                   Dy4o
348000080709     c                   eval      KCOD1 =  'Y4O'
348100080709     c                   eval      KKE1  =  '201'
348200080714     c                   movel(p)  d53fgs        KKE2
348300080709     c     ktbe2         chain     tntbe01l
348400080709     c                   if        %found(tntbe01l)
348500080709     c                   movel     tbeuni        dy4o
348600080709     c                   if        §4OCC7 = *blank or §4OsC7 = *blank
348700080709     C                   SETON                                        4028
348800080709     C                   MOVEL     ERR(102)      $MSG
348900080709     C                   GOTO      UPDRE1
349000080709     c                   end
349100080709     c                   end
349200060419     c* non ammesso incasso parziale
349300060403     C     V02TPA        IFNE      ' '
349400060403     C                   SETON                                        4028
349500060403     C                   MOVEL     ERR(50)       $MSG
349600060403     C                   GOTO      UPDRE1
349700060403     c                   end
349800060403     c                   end
349900941118     C*  Controllo se corretto tipo incasso
350000941118     C                   Z-ADD     1             X
350100941118     C     V3CTIC        LOOKUP    C1A(X)                                 95
350200941118     C     *IN95         IFEQ      '0'
350300941118     C                   SETON                                        4028
350400941118     C                   MOVEL     ERR(38)       $MSG
350500941118     C                   GOTO      UPDRE1
350600941118     C                   ELSE
350700941118     C                   MOVEL     D1A(X)        DS1A
350800080717      *pda incasso dichiarato in contante e diverso da originale
350900080717     c                   if        wpda = 'S' and
351000080717     c                             vh3ctic = 'C' and §1AFCA <> 'S'
351100080717     C                   SETON                                        4028
351200090122     C                   MOVE      'E'           vh3ctic
351300080717     C                   MOVEL     ERR(103)      $MSG
351400080717     C                   GOTO      UPDRE1
351500080717     c                   endif
351600080717      *pda incasso dichiarato con assegno e diverso da originale
351700080717     c                   if        wpda = 'S' and
351800080717     c                             vh3ctic = 'A' and
351900080717     c                             (§1AFCA <> 'B' and §1AFCA <> 'C')
352000080717     C                   SETON                                        4028
352100090122     C                   MOVE      'E'           vh3ctic
352200080717     C                   MOVEL     ERR(104)      $MSG
352300080717     C                   GOTO      UPDRE1
352400080717     c                   endif
352500941118     C                   END
352600991115     C     §1AFMB        COMP      'M'                                    06
352700991116     C   06              MOVE      VH3DIV        V3CVCA
352800100218     c                   if        v3cvca <> ' '
352900991116     C                   Z-ADD     1             X
353000991116     C     V3CVCA        LOOKUP    CCV(X)                                 32
353100991116     C  N06V3CVCA        IFNE      §GEDCR
353200991116     C     FLV(X)        ANDNE     'S'
353300991116     C                   SETON                                        06
353400991116     C                   END
353500100218     C                   END
353600120229     C* opzione ammessa solo x incasso MITTENTE
353700120229     C     V3copz        IFNE      *blank
353800120229     C     §1afmb        ANDNE     'M'
353900120229     C                   SETON                                        40  28
354000120229     C                   MOVEL     ERR(74)       $MSG
354100120229     C                   GOTO      UPDRE1
354200120229     C                   END
354300120315     C* opzione ammessa solo x incasso in 'EUR'
354400120315     C     V3copz        IFNE      *blank
354500130408     C     V3CVCA        andne     'EUR'
354600130408     C     Vh3div        andne     'EUR'
354700120315     C                   SETON                                        40  28
354800120315     C                   MOVEL     ERR(74)       $MSG
354900120315     C                   GOTO      UPDRE1
355000120315     C                   END
355100120229     C*se la riga è già stata trattata è possibile solo opz M               ritrattata
355200120302     c                   if        v3hkey <> ' '
355300120302     C                   if        v3copz <> ' ' and v3copz <> 'M'
355400120229     C                   SETON                                        40  28
355500120229     C                   MOVEL     ERR(75)       $MSG
355600120229     C                   GOTO      UPDRE1
355700120229     C                   END
355800120302     c                   else
355900120302     C                   if        v3copz = 'M'
356000120302     C                   SETON                                        40  28
356100120302     C                   MOVEL     ERR(75)       $MSG
356200120302     C                   GOTO      UPDRE1
356300120302     c                   end
356400120302     c                   end
356500120302     c************ CONTANTE / POS ******************
356600060419     C*  Tipo incasso in contante o POS
356700991008     C     §1AFCA        IFEQ      'S'                                          --- 04 ---
356800060419     C     §1AFCA        orEQ      'P'                                          --- 04 ---
356900011010     C* CONTROLLO VALIDITA DIVISA CON TABELLA CW CONTENENTE DATA SCADENZA
357000011017MM+  C* Do errore solo se non ho la riga di incasso totale, ...
357100011017MM+  C     V3CTOT        IFNE      'SI'                                         -- 04---
357200011017MM+  C     V3DTPI        ANDEQ     'C/A'
357300011017MM+  C     WINTOT        OREQ      'NO'
357400011017MM+  C     V3DTPI        ANDEQ     'C/A'
357500011017MM+  C     V3CTOT        ORNE      'SI'
357600011017MM+  C     V3DTPI        ANDEQ     'Ass'
357700011017MM+  C     WINTOT        OREQ      'NO'
357800011017MM+  C     V3DTPI        ANDEQ     'Ass'
357900011017MM+  C     V3CTOT        OREQ      'SI'
358000011017MM+  C     V3DTPI        ANDNE     'C/A'
358100011017MM+  C     V3DTPI        ANDNE     'Ass'
358200011010     C     §1ACDL        IFEQ      'S'                                          --- 05 ---
358300011010     C                   MOVEL     'CW'          KCOD
358400011010     C                   MOVE      *BLANK        KKEY
358500011010     C                   MOVEL     V3CVCA        KKEY
358600011010     C     KTAB2         CHAIN     TABEL00F                           96
358700011010     C     *IN96         IFEQ      '0'                                          --- 06 ---
358800011010     C                   MOVEL     TBLUNI        §CWDLC
358900011010     C     OGGI          IFGT      §CWDLC                                       --- 07 ---
359000011010     C                   SETON                                        4928
359100011010     C                   MOVEL     ERR(85)       $MSG
359200011010     C                   GOTO      UPDRE1
359300011010     C                   END                                                    --- 07 ---
359400011010     C                   END                                                    --- 06 ---
359500011010     C                   END                                                    --- 05 ---
359600011017MM+  C                   END                                                    --- 04 ---
359700941118     C     V02AS1        IFNE      *ZEROS
359800941129     C     V02AS1        ANDNE     *BLANKS
359900941118     C                   SETON                                        404128
360000941118     C                   MOVEL     ERR(36)       $MSG
360100941118     C                   GOTO      UPDRE1
360200941118     C                   END
360300941118     C     V02AB1        IFNE      *ZEROS
360400941118     C     V02CA1        ORNE      *ZEROS
360500970117     C                   SETON                                        404228
360600941118     C                   MOVEL     ERR(35)       $MSG
360700941118     C                   GOTO      UPDRE1
360800941118     C                   END
360900941118     C     V02DT1        IFNE      *ZEROS
361000941118     C                   SETON                                        404328
361100941118     C                   MOVEL     ERR(37)       $MSG
361200941118     C                   GOTO      UPDRE1
361300941118     C                   END
361400120302     c************** ASSEGNO *****************
361500941118     C*  Incasso con Assegno
361600991008     C                   ELSE                                                   --- 04 ---
361700120228      * verifico prima dei controlli su assegno se scelta opzione per
361800120228      * gestione multiassegno o multispedizione se scelta opzione non esegue    --- 04b---
361900941118     C*  Per riga totale o assegnato non ammesso tipo incasso 'M'
362000991008     C     V3DTPI        IFNE      'C/A'                                        --- 05 ---
362100941118     C*  Controllo tipo incasso
362200941118     C     §1AFMB        IFEQ      'M'
362300941118     C     V3CTIC        OREQ      'BB'
362400010830     C     V3CTIC        OREQ      'BC'
362500970627     C     V3CTIC        OREQ      'CB'
362600150526     C**** V3CTIC        OREQ      'BP'
362700941118     C                   SETON                                        4028
362800941118     C                   MOVEL     ERR(39)       $MSG
362900941118     C                   GOTO      UPDRE1
363000941118     C                   END
363100941118     C                   ELSE
363200120229     C*  Se C/Assegno
363300941118     C*  Controllo se cliente abilitato al "BB"
363400991008     C     V3CTIC        IFEQ      'TM'                                         -- 06 --
363500060626     C     V3CTIC        OREQ      'TO'
363600060626     C     V3CTIC        OREQ      'BB'
363700010830     C     V3CTIC        OREQ      'BC'
363800970627     C     V3CTIC        OREQ      'CB'
363900951116     C     V3CTIC        OREQ      'BA'
364000960725     C     V3CTIC        OREQ      'CA'
364100150526     C**** V3CTIC        OREQ      'BP'
364200150526     C     v3ctic        oreq      'TP'
364300941123     C                   CLEAR                   DSTM
364400130328     C                   MOVEL     'TM'          KCOD
364500130328     C                   MOVE      *BLANK        KKEY
364600130328     C                   MOVEL     §clitm        KKEY
364700130328     C     KTAB2         CHAIN     TABEL00F                           96
364800130328     C     *IN96         IFEQ      '0'                                          --- 06 ---
364900130328     C                   movel     tbluni        dstm
365000130328     c                   endif
365100941118     C*
365200941121     C     V3CTIC        IFEQ      'TM'
365300941118     C     §TMFTM        ANDNE     'S'
365400951108     C     *IN95         ANDEQ     '1'
365500941121     C     V3CTIC        OREQ      'BB'
365600941118     C     §TMFBB        ANDNE     'S'
365700951108     C     *IN95         ANDEQ     '1'
365800060626     C     V3CTIC        OREQ      'TO'
365900060626     C     §TMFTO        ANDNE     'S'
366000060626     C     *IN95         ANDEQ     '1'
366100010831     C     V3CTIC        OREQ      'BB'
366200010831     C     §TMFBC        ANDEQ     'S'
366300010831     C     *IN95         ANDEQ     '1'
366400010830     C     V3CTIC        OREQ      'BC'
366500010830     C     §TMFBC        ANDNE     'S'
366600010830     C     *IN95         ANDEQ     '1'
366700970627     C     V3CTIC        OREQ      'CB'
366800970627     C     §TMFBB        ANDNE     'S'
366900970627     C     *IN95         ANDEQ     '1'
367000010831     C     V3CTIC        OREQ      'CB'
367100010831     C     §TMFBC        ANDEQ     'S'
367200010831     C     *IN95         ANDEQ     '1'
367300951116     C     V3CTIC        OREQ      'BA'
367400951116     C     §TMFBA        ANDNE     'S'
367500951116     C     *IN95         ANDEQ     '1'
367600960725     C     V3CTIC        OREQ      'CA'
367700960725     C     §TMFCA        ANDNE     'S'
367800960725     C     *IN95         ANDEQ     '1'
367900150526     C**** V3CTIC        OREQ      'BP'
368000150526     C**** §TMFbp        ANDNE     'S'
368100150526     C**** *IN95         ANDEQ     '1'
368200150526     C     v3ctic        oreq      'TP'
368300150526     C     §TMFtp        andne     'S'
368400150526     C     *in95         andeq     '1'
368500951108     C     *IN95         OREQ      '0'
368600941118     C                   SETON                                        4028
368700941118     C                   MOVEL     ERR(41)       $MSG
368800941118     C                   GOTO      UPDRE1
368900941121     C                   END
369000991008     C                   END                                                    -- 06 --
369100941123     C*
369200991008     C                   END                                                    -- 05 --
369300130416     c* se tipo incasso non utilizzabile in chiusura distinta errore
369400130416     c     §1achid       ifeq      'N'
369500130416     C                   SETON                                        4028
369600130416     C                   MOVEL     ERR(109)      $MSG
369700130416     C                   GOTO      UPDRE1
369800130416     c                   endif
369900120302     c************* INCASSO DI UN ASSEGNO ****************
370000120302     c                   if        v3copz = ' ' and v3hkey = *blanks
370100000421     C*  Do errore se il tipo incasso prevede l'assegno e non
370200000421     C*  ho specificato il numero
370300960725     C     §1ANAS        IFEQ      *BLANKS
370400941118     C     V02AS1        IFEQ      *BLANKS
370500941118     C     V02AS1        OREQ      *ZEROS
370600941118     C                   SETON                                        4128
370700941118     C                   MOVEL     ERR(42)       $MSG
370800941118     C                   GOTO      UPDRE1
370900120418     c                   else
371000120418     c                   monitor
371100120418     c                   eval      com04 = %int(v02as1)
371200120418     c                   movel     com04         v02as1
371300120418     c                   on-error
371400120418     C                   SETON                                        4128
371500120418     C                   MOVEL     ERR(42)       $MSG
371600120418     C                   GOTO      UPDRE1
371700120418     c                   endmon
371800941118     C                   END
371900960725     C                   END
372000090707     c* se tipo incasso non ammette assegni postali controllo abi  07601
372100090707     c     §1anap        ifeq      'S'
372200090622     c     v02ab1        andeq     07601
372300090622     C                   SETON                                        4228
372400130416     C                   MOVEL     ERR(106)      $MSG
372500090622     C                   GOTO      UPDRE1
372600090622     c                   endif
372700000421     C*  Do errore se il tipo incasso prevede l'ABI e non l'ho
372800000421     C*  indicato
372900120229     C     §1AABI        IFEQ      *BLANKS                                      -- 06 --
373000100825     C     V02AB1        IFlt      *ZEROS
373100120229     C     V02CA1        ORlt      *ZEROS                                       -- 07 --
373200100825     C                   SETON                                        4228
373300100825     C                   MOVEL     ERR(45)       $MSG
373400100825     C                   GOTO      UPDRE1
373500100825     C                   End
373600941118     C     V02AB1        IFEQ      *ZEROS
373700120229     C     V02CA1        OREQ      *ZEROS                                       -- 07 --
373800941118     C                   SETON                                        4228
373900941118     C                   MOVEL     ERR(43)       $MSG
374000941118     C                   GOTO      UPDRE1
374100941118     C                   ELSE
374200000421     C*  Se immesso Abi-CAb verifico congruenza
374300120229     C     SAVABI        IFNE      V02AB1                                       -- 08 --
374400941118     C     SAVCAB        ANDNE     V02CA1
374500941118     C                   Z-ADD     V02AB1        KABI
374600941118     C                   Z-ADD     V02CA1        KCAB
374700941123     C     KABI1         CHAIN     CNABI01L                           95
374800120229     C     *IN95         IFEQ      '1'                                          -- 09 --
374900120229     C     *INKB         IFEQ      '0'                                          -- 10 --
375000941121     C     SAVABI        IFEQ      *ZEROS
375100941121     C     SAVCAB        OREQ      *ZEROS
375200941121     C                   SETON                                        4228
375300941121     C                   MOVEL     ERR(45)       $MSG
375400941121     C                   END
375500120229     C                   ELSE                                                   -- 10 --
375600941121     C     AGGI          IFEQ      ' '
375700941121     C                   Z-ADD     V02AB1        SAVABI
375800941123     C                   Z-ADD     V02CA1        SAVCAB
375900941121     C                   MOVEL     'S'           AGGI
376000941121     C                   END
376100120229     C                   END                                                    -- 10 --
376200120229     C                   END                                                    -- 09 --
376300941123     C*
376400120229     C                   END                                                    -- 08 --
376500120229     C                   END                                                    -- 07 --
376600120229     C                   END                                                    -- 06 --
376700941118     C*
376800000421     C* Se incasso con assegno e non ho indicato la data
376900000421     C* imposto in automatico quella del giorno
377000991008     C     §1ANAS        IFEQ      *BLANKS                                      -- 05 --
377100991008     C     V02DT1        IFEQ      *ZEROS                                       -- 06 --
377200941128     C                   MOVEL     WOGGI         V02DT1
377300941121     C                   Z-ADD     OGGI          WDTASS
377400941121     C                   ELSE
377500941121     C*  Giro la data assegno
377600941222     C                   MOVEL     V02DT1        G02DAT
377700941222     C                   MOVE      V02DT1        MM2
377800000421     C* Postdatati: se sono a fine anno (da ottobre in poi)
377900000421     C* ed indico un mese di incasso inferiore a quelli del
378000000421     C* giorno presuppongo di voler specificare come data
378100000421     C* di incasso quella dell'anno successivo. Lo slittamentp
378200000421     C* deve essere non superiore a 3 mesi quindi ....
378300101020     C* variato il periodo da 3 a 6 mesi
378400991008     C     V3CTIC        IFEQ      'TM'                                         -- 07 --
378500060626     C     V3CTIC        orEQ      'TO'                                         -- 07 --
378600150526     C     v3ctic        orEQ      'TP'                                         -- 07 --
378700000421     C* ... se sono in ottobre al massimo posso accettare
378800000421     C* assegni postdatati a gennaio (ott. + 3 mesi)
378900131010     C     MM2           IFle      3
379000951116     C     MM1           ANDEQ     10
379100000421     C* ... se sono in novembre posso accettare assegni
379200000421     C* postdatati a gennaio o febbraio (nov. + 3 mesi)
379300101020     C     MM2           ORLE      5
379400951116     C     MM1           ANDEQ     11
379500000421     C* ... se sono in dicembre posso accettare assegni
379600000421     C* postdatati a gennaio, febbraio o marzo (dic. + 3 mesi)
379700101020     C     MM2           ORLE      6
379800951116     C     MM1           ANDEQ     12
379900951116     C                   ADD       1             ANNO1
380000991008     C                   END
380100151005      *
380200991008     C                   ELSE                                                   -- 07 --
380300000421     C* ... se il tipo incasso non indica un assegno postdatato
380400000421     C* posso slittare il pagamento solo da dicembre a gennaio
380500000421     C* e viceversa
380600951205     C     MM1           IFEQ      12
380700951205     C     MM2           ANDEQ     1
380800951205     C                   ADD       1             ANNO1
380900951205     C                   END
381000941222     C     MM1           IFEQ      1
381100951116     C     MM2           ANDEQ     12
381200951205     C                   SUB       1             ANNO1
381300941222     C                   END
381400991008     C                   END                                                    -- 07 --
381500151005      **
381600950102     C                   MOVE      ANNO1         G02DAT
381700941121     C                   Z-ADD     0             G02INV
381800941121     C                   MOVEL     *BLANKS       G02ERR
381900941121     C                   CALL      'XSRDA8'
382000941121     C                   PARM                    WLBDA8
382100941121     C     G02ERR        IFEQ      '1'
382200941118     C                   SETON                                        4328
382300941118     C                   MOVEL     ERR(44)       $MSG
382400941118     C                   GOTO      UPDRE1
382500941121     C                   ELSE
382600941121     C                   Z-ADD     G02INV        WDTASS
382700941129     C                   MOVEL     G02DAT        V02DT1
382800991008     C                   END
382900991008     C                   END                                                    -- 06 --
383000941121     C* Controllo data assegno > OGGI se TM
383100941121     C     V3CTIC        IFEQ      'TM'
383200941121     C     WDTASS        ANDLE     OGGI
383300060626     C     V3CTIC        orEQ      'TO'
383400060626     C     WDTASS        ANDLE     OGGI
383500150526     C     v3ctic        oreq      'TP'
383600150526     C     wdtass        andle     oggi
383700941121     C                   SETON                                        404328
383800941121     C                   MOVEL     ERR(52)       $MSG
383900941121     C                   GOTO      UPDRE1
384000941121     C                   END
384100941129     C* Se data assegno > OGGI
384200991008     C     WDTASS        IFGT      OGGI                                         -- 06 --
384300941129     C* X Mittente obbligatorio tipo incasso TM
384400991008     C     §1AFMB        IFEQ      'M'                                          -- 07 --
384500941129     C     V3CTIC        IFNE      'TM'
384600060626     C     V3CTIC        andne     'TO'
384700150526     C     v3ctic        andne     'TP'
384800941129     C                   SETON                                        404328
384900941129     C                   MOVEL     ERR(59)       $MSG
385000941129     C                   GOTO      UPDRE1
385100941129     C                   END
385200941129     C* X Bartolini non si possono inserire postdatati
385300991008     C                   ELSE                                                   -- 07 --
385400941129     C                   SETON                                        404328
385500941129     C                   MOVEL     ERR(60)       $MSG
385600941129     C                   GOTO      UPDRE1
385700991008     C                   END                                                    -- 07 --
385800991008     C                   END                                                    -- 06 --
385900941129     C*
386000991008     C                   END                                                    -- 05 --
386100011012     C* CONTROLLO VALIDITA DIVISA CON TABELLA CW CONTENENTE DATA SCADENZA
386200011017MM+  C* Do errore solo se non ho la riga di incasso totale, ...
386300011017MM+  C     V3CTOT        IFNE      'SI'
386400011017MM+  C     V3DTPI        ANDEQ     'C/A'
386500011017MM+  C     WINTOT        OREQ      'NO'
386600011017MM+  C     V3DTPI        ANDEQ     'C/A'
386700011017MM+  C     V3CTOT        ORNE      'SI'
386800011017MM+  C     V3DTPI        ANDEQ     'Ass'
386900011017MM+  C     WINTOT        OREQ      'NO'
387000011017MM+  C     V3DTPI        ANDEQ     'Ass'
387100011017MM+  C     V3CTOT        OREQ      'SI'
387200011017MM+  C     V3DTPI        ANDNE     'C/A'
387300011017MM+  C     V3DTPI        ANDNE     'Ass'
387400011012     C     §1ACDL        IFEQ      'S'                                          -- 05 --
387500011012     C                   MOVEL     'CW'          KCOD
387600011012     C                   MOVE      *BLANK        KKEY
387700011012     C                   MOVEL     V3CVCA        KKEY
387800011012     C     KTAB2         CHAIN     TABEL00F                           96
387900011012     C     *IN96         IFEQ      '0'                                          -- 06 --
388000011012     C                   MOVEL     TBLUNI        §CWDLC
388100011012     C     WDTASS        IFGT      §CWDLC                                       -- 07 --
388200011012     C                   SETON                                        4928
388300011012     C                   MOVEL     ERR(85)       $MSG
388400011012     C                   GOTO      UPDRE1
388500011012     C                   END                                                    --- 07 ---
388600011012     C                   END                                                    --- 06 ---
388700011012     C                   END                                                    --- 05 ---
388800011017MM+  C                   END                                                    --- 04 ---
388900120228     c* fine if salto controlli se inserita opzione assegno multiplo o
389000120228     c* spedizione multipla
389100120228     c                   endif                                                  -- 04b--
389200120228
389300991008     C                   END                                                    -- 04 --
389400990923     C*  Controllo se divisa valida
389500991201     C     VH3DIV        IFNE      *BLANKS                                      -- 04 --
389600941129     C                   Z-ADD     1             X
389700941222     C     V3CVCA        LOOKUP    CCV(X)                                 32
389800990928     C* Se divisa inesistente do errore
389900990928     C     *IN32         IFEQ      '0'
390000991201     C     V3CVCA        OREQ      *BLANKS
390100990928     C                   SETON                                        4928
390200990928     C                   MOVEL     ERR(27)       $MSG
390300990928     C                   GOTO      UPDRE1
390400990928     C                   END
390500941129     C* Somma nel totale corretto
390600990923     C                   MOVE      FLV(X)        WFLV
390700941129     C                   END
390800000421     C*
390900941129     C*  Controllo se tipo incasso ammesso prevede divisa
391000941129     C     §1AUDE        IFNE      'S'
391100990923     C     WFLV          ANDNE     'S'
391200991011     C     §1AUDE        OREQ      'S'
391300991011     C     WFLV          ANDNE     'S'
391400991011     C     V3CVCA        ANDNE     VH3DIV
391500990923     C*  tipo incasso non valido per Euro
391600990923     C     §1AEUR        ORNE      'S'
391700990923     C     WFLV          ANDEQ     'S'
391800000421     C* Do errore solo se non ho la riga di incasso totale, ...
391900991007     C     V3CTOT        IFNE      'SI'
392000991007     C     V3DTPI        ANDEQ     'C/A'
392100991008     C     WINTOT        OREQ      'NO'
392200991007     C     V3DTPI        ANDEQ     'C/A'
392300991007     C     V3CTOT        ORNE      'SI'
392400991008     C     V3DTPI        ANDEQ     'Ass'
392500991008     C     WINTOT        OREQ      'NO'
392600991008     C     V3DTPI        ANDEQ     'Ass'
392700991011     C     V3CTOT        OREQ      'SI'
392800991011     C     V3DTPI        ANDNE     'C/A'
392900991116     C     V3DTPI        ANDNE     'Ass'
393000941129     C                   SETON                                        4028
393100941129     C                   MOVEL     ERR(40)       $MSG
393200941129     C                   GOTO      UPDRE1
393300991007     C                   ELSE
393400000421     C* .. se ce l'ho, imposto flag per riconoscere che dovrò
393500000421     C* obbligarne l'immissione:
393600991007     C                   MOVEL     'S'           WRIGTO
393700941129     C                   END
393800991007     C                   END
393900991007     C* Flag incasso parziale non ammesso per C/Assegno/totale
394000990923     C     V02TPA        IFNE      ' '
394100990927     C     V3DTPI        ANDNE     'Ass'
394200990923     C                   SETON                                        444528
394300990923     C                   MOVEL     ERR(15)       $MSG
394400990923     C                   GOTO      UPDRE1
394500990923     C                   END
394600990923     C* L'importo incassato obbligatorio se modificata la divisa
394700990923     C     V02ASP        IFEQ      *ZERO
394800990924     C     V3CVCA        ANDNE     VH3DIV
394900990923     C                   SETON                                        444528
395000990923     C                   MOVEL     ERR(6)        $MSG
395100990923     C                   GOTO      UPDRE1
395200990923     C                   END
395300990923     C* Importo incassato valido solo per assegnato se non modifcata
395400990923     C* divisa
395500990923     C     V02ASP        IFNE      *ZERO
395600941121     C     V02TPA        ORNE      *BLANKS
395700941121     C     V3DTPI        IFNE      'Ass'
395800990923     C     V3CVCA        ANDEQ     VH3DIV
395900941121     C                   SETON                                        444528
396000941121     C                   MOVEL     ERR(46)       $MSG
396100941121     C                   GOTO      UPDRE1
396200941121     C                   ELSE
396300941121     C                   Z-ADD     NRR3          SAVNRR
396400990923     C*  Importo incassato superiore a quello da incassare
396500941121     C     V3CIMP        IFLE      V02ASP
396600990923     C     V3CVCA        ANDEQ     VH3DIV
396700941121     C                   SETON                                        4428
396800941121     C                   MOVEL     ERR(48)       $MSG
396900941121     C                   GOTO      UPDRE1
397000941121     C                   END
397100991007     C                   END
397200991007     C                   END
397300990923     C* Se l'importo incassato non è in moneta di conto
397400990923     C* lo converto per calcolare differenza
397500991007     C     V3CVCA        IFNE      §GEDCR
397600990923     C     V02ASP        ANDGT     0
397700990923     C                   CLEAR                   YEURDS
397800990923     C                   MOVEL     V3CVCA        YECDVI
397900990923     C                   Z-ADD     V02ASP        YECIMI
398000991007     C                   MOVEL     §GEDCR        YECDVO
398100990923     C                   MOVEL     'H'           YECTAR
398200990923     C                   CALL      'YEURCO'
398300990923     C                   PARM                    YEURDS
398400990923     C                   Z-ADD     YECIMO        WTOTMC           13 3
398500990923     C                   ELSE
398600990923     C                   Z-ADD     V02ASP        WTOTMC           13 3
398700990923     C                   END
398800990923     C* Converto importo da incassare se non in moneta di conto
398900991007     C     VH3DIV        IFNE      §GEDCR
399000990923     C                   CLEAR                   YEURDS
399100990923     C                   MOVEL     VH3DIV        YECDVI
399200990923     C                   Z-ADD     V3CIMP        YECIMI
399300991007     C                   MOVEL     §GEDCR        YECDVO
399400990923     C                   MOVEL     'H'           YECTAR
399500990923     C                   CALL      'YEURCO'
399600990923     C                   PARM                    YEURDS
399700990923     C                   Z-ADD     YECIMO        WTOTDI           13 3
399800991007     C                   ADD       YECIMO        WTOT1            13 3
399900990923     C                   ELSE
400000990923     C                   Z-ADD     V3CIMP        WTOTDI           13 3
400100991007     C                   ADD       V3CIMP        WTOT1            13 3
400200990923     C                   END
400300990923     C* ... se è stata modificata la divisa per il C/Assegno
400400990923     C* controllo se l'importo incassato è corretto
400500990923     C     V02ASP        IFNE      0
400600990928     C     V02TPA        ORNE      *BLANKS
400700990928     C     VH3DIV        IFNE      V3CVCA
400800990923     C* Se l'importo da incassare non è uguale a quello incassato
400900990923     C* calcolo la differenza
401000990923     C     WTOTDI        IFNE      WTOTMC
401100990923     C     WTOTDI        IFGT      WTOTMC
401200990924     C     WTOTDI        SUB       WTOTMC        WTODIF           13 3
401300990923     C* se differenza > limite monimo tab. GEI e non si tratta di incasso
401400990923     C* Assegnato do errore
401500990923     C     WTODIF        IFGT      §GEMIA
401600990927     C     V3DTPI        ANDNE     'Ass'
401700990927     C                   SETON                                        2844
401800990923     C                   MOVEL     ERR(25)       $MSG
401900990923     C                   GOTO      UPDRE1
402000990923     C                   END
402100990923     C* ... se totale incassato > da incassare lo accetto solo se entro
402200990923     C* i limiti di abbuono
402300990923     C                   ELSE
402400990923     C     WTOTMC        SUB       WTOTDI        WTODIF
402500990923     C* se differenza > limite monimo tab. GEI do erore
402600990923     C     WTODIF        IFGT      §GEMIA
402700990927     C                   SETON                                        2844
402800990923     C                   MOVEL     ERR(26)       $MSG
402900990923     C                   GOTO      UPDRE1
403000990923     C                   END
403100990923     C                   END
403200990923     C                   END
403300990923     C*
403400990928     C* ... se immesso incasso parziale senza modifica divisa eseguo
403500990928     C*     lo stesso il calcolo della differenza
403600990928     C                   ELSE
403700990928     C     WTOTDI        SUB       WTOTMC        WTODIF
403800990923     C                   END
403900941121     C*  Controllo tipo incasso parziale
404000941121     C     '?'           SCAN      V02TPA                                 95
404100941121     C     *IN95         IFEQ      '1'
404200941123     C                   MOVEL     KKUT          §KUT
404300941121     C                   MOVEL     '7M'          §COD
404400941121     C                   MOVE      *BLANKS       V02TPA
404500941121     C                   CALL      'X§TABER'
404600941121     C                   PARM                    §KUT
404700941121     C                   PARM                    §COD
404800941121     C                   PARM                    §KEY
404900941121     C                   PARM                    §DES             30
405000941121     C                   MOVEL     §KEY          V02TPA
405100941121     C                   END
405200950620     C*
405300950620     C     V3HTPA        IFNE      V02TPA
405400950620     C                   MOVEL     V02TPA        V3HTPA
405500950620     C     V02TPA        IFEQ      'C'
405600110613     c* se pda non importa se è già stata chiusa la data per contabilizzare
405700110613     c* gli assegnati, perchè si deve lasciare mettere la C di nota credito
405800110613     c* per evitare lo "storno" dell'assegnato quando l'autista è già
405900110613     c* fuori in consegna. facendo il cambio di porto si creerebbe la
406000110613     c* figlia mentre la mamma è in consegna e si scatena l'errore
406100110613     c* bloccante in pre-chiusura distinta
406200110613     c                   if        (wpda <> 'S' and
406300110613     c                             V3CDFT > §dtcon) or
406400110613     c                             V3CDFT = 0
406500110613     C*    V3CDFT        IFGT      §DTCON
406600110613     C*    V3CDFT        OREQ      0
406700950620     C                   SETON                                        4528
406800950620     C                   MOVEL     ERR(63)       $MSG
406900950620     C                   GOTO      UPDRE1
407000950620     C                   END
407100950620     C                   END
407200950620     C                   END
407300941121     C*
407400991007     C     WTOTDI        SUB       WTOTMC        RESTO            13 3
407500990923     C     WTODIF        IFLT      §GEMIA
407600000117     C     WTODIF        ANDGT     §GEDFC
407700990928     C     V3DTPI        ANDEQ     'Ass'
407800991115     C                   MOVE      'A'           V02TPA
407900000117     C                   ELSE
408000000117     C     WTODIF        IFLT      §GEMIA
408100000117     C     WTODIF        ANDLE     §GEDFC
408200000117     C     V3DTPI        ANDEQ     'Ass'
408300000117     C                   MOVE      ' '           V02TPA
408400941121     C                   END
408500000117     C                   END
408600000421     C* ...se non impostato flag di incasso parziale
408700941121     C     V02TPA        IFEQ      *BLANKS
408800000421     C* ...il tipo incasso è assegnato e
408900990928     C     V3DTPI        ANDEQ     'Ass'
409000000421     C* ...l'importo in divisa di incasso incasso differisce
409100000421     C* di oltre quanto previsto da tabella x arrotondamenti
409200000117     C     WTODIF        ANDGT     §GEDFC
409300000421     C* .. obbligo l'indicazione di un tipo incasso parziale
409400941121     C                   SETON                                        4528
409500941121     C                   MOVEL     ERR(49)       $MSG
409600941121     C                   GOTO      UPDRE1
409700941121     C                   END
409800000421     C* verifico esistenza flag incasso parziale
409900941121     C                   Z-ADD     1             X
410000941121     C     V02TPA        LOOKUP    C7M(X)                                 95
410100941121     C     *IN95         IFEQ      '0'
410200941121     C                   SETON                                        4528
410300941121     C                   MOVEL     ERR(51)       $MSG
410400941121     C                   GOTO      UPDRE1
410500941121     C                   END
410600000421     C* Se differenza bollettato e incassato entro i limiti dell'abbuono
410700990924     C     WTODIF        IFGE      §GEMIA
410800990924     C     WTODIF        ANDLE     §GEMXA
410900000421     C* ... obbligo l'inserimento del tipo incasso parziale abbuono
411000941121     C     V02TPA        ANDNE     'A'
411100941121     C                   SETON                                        4528
411200941121     C                   MOVEL     ERR(50)       $MSG
411300941121     C                   GOTO      UPDRE1
411400941121     C                   END
411500000421     C* Non posso chiudere con incasso parziale un assegnato non
411600000421     C* incasso completamente
411700941121     C     V02TPA        IFEQ      'I'
411800941123     C     RESTO         ANDEQ     V3CIMP
411900941121     C                   SETON                                        4528
412000941121     C                   MOVEL     ERR(50)       $MSG
412100941121     C                   GOTO      UPDRE1
412200941121     C                   END
412300000421     C* Abbuono fuori limiti
412400990924     C     WTODIF        IFGT      §GEMXA
412500941121     C     V02TPA        ANDEQ     'A'
412600941121     C                   SETON                                        4528
412700941214     C                   MOVEL     ERR(50)       $MSG
412800941121     C                   GOTO      UPDRE1
412900941121     C                   END
413000941121     C*
413100011108     C                   END
413200991007     C*
413300011108     C                   END
413400011108     C                   END
413500991007     C*
413600110419     C     V02SN1        IFNE      ' '
413700110419     C     V02sn1        andne     'N'
413800110419     C                   SETON                                        5128
413900110419     C                   MOVEL     ERR(61)       $MSG
414000110419     C                   GOTO      UPDRE1
414100110419     C                   END
414200011108     C* SE IMPOSTATO FLAG NON INCASSATO DEVO PULIRE I CAMPI RELATIVI ALL'INCASSO
414300011108     C     V02SN1        IFNE      ' '
414400011108     C     V02ASP        IFNE      0
414500011108     C     V02AS1        ORNE      *BLANK
414600011108     C     V02AB1        ORNE      0
414700011108     C     V02CA1        ORNE      0
414800011108     C                   SETON                                        444142
414900011108     C                   SETON                                        28
415000011108     C                   MOVEL     ERR(86)       $MSG
415100011108     C                   GOTO      UPDRE1
415200011108     C                   END
415300011108     C                   END
415400941121     C     UPDRE1        TAG
415500991007     C* Imposto dati C/Assegno o Assegnato a seconda del tipo importo
415600991007     C                   EXSR      AGGFLD
415700991007     C*
415800941207     C     V3CAAS        IFEQ      WSVAAS
415900941207     C     V3CLNP        ANDEQ     WSVLNP
416000941207     C     V3CNRS        ANDEQ     WSVNRS
416100941207     C     V3CNSP        ANDEQ     WSVNSP
416200941207     C                   SETON                                        03
416300941207     C                   END
416400061108     c* richiamo la funzione x richiedere il codice fiscale solo una volta
416500061108     c                   move      savksc        com4              4
416600061107     c                   if        v02sn1 = ' ' and v3dtpi = 'Ass'
416700061108     c                             and com4 = '9999' and savcf = ' '
416800061107     c                   exsr      srcdfis
416900061107     c                   if        dz1err <> ' '
417000061107     C                   SETON                                        28
417100061107     C                   MOVEL(p)  dz1msg        $MSG
417200061108     c                   clear                   savcf
417300061107     C                   GOTO      UPDRE1
417400061107     c                   end
417500061108     c                   eval      savcf = '1'
417600061107     c                   end
417700120302      *scrittura rekord nuovo file per incassi mittenti multiassegni
417800120308     C                   IF        not *in28 and *inkf and
417900120308     c                             v3copz <> *blank
418000120308     c                   eval      comnrr = nrr3
418100120308     c* in modifica
418200120308     c                   if        v3copz = 'M'
418300120308     c                   exsr      aggiosped
418400120308     c* in scrittura
418500120308     c                   else
418600120308     c                   exsr      scrivisped
418700120308     c                   end
418800120308     c     comnrr        chain     lr53s03
418900120305     c                   endif
419000991007     C*  Update record
419100120308     c     v3hkey        comp      ' '                                8787
419200941207     C                   SETON                                        22
419300941121     C                   UPDATE    LR53S03
419400941121     C                   SETOFF                                       22
419500061107     c*
419600941207     C                   Z-ADD     V3CAAS        WSVAAS
419700941207     C                   Z-ADD     V3CLNP        WSVLNP
419800941207     C                   Z-ADD     V3CNRS        WSVNRS
419900941207     C                   Z-ADD     V3CNSP        WSVNSP
420000941128     C     *IN28         IFEQ      '0'
420100941129     C     *INKF         IFEQ      '0'
420200941121     C                   READC     LR53S03                                96
420300941129     C                   ELSE
420400941129     C                   ADD       1             NRR3
420500941129     C     NRR3          CHAIN     LR53S03                            96
420600941129     C                   END
420700991007     C*  Eseguo aggiornamento totali a rottura spedizione
420800991007     C*  o fine file
420900991007     C     V3CAAS        IFNE      WSVAAS
421000991007     C     V3CLNP        ORNE      WSVLNP
421100991007     C     V3CNRS        ORNE      WSVNRS
421200991007     C     V3CNSP        ORNE      WSVNSP
421300991007     C     *IN96         OREQ      '1'
421400991007     C   KF              EXSR      AGGTO3
421500991007     C                   END
421600941227     C                   ELSE
421700941227     C                   Z-ADD     NRR3          NR3KEY
421800941121     C                   END
421900941121     C*
422000941121     C                   END
422100941121     C*
422200941121     C                   ENDSR
422300991007      *---------------------------------------------------------*
422400061107      *  richiamo funzione per richiedere codice fiscale        *
422500991007      *---------------------------------------------------------*
422600061107     C     srcdfis       BEGSR
422700991007     C*
422800061107     c                   clear                   fnlrz1ds
422900061107     c                   eval      dz1tipo = 'A'
423000061107     c                   eval      dz1aas = v3caas
423100061107     c                   eval      dz1lnp = v3clnp
423200061107     c                   eval      dz1nrs = v3cnrs
423300061107     c                   eval      dz1nsp = v3cnsp
423400061107     c                   eval      kpjbu = fnlrz1ds
423500061107     c                   call      'FNLRZ1R'
423600061107     c                   parm                    kpjba
423700061107     c                   eval      fnlrz1ds = kpjbu
423800061108     c                   write     FRCDTA
423900061107     C*
424000061107     C                   ENDSR
424100061107      *---------------------------------------------------------*
424200061107      *  Azzero dati della spedizione                           *
424300061107      *---------------------------------------------------------*
424400061107     C     AZZSPE        BEGSR
424500061107     C*
424600991007     C* Importi
424700991007     C                   Z-ADD     0             WTOT1
424800991007     C                   Z-ADD     0             WTOT
424900991007     C                   Z-ADD     0             WASS
425000991007     C                   Z-ADD     0             WCAS
425100991007     C* Divise
425200991007     C                   MOVEL     *BLANKS       WDIV
425300991007     C                   MOVEL     *BLANKS       WVCA
425400991007     C                   MOVEL     *BLANKS       WVTO
425500991007     C* Tipi incasso ...
425600991007     C                   MOVEL     *BLANKS       WTCA
425700991007     C                   MOVEL     *BLANKS       WTIC
425800991007     C                   MOVEL     *BLANKS       WTTO
425900991007     C* .... e flag relativi
426000991007     C                   MOVEL     *BLANKS       WFCA
426100991007     C                   MOVEL     *BLANKS       WFCC
426200991007     C                   MOVEL     *BLANKS       WFCT
426300991007     C                   MOVEL     *BLANKS       WFMA
426400991007     C                   MOVEL     *BLANKS       WFMC
426500991007     C                   MOVEL     *BLANKS       WFMT
426600991007     C* flag S/N incasso
426700991007     C                   MOVEL     *BLANKS       WSNA
426800991007     C                   MOVEL     *BLANKS       WSNC
426900991007     C                   MOVEL     *BLANKS       WSNT
427000991007     C* errore ctrl riga totale
427100991007     C                   MOVEL     *BLANKS       WRIGTO            1
427200991007     C*
427300991007     C                   ENDSR
427400991007      *---------------------------------------------------------*
427500991007      *  Imposto campi nel vari importi                         *
427600991007      *---------------------------------------------------------*
427700991007     C     AGGFLD        BEGSR
427800991007     C*
427900000421     C*  Imposto dati in campi di work per C/Ass.
428000991007     C     V3DTPI        IFEQ      'C/A'
428100000421     C* ... se specificato importo incassato in divisa alternativa
428200000421     C*     vale quello
428300991007     C     V02ASP        IFGT      0
428400991007     C                   Z-ADD     V02ASP        WCAS
428500991007     C                   ELSE
428600991007     C                   Z-ADD     V3CIMP        WCAS
428700991007     C                   END
428800991007     C                   MOVEL     V02SN1        WSNC
428900991007     C                   MOVEL     V3CTIC        WTCA
429000991007     C                   MOVEL     V3CVCA        WVCA
429100991007     C                   MOVEL     §1AFCA        WFCC
429200991007     C                   MOVEL     §1AFMB        WFMC
429300991007     C                   ELSE
429400000421     C*  Imposto dati in campi di work per Assegnato
429500991007     C     V3DTPI        IFEQ      'Ass'
429600000421     C* ... se specificato importo incassato in divisa alternativa
429700000421     C*     o di incasso parziale vale quello
429800991007     C     V02ASP        IFGT      0
429900991027     C     V02TPA        ORNE      ' '
430000991007     C                   Z-ADD     V02ASP        WASS
430100991007     C                   ELSE
430200991007     C                   Z-ADD     V3CIMP        WASS
430300991007     C                   END
430400991007     C                   MOVEL     V02SN1        WSNA
430500991007     C                   MOVEL     V3CTIC        WTIC
430600991007     C                   MOVEL     V3CVCA        WDIV
430700991007     C                   MOVEL     §1AFCA        WFCA
430800991007     C                   MOVEL     §1AFMB        WFMA
430900991007     C* .. totale
431000991007     C                   ELSE
431100991007     C     V02ASP        IFGT      0
431200991007     C                   Z-ADD     V02ASP        WTOT
431300991007     C                   ELSE
431400991007     C                   Z-ADD     V3CIMP        WTOT
431500991007     C                   END
431600991007     C                   MOVEL     V02SN1        WSNT
431700991007     C                   MOVEL     V3CTIC        WTTO
431800991007     C                   MOVEL     V3CVCA        WVTO
431900991007     C                   MOVEL     §1AFCA        WFCT
432000991007     C                   MOVEL     §1AFMB        WFMT
432100991007     C                   END
432200991007     C                   END
432300991007     C*
432400991007     C                   ENDSR
432500941121      *---------------------------------------------------------*
432600000421      *  Aggiorno totali x videata di riepilogo                 *
432700941121      *---------------------------------------------------------*
432800941123     C     AGGTO3        BEGSR
432900941121     C*
433000991129     C                   Z-ADD     0             WIMP
433100991007     C* Verifico se devo gestire somma x la sola riga di totale
433200991008     C     WINTOT        IFNE      'NO'
433300991214     C* DEVO FARLO SOLO SE C'È LA RIGA DI TOTALE !!!
433400991215     C     WTOT          ANDGT     0
433500991007     C     WASS          ANDGT     0
433600991007     C     WCAS          ANDGT     0
433700991007     C* Sommo il totale
433800991008     C     WSNT          ANDNE     'N'
433900991007     C* Imposto dati campi di work subroutine aggiornamento tot.
434000991007     C                   MOVEL     WFCT          WCON
434100991007     C                   MOVEL     WFMT          WMIT
434200991007     C                   MOVEL     WVTO          WVAL
434300991007     C                   Z-ADD     WTOT          WIMP
434400991007     C                   EXSR      CALTOT
434500991007     C* .. no totale
434600991007     C                   ELSE
434700991007     C* ... Assegnato ?
434800991007     C     WASS          IFGT      0
434900991007     C     WSNA          ANDNE     'N'
435000991007     C                   MOVEL     WFCA          WCON
435100991007     C                   MOVEL     WFMA          WMIT
435200991007     C                   MOVEL     WDIV          WVAL
435300991007     C                   Z-ADD     WASS          WIMP
435400991007     C                   EXSR      CALTOT
435500991007     C                   END
435600991007     C* ... C/Assegno ?
435700991007     C     WCAS          IFGT      0
435800991007     C     WSNC          ANDNE     'N'
435900110330     C     WSNC          ANDNE     'R'
436000991007     C                   MOVEL     WFCC          WCON
436100991007     C                   MOVEL     WFMC          WMIT
436200991007     C                   MOVEL     WVCA          WVAL
436300991007     C                   Z-ADD     WCAS          WIMP
436400991007     C                   EXSR      CALTOT
436500991007     C                   END
436600991007     C*
436700991007     C                   END
436800991007     C*
436900991007     C                   ENDSR
437000991007      *---------------------------------------------------------*
437100991007      *  Calcolo totali                                         *
437200991007      *---------------------------------------------------------*
437300991007     C     CALTOT        BEGSR
437400991115     C                   Z-ADD     1             O                 4 0
437500991115     C     'EUR'         LOOKUP    CCV(O)                                 11
437600991007     C     WVAL          IFEQ      'EUR'
437700991115     C     *IN11         ANDEQ     '1'
437800991115     C     FLV(O)        ANDEQ     'S'
437900991007     C* contante
438000991007     C     WCON          IFEQ      'S'                                          Contanti
438100060310     C                   z-add     0             V05TEC
438200991007     C                   ADD       WIMP          V05TCE
438300991007     C                   ELSE                                                   no contanti
438400060331     C* ... pos
438500060331     c     wcon          ifeq      'P'
438600060331     c                   add       wimp          v05pos
438700060331     c                   else
438800060331     C* ... ns/assegni
438900991007     C     WMIT          IFNE      'M'                                          N.Assegni
439000991007     C                   ADD       WIMP          V05TAE
439100991007     C* ... ns/assegni
439200991007     C                   ELSE
439300991007     C                   ADD       WIMP          V05TDE                         Mittenti
439400991007     C                   END
439500991007     C                   END                                                    Contanti
439600060331     C                   END
439700060303     C* ... se no Euro sarà mittenti altre divise
439800991007     C                   ELSE
439900991007     C                   ADD       WIMP          V05TAV
440000991007     C                   END                                                    Euro
440100060320     c* mi salvo i totali
440200060320     c                   z-add     v05tce        savtce
440300060320     c                   z-add     v05tae        savtae
440400060320     c                   z-add     v05pos        savpos
440500941121     C*
440600941121     C                   ENDSR
440700060303      *---------------------------------------------------------*
440800060303      *  aggiunge prepagati ai totali                           *
440900060303      *---------------------------------------------------------*
441000060303     C     CALTOTPRE     BEGSR
441100060303     c* contante
441200060320     C     savtce        ADD       impinccon     V05TCE
441300060303     C* assegni
441400060320     C     savtae        ADD       impincass     V05TAE
441500060303     C* pos
441600060320     C     savpos        ADD       impincpos     V05pos
441700060303     C*
441800060303     C                   ENDSR
441900941123      *---------------------------------------------------------*
442000941123      *  AZZERO  totali                                         *
442100941123      *---------------------------------------------------------*
442200941123     C     AZZTOT        BEGSR
442300941123     C*
442400941123     C*  Totali in valuta
442500060502     c                   z-add     0             savtce
442600060502     c                   z-add     0             savtae
442700060502     c                   z-add     0             savpos
442800990923     C                   Z-ADD     0             V05TCE
442900060303     C                   Z-ADD     0             V05pos
443000060303     C                   Z-ADD     0             V05resto
443100990923     C                   Z-ADD     0             V05TEC
443200990923     C                   Z-ADD     0             V05TAE
443300991007     C                   Z-ADD     0             V05TDE
443400990923     C                   Z-ADD     0             V05TAV
443500941123     C*
443600991007     C                   ENDSR
443700991007      *---------------------------------------------------------*
443800991007      *  Controlli sulla quinta videata                         *
443900991007      *---------------------------------------------------------*
444000991007     C     CTR05         BEGSR
444100060303     C* .. calcolo resto
444200060323     C     V05Tec        SUB       V05Tce        v05resto
444300991007     C* Controllo se differenza è negativa e se si la rendo
444400991007     C* positiva
444500060323     C     v05resto      IFLT      0
444600060323     C                   eval      comresto = v05resto * -1
444700060323     c                   else
444800060323     c                   eval      comresto = v05resto
444900060323     C                   END
445000991007     C* differenza nei limiti ?
445100060323     C     comresto      IFGT      §GEACH
445200060412     C                   SETON                                        41
445300060411     c                   if        v05tec < v05tce
445400060323     C                   SETON                                        28
445500060411     c                   end
445600060323     C                   MOVEL     ERR(47)       $MSG
445700060323     C                   END
445800030318     c* controllo che non ci siano incassi se è già stata fatta quadratura
445900030318     c* giornaliera
446000030318     C     V05TCE        IFNE      *ZEROS
446100060303     C     V05pos        ORNE      *ZEROS
446200060303     C     V05TAE        ORNE      *ZEROS
446300030318     C     V05TAV        ORNE      *ZEROS
446400030318     C     V05TDE        ORNE      *ZEROS
446500060412     c     kqig          chain     yfqig01l
446600060412     c                   if        %found(yfqig01l)
446700030318     C                   SETON                                        28
446800030318     C                   MOVEL     ERR(88)       $MSG
446900030318     c                   end
447000030318     c                   end
447100991007     C*
447200991007     C                   ENDSR
447300941121      *---------------------------------------------------------*
447400941121      *  Aggiorno archivi                                       *
447500941121      *---------------------------------------------------------*
447600941121     C     AGGIOR        BEGSR
447700941121     C*
447800941121     C*  Eseguo apertura file di stampa
447900941121     C                   MOVEA     CMDP          CMDPX            65
448000941121     C                   Z-ADD     65            LUNCMD           15 5
448100941121     C                   CALL      'QCMDEXC'
448200941121     C                   PARM                    CMDPX
448300941121     C                   PARM                    LUNCMD
448400941121     C                   OPEN      QSYSPRT
448500941121     C*  Azzero variabili totale
448600941121     C                   Z-ADD     0             STPTPA
448700941121     C                   Z-ADD     0             STPTCL
448800941121     C                   Z-ADD     0             STPTPK
448900941121     C                   Z-ADD     0             STPTVL
449000941121     C                   Z-ADD     0             STPTST
449100970620     C                   Z-ADD     0             STPNSC
449200970305     C                   Z-ADD     0             STPNFA
449300970620     C                   Z-ADD     0             STPTNP
449400970620     C                   Z-ADD     0             STPTNV
449500941121     C                   Z-ADD     0             STPNGC
449600941121     C                   Z-ADD     0             STPNLA
449700941121     C                   Z-ADD     0             STPTNC
449800941121     C                   Z-ADD     0             STPTPK
449900941121     C                   Z-ADD     0             STPTVL
450000941121     C*  Lettura subfile dati di consegna
450100941121     C                   Z-ADD     1             NRR2
450200970926     C                   Z-ADD     1             SAVNR2
450300941121     C     NRR2          CHAIN     LR53S02                            96
450400941121     C     *IN96         DOWEQ     '0'
450500011121     C                   CLEAR                   fiftd000
450600941122     C                   Z-ADD     0             SAVPKG
450700941122     C                   Z-ADD     0             SAVNCL
450800941122     C                   Z-ADD     0             SAVVOL
450900941121     C*  Alloco record FNARB
451000941121     C                   Z-ADD     1             §§                2 0
451100941129     C                   Z-ADD     V2CAAS        KAAS
451200941129     C                   Z-ADD     V2CLNP        KLNP
451300941129     C                   Z-ADD     V2CNRS        KNRS
451400941129     C                   Z-ADD     V2CNSP        KNSP
451500941121     C     KARB1         CHAIN     FNARB01L                           7190
451600941121     C     *IN90         DOWEQ     '1'
451700941121     C                   ADD       1             §§
451800941121     C     §§            IFGT      4
451900941121     C*  Dopo 4 Tentativi mando messaggio
452000941121     C                   CALL      'FNLR53C'
452100941121     C                   PARM                    KPJBA
452200941121     C                   END
452300941121     C     KARB1         CHAIN     FNARB01L                           7190
452400941121     C                   END
452500971120     C*  reperisco AZORG della linea di arrivo
452600971120     C     ARBLNA        CHAIN     AZORG01L                           74
452700020717     C  N74              MOVEL     ORGDE3        OG143
452800971120     C                   MOVEL     'N'           WAGDT3            1
452900941122     C*  Gestisco rottura numero documento
453000031008     C     WSVNDC        IFNE      VNMDOC
453100031008     C                   Z-ADD     0             WNSS              3 0
453200031008     C                   EXSR      NEWDOC
453300031008     C                   END
453400941122     C*  Giro la data consegna
453500950602     C                   ADD       1             WNSS              3 0
453600941222     C                   MOVEL     VH2DDC        ANNO
453700941222     C                   MOVE      VH2DDC        MMGG              4 0
453800941222     C                   MOVEL     MMGG          MM1               2 0
453900941222     C                   MOVE      V02DTC        MM2               2 0
454000941222     C     MM1           IFEQ      12
454100941222     C     MM2           ANDEQ     1
454200941222     C                   ADD       1             ANNO
454300941222     C                   END
454400941222     C     MM1           IFEQ      1
454500941222     C     MM2           ANDEQ     12
454600941222     C                   SUB       1             ANNO
454700941222     C                   END
454800941122     C     V02DTC        IFEQ      0
454900941122     C                   Z-ADD     DDC(I)        G02DAT
455000941122     C                   ELSE
455100941222     C                   MOVEL     V02DTC        G02DAT
455200941222     C                   MOVE      ANNO          G02DAT
455300941122     C                   END
455400941122     C                   MOVE      *ZEROS        G02INV
455500941122     C                   MOVEL     *BLANKS       G02ERR
455600941122     C                   CALL      'XSRDA8'
455700941122     C                   PARM                    WLBDA8
455800941122     C                   Z-ADD     G02INV        DATCON
455900941122     C* Preparo l'ora avventa/mancata consegna
456000941122     C     V02HCO        IFNE      0
456100941122     C                   MOVEL     V02HCO        ORACON            4 0
456200941122     C                   MOVE      V02MCO        ORACON
456300941122     C                   ELSE
456400941122     C                   Z-ADD     HRC(I)        ORACON
456500941122     C                   END
456600941122     C*  Aggiorno campi stampa con dati di dettaglio spedizione
456700941122     C                   Z-ADD     ARBAAS        STPAAS
456800941122     C                   Z-ADD     ARBLNP        STPLNP
456900941122     C                   Z-ADD     ARBNRS        STPNRS
457000941122     C                   Z-ADD     ARBNSP        STPNSP
457100941123     C                   MOVE      ARBMGS        GG                2 0
457200941123     C                   MOVEL     ARBMGS        MM                2 0
457300941122     C                   MOVE      MM            STPGMS
457400941122     C                   MOVEL     GG            STPGMS
457500941130     C                   MOVEL     ARBRSD        STPRSD           29
457600951025     C                   Z-ADD     VH2DDT        ARBDDC
457700141212     C*
457800941122     C*  Mancata consegna
457900941122     C     V02CCN        IFNE      *BLANKS
458000941122     C                   EXSR      NOCONS
458100941212     C                   EXSR      STPCON
458200941122     C*  Aggiorno archivio conteggi padroncini
458300941122     C                   EXSR      UPDFTD
458400130722     c                   exsr      VerCorAR5
458500941122     C                   ELSE
458600941122     C*  Se no ritorno all'incasso aggiorno dati consegna
458700941122     C     V2CCON        IFEQ      'NO'
458800941122     C*  Consegna parziale
458900970304     C     V02CPZ        IFEQ      'P'
459000941123     C                   EXSR      CONPAR
459100941122     C                   ELSE
459200941122     C*  Consegna normale
459300941122     C                   EXSR      NOPARZ
459400941122     C                   END
459500941123     C                   EXSR      CONDAT
459600141212      *
459700141212ab   C*  Controllo se immesso codice particolarità
459800141212ab   C                   CLEAR                   DS7R
459900141212ab   C     V2CGMA        IFNE      *BLANKS                                      --- 03 ---
460000141212ab   C                   Z-ADD     1             X
460100141212ab   C     V2CGMA        LOOKUP    C7R(X)                                 95
460200141212ab   C     *IN95         IFEQ      '1'
460300141212ab   C                   MOVEL     D7R(X)        DS7R
460400141212ab   C                   END
460500141212ab   C                   END                                                    --- 03 ---
460600141212ab   C*  SOLO se particolarità di consegna con PINCODE
460700141212ab    *  x Pro-memoria di aver Forzato scrive una RA (61)
460800141212ab   C                   if        §7RpinCODE ='S' and
460900141212ab   c                             v2hForza = 'F'
461000141212ab   c                   exsr      write_RA_61
461100141212ab   c                   end
461200141212     C*
461300130722      *consegna parziale o totale se distinta pda cerca coordinate geo e
461400130722      * le trascrive su FIAR5
461500130722     c                   if        wpda = 'S'
461600141202ab    ***
461700141202ab   c                   exsr      Found_CET
461800141202ab    ***
461900141202ab   c                   if        ecco_il_CET = 'S'
462000140609     c                   exsr      srdocum
462100130722     c                   endif
462200130722     c                   exsr      AggCorAR5
462300130722     c                   else
462400130722     c                   exsr      VerCorAR5
462500130722     c                   endif
462600941122     C                   END
462700941130     C*
462800941130     C                   EXSR      STPCON
462900941122     C*  Incassi
463000941122     C     V2CNRA        IFNE      0
463100941122     C                   EXSR      AGGINC
463200941122     C                   END
463300941219     C*  Controllo se devo gestire non incasso
463400941219     C     V2CICA        IFEQ      'S'
463500941219     C     ARBICA        ANDNE     'Y'
463600941219     C                   MOVEL     'Y'           ARBICA
463700941219     C                   END
463800941219     C     V2CICC        IFEQ      'S'
463900941219     C     ARBICC        ANDNE     'Y'
464000941219     C                   MOVEL     'Y'           ARBICC
464100941219     C                   END
464200941130     C*  Se i flag di incasso sono uguali a *blanks impsto 'S'
464300941130     C     ARBICC        IFEQ      *BLANKS
464400990913     C     ARBACC        ANDEQ     'S'
464500941130     C                   MOVEL     'S'           ARBICC
464600941130     C                   END
464700941130     C     ARBICA        IFEQ      *BLANKS
464800990913     C     ARBACA        ANDEQ     'S'
464900941130     C                   MOVEL     'S'           ARBICA
465000941130     C                   END
465100941122     C*
465200941129     C                   MOVEL     ARBICC        COMICC
465300941122     C                   MOVEL     *BLANKS       ARBFT1
465400941123     C                   MOVEL     *BLANKS       ARBDT1
465500020826     C                   Z-ADD     dutpou        ARBIPP
465600020826     C                   Z-ADD     dutcou        ARBCUC
465700941122     C                   MOVE      'S'           ARBFDC
465800941122     C                   Z-ADD     *ZEROS        ARBNMI
465900950602     C     ARBNSS        IFEQ      0
466000970304     C     V02CPZ        ANDNE     'P'
466100970311     C     V2CIDD        ANDNE     'P'
466200950602     C                   Z-ADD     WNSS          ARBNSS
466300950602     C                   END
466400941123     C                   UPDATE    FNARB000
466500941122     C*  Aggiorno archivio conteggi padroncini
466600020524     C*    CNTPDR        IFEQ      'S'
466700941122     C                   EXSR      UPDFTD
466800020524     C*                  ELSE
466900020524     C     CNTPDR        IFEQ      'N'
467000970926     C                   EXSR      CHKSTP
467100941122     C                   END
467200941122     C*  Se c'è bolla mamma la aggiorno
467300941122     C     VH2AAP        IFNE      *ZEROS
467400941122     C                   EXSR      SPEMAM
467500941122     C                   END
467600941122     C*
467700941122     C                   END
467800140306     C*aggiorna tipo rekord GEN  spedizione esitata con PDA o no
467900140306     c                   exsr      AggGenAR5
468000941122     C*  Note Consegna
468100941122     C     V2CNT1        IFNE      *BLANKS
468200941122     C                   EXSR      NOTCON
468300941122     C                   END
468400970121     C*  Note MANCATA CONSEGNA
468500970121     C     V2CNM1        IFNE      *BLANKS
468600970121     C                   EXSR      NOTNM1
468700970121     C                   END
468800970121     C     V2CNM2        IFNE      *BLANKS
468900970121     C                   EXSR      NOTNM2
469000970121     C                   END
469100070313     C*  cancella eventuale FIARP se è rimasto
469200070313     c     karp          chain     fiarp01l
469300070313     c                   if        %found(fiarp01l)
469400070313     c                   delete    fiarp000
469500070313     C                   Endif
469600941122     C*  Nuovo record
469700941122     C                   ADD       1             NRR2
469800941122     C     NRR2          CHAIN     LR53S02                            96
469900941122     C                   END
470000031008     c                   do        5             i
470100031008     c                   if        ndc(i) > 0
470200031008     C                   EXSR      NEWDOCb
470300031008     c                   endif
470400031008     c                   enddo
470500941122     C                   EXSR      GESTOT
470600941122     C                   CLOSE     QSYSPRT
470700971001     C*  MEMORIZZAZIONE STOP PADRONCINI A FINE CICLO
470800971001     C     YY            IFGT      *ZEROS
470900971001     C                   Z-ADD     SAVNR2        NRR2
471000971001     C                   EXSR      ELASTP
471100971001     C                   Z-ADD     NRR2          SAVNR2
471200971001     C                   CLEAR                   NST
471300971001     C                   CLEAR                   SPS
471400971001     C                   MOVE      *ZEROS        YY                3 0
471500971001     C                   END
471600941122     C*
471700941122     C                   COMMIT
471800090804      * operazioni dopo commit
471900090804     C*  verifica record spia per variazione bolla
472000090804     c                   exsr      srback2
472100090804     C*  cancella record spia per variazione bolla
472200090804     c     *loval        setll     wfvarb1l
472300090804     c                   do        *hival
472400090804     c                   read      wfvarb1l
472500090804     c                   if        %eof(wfvarb1l)
472600090804     c                   leave
472700090804     C                   Endif
472800090804     c                   delete    wfvarb00
472900090804     C                   Enddo
473000941122     C                   ENDSR
473100941122      *---------------------------------------------------------*
473200941122      *  Nuovo Documento
473300941122      *---------------------------------------------------------*
473400941122     C     NEWDOC        BEGSR
473500941122     C*
473600970227     C*  Se non è la prima volta e ci sono delle bolle con la
473700970227     C*  data consegna = a quella della distinta richiamo pgm
473800031008     C     WSVNDC        IFNE      0
473900031008     C     WNBOL         IFNE      0
474000970227     C                   MOVEL     'S'           WRISTA            1
474100970227     C                   MOVEL     *BLANKS       SA2TLA            1
474200970227     C                   MOVEL     'C'           SA2TRC            1
474300970228     C                   Z-ADD     CONDIS        SA2DTS            8 0
474400970227     C                   Z-ADD     D53LNA        SA2LNA            3 0
474500970227     C                   CALL      'FNLSA2R3'
474600970227     C                   PARM                    SA2TLA            1
474700970227     C                   PARM                    SA2TRC            1
474800970227     C                   PARM                    SA2DTS            8 0
474900970227     C                   PARM                    SA2LNA            3 0
475000031008     C                   END
475100970926     C*  MEMORIZZO STOP PADRONCINI
475200971001     C     YY            IFGT      *ZEROS
475300970926     C                   Z-ADD     SAVNR2        NRR2
475400970926     C                   EXSR      ELASTP
475500970926     C                   END
475600971110     C                   Z-ADD     NRR2          SAVNR2
475700031008     C                   END
475800970926     C                   CLEAR                   NST
475900970926     C                   CLEAR                   SPS
476000970926     C                   MOVE      *ZEROS        YY                3 0
476100031008     c                   z-add     1             i                 2 0
476200031008     c     arbndc        lookup    ndc(i)                                 95
476300031008     C     DDC(I)        IFEQ      *ZEROS
476400031008     C     *IN95         OREQ      '0'
476500100210     C                   Z-ADD     WOGGI         DDC(I)
476600031008     C                   END
476700031008     C*  Giro data consegna x distinta
476800031008     C                   MOVE      DDC(I)        G02DAT
476900031008     C                   MOVEL     *BLANK        G02ERR
477000031008     C                   CALL      'XSRDA8'
477100031008     C                   PARM                    WLBDA8
477200031008     C                   Z-ADD     G02INV        CONDIS            8 0
477300031008     C*
477400031008     C                   MOVE      DTD(I)        ANNO1
477500031008     C                   MOVEL     DTD(I)        MMGG
477600031008     C                   MOVE      MMGG          MM1
477700031008     C                   Z-ADD     0             WNBOL             4 0
477800031008     C                   MOVE      VNMDOC        WSVNDC
477900031008     c                   endsr
478000031008     c*-----------------------------------------------------------------
478100031008     c     newdocb       begsr
478200031008     c*-----------------------------------------------------------------
478300970227     C*  data e ora di consegna nuovo documento
478400941122     C*  Sfleggo FLFVV
478500000821     C                   MOVEL     KPJBU         WPJBU           256
478600031024      * per estero è a zero
478700031024     c                   if        d53fgs = 0
478800031024     c                   z-add     d53lna        kfgs
478900031024     c                   else
479000031016     c                   z-add     d53fgs        kfgs
479100031024     c                   end
479200031016     C*                  Z-ADD     ARBifp        Kfgs
479300031008     C                   Z-ADD     ndc(i)        KNFV
479400000626     C                   Z-ADD     4             KNPG
479500070316      * se trovo chiudo il dst
479600070316     C                   Z-ADD     ndc(i)        KNFV6
479700070410     C     Kdst          CHAIN     fidst09L                           9594
479800070316     C     *IN95         IFEQ      '0'
479900070410     C                   move      'S'           dstFCF
480000070410     C                   move      FPP(I)        dstFPP
480100070410     C                   move      PEP(I)        dstPEP
480200070316     c                   update    fidst000
480300070316     c                   endif
480400120418     c* crea la fase di chiusura distinta su AS400
480500120418     c                   eval      dsfatb = ' '
480600120418     c                   eval      dsffgs = dstfgs
480700120418     c                   eval      dsfnpg = dstnpg
480800120418     c                   eval      dsfnfv = dstnfv
480900120418     c                   eval      dsftrd = 'CDC'
481000120418     c                   time                    DSFDTORIN
481100120418     C                   TIME                    dtorin           14 0
481200120418     C                   MOVEl     dtorin        orin              6 0
481300120418     C                   MOVE      orin          DSFDTORIN
481400120418     C                   MOVE      dtorin        G02DAT
481500120418     C                   MOVEL     *BLANK        G02ERR
481600120418     C                   CALL      'XSRDA8'
481700120418     C                   PARM                    WLBDA8
481800120418     C                   movel     G02INV        DSFDTORIN
481900120418     c                   eval      DSFPDAAS4 = 'A'
482000120418     c                   eval      DSFdati = knmus
482100120418     c                   write     fidsf00
482200000821      *                    ------         ----
482300020823     C     KFVV          CHAIN     FNFVV01L                           95
482400941122     C     *IN95         IFEQ      '0'
482500941122     C                   MOVE      'S'           FVVFCF
482600941122     C                   MOVE      FPP(I)        FVVFPP
482700941122     C                   MOVE      PEP(I)        FVVPEP
482800941123     C                   UPDATE    FNFVV000
482900000526     C                   MOVEL     FVVDS         FVVDS2
483000000821      *
483100000821      *  Sottometto Trasmissione Spunte CAT 4
483200041021     C*                  CLEAR                   DSLR01
483300041021     C*                  Z-ADD     FVVFGS        DLRFGS                         FIL.GESTIONE
483400041021     C*                  Z-ADD     FVVNPG        DLRNPG                         CATEGORIA
483500041021     C*                  Z-ADD     FVVNFV        DLRNFV                         NUM.FOGLIO
483600041021     C*                  MOVE(P)   '0'           W52ALF           52            "PARNUM"
483700041021     C*                  MOVEL(P)  W52ALF        KPJBU
483800041021     C*                  MOVEL     DSLR01        KPJBU
483900041021     C*                  MOVE      'LR96'        KCOAZ
484000041021     C*                  CALL      'BCH10'
484100041021     C*                  PARM                    KPJBA
484200000821      *
484300000619     C                   Z-ADD     8             KNPG
484400000821      *                    ------         ----
484500020823     C     KFVV          CHAIN     FNFVV01L                           95
484600000526     C     *IN95         IFEQ      '0'
484700000526     C                   MOVEL     FVVDS2        FVVDS
484800000619     C                   Z-ADD     8             FVVNPG
484900000526     C                   UPDATE    FNFVV000
485000010601      *  Sottometto Trasmissione Spunte CAT 8
485100010601     C                   CLEAR                   DSLR01
485200041021     C*                  Z-ADD     FVVFGS        DLRFGS                         FIL.GESTIONE
485300041021     C*                  Z-ADD     FVVNPG        DLRNPG                         CATEGORIA
485400041021     C*                  Z-ADD     FVVNFV        DLRNFV                         NUM.FOGLIO
485500041021     C*                  MOVE(P)   '0'           W52ALF           52            "PARNUM"
485600041021     C*                  MOVEL(P)  W52ALF        KPJBU
485700041021     C*                  MOVEL     DSLR01        KPJBU
485800041021     C*                  MOVE      'LR96'        KCOAZ
485900041021     C*                  CALL      'BCH10'
486000041021     C*                  PARM                    KPJBA
486100010601     C*
486200000526     C                   MOVEL     FVVDS2        FVVDS
486300000821      *
486400000821      *
486500941122     C                   END
486600000526     C                   END
486700000821     C                   MOVEL     WPJBU         KPJBU
486800000626     C                   Z-ADD     4             KNPG
486900000821      *                    ------         ----
487000941122     C*  Aggiorno archivio conteggi padroncini
487100020524     C*    CNTPDR        IFEQ      'S'
487200941122     C                   EXSR      UPDFTT
487300020524     C*                  END
487400031008
487500941122     C                   ENDSR
487600031008      *---------------------------------------------------------*
487700031008      *  pretesta
487800031008      *---------------------------------------------------------*
487900031008     C     pretesta      BEGSR
488000031008     C*  Stampo dati testata
488100031008     C                   MOVEL     CMAT          TPPRE            10
488200031013     C                   MOVEL     CMAT          TPPRE2           10
488300031013     C                   MOVEL     CMAT          TPPRE3           10
488400031013     C                   MOVEL     CMAT          TPPRE4           10
488500031013     C                   MOVEL     CMAT          TPPRE5           10
488600031008     C     FPP(1)        IFEQ      'P'
488700031008     C                   MOVEL     CPOM          TPPRE            10
488800031008     C                   END
488900031008     C                   Z-ADD     NDC(1)        STPNDC
489000031008     C                   Z-ADD     DTD(1)        STPDDC
489100031008     C                   Z-ADD     ARBPDC        STPPDR
489200031008     C                   MOVEL     D53DSF        DESPAD
489300031008     c                   setoff                                       737677
489400031008     c                   setoff                                       78
489500031008     c                   if        ndc(2) > 0
489600031013     C     FPP(2)        IFEQ      'P'
489700031013     C                   MOVEL     CPOM          TPPRE2
489800031013     C                   END
489900031008     c                   seton                                        73
490000031008     c                   endif
490100031008     c                   if        ndc(3) > 0
490200031013     C     FPP(3)        IFEQ      'P'
490300031013     C                   MOVEL     CPOM          TPPRE3
490400031013     C                   END
490500031008     c                   seton                                        76
490600031008     c                   endif
490700031008     c                   if        ndc(4) > 0
490800031013     C     FPP(4)        IFEQ      'P'
490900031013     C                   MOVEL     CPOM          TPPRE4
491000031013     C                   END
491100031008     c                   seton                                        77
491200031008     c                   endif
491300031008     c                   if        ndc(5) > 0
491400031013     C     FPP(5)        IFEQ      'P'
491500031013     C                   MOVEL     CPOM          TPPRE5
491600031013     C                   END
491700031008     c                   seton                                        78
491800031008     c                   endif
491900031008     C                   EXCEPT    TESTA
492000031008     C*
492100031008     C                   ENDSR
492200941122      *---------------------------------------------------------*
492300941122      *  No Consegna
492400941122      *---------------------------------------------------------*
492500941122     C     NOCONS        BEGSR
492600941122     C*
492700951205     C*  Se evento immesso da utente è un lasciato avviso devo
492800951206     C*  ristampare la bolle
492900070405     C                   EXSR      AVVSTB
493000951205     C*
493100960205     C     V2CEVP        IFEQ      *BLANKS
493200960205     C                   MOVEL     V02CCN        V2CEVP
493300960205     C                   END
493400941125     C                   Z-ADD     1             X
493500040224     C*    V02CCN        LOOKUP    C2A(X)                                 95
493600040224     C     V2cevp        LOOKUP    C2A(X)                                 95
493700951205     C                   MOVEL     D2A(X)        DS2A
493800070406     C                   MOVEL     D2z(X)        DS2z
493900941130     C                   MOVEL     §2ADES        STPDCO           20
494000941123     C                   MOVEL     §2AFTC        STPTMC
494100941206     C                   MOVEL     V02CCN        STPCMC
494200941122     C*  Incremento totali stampa
494300941122     C                   ADD       1             STPNSC
494400941122     C                   ADD       V02NCL        STPTNC
494500941122     C                   ADD       V02PKG        STPTNP
494600941122     C                   ADD       V02VOL        STPTNV
494700941122     C*  Totale non fatte, giacenti, lasc. avviso
494800941122     C     §2AFTC        IFEQ      'N'
494900941130     C                   ADD       1             STPNFA
495000941122     C                   ELSE
495100941122     C     §2AFTC        IFEQ      'G'
495200941122     C                   ADD       1             STPNGC
495300941122     C                   ELSE
495400941122     C     §2AFTC        IFEQ      'A'
495500941122     C                   ADD       1             STPNLA
495600941122     C                   END
495700941122     C                   END
495800941122     C                   END
495900941122     C*  Controllo se addizionare numero tentativi consegna
496000941122     C     §2ANTC        IFEQ      'S'
496100941122     C                   ADD       1             ARBNTC
496200941122     C                   END
496300951228     C*  Se forzato l.avviso metto 2 nei tentavi consegna
496400951228     C     V2CF08        IFEQ      '1'
496500990419     C     V2C4NT        ADD       1             ARBNTC
496600951228     C                   END
496700941122     C*  Controllo se devo mettere blocco su bolla
496800941122     C     §2AAGB        IFEQ      'N'
496900950113     C     V2CEVP        OREQ      'RIC'
497000941122     C                   MOVEL     *BLANKS       ARBCMC
497100941122     C                   ELSE
497200960306     C                   MOVE      V2CEVP        ARBCMC
497300941122     C                   END
497400941122     C*  Se vale come giacenza metto blocco
497500941122     C     §2AFTG        IFEQ      'S'
497600950113     C     V2CEVP        ANDNE     'RIC'
497700941122     C                   MOVE      §2AFTC        ARBFBC
497800941122     C                   END
497900941122     C*
498000950113     C     V2CEVP        IFNE      'RIC'
498100941122     C                   MOVE      §2AFTC        ARBTMC
498200951205     C                   ELSE
498300951205     C                   MOVE      WSVFTC        ARBTMC
498400950113     C                   END
498500140207      *scrittura record spia per invio sms/mail al destinatario in caso
498600140207      *di lasciato avviso al 1° tentativo di consegna
498700140207      *N.B deve essere richiamato prima dell'aggiornamento di arb
498800140218     C     V2CEVP        IFEQ      'RIC'
498900140207     c                   exsr      srspiasms
499000140207     c                   end
499100001229     C*PREPARO CAMPI PER NOTE EVENTI
499200001229      *
499300941122     C                   Z-ADD     0             ARBNGD
499400020902     C                   Z-ADD     0             ARBifp
499500131112     C                   Z-ADD     arbndc        wARBNDC           7 0
499600020902     C                   Z-ADD     0             ARBNDC
499700941122     C                   Z-ADD     0             ARBSTP
499800941122     C                   Z-ADD     0             ARBNSS
499900941122     C                   MOVEL     *BLANKS       ARBFDC
500000071003     C* se ho la data consegna richiesta proposta da telefonata non blocco
500100071003     c* la bolla per lasciato avviso
500200101020      * ?asteriscato altrimenti venivano R.B le variazioni inserite da fiarp a fine chiusura
500300101020      * ?i campi vengono puliti dal pgm di assegnazione automatico prima di rimettere a giro le boll
500400110418      * ?TOLTI ASTERISCHI RIMANEVANO BLOCCATE BOLLE CON LASCIATO AVVISO E RIMANDO ALLO SVINCOLO
500500110418     c                   if        v2harp = '1' and arbdcr > 0 and
500600110418     c                             arbfbc = 'A'
500700110418     c                   clear                   arbfbc
500800110418     c                   clear                   arbcmc
500900110418     c                   end
501000941123     C                   UPDATE    FNARB000
501100941122     C*  Controllo se devo gestire scrittura eventi
501200131120     c                   clear                   scriviCEP         1
501300941122     C     §2AEVB        IFEQ      'S'
501400950113     C                   MOVEL     V2CEVP        EVBCEV
501500941122     C                   EXSR      EVENTI
501600941122     C                   END
501700131108     c* verifica se ricevuto doppio evento CET/CEP
501800131120     c                   if        scrivicep = 'S'
501900131108     c                   exsr      eventicep
502000131120     c                   endif
502100070319     c*verifica se inserire il blocco alla consegna se esiste una c.a. aperta
502200070319     c*solo se in organigramma è abilitato il flag per la gestione
502300070319     c                   if        §oggeot = 'S'
502400070319     c                   exsr      insbloc
502500070319     c                   endif
502600110328      * scrive spia file fiilv00f solo per distinte PDA
502700110328     c                   if        wpda  = 'S'
502800110328     c                   move      'S'           wnocons           1
502900110328     c                   exsr      scrivispia
503000110328     c                   endif
503100950615     C*
503200950919     C*  Se non fatta chiudo l'anomalia con causale 'CH'
503300950915     C                   EXSR      CLOANM
503400950919     C*
503500941122     C                   ENDSR
503600950615     C*-----------------------------------------------------*
503700950615     C*  Chiudo eventuali anomalie su dettaglio segnacolli
503800950615     C*-----------------------------------------------------*
503900950915     C     CLOANM        BEGSR
504000960418     C*
504100960418     C                   CLEAR                   DSLR33
504200960418     C                   MOVE      '4'           D33FSC
504300961106     C                   Z-ADD     OGGI          D33DCH
504400960418     C                   MOVEL     'CH'          D33CCH
504500960426     C                   MOVEL     V02CCA        D33CCA
504600960418     C*
504700000207     C     HARLNP        IFNE      0
504800990519     C                   Z-ADD     HARAAS        KAA4
504900950915     C                   Z-ADD     HARLNP        KLNP
505000950915     C                   Z-ADD     HARNRS        KNRS
505100950915     C                   Z-ADD     HARNSP        KNSP
505200960418     C*
505300960418     C                   Z-ADD     1             II                1 0
505400960418     C     II            DOWLE     2
505500960418     C     II            IFEQ      1
505600960418     C*  Vado a chiudere le anomalie di fuori posto
505700950915     C                   Z-ADD     135           KCAA
505800960418     C                   ELSE
505900960418     C*  Vado a chiudere le anomalie di bolla in consegna su altra disti.
506000960418     C                   Z-ADD     150           KCAA
506100960418     C                   END
506200990519     C     KANM1         CHAIN     FNANM01L                           34
506300950915     C     *IN34         DOWEQ     '0'
506400960418     C                   Z-ADD     ANMNR1        D33NRR
506500960418     C                   CALL      'FNLR33R'
506600960418     C                   PARM                    DSLR33
506700990519     C     KANM1         READE     FNANM01L                               34
506800960418     C                   ENDDO
506900960418     C                   ADD       1             II
507000960418     C                   ENDDO
507100000207     C                   END
507200950615     C*
507300950915     C                   ENDSR
507400941122      *---------------------------------------------------------*
507500941122      *  Consegna Parziale
507600941122      *---------------------------------------------------------*
507700941123     C     CONPAR        BEGSR
507800941122     C*
507900950410     C                   MOVEL     'N'           WMDDTI            1
508000950331     C                   MOVEL     'N'           WFBS
508100021107     C                   clear                   wpesparz
508200021107     C                   clear                   wvolparz
508300941201     C                   Z-ADD     V02PKG        SAVPKG
508400941201     C                   Z-ADD     V02NCL        SAVNCL
508500941201     C                   Z-ADD     V02VOL        SAVVOL
508600941122     C                   Z-ADD     V2CPDS        NRR7
508700941122     C                   Z-ADD     0             V02NCL
508800941122     C     NRR7          CHAIN     LR53S07                            95
508900941122     C     *IN95         DOWEQ     '0'
509000941122     C     SFHAAS        ANDEQ     HARAAS
509100941122     C     SFHLNP        ANDEQ     HARLNP
509200941122     C     SFHNRS        ANDEQ     HARNRS
509300941122     C     SFHNSP        ANDEQ     HARNSP
509400941129     C     SFHSEL        IFNE      '1'
509500941129     C                   Z-ADD     SFHAAS        KAAS1
509600941129     C                   Z-ADD     SFHLNP        KLNP1
509700941129     C                   Z-ADD     SFHNRS        KNRS1
509800941129     C                   Z-ADD     SFHNSP        KNSP1
509900941129     C                   Z-ADD     SFHSCN        KNSC
510000941129     C     KART2         CHAIN     FNART01L                           94
510100941122     C     *IN94         IFEQ      '0'
510200941122     C                   Z-ADD     DATCON        ARTDCM
510300950214     C                   MOVEL     *BLANKS       ARTDAB
510400961113     C                   MOVEL     ' '           ARTFTR
510500941122     C* Se manca data arrivo imposto = Data consegna
510600941122     C     ARTDAM        IFEQ      0
510700970620     C     V02CCA        ANDNE     '5'
510800950331     C                   MOVEL     'S'           WFBS              1
510900941122     C                   Z-ADD     DATCON        ARTDAM
511000941122     C                   Z-ADD     85            ARTNAP
511100941122     C                   END
511200961219     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
511300961219     C* ARTDTR  solo x estero
511400961219     C     ARTDTR        IFEQ      0
511500961219     C     V02CCA        ANDEQ     '5'
511600970114     C     ARBLNA        ANDNE     ARBLNP
511700020717     C**** ORGFL1        ANDEQ     'E'
511800020717     C     §OgNTW        ifeq      'FED'
511900020717     C     §OgNTW        oreq      'EEX'
512000020717     C     §OgNTW        oreq      'EUP'
512100961219     C                   Z-ADD     DATCON        ARTDTR
512200020717     C                   End
512300961219     C                   END
512400941122     C                   UPDATE    FNART000
512500941122     C* Sistemo eventuali anomalie del collo consegnato
512600941122?    C                   EXSR      GESANM
512700021107      *
512800021107      * somma i colli consegnati
512900941122     C                   ADD       1             V02NCL
513000021107      *
513100021107      *  Se si tratta di un unica spedizione oppure se multicollo
513200021107      *   e tutti i colli sono passati x VDL memorizza il peso reale
513300021107      *    da sottrarre in seguito per il calcolo del parziale
513400050208     C     arbncp        ifeq      arbncl
513500021107     C                   add       artPUC        wpesparz
513600021107     C                   end
513700021107      *
513800050208     C     arbncr        ifeq      arbncl
513900021107     C                   add       artVUC        wvolparz
514000021107     C                   endif
514100021107      *
514200941122     C                   END
514300941130     C                   END
514400941129     C                   ADD       1             NRR7
514500941129     C     NRR7          CHAIN     LR53S07                            95
514600941122     C                   END
514700941206     C*
514800021107     C*  Ricalcolo il peso / volume
514900021107     C*
515000021107      *  Se si tratta di un unica spedizione oppure se multicollo
515100021107      *   e tutti i colli sono passati x VDL
515200021107      *    imposta il peso calcolato con precisione dal VDL.
515300021107     c                   if        wpesparz > 0
515400021107     c                   z-add     wpesparz      V02pkg
515500021107     c                   else
515600021107     C**** ARBPKF        MULT      V02NCL        APPOG            18 3
515700021107     C     V02PKG        MULT      V02NCL        APPOG            18 3
515800040211     C***  APPOG         DIV(H)    ARBNCL        V02PKG
515900040211     C     APPOG         DIV(H)    SAVNCL        V02PKG
516000021107     c                   end
516100021107     C*
516200021107     C*
516300021107      *  Se si tratta di un unica spedizione oppure se multicollo
516400021107      *   e tutti i colli sono passati x VDL
516500021107      *    imposta il volume calcolato con precisione dal VDL.
516600021107     c                   if        wvolparz > 0
516700021107     c                   z-add     wvolparz      V02vol
516800021107     c                   else
516900941207     C     V02VOL        MULT      V02NCL        APPOG            18 3
517000040211     C***  APPOG         DIV(H)    ARBNCL        V02VOL
517100040211     C     APPOG         DIV(H)    SAVNCL        V02VOL
517200021107     c                   end
517300941201     C*
517400941201     C*  Incremento totale colli non consegnati
517500941201     C                   SUB       V02NCL        SAVNCL
517600941201     C                   SUB       V02PKG        SAVPKG
517700941201     C                   SUB       V02VOL        SAVVOL
517800941201     C                   ADD       SAVNCL        STPTNC
517900941201     C                   ADD       SAVPKG        STPTNP
518000941201     C                   ADD       SAVVOL        STPTNV
518100941122     C*
518200941122     C*  Chain tab. eventi
518300941123     C                   MOVEL     V02CPZ        JEY               3
518400941122     C                   SETOFF                                       95
518500941122     C                   Z-ADD     1             X
518600941123     C     JEY           LOOKUP    C2A(X)                                 95
518700941123     C                   MOVEL     D2A(X)        DS2A
518800070406     C                   MOVEL     D2z(X)        DS2z
518900941122     C*
519000941122     C* Se c'è 'S' in tentata consegna vado addizionare 1
519100941130     C     §2ANTC        IFEQ      'S'
519200941202     C                   ADD       1             ARBNTC
519300941122     C                   END
519400941122     C* Se evento da memorizzare lo memorizzo
519500131120     c                   clear                   scriviCEP
519600941122     C     §2AEVB        IFEQ      'S'
519700941130     C                   MOVEL     *BLANKS       EVBCEV
519800941130     C                   MOVEL     V02CPZ        EVBCEV
519900001229     C* PREPARO CAMPI NOTE EVENTI
520000941122?    C                   EXSR      EVENTI
520100941122     C                   END
520200131108     c* verifica se ricevuto doppio evento CET/CEP
520300131120     c                   if        scrivicep = 'S'
520400131108     c                   exsr      eventicep
520500131120     c                   endif
520600070319     C* Incremento contatore consegne parziali
520700941122     C                   ADD       1             STPTPA
520800941122     C*
520900941122     C                   MOVE      §2AFTC        ARBTMC
521000941122     C                   Z-ADD     0             ARBNGD
521100941122     C                   Z-ADD     0             ARBDCM
521200941122     C                   Z-ADD     0             ARBHMC
521300941122     C                   Z-ADD     0             ARBSTP
521400941122     C                   Z-ADD     0             ARBNSS
521500941122     C* Se prima consegna parziale aggiorno la data
521600941122     C     ARBDCP        IFEQ      0
521700941122     C                   Z-ADD     DATCON        ARBDCP
521800941122     C                   END
521900070319     c*verifica se inserire il blocco alla consegna se esiste una c.a. aperta
522000070319     c*solo se in organogramma è abilitato il flag per la gestione
522100070319     c                   if        §oggeot = 'S'
522200070319     c                   exsr      insbloc
522300070319     c                   endif
522400110328      * scrive spia file fiilv00f solo per distinte PDA
522500110207     c                   if        wpda  = 'S'
522600110328     c                   clear                   wnocons
522700110207     c                   exsr      scrivispia
522800110207     c                   endif
522900950919     C*  Se non fatta chiudo l'anomalia con causale 'CH'
523000950919     C                   EXSR      CLOANM
523100941122     C*
523200941122     C                   ENDSR
523300951205      *---------------------------------------------------------*
523400951205      *  Controllo se devo ristampare la bolla
523500951205      *---------------------------------------------------------*
523600951205     C     AVVSTB        BEGSR
523700951205     C*
523800951205     C                   Z-ADD     1             X
523900951205     C     V02CCN        LOOKUP    C2A(X)                                 95
524000951205     C                   MOVEL     D2A(X)        DS2A
524100070406     C                   MOVEL     D2z(X)        DS2z
524200951205     C                   MOVEL     §2AFTC        WSVFTC            1
524300951205     C*
524400951205     C                   ENDSR
524500941122      *---------------------------------------------------------*
524600941122      *  Consegna Normale
524700941122      *---------------------------------------------------------*
524800941122     C     NOPARZ        BEGSR
524900941122     C*
525000950331     C                   MOVEL     'N'           WMDDTI            1
525100950410     C                   MOVEL     'N'           WFBS
525200941122     C                   Z-ADD     DATCON        ARBDCM
525300941122     C                   Z-ADD     ORACON        ARBHMC
525400941122     C                   Z-ADD     V02STP        ARBSTP
525500941129     C                   Z-ADD     HARAAS        KAAS1
525600941129     C                   Z-ADD     HARLNP        KLNP1
525700941129     C                   Z-ADD     HARNRS        KNRS1
525800941129     C                   Z-ADD     HARNSP        KNSP1
525900090622      *cerca ed eventualmente elimina eventi particolari immessi in automatico con
526000090622      * data o ora maggiori rispetto alla consegna
526100090622?    C                   EXSR      DelEvePar
526200970304      *---------------------------------------------------------*
526300970304     C*  Aggiorno i Segnacolli: se non ho aperto pratica IDD
526400970304     C*                         leggo tutti i segnacolli in
526500970304     C*                         base al numero spedizione e
526600970304     C*                         li aggiorno.
526700970805     C     V2CIDD        IFEQ      ' '
526800980722     C                   ADD       1             ARBNTC
526900970620     C     KART1         CHAIN     FNART01L                           72
527000970620     C     *IN72         IFEQ      '1'
527100021107     C********           Z-ADD     ARBPKF        SAVPKG
527200970620     C                   Z-ADD     ARBNCL        SAVNCL
527300021107     C* Calcolo Volume
527400970620     C                   EXSR      CALVOL
527500970620     C                   Z-ADD     WVOL          SAVVOL
527600021107     C* Calcolo peso
527700021107     C                   exsr      calPES
527800021107     C                   Z-ADD     WPes          SAVPKG
527900970620     C                   END
528000970620     C     *IN72         DOWEQ     '0'
528100941129     C     ARTDCM        IFEQ      0
528200941122     C                   Z-ADD     DATCON        ARTDCM
528300950214     C                   MOVEL     *BLANKS       ARTDAB
528400941220     C     ARBDCP        IFNE      0
528500961113     C                   MOVEL     ' '           ARTFTR
528600941220     C                   END
528700941122     C* Se manca data arrivo imposto = data consegna
528800941122     C     ARTDAM        IFEQ      0
528900970620     C     V02CCA        ANDNE     '5'
529000941122     C                   Z-ADD     DATCON        ARTDAM
529100941122     C                   Z-ADD     85            ARTNAP
529200950329     C                   MOVEL     'S'           WMDDTI
529300941122     C                   END
529400961219     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
529500961219     C* ARTDTR solo x estero
529600961219     C     ARTDTR        IFEQ      0
529700961219     C     V02CCA        ANDEQ     '5'
529800970114     C     ARBLNA        ANDNE     ARBLNP
529900020717     C**** ORGFL1        ANDEQ     'E'
530000020717     C     §OgNTW        ifeq      'FED'
530100020717     C     §OgNTW        oreq      'EEX'
530200020717     C     §OgNTW        oreq      'EUP'
530300961219     C                   Z-ADD     DATCON        ARTDTR
530400020717     c                   end
530500961219     C                   END
530600941123     C                   UPDATE    FNART000
530700941122     C* Sistemo eventuali anomalie del collo consegnato
530800941122?    C                   EXSR      GESANM
530900941123     C                   END
531000941122     C     KART1         READE     FNART01L                               72
531100941122     C                   END
531200970304     C*---------------------------------------------------------*
531300970304     C*                       - se ho aperto pratica IDD leggo
531400970304     C*                         subfile segnacolli e mi posiz.
531500970304     C*                         x nr.spedizione/nr.segnacollo
531600970304     C*                         e li aggiorno.
531700970805     C                   ELSE
531800970805     C                   EXSR      CHNART
531900970805     C                   END
532000970304     C*---------------------------------------------------------*
532100120814     C* Scrive record per segnalare bolla con obbligo restituzione BAM o particolarità simile
532200960116     C     V2C1BF        IFNE      *BLANKS
532300120814      * imposta se c'è linea partenza bolla mamma altrimenti linea partenza bolla
532400120814      * per indicare dove deve essere restituita la "bolla firmata"
532500120814     c                   clear                   darbflrx
532600120814     c                   if        vh2lpp <> 0
532700120814     c                   move      vh2lpp        arblnpx
532800120814     c                   else
532900120814     c                   move      v2clnp        arblnpx
533000120814     c                   endif
533100120814     c                   movel     darbflrx      arbflr
533200960116     C                   Z-ADD     D53FGS        ARBFGS
533300960116     C                   Z-ADD     0             ARBDST
533400960116     C                   WRITE     FNARBX00
533500960116     C                   END
533600961219     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
533700961219     C* ARTDTR solo x estero
533800961219     C     ARBDT3        IFEQ      0
533900961219     C     V02CCA        ANDEQ     '5'
534000970114     C     ARBLNA        ANDNE     ARBLNP
534100020717     C**** ORGFL1        ANDEQ     'E'
534200020717     C     §OgNTW        ifeq      'FED'
534300020717     C     §OgNTW        oreq      'EEX'
534400020717     C     §OgNTW        oreq      'EUP'
534500060203     C     §OgNTW        oreq      'DPD'
534600961219     C                   MOVEL     'S'           WAGDT3
534700961219     C                   Z-ADD     DATCON        ARBDT3
534800020717     C                   End
534900961219     C                   END
535000970227     C*  Se data consegna della bolla è diversa dalla da quella
535100970227     C*  indicata in distinta richiamo pgm x scrittura FNSTA
535200970227     C     DATCON        IFNE      CONDIS
535300970227     C                   MOVEL     'S'           WRISTA            1
535400970227     C                   MOVEL     *BLANKS       SA2TLA            1
535500970227     C                   MOVEL     'C'           SA2TRC            1
535600970227     C                   Z-ADD     DATCON        SA2DTS            8 0
535700970227     C                   Z-ADD     D53LNA        SA2LNA            3 0
535800970227     C                   CALL      'FNLSA2R3'
535900970227     C                   PARM                    SA2TLA            1
536000970227     C                   PARM                    SA2TRC            1
536100970227     C                   PARM                    SA2DTS            8 0
536200970227     C                   PARM                    SA2LNA            3 0
536300970227     C*  altrimenti aggiorno contatore bolle
536400970227     C                   ELSE
536500970227     C                   ADD       1             WNBOL
536600970227     C                   END
536700070319     c*elimina eventuali blocchi dalla bolla
536800070319     c*solo se in organigramma è abilitato il flag per la gestione
536900070319     c                   if        §oggeot = 'S'
537000070319     c                   exsr      elibloc
537100070319     c                   endif
537200110328      * scrive spia file fiilv00f solo per distinte PDA
537300110207     c                   if        wpda  = 'S'
537400110328     c                   clear                   wnocons
537500110207     c                   exsr      scrivispia
537600110207     c                   endif
537700070319     c
537800941122     C*
537900941122     C                   ENDSR
538000970304      *---------------------------------------------------------*
538100970304      *  Pratica IDD
538200970304      *---------------------------------------------------------*
538300970304     C     CHNART        BEGSR
538400970304     C*
538500021107     C*****              Z-ADD     ARBPKF        SAVPKG
538600970304     C                   Z-ADD     ARBNCL        SAVNCL
538700021107     C* Calcolo Volume
538800970304     C                   EXSR      CALVOL
538900970304     C                   Z-ADD     WVOL          SAVVOL
539000021107     C* Calcolo peso
539100021107     C                   exsr      calPES
539200021107     C                   Z-ADD     WPes          SAVPKG
539300970304     C* Leggo subfile segnacolli e aggiorno dett.colli
539400970304     C                   Z-ADD     V2CPDS        NRR7
539500970304     C     NRR7          CHAIN     LR53S07                            95
539600970304     C     *IN95         DOWEQ     '0'
539700970304     C     SFHAAS        ANDEQ     HARAAS
539800970304     C     SFHLNP        ANDEQ     HARLNP
539900970304     C     SFHNRS        ANDEQ     HARNRS
540000970304     C     SFHNSP        ANDEQ     HARNSP
540100970304     C                   Z-ADD     SFHAAS        KAAS1
540200970304     C                   Z-ADD     SFHLNP        KLNP1
540300970304     C                   Z-ADD     SFHNRS        KNRS1
540400970304     C                   Z-ADD     SFHNSP        KNSP1
540500970304     C                   Z-ADD     SFHSCN        KNSC
540600970304     C     KART2         CHAIN     FNART01L                           72
540700970311     C*  Aggiorno dati consegna se tutti consegnati o colli chiusi con IDD
540800970311     C     V2CIDD        IFEQ      'T'
540900970311     C     SFHSEL        OREQ      '1'
541000970311     C     ARTDCM        IFEQ      0
541100970304     C                   Z-ADD     DATCON        ARTDCM
541200970304     C                   MOVEL     *BLANKS       ARTDAB
541300970304     C     ARBDCP        IFNE      0
541400970304     C                   MOVEL     ' '           ARTFTR
541500970304     C                   END
541600970304     C* Se manca data arrivo imposto = data consegna
541700970304     C     ARTDAM        IFEQ      0
541800970320     C     V2CIDD        ANDEQ     'T'
541900970320     C     SFHSEL        ANDNE     '1'
542000970304     C                   Z-ADD     DATCON        ARTDAM
542100970304     C                   Z-ADD     85            ARTNAP
542200970304     C                   MOVEL     'S'           WMDDTI
542300970304     C                   END
542400970304     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
542500970304     C* ARTDTR solo x estero
542600970304     C     ARTDTR        IFEQ      0
542700970304     C     ARBLNA        ANDNE     ARBLNP
542800020717     C**** ORGFL1        ANDEQ     'E'
542900020717     C     §OgNTW        ifeq      'FED'
543000020717     C     §OgNTW        oreq      'EEX'
543100020717     C     §OgNTW        oreq      'EUP'
543200970304     C                   Z-ADD     DATCON        ARTDTR
543300020717     C                   End
543400970304     C                   END
543500970304     C* Se selezionato imposto 5 in artfl1
543600970304     C     SFHSEL        IFEQ      '1'
543700970304     C                   MOVEL     '5'           ARTFL1
543800970305     C                   MOVEL     ' '           ARTFTR
543900970311     C                   END
544000970311     C*
544100970304     C                   UPDATE    FNART000
544200970304     C* Sistemo eventuali anomalie del collo consegnato
544300970304?    C                   EXSR      GESANM
544400970304     C                   END
544500970311     C*
544600970311     C                   END
544700970304     C* Leggo record subfile successivo
544800970304     C                   ADD       1             NRR7
544900970304     C     NRR7          CHAIN     LR53S07                            95
545000970304     C                   END
545100970311     C*
545200970311     C*  Chain tab. eventi
545300970311     C                   Z-ADD     1             X
545400970311     C     V2CEVD        LOOKUP    C2A(X)                                 95
545500970311     C                   MOVEL     D2A(X)        DS2A
545600070406     C                   MOVEL     D2z(X)        DS2z
545700970311     C*
545800970311     C* Se c'è 'S' in tentata consegna vado addizionare 1
545900970311     C     §2ANTC        IFEQ      'S'
546000970311     C                   ADD       1             ARBNTC
546100970311     C                   END
546200970311     C* Se evento da memorizzare lo memorizzo
546300970311     C     §2AEVB        IFEQ      'S'
546400970311     C                   MOVEL     *BLANKS       EVBCEV
546500970311     C                   MOVEL     V2CEVD        EVBCEV
546600970311?    C                   EXSR      EVENTI
546700970311     C                   END
546800970311     C* Incremento contatotre consegne parziali
546900970311     C                   ADD       1             STPTPA
547000970311     C*
547100970311     C     V2CIDD        IFEQ      'P'
547200970311     C                   MOVE      §2AFTC        ARBTMC
547300970311     C                   Z-ADD     0             ARBNGD
547400970311     C                   Z-ADD     0             ARBDCM
547500970311     C                   Z-ADD     0             ARBHMC
547600970311     C                   Z-ADD     0             ARBSTP
547700970311     C                   Z-ADD     0             ARBNSS
547800970311     C* Se prima consegna parziale aggiorno la data
547900970311     C     ARBDCP        IFEQ      0
548000970311     C                   Z-ADD     DATCON        ARBDCP
548100970311     C                   END
548200970311     C*  Se non fatta chiudo l'anomalia con causale 'CH'
548300970311     C                   EXSR      CLOANM
548400970311     C                   END
548500970304     C*
548600970304     C                   ENDSR
548700941206      *---------------------------------------------------------*
548800941206      *  Determinazione del volume da utilizzare
548900941206      *---------------------------------------------------------*
549000941206     C     CALVOL        BEGSR
549100941206     C*  determino volume da utilizzare
549200941206     C     ARBFVF        IFEQ      'T'
549300941206     C                   Z-ADD     ARBVLF        WVOL
549400941206     C                   ELSE
549500050208     C     ARBNCR        ifeq      ARBNCL
549600941206     C                   Z-ADD     ARBVLC        WVOL
549700941206     C                   ELSE
549800941206     C     ARBVLC        IFGE      ARBVLF
549900941206     C                   Z-ADD     ARBVLC        WVOL
550000941206     C                   ELSE
550100941206     C                   Z-ADD     ARBVLF        WVOL
550200941206     C                   ENDIF
550300941206     C                   ENDIF
550400941206     C                   ENDIF
550500941206     C*
550600941206     C                   ENDSR
550700070319      *---------------------------------------------------------*
550800070319      *  Determinazione se bolla da bloccare alla consegna
550900070319      *---------------------------------------------------------*
551000070319     c     insbloc       begsr
551100070319      * con C.A. aperta
551200070319     c                   clear                   fidn12ds
551300070319     c                   eval      i12aas = arbaas
551400070319     c                   eval      i12lnp = arblnp
551500070319     c                   eval      i12nrs = arbnrs
551600070319     c                   eval      i12nsp = arbnsp
551700070319     c                   eval      i12fel = 'E'
551800070319     c                   eval      i12fan = 'E'
551900070319     c                   eval      i12fch = 'E'
552000070319     c                   call      'FIDN12R'
552100070319     c                   parm                    fidn12ds
552200070319      * se c'è almeno una C.A. aperta blocco la bolla
552300070406     c                   if        o12nca > 0  or §2zsos = 'S'
552400070319     C                   move      'L'           KTRC
552500070319     c                   clear                   dsbl4l
552600070319     C     Kar4          chain     fiar401l                           94
552700070319     c  n94              movel(p)  ar4not        dsbl4l
552800070319     c                   move      'S'           §b4lbge
552900070319     c                   movel     dsbl4l        ar4not
553000070319     C                   if        *in94
553100070319     C                   move      'L'           ar4trc
553200070319     C                   z-add     klnp          ar4LNP
553300070319     C                   z-add     kaas          ar4AAS
553400070319     C                   z-add     knrs          ar4NRS
553500070319     C                   z-add     knsp          ar4NSP
553600070319     C                   write     fiar4000
553700070319     C                   else
553800070319     C                   update    fiar4000
553900070319     C                   end
554000070319     C*
554100070319     c                   end
554200070319     c
554300070319     C                   endsr
554400070319      *---------------------------------------------------------*
554500070319      *  elimina blocchi alla consegna da bolla data x consegnata
554600070319      *---------------------------------------------------------*
554700070319     c     elibloc       begsr
554800070321     C                   clear                   dsbl4l
554900070319     C                   move      'L'           KTRC
555000070320     C     Kar4          chain     fiar401l
555100070320     C                   if        %found(fiar401l)
555200070321     C                   movel     ar4not        dsbl4l
555300070321     c                   move      *blank        §b4lbas
555400070321     c                   move      *blank        §b4lbge
555500080131     c*                  move      *blank        §b4ltca
555600070321     c                   move      *blank        §b4ltfd
555700070321     C                   movel     dsbl4l        ar4not
555800070321     c                   update    fiar4000
555900070320     C                   endif
556000070319     c
556100070319     C                   endsr
556200130722      *---------------------------------------------------------*
556300130722      *  Verifica se presente record con coordinate GEO
556400130722      *---------------------------------------------------------*
556500130722     c     VerCorAR5     begsr
556600130723     C                   clear                   dar5cog
556700130722     C                   move      'COG'         trek5
556800130722      * per non consegna oppure consegna senza PDA verifica presenza record AR5-COG
556900130722      * se esiste e ancora da trasmettere a sede cancella
557000130722      * altrimenti aggiorna con tutti blank nelle coordinate
557100130722     C     Kar5          chain     fiar501l
557200130722     C                   if        %found(fiar501l)
557300130722     C                   if        ar5ft2 = *blank
557400130722     c                   delete    fiar5000
557500130722     c                   else
557600140404     C                   movel     ar5uni        dar5cog
557700140404     c                   move      *blank        ar5ft2
557800140404     c                   z-add     0             ar5dt2
557900140404     c                   move      *blank        §ar5xlo
558000140404     c                   move      *blank        §ar5yla
558100140404     C                   movel(p)  dar5cog       ar5uni
558200130722     c                   update    fiar5000
558300130722     c                   endif
558400130722     C                   endif
558500130722     c
558600130722     C                   endsr
558700130722      *---------------------------------------------------------*
558800140609      * verifico flag consegna documento
558900130722      *---------------------------------------------------------*
559000140609     c     srdocum       begsr
559100140609     c                   if        §pctritdoc = 'N'
559200140609     c     kpdc          chain     fipdc03t
559300140609     c                   if        %found(fipdc03t)
559400141021      *
559500141021     c                   if        pdcdocum = *blank
559600141021     c                   clear                   §pctritdoc
559700141021     c                   leavesr
559800141021     c                   endif
559900141021      *
560000140609     c                   movel(p)  pdcdocum      kke1
560100140609     c                   eval      KCOD1 =  'DOC'
560200140609     c     ktbe          chain     tntbe01l
560300140609     c                   if        %found(tntbe01l)
560400140610     c                   movel     tbeuni        ddoc
560500140609     c* se fotocopia c/ass o fattura non scrivo RA in caso di non consegna
560600140609     c* documento
560700140610     c                   if        comflg = ' S  S ' or
560800140610     c                             comflg = '    S ' or
560900140609     c                             comflg = ' S    '
561000140609     c                   clear                   §pctritdoc
561100140609     c                   end
561200140609     c                   end
561300140609     c                   end
561400140609     c                   end
561500140609     C                   endsr
561600140609      *---------------------------------------------------------*
561700140609      *  aggiorna scrive record con coordinate GEO
561800140609      *---------------------------------------------------------*
561900140609     c     AggCorAR5     begsr
562000130722     C                   clear                   dar5cog
562100130722     C                   move      'COG'         trek5
562200130722      * per non consegna oppure consegna senza PDA verifica presenza record AR5-COG
562300130722      * se esiste e ancora da trasmettere a sede cancella
562400130722      * altrimenti aggiorna con tutti blank nelle coordinate
562500130722     C     Kar5          chain     fiar501l
562600130722     C                   if        %found(fiar501l)
562700140404     C                   move      ar5uni        dar5cog
562800140404     c                   clear                   ar5ft2
562900140404     c                   clear                   ar5dt2
563000140415     c                   if        §pctxlo <> *blank and §pctxlo <> '00000000'
563100140404     c                   move      §PCTXLO       §ar5XLO
563200140404     c                   move      §PCTYLA       §ar5YLA
563300140415     c                   else
563400140415     c                   clear                   §ar5xlo
563500140415     c                   clear                   §ar5yla
563600140415     c                   endif
563700140404     c                   move      §PCTritdoc    §ar5docd
563800140404     C                   movel(p)  dar5cog       ar5uni
563900140404     c                   update    fiar5000
564000140415      *
564100130722     c                   else
564200140415      *
564300140415      *se non ci sono ne coordinate ne flag documento esce non scrive il fiar5
564400140415     c                   if        §pctritdoc = *blank and
564500140415     c                             (§pctxlo = *blank or §pctxlo = '00000000')
564600140415     c                   leavesr
564700140415     c                   else
564800140404     c                   clear                   dar5cog
564900130722     c                   clear                   ar5prg
565000140415      *se coordinate = 0 le abblenca
565100140415     c                   if        §pctxlo = '00000000'
565200140415     c                   clear                   §ar5xlo
565300140415     c                   clear                   §ar5yla
565400140415     c                   else
565500130723     c                   move      §PCTXLO       §ar5XLO
565600130723     c                   move      §PCTYLA       §ar5YLA
565700140415     c                   endif
565800140225     c                   move      §PCTritdoc    §ar5docd
565900130723     C                   move      'COG'         ar5trd
566000130730     C                   movel(p)  dar5cog       ar5uni
566100130722     c                   eval      ar5ft1 = 'T'
566200130722     c                   eval      ar5dt1 = 99999999
566300130722     c                   eval      ar5ft3 = 'T'
566400130722     c                   eval      ar5dt3 = 99999999
566500130722     c                   clear                   ar5ft2
566600130722     c                   clear                   ar5dt2
566700130730     c                   move      oggi          ar5dac
566800130722     C                   TIME                    ar5ORc
566900130722     c                   eval      ar5aas = V2CAAS
567000130722     c                   eval      ar5lnp = V2CLNP
567100130722     c                   eval      ar5nrs = V2CNRS
567200130722     c                   eval      ar5nsp = V2CNSP
567300130722     c                   write     fiar5000
567400140415     C                   endif
567500130722     C                   endif
567600140404     c* in uscita dalla routine verifico se il flag per la consegna documenti è = 'N'
567700140404      * richiamo routne per scrivere richiesta assistenza
567800140404     c                   if        §ar5docd  = 'N'
567900140404     c                   clear                   fidna6ds
568000140404     c                   eval      ida06mad = ' 88'
568100140404     c                   eval      ida06tor = 'S'
568200140404     c                   eval      ida06ogg = %editc(ar5lnp:'X') +
568300140404     c                             %editc(ar5nrs:'X') + %editc(ar5nsp:'X') +
568400140404     c                             %editc(ar5aas:'X')
568500140404     c                   eval      ida06ute = uteute
568600140404     c                   eval      ida06fil = utefil
568700140404     c                   eval      ida06cdt = '999'
568800140404     c                   eval      ida06no2 = 'AUT: ' + %editc(vh2pdr:'X') +
568900140407     c                             ' DIST: ' + %editc(vnmdoc:'X')
569000140410     c                   eval      ida06rsc = 'Automatica da PDA'
569100150610     c                   eval      IDA06PRT = '0'
569200150610     c                   eval      IDA06RPR = 'RAC'
569300150610     c                   eval      IDA06RPG = 'RAC'
569400140404     c                   call      'FIDNA6R'
569500140404     c                   parm                    fidna6ds
569600140404     c                   endif
569700130722     c
569800130722     C                   endsr
569900140306      *---------------------------------------------------------*
570000140306      *  aggiorna record GEN di fiar5 con spedizione su pda
570100140306      *---------------------------------------------------------*
570200140306     c     AggGenAR5     begsr
570300140306     C                   move      'GEN'         trek5
570400140306      * imposta campo per dichiarare se spedizione esitata con o senza pda
570500140306     C     Kar5          chain     fiar501l
570600140306     C                   if        %found(fiar501l)
570700140324     C                   movel     ar5uni        dar5gen
570800140326     c                   move      'NO'          wAR5PDACO
570900140805     c                   if        wpda = 'S'
571000140326     c                   move      *blank        WAR5PDACO
571100140306     c                   endif
571200140326      * aggiorno solo se variato
571300140326     c                   if        war5pdaco <> §ar5pdaco
571400140326     c                   move      war5pdaco     §ar5pdaco
571500140306     C                   movel(p)  dar5gen       ar5uni
571600140324     c                   clear                   ar5ft2
571700140324     c                   clear                   ar5dt2
571800140306     c                   update    fiar5000
571900140326     C                   endif
572000140306     C                   endif
572100140306     c
572200140306     C                   endsr
572300110207      *---------------------------------------------------------*
572400110207      *  scrive file spia per immagini ldv
572500110207      *---------------------------------------------------------*
572600110207     c     scrivispia    begsr
572700141202      **
572800141202ab   c                   exsr      Found_CET
572900141202ab    ***
573000141202ab   c                   if        ecco_il_CET = 'S'
573100110328     c*                  if        §PCTLDVFIR = *blank and v2sda = 'L'
573200110207     C                   clear                   fiilv000
573300110221     C                   z-add     d53fgs        ilvfgs
573400110208     C                   z-add     vnmdoc        ilvndc
573500110208     C                   z-add     v2caas        ilvaas
573600110208     C                   z-add     v2clnp        ilvlnp
573700110208     C                   z-add     v2cnrs        ilvnrs
573800110208     C                   z-add     v2cnsp        ilvnsp
573900110214     C*                  time                    ilvtimd
574000110214     C                   TIME                    Wilvtimd         14 0
574100110214     C                   MOVEl     Wilvtimd      wilvtimh          6 0
574200110214     C                   MOVE      Wilvtimh      ilvtimd
574300110214     C                   MOVE      Wilvtimd      G02DAT
574400110214     C                   MOVEL     *BLANK        G02ERR
574500110214     C                   CALL      'XSRDA8'
574600110214     C                   PARM                    WLBDA8
574700110214     C                   movel     G02INV        ilvtimd
574800110207     C                   movel     v02ris        ilvriserv
574900110207     C                   movel     v2cnt1        ilvnotaut
575000110328     C                   move      v2sda         ilvscan
575100110328     c                   move      wnocons       ilvnocdst
575200110328     C                   move      §PCTldvfir    ilvfirpda
575300110207     C                   movel     §PCTFLGSPC    ilvflgspc
575400110207     c                   move      '00'          ilvcol
575500120528     c                   eval      dilvlibero = ilvlibero
575600120528     C                   move      ORACON        §ilvhmp
575700120528     c                   eval      ilvlibero = dilvlibero
575800110207     c                   write     fiilv000
575900110328     C*                  endif
576000110207     C                   endif
576100110207     c
576200110207     C                   endsr
576300120302      *---------------------------------------------------------*
576400120302      *  richiamo scrittura file multispedizione multiassegno   *
576500120302      *---------------------------------------------------------*
576600120302     C     scrivisped    BEGSR
576700120302     c                   clear                   fncss000
576800120305     c     kspevid       chain     fncss01l
576900120518     c                   if        %found(fncss01l)
577000120518     c     nrrcss        chain     fncss00f
577100120518     c                   end
577200130118     c*                  eval      cssfgs = v2clna
577300130118     c                   if        d53fgs = 0
577400130118     c                   z-add     d53lna        cssfgs
577500130118     c                   else
577600130118     c                   z-add     d53fgs        cssfgs
577700130118     c                   end
577800120302     c                   eval      cssndc = vnmdoc
577900120305     c                   movel(p)  v3copz        cssopz
578000120305     c                   clear                   csskey
578100120302     c                   eval      cssvca = v3cvca
578200120302     c                   eval      cssaas = v3caas
578300120302     c                   eval      csslnp = v3clnp
578400120302     c                   eval      cssnrs = v3cnrs
578500120302     c                   eval      cssnsp = v3cnsp
578600120306     C                   IF        VH2LPP <> 0
578700120302     c                   eval      cssaao = vh2aap
578800120302     c                   eval      csslno = vh2lpp
578900120302     c                   eval      cssnro = vh2nrp
579000120302     c                   eval      cssnso = vh2nsp
579100120306     C                   ELSE
579200120306     c                   eval      cssaao = v3caas
579300120306     c                   eval      csslno = v3clnp
579400120306     c                   eval      cssnro = v3cnrs
579500120306     c                   eval      cssnso = v3cnsp
579600120306     C                   END
579700120302     c                   eval      csstic = v3ctic
579800120302     c                   if        v02asp = 0
579900120302     c                   eval      csscas = v3cimp
580000120302     c                   else
580100120302     c                   eval      csscas = v02asp
580200120302     c                   end
580300120302     c                   eval      cssccm = §clitm
580400120302     c                   eval      csspru = knmus
580500120518     c                   eval      cssmit = v3cmit
580600120518     c                   if        v3cmit = ' '
580700120518     c                   eval      cssmit = vh2rsm
580800120518     c                   end
580900120302     c                   time                    cssdatora
581000120518     c                   if        not %found(fncss01l)
581100120302     c                   write     fncss000
581200120518     c                   else
581300120518     c                   update    fncss000
581400120305     c                   end
581500120302     C*
581600120302     C                   ENDSR
581700120228      *---------------------------------------------------------*
581800120228      *  Richiamo pgm per gestione multiassegno multispedizione
581900120228      *---------------------------------------------------------*
582000120308     c     srinsert      begsr
582100120228     c                   clear                   exopz
582200120228     c                   clear                   tnsc30ds
582300120307     c* inserimento assegni multipli
582400120312     c                   if        d53fgs = 0
582500120312     c                   z-add     d53lna        $cssfgs
582600120312     c                   else
582700120312     c                   z-add     d53fgs        $cssfgs
582800120312     c                   end
582900120615     c                   z-add     ndc(i)        NDCI              7 0
583000120312     c     kdst41        setll     fncss04l
583100120228     c                   do        *hival
583200120312     c     kdst4         reade     fncss04l
583300120312     c                   if        %eof(fncss04l)
583400120228     c                   leave
583500120228     c                   endif
583600120312     c                   if        $cssopz <> exopz  and
583700120312     c                             $csskey = ' '
583800120307     c                   eval      sc30cmti = 'F'
583900120312     c                   eval      sc30FGSI = $cssfgs
584000120312     c                   eval      sc30NDCI = $cssndc
584100120312     c                   eval      sc30OPZI = $cssopz
584200120305     c                   eval      SC30TPCI = 'I'
584300120228     c                   eval      kpjbu  = tnsc30ds
584400120228     c                   call      'TNSC30R'
584500120305     c                   parm                    kpjba
584600120305     c                   eval      tnsc30ds = kpjbu
584700120307     c* per errore o nessun aggiornamento (f12) cancello i record
584800120307     c* per ricominciare
584900120305     c                   if        sc30msgo <> ' '
585000120306     c                             or sc30erro = 'L'
585100120308     c                   clear                   tnsc30ds
585200120308     c                   eval      sc30cmti = 'F'
585300120312     c                   eval      sc30AASI = $cssaao
585400120312     c                   eval      sc30LNPI = $csslno
585500120312     c                   eval      sc30NRSI = $cssnro
585600120312     c                   eval      sc30NSPI = $cssnso
585700120312     c                   eval      sc30AANI = $cssaas
585800120312     c                   eval      sc30LPNI = $csslnp
585900120312     c                   eval      sc30NRNI = $cssnrs
586000120312     c                   eval      sc30NSNI = $cssnsp
586100120308     c                   eval      SC30TPCI = 'A'
586200120308     c                   eval      kpjbu  = tnsc30ds
586300120308     c                   call      'TNSC30R'
586400120308     c                   parm                    kpjba
586500120308     c                   eval      kpjbu = tnsc30ds
586600120308     c                   if        sc30msgo <> ' '
586700120308     C                   MOVEL     sc30msgo      $MSG
586800120308     c                   else
586900120307     C                   eval      $MSG = 'Aggiornamento non effettuato'
587000120306     c                   end
587100120308     C                   SETON                                        40  28
587200120308     c                   leavesr
587300120308     c                   end
587400120308     c* se ok
587500120312     c                   eval      exopz = $cssopz
587600120308     c                   endif
587700120313     c* aggiorno il record del sfl della spedizione inserita
587800120312     c                   if        $cssopz = exopz
587900120312     c                   eval      comaas = $cssaas
588000120312     c                   eval      comlnp = $csslnp
588100120312     c                   eval      comnrs = $cssnrs
588200120312     c                   eval      comnsp = $cssnsp
588300120308     c                   clear                   comkey
588400120308     c                   exsr      sraggsfl
588500120308     c                   end
588600120228     c                   enddo
588700120228     C                   endsr
588800120308      *---------------------------------------------------------*
588900120308      *  Richiamo pgm per gestione multiassegno multispedizione
589000120308      *---------------------------------------------------------*
589100120308     c     aggiosped     begsr
589200120308     c* richiamo il pgm per modifica se presente ancora la sped.
589300120308     c     kspevid       chain     fncss01l
589400120308     c                   if        %found(fncss01l) and
589500120308     c                             £csskey <> ' '
589600120308     c                   clear                   tnsc30ds
589700120308     c                   eval      sc30cmti = 'F'
589800120308     C                   IF        VH2LPP = 0
589900120308     c                   eval      sc30AASI = v3caas
590000120308     c                   eval      sc30LNPI = V3CLNP
590100120308     c                   eval      sc30NRSI = V3CNRS
590200120308     c                   eval      sc30NSPI = V3CNSP
590300120308     c                   else
590400120308     c                   eval      sc30AASI = VH2AAP
590500120308     c                   eval      sc30LNPI = VH2LPP
590600120308     c                   eval      sc30NRSI = VH2NRP
590700120308     c                   eval      sc30NSPI = VH2NSP
590800120308     c                   end
590900120308     c                   eval      sc30AANI = v3caas
591000120308     c                   eval      sc30LPNI = V3CLNP
591100120308     c                   eval      sc30NRNI = V3CNRS
591200120308     c                   eval      sc30NSNI = V3CNSP
591300120308     c                   eval      SC30TPCI = 'M'
591400120308     c                   eval      kpjbu  = tnsc30ds
591500120308     c                   call      'TNSC30R'
591600120308     c                   parm                    kpjba
591700120308     c                   eval      tnsc30ds = kpjbu
591800120308     c* errore nell'aggiornamento
591900120308     c                   if        sc30msgo <> ' '
592000120308     C                   SETON                                        40  28
592100120308     C                   MOVEL     sc30msgo      $MSG
592200120308     c                   leavesr
592300120308     c                   end
592400120313     c* se annullato aggiorno tutti i record del sfl con lo stesso legame
592500120313     c                   if        sc30erro = 'Q'
592600120308     c                   eval      comkey = £csskey
592700120308     c                   clear                   comaas
592800120308     c                   clear                   comlnp
592900120308     c                   eval      comnrs = *hival
593000120308     c                   clear                   comnsp
593100120308     c                   clear                   v02as1
593200120308     C                   SETON                                        28
593300120308     c                   clear                   $msg
593400120308     c                   exsr      sraggsfl
593500120313     c                   end
593600120308     c                   end
593700120308     C                   endsr
593800120308      *---------------------------------------------------------*
593900120308      *  Richiamo pgm per gestione multiassegno multispedizione
594000120308      *---------------------------------------------------------*
594100120308     c     sraggsfl      begsr
594200120308     c* aggiorno il sfl e richiamo eventuali manutenzioni
594300120308     c                   do        *hival        nr                4 0
594400120308     C     NR            CHAIN     LR53S03                            96
594500120308     c                   if        *in96
594600120308     c                   leave
594700120308     c                   end
594800120313     c* solo c/assegni
594900120313     c                   if        v3dtpi <> 'C/A'
595000120313     c                   iter
595100120313     c                   end
595200120308     c* aggiorno solo se la stessa sped.
595300120308     c                   if        (v3caas <>comaas and comaas <> 0) or
595400120308     c                             (v3clnp <>comlnp and comlnp <> 0) or
595500120308     c                             (v3cnrs <>comnrs and comnrs <> *hival) or
595600120308     c                             (v3cnsp <>comnsp and comnsp <> 0)
595700120308     c                   iter
595800120308     c                   end
595900120308     c* aggiorno solo se stesso legame
596000120308     c                   if        v3hkey  <> comkey and comkey <> *hival
596100120308     c                   iter
596200120308     c                   end
596300120313     c* aggiorno tutto il sfl dove ci sono legami perchè è stato annullato
596400120313     c* tutto con f12
596500120313     c                   if        comkey = *hival and comlnp = 0 and
596600120313     c                             v3hkey = ' '
596700120313     c                   iter
596800120313     c                   end
596900120313     c*
597000120308     C                   SETOFF                                       404142
597100120308     C                   SETOFF                                       434445
597200120308     C                   SETOFF                                       820503
597300120308     C                   SETOFF                                       4951
597400120308     c                   eval      v3hkey = sc30keyo
597500120308     c* aggiornamento eseguito
597600120308     c                   if        v3hkey <> ' '
597700120308     c                   eval      v02as1 ='9999'
597800120308     c                   else
597900120308     c                   clear                   v02as1
598000120308     c                   end
598100120308     c                   clear                   v3copz
598200120308     c     v3hkey        comp      ' '                                8787
598300120308     c                   seton                                        22
598400120308     c                   update    lr53s03
598500120308     c                   setoff                                       22
598600120308     c                   enddo
598700120308     C                   endsr
598800110207     C**************************************************************************
598900110207     C*  Routine per gestione richiamo interrogazione bolle
599000110207     C**************************************************************************
599100110207     C     richbl        begsr
599200110207      *
599300110207     c                   movel     kpjbu         §kpjbu          256
599400110207      *
599500110207     c                   z-add     v2caas        p36aas
599600110207     c                   z-add     v2clnp        p36lnp
599700110207     c                   z-add     v2cnrs        p36nrs
599800110207     c                   z-add     v2cnsp        p36nsp
599900110207     c                   movel     ' '           p36f03
600000110207     c                   movel     '1'           p36flg
600100110207     c                   movel     dslr36        kpjbu
600200110207     c                   call      'FNLR36R'
600300110207     c                   parm                    kpjba
600400110207     c                   movel     kpjbu         dslr36
600500110207      *
600600110207     c                   movel     §kpjbu        kpjbu
600700110207      *
600800110207     c                   endsr
600900021107      *---------------------------------------------------------*
601000021107      *  Determinazione del peso da utilizzare
601100021107      *---------------------------------------------------------*
601200021107     C     calPES        BEGSR
601300021107      *
601400021107      * se è considerata un'unica spedizione
601500050208     C     arbncp        ifeq      arbncl
601600021107     C                   Z-ADD     arbPKC        wpes
601700021107     C*
601800021107     C                   ELSE
601900021107     C*
602000021107     C     arbPKC        ifge      arbPKF
602100021107     C                   Z-ADD     arbPKC        wpes
602200021107     C                   else
602300021107     C                   Z-ADD     arbPKF        wpes
602400021107     C                   end
602500021107     C*
602600021107     C                   ENDIF
602700021107     C*
602800021107     C                   ENDSR
602900941122      *---------------------------------------------------------*
603000941122      *  Consegna
603100941122      *---------------------------------------------------------*
603200941123     C     CONDAT        BEGSR
603300941122     C*
603400020829     C                   Z-ADD     ARBifp        COMifp
603500020829     C                   Z-ADD     ARBNDC        COMNDC
603600031209     C                   Z-ADD     ARBpdc        COMpdc
603700941122     C                   Z-ADD     ARBDCM        COMDCM
603800990311     C                   Z-ADD     ARBDCP        COMDCP
603900941122     C                   Z-ADD     ARBHMC        COMHMC
604000941122     C*  Aggiorno dati consegna
604100951025     C                   Z-ADD     VH2DDT        COMDDC
604200101028     C*mm                MOVE      V2CCP1        ARBTC1
604300101028     C*mm                MOVE      V2CCP2        ARBTC2
604400970805     C     ARBCCA        IFNE      '5'
604500941122     C                   MOVE      V02CCA        ARBCCA
604600970805     C                   END
604700941123     C                   ADD       V02NCL        STPTCL
604800941123     C                   ADD       V02PKG        STPTPK
604900941123     C                   ADD       V02VOL        STPTVL
605000941122     C*  Gestisco codice riserva
605100131120     c                   clear                   scriviCEP
605200941122     C     V02RIS        IFNE      *BLANK
605300941122     C                   Z-ADD     1             X
605400941122     C     V02RIS        LOOKUP    C2A(X)                                 15
605500941122     C                   MOVEL     D2A(X)        DS2A
605600070406     C                   MOVEL     D2z(X)        DS2z
605700941122     C* Se prevista memorizzazione evento procedo di conseguenza
605800941122     C     §2AEVB        IFEQ      'S'
605900941122     C                   MOVEL     V02RIS        EVBCEV
606000941122?    C                   EXSR      EVENTI
606100941122     C                   END
606200131121     c                   else
606300131121      *ricerca forzata evento CEP
606400131121     c                   exsr      leggiCep
606500941122     C                   END
606600131108     c* verifica se ricevuto doppio evento CET/CEP
606700131120     c                   if        scriviCEP = 'S'
606800131108     c                   exsr      eventicep
606900131120     c                   endif
607000941129     C*  Firmatario
607100941129     C     V2CFIR        IFNE      *BLANKS
607200941129     C                   EXSR      AGGFIR
607300941129     C                   END
607400941129     C* Se non c'è data arrivo forzo data consegna
607500941129     C     ARBDAM        IFEQ      *ZEROS
607600970617     C     V02CCA        ANDNE     '5'
607700941129     C                   Z-ADD     DATCON        ARBDAM
607800950329     C                   END
607900941129     C     ARBDTI        IFEQ      *ZEROS
608000950329     C     WMDDTI        ANDEQ     'S'
608100970620     C     V02CCA        ANDNE     '5'
608200941129     C                   Z-ADD     DATCON        ARBDTI
608300950329     C                   Z-ADD     0202          ARBHTI
608400970620     C* RICHIAMO PGM PER RIELABORARE LA STATISTICA ARRIVI
608500970620     C                   CLEAR                   SA2TLA
608600970620     C                   MOVEL     'S'           WRISTA            1
608700970620     C                   MOVEL     *BLANKS       SA2TLA            1
608800970620     C                   MOVEL     'A'           SA2TRC            1
608900970620     C                   Z-ADD     DATCON        SA2DTS            8 0
609000970620     C                   CLEAR                   SA2LNA
609100970620     C                   CALL      'FNLSA2R3'
609200970620     C                   PARM                    SA2TLA            1
609300970620     C                   PARM                    SA2TRC            1
609400970620     C                   PARM                    SA2DTS            8 0
609500970620     C                   PARM                    SA2LNA            3 0
609600941129     C                   END
609700970620     C*
609800950331     C     WFBS          IFEQ      'S'
609900950410     C     VH2LPP        IFEQ      0
610000950410     C     VH2AAP        ORNE      HARAAS
610100950410     C     VH2LPP        ORNE      HARLNP
610200950410     C     VH2NRP        ORNE      HARNRS
610300950410     C     VH2NSP        ORNE      HARNSP
610400040505     C***                MOVEL     'S'           WRICH1
610500970924     C     ARBFBS        IFEQ      ' '
610600950331     C                   MOVEL     'S'           ARBFBS
610700970924     C                   ELSE
610800970924     C     ARBFBS        IFEQ      'V'
610900040505     C     ARBFBS        oreq      'P'
611000970924     C                   MOVEL     'E'           ARBFBS
611100970924     C                   END
611200970924     C                   END
611300950331     C                   END
611400950410     C                   END
611500941122     C*
611600941122     C                   ENDSR
611700941122      *---------------------------------------------------------*
611800941122      *  Aggiorno dati di incasso
611900941122      *---------------------------------------------------------*
612000941122     C     AGGINC        BEGSR
612100941122     C*
612200941122     C                   Z-ADD     V2CNRA        NRR3
612300941122     C                   MOVE      *BLANKS       WSVTIC
612400941122     C                   MOVE      *ZERO         WSVAS1
612500941122     C                   MOVE      *ZERO         WSVAB1
612600941122     C                   MOVE      *ZERO         WSVCA1
612700941122     C                   MOVE      *ZERO         WSVDT1
612800941122     C*
612900941122     C     NRR3          CHAIN     LR53S03                            95
613000941123     C     *IN95         DOWEQ     '0'                                          --- 01 ---
613100941122     C     NRR3          ANDGE     V2CNRD
613200000421     C* totale
613300991008     C     V3DTPI        IFNE      'C/A'                                        --- 02 ---
613400991007     C     V3DTPI        ANDNE     'Ass'                                        --- 02 ---
613500941213     C     V3CTIC        IFNE      *BLANKS
613600000421     C* ... in fase di aggiornamento dati il programma legge x
613700000421     C* ogni spedizione il subfile degli importi alla rovescio
613800000421     C* (parte cioè dalla riga di totale) quando leggo questa
613900000421     C* mi salvo i dati di incasso se specificati in appositi
614000000421     C* campi di work
614100941122     C                   MOVE      V3CTIC        WSVTIC
614200941122     C                   MOVE      V02AS1        WSVAS1
614300941122     C                   MOVE      V02AB1        WSVAB1
614400941122     C                   MOVE      V02CA1        WSVCA1
614500941122     C                   MOVE      V02DT1        WSVDT1
614600941122     C*  Scrittura record importi
614700941122     C                   EXSR      WRTIMP
614800941213     C                   END
614900991008     C*  Scrittura record importi parziali x totali
615000991011     C                   EXSR      AZZARI
615100941122     C*
615200941123     C                   ELSE                                                   --- 02 ---
615300000421     C* ... sto gestendo l'assegnato o il C/Asseegno
615400941122     C     V3CTIC        IFEQ      *BLANKS
615500000421     C* se non ho specificato su questa riga i dati di incasso
615600000421     C* controllo se li ho specificati a livello di totale
615700941122     C     WSVTIC        ANDNE     *BLANKS
615800000421     C* ... se si sostituisco i dati dell'assegno con quelli
615900000421     C* impostati nel totale per effettuare i ragionamenti
616000000421     C* successivi ma non scrivo dati in stampa ne su Cnass
616100941122     C                   MOVE      WSVTIC        V3CTIC
616200941122     C                   MOVE      WSVAS1        V02AS1
616300941122     C                   MOVE      WSVAB1        V02AB1
616400941122     C                   MOVE      WSVCA1        V02CA1
616500941122     C                   MOVE      WSVDT1        V02DT1
616600941122     C                   ELSE
616700000421     C* ... altrimenti se non c'è o non devo considerare la
616800120302     C* riga di totale scrivo dati in stampa e su Cnass x
616900000421     C* singola riga di C/Assegno o Assegnato
617000941122     C                   EXSR      WRTIMP
617100941122     C                   END
617200000421     C*
617300990924     C*  Verifico se devo aggiornare/scrivere archivio incassi
617400000421     C* (FIARI)
617500990924     C*  ... se impostato incasso parziale devo aggiornarlo
617600990924     C     V02ASP        IFNE      0
617700011108MM+  C     V02SN1        ANDEQ     ' '
617800991027     C     V02TPA        ORNE      ' '
617900000421     C*      oppure se non specificato x assegnati verifico se era
618000000421     C*      stato precedente effettuato un incasso parziale
618100991027     C     V3DTPI        OREQ      'Ass'
618200990924     C     ARBICA        ANDEQ     'R'
618300990924     C     ARBFIP        ANDEQ     'I'
618400000421     C* ... oppure se ho impostato riga totale scrivo sempre FIARI
618500991011     C     WPZTOT        OREQ      'SI'
618600991011     C     V3CTOT        ANDEQ     'SI'
618700991011     C* Se c'è riga totale memorizzo dati altrimenti vado in
618800991011     C* scrittura importo
618900991011     C                   EXSR      WRTARI
619000991011     C                   END
619100941122     C*  C/Assegno
619200941123     C     V3DTPI        IFEQ      'C/A'                                        --- 03 ---
619300941123     C     V02SN1        IFEQ      ' '                                          --- 04 ---
619400941122     C*  Aggiorno Flag
619500941122     C     ARBICC        IFEQ      ' '
619600941122     C                   MOVEL     'S'           ARBICC
619700941122     C                   ELSE
619800941122     C     ARBICC        IFEQ      'S'
619900941122     C                   MOVEL     'Y'           ARBICC
620000941122     C                   ELSE
620100941122     C     ARBICC        IFEQ      'R'
620200941122     C                   MOVEL     'I'           ARBICC
620300941122     C                   ELSE
620400941122     C     ARBICC        IFEQ      'I'
620500941122     C                   MOVEL     'Y'           ARBICC
620600941122     C                   END
620700941122     C                   END
620800941122     C                   END
620900941123     C                   END
621000120302     C* ???
621100941122     C                   MOVE      V02AB1        FTDAB1
621200941122     C                   MOVE      V02CA1        FTDCA1
621300941122     C                   MOVE      V02AS1        FTDAS1
621400941123     C                   MOVE      V3CTIC        FTDTIC
621500941122     C*
621600941122     C*  Chain dati C/Assegno
621700051115     C     KARB1         CHAIN     FIAR901L                           94
621800941122     C*  Se non esiste il C/Assegno sulla figlia lo cerco sulla mamma
621900941122     C     *IN94         IFEQ      '1'
622000941122     C                   Z-ADD     VH2AAP        KAAS2
622100941122     C                   Z-ADD     VH2LPP        KLNP2
622200941122     C                   Z-ADD     VH2NRP        KNRS2
622300941122     C                   Z-ADD     VH2NSP        KNSP2
622400051115     C     KARB2         CHAIN     FIAR901L                           95
622500941122     C                   END
622600941122     C     *IN95         IFEQ      '0'
622700941122     C                   MOVE      V3CTIC        AR9TIC
622800991013     C     V02ASP        IFEQ      0
622900991013     C                   Z-ADD     V3CIMP        FTDCAS
623000991013     C                   ELSE
623100991013     C                   Z-ADD     V02ASP        FTDCAS
623200991013     C                   END
623300991013     C                   MOVE      V3CVCA        FTDVCA
623400051115     C                   UPDATE    FIAR9000
623500941122     C                   END
623600941122     C*  Ritorno all'incasso
623700941122     C                   ELSE
623800941220     C                   MOVEL     'R'           ARBICC
623900941123     C                   END                                                    --- 04 ---
624000941123     C                   END                                                    --- 03 ---
624100941122     C*  Assegnato
624200941122     C     V3DTPI        IFEQ      'Ass'
624300941122     C     V02SN1        IFEQ      ' '
624400941122     C                   MOVE      V02AB1        FTDAB2
624500941122     C                   MOVE      V02CA1        FTDCA2
624600941122     C                   MOVE      V02AS1        FTDAS2
624700941123     C                   MOVE      V3CTIC        FTDMIF
624800941122     C*  Aggiorno Flag
624900941122     C     ARBICA        IFEQ      ' '
625000990913     C     ARBACA        ANDEQ     'S'
625100941206     C     V02TPA        IFNE      'I'
625200941122     C                   MOVEL     'S'           ARBICA
625300941122     C                   ELSE
625400941122     C                   MOVEL     'I'           ARBICA
625500941122     C                   END
625600941122     C                   ELSE
625700941122     C     ARBICA        IFEQ      'S'
625800941122     C                   MOVEL     'Y'           ARBICA
625900941122     C                   ELSE
626000941122     C     ARBICA        IFEQ      'R'
626100941122     C                   MOVEL     'I'           ARBICA
626200941122     C                   ELSE
626300941122     C     ARBICA        IFEQ      'I'
626400941122     C                   MOVEL     'Y'           ARBICA
626500941122     C                   END
626600941122     C                   END
626700941122     C                   END
626800941122     C                   END
626900990924     C* Se la divisa dell'assegnato non è uguale a Lit converto
627000011010     C     V3CVCA        IFNE      §GEDCR
627100990924     C                   CLEAR                   YEURDS
627200990924     C                   MOVEL     V3CVCA        YECDVI
627300990924     C                   Z-ADD     V3CIMP        YECIMI
627400990924     C     V02ASP        IFNE      0
627500990924     C                   Z-ADD     V02ASP        YECIMI
627600990924     C                   END
627700011010     C                   MOVEL     §GEDCR        YECDVO
627800990924     C                   MOVEL     'H'           YECTAR
627900990924     C                   CALL      'YEURCO'
628000990924     C                   PARM                    YEURDS
628100990924     C                   Z-ADD     YECIMO        FTDIFP
628200990924     C                   ELSE
628300990924     C                   Z-ADD     V3CIMP        FTDIFP
628400990924     C     V02ASP        IFNE      0
628500991027     C     V02TPA        ORNE      ' '
628600990924     C                   Z-ADD     V02ASP        FTDIFP
628700990924     C                   END
628800990924     C                   END
628900020725     c                   if        ftdifp <> 0
629000020725     c                   movel     v3cvca        ftddiv
629100020725     c                   else
629200020725     c                   movel     *blanks       ftddiv
629300020725     c                   end
629400941122     C                   MOVE      V3CTIC        ARBMIF
629500941122     C                   MOVE      V02TPA        ARBFIP
629600941122     C     V02TPA        IFEQ      *BLANKS
629700941207     C     V2CCON        ANDEQ     'SI'
629800941122     C                   END
629900941122     C*  Ritorno all'incasso
630000941122     C                   ELSE
630100941220     C                   MOVEL     'R'           ARBICA
630200941122     C                   END
630300941122     C                   END
630400941123     C                   END
630500941122     C*  Lettura successiva
630600941122     C                   SUB       1             NRR3
630700941122     C     NRR3          CHAIN     LR53S03                            95
630800941122     C                   END
630900941122     C*
631000941123     C     NRR2          CHAIN     LR53S02                            95
631100941122     C                   ENDSR
631200941122      *---------------------------------------------------------*
631300941122      *  SCrittura importi
631400941122      *---------------------------------------------------------*
631500941122     C     WRTIMP        BEGSR
631600941122     C*
631700000421     C* Stampa riga importi
631800941207     C     V02SN1        IFEQ      *BLANKS
631900941206     C                   Z-ADD     V3CIMP        STPIMP
632000120302     c     V02ASP        IFGT      0
632100991027     C     V02TPA        ORNE      ' '
632200941206     C                   Z-ADD     V02ASP        STPIMP
632300941206     C                   END
632400941123     C                   MOVE      V3CVCA        STPVAL
632500120314     C*                  MOVE      STPIMP        APPO3             3 0
632600120314     C*    APPO3         COMP      0                                      06
632700941122     C   OF              EXCEPT    TESTA
632800120302     c* singolo assegno
632900120302     c                   if        v3hkey = ' '
633000120302     C     V02AS1        COMP      *BLANKS                            04
633100120302     C                   MOVEL     V3CTIC        STPTIC            2
633200120302     C                   MOVE      V02AS1        STPNAS            4
633300120302     C                   MOVE      V02AB1        STPABI
633400120302     C                   MOVE      V02CA1        STPCAB
633500941122     C                   EXCEPT    DETIMP
633600120302     c                   else
633700120302     c* multiassegni
633800120302     c     v3hkey        setll     tncsm03l
633900120302     c                   do        *hival
634000120302     c     v3hkey        reade     tncsm03l
634100120302     c                   if        %eof(tncsm03l)
634200120302     c                   leave
634300120302     c                   end
634400120302     c                   seton                                        04
634500120313     c                   movel     csmdiv        stpval
634600120313     c                   z-add     csmidc        stpimp
634700120313     C                   MOVEl     CSmNRA        stpnas
634800120302     C                   MOVE      CSmABI        stpABI
634900120302     C                   MOVE      CSmCAB        stpCAB
635000120302     C                   MOVEL     CSmTPA        stptic
635100120302     C                   MOVE      CSmTPI        stptic
635200120302     C   OF              EXCEPT    TESTA
635300120302     C                   EXCEPT    DETIMP
635400120302     C                   ENDdo
635500120302     C                   END
635600120302     C                   END
635700941122     C*
635800941212     C*  Eventuale scrittura CNASS
635900941212     C                   Z-ADD     1             X
636000941212     C     V3CTIC        LOOKUP    C1A(X)                                 94
636100941212     C     *IN94         IFEQ      '1'
636200941212     C                   MOVEL     D1A(X)        DS1A
636300951113     C     V02SN1        IFEQ      ' '
636400941212     C     §1AFCA        ANDNE     'S'
636500060331     C     §1AFCA        ANDNE     'P'
636600020827     C                   Z-ADD     dutpou        ASSFLE
636700941212     C     VH2AAP        IFEQ      *ZEROS
636800951113     C     V3DTPI        OREQ      'Ass'
636900941212     C                   Z-ADD     V3CLNP        ASSLNP
637000941212     C                   Z-ADD     V3CAAS        ASSAAS
637100941212     C                   Z-ADD     V3CNRS        ASSNRS
637200941212     C                   Z-ADD     V3CNSP        ASSNSP
637300941212     C                   ELSE
637400941212     C                   Z-ADD     VH2LPP        ASSLNP
637500941212     C                   Z-ADD     VH2AAP        ASSAAS
637600941212     C                   Z-ADD     VH2NRP        ASSNRS
637700941212     C                   Z-ADD     VH2NSP        ASSNSP
637800941212     C                   END
637900941212     C*
638000941212     C                   MOVE      §1AFCA        ASSTPA
638100941212     C     §1AFMB        IFEQ      'M'
638200941212     C                   MOVE      'M'           ASSTPI
638300941212     C                   ELSE
638400941212     C                   MOVE      ' '           ASSTPI
638500941212     C                   END
638600941212     C*
638700941212     C                   MOVEL     *BLANKS       ASSTPP
638800941212     C                   MOVEL     *BLANKS       ASSPIA
638900941212     C                   MOVEL     *BLANKS       ASSNOT
639000941212     C                   Z-ADD     *ZEROS        ASSKCC
639100941212     C                   Z-ADD     *ZEROS        ASSKSC
639200941212     C                   Z-ADD     *ZEROS        ASSNDV
639300941212     C                   Z-ADD     *ZEROS        ASSDDV
639400941212     C                   MOVE      *BLANK        ASSFTR
639500941212     C                   MOVEL     V3DTPI        ASSTPD
639600991122     C     ASSTPD        IFNE      'A'
639700991122     C     ASSTPD        ANDNE     'C'
639800991122     C                   MOVEL     'T'           ASSTPD
639900991122     C                   END
640000960730     C                   Z-ADD     V3CIMP        ASSCAS
640100941212     C     V02ASP        IFGT      0
640200990924     C                   Z-ADD     V02ASP        ASSCAS
640300941212     C                   END
640400950223     C                   MOVEL     V3CVCA        ASSVCA
640500941212     C     V3CVCA        IFNE      *BLANKS
640600011010      *FORZATO A 0 X MODIFICHE EURO
640700011010     C                   Z-ADD     0             ASSCMB
640800941212     C                   END
640900120302     C* SINGOLO ASSEGNO
641000120302     C                   IF        V3HKEY = ' '
641100960725     C*  Se non devo impostare nr.assegno scrivo 333333333
641200960725     C     §1ANAS        IFEQ      'N'
641300960725     C                   MOVEL     '3333'        ASSNRA
641400960725     C                   Z-ADD     OGGI          ASSDTE
641500960725     C                   ELSE
641600960725     C                   MOVEL     *BLANKS       ASSNRA
641700960725     C                   MOVEL     V02AS1        ASSNRA
641800941222     C                   MOVE      V02DT1        MM2
641900970113     C                   MOVE      DTD(I)        ANNO1
642000000421     C* Assegno Postdatato
642100951116     C     V3CTIC        IFEQ      'TM'
642200060626     C     V3CTIC        orEQ      'TO'
642300150526     C     v3ctic        oreq      'TP'
642400151005      *
642500000421     C* slitto avanti fino a mesi
642600151005     C**** MM2           IFEQ      1
642700151005     C**** MM1           ANDEQ     10
642800151005     C**** MM2           ORLE      2
642900151005     C**** MM1           ANDEQ     11
643000151005     C**** MM2           ORLE      3
643100151005     C**** MM1           ANDEQ     12
643200151005      *
643300151005     C* ... se sono in ottobre al massimo posso accettare
643400151005     C* assegni postdatati a gennaio (ott. + 3 mesi)
643500151005     C     MM2           IFle      3
643600151005     C     MM1           ANDEQ     10
643700151005     C* ... se sono in novembre posso accettare assegni
643800151005     C* postdatati a gennaio o febbraio (nov. + 3 mesi)
643900151005     C     MM2           ORLE      5
644000151005     C     MM1           ANDEQ     11
644100151005     C* ... se sono in dicembre posso accettare assegni
644200151005     C* postdatati a gennaio, febbraio o marzo (dic. + 3 mesi)
644300151005     C     MM2           ORLE      6
644400151005     C     MM1           ANDEQ     12
644500951116     C                   ADD       1             ANNO1
644600951116     C                   END
644700151005      *
644800951116     C                   ELSE
644900000421     C* .. se no postdatato slitto di un mese avanti
645000951205     C     MM1           IFEQ      12
645100951205     C     MM2           ANDEQ     1
645200951205     C                   ADD       1             ANNO1
645300951205     C                   END
645400000421     C* .. o indietro
645500941222     C     MM1           IFEQ      1
645600941222     C     MM2           ANDEQ     12
645700950102     C                   SUB       1             ANNO1
645800941222     C                   END
645900951116     C                   END
646000151005      **
646100941212     C                   MOVEL     V02DT1        G02DAT
646200950102     C                   MOVE      ANNO1         G02DAT
646300941212     C                   MOVEL     ' '           G02ERR
646400941212     C                   CALL      'XSRDA8'
646500941212     C                   PARM                    WLBDA8
646600941212     C                   MOVE      G02INV        ASSDTE
646700960725     C                   END
646800960725     C*  Scrivo 33333 - 33333 x abi cab se non obbligatorio
646900960725     C     §1AABI        IFEQ      'N'
647000960725     C                   Z-ADD     1000          ASSABI
647100960725     C                   Z-ADD     1000          ASSCAB
647200960725     C                   ELSE
647300941212     C                   Z-ADD     V02AB1        ASSABI
647400941212     C                   Z-ADD     V02CA1        ASSCAB
647500960725     C                   END
647600120302     C* MULTIASSEGNI
647700120302     c                   else
647800120302     C                   CLEAR                   assdte
647900120302     C                   CLEAR                   assabi
648000120302     C                   CLEAR                   asscab
648100120302     c                   eval      assnra = v3hkey
648200120302     C                   END
648300941212     C                   WRITE     CNASS000
648400941212     C                   END
648500941212     C                   END
648600941212     C*
648700941122     C                   ENDSR
648800991011      *---------------------------------------------------------*
648900991011      *  Scrittura file incassi
649000991011      *---------------------------------------------------------*
649100991011     C     WRTARI        BEGSR
649200991011     C*
649300991011     C* Se sto gestendo un Assegnato che ha già avuto degli incassi
649400991011     C* parziali eseguo chain + update
649500991011     C     V3DTPI        IFEQ      'Ass'
649600991011     C* Se c'è importo totale incassato in un altra valuta converto
649700000421     C* importo assegnato nella valuta della riga di totale
649800991018     C     V3CTOT        IFEQ      'SI'
649900991011     C     WPZTOT        ANDEQ     'SI'
650000991011     C     V3CVCA        ANDNE     WVTA
650100991011     C                   CLEAR                   YEURDS
650200991011     C     V02ASP        IFEQ      0
650300991011     C                   Z-ADD     V3CIMP        YECIMI
650400991011     C                   ELSE
650500991011     C                   Z-ADD     V02ASP        YECIMI
650600991011     C                   END
650700991011     C                   MOVEL     V3CVCA        YECDVI
650800991011     C                   MOVEL     WVTA          YECDVO
650900991011     C                   MOVEL     'H'           YECTAR
651000991011     C                   CALL      'YEURCO'
651100991011     C                   PARM                    YEURDS
651200991011     C                   Z-ADD     YECIMO        V02ASP
651300991011     C                   MOVEL     WVTA          V3CVCA
651400991011     C                   END
651500991011     C                   Z-ADD     V3CAAS        KAAS
651600991011     C                   Z-ADD     V3CLNP        KLNP
651700991011     C                   Z-ADD     V3CNRS        KNRS
651800991011     C                   Z-ADD     V3CNSP        KNSP
651900991011     C                   MOVEL     'A'           KTIP
652000991011     C                   MOVEL     V3CVCA        KDIV
652100991011     C     KARI          CHAIN     FIARI01L                           44
652200991011     C* Se non ho trovato record lo azzero e imposto dati
652300991011     C* key
652400991011     C     *IN44         IFEQ      '1'
652500991011     C                   CLEAR                   FIARI000
652600991011     C                   Z-ADD     V3CAAS        ARIAAS
652700991011     C                   Z-ADD     V3CLNP        ARILNP
652800991011     C                   Z-ADD     V3CNRS        ARINRS
652900991011     C                   Z-ADD     V3CNSP        ARINSP
653000991011     C                   MOVEL     'A'           ARITIP
653100991011     C                   MOVEL     V3CVCA        ARIDIV
653200991011     C                   END
653300991011     C* Per assegnato verifico se devo memorizzare importo
653400991011     C* incassato o da incassare
653500991027     C     V02ASP        IFEQ      0
653600991027     C     V02TPA        ANDEQ     ' '
653700991011     C                   Z-ADD     V3CIMP        ARIIMP
653800991011     C                   ELSE
653900991011     C                   Z-ADD     V02ASP        ARIIMP
654000991011     C                   END
654100991011     C   44              WRITE     FIARI000
654200991011     C  N44              UPDATE    FIARI000
654300991116     C                   Z-ADD     ARIIMP        WIMA
654400991011     C* C/Assegno: sempre write
654500991011     C                   ELSE
654600991011     C* Se c'è importo totale incassato in un altra valuta calcolo
654700000421     C* C/Assegno x differenza x evitare problemi di arrotondamenti
654800000421     C* (è più semplice effettuarlo sul C/Ass. perchè è l'ultima riga
654900000421     C* del sbf degli importi che leggo --> sbf letto alla rovescia
655000000421     C* e anche perchè non devo gestire incassi parziali)
655100991018     C     V3CTOT        IFEQ      'SI'
655200991011     C     WPZTOT        ANDEQ     'SI'
655300991011     C     V3CVCA        ANDNE     WVTA
655400991011     C     WTOT          SUB       WIMA          V02ASP
655500991011     C                   MOVEL     WVTA          V3CVCA
655600991011     C                   END
655700991011     C* Per C/Assegno verifico se il C/Assegno è sulla mamma
655800991011     C* o sulla figlia
655900991011     C     VH2AAP        IFEQ      *ZEROS
656000991011     C                   Z-ADD     V3CLNP        ARILNP
656100991011     C                   Z-ADD     V3CAAS        ARIAAS
656200991011     C                   Z-ADD     V3CNRS        ARINRS
656300991011     C                   Z-ADD     V3CNSP        ARINSP
656400991011     C                   ELSE
656500991011     C                   Z-ADD     VH2LPP        ARILNP
656600991011     C                   Z-ADD     VH2AAP        ARIAAS
656700991011     C                   Z-ADD     VH2NRP        ARINRS
656800991011     C                   Z-ADD     VH2NSP        ARINSP
656900991011     C                   END
657000991011     C                   MOVEL     'C'           ARITIP
657100991011     C                   MOVEL     V3CVCA        ARIDIV
657200991118     C     V02ASP        IFGT      0
657300991011     C                   Z-ADD     V02ASP        ARIIMP
657400991118     C                   ELSE
657500991118     C                   Z-ADD     V3CIMP        ARIIMP
657600991118     C                   END
657700010412MM   C                   Z-ADD     ARIAAS        KAAS
657800010412MM   C                   Z-ADD     ARILNP        KLNP
657900010412MM   C                   Z-ADD     ARINRS        KNRS
658000010412MM   C                   Z-ADD     ARINSP        KNSP
658100010412MM   C                   MOVEL     ARITIP        KTIP
658200010412MM   C                   MOVEL     ARIDIV        KDIV
658300010412     C     KARI          CHAIN     FIARI01L                           44
658400010412     C   44              WRITE     FIARI000
658500010412     C  N44              UPDATE    FIARI000
658600991011     C                   END
658700991011     C*
658800991011     C                   ENDSR
658900991011      *---------------------------------------------------------*
659000991011      *  AZZARI: Verifico se devo srivere dati incasso x totale
659100991011      *---------------------------------------------------------*
659200991011     C     AZZARI        BEGSR
659300991011     C*
659400991011     C                   Z-ADD     0             WIMA             13 3
659500991011     C                   MOVEL     *BLANKS       WVTA              3
659600991011     C                   Z-ADD     0             WTOT
659700991011     C                   MOVEL     *BLANKS       WPZTOT            2
659800991011     C* Verifico se impostato incasso parziale su riga di totale
659900991116     C     V02SN1        IFNE      'N'
660000991011     C* Se si imposto variabile totale e valuta relativa
660100991011     C                   MOVEL     V3CVCA        WVTA
660200991011     C                   MOVEL     'SI'          WPZTOT
660300991116     C* Imposto totale
660400991116     C     V02ASP        IFNE      0
660500991116     C                   Z-ADD     V02ASP        WTOT
660600991116     C                   ELSE
660700991116     C                   Z-ADD     V3CIMP        WTOT
660800991011     C                   END
660900991116     C                   END
661000991011     C*
661100991011     C                   ENDSR
661200941130      *---------------------------------------------------------*
661300941130      *  SCrittura importi
661400941130      *---------------------------------------------------------*
661500941130     C     STPCON        BEGSR
661600941130     C*
661700941130     C                   MOVE      V02NCL        STPNCL
661800941130     C                   MOVE      V02PKG        STPPKG
661900941130     C                   MOVE      V02VOL        STPVOL
662000941130     C                   MOVE      V02CPZ        STPCPZ
662100941130     C                   MOVEL     V02RIS        STPRIS
662200941130     C*  Stampa
662300031008     c                   if        pretes = *blank
662400031008     c                   exsr      pretesta
662500031008     c                   move      'X'           pretes            1
662600031008     c                   endif
662700941130     C   OF              EXCEPT    TESTA
662800941130     C                   EXCEPT    DETCON
662900941130     C*
663000941130     C                   ENDSR
663100941122      *---------------------------------------------------------*
663200941122      *  Aggiorno dati firmatario
663300941122      *---------------------------------------------------------*
663400941122     C     AGGFIR        BEGSR
663500941122     C*
663600941122     C                   MOVE      '1'           KTRC
663700060214     C     KAR4          CHAIN     FiAR401L                           94
663800970220     C     '°':' '       XLATE     V2CFIR        AR4NOT
663900941122     C                   MOVE      *BLANKS       AR4FTR
664000941122     C                   MOVE      *ZEROS        AR4DTR
664100941122     C     *IN94         IFEQ      '1'
664200941122     C                   MOVE      '1'           AR4TRC
664300941228     C                   Z-ADD     KLNP          AR4LNP
664400941228     C                   Z-ADD     KAAS          AR4AAS
664500941228     C                   Z-ADD     KNRS          AR4NRS
664600941228     C                   Z-ADD     KNSP          AR4NSP
664700060214     C                   WRITE     FiAR4000
664800941122     C                   ELSE
664900060214     C                   UPDATE    FiAR4000
665000941122     C                   END
665100941122     C*
665200941122     C                   ENDSR
665300941122      *---------------------------------------------------------*
665400941122      *  Note Consegna
665500941122      *---------------------------------------------------------*
665600941122     C     NOTCON        BEGSR
665700941122     C*
665800941130     C                   MOVE      '0'           KTRC
665900060214     C     KAR4          CHAIN     FiAR401L                           94
666000970220     C     '°':' '       XLATE     V2CNT1        AR4NOT
666100941122     C                   MOVE      *BLANKS       AR4FTR
666200941122     C                   MOVE      *ZEROS        AR4DTR
666300941130     C     *IN94         IFEQ      '1'
666400941122     C                   MOVE      '0'           AR4TRC
666500941228     C                   Z-ADD     KLNP          AR4LNP
666600941228     C                   Z-ADD     KAAS          AR4AAS
666700941228     C                   Z-ADD     KNRS          AR4NRS
666800941228     C                   Z-ADD     KNSP          AR4NSP
666900060214     C                   WRITE     FiAR4000
667000941122     C                   ELSE
667100060214     C                   UPDATE    FiAR4000
667200941122     C                   END
667300941122     C*
667400941122     C                   ENDSR
667500970121      *---------------------------------------------------------*
667600970121      *  Note MANCATA Consegna
667700970121      *---------------------------------------------------------*
667800970121     C     NOTNM1        BEGSR
667900970121     C*
668000970121     C                   MOVE      V2CTR1        KTRC
668100060214     C     KAR4          CHAIN     FiAR401L                           94
668200970220     C     '°':' '       XLATE     V2CNM1        AR4NOT
668300970121     C                   MOVE      *BLANKS       AR4FTR
668400970121     C                   MOVE      *ZEROS        AR4DTR
668500970121     C     *IN94         IFEQ      '1'
668600970121     C                   MOVE      V2CTR1        AR4TRC
668700970121     C                   Z-ADD     KLNP          AR4LNP
668800970121     C                   Z-ADD     KAAS          AR4AAS
668900970121     C                   Z-ADD     KNRS          AR4NRS
669000970121     C                   Z-ADD     KNSP          AR4NSP
669100060214     C                   WRITE     FiAR4000
669200970121     C                   ELSE
669300060214     C                   UPDATE    FiAR4000
669400970121     C                   END
669500970121     C*
669600970121     C                   ENDSR
669700970121      *---------------------------------------------------------*
669800970121      *  Note MANCATA Consegna
669900970121      *---------------------------------------------------------*
670000970121     C     NOTNM2        BEGSR
670100970121     C*
670200970121     C                   MOVE      V2CTR2        KTRC
670300060214     C     KAR4          CHAIN     FiAR401L                           94
670400970220     C     '°':' '       XLATE     V2CNM2        AR4NOT
670500970121     C                   MOVE      *BLANKS       AR4FTR
670600970121     C                   MOVE      *ZEROS        AR4DTR
670700970121     C     *IN94         IFEQ      '1'
670800970121     C                   MOVE      V2CTR2        AR4TRC
670900970121     C                   Z-ADD     KLNP          AR4LNP
671000970121     C                   Z-ADD     KAAS          AR4AAS
671100970121     C                   Z-ADD     KNRS          AR4NRS
671200970121     C                   Z-ADD     KNSP          AR4NSP
671300060214     C                   WRITE     FiAR4000
671400970121     C                   ELSE
671500060214     C                   UPDATE    FiAR4000
671600970121     C                   END
671700970121     C*
671800970121     C                   ENDSR
671900941122      *---------------------------------------------------------*
672000941122      *  Aggiorno spedizione mamma
672100941122      *---------------------------------------------------------*
672200941122     C     SPEMAM        BEGSR
672300941122     C*
672400941122     C                   Z-ADD     VH2AAP        KAAS2
672500941122     C                   Z-ADD     VH2LPP        KLNP2
672600941122     C                   Z-ADD     VH2NRP        KNRS2
672700941122     C                   Z-ADD     VH2NSP        KNSP2
672800941122     C     KARB2         CHAIN     FNARB01L                           95
672900941216     C     *IN95         IFEQ      '0'
673000941122     C     ARBDCM        IFEQ      0
673100990311     C                   Z-ADD     COMDCP        ARBDCP
673200990311     C                   Z-ADD     COMDCM        ARBDCM
673300020829     C                   Z-ADD     COMifp        ARBifp
673400031209     C                   Z-ADD     COMpdc        ARBpdc
673500020829     C                   Z-ADD     COMNDC        ARBNDC
673600941228     C                   Z-ADD     COMDDC        ARBDDC
673700941122     C                   Z-ADD     COMHMC        ARBHMC
673800941230     C                   Z-ADD     0             ARBDT1
673900941230     C                   MOVEL     *BLANKS       ARBFT1
674000941122     C                   END
674100941123     C                   MOVE      COMICC        ARBICC
674200941122     C                   MOVEL     'Y'           ARBICA
674300941122     C                   Z-ADD     *ZEROS        ARBNMI
674400020826     C                   Z-ADD     dutpou        ARBIPP
674500020826     C                   Z-ADD     dutcou        ARBCUC
674600941122     C                   MOVE      *BLANKS       ARBFBC
674700941122     C                   MOVE      'S'           ARBFDC
674800950602     C                   Z-ADD     0             ARBNSS
674900941122     C* Se non c'è data arrivo merce forzo data consegna
675000941122     C     ARBDAM        IFEQ      *ZEROS
675100941122     C                   Z-ADD     DATCON        ARBDAM
675200950329     C                   END
675300941122     C     ARBDTI        IFEQ      *ZEROS
675400950329     C     WMDDTI        ANDEQ     'S'
675500941122     C                   Z-ADD     DATCON        ARBDTI
675600950329     C                   Z-ADD     0202          ARBHTI
675700941122     C                   END
675800950331     C     WFBS          IFEQ      'S'
675900950410     C     VH2AAP        ANDEQ     HARAAS
676000950410     C     VH2LPP        ANDEQ     HARLNP
676100950410     C     VH2NRP        ANDEQ     HARNRS
676200950410     C     VH2NSP        ANDEQ     HARNSP
676300970924     C     ARBFBS        IFEQ      ' '
676400970924     C                   MOVEL     'S'           ARBFBS
676500970924     C                   ELSE
676600970924     C     ARBFBS        IFEQ      'V'
676700040505     C     ARBFBS        oreq      'P'
676800970924     C                   MOVEL     'E'           ARBFBS
676900970924     C                   END
677000970924     C                   END
677100040505     C***                MOVEL     'S'           WRICH1
677200950331     C                   END
677300961219     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
677400961219     C* ARTDTR solo x estero
677500961219     C     WAGDT3        IFEQ      'S'
677600961219     C     ARBDT3        ANDEQ     0
677700961219     C                   Z-ADD     DATCON        ARBDT3
677800961219     C                   END
677900941122     C                   UPDATE    FNARB000
678000941122     C                   END
678100941122     C*
678200941122     C                   ENDSR
678300941122     C*------------------------------------------------------
678400941122     C* Aggior. record totale distinta tassazione padroncini
678500941122     C*------------------------------------------------------
678600941122     C     UPDFTT        BEGSR
678700941122     C*  Procedo all'aggiornamento archivio conteggi
678800941122     C                   MOVE      ARBPDC        KPDR
678900941122     C                   MOVE      'C'           KTSR
679000941122     C                   MOVE      NDC(I)        KNDC
679100011121     C     KFTT          CHAIN     fiftt01L                           91
679200011121     C   91              CLEAR                   fiftt000
679300941122     C                   MOVE      ARBPDC        FTTPDR
679400140225     C                   MOVE      fttflr        dftt01
679500060120     c                   if        arbifp > 0
679600060120     C                   z-add     ARBifp        fttfgs
679700060120     c                   else
679800060120     C                   z-add     d53fgs        fttfgs
679900060120     c                   endif
680000060120     c                   if        fttfgs = 0
680100060120     c                   movel     fttpdr        fttfgs
680200060120     c                   end
680300941122     C                   MOVE      'C'           FTTTSR
680400031008     C                   MOVE      ndc(i)        FTTNDC
680500031009     C                   MOVE      Dtd(I)        G02DAT
680600031009     C                   MOVEL     *BLANK        G02ERR
680700031009     C                   CALL      'XSRDA8'
680800031009     C                   PARM                    WLBDA8
680900031009     C                   Z-ADD     G02INV        fttddc
681000051222     C                   Z-ADD     oggi          fttdtr
681100941122     C                   MOVE      SET(I)        FTTSET
681200941122     C                   MOVE      SNT(I)        FTTSNT
681300020826     C                   MOVE      dutcou        FTTCUC
681400140225     C                   MOVE      dutpou        §FTTpou
681500140225     c                   movel     dftt01        fttflr
681600941122     C                   MOVE      FPP(I)        FTTFPP
681700941122     C                   MOVE      PEP(I)        FTTPEP
681800030204     C                   move      Car(I)        FTTlvl
681900941122     C*
682000941122     C*  Se in distinta non esistono spedizioni consegnate non metto
682100941122     C*  il blocco
682200941122     C                   MOVE      'B'           FTTFVL
682300941122     C     SET(I)        IFEQ      SNT(I)
682400941122     C                   MOVE      ' '           FTTFVL
682500941122     C                   END
682600941202     C*
682700011121      * In base alla data del documento aggancio la tab.GEDCN per
682800011121      * impostare la divisa della valorizzazione dei conteggi.
682900011121     c                   if        fttddc <= §geddtfv
683000011121     c                   movel     §geditl       fttdiv
683100011121     c                   else
683200011121     c                   movel     §gedeur       fttdiv
683300011121     c                   end
683400011121      *
683500011121     C   91              WRITE     fiftt000
683600011121     C  N91              UPDATE    fiftt000
683700941122     C*
683800941122     C                   ENDSR
683900941122     C*---------------------------------------------------------
684000941122     C* Aggior. record dettaglio distinta tassazione padroncini
684100941122     C*--------------------------------------------------------
684200941122     C     UPDFTD        BEGSR
684300941122     C*
684400941122     C*  Procedo all'aggiornamento archivio conteggi
684500060203     C     KFTD          CHAIN     fiftd22L                           91
684600941122     C                   MOVEL     *BLANKS       FTDATB
684700941122     C                   MOVE      ARBPDC        FTDPDR
684800041022     c                   if        arbifp > 0
684900020829     C                   z-add     ARBifp        ftdfgs
685000041022     c                   else
685100041022     C                   z-add     d53fgs        ftdfgs
685200041022     c                   endif
685300060119     c                   if        ftdfgs = 0
685400060119     c                   movel     ftdpdr        ftdfgs
685500060119     c                   end
685600941122     C                   MOVE      'C'           FTDTSR
685700941122     C                   MOVE      V2CLNP        FTDLNP
685800941122     C                   MOVE      V2CAAS        FTDAAS
685900941122     C                   MOVE      V2CNRS        FTDNRS
686000941122     C                   MOVE      V2CNSP        FTDNSP
686100941122     C                   MOVE      NDC(I)        FTDNDC
686200031009     C                   MOVE      arbddc        FTDDDC
686300941122     C                   MOVE      ARBRSD        FTDRSC
686400950111ES   C                   MOVE      ARBCAD        FTDCAP
686500950111ES   C                   MOVE      ARBFIN        FTDFIN
686600950118ES   C                   Z-ADD     1             FTDSET
686700941122     C*  Solo incasso: tipo record 50
686800941122     C     V2CCON        IFEQ      'NO'
686900941122     C                   MOVE      ' '           FTDSIC
687000941207     C                   MOVE      STPNCL        FTDNCL
687100941207     C                   MOVE      STPPKG        FTDPKL
687200941207     C                   MOVE      STPVOL        FTDVLU
687300941122     C                   ELSE
687400941122     C                   MOVE      'S'           FTDSIC
687500941122     C                   END
687600941122     C*  Mancata consegna: 'N' in riconsegna giacenza
687700941122     C     V02CCN        IFNE      *BLANKS
687800941122     C     V02CCN        ANDNE     'P  '
687900941122     C                   MOVE      'N'           FTDFGC
688000941130     C                   MOVE      §2AFTC        FTDTMC
688100941130     C                   MOVE      V02CCN        FTDCMC
688200941122     C                   ELSE
688300941122     C                   MOVE      ' '           FTDFGC
688400941122     C*  Controllo se bolla senza dettaglio segnacolli
688500941202     C     V02CPZ        IFNE      'P'
688600941122     C     SAVNCL        IFNE      0
688700941122     C     SAVPKG        ORNE      0
688800941122     C     SAVVOL        ORNE      0
688900941122     C                   MOVE      ' '           FTDFGC
689000941122     C                   MOVE      SAVNCL        FTDNCL
689100941123     C                   MOVE      SAVPKG        FTDPKL
689200941123     C                   MOVE      SAVVOL        FTDVLU
689300941122     C                   END
689400941130     C                   ELSE
689500941130     C                   MOVEL     V02CPZ        FTDCMC
689600941130     C                   END
689700941122     C                   END
689800941122     C*
689900941122     C                   MOVE      V02STP        FTDSTP
690000950602     C                   MOVE      WNSS          FTDNSS
690100941122     C*  Mancata consegna: non valorizzo data consegna
690200941122     C     V02CCN        IFEQ      *BLANKS
690300941122     C     V02CCN        OREQ      'P  '
690400941122     C                   MOVE      DATCON        FTDDCM
690500941122     C                   MOVE      ORACON        FTDHMC
690600941122     C                   ELSE
690700941122     C                   MOVE      *ZERO         FTDDCM
690800941122     C                   MOVE      *ZERO         FTDHMC
690900941122     C                   END
691000941122     C                   MOVEL     V02RIS        FTDCRC
691100941122     C*
691200011121     C   91              WRITE     fiftd000
691300011121     C  N91              UPDATE    fiftd000
691400941122     C*
691500941122     C                   ENDSR
691600970926     C*---------------------------------------------------------
691700970926     C* Aggior. record dettaglio trasmissione in sede stop
691800970926     C*--------------------------------------------------------
691900970926     C     ELASTP        BEGSR
692000000421     C*
692100970926     C     NRR2          CHAIN     LR53S02                            96
692200970926    1C     *IN96         DOWEQ     '0'
692300970926     C     VNMDOC        ANDEQ     WSVNDC
692400970926    2C     V2CCON        IFEQ      'NO'
692500970926     C     V2CDCM        ANDGT     0
692600970926     C     V02STP        ANDGT     0
692700971001     C     V2CRBL        ANDNE     'R'
692800970926    3C     V2CBSD        IFNE      'N'
692900970926     C     V2CBFI        ORNE      'N'
693000970926     C                   Z-ADD     1             WW
693100970926     C     V02STP        LOOKUP    NST(WW)                                95
693200970926     C                   Z-ADD     SPS(WW)       WSET
693300970926     C                   EXSR      UPDSTP
693400970926    3C                   END
693500970926    2C                   END
693600970926     C                   ADD       1             NRR2
693700970926     C     NRR2          CHAIN     LR53S02                            96
693800970926    1C                   ENDDO
693900970926     C*
694000970926     C                   ENDSR
694100950215     C*---------------------------------------------------------
694200950215     C* Aggior. record dettaglio trasmissione in sede stop
694300950215     C*--------------------------------------------------------
694400970926     C     CHKSTP        BEGSR
694500970926     C*
694600970926     C*  Controllo il codice bolla
694700970926     C                   CLEAR                   DS3A
694800970926     C                   Z-ADD     1             X
694900970926     C     ARBCBO        LOOKUP    C3A(X)                                 32
695000970926     C   32              MOVEL     D3A(X)        DS3A
695100971001     C*
695200971001     C                   MOVE      §3ABSD        V2CBSD
695300971001     C                   MOVE      §3ABFI        V2CBFI
695400971001     C                   MOVEL     §3ARBL        V2CRBL
695500971001     C                   Z-ADD     ARBDCM        V2CDCM
695600971007     C     NRR2          CHAIN     LR53S02                            96
695700990423     C                   MOVEL     V2IN09        *IN09
695800000522     C                   MOVEL     V2IN24        *IN24
695900971001     C                   UPDATE    LR53S02
696000971001     C*
696100970926    1C     V2CCON        IFEQ      'NO'
696200970926     C     ARBDCM        ANDGT     0
696300971001     C* Escludo recupero perchè non sono previste in C/E competenze di
696400971001     C* stop sulle spedizioni di recupero
696500971001     C     §3ARBL        ANDNE     'R'
696600970926    2C     §3ABSD        IFNE      'N'
696700970926     C     §3ABFI        ORNE      'N'
696800970926     C*
696900970926    3C     D53CMG        IFEQ      'S'
697000000615     C                   Z-ADD     99999         WSET
697100970926     C                   EXSR      UPDSTP
697200970926   X3C                   ELSE
697300970926    4C     V02STP        IFGT      *ZEROS
697400970926     C                   Z-ADD     1             WW                3 0
697500970926     C     V02STP        LOOKUP    NST(WW)                                95
697600970926     C   95              ADD       1             SPS(WW)
697700080903    5C  N95YY            IFLT      200
697800970926     C                   ADD       1             YY
697900970926     C                   Z-ADD     V02STP        NST(YY)
698000970926     C                   Z-ADD     1             SPS(YY)
698100970926    5C                   END
698200970926    4C                   END
698300970926    3C                   END
698400970926    2C                   END
698500970926    1C                   END
698600950215     C*
698700950215     C                   ENDSR
698800970926     C*---------------------------------------------------------
698900970926     C* Aggior. record dettaglio trasmissione in sede stop
699000970926     C*--------------------------------------------------------
699100970926     C     UPDSTP        BEGSR
699200970926     C*
699300970926     C                   CLEAR                   FNSTP100
699400000615     C* SE NO CONS.A MAGAZZINO E NUMERO SPED.=99999 SCRIVO 99998
699500000615     C* (99999 E'USATO SOLO PER LE CONSEGNE A MAGAZZINO)
699600971028     C     D53CMG        IFEQ      *BLANKS
699700000615     C     WSET          ANDEQ     99999
699800000615     C                   Z-ADD     99998         FTPSET
699900971028     C                   ELSE
700000970926     C                   Z-ADD     WSET          FTPSET
700100971028     C                   END
700200970926     C                   MOVE      'C'           FTPTSR
700300970926     C                   MOVE      V2CLNP        FTPLNP
700400970926     C                   MOVE      V2CAAS        FTPAAS
700500970926     C                   MOVE      V2CNRS        FTPNRS
700600970926     C                   MOVE      V2CNSP        FTPNSP
700700970926    3C                   SELECT
700800970926     C     §3ABSD        WHENEQ    *BLANKS
700900970926     C     §3ABFI        ANDEQ     *BLANKS
701000970926     C                   MOVEL     'E'           FTPINV
701100970926     C     §3ABSD        WHENEQ    *BLANKS
701200970926     C                   MOVEL     'S'           FTPINV
701300970926     C     §3ABFI        WHENEQ    *BLANKS
701400970926     C                   MOVEL     'F'           FTPINV
701500970926    3C                   ENDSL
701600970926     C                   WRITE     FNSTP100
701700970926     C*
701800970926     C                   ENDSR
701900941123     C*---------------------------------------------------------
702000941123     C*  Totali stampa
702100941123     C*--------------------------------------------------------
702200941123     C     GESTOT        BEGSR
702300941123     C*
702400941123     C*  Riporto totali in stampa
702500941123     C                   Z-ADD     V05TSC        STPTSC
702600941123     C                   Z-ADD     V05STP        STPTST
702700000505     C                   Z-ADD     V05ADR        STPADR
702800000505     C                   Z-ADD     V05NDR        STPNDR
702900941206     C     STPLI1        IFGT      51
703000941123     C                   EXCEPT    STAMPA
703100941123     C                   END
703200941123     C                   EXCEPT    TOTAL
703300941123     C*  Totale da esigere in divisa
703400991007     C     V05TCE        IFNE      *ZEROS
703500990924     C     V05TAE        ORNE      *ZEROS
703600060419     C     V05pos        ORNE      *ZEROS
703700990924     C     V05TAV        ORNE      *ZEROS
703800991007     C     V05TDE        ORNE      *ZEROS
703900991007     C                   Z-ADD     V05TCE        STPEES
704000991007     C                   ADD       V05TAE        STPEES
704100060419     C                   ADD       V05pos        STPEES
704200990924     C                   Z-ADD     V05TCE        STPECO
704300060419     C                   ADD       V05pos        STPECO
704400990924     C                   Z-ADD     V05TAE        STPEAN
704500991007     C                   Z-ADD     V05TDE        STPEAD
704600991007     C                   Z-ADD     V05TAV        STPDAD
704700990924     C                   EXCEPT    TOTEU
704800941123     C                   END
704900970227     C*  Se non è la prima volta e ci sono delle bolle con la
705000970227     C*  data consegna = a quella della distinta richiamo pgm
705100970227     C     WSVNDC        IFNE      0
705200970227     C     WNBOL         ANDNE     0
705300970227     C                   MOVEL     'S'           WRISTA            1
705400970227     C                   MOVEL     *BLANKS       SA2TLA            1
705500970227     C                   MOVEL     'C'           SA2TRC            1
705600970228     C                   Z-ADD     CONDIS        SA2DTS            8 0
705700970227     C                   Z-ADD     D53LNA        SA2LNA            3 0
705800970227     C                   CALL      'FNLSA2R3'
705900970227     C                   PARM                    SA2TLA            1
706000970227     C                   PARM                    SA2TRC            1
706100970227     C                   PARM                    SA2DTS            8 0
706200970227     C                   PARM                    SA2LNA            3 0
706300970227     C                   END
706400941123     C*
706500941123     C                   ENDSR
706600941123     C*---------------------------------------------------------
706700941123     C* MEMORIZZAZIONE EVENTI BOLLE
706800941123     C*----------------------------------------------------------
706900941123?    C     EVENTI        BEGSR
707000941123     C*
707100941123     C                   MOVEL     *BLANKS       EVBATB
707200941123     C                   Z-ADD     OGGI          EVBDTV
707300941123     C                   TIME                    EVBORV
707400020905     C*                  Z-ADD     D53LNA        EVBFLE
707500020905     C                   Z-ADD     dutpou        EVBFLE
707600020826     C                   Z-ADD     dutcou        EVBCDU
707700941123     C* Sulla bolla metto i riferimenti della mamma se esiste legame
707800941123     C     VH2AAP        IFEQ      *ZEROS
707900941123     C                   Z-ADD     V2CLNP        EVBLNP
708000941123     C                   Z-ADD     V2CAAS        EVBAAS
708100941123     C                   Z-ADD     V2CNRS        EVBNRS
708200941123     C                   Z-ADD     V2CNSP        EVBNSP
708300941123     C                   ELSE
708400941123     C                   Z-ADD     VH2LPP        EVBLNP
708500941123     C                   Z-ADD     VH2AAP        EVBAAS
708600941123     C                   Z-ADD     VH2NRP        EVBNRS
708700941123     C                   Z-ADD     VH2NSP        EVBNSP
708800941123     C                   END
708900941123     C                   Z-ADD     DATCON        EVBDEV
709000941123     C                   Z-ADD     ORACON        EVBHEV
709100941123     C                   MOVEL     *BLANKS       EVBFTR
709200941123     C                   Z-ADD     0             EVBDTR
709300131112     c                   clear                   devb01
709400131205      * imposta flag per sapere chi ha inserito l'evento  se cassiere spc o autista
709500131205      * se la distinta è pda altrimenti sempre A = cassiere
709600131205     c                   if        wpda = 'S'
709700131205     c                   if        v02ccn <> *blank and
709800131205     c                             v02ccn <> vh2ccn
709900131205     c                   move      'A'           §notagg
710000131205     c                   else
710100131205     c                   if        vh2spc = 'S'
710200131205     c                   move      'S'           §notagg
710300131205     c                   else
710400131205     c                   move      'P'           §notagg
710500131205     c                   endif
710600131205     c                   endif
710700131205     c                   else
710800131205     c                   move      'A'           §notagg
710900131205     c                   endif
711000131112     C                   move      ARBPDC        §NOTPDC
711100131112     c                   if        arbndc > 0
711200131112     C                   move      ARBNDC        §NOTNDC
711300131112     c                   else
711400131112     c                   move      warbndc       §notndc
711500131112     c                   endif
711600131121      *leggo eventuale evento CEP
711700131121     c                   move      'X'           DaEventi          1
711800131121     c                   exsr      leggiCEP
711900131121     c                   clear                   DaEventi
712000131120     c                   if        scriviCEP = 'S'
712100131120     c                   move      'U'           §notprg
712200131120     c                   else
712300131120     c                   move      *blank        §notprg
712400131120     c                   endif
712500131106     C                   MOVEL     Devb01        EVBNOT
712600990803     C                   Z-ADD     0             EVBDTC
712700941123     C*
712800990803     C                   WRITE     FNEVB000
712900070406      * verifico se codice evento richiede gestione sospensione bolla dalla
713000070406      * consegna ESEMPIO: RS riserva speciale
713100070406     c                   if        §oggeot = 'S'
713200070406     c                   if        §2zsos = 'S'
713300070406     c                   exsr      insbloc
713400070406     c                   endif
713500070406     c                   endif
713600070312      * verifico se evento proposto abilita scrittura estensione evento
713700070312     C                   Z-ADD     1             Yz                3 0
713800140114     c                   if        v2cevp <> *blank
713900070312     C     V2cevp        LOOKUP    C2A(Yz)                                95
714000140114     c                   else
714100140114     C     evbcev        LOOKUP    C2A(Yz)                                95
714200140114     c                   endif
714300070312     C     *IN95         IFEQ      '1'                                          --- 04 ---
714400070312     C                   MOVEL     D2Z(Yz)       DS2Z
714500070312     C                   endif                                                  --- 04 ---
714600140207      *scrittura estensione evento per l.Avv. solo se filiale abilitata
714700070319     c                   if        §oggeot = 'S'
714800070312     c                   if        §2zev2 = 'S'
714900070312     c                   exsr      eventi2
715000070312     c                   endif
715100070319     c                   endif
715200941123     C*
715300941123?    C                   ENDSR
715400140207     C*---------------------------------------------------------
715500140207      *scrittura record spia per invio sms/mail al destinatario in caso
715600140207      *di lasciato avviso al 1° tentativo di consegna
715700140207     C*----------------------------------------------------------
715800140207?    C     srspiasms     begsr
715900140207     c                   clear                   trtcp2ds
716000140207     c                   eval      t2chi= 'C'
716100140207     c                   z-add     arbifp        t2fgs
716200140207     c                   z-add     arbndc        t2ndc
716300140207     C                   Z-ADD     arbLNP        t2lnp
716400140207     C                   Z-ADD     arbAAS        t2aas
716500140207     C                   Z-ADD     arbNRS        t2nrs
716600140207     C                   Z-ADD     arbNSP        t2nsp
716700140207     C                   Z-ADD     DATCON        t2DEV
716800140207     C                   Z-ADD     ORACON        t2HEV
716900140207     c                   call      'TRTCP2R'
717000140207     c                   parm                    trtcp2ds
717100140207?    C                   ENDSR
717200140113     C*---------------------------------------------------------
717300140113     C* scrittura estensione EVENTI
717400140113     C*----------------------------------------------------------
717500140113?    C     eventi2       begsr
717600140113      *
717700140207     c                   clear                   fnevb2ds
717800140113     C                   move      evblnp        evb2lnp
717900140113     C                   move      evbaas        evb2aas
718000140113     C                   move      evbnrs        evb2nrs
718100140113     C                   move      evbnsp        evb2nsp
718200140113     C                   move      evbcev        evb2cev
718300140113     C                   move      evbdtv        evb2dtv
718400140113     C                   move      evborv        evb2orv
718500140113     C                   move      evbfle        evb2fle
718600140113     C                   move      evbdev        evb2DEV
718700140113     C                   move      evbhev        evb2HEV
718800140113     C                   move      §2ztel        evb2TEL
718900140113     C                   move      *blank        evb2FLR
719000140113     C                   move      knmus         evb2ute
719100140113     C* se ho la data consegna richiesta proposta da telefonata non metto
719200140113     c* il flag da telefonare
719300140113     c                   if        v2harp = '1'
719400140113     c                   clear                   evb2tel
719500140113     c                   end
719600140212     c* se non devo telefonare e se i campi del firmatario o delle note sono
719700140212      * vuoti, non scrivo il record
719800140212     c                   if        evb2tel = ' ' and
719900140212     c                             v2cfir = *blank and
720000140212     c                             v2cnt1 = *blank
720100140212     c                   leavesr
720200140212     c                   end
720300140113      * se sono con bolla consegnata i presenza di firmatario inserisco un ulteriore record
720400140113      * se è presente anche la nota
720500140113     c                   if        v2cfir <> *blank
720600140113     C                   movel     v2cfir        evb2dati
720700140113     C                   move      'FIR'         evb2trd
720800140113     C                   move      v02ris        evb2ris
720900140113     C                   WRITE     FNEVB200
721000140113     c                   if        v2cnt1 <> *blank
721100140113     C                   movel     v2cnt1        evb2dati
721200140113     C                   move      'NOT'         evb2trd
721300140113     C                   WRITE     FNEVB200
721400140113     c                   endif
721500140113     c                   else
721600140113      * lasciato avviso se presente nota inserisco tipo record
721700140113     c                   if        v2cnt1 <> *blank
721800140113     C                   move      'NOT'         evb2trd
721900140113     C                   movel     v2cnt1        evb2dati
722000140113     c                   endif
722100140113     C                   WRITE     FNEVB200
722200140113     c                   endif
722300140113      *
722400140113     c                   endsr
722500131121      *________________________________________________________________
722600131121     c     leggiCep      begsr
722700131121      *________________________________________________________________
722800131121     c** verificop se esiste Rekord CEP abbinato per segnare l'evento che sto scrivendo
722900131121      * e allo stesso tempo memorizzo se devo scrivere l'evento con il motivo del CEP
723000131121     c                   move      'CEP'         ttrd
723100131121     c     kcet          setgt     fipct02l
723200131121     c     kcet          readpe    fipct02l
723300131121     c                   if        not %eof(fipct02l)
723400131121     c                   clear                   ds2acep
723500131121     c                   movel     pctdati       fipctcetds
723600141202      ****
723700131121     c                   if        §pctcmc <> *blank
723800131121     c                   z-add     1             ww
723900131121     c     §pctcmc       lookup    c2a(ww)                                85
724000131121     c   85              movel     d2a(ww)       ds2acep
724100131121     c                   if        w§2aevb = 'S'
724200131121     c                   move      'S'           scriviCEP
724300131121      *forza riempimento campi se non sono passato già dagli eventi quindi
724400131121      * sono in situazione di evento CEP ma CET consegnato e senza riserva
724500131121     c                   if        DaEventi = *blank
724600131121     C                   MOVEL     *BLANKS       EVBATB
724700131121     C                   Z-ADD     OGGI          EVBDTV
724800131121     C                   TIME                    EVBORV
724900131121     C*                  Z-ADD     D53LNA        EVBFLE
725000131121     C                   Z-ADD     dutpou        EVBFLE
725100131121     C                   Z-ADD     dutcou        EVBCDU
725200131121     C* Sulla bolla metto i riferimenti della mamma se esiste legame
725300131121     C     VH2AAP        IFEQ      *ZEROS
725400131121     C                   Z-ADD     V2CLNP        EVBLNP
725500131121     C                   Z-ADD     V2CAAS        EVBAAS
725600131121     C                   Z-ADD     V2CNRS        EVBNRS
725700131121     C                   Z-ADD     V2CNSP        EVBNSP
725800131121     C                   ELSE
725900131121     C                   Z-ADD     VH2LPP        EVBLNP
726000131121     C                   Z-ADD     VH2AAP        EVBAAS
726100131121     C                   Z-ADD     VH2NRP        EVBNRS
726200131121     C                   Z-ADD     VH2NSP        EVBNSP
726300131121     C                   END
726400131121     c                   endif
726500131121     c                   endif
726600131121     c                   endif
726700131121     c                   endif
726800131121     c                   endsr
726900131108     C*---------------------------------------------------------
727000131108     C* scrittura EVENTI generati da doppio tentativo di consegna
727100131108     C*----------------------------------------------------------
727200131108?    C     eventicep     begsr
727300131120      *
727400131108     C                   move      §pctcmc       EVBcev
727500131108     C                   movel     *blank        evbatb
727600131118     C                   move      evborv        woramin
727700131108     c     woramin       subdur    1:*mn         woramin
727800131108     C                   move      woramin       evborv
727900131108     C                   Z-ADD     pctfgs        evbfle
728000131108     C                   Z-ADD     dutcou        evbcdu
728100131108     C                   movel     pctdtorin     evbdev
728200131118     C                   move      pctdtorin     wevbhev           6 0
728300131118     C                   movel     wevbhev       evbhev
728400131108     C                   MOVEL     *BLANKS       evbftr
728500131108     C                   Z-ADD     0             evbdtr
728600131108     c                   clear                   devb01
728700131108     C                   move      ARBPDC        §NOTPDC
728800131112     c                   if        arbndc > 0
728900131108     C                   move      ARBNDC        §NOTNDC
729000131112     c                   else
729100131112     c                   move      warbndc       §notndc
729200131112     c                   endif
729300131108     c                   move      'P'           §notprg
729400131213     c                   move      'P'           §notagg
729500131108     C                   MOVEL     Devb01        EVBNOT
729600131108     C                   Z-ADD     0             EVBDTC
729700131121     C* se motivo di lasciato avviso e non ho ancora avuto un ric per la bolla lo forzo
729800131121     c                   if        w§2aftc = 'A'
729900131121     c     kevbric       setll     fnevb04l                               86
730000131121     c  n86              move      'RIC'         evbcev
730100131121     c                   endif
730200131121      *
730300131108     C                   WRITE     FNEVB000
730400131108      *
730500131108     c                   endsr
730600090622     C*---------------------------------------------------------
730700090622     C* Cancellazione eventuali eventi particolari inseriti con data ora maggiori della consegna
730800090622     C*---------------------------------------------------------
730900090622?    C     delevepar     BEGSR
731000090622     c     kevbpar       setll     fnevb01l
731100090622     c                   do        *hival
731200090622     c     kevbpar2      reade     fnevb01l
731300090622     c                   if        %eof(fnevb01l)
731400090622     c                   leave
731500090622     c                   endif
731600090622     c                   z-add     1             m                 3 0
731700090622     c     evbcev        lookup    c2a(m)                                 67
731800090622     c   67              movel     d2z(m)        ds2z
731900090622     c                   if        §2zcep <> *blank
732000090622     c                   delete    fnevb000
732100090622     c                   endif
732200090622     c                   enddo
732300090622     c                   endsr
732400941123     C*---------------------------------------------------------
732500941123     C* GESTIONE ANOMALIE COLLI CONSEGNATI
732600941123     C*---------------------------------------------------------
732700941123?    C     GESANM        BEGSR
732800941123     C*
732900960418     C                   CLEAR                   DSLR33
733000960418     C                   Z-ADD     ARTDCM        D33DCH
733100960418     C                   MOVEL     '4'           D33FSC
733200960418     C                   MOVEL     'CO'          D33CCH
733300960426     C                   MOVEL     V02CCA        D33CCA
733400960418     C*
733500941129     C                   Z-ADD     ARTFLS        KFLS
733600941129     C                   Z-ADD     ARTLNA        KLNA
733700941130     C                   Z-ADD     ARTNRS        KNRS1
733800941123     C                   Z-ADD     ARTNSC        KSCN
733900990519     C     KANM          CHAIN     FNANM02L                           94
734000941123     C     *IN94         DOWEQ     '0'
734100960418     C                   Z-ADD     ANMNR2        D33NRR
734200960418     C                   CALL      'FNLR33R'
734300960418     C                   PARM                    DSLR33
734400990519     C     KANM          READE     FNANM02L                               94
734500960418     C                   ENDDO
734600941123     C*
734700941123?    C                   ENDSR
734800941123     C*-----------------------------------------------------*
734900941123     C*  Operazioni iniziali
735000941123     C*-----------------------------------------------------*
735100941123     C     DEFCAM        BEGSR
735200941123     C*
735300970227     C                   MOVEL     *BLANKS       WRISTA
735400941213     C                   Z-ADD     D53LNA        V1CLNA            3 0
735500941123     C                   Z-ADD     ND1           NDC
735600941123     C                   MOVEL     'N'           WAGGIO            1
735700941123     C                   MOVEL     ' '           WFINE             1
735800941124     C                   MOVEL     '2'           WTPVID            1
735900941123     C                   MOVEL     'N'           WTRIMP            1
736000060303     C                   MOVEL     'N'           WTRpre            1
736100941124     C                   MOVEL     D53DSF        V2DRSC
736200941130     C                   MOVEL     D53DSF        DESPAD
736300941130     C                   MOVEL     D53WRT        CNTPDR
736400941124     C                   Z-ADD     1             NRR2
736500941124     C                   Z-ADD     1             NRR3
736600941124     C                   Z-ADD     1             NRR6
736700941124     C                   Z-ADD     1             NRR7
736800941124     C                   Z-ADD     1             NR2KEY
736900941124     C                   Z-ADD     1             NR3KEY
737000950221     C* SE NON DEVO FARE CONTEGGI PADR.
737100950221     C     CNTPDR        IFEQ      'N'
737200950221     C*----------------------------------------------------*
737300970926     C                   OPEN      FNSTP10F
737400971001     C* EFFETTUO UNA CHAIN FITTIZIA DI FNSTP10F PER GIUSTIFICARE
737500971001     C* L'APERTURA IN UPDATE
737600971001     C                   READ(N)   FNSTP10F                               96
737700950221     C                   ENDIF
737800070319     C*  reperisco AZORG della linea di arrivo per abilitazioni
737900070319     c                   clear                   og148
738000070405     c                   clear                   og146
738100070319     C     v1cLNA        CHAIN     AZORG01L                           74
738200070319     C  N74              MOVEL     ORGDE8        og148                           L.ARRIVO
738300070405     C  N74              MOVEL     ORGDE6        og146                           L.ARRIVO
738400070424     c                   if        §ogdda <> *blank
738500070424     c                   move      §ogdda        ogddan            8 0
738600070424     c                   endif
738700950221      *
738800941123     C                   ENDSR
738900941123     C*-----------------------------------------------------*
739000941123     C*  Operazioni iniziali
739100941123     C*-----------------------------------------------------*
739200941123     C     *INZSR        BEGSR
739300941123     C*
739400941123     C* Apertura controllata dei files
739500020823     C                   OPEN      FNFVV01L                             16
739600070410     C  N16              OPEN      fidst09l                             16
739700941124     C  N16              OPEN      CNASS00F                             16
739800990519     C  N16              OPEN      FNANM02L                             16
739900990519     C  N16              OPEN      FNANM01L                             16
740000990803     C  N16              OPEN      FNEVB01L                             16
740100070312     C  N16              OPEN      FNEVB20f                             16
740200011121     C  N16              OPEN      fiftt01L                             16
740300060203     C  N16              OPEN      fiftd22L                             16
740400941124     C  N16              OPEN      FNARB01L                             16
740500070313     C  N16              OPEN      Fiarp01L                             16
740600060214     C  N16              OPEN      FiAR401L                             16
740700051115     C  N16              OPEN      FIAR901L                             16
740800941124     C  N16              OPEN      FNART01L                             16
740900960116     C  N16              OPEN      FNARBX0F                             16
741000980402     C  N16              OPEN      FNCDS02L                             16
741100051027     C  N16              OPEN      FIARS01L                             16
741200990924     C  N16              OPEN      FIARI01L                             16
741300941123     C   16              GOTO      FINE                                         OPEN vado a fine
741400950619     C*  Richiamo PARUT
741500950619     C                   Z-ADD     1             CODUT
741600950619     C                   CALL      'X§PARUT'
741700950619     C                   PARM                    UTEDSE
741800950619      * Trasformo data contabilizz. da 6 a 8
741900950619     C                   MOVEL     DTU(10)       W20               2 0
742000950619     C     W20           IFGT      50
742100950619     C     1900          ADD       W20           W40               4 0
742200950619     C                   ELSE
742300950619     C     2000          ADD       W20           W40
742400950619     C                   END
742500950619     C                   MOVE      DTU(10)       §DTCON            8 0
742600950619     C                   MOVEL     W40           §DTCON
742700020822     c*
742800020822     c     *dtaara       define    §azute        azuteds
742900020822     c     *dtaara       define    §datiute      ddatiute
743000020822     C                   in(E)     *dtaara
743100020822     C                   IF        %Error  or  RSUT = *blanks
743200020822     C                   call      'TIBS34R'
743300020822     C                   parm                    Tibs34Ds
743400020822     C                   in        *dtaara
743500020822     c                   ENDIF
743600941123     C*----------------------------------------------------*
743700941123     C*  Definizione chiavi
743800080808     c     kar5          klist
743900080808     c                   kfld                    V2CAAS
744000080808     c                   kfld                    V2CLNP
744100080808     c                   kfld                    V2CNRS
744200080808     c                   kfld                    V2CNSP
744300080808     c                   kfld                    trek5             3
744400120302     c     kspevid       klist
744500120302     c                   kfld                    V3CAAS
744600120302     c                   kfld                    V3CLNP
744700120302     c                   kfld                    V3CNRS
744800120302     c                   kfld                    V3CNSP
744900080808
745000080806     C     Kcet          KLIST
745100100128     C                   KFLD                    d53fgs
745200080806     C                   KFLD                    vnmdoc6           6 0
745300080808     C                   KFLD                    w53pdc
745400080806     C                   KFLD                    TTRD
745500080806     C                   KFLD                    V2CAAS
745600080806     C                   KFLD                    V2CLNP
745700080806     C                   KFLD                    V2CNRS
745800080806     C                   KFLD                    V2CNSP
745900080806     C                   move      'CET'         TTRD              3
746000140609     C     Kpdc          KLIST
746100140609     C                   KFLD                    d53fgs
746200140609     C                   KFLD                    vnmdoc6           6 0
746300140609     C                   KFLD                    w53pdc
746400140609     C                   KFLD                    V2CAAS
746500140609     C                   KFLD                    V2CLNP
746600140609     C                   KFLD                    V2CNRS
746700140609     C                   KFLD                    V2CNSP
746800140609     C     Kevbric       KLIST
746900131121     C                   KFLD                    evbAAS
747000131121     C                   KFLD                    evbLNP
747100131121     C                   KFLD                    evbNRS
747200131121     C                   KFLD                    evbNSP
747300131121     C                   KFLD                    wevbric
747400131121     C                   move      'RIC'         wevbric           3
747500941123     C     KARB1         KLIST
747600941123     C                   KFLD                    KAAS
747700941123     C                   KFLD                    KLNP
747800941123     C                   KFLD                    KNRS
747900941123     C                   KFLD                    KNSP
748000990924     C*
748100990924     C     KARI          KLIST
748200990924     C                   KFLD                    KAAS
748300990924     C                   KFLD                    KLNP
748400990924     C                   KFLD                    KNRS
748500990924     C                   KFLD                    KNSP
748600990924     C                   KFLD                    KTIP
748700990924     C                   KFLD                    KDIV
748800941123     C*
748900941123     C     KABI1         KLIST
749000941123     C                   KFLD                    KABI
749100941123     C                   KFLD                    KCAB
749200941123     C*
749300941123     C     KART1         KLIST
749400941129     C                   KFLD                    KAAS1
749500941129     C                   KFLD                    KLNP1
749600941129     C                   KFLD                    KNRS1
749700941129     C                   KFLD                    KNSP1
749800941129     C*
749900941129     C     KART2         KLIST
750000941129     C                   KFLD                    KAAS1
750100941129     C                   KFLD                    KLNP1
750200941129     C                   KFLD                    KNRS1
750300941129     C                   KFLD                    KNSP1
750400941130     C                   KFLD                    KNSC
750500941123     C*
750600941123     C     KGCA          KLIST
750700941123     C                   KFLD                    KAAS
750800941123     C                   KFLD                    KLNP
750900941123     C                   KFLD                    KNRS
751000941123     C                   KFLD                    KNSP
751100941123     C*
751200941123     C     KARB2         KLIST
751300941123     C                   KFLD                    KAAS2
751400941123     C                   KFLD                    KLNP2
751500941123     C                   KFLD                    KNRS2
751600941123     C                   KFLD                    KNSP2
751700941123     C*
751800941123     C     KAR4          KLIST
751900941123     C                   KFLD                    KAAS
752000941123     C                   KFLD                    KLNP
752100941123     C                   KFLD                    KNRS
752200941123     C                   KFLD                    KNSP
752300941123     C                   KFLD                    KTRC
752400941123     C*
752500941123     C     KFVV          KLIST
752600941123     C                   KFLD                    KNPG
752700941123     C                   KFLD                    KNFV
752800020902     C                   KFLD                    kfgs
752900070316     C     Kdst          KLIST
753000070410     C                   KFLD                    kfgs
753100070316     C                   KFLD                    KNPG
753200070316     C                   KFLD                    KNFV6             6 0
753300120305     C     Kdst4         KLIST
753400120305     C                   KFLD                    $cssfgs
753500120615     C                   KFLD                    NDCI
753600120312     C     Kdst41        KLIST
753700120312     C                   KFLD                    $cssfgs
753800120615     C                   KFLD                    NDCI
753900120312     C                   KFLD                    exopz
754000941229     C*
754100941123     C                   Z-ADD     4             KNPG
754200941123     C*
754300941123     C     KTAB1         KLIST
754400941123     C                   KFLD                    KKUT
754500941123     C                   KFLD                    KCOD
754600941123     C*
754700941123     C     KTAB2         KLIST
754800941123     C                   KFLD                    KKUT
754900941123     C                   KFLD                    KCOD
755000941123     C                   KFLD                    KKEY
755100990923     C*
755200990923     C     KTBE          KLIST
755300990924     C                   KFLD                    KCOD1
755400990923     C                   KFLD                    KKE1
755500941123     C*
755600080709     C     KTBE2         KLIST
755700080709     C                   KFLD                    KCOD1
755800080709     C                   KFLD                    KKE1
755900080709     C                   KFLD                    KKE2
756000080709     C*
756100941123     C     KFTT          KLIST
756200941123     C                   KFLD                    KPDR
756300941123     C                   KFLD                    KTSR
756400941123     C                   KFLD                    KNDC
756500941123     C*
756600941123     C     KFTD          KLIST
756700941123     C                   KFLD                    KAAS
756800941123     C                   KFLD                    KLNP
756900941123     C                   KFLD                    KNRS
757000941123     C                   KFLD                    KNSP
757100950113     C                   KFLD                    KTSR
757200941214     C                   KFLD                    KPDR
757300941214     C                   KFLD                    KNDC
757400941123     C*
757500941123     C     KANM          KLIST
757600941123     C                   KFLD                    KFLS
757700941123     C                   KFLD                    KLNA
757800941130     C                   KFLD                    KNRS1
757900941123     C                   KFLD                    KSCN
758000950915     C     KANM1         KLIST
758100990519     C                   KFLD                    KAA4
758200950915     C                   KFLD                    KLNP
758300950915     C                   KFLD                    KNRS
758400950915     C                   KFLD                    KNSP
758500950915     C                   KFLD                    KCAA
758600941123     C*
758700941123     C     KEVB2         KLIST
758800990803     C                   KFLD                    KAAS
758900941123     C                   KFLD                    KLNP
759000941123     C                   KFLD                    KNRS
759100941123     C                   KFLD                    KNSP
759200941123     C*
759300941123     C     KEVB1         KLIST
759400990803     C                   KFLD                    KAAS
759500941123     C                   KFLD                    KLNP
759600941123     C                   KFLD                    KNRS
759700941123     C                   KFLD                    KNSP
759800941123     C                   KFLD                    KDEV
759900941123     C                   KFLD                    KHEV
760000090622     C*
760100090622     C     kevbpar       KLIST
760200090622     C                   KFLD                    KAAS1
760300090622     C                   KFLD                    KLNP1
760400090622     C                   KFLD                    KNRS1
760500090622     C                   KFLD                    KNSP1
760600090622     C                   KFLD                    ARBDCM
760700090622     C                   KFLD                    ARBHMC
760800090622     C*
760900090622     C     kevbpar2      KLIST
761000090622     C                   KFLD                    KAAS1
761100090622     C                   KFLD                    KLNP1
761200090622     C                   KFLD                    KNRS1
761300090622     C                   KFLD                    KNSP1
761400090622      *
761500980402     C     KARS0         KLIST
761600980402     C                   KFLD                    KFLS
761700980402     C                   KFLD                    KLNA
761800980402     C                   KFLD                    KNRS1
761900980402     C                   KFLD                    KNSC
762000980402     C                   KFLD                    KTRC
762100060323      *
762200060323     C     Kass          KLIST
762300060323     C                   KFLD                    incfle
762400060323     C                   KFLD                    inclnp
762500060323     C                   KFLD                    incaas
762600060323     C                   KFLD                    incnrs
762700060323     C                   KFLD                    incnsp
762800980402      *
762900980402     C     KCDS0         KLIST
763000980402     C                   KFLD                    KNDC
763100980402     C                   KFLD                    KAAS1
763200980402     C                   KFLD                    KLNP1
763300980402     C                   KFLD                    KNRS1
763400980402     C                   KFLD                    KNSP1
763500030318     c* quadratura giornaliera
763600030318     C     Kqig          KLIST
763700030318     C                   KFLD                    qigann
763800030318     C                   KFLD                    dutpou
763900030318     C                   KFLD                    oggi
764000030318     c                   clear                   qigann
764100050516     C     Kapd          KLIST
764200050516     C                   KFLD                    apdtip
764300050516     C                   KFLD                    w53pdc            7 0
764400050516     c                   movel     'A'           apdtip
764500070309     C     Karp          KLIST
764600070309     C                   KFLD                    v2cAAS
764700070309     C                   KFLD                    v2cLNP
764800070309     C                   KFLD                    v2cNRS
764900070309     C                   KFLD                    v2cNSP
765000071106     C     Kvarb1        KLIST
765100071106     C                   KFLD                    v2cAAS
765200071106     C                   KFLD                    v2cLNP
765300071106     C                   KFLD                    v2cNRS
765400071106     C                   KFLD                    v2cNSP
765500071026     C     Kvarb         KLIST
765600071026     C                   KFLD                    d48AAS
765700071026     C                   KFLD                    d48LNP
765800071026     C                   KFLD                    d48NRS
765900071026     C                   KFLD                    d48NSP
766000071030     C     Kvar          KLIST
766100071030     C                   KFLD                    d48AAS
766200071030     C                   KFLD                    d48LNP
766300071030     C                   KFLD                    d48NRS
766400071030     C                   KFLD                    d48NSP
766500071026     C     Kwf           KLIST
766600071026     C                   KFLD                    varbAAS
766700071026     C                   KFLD                    varbLNP
766800071026     C                   KFLD                    varbNRS
766900071026     C                   KFLD                    varbNSP
767000071026     C                   KFLD                    varbdim
767100090804     C     Kwf1          KLIST
767200090804     C                   KFLD                    varbAAS
767300090804     C                   KFLD                    varbLNP
767400090804     C                   KFLD                    varbNRS
767500090804     C                   KFLD                    varbNSP
767600080714     C     Kbrv          KLIST
767700080714     C                   KFLD                    NPG8
767800080714     C                   KFLD                    wdisbrv           6 0
767900100128     C                   KFLD                    d53fgs
768000080714     C                   KFLD                    artfls
768100080714     C                   KFLD                    artlna
768200080714     C                   KFLD                    artnrs
768300080714     C                   KFLD                    artnsc
768400080714     C                   z-add     8             NPG8              1 0
768500080715     C     Kdsf          KLIST
768600080715     C                   KFLD                    NPG4              1 0
768700080715     C                   KFLD                    wDSFndc           6 0
768800100118     C                   KFLD                    d53fgs
768900080715     C                   KFLD                    fase              3
769000080715     C                   move      'CRI'         fase
769100080715     C                   z-add     4             NPG4
769200080723     C     Kced          KLIST
769300100128     C                   KFLD                    d53fgs
769400080805     C                   KFLD                    wdisbrv
769500101115     C                   KFLD                    w53pdc
769600080805     C                   KFLD                    TRDCED
769700080723     C                   KFLD                    artAAS
769800080723     C                   KFLD                    artLNP
769900080723     C                   KFLD                    artNRS
770000080723     C                   KFLD                    artNSP
770100080723     C                   KFLD                    artNSC
770200080723     C                   move      'CED'         trdced            3
770300141212ab   C*
770400141212ab   c     Key_TGD11     klist
770500141212ab   c                   kfld                    tgDTOR
770600141212ab   c                   kfld                    tgDOGG
770700141212ab   c                   kfld                    tgDMAD
770800141212ab    *
770900141212ab   c     Key_TGR01     klist
771000141212ab   c                   kfld                    tgRANT
771100141212ab   c                   kfld                    tgRNUT
771200141212ab   c                   kfld                    tgRPDT
771300141212ab   c                   kfld                    tgRTRI
771400941123     C*----------------------------------------------------*
771500941123     C*  Definizione delle variabili
771600000526     C     *LIKE         DEFINE    FVVDS         FVVDS2
771700980402     C     *LIKE         DEFINE    ARBAAS        KAAS2
771800941123     C     *LIKE         DEFINE    ARBLNP        KLNP2
771900941123     C     *LIKE         DEFINE    ARBNRS        KNRS2
772000941123     C     *LIKE         DEFINE    ARBNSP        KNSP2
772100990924     C     *LIKE         DEFINE    ARITIP        KTIP
772200990924     C     *LIKE         DEFINE    ARIDIV        KDIV
772300941129     C     *LIKE         DEFINE    ARBAAS        KAAS1
772400941129     C     *LIKE         DEFINE    ARBLNP        KLNP1
772500941129     C     *LIKE         DEFINE    ARBNRS        KNRS1
772600941129     C     *LIKE         DEFINE    ARBNSP        KNSP1
772700941123     C     *LIKE         DEFINE    ARBAAS        KAAS
772800941123     C     *LIKE         DEFINE    ARBLNP        KLNP
772900941123     C     *LIKE         DEFINE    ARBNRS        KNRS
773000941123     C     *LIKE         DEFINE    ARBNSP        KNSP
773100941123     C     *LIKE         DEFINE    AR4TRC        KTRC
773200941123     C     *LIKE         DEFINE    ARBAAS        KAAP
773300941123     C     *LIKE         DEFINE    ARBLNP        KLPP
773400941123     C     *LIKE         DEFINE    ARBNRS        KNRP
773500941123     C     *LIKE         DEFINE    ARBVLF        WVOL
773600021107     C     *like         define    ARBVLF        wvolparz
773700021107     C     *like         define    arbpkF        wpes
773800021107     C     *like         define    arbpkF        wpesparz
773900941123     C     *LIKE         DEFINE    FVVNPG        KNPG
774000941123     C     *LIKE         DEFINE    FVVNFV        KNFV
774100020902     C     *LIKE         DEFINE    FVVfgs        Kfgs
774200941123     C     *LIKE         DEFINE    ABIABI        KABI
774300941123     C     *LIKE         DEFINE    ABICAB        KCAB
774400941123     C     *LIKE         DEFINE    TBLKUT        KKUT
774500941123     C     *LIKE         DEFINE    TBLCOD        KCOD
774600941123     C     *LIKE         DEFINE    TBLKEY        KKEY
774700990923     C     *LIKE         DEFINE    TBEKE1        KKE1
774800080709     C     *LIKE         DEFINE    TBEKE2        KKE2
774900941123     C     *LIKE         DEFINE    TBLKUT        §KUT
775000941123     C     *LIKE         DEFINE    TBLCOD        §COD
775100990924     C     *LIKE         DEFINE    TBECOD        KCOD1
775200941123     C     *LIKE         DEFINE    EVBDEV        KDEV
775300941123     C     *LIKE         DEFINE    EVBHEV        KHEV
775400941123     C     *LIKE         DEFINE    ANMFLS        KFLS
775500950918     C     *LIKE         DEFINE    ANMCAA        KCAA
775600950620     C     *LIKE         DEFINE    ANMLNA        KLNA
775700941123     C     *LIKE         DEFINE    ANMSCN        KSCN
775800941129     C     *LIKE         DEFINE    ARTNSC        KNSC
775900990519     C     *LIKE         DEFINE    ANMAAS        KAA4
776000941123     C     *LIKE         DEFINE    FTTPDR        KPDR
776100941123     C     *LIKE         DEFINE    FTTNDC        KNDC
776200941123     C     *LIKE         DEFINE    FTTTSR        KTSR
776300970926     C     *LIKE         DEFINE    FTTSET        WSET
776400941123     C     *LIKE         DEFINE    NRR6          CONT
776500941123     C     *LIKE         DEFINE    NRR6          TOTCAR
776600941123     C     *LIKE         DEFINE    NRR6          KJ
776700941123     C     *LIKE         DEFINE    NRR6          TOTSFH
776800941123     C     *LIKE         DEFINE    NRR6          POSIZ
776900941123     C     *LIKE         DEFINE    NRR6          SAVREC
777000941123     C     *LIKE         DEFINE    NRR6          SAVNRR
777100941123     C     *LIKE         DEFINE    NRR6          NRR2
777200970926     C     *LIKE         DEFINE    NRR6          SAVNR2
777300941123     C     *LIKE         DEFINE    NRR6          NRR3
777400941123     C     *LIKE         DEFINE    ARBDCM        OGGI
777500941123     C     *LIKE         DEFINE    ARBDCM        WOGGI
777600941123     C     *LIKE         DEFINE    ARBDCM        DATCON
777700941123     C     *LIKE         DEFINE    ARBDCM        DTCONS
777800941123     C     *LIKE         DEFINE    ARBDCM        DTSPED
777900941123     C     *LIKE         DEFINE    ARBDCM        WDTGIA
778000941123     C     *LIKE         DEFINE    ARBDCM        WDTASS
778100941123     C     *LIKE         DEFINE    ARBDCM        WDTAVV
778200941123     C     *LIKE         DEFINE    ARBNDC        WSVNDC
778300941123     C     *LIKE         DEFINE    ARBICC        COMICC
778400941128     C     *LIKE         DEFINE    ARBAAS        ANNO
778500941123     C     *LIKE         DEFINE    V2CNT1        NOTA
778600941123     C     *LIKE         DEFINE    V02DT1        DAT4
778700941123     C     *LIKE         DEFINE    §2ZTIC        WTPGIA
778800941123     C     *LIKE         DEFINE    ARBPKF        SAVPKG
778900941123     C     *LIKE         DEFINE    ARBNCL        SAVNCL
779000941123     C     *LIKE         DEFINE    ARBVLF        SAVVOL
779100020829     C     *LIKE         DEFINE    ARBifp        COMifp
779200031209     C     *LIKE         DEFINE    ARBpdc        COMpdc
779300020829     C     *LIKE         DEFINE    ARBNDC        COMNDC
779400941123     C     *LIKE         DEFINE    ARBDCM        COMDCM
779500990311     C     *LIKE         DEFINE    ARBDCP        COMDCP
779600941123     C     *LIKE         DEFINE    ARBHMC        COMHMC
779700941123     C     *LIKE         DEFINE    ARBDDC        COMDDC
779800941123     C     *LIKE         DEFINE    V3CTIC        WSVTIC
779900941123     C     *LIKE         DEFINE    V02AS1        WSVAS1
780000941123     C     *LIKE         DEFINE    V02AB1        WSVAB1
780100941123     C     *LIKE         DEFINE    V02CA1        WSVCA1
780200941123     C     *LIKE         DEFINE    V02DT1        WSVDT1
780300941123     C     *LIKE         DEFINE    ARBNDC        STPNDC
780400941123     C     *LIKE         DEFINE    ARBDDC        STPDDC
780500941123     C     *LIKE         DEFINE    ARBPDC        STPPDR
780600941123     C     *LIKE         DEFINE    D53DSF        DESPAD
780700941123     C     *LIKE         DEFINE    ARBLNP        STPLNP
780800941123     C     *LIKE         DEFINE    ARBAAS        STPAAS
780900941123     C     *LIKE         DEFINE    ARBNRS        STPNRS
781000941123     C     *LIKE         DEFINE    ARBNSP        STPNSP
781100941123     C     *LIKE         DEFINE    ARBMGS        STPGMS
781200941123     C     *LIKE         DEFINE    ARBCMC        STPCMC
781300941123     C     *LIKE         DEFINE    ARBTMC        STPTMC
781400941123     C     *LIKE         DEFINE    ARBNCL        STPNCL
781500941123     C     *LIKE         DEFINE    ARBPKF        STPPKG
781600941123     C     *LIKE         DEFINE    ARBVLF        STPVOL
781700941123     C     *LIKE         DEFINE    V02CPZ        STPCPZ
781800941123     C     *LIKE         DEFINE    V02RIS        STPRIS
781900941123     C     *LIKE         DEFINE    ABIABI        STPABI
782000941123     C     *LIKE         DEFINE    ABICAB        STPCAB
782100941123     C     *LIKE         DEFINE    V3CIMP        STPIMP
782200941123     C     *LIKE         DEFINE    V3CVCA        STPVAL
782300941123     C     *LIKE         DEFINE    ARBNCL        STPTCL
782400941123     C     *LIKE         DEFINE    ARBPKF        STPTPK
782500941123     C     *LIKE         DEFINE    ARBVLF        STPTVL
782600941123     C     *LIKE         DEFINE    ARBSTP        STPTST
782700000505     C     *LIKE         DEFINE    V05ADR        STPADR
782800000505     C     *LIKE         DEFINE    V05NDR        STPNDR
782900941123     C     *LIKE         DEFINE    ARBNCL        STPTNC
783000941123     C     *LIKE         DEFINE    ARBPKF        STPTNP
783100941123     C     *LIKE         DEFINE    ARBVLF        STPTNV
783200941123     C     *LIKE         DEFINE    V05TSC        STPTSC
783300941123     C     *LIKE         DEFINE    V05TSC        STPTPA
783400941123     C     *LIKE         DEFINE    V05TSC        STPNSC
783500941123     C     *LIKE         DEFINE    V05TSC        STPNFA
783600941123     C     *LIKE         DEFINE    V05TSC        STPNGC
783700941123     C     *LIKE         DEFINE    V05TSC        STPNLA
783800991007     C     *LIKE         DEFINE    V05TCE        STPEES
783900990924     C     *LIKE         DEFINE    V05TCE        STPECO
784000990924     C     *LIKE         DEFINE    V05TAE        STPEAN
784100991007     C     *LIKE         DEFINE    V05TAV        STPDAD
784200991007     C     *LIKE         DEFINE    V05TDE        STPEAD
784300990924     C     *LIKE         DEFINE    D53WRT        CNTPDR
784400941207     C     *LIKE         DEFINE    V3CAAS        WSVAAS
784500941207     C     *LIKE         DEFINE    V3CLNP        WSVLNP
784600941207     C     *LIKE         DEFINE    V3CNRS        WSVNRS
784700941207     C     *LIKE         DEFINE    V3CNSP        WSVNSP
784800991007     C     *LIKE         DEFINE    V3CNSP        WTOTSP
784900991007     C     *LIKE         DEFINE    V3CIMP        WASS
785000991007     C     *LIKE         DEFINE    V3CIMP        WCAS
785100991007     C     *LIKE         DEFINE    V3CIMP        WTOT
785200991007     C     *LIKE         DEFINE    V3CIMP        WIMP
785300991007     C     *LIKE         DEFINE    V3CVCA        WDIV
785400991007     C     *LIKE         DEFINE    V3CVCA        WVCA
785500991007     C     *LIKE         DEFINE    V3CVCA        WVTO
785600991007     C     *LIKE         DEFINE    V3CVCA        WVAL
785700991007     C     *LIKE         DEFINE    V3CTIC        WTCA
785800991007     C     *LIKE         DEFINE    V3CTIC        WTIC
785900991007     C     *LIKE         DEFINE    V3CTIC        WTTO
786000991007     C     *LIKE         DEFINE    §1AFCA        WFCA
786100991007     C     *LIKE         DEFINE    §1AFCA        WFCC
786200991007     C     *LIKE         DEFINE    §1AFCA        WFCT
786300991007     C     *LIKE         DEFINE    §1AFCA        WCON
786400991007     C     *LIKE         DEFINE    §1AFMB        WFMA
786500991007     C     *LIKE         DEFINE    §1AFMB        WFMC
786600991007     C     *LIKE         DEFINE    §1AFMB        WFMT
786700991007     C     *LIKE         DEFINE    §1AFMB        WMIT
786800991007     C     *LIKE         DEFINE    V02SN1        WSNA
786900991007     C     *LIKE         DEFINE    V02SN1        WSNC
787000991007     C     *LIKE         DEFINE    V02SN1        WSNT
787100941123     C*----------------------------------------------------*
787200941123     C* Giro data del giorno
787300941123     C                   TIME                    WHHDAT           14 0
787400941123     C                   MOVEL     WHHDAT        TIMES             6 0
787500941123     C                   MOVE      WHHDAT        G02DAT
787600941123     C                   MOVEL     *BLANK        G02ERR
787700941123     C                   CALL      'XSRDA8'
787800941123     C                   PARM                    WLBDA8
787900941123     C                   Z-ADD     G02INV        OGGI
788000941123     C                   Z-ADD     G02DAT        WOGGI
788100950206     C                   MOVEL     G02DAT        WOGGI6            6 0
788200950206     C                   MOVE      G02DAT        AA2               2 0
788300950206     C                   MOVE      AA2           WOGGI6
788400941123     C*----------------------------------------------------*
788500941123     C                   Z-ADD     1             KKUT
788600941123     C*----------------------------------------------------*
788700990923     C*  tabelle GED x reperimento divisa corrente
788800000211     C                   MOVEL     'GED'         KCOD1
788900990923     C                   MOVEL     *BLANKS       KKE1
789000990923     C                   MOVEL     '1'           KKE1
789100990923     C     KTBE          CHAIN     TNTBE01L                           96
789200990923     C     *IN96         IFEQ      '0'
789300990924     C                   MOVEL     TBEUNI        DGED
789400990923     C                   END
789500941123     C*  Caricamento tabelle
789600941123     C                   Z-ADD     0             X                 3 0
789700941123     C                   MOVEL     'CV'          KCOD
789800941123     C     KTAB1         CHAIN     TABEL00F                           96
789900941123     C     *IN96         DOWEQ     '0'
790000080903     C     X             ANDLT     200
790100941123     C     TBLFLG        IFEQ      ' '
790200941123     C                   ADD       1             X
790300941123     C                   MOVEL     TBLKEY        CCV(X)
790400941123     C                   MOVEL     TBLUNI        DSCV
790500990923     C                   MOVEL     §CVDVA        FLV(X)
790600941123     C                   END
790700941123     C     KTAB1         READE     TABEL00F                               96
790800941123     C                   END
790900951019     C     *IN96         IFEQ      '0'
791000000211     C                   MOVEL     MSG(1)        DSMSG
791100000211     C                   MOVEL     KCOD          KCODM
791200000211     C                   MOVEL     DSMSG         MSG(1)
791300100825     c                   exsr      sndmail
791400951019     C                   END
791500941123     C*
791600000505     C                   CLEAR                   DS1A
791700000505     C                   Z-ADD     0             Z                 3 0
791800941123     C                   Z-ADD     0             X                 3 0
791900941123     C                   MOVEL     '1A'          KCOD
792000941123     C     KTAB1         CHAIN     TABEL00F                           96
792100941123     C     *IN96         DOWEQ     '0'
792200080903     C     X             ANDLT     200
792300941123     C     TBLFLG        IFEQ      ' '
792400000505     C                   MOVEL     TBLUNI        DS1A
792500941123     C                   ADD       1             X
792600941123     C                   MOVEL     TBLKEY        C1A(X)
792700941123     C                   MOVEL     TBLUNI        D1A(X)
792800941123     C                   END
792900941123     C     KTAB1         READE     TABEL00F                               96
793000941123     C                   END
793100951019     C     *IN96         IFEQ      '0'
793200000211     C                   MOVEL     MSG(1)        DSMSG
793300000211     C                   MOVEL     KCOD          KCODM
793400000211     C                   MOVEL     DSMSG         MSG(1)
793500100825     c                   exsr      sndmail
793600951019     C                   END
793700941123     C*
793800941123     C                   Z-ADD     0             X                 3 0
793900941123     C                   MOVEL     '2A'          KCOD
794000941123     C     KTAB1         CHAIN     TABEL00F                           96
794100941123     C     *IN96         DOWEQ     '0'
794200080903     C     X             ANDLT     200
794300941123     C     TBLFLG        IFEQ      ' '
794400941123     C                   ADD       1             X
794500941123     C                   MOVEL     TBLKEY        C2A(X)
794600941123     C                   MOVEL     TBLUNI        D2A(X)
794700941123     C                   END
794800951019     C     KTAB1         READE     TABEL00F                               96
794900941123     C                   END
795000951019     C     *IN96         IFEQ      '0'
795100000211     C                   MOVEL     MSG(1)        DSMSG
795200000211     C                   MOVEL     KCOD          KCODM
795300000211     C                   MOVEL     DSMSG         MSG(1)
795400100825     c                   exsr      sndmail
795500951019     C                   END
795600941123     C*
795700941123     C                   MOVEL     '2Z'          KCOD
795800941123     C     KTAB1         CHAIN     TABEL00F                           96
795900941123     C     *IN96         DOWEQ     '0'
796000941123     C     TBLFLG        IFEQ      ' '
796100941123     C                   MOVEL     TBLKEY        COD2Z             3
796200941123     C                   Z-ADD     1             X
796300941123     C     COD2Z         LOOKUP    C2A(X)                                 95
796400941123     C     *IN95         IFEQ      '1'
796500941124     C                   MOVEL     TBLUNI        D2Z(X)
796600941123     C                   END
796700941123     C                   END
796800941123     C     KTAB1         READE     TABEL00F                               96
796900941123     C                   END
797000951019     C     *IN96         IFEQ      '0'
797100000211     C                   MOVEL     MSG(1)        DSMSG
797200000211     C                   MOVEL     KCOD          KCODM
797300000211     C                   MOVEL     DSMSG         MSG(1)
797400100825     c                   exsr      sndmail
797500951019     C                   END
797600941123     C*
797700941123     C                   Z-ADD     0             X                 3 0
797800941123     C                   MOVEL     '3A'          KCOD
797900941123     C     KTAB1         CHAIN     TABEL00F                           96
798000941123     C     *IN96         DOWEQ     '0'
798100080903     C     X             ANDLT     200
798200941123     C     TBLFLG        IFEQ      ' '
798300941123     C                   ADD       1             X
798400941123     C                   MOVEL     TBLKEY        C3A(X)
798500941123     C                   MOVEL     TBLUNI        D3A(X)
798600941123     C                   END
798700941123     C     KTAB1         READE     TABEL00F                               96
798800941123     C                   END
798900951019     C     *IN96         IFEQ      '0'
799000000211     C                   MOVEL     MSG(1)        DSMSG
799100000211     C                   MOVEL     KCOD          KCODM
799200000211     C                   MOVEL     DSMSG         MSG(1)
799300100825     c                   exsr      sndmail
799400951019     C                   END
799500941123     C*
799600941123     C                   Z-ADD     0             X                 3 0
799700941123     C                   MOVEL     '7R'          KCOD
799800941123     C     KTAB1         CHAIN     TABEL00F                           96
799900941123     C     *IN96         DOWEQ     '0'
800000080903     C     X             ANDLT     200
800100941123     C     TBLFLG        IFEQ      ' '
800200941123     C                   ADD       1             X
800300941123     C                   MOVEL     TBLKEY        C7R(X)
800400941123     C                   MOVEL     TBLUNI        D7R(X)
800500941123     C                   END
800600941123     C     KTAB1         READE     TABEL00F                               96
800700941123     C                   END
800800951019     C     *IN96         IFEQ      '0'
800900000211     C                   MOVEL     MSG(1)        DSMSG
801000000211     C                   MOVEL     KCOD          KCODM
801100000211     C                   MOVEL     DSMSG         MSG(1)
801200100825     c                   exsr      sndmail
801300951019     C                   END
801400941123     C*
801500000505     C                   Z-ADD     0             Z                 3 0
801600941123     C                   Z-ADD     0             X                 3 0
801700000505     C                   CLEAR                   DS7O
801800941123     C                   MOVEL     '7O'          KCOD
801900941123     C     KTAB1         CHAIN     TABEL00F                           96
802000941123     C     *IN96         DOWEQ     '0'
802100080903     C     X             ANDLT     200
802200941123     C     TBLFLG        IFEQ      ' '
802300000505     C                   MOVEL     TBLUNI        DS7O
802400000512     C     §7OFPT        IFEQ      'B'
802500000512     C     §7OFPT        OREQ      *BLANK
802600941123     C                   ADD       1             X
802700941123     C                   MOVEL     TBLKEY        C7O(X)
802800941123     C                   MOVEL     TBLUNI        D7O(X)
802900000512     C                   END
803000941123     C                   END
803100941123     C     KTAB1         READE     TABEL00F                               96
803200941123     C                   END
803300951019     C     *IN96         IFEQ      '0'
803400000211     C                   MOVEL     MSG(1)        DSMSG
803500000211     C                   MOVEL     KCOD          KCODM
803600000211     C                   MOVEL     DSMSG         MSG(1)
803700100825     c                   exsr      sndmail
803800951019     C                   END
803900941123     C*
804000941123     C                   Z-ADD     0             X                 3 0
804100000505     C                   Z-ADD     0             Z                 3 0
804200000505     C                   CLEAR                   DS1P
804300941123     C                   MOVEL     '1P'          KCOD
804400941123     C     KTAB1         CHAIN     TABEL00F                           96
804500941123     C     *IN96         DOWEQ     '0'
804600080903     C     X             ANDLT     200
804700941123     C     TBLFLG        IFEQ      ' '
804800000505     C                   MOVEL     TBLUNI        DS1P
804900000512     C     §1PUAR        IFEQ      'S'
805000941123     C                   ADD       1             X
805100941123     C                   MOVEL     TBLKEY        C1P(X)
805200941123     C                   MOVEL     TBLUNI        D1P(X)
805300000512     C                   END
805400941123     C                   END
805500941123     C     KTAB1         READE     TABEL00F                               96
805600941123     C                   END
805700951019     C     *IN96         IFEQ      '0'
805800000211     C                   MOVEL     MSG(1)        DSMSG
805900000211     C                   MOVEL     KCOD          KCODM
806000000211     C                   MOVEL     DSMSG         MSG(1)
806100100825     c                   exsr      sndmail
806200951019     C                   END
806300960227     C*
806400960227     C                   Z-ADD     0             X                 3 0
806500960227     C                   MOVEL     '7U'          KCOD
806600960227     C     KTAB1         CHAIN     TABEL00F                           96
806700960227     C     *IN96         DOWEQ     '0'
806800000211     C     X             ANDLT     500
806900960227     C     TBLFLG        IFEQ      ' '
807000050221     C     §7ustb        andeq     'S'
807100960227     C                   ADD       1             X
807200960227     C                   MOVEL     TBLUNI        DS7U
807300960228     C                   MOVEL     TBLKEY        WC7U              7
807400960228     C                   MOVEL     WC7U          C7U(X)
807500960227     C                   MOVEL     §7USTB        F7U(X)
807600960227     C                   END
807700960227     C     KTAB1         READE     TABEL00F                               96
807800960227     C                   END
807900960227     C     *IN96         IFEQ      '0'
808000000211     C                   MOVEL     MSG(1)        DSMSG
808100000211     C                   MOVEL     KCOD          KCODM
808200000211     C                   MOVEL     DSMSG         MSG(1)
808300100825     c                   exsr      sndmail
808400960227     C                   END
808500941123     C*
808600941123     C                   Z-ADD     0             X                 3 0
808700941123     C                   MOVEL     '7M'          KCOD
808800941123     C     KTAB1         CHAIN     TABEL00F                           96
808900941123     C     *IN96         DOWEQ     '0'
809000080903     C     X             ANDLT     200
809100941123     C     TBLFLG        IFEQ      ' '
809200941123     C                   ADD       1             X
809300941123     C                   MOVEL     TBLKEY        C7M(X)
809400941123     C                   END
809500941123     C     KTAB1         READE     TABEL00F                               96
809600941123     C                   END
809700951019     C     *IN96         IFEQ      '0'
809800000211     C                   MOVEL     MSG(1)        DSMSG
809900000211     C                   MOVEL     KCOD          KCODM
810000000211     C                   MOVEL     DSMSG         MSG(1)
810100100825     c                   exsr      sndmail
810200951019     C                   END
810300941123     C*
810400000211     C                   MOVEL     'GEI'         KCOD1
810500990923     C                   MOVEL     *BLANKS       KKE1
810600991007     C                   MOVEL     §GEDCR        KKE1
810700990923     C                   CLEAR                   DGEI
810800990924     C     KTBE          CHAIN     TNTBE01L                           96
810900941123     C     *IN96         IFEQ      '0'
811000990923     C                   MOVEL     TBEUNI        DGEI
811100941123     C                   END
811200941123     C*
811300011121     C*  tabelle GEDCN x reperimento divisa conteggi padroncini/tariffe
811400011121     C                   movel     'GED'         kcod1
811500011121     C                   movel     *blanks       kke1
811600011121     C                   movel     'CN'          kke1
811700011121     C                   clear                   dgedcn
811800011121     C     ktbe          chain     tntbe01l                           96
811900011121     C     *in96         ifeq      '0'
812000011121     C                   movel     tbeuni        dgedcn
812100011121     C                   end
812200020418     C*  tabelle dgd x reperimento limiti chiusura spedizioni
812300020418     C                   MOVEL     'DGD'         KCOD1
812400020418     C                   MOVEL     *BLANKS       KKE1
812500020418     C                   MOVEL     '1'           KKE1
812600020418     C     KTBE          CHAIN     TNTBE01L                           96
812700020418     C     *IN96         IFEQ      '0'
812800020418     C                   MOVEL     TBEUNI        DDGD
812900020418     C                   END
813000011121     C*
813100040330     C*tabelle vpo x reperimento limite massimo inserimento data cons.rich.
813200040330     C                   MOVEL     'VPO'         KCOD1
813300040330     C                   MOVEL     *BLANKS       KKE1
813400040330     C                   MOVEL     'VPO'         KKE1
813500040330     C     KTBE          CHAIN     TNTBE01L                           96
813600040330     C     *IN96         IFEQ      '0'
813700040330     C                   MOVEL     TBEUNI        Dvpo
813800040330     C                   END
813900040330     c                   move      oggi          dataiso
814000040330     c     dataiso       adddur    §vpodcr:*d    dataiso
814100040330     c                   move      dataiso       wvpodcr           8 0
814200040330     C*
814300941123     C                   ENDSR
814400100825      *----------------------------------------------------------------
814500100825     c     sndmail       begsr
814600100825      *----------------------------------------------------------------
814700120305     c                   eval      emlto = 'CedAlert@brt.it'
814800100825     c                   eval      emlcc = *blank
814900100825     c                   eval      emogg = 'ATTENZIONE Chiusura distinta '
815000100825     C                   eval      emmsg = msg(1)  + ' ' +
815100100825     c                             msg(2) + ' ' + msg(3)
815200100825     C                   CALL      'TIS701C1'
815300100825     C                   PARM                    emlto           253
815400100825     C                   PARM                    emlcc           253
815500100825     C                   PARM                    emogg            44
815600100825     C                   PARM                    emmsg          5000
815700100825     C                   PARM                    emesito           1
815800100825      *
815900100825     C                   ENDSR
816000941121     C*-----------------------------------------------------
816100941121     OQSYSPRT   E            TESTA            01
816200941121     O                       STT(1)              66
816300991115     O                       STT(2)             132
816400021016     O                       rsut                20
816500021016     O                                           21 '/'
816600941213     O                       D53LNA              24
816700941130     O                       WOGGI6             123 '  /  /  '
816800941121     O                       PAGE          Z    132
816900941130     O          E            TESTA            02
817000941130     O                       TIMES              123 '  .  .  '
817100941130     O          E            TESTA            03
817200941130     O                                           10 'DISTINTA: '
817300941121     O                       STPNDC        Z     18
817400941121     O                                         +  1 'DEL'
817500941122     O                       STPDDC            +  1 '  /  /    '
817600960607     O                                         +  1 'AUTOTRASPORTATORE:'
817700941130     O                       STPPDR        Z   +  1
817800941121     O                       DESPAD            +  2
817900941227     O                                         +  2 'TIPO PRESTAZIONE:'
818000941227     O                       TPPRE             +  1
818100031008     O          E    73      TESTA       1
818200031008     O                                           10 'DISTINTA: '
818300031008     O                       ndc(2)        Z     18
818400031008     O                                         +  1 'DEL'
818500031008     O                       dtd(2)            +  1 '  /  /    '
818600031008     O                                         +  1 '                  '
818700031008     O                       STPPDR        Z   +  1
818800031008     O                       DESPAD            +  2
818900031008     O                                         +  2 '                 '
819000031013     O                       tppre2            +  1
819100031008     O          E    76      TESTA       1
819200031008     O                                           10 'DISTINTA: '
819300031008     O                       ndc(3)        Z     18
819400031008     O                                         +  1 'DEL'
819500031008     O                       dtd(3)            +  1 '  /  /    '
819600031008     O                                         +  1 '                  '
819700031008     O                       STPPDR        Z   +  1
819800031008     O                       DESPAD            +  2
819900031008     O                                         +  2 '                 '
820000031013     O                       tppre3            +  1
820100031008     O          E    77      TESTA       1
820200031008     O                                           10 'DISTINTA: '
820300031008     O                       ndc(4)        Z     18
820400031008     O                                         +  1 'DEL'
820500031008     O                       dtd(4)            +  1 '  /  /    '
820600031008     O                                         +  1 '                  '
820700031008     O                       STPPDR        Z   +  1
820800031008     O                       DESPAD            +  2
820900031008     O                                         +  2 '                 '
821000031013     O                       tppre4            +  1
821100031008     O          E    78      TESTA       1
821200031008     O                                           10 'DISTINTA: '
821300031008     O                       ndc(5)        Z     18
821400031008     O                                         +  1 'DEL'
821500031008     O                       dtd(5)            +  1 '  /  /    '
821600031008     O                                         +  1 '                  '
821700031008     O                       STPPDR        Z   +  1
821800031008     O                       DESPAD            +  2
821900031008     O                                         +  2 '                 '
822000031013     O                       tppre5            +  1
822100031008     O          E            TESTA       2
822200941121     O                       STT(3)              66
822300941121     O                       STT(4)             132
822400031008     O          E            TESTA       1  1
822500941121     O                       STT(5)              66
822600941121     O                       STT(6)             132
822700941130     O          E            DETCON         1
822800941206     O                       STPLNP               3
822900941206     O                       STPNRS        Z      6
823000941121     O                       STPNSP        Z     14
823100941121     O                       STPGMS              21 '  /  /'
823200941121     O                       STPAAS              25
823300941206     O                       STPCMC         B    29
823400941206     O                       STPTMC         B    31
823500941206     O                       STPDCO         B    52
823600941121     O                       STPRSD              82
823700941121     O                       STPNCL        Z    110
823800941121     O                       STPPKG        2    119
823900941121     O                       STPVOL        2    126
824000941121     O                       STPCPZ             129
824100941121     O                       STPRIS             132
824200941121     O          E            DETIMP         1
824300941202     O                       STPTIC              50
824400941202     O               04                          58 'Ass.Nr.'
824500941202     O                       STPNAS              62
824600941202     O               04                          71 'ABI/CAB'
824700941202     O                       STPABI        Z     76
824800941202     O               04                          77 '/'
824900941202     O                       STPCAB        Z     82
825000941121     O                       STPIMP        2    100
825100120314     O*              06                         100 '   '
825200991008     O                       STPVAL             104
825300941123     O          E            STAMPA           01
825400941121     O                       STT(1)              66
825500941121     O                       STT(2)             132
825600021016     O                       rsut                20
825700021016     O                                           21 '/'
825800941213     O                       D53LNA              24
825900941123     O                       WOGGI6             118 '  /  /  '
826000941121     O                       PAGE          Z    132
826100941121     O          E            STAMPA           02
826200941121     O                       TIMES              118 '  .  .  '
826300941121     O          E            TOTAL       2
826400960607     O                                           18 'AUTOTRASPORTATORE:'
826500941121     O                       VIDDES            +  1
826600941121     O                                         +  1 'OPER.:'
826700020826     O                       dutpou            +  1
826800020826     O                       dutcou            +  1
826900941121     O          E            TOTAL       1
827000941121     O                                           10 'DISTINTA: '
827100941121     O                                           34 'DATA CONS.'
827200941121     O                                           45 'ORA CONS.'
827300941121     O          E            TOTAL       1
827400941121     O                       NDC(1)        Z      8
827500941122     O                       DTD(1)              21 '  /  /    '
827600941122     O                       DDC(1)              32 '  /  /    '
827700941121     O                       HRC(1)              43 '  :  '
827800941121     O          E            TOTAL       1
827900941121     O                       NDC(2)        Z      8
828000941122     O                       DTD(2)              21 '  /  /    '
828100941122     O                       DDC(2)              32 '  /  /    '
828200941121     O                       HRC(2)              43 '  :  '
828300941121     O          E            TOTAL       1
828400941121     O                       NDC(3)        Z      8
828500941122     O                       DTD(3)              21 '  /  /    '
828600941122     O                       DDC(3)              32 '  /  /    '
828700941121     O                       HRC(3)              43 '  :  '
828800941121     O          E            TOTAL       1
828900941121     O                       NDC(4)        Z      8
829000941122     O                       DTD(4)              21 '  /  /    '
829100941122     O                       DDC(4)              32 '  /  /    '
829200941121     O                       HRC(4)              43 '  :  '
829300941121     O          E            TOTAL       1
829400941121     O                       NDC(5)        Z      8
829500941122     O                       DTD(5)              21 '  /  /    '
829600941122     O                       DDC(5)              32 '  /  /    '
829700941121     O                       HRC(5)              43 '  :  '
829800941121     O          E            TOTAL       1
829900941130     O                                           22 'SPEDIZ.CONSEGNATE....:'
830000941130     O                       STPTSC        Z     26
830100941130     O                                           44 'DI CUI PARZIALI.:'
830200941130     O                       STPTPA        Z     47
830300941121     O                                           89 'NR.COLLI:'
830400941121     O                       STPTCL        Z     94
830500941121     O                                          100 'PESO:'
830600941121     O                       STPTPK        3    108
830700941121     O                                          116 'VOLUME:'
830800941121     O                       STPTVL        3    122
830900941121     O                                          128 'STOP:'
831000941121     O                       STPTST        Z    132
831100000505     O          E            TOTAL       1
831200000508     O                                           22 'SPEDIZ.CON ADR CONS..:'
831300000505     O                       STPADR        Z     26
831400000508     O                                           54 'SPEDIZ.CON ADR NON CONS.'
831500000508     O                       STPNDR        Z     59
831600941130     O          E            TOTAL       1  1
831700941130     O                                           22 'SPEDIZ.NON CONSEGNATE:'
831800941130     O                       STPNSC        Z     26
831900941130     O                                           44 'DI CUI NON FATTE:'
832000941130     O                       STPNFA        Z     47
832100941121     O                                           57 'GIACENTI:'
832200941121     O                       STPNGC        Z     60
832300941121     O                                           75 'LASC.AVVISO:'
832400941121     O                       STPNLA        Z     78
832500941121     O                                           89 'NR.COLLI:'
832600941121     O                       STPTNC        Z     94
832700941121     O                                          100 'PESO:'
832800941122     O                       STPTNP        3    108
832900941121     O                                          116 'VOLUME:'
833000941122     O                       STPTNV        3    122
833100990924     O          E            TOTEU       1
833200991007     O                                           16 'TOT.INC.IN EURO:'
833300991007     O                       STPEES        2     30
833400060419     O                                           47 'DI CUI CON.+POS:'
833500991007     O                       STPECO        2     61
833600991007     O                                           69 'ASS.NS:'
833700991008     O                       STPEAN        2     83
833800991007     O                                           96 'ASS.DIRETTI:'
833900991008     O                       STPEAD        2    110
834000991007     O                                          121 'ALTRE DIV:'
834100991008     O                       STPDAD        2    132
834200011010** STT
834300941121                                    ***  C H I U S U R A     D I S
834400941130 T I N T A    C O N S E G N A  ***      FNLR53R           PAG.
834500941130LNP     SPEDIZIONE        COD. E DESCR. CONSEGNA     DESTINATARIO
834600941121                     INCASSATO         COLLI   PESO   VOLUME PRZ R
834700941121------------------------------------------------------------------
834800941121------------------------------------------------------------------
834900920221**  CMDP
835000940415OVRPRTF FILE(QSYSPRT) FORMTYPE(CHIU_DIS) SAVE(*YES) HOLD(*YES)
835100930305**  ERRORI
835200941116Codice mancata consegna inesistente                                    01
835300941116Codice mancata consegna non valido                                     02
835400941116Già effettuata consegna parziale impossibile aprire giacenza           03
835500941116Se merce non consegnata impossibile immettere consegna con anomalie !  04
835600941116Se merce non consegnata impossibile immettere consegna con riserva !   05
835700990923Importo incassato obbligatorio se modificata divisa incasso !          06
835800990423Attenzione ! Modificata mancata consegna                               07
835900991007Nelle righe precedenti presente tipo incasso non congruente con divisa 08
836000941116Cod.giacenza generica incompatibile con particolarità consegna         09
836100000616Codice consegna anomala inesistente o non utilizzabile                 10
836200941116Codice riserva inesistente o errato                                    11
836300941117Obbligatorio firmatario bolla                                          12
836400091221Ora consegna obbligatoria                                              13
836500941117Ora consegna errata !!                                                 14
836600990923Importo parziale incassato ammesso solo per incasso Assegnato          15
836700941117Spedizione non consegnata !! Impossibile immettere firmatario          16
836800941117Data consegna anteriore data spedizione                                17
836900941117Data consegna errata                                                   18
837000941117Attenzione ! Spedizione con data consegna inferiori alla data distinta 19
837100020418Data consegna inferiore oltre il limite ammesso rispetto a oggi        20
837200941117Data consegna superiore data del giorno                                21
837300941117Codice consegna particolare non utilizzabile                           22
837400000522Codice consegna particolare inesistente o non utilizzabile             23
837500941117Attenzione !! Variazione bolla non effettuata per conflitto            24
837600990928Incasso parziale ammesso solo per assegnato !                          25
837700990923Importo incassato superiore al totale da incassare                     26
837800990928Divisa di incasso inesistente                                          27
837900941117Immettere data documento rimborso                                      28
838000941117Data documento rimborso errata                                         29
838100941117Per immettere importo rimborso specificare causale relativa            30
838200941117Per immettere numero documento rimborso specificare causale relativa   31
838300941117Per immettere data documento rimborso specificare causale relativa     32
838400941117Per immettere note rimborso specificare causale relativa               33
838500941118Impossibile gestire incasso sulla riga di totale                       34
838600060419Impossibile immettere ABI/CAB per tipo incasso in contante o pos       35
838700060419Impossibile immettere numero assegno per tipo incasso contante o pos   36
838800060419Impossibile immettere data assegno per tipo incasso contante o pos     37
838900941118Tipo incasso inesistente                                               38
839000941118Tipo incasso valido solo per C/Assegno                                 39
839100990923Tipo incasso non utilizzabile con divisa indicata                      40
839200941118Cliente non abilitato alla gestione del tipo incasso indicato          41
839300120418Numero assegno obbligatorio o errato per il tipo incasso indicato      42
839400941118ABI/CAB Obbligatori                                                    43
839500941121Data assegno errata                                                    44
839600941118ABI/CAB Errato                                                         45
839700991115Imp. incassato valido solo x assegnati o C/A inc.in divisa alternativa 46
839800991007Differenza importi da incassare e incassati in contanti troppo alta !! 47
839900941121Incasso parziale superiore al importo totale assegnato !!              48
840000941121Immettere tipo incasso parziale  !!!                                   49
840100941121Tipo incasso parziale non ammesso !!                                   50
840200941121Tipo incasso parziale inesistente                                      51
840300150601Se tipo incasso C/A è TM/TO/TP, l'assegno deve essere postdatato       52
840400941122Lasciato avviso già emesso in precedenza impossibile reimetterlo       53
840500941125Giorno di chiusura non valido                                          54
840600941128Codice mancata consegna non gestibile da utente !!                     55
840700950208Collo senza data arrivo premi Invio poi F7 per forzare                 56
840800941128No consegnati tutti i colli o nessuno !                                57
840900941128Nessuna consegna parziale da annullare !                               58
841000150601Assegno postdatato utilizzabile solo con tipo incasso TM/TO/TP         59
841100971120Impossibile inserire assegni a noi intestati postdatati                60
841200110419Modificare esito incasso deve essere uguale a N oppure blank           61
841300941205Impossibile gestire consegna parziale: non ci sono colli da consegnare 62
841400950620Puoi ancora modificare la tassazione invece di chiedere N.Accredtito   63
841500970120Obbligatorio immettere descrizione motivo mancata consegna             64
841600970123Impossibile immettere note mancata consegna: spedizione già consegnata 65
841700970311Indicare evento consegna parziale con IDD                              66
841800970304Impossibile gestire pratica IDD: non ci sono colli da consegnare       67
841900970304Nessuna pratica IDD da annullare !                                     68
842000970304Indicare i colli da chiudere con IDD                                   69
842100970311Indicare codice evento IDD                                             70
842200970311Tutti i colli mancanti obbligatorio indicare IDD totale                71
842300140423Codice mancata consegna non valido incongruente con particolarità      72
842400970805Se merce consegnata parzialmente impossibile attivare pratica IDD      73
842500130408Incasso non destinato a mittente o divisa non gestita con multiassegno 74
842600120228Incasso già gestito come multiplo assegno o spedizione                 75
842700120228Incasso non effettuato impossibile gestire opzione                     76
842800141128Spedizione consegnabile solo con PINCODE da PDA !!!                    77
842900141201Codice evento non ammesso Spedizione SENZA il PINCODE !!!              78
843000100205                                                                       79
843100100205                                                                       80
843200100205                                                                       81
843300100205                                                                       82
843400131204Ora di consegna a video non può essere inferiore al primo tentativo    83
843500011010Ora consegna errata ! PER DISTINTA POMERIDIANA IL MINIMO SONO LE 13,00 84
843600011108ATTENZIONE !! LA DATA D'INCASSO E' OLTRE LA DATA SCADENZA DELLA DIVISA 85
843700011108LA SPEDIZIONE NON È INCASSATA I CAMPI DI INCASSO DEVONO ESSERE VUOTI   86
843800020610Non è possibile impostare il numero di STOP a (0) !!!!                 87
843900030318Non è possibile continuare. Esiste già quadratura giornaliera          88
844000031202Evento non inseribile prima di 2 tentativi di consegna                 89
844100040213Data consegna richiesta errata o non inserita                          90
844200040213Data consegna richiesta deve essere superiore alla data odierna        91
844300040330Data consegna richiesta superiore al limite ammesso                    92
844400051215Motivo immesso o proposto non valido la bolla ha Consegna TASSATIVA    93
844500060320Il tipo incasso POS è ammesso solo per consegne a magazziono           94
844600070313Data/ora evento reale errata o non inserita                            95
844700080204Ora/minuti evento errata verificare (08:00 - 19:59)                    96
844800070321Tipo nota non gestibile filiale non abilitata                          97
844900070412Data evento reale non può essere maggiore di oggi                      98
845000070412Data evento reale non può essere inferiore alla spedizione             99
845100070412Data evento reale non corretta fuori dai limiti aziendali              00
845200070517Attenzione è presente una data concordata con il destinatario          01
845300080709Manca Tabella Y4O x incasso POS chiamare la sede Uff.Banche            02
845400080717Incasso dichiarato in contante su PDA ma diverso da originale          03
845500080717Incasso dichiarato con assegno su PDA ma diverso da originale          04
845600090220Presente NOTA AUTISTA visualizzarla per proseguire                     05
845700090622Codice ABI 07601 non ammesso per tipo incasso "BP"                     06
845800091221Attenzione tipo servizio H10:30 orario oltre le 10:30 !!!              07
845900091221Attenzione tipo servizio Priority consegna fuori affidabilità !!!      08
846000130416Tipo incasso non utilizzabile variare il tipo incasso per proseguire   09
846100011010**
846200011010ERRORE IN CHIUSURA DISTINTA
846300011010**
846400011010ATTENZIONE ! La tabella XX utilizzata nella chiusura distinta ha r
846500011010aggiunto la capienza massima ! Correggere il programma FNLR53R inc
846600011010rementando la relativa schiera.

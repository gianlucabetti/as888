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
120800051215     c                   if        v2ctat = 'S'
120900051215     c                   select
121000051215?     * evento proposto da particolarità
121100051215     c                   when      v2cevp <> *blank
121200051215     c     v2cevp        lookup    c2a(tat)                               85
121300051215     c   85              movel     d2a(tat)      ds2atat
121400051215     c                   if        x§2aftc = 'A'
121500051215     C                   seton                                        4028
121600051215     C                   movel     err(93)       $msg
121700051215     c                   goto      aggrec
121800051215     c                   endif
121900051215?     * evento forzato legato in tabella 2z
122000051215     c                   when      w2zmdr <> *blank
122100051215     c     w2zmdr        lookup    c2a(tat)                               85
122200051215     c   85              movel     d2a(tat)      ds2atat
122300051215     c                   if        x§2aftc = 'A'
122400051215     C                   seton                                        4028
122500051215     C                   movel     err(93)       $msg
122600051215     c                   goto      aggrec
122700051215     c                   endif
122800051215?     * evento inserito a video
122900051215     c                   other
123000051215     c                   if        §2aftc = 'A'
123100051215     C                   seton                                        4028
123200051215     C                   movel     err(93)       $msg
123300051215     c                   goto      aggrec
123400051215     c                   endif
123500051215     c                   endsl
123600051215     c                   endif
123700051215?     *
123800040213      *data consegna richiesta forza evento se non c'è particolarità
123900100208     c                   if        w2zmdr <> *blank
124000040224     c                             and d53ita <>'E'
124100100208     c                   if        v2cevp = *blank
124200040213     c                   movel     w2zmdr        v2cevp
124300140423     c                   else
124400140423     c                   if        v02ccn = '016' and v2cevp <> 'A16' or
124500140423     c                             v02ccn = '023' and v2cevp <> 'A23'
124600140423     C                   seton                                        4028
124700140423     C                   movel     err(72)       $msg
124800140423     c                   goto      aggrec
124900140423     c                   endif
125000100208     c                   end
125100101015     c                   if        v2cevp = 'A16'
125200120810     c                   move      ' '           w13doil
125300101015     c                   move      ' '           v2ctcr
125400101015     c                   end
125500101015     c                   if        v2cevp = 'A23'
125600120810     c                   move      'D'           w13doil
125700101015     c                   move      'D'           v2ctcr
125800101015     c                   end
125900100308     c                   else
126000100308     c                   clear                   w2zmdr
126100100211     c                   endif
126200070405      * se dopo la particolarità o eventuali forzature eventi proposti per
126300070405      * consegna richiesta il codice evento proposto è ancora a blank forzo
126400070405      * il codice evento immesso a video per agganciare gestione estensione eventi
126500070405     c                   if        v2cevp = *blank
126600070405     c                   move      v02ccn        v2cevp
126700070405     c                   seton                                        70
126800070517      * abblenco anche campo per pilotare segnalazione eventuale FIARP
126900070517     c                   clear                   segnale           1
127000070405     c                   endif
127100101027mm+  c* se non consegnata e l'evento immesso prevede il tentativo di conseg.
127200101027mm+  c                   exsr      srctrora
127300101027mm+  C   47              GOTO      AGGREC
127400101015     c* se pda verifico se immesso evento con data cons. ric. solo la 1°
127500101015     c* volta
127600101015     c                   if        wpda = 'S' and w2zmdr <> ' '
127700101018     c                             and v2hevp = ' '
127800101015     c                   z-add     §PCTDCR       V2CDCR
127900101015     c                   z-add     §PCTHCR       V2CHCR
128000101015     c                   if        §pctcmc = '016'
128100101015     c                   move      ' '           v2ctcr
128200101015     c                   end
128300101015     c                   if        §pctcmc = '023'
128400101015     c                   move      'D'           v2ctcr
128500101015     c                   end
128600101015     c                   end
128700101015      * verifico se evento proposto abilita scrittura estensione evento
128800101015      * oppure se evento proposto forzato abilita reperimento DCR da FIARP o
128900101015      * da PDA
129000101015     C                   z-add     1             Yz                3 0
129100101015     C     V2cevp        lookup    c2a(Yz)                                95
129200101015     C                   if        *in95                                        --- 04 ---
129300101015     C                   movel     d2z(Yz)       ds2z
129400101015     C                   MOVEL     §2Zadr        W2zadr            1
129500101015     c                   else
129600101015     C                   clear                   W2zadr
129700101015     C                   endif                                                  --- 04 ---
129800070309      *data consegna richiesta da file FIARP x eventi abilitati
129900101015     c*                  clear                   v13tcrd
130000070518      *segnala se immesso evento che non gestisce fiarp ma esiste un record
130100070518      *riferito alla bolla
130200101015     c                   clear                   fiarpds
130300101015     c                   setoff                                       66
130400070313     c     karp          chain(n)  fiarp01l
130500070517     c                   if        %found(fiarp01l) and w2zadr = *blank
130600070517     c                             and d53ita <>'E' and segnale = *blank
130700070518     c                             and arpdcr > oggi
130800070517     c                   move      'X'           segnale
130900070517     c                   movel     err(101)      $msg
131000070517     C                   seton                                        4028
131100070517     c                   goto      aggrec
131200070517     c                   endif
131300070309     c                   if        %found(fiarp01l) and w2zadr <> *blank
131400070309     c                             and d53ita <>'E'
131500070309     c                   move      arpdcr        v2cdcr
131600101015     c                   move      arptcr        v2ctcr
131700101015     c                   move      arphcr        v2chcr
131800101015     c*                  move      arptcr        v13tcr
131900101015     c*                  move      arphcr        v13hcr
132000101015     c                   seton                                        66
132100071003     c                   eval      v2harp = '1'
132200070405      * imposta descrizione del flad x data consegna richiesta
132300101015     c*                  if        v13tcr = *blank
132400101015     c*                  move      '  Il'        v13tcrd
132500101015     c*                  else
132600101015     c*                  move      'Dopo'        v13tcrd
132700101015     c*                  endif
132800070405      *
132900070309     c                   else
133000101015     c*                  clear                   v13tcr
133100101015     c*                  clear                   v13hcr
133200070309     c                   clear                   w2zadr
133300070309     c                   endif
133400950113     C   28              GOTO      AGGREC
133500970122     C*  Se richiesto richiamo pgm x invio dei motivi di
133600970122     C*  mancata consegna
133700970122     C     V02SEL        IFEQ      'W'
133800970122     C                   MOVEL     'S'           D53VIS
133900970122     C                   EXSR      CALLR3
134000970122     C                   MOVEL     *BLANKS       V02SEL
134100970122     C     D53ERR        IFEQ      '1'
134200970122     C                   MOVEL     D53MSG        $MSG
134300970122     C                   SETON                                        28
134400970122     C                   GOTO      AGGREC
134500970122     C                   END
134600970122     C                   END
134700100205     c* se non consegnata e l'evento immesso prevede il tentativo di conseg.
134800101022     c*m                 exsr      srctrora
134900101022     C*m 47              GOTO      AGGREC
135000100205     c*********************************
135100100205     C*  MANCATA CONSEGNA, NO PARZIALE
135200100205     c*********************************
135300941123     C     §2ATIP        IFNE      'P'                                          --- 04 ---
135400941116     C*  Se c'è precedente consegna parziale non si può mettere giacenza
135500941116     C     §2AFTG        IFEQ      'S'
135600941116     C     V02PRZ        ANDEQ     'S'
135700941116     C                   SETON                                        4028
135800941116     C                   MOVEL     ERR(3)        $MSG
135900941116     C                   GOTO      AGGREC
136000941116     C                   END
136100941116     C*  Se non consegnata impossibile mettere codice consegna anomala
136200941116     C     V02CCA        IFNE      *BLANKS
136300941116     C                   SETON                                        404528
136400941116     C                   MOVEL     ERR(4)        $MSG
136500941116     C                   GOTO      AGGREC
136600941116     C                   END
136700941116     C*  Se non consegnata impossibile mettere riserva
136800941116     C     V02RIS        IFNE      *BLANKS
136900941116     C                   SETON                                        404328
137000941116     C                   MOVEL     ERR(5)        $MSG
137100941116     C                   GOTO      AGGREC
137200941116     C                   END
137300031202     C*  Se tentativi di consegna < a 2 non si può inserire l'evento 017
137400031202     C     V02ccn        IFeq      '017'
137500031202     C     V2cntc        andlt     2
137600031202     C                   SETON                                        4028
137700031202     C                   MOVEL     ERR(89)       $MSG
137800031202     C                   GOTO      AGGREC
137900031202     C                   END
138000941122     C*  Se immesso codice di lasc.avviso controllo che
138100941122     C*  non sia già stato immesso un codice analogo
138200960119     C*  a meno che non sia previsto dalla particol.
138300091026     c     d53ita        ifne      'E'
138400960119     C     §7R4TM        IFNE      §2AFTC
138500960205     C     §7R4NT        ORNE      V2CNTC
138600941222     C     §2AFTC        IFEQ      'A'
138700941122     C                   EXSR      GESAVV
138800941122     C   28              GOTO      AGGREC
138900941122     C                   END
139000960119     C                   END
139100091026     C                   END
139200100311     C*  Se mancata consegna impossibile immettere firmatario
139300100311     C     V2CFIR        IFNE      *BLANKS
139400100311     C                   SETON                                        404228
139500100311     C                   MOVEL     ERR(16)       $MSG
139600100311     C                   GOTO      AGGREC
139700100311     C                   END
139800941116     C*  Se mancata consegna vado a bloccare gli incassi sulla
139900941116     C*  relativa videata
140000941125     C     *INKF         IFEQ      '1'
140100941125     C     V2CNRD        IFNE      0                                            --- 05 ---
140200941117     C                   Z-ADD     NRR2          SAVREC
140300941207     C                   SETOFF                                       03
140400941116     C     V2CNRD        DO        V2CNRA        NRR3
140500941116     C     NRR3          CHAIN     LR53S03                            95
140600941116     C     *IN95         IFEQ      '0'
140700991007     C     V3DTPI        IFNE      'Ass'
140800991007     C     V3DTPI        ANDNE     'C/A'
140900991007     C                   SETON                                        05
141000991007     C                   ELSE
141100120302     C                   SETOFF                                       05
141200991007     C                   END
141300941116     C                   SETON                                        2282
141400941129     C                   MOVEL     'SI'          V3CNCO
141500941116     C                   MOVEL     'N'           V02SN1
141600120308     c     v3hkey        comp      ' '                                8787
141700941116     C                   UPDATE    LR53S03
141800941116     C                   SETOFF                                       22
141900941207     C                   SETON                                        03
142000941116     C                   END
142100941116     C                   END
142200941116     C                   Z-ADD     SAVREC        NRR2
142300100311     C*    NRR2          CHAIN     LR53S02                            95
142400941207     C                   MOVEL     'SI'          V2CBLO
142500941117     C                   END                                                    --- 05 ---
142600941116     C*
142700941116     C*  Se richiesta conferma aggiorno numero spedizioni non consegnate
142800941116     C                   Z-ADD     1             X
142900941116     C     VNMDOC        LOOKUP    NDC(X)                                 95
143000941129     C   95              ADD       1             SNT(X)
143100941116     C                   END
143200100205     c* Gestione data consegna richiesta
143300100211     c                   if        v2cevp <> v2hevp and
143400100211     c                             (w2zmdr <> *blank or w2zadr <> *blank)
143500100205     c                   setoff                                       32
143600100205     c                   exsr      vidrich
143700100211     c                   endif
143800100205     C*  Gestione turno
143900100205     C     V02CCN        IFEQ      'T  '
144000140313     C     V2cgc1        IFEQ      *BLANKS
144100140313     C     V2cgc2        andeq     *BLANKS
144200100205     C*
144300140313     C     V021gc        IFNE      *BLANKS
144400140313     C     V022gc        ORNE      *BLANKS
144500140313     C                   MOVEL     V021gc        W1GC
144600140313     C                   MOVEL     V022gc        W2GC
144700100205     C                   END
144800100205     C*
144900100205     C                   EXSR      TURNO
145000100205     C                   END
145100100205     C                   END
145200941117     C                   ELSE                                                   --- 04 ---
145300100205     c*****************************************
145400120109     C*  MANCATA CONSEGNA,  CONSEGNA PARZIALE
145500100205     c*****************************************
145600970805     C     V02CCA        IFEQ      '5'
145700970805     C     §2AIDD        ANDEQ     'P'
145800970805     C                   SETON                                        4528
145900970805     C                   MOVEL     ERR(73)       $MSG
146000970805     C                   GOTO      AGGREC
146100970805     C                   END
146200970805     C     §2AIDD        IFEQ      *BLANKS
146300941116     C                   EXSR      GESPAR
146400970805     C                   ELSE
146500970805     C                   MOVEL     '5'           V02CCA
146600970805     C                   MOVEL     V02CCN        V2CEVD
146700970805     C                   MOVEL     *BLANKS       V02CCN
146800970313     C* Se immesso codice consegna anomala '5' --> IDD apro finestra
146900970313     C* dettaglio segnacolli per richiedere all'utente quali sono i
147000970313     C* colli mancanti
147100970805     C                   EXSR      GESIDD
147200970805     C   28              GOTO      AGGREC
147300970805     C                   END
147400110117     C  nkl              MOVEL     *BLANKS       V02CCN
147500941116     C   28              GOTO      AGGREC
147600941117     C                   END                                                    --- 04 ---
147700950113     C*
147800941125     C                   END                                                    --- 03 ---
147900100205     c*********************
148000100205     c* CONSEGNA
148100100205     c*********************
148200941125     C     V02CCN        IFEQ      *BLANKS                                      --- 03 ---
148300100205     c* totalizzo ADR
148400000508     C     V2CCP1        IFEQ      'D'
148500000508     C     V2CCP2        OREQ      'D'
148600000508     C                   ADD       1             V05ADR
148700000508     C                   END
148800970123     C*  Se richiesto richiamo pgm x invio dei motivi di
148900970123     C*  mancata consegna
149000970123     C     V02SEL        IFEQ      'W'
149100970123     C                   MOVEL     'S'           D53VIS
149200970123     C                   EXSR      CALLR3
149300970123     C                   MOVEL     *BLANKS       V02SEL
149400970123     C     D53ERR        IFEQ      '1'
149500970123     C                   MOVEL     D53MSG        $MSG
149600970123     C                   SETON                                        28
149700970123     C                   GOTO      AGGREC
149800970123     C                   END
149900970123     C                   END
150000941116     C*  Gestisco i controlli da effettuare solo se ho consegnato
150100941124     C     V02CCA        IFNE      *BLANKS                                      --- 04 ---
150200941117     C*  Controllo codice consegna anomala
150300941117     C     '?'           SCAN      V02CCA                                 95
150400941117     C     *IN95         IFEQ      '1'
150500941123     C                   MOVEL     KKUT          §KUT
150600941117     C                   MOVEL     '7O'          §COD
150700941117     C                   MOVE      *BLANKS       V02CCA
150800941117     C                   CALL      'X§TABER'
150900941117     C                   PARM                    §KUT
151000941117     C                   PARM                    §COD
151100941117     C                   PARM                    §KEY
151200941117     C                   PARM                    §DES             30
151300941117     C                   MOVEL     §KEY          V02CCA
151400941117     C                   END
151500941124     C                   END
151600941117     C*
151700941124     C     V02CCA        IFNE      *BLANK                                       --- 04 ---
151800000505     C     V02CCA        LOOKUP    C7O                                    95
151900941128     C     *IN95         IFEQ      '0'
152000941116     C                   SETON                                        4528
152100941116     C                   MOVEL     ERR(10)       $MSG
152200941116     C                   GOTO      AGGREC
152300941124     C                   END
152400941124     C                   END                                                    --- 04 ---
152500941116     C*  Controllo codice consegna con riserva
152600941117     C     '?'           SCAN      V02RIS                                 95
152700941117     C     *IN95         IFEQ      '1'
152800941117     C                   MOVE      *BLANKS       V02RIS
152900941117     C                   MOVEL     *BLANKS       §TIP
153000941117     C                   MOVEL     'R*******'    §TIP
153100941117     C                   CALL      'TRUL40R'
153200941123     C                   PARM                    §TIP              8
153300941123     C                   PARM                    §KEY              8
153400941117     C                   PARM                    §DES             30
153500941117     C                   MOVEL     §KEY          V02RIS
153600941117     C                   END
153700941117     C*
153800941117     C     V02RIS        IFNE      *BLANK                                       --- 04 ---
153900941116     C                   Z-ADD     1             X
154000941116     C     V02RIS        LOOKUP    C2A(X)                                 95
154100941128     C     *IN95         IFEQ      '0'
154200941116     C                   SETON                                        4328
154300941116     C                   MOVEL     ERR(11)       $MSG
154400941116     C                   GOTO      AGGREC
154500941116     C                   ELSE
154600941116     C                   MOVEL     D2A(X)        DS2A
154700941116     C     §2ATIP        IFNE      'R'
154800941116     C                   SETON                                        4328
154900941116     C                   MOVEL     ERR(11)       $MSG
155000941116     C                   GOTO      AGGREC
155100941116     C                   END
155200941116     C                   END
155300941117     C                   END                                                    --- 04 ---
155400141127ab    *
155500141201ab   C*  La segnalazione la deve fare solo una volta poi
155600141201ab    *    è come se forzasse il controllo
155700141201ab   C*  SOLO se particolarità di consegna con PINCODE
155800141202ab   C                   if        §7RpinCODE ='S'
155900141202ab   c                              and v2hForza = *blank
156000141201ab    * ERRORE :
156100141201ab   C*   se si vuole mettere in consegna su una Distinta senza PDA
156200141201ab    *    oppure:
156300141201ab   C*   se la distinta è con PDA e la consegna non l'ha esitata l'Autista
156400141201ab   c                   if        wpda <>'S'
156500141201ab   c                              or
156600141201ab   c                             ecco_il_CET ='S' and
156700141212ab   c                             (§PCTFLGSPC='S' or PCTPDAAS4='A')
156800141202ab   c                              or
156900141202ab   c                             (vh2CCN <> *blank and vh2CCN <> 'P')
157000141211ab    *
157100141212ab    *  Controlla presenza di una RA (60) per verificare se Autorizzata
157200141212ab    *   la consegna oppure NO
157300141212ab   c                   exsr      Found_RA_60
157400141212ab    *
157500141212ab    * Se Presente la RA (60) ed è autorizzata
157600141212ab    *   deve INFORMARE e RICORDARE di apporre il timbro o la dicitura sulla LDV
157700141212ab   c                   IF         ecco_la_RA_60 = 'S' and Autorizzata ='S'
157800141212ab    *
157900141212ab    *  Emette una Finestra informativa
158000141212ab    *   e flagga comunque il record per non ripetere l'emissione
158100141212ab   c                   exsr      WIN_INFO
158200141212ab   c                   eval           v2hForza = 'I'
158300141212ab    *
158400141212ab    * Se NON presente la RA (60) oppure è presente ma non è autorizzata,
158500141212ab    *  deve proporre una WINDOW di Forzatura
158600141212ab    *    e se NON Forzato emettere l'errore sulla Consegna
158700141212ab    *
158800141212ab   c                   ElseIf     ecco_la_RA_60 <> 'S'
158900141212ab   c                                or
159000141212ab   c                              ecco_la_RA_60 = 'S' and Autorizzata <>'S'
159100141212ab    *
159200141211ab    *  Emette una Finestra di Forzatura
159300141211ab   c                   exsr      WIN_Forza
159400141202ab    *
159500141202ab    *   Se non si è forzato emissione dell'errore
159600141209ab   c                   if        lrq0errO = '0' and lrq0frzIO <> 'F'
159700141127ab   C                   SETON                                        4028
159800141127ab   C                   MOVEL     ERR(77)       $MSG
159900141127ab   C                   GOTO      AGGREC
160000141211ab    *
160100141209ab   c                   elseIf        lrq0frzIO = 'F'
160200141211ab    *
160300141212ab    *  x Pro-memoria di aver Forzato
160400141212ab    *    serve "F" per scrivere una RA (61) quando fa la AGGIOR
160500141209ab   c                   eval      v2hForza = lrq0frzIO
160600141202ab   c                   end
160700141202ab    *
160800141212ab   c                   endIF
160900141212ab    *
161000141201ab   C                   END
161100141127ab   C                   END
161200141127ab   C*
161300950113     C*  Controllo se obbligatorio il firmatario
161400950113     C     §7R2FI        IFEQ      'S'
161500950113     C     V2CFIR        ANDEQ     *BLANKS
161600950113     C                   SETON                                        4228
161700950113     C                   MOVEL     ERR(12)       $MSG
161800950113     C                   GOTO      AGGREC
161900950113     C                   END
162000970623     C*  Se non consegnata impossibile mettere codice consegna anomala
162100970623     C     V02CCA        IFEQ      '5'
162200970623     C     V02CPZ        ANDEQ     'P'
162300970623     C                   SETON                                        404528
162400970623     C                   MOVEL     ERR(73)       $MSG
162500970623     C                   GOTO      AGGREC
162600970623     C                   END
162700100205     c* controllo ora consegna
162800100205     c                   exsr      srctrora
162900100205     C   47              GOTO      AGGREC
163000941118     C*
163100941123     C   KF              EXSR      AGGTO2
163200941124     C                   END
163300941205     C*
163400941205     C     V2CTUR        IFEQ      'T'
163500941205     C                   EXSR      TURNO
163600941205     C                   MOVEL     ' '           V2CTUR
163700941205     C                   END
163800100205     c*************
163900080806     c* richiamo pgm variazione bolle x inserire bancali causale BR
164000150224     c*m                 if        v02ccn = *blank or v02ccn ='P  '
164100141202ab    ****
164200150224ab   c*m                 if        ecco_il_CET = 'S'
164300141202ab    ****
164400150224     c*m                 CLEAR                   darendere         2 0
164500150224     c*m                 CLEAR                   dar5ban
164600150224     c*m                 move      'BAN'         trek5
164700150224     c*m   kar5          chain(n)  fiar501l
164800150224     c*m                 if        %found(fiar501l)
164900150224     c*m                 movel     ar5uni        dar5ban
165000150224     c*m                 eval      darendere =  §ar5nba + §ar5nb2
165100150224     c*m                 endif
165200141202      ****
165300150224     c*m                 if        wnspbr <> v2cnsp and darendere > 0
165400150224     c*m                 z-add     v2cnsp        wnspbr            7 0
165500150224     C*m                 CLEAR                   FNLR48
165600150224     C*m                 CLEAR                   DSARBD
165700150224     C*m                 CLEAR                   DSARBG
165800150224     C*m                 CLEAR                   DSARBk
165900150224     C*m                 CLEAR                   DARBt
166000080806      *
166100150224     C*m                 Z-ADD     V2CLNP        D48LNP
166200150224     C*m                 Z-ADD     V2CAAS        D48AAS
166300150224     C*m                 Z-ADD     V2CNRS        D48NRS
166400150224     C*m                 Z-ADD     V2CNSP        D48NSP
166500150224     C*m                 MOVEL     'E'           D48FFR
166600150224     C*m                 MOVEL     V2CCBO        D48CBO
166700150224     C*m                 MOVEL     'BR'          D48CVB
166800150224     C*m                 MOVEL     'D'           D48TRC
166900150224     c*m                 z-add     §PCTNRBANR    §BDPKF
167000150224     c*m                 if        §PCTNOMFIR <> *blank
167100150224     c*m                 movel     §PCTNOMFIR    §BDRSD
167200150224     c*m                 else
167300150224     c*m                 eval      §BDRSD  = 'Nominativo non inserito'
167400150224     c*m                 endif
167500150224     c*m                 move      'PDA'         §BDRD2
167600150224     C*m                 MOVEL     fnlr48        kpjbu
167700150224     C*m                 CALL      'FNLR48R'
167800150224     C*m                 PARM                    KPJBA
167900150224     C*m                 PARM                    DSARBD
168000150224     C*m                 PARM                    DSARBK
168100150224     C*m                 PARM                    DARBT
168200150224     C*m                 PARM                    DSARBG
168300150224     C*m                 PARM                    trul90ds
168400150224     C*m                 MOVEL     KPJBU         FNLR48
168500150224     c*m                 endif
168600150224     c*m                 endif
168700150224     c*m                 endif
168800150224     C*
168900941117     C*  Controllo se variati codici consegna particolare o gg.
169000941117     C*  di chiusura x evetuale richiamo pgm. variazione bolle
169100941130     C     V2CCP1        IFNE      V2HCP1
169200950206     C     V2CCP1        ANDNE     *BLANKS
169300941130     C     V2CCP2        ORNE      V2HCP2
169400950206     C     V2CCP2        ANDNE     *BLANKS
169500941117     C     W1GC          ORNE      V021GC
169600941117     C     W2GC          ORNE      V022GC
169700950322     C     V2CGC1        ORNE      *BLANKS
169800950322     C     V2CGC1        ANDNE     V021GC
169900950322     C     V2CGC2        ORNE      *BLANKS
170000950322     C     V2CGC2        ANDNE     V022GC
170100040223     c     w13dcrg       orne      v2cdcr
170200051010     c     w13doil       orne      v2ctcr
170300101214     c     w2zadr        orne      *blank
170400941117     C                   EXSR      VARBOL
170500941117     C                   END
170600020610      *
170700941117     C*  Eseguo update subfile
170800941117     C     AGGREC        TAG
170900990922     C     V2CNT1        IFNE      *BLANKS
171000090220     C     V02selb       IFeq      *BLANKS
171100941214     C                   SETON                                        02
171200090220     c                   else
171300090220     c                   setoff                                       02
171400090220     C                   endif
171500941214     C                   END
171600941128     C     V02CPZ        COMP      'P'                                    07
171700970805     C  N07V2CIDD        COMP      'P'                                    07
171800941117     C                   SETON                                        22
171900970218     C                   SETOFF                                       35
172000970218     C     *IN28         IFEQ      '0'
172100970218     C     *IN40         OREQ      '0'
172200970218     C     V2CEXC        COMP      *BLANKS                            3535
172300970218     C                   ELSE
172400970218     C                   MOVEL     *BLANKS       V2CEXC
172500970218     C                   MOVEL     *BLANKS       V2CNM1
172600970218     C                   MOVEL     *BLANKS       V2CNM2
172700970218     C                   MOVEL     *BLANKS       V2CTR1
172800970218     C                   MOVEL     *BLANKS       V2CTR2
172900970218     C                   END
173000970805     C     V2CIDD        COMP      ' '                                7575
173100131205     C     Vh2hco        COMP      0                                  52
173200990423     C                   MOVEL     V2IN09        *IN09
173300000522     C                   MOVEL     V2IN24        *IN24
173400941117     C                   UPDATE    LR53S02
173500941117     C                   SETOFF                                       22
173600941117     C     *IN28         IFEQ      '0'
173700941118     C     *INKF         IFEQ      '1'
173800941118     C                   ADD       1             NRR2
173900941118     C     NRR2          CHAIN     LR53S02                            96
174000941118     C                   ELSE
174100941117     C                   READC     LR53S02                                96
174200941123     C                   END
174300941117     C                   ELSE
174400941117     C                   MOVE      NRR2          NR2KEY
174500941117     C                   END
174600941117     C*
174700941117     C                   END                                                    --- 01 ---
174800941117     C*
174900941117     C                   ENDSR
175000091216     C*----------------------------------------------------*
175100091216     C*  controllo ora consegna per h10,30 e priority
175200091216     C*----------------------------------------------------*
175300091216     C     srctrora      BEGSR
175400091216     C     V02CCN        IFne      *BLANKS                                      --- 03 ---
175500091216     C     V2CEVP        IFEQ      *BLANKS
175600091216     C                   MOVEL     V02CCN        V2CEVP
175700091216     C                   END
175800091216     C                   Z-ADD     1             X
175900091216     C     V2cevp        LOOKUP    C2A(X)                                 95
176000091216     C                   MOVEL     D2A(X)        DS2A
176100091216     c                   if        §2antc <> 'S'
176200091216     c                   leavesr
176300091216     c                   end
176400100205     C                   MOVEL     D2Z(X)        DS2Z
176500091216     c                   end
176600100205     c* obbligo ora se H1030, priority, se richiesto nella part.cons.
176700100205     c* oppure se estensione eventi
176800091216     C                   if        v02hco = 0 and v02mco = 0
176900091216     C                             and (v02tsp = 'H'
177000100203     c                             or v02tsp = 'E'
177100100112     c                             or §7R2OR = 'S' or
177200140221     c                             §2ztre = 'S')
177300091221     c                   movel     v02tsp        v02fhob
177400091216     C                   SETON                                        4728
177500091216     C                   MOVEL     ERR(13)       $MSG
177600091218     c                   leavesr
177700091216     C                   END
177800091216     C*  Ora consegna errata  per H10,30
177900091216     C     v02fhob       IFeq      'H'
178000131203      *se impostata ora da esistenza esito CEP utilizzo quella per calcolare affidabilità
178100131203     C     Vh2HCO        IFGT      0
178200131203     C     Vh2HCO        IFGT      10
178300131203     C     Vh2hco        OReq      10
178400131203     C     Vh2MCO        ANDGT     30
178500131203     C                   SETON                                        4728
178600131203     C                   MOVEL     ERR(107)      $MSG
178700131203     c                   movel     'X'           v02fhob
178800131203     c                   leavesr
178900131203     C                   Endif
179000131203      *cenza CEP uso campi a video normalmente
179100131203     C                   Else
179200091216     C     V02HCO        IFGT      10
179300091216     C     V02hco        OReq      10
179400091216     C     V02MCO        ANDGT     30
179500091216     C                   SETON                                        4728
179600091216     C                   MOVEL     ERR(107)      $MSG
179700091216     c                   movel     'X'           v02fhob
179800091218     c                   leavesr
179900091216     C                   END
180000131203     C                   endif
180100091216     C                   END
180200091216     C*  Ora consegna errata  per priority
180300091216     C     v02fhob       IFeq      'E'
180400131203     c                   if        vh2hco > 0
180500131203     c                   movel     vh2hco        comora            4 0
180600131203     c                   move      vh2mco        comora            4 0
180700131203     c                   else
180800091216     c                   movel     v02hco        comora            4 0
180900091216     c                   move      v02mco        comora            4 0
181000131203     c                   endif
181100091218     c                   if        vh2ddc > v2hdci or
181200091218     c                             (vh2ddc = v2hdci and
181300091216     c                             comora > v2hhci )
181400091216     C                   SETON                                        4728
181500091216     C                   MOVEL     ERR(108)      $MSG
181600091216     c                   movel     'X'           v02fhob
181700091218     c                   leavesr
181800091216     C                   END
181900091216     C                   END
182000131204     c* controllo  se presente ora primo evento l'ora a video non può essere inferiore
182100131204     c                   if        vh2hco > 0
182200131204     c                   movel     vh2hco        comorap           4 0
182300131204     c                   move      vh2mco        comorap
182400131204     c                   movel     v02hco        comorav           4 0
182500131204     c                   move      v02mco        comorav
182600131204     c                   if        comorav < comorap
182700131204     c                   seton                                        4728
182800131204     c                   movel     err(83)       $msg
182900131204     c                   leavesr
183000131204     c                   endif
183100131204     c                   endif
183200100112     C*  Controllo L'ora di consegna SE DISTINTA DEL POMERIGGIO L'ORARIO
183300100112      *  DEVE ESSERE MAGGIORE UGUALE ALLE 13
183400100112     C     V02HCO        IFNE      0
183500100112     C                   Z-ADD     1             XY                3 0
183600100112     C     VNMDOC        LOOKUP    ND1(XY)                                99
183700100112     C   99FPP(XY)       IFEQ      'P'
183800120814     C     wpda          ANDne     'S'
183900100112     C     V02HCO        ANDLE     12
184000100112     C                   SETON                                        4728
184100100112     C                   MOVEL     ERR(84)       $MSG
184200100112     c                   leavesr
184300100112     C                   END
184400100112     C                   ENDIF
184500100112     C*  Ora consegna errata
184600100112     C     D53ITA        IFNE      'E'
184700100112     C     V02HCO        IFGT      21
184800100112     C     V02HCO        ORLT      7
184900100112     C     V02HCO        ANDGT     0
185000100112     C     V02MCO        ORNE      0
185100100112     C     V02MCO        ANDGT     59
185200100112     C                   SETON                                        4728
185300100112     C                   MOVEL     ERR(14)       $MSG
185400100112     c                   leavesr
185500100112     C                   END
185600100112     C                   END
185700091216     C                   ENDSR
185800941116     C*----------------------------------------------------*
185900941116     C*  GESTIONE PARTICOLARITA' CONSEGNA
186000941116     C*----------------------------------------------------*
186100941116     C     GESPCO        BEGSR
186200970617      *
186300040213 1   C     V2CGMA        IFNE      *BLANKS
186400970211     C*  Se esiste particolarità che prevede l'invio dei
186500970211     C*  motivi di mancata consegna ed ho immesso G o GEN
186600970211     C*  richiamo pgm x immissione decodifica
186700040213 2   C     §2ATIP        IFNE      'P'
186800970211     C                   MOVEA     TMC           WTMC              5
186900040213 3   C     WTMC          IFNE      *BLANKS
187000970211     C     'T'           LOOKUP    TMC                                    32
187100970211     C  N32'G'           LOOKUP    TMC                                    32
187200040213 4   C   32V02CCN        IFEQ      'G  '
187300970211     C     V02CCN        OREQ      'GEN'
187400040213 5   C     V2CEXC        IFEQ      *BLANKS
187500970211     C                   MOVEL     'S'           D53VIS
187600970211     C                   EXSR      CALLR3
187700040213 6   C     D53ERR        IFEQ      '1'
187800970211     C                   MOVEL     D53MSG        $MSG
187900970211     C                   SETON                                        28
188000040213 x6  C                   ELSE
188100040213 7   C     D53NT1        IFEQ      *BLANKS
188200970220     C     D53MSG        ANDEQ     *BLANKS
188300970211     C                   SETON                                        28
188400970211     C                   MOVEL     ERR(64)       $MSG
188500040213 e7  C                   END
188600040213 e6  C                   END
188700040213 e5  C                   END
188800970211     C*
188900040213 x4  C                   ELSE
189000970211     C                   MOVEL     'N'           D53VIS
189100970211     C                   EXSR      CALLR3
189200040213 5   C     D53ERR        IFEQ      '1'
189300970211     C                   MOVEL     D53MSG        $MSG
189400970211     C                   SETON                                        28
189500040213 e5  C                   END
189600040213 e4  C                   END
189700040213 e3  C                   END
189800941116     C*  Se cod.particolarità indica di ritentare consegna x n.volte
189900000421     C*  per lasciato avviso sostiutuisco all'evento di lasciato avv.
190000000421     C*  quello che la particolarità mi dice di proporre fino a che
190100000421     C*  non ho raggiunto gli x tentativi
190200040213 3   C     §2AFTC        IFEQ      §7R4TM
190300941123     C     V2CNTC        ANDLT     §7R4NT
190400951228     C     V02CCN        ANDNE     §7R4EV
190500951228     C     V2CF08        ANDEQ     *BLANKS
190600040213 4   C     V2CEVP        IFNE      §7R4EV
190700030829     C*                  SETON                                        2810
190800030829     C                   SETON                                        28
190900941116     C                   MOVEL     §7R4EV        V2CEVP
191000941116     C                   MOVEL     ERR(7)        $MSG
191100941116     C                   GOTO      FINPAR
191200040213 x4  C                   ELSE
191300951228     C                   MOVEL     *INKH         V2CF08
191400040213 6   C     *INKH         IFEQ      '1'
191500040213 7   C     §7R3EV        IFEQ      *BLANKS
191600990419     C                   MOVEL     V02CCN        V2CEVP
191700040213 x7  C                   ELSE
191800990419     C                   MOVEL     §7R3EV        V2CEVP
191900040213 e7  C                   END
192000040213 e5  C                   END
192100951228     C                   SETOFF                                       10
192200040213 e4  C                   END
192300040213 e3  C                   END
192400941125     C*  Lasciato avviso diverso da standard
192500040213 3   C     WTMC          IFNE      *BLANKS
192600970211     C     D53TRV        ANDEQ     'S'
192700970211     C     §2AFTC        ANDEQ     §7R3TM
192800970211     C     V02CCN        ANDNE     §7R3EV
192900970211     C     V2CEVP        ANDNE     §7R3EV
193000970214     C                   MOVEL     'AVV'         §7R3EV
193100040213 e3  C                   END
193200970211     C*
193300040213 3   C     §2AFTC        IFEQ      §7R3TM
193400941116     C     V02CCN        ANDNE     §7R3EV
193500941116     C     V2CEVP        ANDNE     §7R3EV
193600040213 4   C     §7R4NT        IFLE      V2CNTC
193700990419     C     §7R4NT        OREQ      *ZEROS
193800970211     C*  Se previsto invio e già inviato x l.avviso non do più attesa
193900941116     C                   SETON                                        28
194000941116     C                   MOVEL     §7R3EV        V2CEVP
194100941116     C                   MOVEL     ERR(7)        $MSG
194200941116     C                   GOTO      FINPAR
194300040213 e4  C                   END
194400040213 e3  C                   END
194500941116     C*  Se sono impostati dei codici tipologie giacenze controllo
194600941116     C*  attendibilità con codice mancata consegna
194700960119     C                   MOVEA     TPG           WTPG              5
194800040213 3   C     §2AFTC        IFEQ      'G'
194900961129     C*
195000040213 4   C     WTPG          IFNE      *BLANKS
195100040213 5   C     V02CCN        IFEQ      'G  '
195200961205     C     WTPG          ANDNE     'T'
195300960119     C                   SETON                                        2840
195400960119     C                   MOVEL     ERR(9)        $MSG
195500960119     C                   GOTO      FINPAR
195600040213 e5  C                   END
195700960116     C                   SETOFF                                       95
195800040213 5   C     WTPGIA        IFNE      *BLANKS
195900941116     C                   Z-ADD     1             X
196000950113     C     WTPGIA        LOOKUP    TPG(X)                                 95
196100040213 e5  C                   END
196200040213 5   C     *IN95         IFEQ      '0'
196300941116     C                   Z-ADD     1             X
196400941116     C     'T'           LOOKUP    TPG(X)                                 95
196500040213 e5  C                   END
196600040213 5   C     *IN95         IFEQ      '1'
196700941116     C     V2CNTC        ANDLT     NTC(X)
196800941116     C     V2CEVP        ANDNE     EVE(X)
196900941116     C     V02CCN        ANDNE     EVE(X)
197000941116     C                   SETON                                        28
197100941116     C                   MOVEL     EVE(X)        V2CEVP
197200941116     C                   MOVEL     ERR(7)        $MSG
197300941116     C                   GOTO      FINPAR
197400040213 e5  C                   END
197500040213 e4  C                   END
197600941125     C*  Apertura giacenza dopo x giorni
197700040213 4   C     §2AFTC        IFEQ      §7R5TM
197800941116     C     V2CEVP        ANDNE     §7R5EV
197900941116     C     V02CCN        ANDNE     §7R5EV
198000941116     C                   SETON                                        28
198100941116     C                   MOVEL     §7R5EV        V2CEVP
198200941116     C                   MOVEL     ERR(7)        $MSG
198300941116     C                   GOTO      FINPAR
198400040213 e4  C                   END
198500040213 e3  C                   END
198600961129     C*
198700040213 e2  C                   END
198800941116     C*  Se immesso codice particolarità da usare solo se cliente
198900941116     C*  abilitato controllo se = codice particolarità da propore
199000131111 2   C*m   §2ZUTP        IFEQ      'S'
199100131111     C*m   C2A(Y)        ANDEQ     V02CCN
199200131111     C*m                 SETON                                        2840
199300131111     C*m                 MOVEL     ERR(55)       $MSG
199400131111     C*m                 GOTO      FINPAR
199500131111 e2  C*m                 END
199600040213 e1  C                   END
199700941116     C*----------------------------------------------------*
199800941116     C*  FINE GESTIONE PARTICOLARITA' CONSEGNA
199900941116     C*----------------------------------------------------*
200000941123     C     FINPAR        ENDSR
200100970121     C*----------------------------------------------------*
200200970121     C*  RICHIAMO PGM FNLR53R3
200300970121     C*----------------------------------------------------*
200400970121     C     CALLR3        BEGSR
200500970121     C*
200600970121     C                   MOVEL     D53VIS        WSVVIS            1
200700970121     C                   CLEAR                   LR533
200800970220     C                   MOVEL     V02SEL        D53RIC
200900970220     C                   MOVEL     WSVVIS        D53VIS
201000970121     C                   Z-ADD     V2CAAS        D53AAS
201100970121     C                   Z-ADD     V2CLNP        D53LNP
201200970121     C                   Z-ADD     V2CNRS        D53NRS
201300970121     C                   Z-ADD     V2CNSP        D53NSP
201400970121     C                   MOVEL     §2AFTC        D53TMC
201500061030      * passo evento proposto altrimenti passo evento inserito a video
201600090507      * se evento di giacenza passo sempre quello del video da riportare sul tivgd
201700090507     c                   if        §2aftc = 'G'
201800090507     c                   movel     v02ccn        d53cmc
201900090507     c                   else
202000090507     c                   if        v2cevp <> *blank
202100090507     c                   movel     v2cevp        d53cmc
202200090507     c                   else
202300090507     C                   MOVEL     V02CCN        D53CMC
202400090507     c                   endif
202500090507     c                   endif
202600970121     C                   MOVEL     §2ADES        D53DMC
202700970121     C                   MOVEL     V2CGMA        D53GMA
202800970121     C                   MOVEL     V2CEXC        D53EXC
202900970121     C                   MOVEL     V2CNM1        D53NT1
203000970121     C                   MOVEL     V2CNM2        D53NT2
203100970121     C                   MOVEL     V2CTR1        D53TR1
203200970121     C                   MOVEL     V2CTR2        D53TR2
203300970121     C                   MOVEL     LR533         KPJBU
203400970121     C                   CALL      'FNLR53R3'
203500970121     C                   PARM                    KPJBA
203600970121     C                   MOVEL     KPJBU         LR533
203700970121     C*  Se non ci sono errori aggiorno campi subfile
203800970121     C     D53ERR        IFEQ      *BLANKS
203900970121     C     D53F12        ANDEQ     *BLANKS
204000970121     C                   MOVEL     D53EXC        V2CEXC
204100970121     C                   MOVEL     D53NT1        V2CNM1
204200970121     C                   MOVEL     D53NT2        V2CNM2
204300970121     C                   MOVEL     D53TR1        V2CTR1
204400970121     C                   MOVEL     D53TR2        V2CTR2
204500970121     C*  Se ho immesso note aggiorno EXC con CMC
204600970121     C     D53NT1        IFNE      *BLANKS
204700970121     C                   MOVEL     D53CMC        V2CEXC
204800970121     C                   END
204900970121     C                   END
205000970121     C*
205100970121     C                   ENDSR
205200941122     C*----------------------------------------------------*
205300941122     C*  GESTIONE LASCIATO AVVISO
205400941122     C*----------------------------------------------------*
205500941122     C     GESAVV        BEGSR
205600941122     C*
205700941222     C                   Z-ADD     V2CLNP        KLNP
205800941222     C                   Z-ADD     V2CNRS        KNRS
205900941222     C                   Z-ADD     V2CNSP        KNSP
206000990803     C                   Z-ADD     V2CAAS        KAAS
206100941122     C                   MOVE      *HIVAL        KDEV
206200941122     C                   MOVE      *HIVAL        KHEV
206300941123     C                   MOVE      'N'           WTRAVV            1
206400941123     C                   MOVE      'N'           WTRGIA            1
206500000421     C* A fronte di un immissione di un evento di lasciato
206600000421     C* avviso in distinta verifico se ne ho immesso un altro
206700990803     C     KEVB1         SETGT     FNEVB01L
206800990803     C     KEVB2         READPE    FNEVB01L                               95
206900941122     C     *IN95         DOWEQ     '0'
207000941222     C     WTRAVV        ANDEQ     'N'
207100941122     C                   Z-ADD     1             X
207200941122     C     EVBCEV        LOOKUP    C2A(X)                                 94
207300941122     C     *IN94         IFEQ      '1'
207400941122     C                   MOVEL     D2A(X)        DS2A
207500070406     C                   MOVEL     D2z(X)        DS2z
207600941122     C     §2AFTC        IFEQ      'A'
207700941122     C                   MOVEL     'S'           WTRAVV
207800941123     C                   Z-ADD     EVBDEV        WDTAVV
207900941122     C                   END
208000941122     C                   END
208100990803     C     KEVB2         READPE    FNEVB01L                               95
208200941122     C                   END
208300941122     C*
208400000421     C* ... se ho trovato un evento di lasciato avviso verifico
208500000421     C* se dopo l'immissione di quest'ultimo ho avuto una apert.
208600000421     C* di giacenza
208700941222     C                   MOVE      V2CAAS        KAAS
208800941125     C     WTRAVV        IFEQ      'S'
208900050406     C     KGCA          CHAIN     tigcp21L                           95
209000941125     C     *IN95         IFEQ      '0'
209100941122     C                   MOVE      GCPDUR        WDTGIA
209200941122     C     WDTGIA        IFGT      WDTAVV
209300941122     C                   MOVE      'S'           WTRGIA
209400941122     C                   END
209500941122     C                   END
209600941122     C*  Se trovato lasciato avviso e non trovata giacenza errore
209700000421     C*  (non posso fare + lasciati avvisi senza giacenza a meno
209800000421     C*   che non sia previsto dalla perticolarità consegne)
209900941125     C     WTRGIA        IFNE      'S'
210000941122     C                   SETON                                        4028
210100941122     C                   MOVEL     ERR(53)       $MSG
210200941122     C                   END
210300941122     C                   END
210400941122     C*
210500941122     C                   ENDSR
210600941117     C*----------------------------------------------------*
210700941117     C*  GESTIONE RICHIAMO PGM VARIAZIONE BOLLE
210800941117     C*----------------------------------------------------*
210900941117     C     VARBOL        BEGSR
211000941117      *
211100941117     C                   Z-ADD     1             X                 3 0
211200941117     C                   CLEAR                   DS3A
211300941125     C                   Z-ADD     1             X
211400941117     C     V2CCBO        LOOKUP    C3A(X)                                 95
211500941117     C     *IN95         IFEQ      '1'
211600941117     C                   MOVEL     D3A(X)        DS3A
211700941117     C                   END
211800941117     C                   MOVEL     §3ATB1        PORTO             1
211900941117     C*
212000941117     C* Carico la variazione sulla bolla a video tranne nei casi
212100000421     C* di bolla figlia non Assegnato
212200941222     C                   CLEAR                   FNLR48
212300941222     C                   CLEAR                   DSARBD
212400941117     C                   CLEAR                   DSARBG
212500941117     C     VH2LPP        IFEQ      0
212600941117     C     PORTO         OREQ      'A'
212700941117     C                   Z-ADD     V2CLNP        D48LNP
212800941117     C                   Z-ADD     V2CAAS        D48AAS
212900941117     C                   Z-ADD     V2CNRS        D48NRS
213000941117     C                   Z-ADD     V2CNSP        D48NSP
213100941117     C                   ELSE
213200941117     C                   Z-ADD     VH2LPP        D48LNP
213300941117     C                   Z-ADD     VH2AAP        D48AAS
213400941117     C                   Z-ADD     VH2NRP        D48NRS
213500941117     C                   Z-ADD     VH2NSP        D48NSP
213600941117     C                   END
213700941117     C*
213800941215     C                   MOVEL     'E'           D48FFR
213900941117     C                   MOVEL     V2CCBO        D48CBO
214000941117     C                   MOVEL     'CR'          D48CVB
214100941117     C                   MOVEL     'G'           D48TRC
214200941222     C                   MOVEL     'A'           D48TBO
214300040220      * variata data consegna richiesta
214400040220      *  in questo caso viene forzata la causale 'CD'
214500101214     c*                  if        w13dcrg > 0 and
214600101214     c*                            (w13dcrg <> v2cdcr or w13doil <> v2ctcr)
214700101214     c*                            or w2zadr <> *blank
214800101214     c                   if        (w13dcrg <> v2cdcr or w13doil <> v2ctcr)
214900101214     c                             or w2zadr <> *blank
215000101015     c*                  z-add     w13dcrg       v2cdcr
215100101015     c*                  move      w13doil       v2ctcr
215200101015     c*                  z-add     v13hcr        v2chcr
215300101015     c                   eval      w13dcrg   =   v2cdcr
215400101015     c                   eval      w13doil   =   v2ctcr
215500040220     C                   MOVEL     'CD'          D48CVB
215600040220     c                   endif
215700040220
215800030127      * Se la bolla prevede la consegna particolare supermercato
215900030127      * oppure è stata inserita la consegna particolare supermercato
216000030127      * verifico se devo richiamare il pgm FNLR48R con la causale 'CS'
216100040220      * al posto della causale 'CR' o 'CD'
216200030127     c                   If        V2cCp1 = 'S' or V2cCp2 = 'S' or
216300030127     c                             V2hCp1 = 'S' or V2hCp2 = 'S'
216400030127     c                   Clear                   Ds7l
216500030127     c                   Movel     '7L'          kCod
216600030127     c                   Movel(p)  'CS'          kKey
216700030127     c     kTab2         Chain     Tabel00f                           96
216800030127     c                   If        Not *In96 and TblFlg <> ' '
216900030127     c                   Eval      *In96 = *On
217000030127     c                   EndIf
217100030127     c  n96              Movel     TblUni        Ds7l
217200030127     c                   If        Not *In96 and §7LSup <> 'N'
217300030127     c                   Movel     'CS'          D48Cvb
217400050105     c                   Movel     'V'           D48Ffr
217500050105     c                   Movel     'S'           D48F12
217600030127     c                   EndIf
217700030127     c                   EndIf
217800040216
217900941117     C                   Z-ADD     V2CHCR        §BGHCR
218000941117     C                   Z-ADD     V2CDCR        §BGDCR
218100941117     C                   MOVEL     V2CTCR        §BGTCR
218200941117     C                   MOVEL     W1GC          §BGGC1
218300941117     C                   MOVEL     W2GC          §BGGC2
218400950322     C     V2CGC1        IFNE      *BLANKS
218500950322     C     V2CGC2        ORNE      *BLANKS
218600950321     C                   MOVEL     V2CGC1        §BGGC1
218700950321     C                   MOVEL     V2CGC2        §BGGC2
218800950321     C                   END
218900941117     C     V2CCP1        IFNE      *BLANKS
219000941117     C                   MOVEL     V2CCP1        §BGTC1
219100941117     C                   ELSE
219200941123     C                   MOVEL     V2HCP1        §BGTC1
219300941117     C                   END
219400941117     C     V2CCP2        IFNE      *BLANKS
219500941117     C                   MOVEL     V2CCP2        §BGTC2
219600941117     C                   ELSE
219700941123     C                   MOVEL     V2HCP2        §BGTC2
219800941117     C                   END
219900941117     C                   MOVEL     FNLR48        KPJBU
220000071031     C                   TIME                    WHHDAT
220100071031     C                   MOVEL     WHHDAT        TIMESav
220200071031     C                   MOVE      WHHDAT        G02DAT
220300071031     C                   MOVEL     *BLANK        G02ERR
220400071031     C                   CALL      'XSRDA8'
220500071031     C                   PARM                    WLBDA8
220600071031     C                   Z-ADD     G02INV        OGGIsav
220700941117     C                   CALL      'FNLR48R'
220800941117     C                   PARM                    KPJBA
220900941117     C                   PARM                    DSARBD
221000941117     C                   PARM                    DSARBK
221100991129     C                   PARM                    DARBT
221200941117     C                   PARM                    DSARBG
221300030718     C                   PARM                    trul90ds
221400941117     C                   MOVEL     KPJBU         FNLR48
221500941117     C     D48ERR        IFEQ      *BLANKS
221600050105     C     D48f12        andne     '1'
221700071026     c* memorizzo la spia per le spedioni che vengono variate perchè in caso
221800071026     c* di non conferma devo riportare indietro la variazione
221900071026     c                   exsr      srmemo
222000941125     C                   MOVEL     §BGTC1        V2CCP1
222100941125     C                   MOVEL     §BGTC2        V2CCP2
222200941123     C                   MOVEL     V2CCP1        V2HCP1
222300941123     C                   MOVEL     V2CCP2        V2HCP2
222400941123     C                   MOVE      W1GC          V021GC
222500941123     C                   MOVE      W2GC          V022GC
222600101015     C*                  MOVE      W13dcrg       V2cdcr
222700101015     C*                  MOVE      W13doil       V2ctcr
222800100211     C                   MOVEl     v2cevp        V2hevp
222900950322     C     V2CGC1        IFNE      *BLANKS
223000950322     C     V2CGC2        ORNE      *BLANKS
223100950322     C                   MOVE      V2CGC1        V021GC
223200950322     C                   MOVE      V2CGC2        V022GC
223300140313     C*mm                MOVEL     *BLANKS       V2CGC1
223400140313     C*mm                MOVEL     *BLANKS       V2CGC2
223500950322     C                   END
223600941117     C                   ELSE
223700941117     C                   MOVEL     *BLANKS       V2CCP1
223800941117     C                   MOVEL     *BLANKS       V2CCP2
223900101201     C*mm                MOVEL     '§  '         V02ccn
224000101201     C                   MOVEL     '£££'         V02ccn
224100941117     C                   SETON                                        28
224200941117     C                   MOVEL     ERR(24)       $MSG
224300941215     C     D48MSG        IFNE      *BLANKS
224400941215     C                   MOVEL     D48MSG        $MSG
224500941117     C                   END
224600941215     C                   END
224700941117     C*
224800941117     C                   ENDSR
224900941117     C*----------------------------------------------------*
225000071026     C*  memorizzo spia x bolla variata
225100941117     C*----------------------------------------------------*
225200071026     C     srmemo        BEGSR
225300071026     C*
225400071026     c                   clear                   varbtrd
225500071026     c     kvarb         chain     wfvarb1l
225600071026     c                   if        not %found(wfvarb1l)
225700090731      * se motivo 16 o 23 date consegna richiesta imposto il motivo per verificare
225800090731      * se quando vado ad aggiornare il rekord è stato cambiato ma è stata fatta
225900090731      * una variazione bolla con data consegna richiesta
226000090731     c                   if        v02ccn = '016' or v02ccn = '023'
226100090731     c                   move      v02ccn        varbtrd
226200090731     c                   endif
226300071026     C                   eval      varblnp =     D48LNP
226400071026     C                   eval      varbaas =     D48AAS
226500071026     C                   eval      varbnrs =     D48NRS
226600071026     C                   eval      varbnsp =     D48NSP
226700071026     C                   eval      varbpru =     knmus
226800071031     C                   Z-ADD     oggisav       vARBdim
226900071030     C                   eval      vARBoim = timesav
227000110418     c                   if        arpnsp =  d48nsp
227100110418     c*                  if        arpnsp <> 0
227200071026     c                   eval      varbflr = fiarpds
227300071029     c                   else
227400071029     c                   clear                   varbflr
227500071029     c                   end
227600071026     c                   write     wfvarb00
227700090731     c                   else
227800090731     c                   if        v02ccn = '016' or v02ccn = '023'
227900090731     c                   move      v02ccn        varbtrd
228000090731     c                   update    wfvarb00
228100090731     c                   endif
228200071026     c                   end
228300071026     C*
228400071026     C                   ENDSR
228500090731     C*----------------------------------------------------*
228600090731     C*  richiamo la variazione bolle per riportare la situazione iniziale
228700090731     C*----------------------------------------------------*
228800090731     C     srback        BEGSR
228900090731     c* trucco il profilo per scartare quelle automatiche che saranno con il
229000090731     C* profilo EDPCED
229100090731     c                   eval      kpjbas = kpjba
229200090731     c                   eval      sknmus = 'EDPCED'
229300090731     c     *loval        setll     wfvarb1l
229400090731     c                   do        *hival
229500090731     c                   read      wfvarb1l
229600090731     c                   if        %eof(wfvarb1l)
229700090731     c                   leave
229800090731     c                   end
229900131014     c     kwf           setgt     FIARBF2C
230000090731     c                   do        *hival
230100131014     c     kwf           readpe    FIARBF2C
230200131014     c                   if        %eof(fiarbf2c)
230300090731     c                   leave
230400090731     c                   end
230500090731     c* prendo le variazioni bolle fatte con il profilo fino alla data e ora
230600090731     c* della chiusura che sto facendo (escludo l'ultimo bite perchè
230700090731     c* c'è A x bolle arrivi e P per bolle in partenza)
230800090731     c                   if        %subst($ARBPRU: 1: 9) <>
230900090731     c                             %subst(knmus: 1: 9)
231000090731     c                   iter
231100090731     c                   end
231200090731     c                   movel     $arbdtv       com$arb
231300090731     c                   move      $arborv       com$arb
231400090731     c                   movel     varbdim       comvarb
231500090731     c                   move      varboim       comvarb
231600090731     c                   if        com$arb <comvarb
231700090731     c                   leave
231800090731     c                   end
231900090731     C                   CLEAR                   FNLR48
232000090731     C                   CLEAR                   DSARBD
232100090731     C                   CLEAR                   DSARBG
232200090731     C                   Z-ADD     $arbLNP       D48LNP
232300090731     C                   Z-ADD     $arbAAS       D48AAS
232400090731     C                   Z-ADD     $arbNRS       D48NRS
232500090731     C                   Z-ADD     $arbNSP       D48NSP
232600090731     C                   MOVEL     $arbcbo       D48CBO
232700090731     C                   MOVEL     $arbcvb       D48CVB
232800090731     C                   MOVEL     'E'           D48FFR
232900090731     C                   MOVEL     'G'           D48TRC
233000090731     C                   MOVEL     'A'           D48TBO
233100090731     C                   Z-ADD     $arbhcr       §BGHCR
233200090731     C                   Z-ADD     $arbdcr       §BGDCR
233300090731     C                   MOVEL     $arbtcr       §BGTCR
233400090731     C                   MOVEL     $arbgc1       §BGGC1
233500090731     C                   MOVEL     $arbgc2       §BGGC2
233600090731     C                   MOVEL     $arbtc1       §BGTC1
233700090731     C                   MOVEL     $arbtc2       §BGTC2
233800090731     C                   MOVEL     FNLR48        sKPJBU
233900090731     C                   CALL      'FNLR48R'
234000090731     C                   PARM                    KPJBAs
234100090731     C                   PARM                    DSARBD
234200090731     C                   PARM                    DSARBK
234300090731     C                   PARM                    DARBT
234400090731     C                   PARM                    DSARBG
234500090731     C                   PARM                    trul90ds
234600090731     C                   MOVEL     sKPJBU        FNLR48
234700090731     C     D48ERR        IFEQ      *BLANKS
234800090731     c* verifico se devo ricreare il fiarp00f
234900090731     c                   if        varbflr <> ' '
235000090731     c     kvar          chain(n)  fiarp01l
235100090731     c                   if        not %found(fiarp01l)
235200090731     c                   movel     varbflr       fiarpds
235300090731     c                   write     fiarp000
235400090731     c                   end
235500090731     c                   end
235600090731     c                   end
235700090731     c                   enddo
235800090731     c                   delete    wfvarb1l
235900090731     c                   enddo
236000090731     C*
236100090731     C                   ENDSR
236200090731     C*----------------------------------------------------*
236300090731     C*  richiamo la variazione bolle se è stato cambiato il motivo dopo variazione
236400090731     C*----------------------------------------------------*
236500090731     C     srback2       BEGSR
236600090731     c* trucco il profilo per scartare quelle automatiche che saranno con il
236700090731     C* profilo EDPCED
236800090731     c                   eval      kpjbas = kpjba
236900090731     c                   eval      sknmus = 'EDPCED'
237000090804     c     *loval        setll     wfvarb1l
237100090731     c                   do        *hival
237200090804     c                   read      wfvarb1l
237300090731     c                   if        %eof(wfvarb1l)
237400090731     c                   leave
237500090731     c                   end
237600110418     c*                  if        varbtrd = '016' or varbtrd = '023'
237700110418     c                   if        varbflr = *blank and
237800110418     c                             (varbtrd = '016' or varbtrd = '023')
237900090804     c     kwf1          chain(n)  fnarb01l
238000090804     c                   if        %found(fnarb01l) and
238100090804     c                             (arbcmc <> 'A16' and arbcmc <> 'A23')
238200131014     c     kwf           setgt     FIARBF2C
238300090731     c                   do        *hival
238400131014     c     kwf           readpe    FIARBF2C
238500131014     c                   if        %eof(fiarbf2c)
238600090731     c                   leave
238700090731     c                   end
238800090731     c* prendo le variazioni bolle fatte con il profilo fino alla data e ora
238900090731     c* della chiusura che sto facendo (escludo l'ultimo bite perchè
239000090731     c* c'è A x bolle arrivi e P per bolle in partenza)
239100090731     c                   if        %subst($ARBPRU: 1: 9) <>
239200090731     c                             %subst(knmus: 1: 9)
239300090731     c                   iter
239400090731     c                   end
239500090731     c                   movel     $arbdtv       com$arb
239600090731     c                   move      $arborv       com$arb
239700090731     c                   movel     varbdim       comvarb
239800090731     c                   move      varboim       comvarb
239900090731     c                   if        com$arb <comvarb
240000090731     c                   leave
240100090731     c                   end
240200090731     C                   CLEAR                   FNLR48
240300090731     C                   CLEAR                   DSARBD
240400090731     C                   CLEAR                   DSARBG
240500090731     C                   Z-ADD     $arbLNP       D48LNP
240600090731     C                   Z-ADD     $arbAAS       D48AAS
240700090731     C                   Z-ADD     $arbNRS       D48NRS
240800090731     C                   Z-ADD     $arbNSP       D48NSP
240900090731     C                   MOVEL     $arbcbo       D48CBO
241000090731     C                   MOVEL     $arbcvb       D48CVB
241100090731     C                   MOVEL     'E'           D48FFR
241200090731     C                   MOVEL     'G'           D48TRC
241300090731     C                   MOVEL     'A'           D48TBO
241400090731     C                   Z-ADD     $arbhcr       §BGHCR
241500090731     C                   Z-ADD     $arbdcr       §BGDCR
241600090731     C                   MOVEL     $arbtcr       §BGTCR
241700090731     C                   MOVEL     $arbgc1       §BGGC1
241800090731     C                   MOVEL     $arbgc2       §BGGC2
241900090731     C                   MOVEL     $arbtc1       §BGTC1
242000090731     C                   MOVEL     $arbtc2       §BGTC2
242100090731     C                   MOVEL     FNLR48        sKPJBU
242200090731     C                   CALL      'FNLR48R'
242300090731     C                   PARM                    KPJBAs
242400090731     C                   PARM                    DSARBD
242500090731     C                   PARM                    DSARBK
242600090731     C                   PARM                    DARBT
242700090731     C                   PARM                    DSARBG
242800090731     C                   PARM                    trul90ds
242900090731     C                   MOVEL     sKPJBU        FNLR48
243000090731     C     D48ERR        IFEQ      *BLANKS
243100090731     c                   end
243200090731     c                   enddo
243300090731     c                   endif
243400090804     c                   endif
243500090731     c                   enddo
243600090731     C*
243700090731     C                   ENDSR
243800071026     C*----------------------------------------------------*
243900071026     C*  AGGIORNO TOTALI
244000071026     C*----------------------------------------------------*
244100071026     C     AGGTO2        BEGSR
244200941117      *
244300941123     C     V2CNRD        IFGT      *ZEROS
244400941123     C                   MOVEL     'S'           WTRIMP
244500941123     C                   END
244600941117     C     V02STP        IFEQ      *ZEROS
244700941117     C                   ADD       1             V05STP
244800941117     C                   ELSE
244900941117     C     V02STP        LOOKUP    STP                                    95
245000941117     C     *IN95         IFEQ      '0'
245100941117     C                   ADD       1             V05STP
245200941117     C                   ADD       1             F
245300941117     C                   MOVE      V02STP        STP(F)
245400941117     C                   END
245500941117     C                   END
245600941117     C*
245700941118     C*  Se i record degli incassi erano stati in precedenza bloccati
245800941118     C*  provvedo a sbloccarli
245900941118     C     V2CBLO        IFEQ      'SI'
246000941118     C                   Z-ADD     NRR2          SAVREC
246100941207     C                   SETOFF                                       03
246200941118     C     V2CNRD        DO        V2CNRA        NRR3
246300120228     C     NRR3          CHAIN     lr53S03                            95
246400941118     C     *IN95         IFEQ      '0'
246500991007     C     V3DTPI        IFNE      'Ass'
246600991007     C     V3DTPI        ANDNE     'C/A'
246700991007     C                   SETON                                        05
246800991007     C                   ELSE
246900991007     C                   SETOFF                                       05
247000991007     C                   END
247100941118     C                   SETON                                        22
247200120308     C                   SETOFF                                       82
247300941129     C                   MOVEL     '  '          V3CNCO
247400941118     C                   MOVEL     ' '           V02SN1
247500941118     C                   MOVEL     '  '          V2CBLO
247600120308     c     v3hkey        comp      ' '                                8787
247700941118     C                   UPDATE    LR53S03
247800941118     C                   SETOFF                                       22
247900941207     C                   SETON                                        03
248000941118     C                   END
248100941118     C                   END
248200941118     C                   Z-ADD     SAVREC        NRR2
248300941124     C     NRR2          CHAIN     LR53S02                            95
248400941118     C                   END
248500941129     C*
248600941129     C                   MOVE      D53DSF        VIDDES
248700941118     C*
248800941117     C                   ENDSR
248900141201ab   C*------------------------------------------------------*
249000141201ab   C* cerca il CET per verificare la consegna
249100141201ab   C*------------------------------------------------------*
249200141201ab   C     Found_CET     BEGSR
249300141201ab   C*
249400141202ab   c                   clear                   ecco_il_CET       1
249500141201ab   C*  solo se PDA
249600141201ab   c                   clear                   fipctcetds
249700141201ab   c                   z-add     vnmdoc        vnmdoc6
249800141201ab   c                   move      'CET'         ttrd
249900141201ab   C*
250000141201ab   c     kcet          setgt     fipct02l
250100141201ab   c     kcet          readpe    fipct02l
250200141201ab   C*
250300141201ab   c                   if        not %eof(fipct02l)
250400141202ab   c                   eval         ecco_il_CET = 'S'
250500141201ab   c                   movel     pctdati       fipctcetds
250600141201ab   c                   endif
250700141201ab   C*
250800141201?ab  C                   ENDSR
250900141210ab   C*------------------------------------------------------*
251000141210ab   C* cerca la presenza di una R.A.(60)
251100141210ab   C*------------------------------------------------------*
251200141211ab   C     Found_RA_60   BEGSR
251300141210ab   C*
251400141210ab   c                   clear                   ecco_la_RA_60     1
251500141212ab   c                   clear                   autorizzata       1
251600141212ab   C*
251700141212ab   c                   eval      tgdTOR = 'S'
251800141212ab   c                   eval      tgdOGG = %editc(v2clnp:'X') +
251900141212ab   c                                      %editc(v2cnrs:'X') +
252000141212ab   c                                      %editc(v2cnsp:'X') +
252100141212ab   c                                      %editc(v2caas:'X')
252200141212ab   c                   eval      tgdMAD = ' 60'
252300141212ab    *
252400141212ab    * CERCA l'ultima inserita
252500141212ab    *  cronologicamente
252600141212ab   c     Key_TGD11     setgt     fitgd11l
252700141212ab   c     Key_TGD11     readpe    fitgd11l
252800141212ab   c                   dow       not %EoF(fitgd11l)
252900141212ab    *
253000141212ab   c                   eval        tgRANT = tgDANT
253100141212ab   c                   eval        tgRNUT = tgDNUT
253200141212ab   c                   eval        tgRPDT = tgDPDT
253300141212ab   c                   eval        tgRTRI = 'D'
253400141212ab   c     Key_TGR01     chain     fitgr01l
253500141212ab    *
253600141212ab    * se l'ha trovata ed è della distinta che sta trattando
253700141212ab    *  allora è proprio quello che stava cercando.
253800141212ab   c                   if        %Found(fitgr01l)
253900141212ab   c                   eval      dTGR01 = tgRRIF
254000141212ab    *
254100141212ab    *  Deve essere della stessa Distinta
254200141212ab    *      per poter verificare se Autorizzato
254300141212ab    *   altrimenti dovrà leggere una nuova testata.
254400141212ab   c                   if        §TGRNFV =  vnmdoc
254500141212ab   c                   eval       ecco_la_RA_60 = 'S'
254600141212ab    *
254700141212ab    *  se l'esito è Positivo
254800141212ab   c                   if        §TGRESITO = 'P'
254900141212ab    *  allora è autorizzata la consegna
255000141212ab   c                   eval       autorizzata   = 'S'
255100141212ab   c                   end
255200141212ab    * ed
255300141212ab    *  ESCE con i flag impostati
255400141212ab   c                   leave
255500141212ab   c                   end
255600141212ab    *
255700141212ab   c                   end
255800141212ab    *
255900141212ab    *  altrimenti dev provare con un'altra testata se c'è
256000141212ab   c     Key_TGD11     readpe    fitgd11l
256100141212ab   c                   enddo
256200141212ab    *
256300141210?ab  C                   ENDSR
256400141210ab   C*------------------------------------------------------*
256500141211ab   C* FINESTRA di alert e Forzatura
256600141210ab   C*------------------------------------------------------*
256700141211ab   C     WIN_FORZA     BEGSR
256800141212ab    **
256900141211ab   c                   clear                   fnlrq0ds
257000141211ab   c                   eval      lrq0aasI =  v2caas
257100141211ab   c                   eval      lrq0lnpI =  v2clnp
257200141211ab   c                   eval      lrq0nrsI =  v2cnrs
257300141211ab   c                   eval      lrq0nspI =  v2cnsp
257400141211ab   c                   eval      lrq0txtI =
257500141215ab   c                             'La spedizione è stata consegnata senza l''i-
257600141215ab   c                             mmisione del PINCODE.  F7 per procedere'
257700141211ab   c                   eval      lrq0tipI = 'PIN'
257800141211ab   c                   eval      lrq0pgmI = 'FNLR53R'
257900141211ab   c                   eval      lrq0frzIO= 'F'
258000141211ab   c                   call      'FNLRQ0R'
258100141211ab   c                   parm                    fnlrq0ds
258200141212ab    **
258300141211?ab  C                   ENDSR
258400141211ab   C*------------------------------------------------------*
258500141212ab   C* FINESTRA di INFO x apporre la dicitura o il Timbro sulla LDV
258600141211ab   C*------------------------------------------------------*
258700141211ab   C     WIN_INFO      BEGSR
258800141212ab    **
258900141211ab   c                   clear                   fnlrq0ds
259000141211ab   c                   eval      lrq0aasI =  v2caas
259100141211ab   c                   eval      lrq0lnpI =  v2clnp
259200141211ab   c                   eval      lrq0nrsI =  v2cnrs
259300141211ab   c                   eval      lrq0nspI =  v2cnsp
259400141211ab   c                   eval      %subst(lrq0txtI:1:50) =
259500141215ab   c                             'Apporre sulla LDV la scritta:'
259600141211ab   c                   eval      %subst(lrq0txtI:51:50) =
259700141211ab   c                             '"PINCODE verificato telefonicamente"'
259800141211ab   c                   eval      lrq0tipI = 'PIN'
259900141211ab   c                   eval      lrq0pgmI = 'FNLR53R'
260000141211ab   c                   eval      lrq0frzIO= 'I'
260100141211ab   c                   call      'FNLRQ0R'
260200141211ab   c                   parm                    fnlrq0ds
260300141212ab    **
260400141211?ab  C                   ENDSR
260500141211ab   C*------------------------------------------------------*
260600141211ab   C* scrive R.A.(61) dopo aver Forzato la Consegna (con PC PINCode)
260700141211ab   C*------------------------------------------------------*
260800141211ab   C     Write_RA_61   BEGSR
260900141212ab    **
261000141212ab    **    Data Distinta in *EUR (ddmmyyyy)
261100141212ab   c                   move      vh2ddt        dataiso
261200141212ab   c                   move      dataiso       dataeur
261300141212ab   c                   move      dataeur       gmaDDT            8 0
261400141212ab    **  Filiale
261500141212ab   c                   if        d53fgs = 0
261600141212ab   c                   z-add     d53lna        Filiale           3 0
261700141212ab   c                   else
261800141212ab   c                   z-add     d53fgs        Filiale
261900141212ab   c                   end
262000141212ab    **
262100141211ab    *    imposta la DS di scrittura
262200141212ab   c                   clear                   fidna6ds
262300141212ab   c                   eval      ida06mad = ' 61'
262400141212ab   c                   eval      ida06tor = 'S'
262500141212ab   c                   eval      ida06ogg = %editc(v2clnp:'X') +
262600141212ab   c                                        %editc(v2cnrs:'X') +
262700141212ab   c                                        %editc(v2cnsp:'X') +
262800141212ab   c                                        %editc(v2caas:'X')
262900141212ab   c                   eval      ida06ute = uteute
263000141212ab   c                   eval      ida06fil = utefil
263100141212ab   c                   eval      ida06no2 =
263200141218ab   c                             'DIST: ' + %editc(Filiale:'X')+ ' '  +
263300141218ab   c                                  %trim(%editc(vnmdoc:'Z'))+
263400141218ab   c                             ' DEL ' + %editw(gmaDDT:'  /  /    ') +
263500141218ab   c                             '  AUT: ' + %editc(vh2pdr:'X')
263600141212ab   c                   eval      ida06rsc = 'Automatica da Chius.Dist.'
263700141212ab   c                   call      'FIDNA6R'
263800141212ab   c                   parm                    fidna6ds
263900141210ab   C*
264000141210?ab  C                   ENDSR
264100941117     C*------------------------------------------------------*
264200941117     C* GESTIONE FINESTRA PER GIORNI CHIUSURA DESTINATARIO
264300941117     C*------------------------------------------------------*
264400941117?    C     TURNO         BEGSR
264500941117     C*
264600101020     C                   SETOFF                                       32
264700941128     C                   MOVE      V2CAAS        V9CAAS
264800941128     C                   MOVE      V2CLNP        V9CLNP
264900941128     C                   MOVE      V2CNRS        V9CNRS
265000941128     C                   MOVE      V2CNSP        V9CNSP
265100941117     C                   MOVEL     *BLANKS       WINMSG
265200141202      ***
265300101123      * se distinta pda imposta eventuali turni rilevati dall'autista su pda
265400101123     c                   if        wpda = 'S'
265500141202      ***
265600101123     c                   exsr      impturno
265700141202      ***
265800101123      * turno trovato da impostare verifico fra primo e secondo quale è libero se
265900101123      * pieni entrambi copro il primo
266000101123     c                   if        wtur <> *blank
266100130709      * verifica che il turno inserito dal PDA non sia già presente
266200130709     c                   if        wtur <> w1gc and
266300130709     c                             wtur <> w2gc
266400101123     c                   if        w1gc = *blank
266500101123     c                   move      wtur          w1gc
266600101123     c                   else
266700101123     c                   if        w2gc = *blank
266800101123     c                   move      wtur          w2gc
266900101123     c                   else
267000101123     c                   move      wtur          w1gc
267100101123     c                   endif
267200130709     c                   endif
267300130709      * se inserito il turno verifica il primo bite dei campi se è uguale
267400130709      * pulisce il secondo campo e il secondo byte del primo
267500130709      * esempio se ci trovassimo con 1M e 1P vengono trasformati in '1 ' e '  '
267600130709     c                   if        %subst(w1gc:1:1) = %subst(w2gc:1:1)
267700130709     c                   clear                   w2gc
267800130709     c                   move      ' '           w1gc
267900130709     c                   endif
268000130709     c                   endif
268100101123     c                   endif
268200101123     c                   endif
268300101123      *
268400941117     C     *IN32         DOUEQ     '0'
268500941128     C   32              EXFMT     LR53W08
268600941125     C                   EXFMT     LR53W09
268700941117     C                   SETOFF                                       32
268800941117     C                   MOVEL     *BLANKS       WINMSG
268900941117     C  NKL              DO
269000941117     C     W1GC          IFEQ      *BLANKS
269100941117     C     W2GC          ANDEQ     *BLANKS
269200941117     C     V021GC        ANDEQ     *BLANKS
269300941117     C     V022GC        ANDEQ     *BLANKS
269400941117     C                   SETON                                        32
269500941125     C                   MOVEL     ERR(54)       WINMSG
269600941117     C                   END
269700941117     C     DSG1          IFGT      '7'
269800941117     C     DSG2          ORGT      '7'
269900941117     C     DSG1          ORLT      '1'
270000941117     C     DSG1          ANDNE     *BLANKS
270100941117     C     DSG2          ORLT      '1'
270200941117     C     DSG2          ANDNE     *BLANKS
270300941117     C                   SETON                                        32
270400941125     C                   MOVEL     ERR(54)       WINMSG
270500941117     C                   END
270600941117     C     DSMP1         IFNE      *BLANKS
270700941117     C     DSMP1         ANDNE     'P'
270800941117     C     DSMP1         ANDNE     'M'
270900941117     C     DSMP2         ORNE      *BLANKS
271000941117     C     DSMP2         ANDNE     'P'
271100941117     C     DSMP2         ANDNE     'M'
271200941117     C                   SETON                                        32
271300941125     C                   MOVEL     ERR(54)       WINMSG
271400941117     C                   END
271500941117     C     W1GC          IFEQ      W2GC
271600941117     C     W1GC          ANDNE     *BLANKS
271700941117     C                   SETON                                        32
271800941125     C                   MOVEL     ERR(54)       WINMSG
271900941117     C                   END
272000941117     C                   END
272100941117     C                   END
272200941117     C*
272300941117     C* Memorizzo numero relativo di record dove avevo richiesto la finestra
272400140313     c                   move      w1gc          v2cgc1
272500140313     c                   move      w2gc          v2cgc2
272600941117     C                   Z-ADD     NRR2          NR2KEY
272700941117     C*
272800941117?    C                   ENDSR
272900101123     C*------------------------------------------------------*
273000101123     C* reperisce turni chiusura rilevati da autista su pda
273100101123     C*------------------------------------------------------*
273200101123     C     impturno      BEGSR
273300101123     c                   clear                   wtur              2
273400101123      * non consegnato
273500140219     c                   z-add     1             tur               3 0
273600101123     c     §pctcmc       lookup    c2a(tur)                               92
273700101123     c                   if        *in92
273800101123     c                   movel     d2a(tur)      ds2atur
273900101124     c                   if        y§2acpt = 'S'
274000101123     c                   exsr      impgior
274100101123     c                   movel     §pctcmc       wtur
274200101123     c                   else
274300101123      * tentativo precedente
274400140219     c                   z-add     1             tur               3 0
274500101123     c     §pctcmcp      lookup    c2a(tur)                               92
274600101123     c                   if        *in92
274700101123     c                   movel     d2a(tur)      ds2atur
274800101124     c                   if        y§2acpt = 'S'
274900101123     c                   exsr      impgior
275000101123     c                   movel     §pctcmcp      wtur
275100101123     c                   endif
275200101123     c                   endif
275300101123     c                   endif
275400101123     c                   endif
275500101123     c                   movel     Wgioset       wtur
275600101123     C*
275700101123     c                   endsr
275800101123     C*------------------------------------------------------*
275900101123     C* ricerca giorno della settimana
276000101123     C*------------------------------------------------------*
276100101123     C     impgior       BEGSR
276200101123     c     *iso          move      vh2ddc        wData_Dmy
276300101123      * Reperimento del giorno della settimana da stampare
276400101123     c                   move      wData_Dmy     wData_6
276500101123     c                   move      wData_6       Wggmmaa
276600101123     c                   call      'XGIOSE1'
276700101123     c                   parm                    Wggmmaa           6
276800101123     c                   parm      *blanks       Wgioset           1
276900101123      *
277000101123     c                   endsr
277100040216     C*------------------------------------------------------*
277200040216     C* GESTIONE FINESTRA PER data consegna richiesta
277300040216     C*------------------------------------------------------*
277400040216     C     vidrich       BEGSR
277500040216     C*
277600040216     C                   MOVE      V2CAAS        V9CAAS
277700040216     C                   MOVE      V2CLNP        V9CLNP
277800040216     C                   MOVE      V2CNRS        V9CNRS
277900040216     C                   MOVE      V2CNSP        V9CNSP
278000040216     C                   MOVEL     *BLANKS       WINMSG
278100101015     c
278200101015     c*                  if        *in66
278300101015     c*                  move      v13tcr        w13doil
278400101015     c*                  endif
278500101021     c                   movel     v2ctcr        v13tcr
278600101021     c                   if        v13tcr = *blank
278700101021     c                   move      '  Il'        v13tcrd
278800101021     c                   else
278900101021     c                   move      'Dopo'        v13tcrd
279000101021     c                   endif
279100101021     c*
279200040220     c                   if        v2cdcr > 0
279300040220     C                   z-add     0             G02DAT
279400040220     C                   Z-ADD     v2cdcr        G02INV
279500040220     C                   MOVEL     '3'           G02ERR
279600040220     C                   CALL      'XSRDA8'
279700040220     C                   PARM                    WLBDA8
279800040220     C                   z-add     G02DAT        v13dcr
279900101214     C*                  z-add     G02inv        w13dcrg
280000101015     c                   movel     v2chcr        v13hcr
280100040220     c                   else
280200040217     c                   clear                   v13dcr
280300101214     c*                  clear                   w13dcrg
280400101015     c                   clear                   v13hcr
280500101015     c                   setoff                                       66
280600040220     c                   endif
280700101021     c*
280800040216     C                   DO        *hival
280900040216     C   32              EXFMT     LR53W08
281000040216     C                   EXFMT     LR53W13
281100040330     c                   setoff                                       32
281200040330     C                   MOVEL     *BLANKS       WINMSG
281300040216
281400040216     C     v13dcr        IFEQ      0
281500040216     C                   SETON                                        32
281600040216     C                   MOVEL     ERR(90)       WINMSG
281700040216     c                   iter
281800040216     C                   END
281900040216     C     v13dcr        IFGT      0
282000040216     C                   MOVE      v13dcr        G02DAT
282100040216     C                   Z-ADD     0             G02INV
282200040216     C                   MOVEL     *BLANKS       G02ERR
282300040216     C                   CALL      'XSRDA8'
282400040216     C                   PARM                    WLBDA8
282500040216     C     G02ERR        IFEQ      '1'
282600040216     C                   SETON                                          32
282700040216     C                   MOVEL     ERR(90)       winmsg
282800040216     C                   iter
282900040216     C                   ELSE
283000101214     C*                  Z-ADD     G02INV        W13dcrg           8 0
283100040216     C                   MOVEL     G02DAT        V13dcr
283200040216     C                   END
283300101015     C*                  IF        w13dcrg< oggi
283400101015     C                   IF        g02inv < oggi
283500040216     C                   SETON                                        32
283600040216     C                   MOVEL     ERR(91)       WINMSG
283700040216     c                   iter
283800040216     C                   END
283900101015     C*                  IF        w13dcrg > wvpodcr
284000101015     C                   IF        g02inv  > wvpodcr
284100040330     C                   SETON                                        32
284200040330     C                   MOVEL     ERR(92)       WINMSG
284300040330     c                   iter
284400040330     C                   END
284500040216     C                   END
284600040702      *uscita solo con data valorizzata
284700101015     c*                  if        *inkj and w13dcrg > 0 and not *in32
284800101015     c                   if        *inkj and g02inv > 0 and not *in32
284900101021     c                   eval      v2cdcr = g02inv
285000040702     c                   leave
285100040702     c                   endif
285200040216     c                   enddo
285300040216     C*
285400040216     C* Memorizzo numero relativo di record dove avevo richiesto la finestra
285500040216     C                   Z-ADD     NRR2          NR2KEY
285600040216     C*
285700040216?    C                   ENDSR
285800941117     C*------------------------------------------------------*
285900941117     C* GESTIONE DEL FORMATO DEI RIMBORSI E NOTE
286000941117     C*------------------------------------------------------*
286100941117?    C     GESRMB        BEGSR
286200941117     C*
286300941117     C* Valorizzo i campi dal subile
286400941117     C                   MOVEL     V2CFIR        V4CFIR
286500941117     C                   MOVEL     V2CNT1        V4CNT1
286600101111     C                   MOVEL     V2Clnp        V4Clnp
286700101111     C                   MOVEL     V2Cnrs        V4Cnrs
286800101111     C                   MOVEL     V2Cnsp        V4Cnsp
286900101111     C                   MOVEL     V02des        V4Cdes
287000990923     C                   SETOFF                                       28
287100941117     C* Gestione videata
287200941117     C                   EXSR      GESD04
287300941117     C*
287400941117     C* VALORIZZO I CAMPI DEL SUBFILE
287500941125     C                   SETOFF                                       28
287600941117     C     *INKL         IFEQ      '0'
287700941117     C                   MOVE      V4CFIR        V2CFIR
287800941117     C                   MOVE      V4CNT1        V2CNT1
287900941117     C                   END
288000941117     C*
288100941117?    C                   ENDSR
288200941117     C*------------------------------------------------------*
288300941117     C* GESTIONE VIDEATA RIMBORSI/NOTE
288400941117     C*------------------------------------------------------*
288500941117?    C     GESD04        BEGSR
288600941117     C*
288700941123     C                   EXFMT     LR53D04
288800990923     C                   SETOFF                                       28
288900941125     C* Fine Rimborsi
289000941125     C   KL              GOTO      FINVD4
289100090216     C                   if        *inkr
289200140210     c                   clear                   fnlrn6ds
289300090216     C                   movel     kpjbu         WSVKPJ          256
289400140210     C                   move      'I'           in6mod
289500140210     C                   z-add     V2CAAS        in6AAS
289600140210     C                   z-add     V2CLNP        in6LNP
289700140210     C                   z-add     V2CNRS        in6NRS
289800140210     C                   z-add     V2CNSP        in6NSP
289900140210     C                   movel     v4cnt1        IN6NOT1
290000140210     C                   movel     'FNLR53R'     in6pgm
290100140210     C                   movel     'S'           IN6CHDIST
290200140210     C                   call      'FNLRN6R'                            98
290300090216     C                   parm                    kpjba
290400140210     C                   parm                    fnlrn6ds
290500090216     C                   movel     WSVKPJ        kpjbu
290600090216     C                   MOVEL     ' '           V02SEL
290700090216     C                   Z-ADD     NRR2          NR2KEY
290800090216     c                   endif
290900100223     C                   MOVE      'X'           V02SELb
291000941117     C*
291100941117     C     FINVD4        ENDSR
291200110908     C*------------------------------------------------------*
291300110908     C* GESTIONE DEL FORMATO 7 visualizza particolarità consegna documenti
291400110908     C*------------------------------------------------------*
291500110908?    C     GESvid7       BEGSR
291600110908     C*
291700110908     C* Valorizzo i campi dal subile
291800110908     C                   MOVEL     V2Clnp        V7Clnp
291900110908     C                   MOVEL     V2Cnrs        V7Cnrs
292000110908     C                   MOVEL     V2Cnsp        V7Cnsp
292100110908     C                   MOVEL     V02des        V7Cdes
292200110908     c                   movel     vh2rsd        v7dest
292300110908     c                   movel     vh2rsm        v7mitt
292400110908     c                   exsr      RepDesPar
292500110908     C* Gestione videata
292600110908     C                   EXSR      GESvid7b
292700110908     C*
292800110908     C*
292900110908?    C                   ENDSR
293000110908     C*------------------------------------------------------*
293100110908     C* GESTIONE VIDEATA con particolarità consegna x documenti
293200110908     C*------------------------------------------------------*
293300110908?    C     GESvid7b      BEGSR
293400110908     C*
293500110908     C                   EXFMT     LR53D07
293600110908     C                   SETOFF                                       28
293700110908     C                   Z-ADD     NRR2          NR2KEY
293800110909     C                   MOVE      'D'           V02SELd
293900110908     C*
294000110914     C                   ENDSR
294100110908     C*------------------------------------------------------*
294200110908?    C* reperimento descrizione particolarità
294300110908     C*------------------------------------------------------*
294400110908?    C     repdespar     BEGSR
294500110908     c                   clear                   v7despa
294600110908     C                   MOVEL     '7V'          KCOD
294700110908     C                   eval      KKEY = 'R' + v2cgma + 'V0001'
294800110908     C     KTAB2         setll     TABEL00F
294900110908     c                   do        *hival
295000110908     c                   read      tabel00f
295100110908     c                   if        %eof(tabel00f) or
295200110908     c                             %subst(tblkey:2:2) <> v2cgma
295300110908     c                   leave
295400110908     c                   endif
295500110908     c                   movel     tbluni        ds7v
295600110908     c                   eval      v7despa =%trim(v7despa)+ §7vnot
295700110908     C                   enddo
295800110908
295900110908     C                   endsr
296000941117     C*------------------------------------------------------*
296100941117     C* GESTIONE CONSEGNA PARZIALE
296200941117     C*------------------------------------------------------*
296300941117?    C     GESPAR        BEGSR
296400941117     C*
296500970304     C                   SETON                                        6301
296600941123     C                   WRITE     LR53C06
296700941117     C                   Z-ADD     0             NRR6
296800941128     C                   SETOFF                                       6388
296900941117     C*
297000941117     C     V2CPDS        IFEQ      0
297100941117?    C                   EXSR      CARS06
297200941117     C                   ELSE
297300941117?    C                   EXSR      RCRS06
297400941117     C                   END
297500941117     C*
297600941117     C     NRR6          IFNE      0
297700941205     C     *INKA         DOUNE     '0'
297800941205     C     *INKL         ORNE      '0'
297900941205     C     *INKG         ORNE      '0'
298000941205     C     *INKQ         ORNE      '0'
298100941205     C     *IN88         DOUEQ     '0'
298200941117?    C                   EXSR      GESS06
298300941117     C                   END
298400941205     C                   END
298500941117     C                   ELSE
298600941117     C                   SETON                                        2840
298700941205     C                   MOVEL     ERR(62)       $MSG
298800941117     C                   END
298900941117     C*
299000941117     C* Memorizzo numero relativo di record dove avevo richiesto la finestra
299100941117     C                   Z-ADD     NRR2          NR2KEY
299200941123     C     NRR2          CHAIN     LR53S02                            89
299300941117     C     *INKL         IFEQ      '0'
299400941117     C     CONT          IFGT      0
299500941117     C     NRR6          ANDGT     0
299600941117     C     *INKL         ANDEQ     '0'
299700941117     C     *INKQ         ANDEQ     '0'
299800941117     C                   MOVEL     'P'           V02CPZ
299900941128     C                   ELSE
300000941128     C     *INKQ         IFEQ      '1'
300100941128     C                   MOVEL     ' '           V02CPZ
300200941128     C                   END
300300941128     C                   END
300400941117     C     V2CPDS        IFEQ      0
300500941117     C                   Z-ADD     POSIZ         V2CPDS
300600941117     C                   END
300700941117     C                   END
300800110117     C  nkl              MOVEL     *BLANKS       V02CCN
300900941117     C*
301000941117?    C                   ENDSR
301100970304     C*------------------------------------------------------*
301200970304     C* Colli persi definitivamente --> chiudo bolla con IDD
301300970304     C*------------------------------------------------------*
301400970620?    C     GESIDD        BEGSR
301500970304     C*
301600970304     C                   SETON                                        63
301700970304     C                   SETOFF                                       01
301800970304     C                   WRITE     LR53C06
301900970304     C                   Z-ADD     0             NRR6
302000970304     C                   SETOFF                                       6388
302100970304     C*
302200970304     C     V2CPDS        IFEQ      0
302300970304?    C                   EXSR      CARS06
302400970304     C                   ELSE
302500970304?    C                   EXSR      RCRS06
302600970304     C                   END
302700970304     C*
302800970304     C     NRR6          IFNE      0
302900970304     C     *INKA         DOUNE     '0'
303000970304     C     *INKL         ORNE      '0'
303100970304     C     *INKG         ORNE      '0'
303200970304     C     *INKQ         ORNE      '0'
303300970304     C     *IN88         DOUEQ     '0'
303400970304?    C                   EXSR      GESS06
303500970304     C                   END
303600970304     C                   END
303700970311     C                   ELSE
303800970304     C                   SETON                                        2840
303900970304     C                   MOVEL     ERR(67)       $MSG
304000970304     C                   END
304100970304     C*
304200970304     C* Memorizzo numero relativo di record dove avevo richiesto la finestra
304300970304     C                   Z-ADD     NRR2          NR2KEY
304400970304     C     NRR2          CHAIN     LR53S02                            89
304500970304     C     *INKL         IFEQ      '0'
304600970305     C     CONT          IFNE      TOTCAR
304700970304     C     NRR6          ANDGT     0
304800970304     C     *INKL         ANDEQ     '0'
304900970304     C     *INKQ         ANDEQ     '0'
305000970311     C                   MOVEL     §2AIDD        V2CIDD
305100970311     C                   MOVEL     '5'           V02CCA
305200970311     C                   MOVEL     V02CCN        V2CEVD
305300970304     C                   ELSE
305400970304     C     *INKQ         IFEQ      '1'
305500970311     C                   MOVEL     *BLANKS       V2CEVD
305600970311     C                   MOVEL     ' '           V2CIDD
305700970311     C                   MOVEL     ' '           V02CCA
305800970311     C                   SETOFF                                       75
305900970304     C                   END
306000970304     C                   END
306100970304     C     V2CPDS        IFEQ      0
306200970304     C                   Z-ADD     POSIZ         V2CPDS
306300970304     C                   END
306400970304     C                   END
306500970304     C                   MOVEL     *BLANKS       V02CCN
306600970304     C*
306700970311?    C                   ENDSR
306800941117     C*------------------------------------------------------*
306900941117     C* CARICAMENTO DETTAGLIO SEGNACOLLI DA ARCHIVIO
307000941117     C*------------------------------------------------------*
307100941118?    C     CARS06        BEGSR
307200980402      *
307300980402      * Se c'è legame utilizzo il legame per agganciare il dettaglio
307400941117     C                   SETOFF                                       15
307500980402     C                   Z-ADD     V2CAAS        V6CAAS
307600980402     C                   Z-ADD     V2CLNP        V6CLNP
307700980402     C                   Z-ADD     V2CNRS        V6CNRS
307800980402     C                   Z-ADD     V2CNSP        V6CNSP
307900980402     C                   Z-ADD     V2CFLS        SFWFLS
308000980402      *
308100941129     C                   Z-ADD     HARAAS        KAAS1
308200941129     C                   Z-ADD     HARLNP        KLNP1
308300941129     C                   Z-ADD     HARNRS        KNRS1
308400941129     C                   Z-ADD     HARNSP        KNSP1
308500941117     C     KART1         CHAIN     FNART000                           26
308600980402      *
308700941117     C     *IN26         DOWEQ     '0'
308800941128     C     ARTDAM        COMP      0                                      27
308900980402      *
309000941128     C     ARTDCM        IFEQ      0
309100941123     C                   Z-ADD     ARTNSC        SFWSCN
309200941117     C     ARTDAM        IFEQ      0
309300941117     C                   MOVEL     'N'           SFWARR
309400941117     C                   ELSE
309500941117     C                   MOVEL     *BLANKS       SFWARR
309600941117     C                   END
309700980402      * Per estero verifico se preimpostare la scelta
309800980402     C                   MOVEL     *BLANKS       SELEEX            1
309900980402     C     D53ITA        IFEQ      'E'
310000980402     C                   EXSR      SCES06
310100980402     C                   END
310200980402     C                   MOVEL     SELEEX        SFWSEL
310300080723      * distinte PDA preimposta scelta nel SFL
310400080723     c                   if        wpda = 'S'
310500081111     c                   clear                   SFWSEL
310600080723      * imposta scelta nel sfl Se: trova spunta 8
310700080723      * oppure non trova la spunta verifica dettaglio ritorno da pda
310800080723      * imposta scelta nel sfl Se: non trova dettaglio
310900080723      *                            trova il dettaglio ed è spuntato al rientro
311000080723      *                            trova il dettaglio e non è spuntato in consegna
311100080714     c                   z-add     vnmdoc        wdisbrv
311200080714     c     kbrv          chain     fnbrv01l
311300080714     c                   if        %found(fnbrv01l)
311400080714     c                   eval      SFWSEL= '1'
311500080723     c                   else
311600080805     c     kced          chain     fipcd03l
311700080805     c                   if        %found(fipcd03l)
311800080723     c                   movel     pcddati       fipcdcedds
311900080723     c                   if        §PCDSPUSPC <> *blank or
312000080723     c                             §PCDSPUcon <> 'S'
312100080723     c                   eval      SFWSEL= '1'
312200080723     c                   endif
312300101115     c                   else
312400101115     c                   eval      SFWSEL= '1'
312500080723     c                   endif
312600080714     c                   endif
312700080723     c                   endif
312800980402      *
312900941117     C                   Z-ADD     0             SFWNRH
313000941117     C                   ADD       1             NRR6
313100941118     C                   WRITE     LR53S06
313200941117     C                   END
313300980402      *
313400941117     C     KART1         READE     FNART000                               26
313500941117     C                   END
313600980402      *
313700941123     C                   Z-ADD     NRR6          TOTCAR                         TOT.COLLI CARICATI
313800980402      *
313900941117?    C                   ENDSR
314000980402      *------------------------------------------------------------------------*
314100980402      * PER ESTERO DETERMINO SE PREIMPOSTARE LA SELEZIONE PER DETTAGLIO SEGNAC.
314200980402      *------------------------------------------------------------------------*
314300980402?    C     SCES06        BEGSR
314400980402      *
314500980402      * Ricerco se esiste il segnacollo Euroexpress
314600980402     C                   Z-ADD     ARTFLS        KFLS
314700980402     C                   Z-ADD     ARTLNA        KLNA
314800980402     C                   Z-ADD     ARTNSC        KNSC
314900980402     C                   MOVEL     'E'           KTRC
315000051027     C     KARS0         CHAIN     FIARS000                           33
315100980402      *
315200980402     C     *IN33         IFEQ      '0'
315300980402      *
315400980402      * Ricerco se è fra quelli selezionati
315500980402     C                   Z-ADD     NDC(1)        KNDC
315600980402     C     KCDS0         CHAIN     FNCDS000                           33
315700980402      *
315800980402     C     *IN33         DOWEQ     '0'
315900980402     C     CDSSEE        IFEQ      ARSNOT
316000980402     C                   SETON                                        33
316100980402     C                   MOVEL     1             SELEEX
316200980402     C                   END
316300980402     C  N33KCDS0         READE     FNCDS000                               33
316400980402     C                   END
316500980402      *
316600980402     C                   END
316700980402      *
316800980402?    C                   ENDSR
316900980402     C*------------------------------------------------------*
317000980402     C* CARICAMENTO DETTAGLIO SEGNACOLLI DA SUB-FILE HIDDEN
317100980402     C*------------------------------------------------------*
317200980402?    C     RCRS06        BEGSR
317300980402     C*
317400941123     C                   Z-ADD     V2CAAS        V6CAAS
317500941123     C                   Z-ADD     V2CLNP        V6CLNP
317600941123     C                   Z-ADD     V2CNRS        V6CNRS
317700941123     C                   Z-ADD     V2CNSP        V6CNSP
317800941117     C                   Z-ADD     V2CFLS        SFWFLS
317900941117     C*
318000941123     C                   Z-ADD     V2CPDS        XX                4 0
318100941123     C     XX            CHAIN     LR53S07                            94
318200941117     C*
318300941117     C     XX            DOWLE     TOTSFH
318400941123     C     *IN94         ANDEQ     '0'
318500941117     C     HARAAS        ANDEQ     SFHAAS
318600941117     C     HARLNP        ANDEQ     SFHLNP
318700941117     C     HARNRS        ANDEQ     SFHNRS
318800941117     C     HARNSP        ANDEQ     SFHNSP
318900941117     C                   MOVEL     SFHSEL        SFWSEL
319000941117     C                   Z-ADD     SFHSCN        SFWSCN
319100941117     C                   MOVEL     SFHARR        SFWARR
319200941117     C     SFWARR        COMP      'N'                                    27
319300941117     C                   Z-ADD     XX            SFWNRH
319400941117     C                   ADD       1             NRR6
319500941117     C                   WRITE     LR53S06
319600941117     C                   ADD       1             XX
319700941123     C     XX            CHAIN     LR53S07                            94
319800941117     C                   END
319900941117     C*
320000941117     C                   Z-ADD     NRR6          TOTCAR                         TOT.COLLI CARICATI
320100941117     C*
320200941117?    C                   ENDSR
320300941117     C*------------------------------------------------------*
320400941117     C* AGGIORNAMENTO SUB-FILE HIDDEN DETTAGLIO SEGNACOLLI
320500000421     C*-------------------------
320600941118?    C     AGGS07        BEGSR
320700941117     C*
320800941117     C                   Z-ADD     0             POSIZ
320900941117     C                   Z-ADD     HARAAS        SFHAAS
321000941117     C                   Z-ADD     HARLNP        SFHLNP
321100941117     C                   Z-ADD     HARNRS        SFHNRS
321200941117     C                   Z-ADD     HARNSP        SFHNSP
321300941117     C                   Z-ADD     V2CFLS        SFHFLS
321400941123     C                   DO        TOTCAR        KJ
321500941117     C     KJ            CHAIN     LR53S06                            89
321600941117     C  N89V2CPDS        IFEQ      0
321700941117     C                   ADD       1             NRR7
321800941123     C                   ADD       1             TOTSFH
321900941117     C     POSIZ         IFEQ      0
322000941123     C                   Z-ADD     NRR7          POSIZ
322100941117     C                   END
322200941128     C                   MOVEL     SFWSEL        SFHSEL
322300941128     C                   Z-ADD     SFWSCN        SFHSCN
322400941128     C                   MOVEL     SFWARR        SFHARR
322500941117     C                   WRITE     LR53S07
322600941117     C                   ELSE
322700941123     C     SFWNRH        CHAIN     LR53S07                            89
322800941117     C   KQ              MOVEL     *BLANKS       SFWSEL
322900941117     C                   MOVEL     SFWSEL        SFHSEL
323000941117     C                   Z-ADD     SFWSCN        SFHSCN
323100941117     C                   MOVEL     SFWARR        SFHARR
323200941117     C  N89              UPDATE    LR53S07
323300941117     C                   END
323400941117     C                   END
323500941117     C*
323600941117?    C                   ENDSR
323700941117     C*------------------------------------------------------*
323800941117     C* GESTIONE SUB-FILE DETTAGLIO SEGNACOLLI
323900941117     C*------------------------------------------------------*
324000941117?    C     GESS06        BEGSR
324100941117     C*
324200941128     C   88              EXFMT     LR53W08
324300941117     C                   WRITE     LR53Z06
324400941117     C                   EXFMT     LR53C06
324500941117     C                   SETOFF                                       272888
324600941117     C*
324700941117     C   KL              GOTO      FINVD6
324800970305     C* Tutti: seleziono tutti i record del subfile
324900970805     C     *INKB         IFEQ      '1'
325000970805     C                   Z-ADD     1             NRR6
325100970805     C     NRR6          CHAIN     LR53S06                            89
325200970805     C     *IN89         DOWEQ     '0'
325300970805     C                   MOVEL     '1'           SFWSEL
325400970805     C                   UPDATE    LR53S06
325500970805     C                   ADD       1             NRR6
325600970805     C     NRR6          CHAIN     LR53S06                            89
325700970805     C                   END
325800970805     C                   GOTO      FINVD6
325900970805     C                   END
326000970305     C*
326100941123     C                   Z-ADD     0             CONT
326200941123     C                   Z-ADD     1             JX                4 0
326300941118     C     JX            CHAIN     LR53S06                            89
326400941117     C     *IN89         DOWEQ     '0'
326500941123     C     *IN88         ANDEQ     '0'
326600941117     C     JX            ANDLE     TOTCAR
326700941128     C     SFWSEL        IFNE      '1'
326800941117     C                   ADD       1             CONT
326900941117     C     SFWARR        IFEQ      'N'
327000970805     C     §2AIDD        ANDNE     'P'
327100941117     C  NKG
327200941117     CANNKQ              SETON                                        282788
327300941128     C  NKG
327400941128     CANNKQ              MOVEL     ERR(56)       WINMSG
327500941117     C                   END
327600941118     C                   UPDATE    LR53S06
327700941117     C                   END
327800941117     C                   ADD       1             JX
327900941118     C     JX            CHAIN     LR53S06                            89
328000941117     C                   END
328100941117     C*
328200970620     C  NKQ
328300970620     CANN88
328400970620     CAN 01CONT          IFEQ      TOTCAR
328500941117     C     CONT          OREQ      0
328600941117     C                   SETON                                            88    SE CONS. TUTTI ERROR
328700941128     C                   MOVEL     ERR(57)       WINMSG
328800941117     C                   END
328900970805     C  NKQ
329000970805     CANN88
329100970805     CANN01CONT          IFEQ      TOTCAR
329200970805     C                   SETON                                            88    SE NESSUNO ERRORE
329300970805     C                   MOVEL     ERR(69)       WINMSG
329400970805     C                   END
329500970805     C  NKQ
329600970805     CANN88
329700970805     CANN01CONT          IFEQ      0
329800970805     C     §2AIDD        ANDEQ     'P'
329900970805     C                   SETON                                            88    SE NESSUNO ERRORE
330000970805     C                   MOVEL     ERR(71)       WINMSG
330100970805     C                   END
330200941117     C* Se richiesto annullamento consegna parziale controllo che lo fosse
330300970304     C   KQ
330400970304     CAN 01V02CPZ        IFNE      'P'
330500941117     C                   SETON                                          88
330600941128     C                   MOVEL     ERR(58)       WINMSG
330700941117     C                   END
330800970304     C* Se richiesto annullamento pratica IDD controllo di averla immessa
330900970805     C   KQ
331000970805     CANN01V2CIDD        IFEQ      ' '
331100970805     C                   SETON                                          88
331200970805     C                   MOVEL     ERR(68)       WINMSG
331300970805     C                   END
331400941117     C* Se ci sono errori vado a fine pgm
331500941117     C   88              GOTO      FINVD6
331600941117     C*
331700941128     C     *INKA         IFEQ      '1'
331800941117     C     *INKQ         OREQ      '1'
331900941117     C     *INKG         OREQ      '1'
332000941118?    C                   EXSR      AGGS07
332100941117     C                   END
332200941117     C*
332300941118     C     FINVD6        ENDSR
332400941118      *---------------------------------------------------------*
332500941118      *  Controlli sulla terza videata                          *
332600941118      *---------------------------------------------------------*
332700941118     C     CTR03         BEGSR
332800941118      *  Se ho chiesto la conferma leggo tutti i record e calcolo
332900941118      *  i totali
333000941207     C                   Z-ADD     0             WSVAAS
333100941207     C                   Z-ADD     0             WSVLNP
333200941207     C                   Z-ADD     0             WSVNRS
333300941207     C                   Z-ADD     0             WSVNSP
333400991007     C                   Z-ADD     0             WTOT1
333500941212     C                   MOVEL     *BLANKS       WINTOT
333600941118     C     *INKF         IFEQ      '1'
333700941118     C                   EXSR      AZZTOT
333800941118     C     1             CHAIN     LR53S03                            96
333900941118     C                   ELSE
334000941118     C                   READC     LR53S03                                96
334100941118     C                   END
334200941118     C*  Loop letture
334300941118     C                   Z-ADD     1             NR3KEY
334400991008     C     *IN96         DOWEQ     '0'                                          -- 01 --
334500941118     C     *IN28         ANDEQ     '0'
334600941118     C                   SETOFF                                       404142
334700941118     C                   SETOFF                                       434445
334800941207     C                   SETOFF                                       820503
334900120308     C                   SETOFF                                       4951
335000941121     C                   MOVEL     *BLANKS       AGGI              1
335100120302     c************ NO CONSEGNA ****************
335200991008     C     V3CNCO        IFEQ      'SI'                                         -- 02 --
335300941129     C                   SETON                                        82
335400991007     C     V3DTPI        IFNE      'C/A'
335500991007     C     V3DTPI        ANDNE     'Ass'
335600991007     C                   SETON                                        05
335700991007     C                   ELSE
335800991007     C                   SETOFF                                       05
335900991007     C                   END
336000991130     C* Azzero dati spedizione se modificata
336100991130     C     V3CAAS        IFNE      WSVAAS
336200991130     C     V3CLNP        ORNE      WSVLNP
336300991130     C     V3CNRS        ORNE      WSVNRS
336400991130     C     V3CNSP        ORNE      WSVNSP
336500991130     C                   EXSR      AZZSPE
336600991130     C                   END
336700120302     c************ CONSEGNA ****************
336800991008     C                   ELSE                                                   -- 02 --
336900990924     C                   MOVEL     'S'           WFLV              1
337000941128     C                   Z-ADD     1             X
337100941128     C     VNMDOC        LOOKUP    NDC(X)                                 95
337200950102     C                   MOVE      DTD(X)        ANNO1             4 0
337300941222     C                   MOVEL     DTD(X)        MMGG
337400941222     C                   MOVE      MMGG          MM1
337500120302     c************ INCASSO TOTALE **********
337600941118     C*  Controllo se devo gestire incassi sulla riga di totale
337700991008     C     V3DTPI        IFNE      'C/A'                                        -- 03 --
337800991007     C     V3DTPI        ANDNE     'Ass'
337900990928     C                   SETON                                        05
338000991007     C                   Z-ADD     WTOT1         V3CIMP
338100941212     C                   SUB       RESTO         V3CIMP
338200991008     C     WINTOT        IFEQ      'NO'                                         -- 04 --
338300000421     C* Se devo incassare sia il c/assegno che l'assegnato ed ho
338400000421     C* scritto la riga per l'incasso totale,nell'ipotesi in cui
338500000421     C* abbia proposto e non modificato la prima riga di incasso
338600000421     C* C/Assegno con un tipo incasso non congruente con la div.
338700000421     C* (es: C/Assegno in Euro in contanti) prima di dare l'errore
338800000421     C* sulla riga di incasso C/Assegno debbo verificare di non
338900000421     C* voler gestire l'incasso sulla riga di totale. Se mi accorgo
339000000421     C* che non posso gestire l'incasso sulla riga di totale xchè
339100000421     C* ho dato per non incassata la seconda riga di incasso
339200000421     C* quando leggo la terza riga emetto l'errore: nelle righe
339300000421     C* precedenti presente incasso non congruente con divisa.
339400000421     C* Per sistemare il tutto l'utente deve o aggiornare la
339500000421     C* prima riga modificando il tipo incasso oppure deve abblencare
339600000421     C* il flag di non incasso sulla seconda riga e gestire l'incasso
339700000421     C* sulla terza
339800991008     C     WRIGTO        IFEQ      'S'
339900991008     C                   SETON                                        4028
340000991008     C                   MOVEL     ERR(8)        $MSG
340100991008     C                   GOTO      UPDRE1
340200991008     C                   END
340300941123     C                   MOVEL     'N'           V02SN1
340400941207     C                   MOVEL     *BLANKS       WINTOT            2
340500000421     C*  Se non posso gestire l'incasso sulla terza riga in
340600000421     C*  quanto ho modificato una delle precedenti dandole x
340700000421     C*  non incassate o specificando dei dati di incasso a
340800000421     C*  livello di dettaglio su almeno una di esse controllo
340900000421     C*  che non siano stati impostati dati di incasso sulla
341000000421     C*  stessa
341100941125     C     V3CTIC        IFNE      *BLANKS
341200941227     C     V02AS1        ORNE      *BLANKS
341300941125     C     V02AB1        ORNE      *ZEROS
341400941125     C     V02CA1        ORNE      *ZEROS
341500941125     C     V02DT1        ORNE      *ZEROS
341600990928     C     V02ASP        ORNE      *ZEROS
341700990928     C     V3CVCA        ORNE      VH3DIV
341800941125     C                   SETON                                        404142
341900941125     C                   SETON                                        4328
342000941125     C                   MOVEL     ERR(34)       $MSG
342100941125     C                   GOTO      UPDRE1
342200941125     C                   END
342300991008     C                   ELSE                                                   --  04 --
342400941207     C                   MOVEL     ' '           V02SN1
342500991008     C                   END                                                    -- 04 --
342600991008     C                   ELSE                                                   -- 03 --
342700991008     C     V3CTOT        IFEQ      'SI'                                         -- 04 --
342800941118     C*  Se flag di effettuato incasso = 'N' non devo incassare
342900941123     C     V02SN1        IFEQ      'N'
343000941118     C     V3CTIC        ORNE      *BLANKS
343100990928     C     V3CVCA        ORNE      VH3DIV
343200941125     C                   MOVEL     'NO'          WINTOT            2
343300941125     C                   END
343400991008     C                   END                                                    -- 04 --
343500991008     C                   END                                                    -- 03 --
343600991007     C* Azzero dati spedizione se modificata
343700991007     C     V3CAAS        IFNE      WSVAAS
343800991007     C     V3CLNP        ORNE      WSVLNP
343900991007     C     V3CNRS        ORNE      WSVNRS
344000991007     C     V3CNSP        ORNE      WSVNSP
344100991007     C                   EXSR      AZZSPE
344200991007     C                   END
344300991007     C* Azzero sempre resto
344400991007     C                   Z-ADD     0             RESTO
344500120302     C*
344600120302     C*se opzione in riga incasso non effettuato impossibile inserire opzione
344700120302     C     V3copz        IFNE      *blank
344800120302     C     v02sn1        ANDNE     *blank
344900120302     C                   SETON                                        40  28
345000120302     C                   MOVEL     ERR(76)       $MSG
345100120302     C                   GOTO      UPDRE1
345200120302     C                   END
345300120302     C************* INCASSATO ****************
345400941118     C*  Se flag incasso impostato a ' ' controllo attendbilità dati
345500991008     C     V02SN1        IFEQ      ' '                                          --- 03 ---
345600950314     C*  Controllo se richiesta ricerca tipo incasso
345700950314     C     '?'           SCAN      V3CTIC                                 15
345800950314     C     *IN15         IFEQ      '1'
345900950314     C                   MOVEL     KKUT          §KUT
346000950314     C                   MOVEL     '1A'          §COD
346100950314     C                   MOVE      *BLANKS       V3CTIC
346200950314     C                   CALL      'X§TABER'
346300950314     C                   PARM                    §KUT
346400950314     C                   PARM                    §COD
346500950314     C                   PARM                    §KEY
346600950314     C                   PARM                    §DES             30
346700950314     C                   MOVEL     §KEY          V3CTIC
346800950314     C                   END
346900060320     c* per il momento il tipo incasso POS è ammesso solo per aut. fittizio
347000060320     c* consegne a magazzino
347100060403     c                   if        v3ctic = 'P'
347200060403     c                   if        apdpdd = ' '
347300060320     C                   SETON                                        4028
347400060320     C                   MOVEL     ERR(94)       $MSG
347500060320     C                   GOTO      UPDRE1
347600060320     c                   end
347700080709     c* Verifica se possibile incasso POS tramite tabella Y4O
347800080709     c                   clear                   Dy4o
347900080709     c                   eval      KCOD1 =  'Y4O'
348000080709     c                   eval      KKE1  =  '201'
348100080714     c                   movel(p)  d53fgs        KKE2
348200080709     c     ktbe2         chain     tntbe01l
348300080709     c                   if        %found(tntbe01l)
348400080709     c                   movel     tbeuni        dy4o
348500080709     c                   if        §4OCC7 = *blank or §4OsC7 = *blank
348600080709     C                   SETON                                        4028
348700080709     C                   MOVEL     ERR(102)      $MSG
348800080709     C                   GOTO      UPDRE1
348900080709     c                   end
349000080709     c                   end
349100060419     c* non ammesso incasso parziale
349200060403     C     V02TPA        IFNE      ' '
349300060403     C                   SETON                                        4028
349400060403     C                   MOVEL     ERR(50)       $MSG
349500060403     C                   GOTO      UPDRE1
349600060403     c                   end
349700060403     c                   end
349800941118     C*  Controllo se corretto tipo incasso
349900941118     C                   Z-ADD     1             X
350000941118     C     V3CTIC        LOOKUP    C1A(X)                                 95
350100941118     C     *IN95         IFEQ      '0'
350200941118     C                   SETON                                        4028
350300941118     C                   MOVEL     ERR(38)       $MSG
350400941118     C                   GOTO      UPDRE1
350500941118     C                   ELSE
350600941118     C                   MOVEL     D1A(X)        DS1A
350700080717      *pda incasso dichiarato in contante e diverso da originale
350800080717     c                   if        wpda = 'S' and
350900080717     c                             vh3ctic = 'C' and §1AFCA <> 'S'
351000080717     C                   SETON                                        4028
351100090122     C                   MOVE      'E'           vh3ctic
351200080717     C                   MOVEL     ERR(103)      $MSG
351300080717     C                   GOTO      UPDRE1
351400080717     c                   endif
351500080717      *pda incasso dichiarato con assegno e diverso da originale
351600080717     c                   if        wpda = 'S' and
351700080717     c                             vh3ctic = 'A' and
351800080717     c                             (§1AFCA <> 'B' and §1AFCA <> 'C')
351900080717     C                   SETON                                        4028
352000090122     C                   MOVE      'E'           vh3ctic
352100080717     C                   MOVEL     ERR(104)      $MSG
352200080717     C                   GOTO      UPDRE1
352300080717     c                   endif
352400941118     C                   END
352500991115     C     §1AFMB        COMP      'M'                                    06
352600991116     C   06              MOVE      VH3DIV        V3CVCA
352700100218     c                   if        v3cvca <> ' '
352800991116     C                   Z-ADD     1             X
352900991116     C     V3CVCA        LOOKUP    CCV(X)                                 32
353000991116     C  N06V3CVCA        IFNE      §GEDCR
353100991116     C     FLV(X)        ANDNE     'S'
353200991116     C                   SETON                                        06
353300991116     C                   END
353400100218     C                   END
353500120229     C* opzione ammessa solo x incasso MITTENTE
353600120229     C     V3copz        IFNE      *blank
353700120229     C     §1afmb        ANDNE     'M'
353800120229     C                   SETON                                        40  28
353900120229     C                   MOVEL     ERR(74)       $MSG
354000120229     C                   GOTO      UPDRE1
354100120229     C                   END
354200120315     C* opzione ammessa solo x incasso in 'EUR'
354300120315     C     V3copz        IFNE      *blank
354400130408     C     V3CVCA        andne     'EUR'
354500130408     C     Vh3div        andne     'EUR'
354600120315     C                   SETON                                        40  28
354700120315     C                   MOVEL     ERR(74)       $MSG
354800120315     C                   GOTO      UPDRE1
354900120315     C                   END
355000120229     C*se la riga è già stata trattata è possibile solo opz M               ritrattata
355100120302     c                   if        v3hkey <> ' '
355200120302     C                   if        v3copz <> ' ' and v3copz <> 'M'
355300120229     C                   SETON                                        40  28
355400120229     C                   MOVEL     ERR(75)       $MSG
355500120229     C                   GOTO      UPDRE1
355600120229     C                   END
355700120302     c                   else
355800120302     C                   if        v3copz = 'M'
355900120302     C                   SETON                                        40  28
356000120302     C                   MOVEL     ERR(75)       $MSG
356100120302     C                   GOTO      UPDRE1
356200120302     c                   end
356300120302     c                   end
356400120302     c************ CONTANTE / POS ******************
356500060419     C*  Tipo incasso in contante o POS
356600991008     C     §1AFCA        IFEQ      'S'                                          --- 04 ---
356700060419     C     §1AFCA        orEQ      'P'                                          --- 04 ---
356800011010     C* CONTROLLO VALIDITA DIVISA CON TABELLA CW CONTENENTE DATA SCADENZA
356900011017MM+  C* Do errore solo se non ho la riga di incasso totale, ...
357000011017MM+  C     V3CTOT        IFNE      'SI'                                         -- 04---
357100011017MM+  C     V3DTPI        ANDEQ     'C/A'
357200011017MM+  C     WINTOT        OREQ      'NO'
357300011017MM+  C     V3DTPI        ANDEQ     'C/A'
357400011017MM+  C     V3CTOT        ORNE      'SI'
357500011017MM+  C     V3DTPI        ANDEQ     'Ass'
357600011017MM+  C     WINTOT        OREQ      'NO'
357700011017MM+  C     V3DTPI        ANDEQ     'Ass'
357800011017MM+  C     V3CTOT        OREQ      'SI'
357900011017MM+  C     V3DTPI        ANDNE     'C/A'
358000011017MM+  C     V3DTPI        ANDNE     'Ass'
358100011010     C     §1ACDL        IFEQ      'S'                                          --- 05 ---
358200011010     C                   MOVEL     'CW'          KCOD
358300011010     C                   MOVE      *BLANK        KKEY
358400011010     C                   MOVEL     V3CVCA        KKEY
358500011010     C     KTAB2         CHAIN     TABEL00F                           96
358600011010     C     *IN96         IFEQ      '0'                                          --- 06 ---
358700011010     C                   MOVEL     TBLUNI        §CWDLC
358800011010     C     OGGI          IFGT      §CWDLC                                       --- 07 ---
358900011010     C                   SETON                                        4928
359000011010     C                   MOVEL     ERR(85)       $MSG
359100011010     C                   GOTO      UPDRE1
359200011010     C                   END                                                    --- 07 ---
359300011010     C                   END                                                    --- 06 ---
359400011010     C                   END                                                    --- 05 ---
359500011017MM+  C                   END                                                    --- 04 ---
359600941118     C     V02AS1        IFNE      *ZEROS
359700941129     C     V02AS1        ANDNE     *BLANKS
359800941118     C                   SETON                                        404128
359900941118     C                   MOVEL     ERR(36)       $MSG
360000941118     C                   GOTO      UPDRE1
360100941118     C                   END
360200941118     C     V02AB1        IFNE      *ZEROS
360300941118     C     V02CA1        ORNE      *ZEROS
360400970117     C                   SETON                                        404228
360500941118     C                   MOVEL     ERR(35)       $MSG
360600941118     C                   GOTO      UPDRE1
360700941118     C                   END
360800941118     C     V02DT1        IFNE      *ZEROS
360900941118     C                   SETON                                        404328
361000941118     C                   MOVEL     ERR(37)       $MSG
361100941118     C                   GOTO      UPDRE1
361200941118     C                   END
361300120302     c************** ASSEGNO *****************
361400941118     C*  Incasso con Assegno
361500991008     C                   ELSE                                                   --- 04 ---
361600120228      * verifico prima dei controlli su assegno se scelta opzione per
361700120228      * gestione multiassegno o multispedizione se scelta opzione non esegue    --- 04b---
361800941118     C*  Per riga totale o assegnato non ammesso tipo incasso 'M'
361900991008     C     V3DTPI        IFNE      'C/A'                                        --- 05 ---
362000941118     C*  Controllo tipo incasso
362100941118     C     §1AFMB        IFEQ      'M'
362200941118     C     V3CTIC        OREQ      'BB'
362300010830     C     V3CTIC        OREQ      'BC'
362400970627     C     V3CTIC        OREQ      'CB'
362500150526     C**** V3CTIC        OREQ      'BP'
362600941118     C                   SETON                                        4028
362700941118     C                   MOVEL     ERR(39)       $MSG
362800941118     C                   GOTO      UPDRE1
362900941118     C                   END
363000941118     C                   ELSE
363100120229     C*  Se C/Assegno
363200941118     C*  Controllo se cliente abilitato al "BB"
363300991008     C     V3CTIC        IFEQ      'TM'                                         -- 06 --
363400060626     C     V3CTIC        OREQ      'TO'
363500060626     C     V3CTIC        OREQ      'BB'
363600010830     C     V3CTIC        OREQ      'BC'
363700970627     C     V3CTIC        OREQ      'CB'
363800951116     C     V3CTIC        OREQ      'BA'
363900960725     C     V3CTIC        OREQ      'CA'
364000150526     C**** V3CTIC        OREQ      'BP'
364100150526     C     v3ctic        oreq      'TP'
364200941123     C                   CLEAR                   DSTM
364300130328     C                   MOVEL     'TM'          KCOD
364400130328     C                   MOVE      *BLANK        KKEY
364500130328     C                   MOVEL     §clitm        KKEY
364600130328     C     KTAB2         CHAIN     TABEL00F                           96
364700130328     C     *IN96         IFEQ      '0'                                          --- 06 ---
364800130328     C                   movel     tbluni        dstm
364900130328     c                   endif
365000941118     C*
365100941121     C     V3CTIC        IFEQ      'TM'
365200941118     C     §TMFTM        ANDNE     'S'
365300951108     C     *IN95         ANDEQ     '1'
365400941121     C     V3CTIC        OREQ      'BB'
365500941118     C     §TMFBB        ANDNE     'S'
365600951108     C     *IN95         ANDEQ     '1'
365700060626     C     V3CTIC        OREQ      'TO'
365800060626     C     §TMFTO        ANDNE     'S'
365900060626     C     *IN95         ANDEQ     '1'
366000010831     C     V3CTIC        OREQ      'BB'
366100010831     C     §TMFBC        ANDEQ     'S'
366200010831     C     *IN95         ANDEQ     '1'
366300010830     C     V3CTIC        OREQ      'BC'
366400010830     C     §TMFBC        ANDNE     'S'
366500010830     C     *IN95         ANDEQ     '1'
366600970627     C     V3CTIC        OREQ      'CB'
366700970627     C     §TMFBB        ANDNE     'S'
366800970627     C     *IN95         ANDEQ     '1'
366900010831     C     V3CTIC        OREQ      'CB'
367000010831     C     §TMFBC        ANDEQ     'S'
367100010831     C     *IN95         ANDEQ     '1'
367200951116     C     V3CTIC        OREQ      'BA'
367300951116     C     §TMFBA        ANDNE     'S'
367400951116     C     *IN95         ANDEQ     '1'
367500960725     C     V3CTIC        OREQ      'CA'
367600960725     C     §TMFCA        ANDNE     'S'
367700960725     C     *IN95         ANDEQ     '1'
367800150526     C**** V3CTIC        OREQ      'BP'
367900150526     C**** §TMFbp        ANDNE     'S'
368000150526     C**** *IN95         ANDEQ     '1'
368100150526     C     v3ctic        oreq      'TP'
368200150526     C     §TMFtp        andne     'S'
368300150526     C     *in95         andeq     '1'
368400951108     C     *IN95         OREQ      '0'
368500941118     C                   SETON                                        4028
368600941118     C                   MOVEL     ERR(41)       $MSG
368700941118     C                   GOTO      UPDRE1
368800941121     C                   END
368900991008     C                   END                                                    -- 06 --
369000941123     C*
369100991008     C                   END                                                    -- 05 --
369200130416     c* se tipo incasso non utilizzabile in chiusura distinta errore
369300130416     c     §1achid       ifeq      'N'
369400130416     C                   SETON                                        4028
369500130416     C                   MOVEL     ERR(109)      $MSG
369600130416     C                   GOTO      UPDRE1
369700130416     c                   endif
369800120302     c************* INCASSO DI UN ASSEGNO ****************
369900120302     c                   if        v3copz = ' ' and v3hkey = *blanks
370000000421     C*  Do errore se il tipo incasso prevede l'assegno e non
370100000421     C*  ho specificato il numero
370200960725     C     §1ANAS        IFEQ      *BLANKS
370300941118     C     V02AS1        IFEQ      *BLANKS
370400941118     C     V02AS1        OREQ      *ZEROS
370500941118     C                   SETON                                        4128
370600941118     C                   MOVEL     ERR(42)       $MSG
370700941118     C                   GOTO      UPDRE1
370800120418     c                   else
370900120418     c                   monitor
371000120418     c                   eval      com04 = %int(v02as1)
371100120418     c                   movel     com04         v02as1
371200120418     c                   on-error
371300120418     C                   SETON                                        4128
371400120418     C                   MOVEL     ERR(42)       $MSG
371500120418     C                   GOTO      UPDRE1
371600120418     c                   endmon
371700941118     C                   END
371800960725     C                   END
371900090707     c* se tipo incasso non ammette assegni postali controllo abi  07601
372000090707     c     §1anap        ifeq      'S'
372100090622     c     v02ab1        andeq     07601
372200090622     C                   SETON                                        4228
372300130416     C                   MOVEL     ERR(106)      $MSG
372400090622     C                   GOTO      UPDRE1
372500090622     c                   endif
372600000421     C*  Do errore se il tipo incasso prevede l'ABI e non l'ho
372700000421     C*  indicato
372800120229     C     §1AABI        IFEQ      *BLANKS                                      -- 06 --
372900100825     C     V02AB1        IFlt      *ZEROS
373000120229     C     V02CA1        ORlt      *ZEROS                                       -- 07 --
373100100825     C                   SETON                                        4228
373200100825     C                   MOVEL     ERR(45)       $MSG
373300100825     C                   GOTO      UPDRE1
373400100825     C                   End
373500941118     C     V02AB1        IFEQ      *ZEROS
373600120229     C     V02CA1        OREQ      *ZEROS                                       -- 07 --
373700941118     C                   SETON                                        4228
373800941118     C                   MOVEL     ERR(43)       $MSG
373900941118     C                   GOTO      UPDRE1
374000941118     C                   ELSE
374100000421     C*  Se immesso Abi-CAb verifico congruenza
374200120229     C     SAVABI        IFNE      V02AB1                                       -- 08 --
374300941118     C     SAVCAB        ANDNE     V02CA1
374400941118     C                   Z-ADD     V02AB1        KABI
374500941118     C                   Z-ADD     V02CA1        KCAB
374600941123     C     KABI1         CHAIN     CNABI01L                           95
374700120229     C     *IN95         IFEQ      '1'                                          -- 09 --
374800120229     C     *INKB         IFEQ      '0'                                          -- 10 --
374900941121     C     SAVABI        IFEQ      *ZEROS
375000941121     C     SAVCAB        OREQ      *ZEROS
375100941121     C                   SETON                                        4228
375200941121     C                   MOVEL     ERR(45)       $MSG
375300941121     C                   END
375400120229     C                   ELSE                                                   -- 10 --
375500941121     C     AGGI          IFEQ      ' '
375600941121     C                   Z-ADD     V02AB1        SAVABI
375700941123     C                   Z-ADD     V02CA1        SAVCAB
375800941121     C                   MOVEL     'S'           AGGI
375900941121     C                   END
376000120229     C                   END                                                    -- 10 --
376100120229     C                   END                                                    -- 09 --
376200941123     C*
376300120229     C                   END                                                    -- 08 --
376400120229     C                   END                                                    -- 07 --
376500120229     C                   END                                                    -- 06 --
376600941118     C*
376700000421     C* Se incasso con assegno e non ho indicato la data
376800000421     C* imposto in automatico quella del giorno
376900991008     C     §1ANAS        IFEQ      *BLANKS                                      -- 05 --
377000991008     C     V02DT1        IFEQ      *ZEROS                                       -- 06 --
377100941128     C                   MOVEL     WOGGI         V02DT1
377200941121     C                   Z-ADD     OGGI          WDTASS
377300941121     C                   ELSE
377400941121     C*  Giro la data assegno
377500941222     C                   MOVEL     V02DT1        G02DAT
377600941222     C                   MOVE      V02DT1        MM2
377700000421     C* Postdatati: se sono a fine anno (da ottobre in poi)
377800000421     C* ed indico un mese di incasso inferiore a quelli del
377900000421     C* giorno presuppongo di voler specificare come data
378000000421     C* di incasso quella dell'anno successivo. Lo slittamentp
378100000421     C* deve essere non superiore a 3 mesi quindi ....
378200101020     C* variato il periodo da 3 a 6 mesi
378300991008     C     V3CTIC        IFEQ      'TM'                                         -- 07 --
378400060626     C     V3CTIC        orEQ      'TO'                                         -- 07 --
378500150526     C     v3ctic        orEQ      'TP'                                         -- 07 --
378600000421     C* ... se sono in ottobre al massimo posso accettare
378700000421     C* assegni postdatati a gennaio (ott. + 3 mesi)
378800131010     C     MM2           IFle      3
378900951116     C     MM1           ANDEQ     10
379000000421     C* ... se sono in novembre posso accettare assegni
379100000421     C* postdatati a gennaio o febbraio (nov. + 3 mesi)
379200101020     C     MM2           ORLE      5
379300951116     C     MM1           ANDEQ     11
379400000421     C* ... se sono in dicembre posso accettare assegni
379500000421     C* postdatati a gennaio, febbraio o marzo (dic. + 3 mesi)
379600101020     C     MM2           ORLE      6
379700951116     C     MM1           ANDEQ     12
379800951116     C                   ADD       1             ANNO1
379900991008     C                   END
380000151005      *
380100991008     C                   ELSE                                                   -- 07 --
380200000421     C* ... se il tipo incasso non indica un assegno postdatato
380300000421     C* posso slittare il pagamento solo da dicembre a gennaio
380400000421     C* e viceversa
380500951205     C     MM1           IFEQ      12
380600951205     C     MM2           ANDEQ     1
380700951205     C                   ADD       1             ANNO1
380800951205     C                   END
380900941222     C     MM1           IFEQ      1
381000951116     C     MM2           ANDEQ     12
381100951205     C                   SUB       1             ANNO1
381200941222     C                   END
381300991008     C                   END                                                    -- 07 --
381400151005      **
381500950102     C                   MOVE      ANNO1         G02DAT
381600941121     C                   Z-ADD     0             G02INV
381700941121     C                   MOVEL     *BLANKS       G02ERR
381800941121     C                   CALL      'XSRDA8'
381900941121     C                   PARM                    WLBDA8
382000941121     C     G02ERR        IFEQ      '1'
382100941118     C                   SETON                                        4328
382200941118     C                   MOVEL     ERR(44)       $MSG
382300941118     C                   GOTO      UPDRE1
382400941121     C                   ELSE
382500941121     C                   Z-ADD     G02INV        WDTASS
382600941129     C                   MOVEL     G02DAT        V02DT1
382700991008     C                   END
382800991008     C                   END                                                    -- 06 --
382900941121     C* Controllo data assegno > OGGI se TM
383000941121     C     V3CTIC        IFEQ      'TM'
383100941121     C     WDTASS        ANDLE     OGGI
383200060626     C     V3CTIC        orEQ      'TO'
383300060626     C     WDTASS        ANDLE     OGGI
383400150526     C     v3ctic        oreq      'TP'
383500150526     C     wdtass        andle     oggi
383600941121     C                   SETON                                        404328
383700941121     C                   MOVEL     ERR(52)       $MSG
383800941121     C                   GOTO      UPDRE1
383900941121     C                   END
384000941129     C* Se data assegno > OGGI
384100991008     C     WDTASS        IFGT      OGGI                                         -- 06 --
384200941129     C* X Mittente obbligatorio tipo incasso TM
384300991008     C     §1AFMB        IFEQ      'M'                                          -- 07 --
384400941129     C     V3CTIC        IFNE      'TM'
384500060626     C     V3CTIC        andne     'TO'
384600150526     C     v3ctic        andne     'TP'
384700941129     C                   SETON                                        404328
384800941129     C                   MOVEL     ERR(59)       $MSG
384900941129     C                   GOTO      UPDRE1
385000941129     C                   END
385100941129     C* X Bartolini non si possono inserire postdatati
385200991008     C                   ELSE                                                   -- 07 --
385300941129     C                   SETON                                        404328
385400941129     C                   MOVEL     ERR(60)       $MSG
385500941129     C                   GOTO      UPDRE1
385600991008     C                   END                                                    -- 07 --
385700991008     C                   END                                                    -- 06 --
385800941129     C*
385900991008     C                   END                                                    -- 05 --
386000011012     C* CONTROLLO VALIDITA DIVISA CON TABELLA CW CONTENENTE DATA SCADENZA
386100011017MM+  C* Do errore solo se non ho la riga di incasso totale, ...
386200011017MM+  C     V3CTOT        IFNE      'SI'
386300011017MM+  C     V3DTPI        ANDEQ     'C/A'
386400011017MM+  C     WINTOT        OREQ      'NO'
386500011017MM+  C     V3DTPI        ANDEQ     'C/A'
386600011017MM+  C     V3CTOT        ORNE      'SI'
386700011017MM+  C     V3DTPI        ANDEQ     'Ass'
386800011017MM+  C     WINTOT        OREQ      'NO'
386900011017MM+  C     V3DTPI        ANDEQ     'Ass'
387000011017MM+  C     V3CTOT        OREQ      'SI'
387100011017MM+  C     V3DTPI        ANDNE     'C/A'
387200011017MM+  C     V3DTPI        ANDNE     'Ass'
387300011012     C     §1ACDL        IFEQ      'S'                                          -- 05 --
387400011012     C                   MOVEL     'CW'          KCOD
387500011012     C                   MOVE      *BLANK        KKEY
387600011012     C                   MOVEL     V3CVCA        KKEY
387700011012     C     KTAB2         CHAIN     TABEL00F                           96
387800011012     C     *IN96         IFEQ      '0'                                          -- 06 --
387900011012     C                   MOVEL     TBLUNI        §CWDLC
388000011012     C     WDTASS        IFGT      §CWDLC                                       -- 07 --
388100011012     C                   SETON                                        4928
388200011012     C                   MOVEL     ERR(85)       $MSG
388300011012     C                   GOTO      UPDRE1
388400011012     C                   END                                                    --- 07 ---
388500011012     C                   END                                                    --- 06 ---
388600011012     C                   END                                                    --- 05 ---
388700011017MM+  C                   END                                                    --- 04 ---
388800120228     c* fine if salto controlli se inserita opzione assegno multiplo o
388900120228     c* spedizione multipla
389000120228     c                   endif                                                  -- 04b--
389100120228
389200991008     C                   END                                                    -- 04 --
389300990923     C*  Controllo se divisa valida
389400991201     C     VH3DIV        IFNE      *BLANKS                                      -- 04 --
389500941129     C                   Z-ADD     1             X
389600941222     C     V3CVCA        LOOKUP    CCV(X)                                 32
389700990928     C* Se divisa inesistente do errore
389800990928     C     *IN32         IFEQ      '0'
389900991201     C     V3CVCA        OREQ      *BLANKS
390000990928     C                   SETON                                        4928
390100990928     C                   MOVEL     ERR(27)       $MSG
390200990928     C                   GOTO      UPDRE1
390300990928     C                   END
390400941129     C* Somma nel totale corretto
390500990923     C                   MOVE      FLV(X)        WFLV
390600941129     C                   END
390700000421     C*
390800941129     C*  Controllo se tipo incasso ammesso prevede divisa
390900941129     C     §1AUDE        IFNE      'S'
391000990923     C     WFLV          ANDNE     'S'
391100991011     C     §1AUDE        OREQ      'S'
391200991011     C     WFLV          ANDNE     'S'
391300991011     C     V3CVCA        ANDNE     VH3DIV
391400990923     C*  tipo incasso non valido per Euro
391500990923     C     §1AEUR        ORNE      'S'
391600990923     C     WFLV          ANDEQ     'S'
391700000421     C* Do errore solo se non ho la riga di incasso totale, ...
391800991007     C     V3CTOT        IFNE      'SI'
391900991007     C     V3DTPI        ANDEQ     'C/A'
392000991008     C     WINTOT        OREQ      'NO'
392100991007     C     V3DTPI        ANDEQ     'C/A'
392200991007     C     V3CTOT        ORNE      'SI'
392300991008     C     V3DTPI        ANDEQ     'Ass'
392400991008     C     WINTOT        OREQ      'NO'
392500991008     C     V3DTPI        ANDEQ     'Ass'
392600991011     C     V3CTOT        OREQ      'SI'
392700991011     C     V3DTPI        ANDNE     'C/A'
392800991116     C     V3DTPI        ANDNE     'Ass'
392900941129     C                   SETON                                        4028
393000941129     C                   MOVEL     ERR(40)       $MSG
393100941129     C                   GOTO      UPDRE1
393200991007     C                   ELSE
393300000421     C* .. se ce l'ho, imposto flag per riconoscere che dovrò
393400000421     C* obbligarne l'immissione:
393500991007     C                   MOVEL     'S'           WRIGTO
393600941129     C                   END
393700991007     C                   END
393800991007     C* Flag incasso parziale non ammesso per C/Assegno/totale
393900990923     C     V02TPA        IFNE      ' '
394000990927     C     V3DTPI        ANDNE     'Ass'
394100990923     C                   SETON                                        444528
394200990923     C                   MOVEL     ERR(15)       $MSG
394300990923     C                   GOTO      UPDRE1
394400990923     C                   END
394500990923     C* L'importo incassato obbligatorio se modificata la divisa
394600990923     C     V02ASP        IFEQ      *ZERO
394700990924     C     V3CVCA        ANDNE     VH3DIV
394800990923     C                   SETON                                        444528
394900990923     C                   MOVEL     ERR(6)        $MSG
395000990923     C                   GOTO      UPDRE1
395100990923     C                   END
395200990923     C* Importo incassato valido solo per assegnato se non modifcata
395300990923     C* divisa
395400990923     C     V02ASP        IFNE      *ZERO
395500941121     C     V02TPA        ORNE      *BLANKS
395600941121     C     V3DTPI        IFNE      'Ass'
395700990923     C     V3CVCA        ANDEQ     VH3DIV
395800941121     C                   SETON                                        444528
395900941121     C                   MOVEL     ERR(46)       $MSG
396000941121     C                   GOTO      UPDRE1
396100941121     C                   ELSE
396200941121     C                   Z-ADD     NRR3          SAVNRR
396300990923     C*  Importo incassato superiore a quello da incassare
396400941121     C     V3CIMP        IFLE      V02ASP
396500990923     C     V3CVCA        ANDEQ     VH3DIV
396600941121     C                   SETON                                        4428
396700941121     C                   MOVEL     ERR(48)       $MSG
396800941121     C                   GOTO      UPDRE1
396900941121     C                   END
397000991007     C                   END
397100991007     C                   END
397200990923     C* Se l'importo incassato non è in moneta di conto
397300990923     C* lo converto per calcolare differenza
397400991007     C     V3CVCA        IFNE      §GEDCR
397500990923     C     V02ASP        ANDGT     0
397600990923     C                   CLEAR                   YEURDS
397700990923     C                   MOVEL     V3CVCA        YECDVI
397800990923     C                   Z-ADD     V02ASP        YECIMI
397900991007     C                   MOVEL     §GEDCR        YECDVO
398000990923     C                   MOVEL     'H'           YECTAR
398100990923     C                   CALL      'YEURCO'
398200990923     C                   PARM                    YEURDS
398300990923     C                   Z-ADD     YECIMO        WTOTMC           13 3
398400990923     C                   ELSE
398500990923     C                   Z-ADD     V02ASP        WTOTMC           13 3
398600990923     C                   END
398700990923     C* Converto importo da incassare se non in moneta di conto
398800991007     C     VH3DIV        IFNE      §GEDCR
398900990923     C                   CLEAR                   YEURDS
399000990923     C                   MOVEL     VH3DIV        YECDVI
399100990923     C                   Z-ADD     V3CIMP        YECIMI
399200991007     C                   MOVEL     §GEDCR        YECDVO
399300990923     C                   MOVEL     'H'           YECTAR
399400990923     C                   CALL      'YEURCO'
399500990923     C                   PARM                    YEURDS
399600990923     C                   Z-ADD     YECIMO        WTOTDI           13 3
399700991007     C                   ADD       YECIMO        WTOT1            13 3
399800990923     C                   ELSE
399900990923     C                   Z-ADD     V3CIMP        WTOTDI           13 3
400000991007     C                   ADD       V3CIMP        WTOT1            13 3
400100990923     C                   END
400200990923     C* ... se è stata modificata la divisa per il C/Assegno
400300990923     C* controllo se l'importo incassato è corretto
400400990923     C     V02ASP        IFNE      0
400500990928     C     V02TPA        ORNE      *BLANKS
400600990928     C     VH3DIV        IFNE      V3CVCA
400700990923     C* Se l'importo da incassare non è uguale a quello incassato
400800990923     C* calcolo la differenza
400900990923     C     WTOTDI        IFNE      WTOTMC
401000990923     C     WTOTDI        IFGT      WTOTMC
401100990924     C     WTOTDI        SUB       WTOTMC        WTODIF           13 3
401200990923     C* se differenza > limite monimo tab. GEI e non si tratta di incasso
401300990923     C* Assegnato do errore
401400990923     C     WTODIF        IFGT      §GEMIA
401500990927     C     V3DTPI        ANDNE     'Ass'
401600990927     C                   SETON                                        2844
401700990923     C                   MOVEL     ERR(25)       $MSG
401800990923     C                   GOTO      UPDRE1
401900990923     C                   END
402000990923     C* ... se totale incassato > da incassare lo accetto solo se entro
402100990923     C* i limiti di abbuono
402200990923     C                   ELSE
402300990923     C     WTOTMC        SUB       WTOTDI        WTODIF
402400990923     C* se differenza > limite monimo tab. GEI do erore
402500990923     C     WTODIF        IFGT      §GEMIA
402600990927     C                   SETON                                        2844
402700990923     C                   MOVEL     ERR(26)       $MSG
402800990923     C                   GOTO      UPDRE1
402900990923     C                   END
403000990923     C                   END
403100990923     C                   END
403200990923     C*
403300990928     C* ... se immesso incasso parziale senza modifica divisa eseguo
403400990928     C*     lo stesso il calcolo della differenza
403500990928     C                   ELSE
403600990928     C     WTOTDI        SUB       WTOTMC        WTODIF
403700990923     C                   END
403800941121     C*  Controllo tipo incasso parziale
403900941121     C     '?'           SCAN      V02TPA                                 95
404000941121     C     *IN95         IFEQ      '1'
404100941123     C                   MOVEL     KKUT          §KUT
404200941121     C                   MOVEL     '7M'          §COD
404300941121     C                   MOVE      *BLANKS       V02TPA
404400941121     C                   CALL      'X§TABER'
404500941121     C                   PARM                    §KUT
404600941121     C                   PARM                    §COD
404700941121     C                   PARM                    §KEY
404800941121     C                   PARM                    §DES             30
404900941121     C                   MOVEL     §KEY          V02TPA
405000941121     C                   END
405100950620     C*
405200950620     C     V3HTPA        IFNE      V02TPA
405300950620     C                   MOVEL     V02TPA        V3HTPA
405400950620     C     V02TPA        IFEQ      'C'
405500110613     c* se pda non importa se è già stata chiusa la data per contabilizzare
405600110613     c* gli assegnati, perchè si deve lasciare mettere la C di nota credito
405700110613     c* per evitare lo "storno" dell'assegnato quando l'autista è già
405800110613     c* fuori in consegna. facendo il cambio di porto si creerebbe la
405900110613     c* figlia mentre la mamma è in consegna e si scatena l'errore
406000110613     c* bloccante in pre-chiusura distinta
406100110613     c                   if        (wpda <> 'S' and
406200110613     c                             V3CDFT > §dtcon) or
406300110613     c                             V3CDFT = 0
406400110613     C*    V3CDFT        IFGT      §DTCON
406500110613     C*    V3CDFT        OREQ      0
406600950620     C                   SETON                                        4528
406700950620     C                   MOVEL     ERR(63)       $MSG
406800950620     C                   GOTO      UPDRE1
406900950620     C                   END
407000950620     C                   END
407100950620     C                   END
407200941121     C*
407300991007     C     WTOTDI        SUB       WTOTMC        RESTO            13 3
407400990923     C     WTODIF        IFLT      §GEMIA
407500000117     C     WTODIF        ANDGT     §GEDFC
407600990928     C     V3DTPI        ANDEQ     'Ass'
407700991115     C                   MOVE      'A'           V02TPA
407800000117     C                   ELSE
407900000117     C     WTODIF        IFLT      §GEMIA
408000000117     C     WTODIF        ANDLE     §GEDFC
408100000117     C     V3DTPI        ANDEQ     'Ass'
408200000117     C                   MOVE      ' '           V02TPA
408300941121     C                   END
408400000117     C                   END
408500000421     C* ...se non impostato flag di incasso parziale
408600941121     C     V02TPA        IFEQ      *BLANKS
408700000421     C* ...il tipo incasso è assegnato e
408800990928     C     V3DTPI        ANDEQ     'Ass'
408900000421     C* ...l'importo in divisa di incasso incasso differisce
409000000421     C* di oltre quanto previsto da tabella x arrotondamenti
409100000117     C     WTODIF        ANDGT     §GEDFC
409200000421     C* .. obbligo l'indicazione di un tipo incasso parziale
409300941121     C                   SETON                                        4528
409400941121     C                   MOVEL     ERR(49)       $MSG
409500941121     C                   GOTO      UPDRE1
409600941121     C                   END
409700000421     C* verifico esistenza flag incasso parziale
409800941121     C                   Z-ADD     1             X
409900941121     C     V02TPA        LOOKUP    C7M(X)                                 95
410000941121     C     *IN95         IFEQ      '0'
410100941121     C                   SETON                                        4528
410200941121     C                   MOVEL     ERR(51)       $MSG
410300941121     C                   GOTO      UPDRE1
410400941121     C                   END
410500000421     C* Se differenza bollettato e incassato entro i limiti dell'abbuono
410600990924     C     WTODIF        IFGE      §GEMIA
410700990924     C     WTODIF        ANDLE     §GEMXA
410800000421     C* ... obbligo l'inserimento del tipo incasso parziale abbuono
410900941121     C     V02TPA        ANDNE     'A'
411000941121     C                   SETON                                        4528
411100941121     C                   MOVEL     ERR(50)       $MSG
411200941121     C                   GOTO      UPDRE1
411300941121     C                   END
411400000421     C* Non posso chiudere con incasso parziale un assegnato non
411500000421     C* incasso completamente
411600941121     C     V02TPA        IFEQ      'I'
411700941123     C     RESTO         ANDEQ     V3CIMP
411800941121     C                   SETON                                        4528
411900941121     C                   MOVEL     ERR(50)       $MSG
412000941121     C                   GOTO      UPDRE1
412100941121     C                   END
412200000421     C* Abbuono fuori limiti
412300990924     C     WTODIF        IFGT      §GEMXA
412400941121     C     V02TPA        ANDEQ     'A'
412500941121     C                   SETON                                        4528
412600941214     C                   MOVEL     ERR(50)       $MSG
412700941121     C                   GOTO      UPDRE1
412800941121     C                   END
412900941121     C*
413000011108     C                   END
413100991007     C*
413200011108     C                   END
413300011108     C                   END
413400991007     C*
413500110419     C     V02SN1        IFNE      ' '
413600110419     C     V02sn1        andne     'N'
413700110419     C                   SETON                                        5128
413800110419     C                   MOVEL     ERR(61)       $MSG
413900110419     C                   GOTO      UPDRE1
414000110419     C                   END
414100011108     C* SE IMPOSTATO FLAG NON INCASSATO DEVO PULIRE I CAMPI RELATIVI ALL'INCASSO
414200011108     C     V02SN1        IFNE      ' '
414300011108     C     V02ASP        IFNE      0
414400011108     C     V02AS1        ORNE      *BLANK
414500011108     C     V02AB1        ORNE      0
414600011108     C     V02CA1        ORNE      0
414700011108     C                   SETON                                        444142
414800011108     C                   SETON                                        28
414900011108     C                   MOVEL     ERR(86)       $MSG
415000011108     C                   GOTO      UPDRE1
415100011108     C                   END
415200011108     C                   END
415300941121     C     UPDRE1        TAG
415400991007     C* Imposto dati C/Assegno o Assegnato a seconda del tipo importo
415500991007     C                   EXSR      AGGFLD
415600991007     C*
415700941207     C     V3CAAS        IFEQ      WSVAAS
415800941207     C     V3CLNP        ANDEQ     WSVLNP
415900941207     C     V3CNRS        ANDEQ     WSVNRS
416000941207     C     V3CNSP        ANDEQ     WSVNSP
416100941207     C                   SETON                                        03
416200941207     C                   END
416300061108     c* richiamo la funzione x richiedere il codice fiscale solo una volta
416400061108     c                   move      savksc        com4              4
416500061107     c                   if        v02sn1 = ' ' and v3dtpi = 'Ass'
416600061108     c                             and com4 = '9999' and savcf = ' '
416700061107     c                   exsr      srcdfis
416800061107     c                   if        dz1err <> ' '
416900061107     C                   SETON                                        28
417000061107     C                   MOVEL(p)  dz1msg        $MSG
417100061108     c                   clear                   savcf
417200061107     C                   GOTO      UPDRE1
417300061107     c                   end
417400061108     c                   eval      savcf = '1'
417500061107     c                   end
417600120302      *scrittura rekord nuovo file per incassi mittenti multiassegni
417700120308     C                   IF        not *in28 and *inkf and
417800120308     c                             v3copz <> *blank
417900120308     c                   eval      comnrr = nrr3
418000120308     c* in modifica
418100120308     c                   if        v3copz = 'M'
418200120308     c                   exsr      aggiosped
418300120308     c* in scrittura
418400120308     c                   else
418500120308     c                   exsr      scrivisped
418600120308     c                   end
418700120308     c     comnrr        chain     lr53s03
418800120305     c                   endif
418900991007     C*  Update record
419000120308     c     v3hkey        comp      ' '                                8787
419100941207     C                   SETON                                        22
419200941121     C                   UPDATE    LR53S03
419300941121     C                   SETOFF                                       22
419400061107     c*
419500941207     C                   Z-ADD     V3CAAS        WSVAAS
419600941207     C                   Z-ADD     V3CLNP        WSVLNP
419700941207     C                   Z-ADD     V3CNRS        WSVNRS
419800941207     C                   Z-ADD     V3CNSP        WSVNSP
419900941128     C     *IN28         IFEQ      '0'
420000941129     C     *INKF         IFEQ      '0'
420100941121     C                   READC     LR53S03                                96
420200941129     C                   ELSE
420300941129     C                   ADD       1             NRR3
420400941129     C     NRR3          CHAIN     LR53S03                            96
420500941129     C                   END
420600991007     C*  Eseguo aggiornamento totali a rottura spedizione
420700991007     C*  o fine file
420800991007     C     V3CAAS        IFNE      WSVAAS
420900991007     C     V3CLNP        ORNE      WSVLNP
421000991007     C     V3CNRS        ORNE      WSVNRS
421100991007     C     V3CNSP        ORNE      WSVNSP
421200991007     C     *IN96         OREQ      '1'
421300991007     C   KF              EXSR      AGGTO3
421400991007     C                   END
421500941227     C                   ELSE
421600941227     C                   Z-ADD     NRR3          NR3KEY
421700941121     C                   END
421800941121     C*
421900941121     C                   END
422000941121     C*
422100941121     C                   ENDSR
422200991007      *---------------------------------------------------------*
422300061107      *  richiamo funzione per richiedere codice fiscale        *
422400991007      *---------------------------------------------------------*
422500061107     C     srcdfis       BEGSR
422600991007     C*
422700061107     c                   clear                   fnlrz1ds
422800061107     c                   eval      dz1tipo = 'A'
422900061107     c                   eval      dz1aas = v3caas
423000061107     c                   eval      dz1lnp = v3clnp
423100061107     c                   eval      dz1nrs = v3cnrs
423200061107     c                   eval      dz1nsp = v3cnsp
423300061107     c                   eval      kpjbu = fnlrz1ds
423400061107     c                   call      'FNLRZ1R'
423500061107     c                   parm                    kpjba
423600061107     c                   eval      fnlrz1ds = kpjbu
423700061108     c                   write     FRCDTA
423800061107     C*
423900061107     C                   ENDSR
424000061107      *---------------------------------------------------------*
424100061107      *  Azzero dati della spedizione                           *
424200061107      *---------------------------------------------------------*
424300061107     C     AZZSPE        BEGSR
424400061107     C*
424500991007     C* Importi
424600991007     C                   Z-ADD     0             WTOT1
424700991007     C                   Z-ADD     0             WTOT
424800991007     C                   Z-ADD     0             WASS
424900991007     C                   Z-ADD     0             WCAS
425000991007     C* Divise
425100991007     C                   MOVEL     *BLANKS       WDIV
425200991007     C                   MOVEL     *BLANKS       WVCA
425300991007     C                   MOVEL     *BLANKS       WVTO
425400991007     C* Tipi incasso ...
425500991007     C                   MOVEL     *BLANKS       WTCA
425600991007     C                   MOVEL     *BLANKS       WTIC
425700991007     C                   MOVEL     *BLANKS       WTTO
425800991007     C* .... e flag relativi
425900991007     C                   MOVEL     *BLANKS       WFCA
426000991007     C                   MOVEL     *BLANKS       WFCC
426100991007     C                   MOVEL     *BLANKS       WFCT
426200991007     C                   MOVEL     *BLANKS       WFMA
426300991007     C                   MOVEL     *BLANKS       WFMC
426400991007     C                   MOVEL     *BLANKS       WFMT
426500991007     C* flag S/N incasso
426600991007     C                   MOVEL     *BLANKS       WSNA
426700991007     C                   MOVEL     *BLANKS       WSNC
426800991007     C                   MOVEL     *BLANKS       WSNT
426900991007     C* errore ctrl riga totale
427000991007     C                   MOVEL     *BLANKS       WRIGTO            1
427100991007     C*
427200991007     C                   ENDSR
427300991007      *---------------------------------------------------------*
427400991007      *  Imposto campi nel vari importi                         *
427500991007      *---------------------------------------------------------*
427600991007     C     AGGFLD        BEGSR
427700991007     C*
427800000421     C*  Imposto dati in campi di work per C/Ass.
427900991007     C     V3DTPI        IFEQ      'C/A'
428000000421     C* ... se specificato importo incassato in divisa alternativa
428100000421     C*     vale quello
428200991007     C     V02ASP        IFGT      0
428300991007     C                   Z-ADD     V02ASP        WCAS
428400991007     C                   ELSE
428500991007     C                   Z-ADD     V3CIMP        WCAS
428600991007     C                   END
428700991007     C                   MOVEL     V02SN1        WSNC
428800991007     C                   MOVEL     V3CTIC        WTCA
428900991007     C                   MOVEL     V3CVCA        WVCA
429000991007     C                   MOVEL     §1AFCA        WFCC
429100991007     C                   MOVEL     §1AFMB        WFMC
429200991007     C                   ELSE
429300000421     C*  Imposto dati in campi di work per Assegnato
429400991007     C     V3DTPI        IFEQ      'Ass'
429500000421     C* ... se specificato importo incassato in divisa alternativa
429600000421     C*     o di incasso parziale vale quello
429700991007     C     V02ASP        IFGT      0
429800991027     C     V02TPA        ORNE      ' '
429900991007     C                   Z-ADD     V02ASP        WASS
430000991007     C                   ELSE
430100991007     C                   Z-ADD     V3CIMP        WASS
430200991007     C                   END
430300991007     C                   MOVEL     V02SN1        WSNA
430400991007     C                   MOVEL     V3CTIC        WTIC
430500991007     C                   MOVEL     V3CVCA        WDIV
430600991007     C                   MOVEL     §1AFCA        WFCA
430700991007     C                   MOVEL     §1AFMB        WFMA
430800991007     C* .. totale
430900991007     C                   ELSE
431000991007     C     V02ASP        IFGT      0
431100991007     C                   Z-ADD     V02ASP        WTOT
431200991007     C                   ELSE
431300991007     C                   Z-ADD     V3CIMP        WTOT
431400991007     C                   END
431500991007     C                   MOVEL     V02SN1        WSNT
431600991007     C                   MOVEL     V3CTIC        WTTO
431700991007     C                   MOVEL     V3CVCA        WVTO
431800991007     C                   MOVEL     §1AFCA        WFCT
431900991007     C                   MOVEL     §1AFMB        WFMT
432000991007     C                   END
432100991007     C                   END
432200991007     C*
432300991007     C                   ENDSR
432400941121      *---------------------------------------------------------*
432500000421      *  Aggiorno totali x videata di riepilogo                 *
432600941121      *---------------------------------------------------------*
432700941123     C     AGGTO3        BEGSR
432800941121     C*
432900991129     C                   Z-ADD     0             WIMP
433000991007     C* Verifico se devo gestire somma x la sola riga di totale
433100991008     C     WINTOT        IFNE      'NO'
433200991214     C* DEVO FARLO SOLO SE C'È LA RIGA DI TOTALE !!!
433300991215     C     WTOT          ANDGT     0
433400991007     C     WASS          ANDGT     0
433500991007     C     WCAS          ANDGT     0
433600991007     C* Sommo il totale
433700991008     C     WSNT          ANDNE     'N'
433800991007     C* Imposto dati campi di work subroutine aggiornamento tot.
433900991007     C                   MOVEL     WFCT          WCON
434000991007     C                   MOVEL     WFMT          WMIT
434100991007     C                   MOVEL     WVTO          WVAL
434200991007     C                   Z-ADD     WTOT          WIMP
434300991007     C                   EXSR      CALTOT
434400991007     C* .. no totale
434500991007     C                   ELSE
434600991007     C* ... Assegnato ?
434700991007     C     WASS          IFGT      0
434800991007     C     WSNA          ANDNE     'N'
434900991007     C                   MOVEL     WFCA          WCON
435000991007     C                   MOVEL     WFMA          WMIT
435100991007     C                   MOVEL     WDIV          WVAL
435200991007     C                   Z-ADD     WASS          WIMP
435300991007     C                   EXSR      CALTOT
435400991007     C                   END
435500991007     C* ... C/Assegno ?
435600991007     C     WCAS          IFGT      0
435700991007     C     WSNC          ANDNE     'N'
435800110330     C     WSNC          ANDNE     'R'
435900991007     C                   MOVEL     WFCC          WCON
436000991007     C                   MOVEL     WFMC          WMIT
436100991007     C                   MOVEL     WVCA          WVAL
436200991007     C                   Z-ADD     WCAS          WIMP
436300991007     C                   EXSR      CALTOT
436400991007     C                   END
436500991007     C*
436600991007     C                   END
436700991007     C*
436800991007     C                   ENDSR
436900991007      *---------------------------------------------------------*
437000991007      *  Calcolo totali                                         *
437100991007      *---------------------------------------------------------*
437200991007     C     CALTOT        BEGSR
437300991115     C                   Z-ADD     1             O                 4 0
437400991115     C     'EUR'         LOOKUP    CCV(O)                                 11
437500991007     C     WVAL          IFEQ      'EUR'
437600991115     C     *IN11         ANDEQ     '1'
437700991115     C     FLV(O)        ANDEQ     'S'
437800991007     C* contante
437900991007     C     WCON          IFEQ      'S'                                          Contanti
438000060310     C                   z-add     0             V05TEC
438100991007     C                   ADD       WIMP          V05TCE
438200991007     C                   ELSE                                                   no contanti
438300060331     C* ... pos
438400060331     c     wcon          ifeq      'P'
438500060331     c                   add       wimp          v05pos
438600060331     c                   else
438700060331     C* ... ns/assegni
438800991007     C     WMIT          IFNE      'M'                                          N.Assegni
438900991007     C                   ADD       WIMP          V05TAE
439000991007     C* ... ns/assegni
439100991007     C                   ELSE
439200991007     C                   ADD       WIMP          V05TDE                         Mittenti
439300991007     C                   END
439400991007     C                   END                                                    Contanti
439500060331     C                   END
439600060303     C* ... se no Euro sarà mittenti altre divise
439700991007     C                   ELSE
439800991007     C                   ADD       WIMP          V05TAV
439900991007     C                   END                                                    Euro
440000060320     c* mi salvo i totali
440100060320     c                   z-add     v05tce        savtce
440200060320     c                   z-add     v05tae        savtae
440300060320     c                   z-add     v05pos        savpos
440400941121     C*
440500941121     C                   ENDSR
440600060303      *---------------------------------------------------------*
440700060303      *  aggiunge prepagati ai totali                           *
440800060303      *---------------------------------------------------------*
440900060303     C     CALTOTPRE     BEGSR
441000060303     c* contante
441100060320     C     savtce        ADD       impinccon     V05TCE
441200060303     C* assegni
441300060320     C     savtae        ADD       impincass     V05TAE
441400060303     C* pos
441500060320     C     savpos        ADD       impincpos     V05pos
441600060303     C*
441700060303     C                   ENDSR
441800941123      *---------------------------------------------------------*
441900941123      *  AZZERO  totali                                         *
442000941123      *---------------------------------------------------------*
442100941123     C     AZZTOT        BEGSR
442200941123     C*
442300941123     C*  Totali in valuta
442400060502     c                   z-add     0             savtce
442500060502     c                   z-add     0             savtae
442600060502     c                   z-add     0             savpos
442700990923     C                   Z-ADD     0             V05TCE
442800060303     C                   Z-ADD     0             V05pos
442900060303     C                   Z-ADD     0             V05resto
443000990923     C                   Z-ADD     0             V05TEC
443100990923     C                   Z-ADD     0             V05TAE
443200991007     C                   Z-ADD     0             V05TDE
443300990923     C                   Z-ADD     0             V05TAV
443400941123     C*
443500991007     C                   ENDSR
443600991007      *---------------------------------------------------------*
443700991007      *  Controlli sulla quinta videata                         *
443800991007      *---------------------------------------------------------*
443900991007     C     CTR05         BEGSR
444000060303     C* .. calcolo resto
444100060323     C     V05Tec        SUB       V05Tce        v05resto
444200991007     C* Controllo se differenza è negativa e se si la rendo
444300991007     C* positiva
444400060323     C     v05resto      IFLT      0
444500060323     C                   eval      comresto = v05resto * -1
444600060323     c                   else
444700060323     c                   eval      comresto = v05resto
444800060323     C                   END
444900991007     C* differenza nei limiti ?
445000060323     C     comresto      IFGT      §GEACH
445100060412     C                   SETON                                        41
445200060411     c                   if        v05tec < v05tce
445300060323     C                   SETON                                        28
445400060411     c                   end
445500060323     C                   MOVEL     ERR(47)       $MSG
445600060323     C                   END
445700030318     c* controllo che non ci siano incassi se è già stata fatta quadratura
445800030318     c* giornaliera
445900030318     C     V05TCE        IFNE      *ZEROS
446000060303     C     V05pos        ORNE      *ZEROS
446100060303     C     V05TAE        ORNE      *ZEROS
446200030318     C     V05TAV        ORNE      *ZEROS
446300030318     C     V05TDE        ORNE      *ZEROS
446400060412     c     kqig          chain     yfqig01l
446500060412     c                   if        %found(yfqig01l)
446600030318     C                   SETON                                        28
446700030318     C                   MOVEL     ERR(88)       $MSG
446800030318     c                   end
446900030318     c                   end
447000991007     C*
447100991007     C                   ENDSR
447200941121      *---------------------------------------------------------*
447300941121      *  Aggiorno archivi                                       *
447400941121      *---------------------------------------------------------*
447500941121     C     AGGIOR        BEGSR
447600941121     C*
447700941121     C*  Eseguo apertura file di stampa
447800941121     C                   MOVEA     CMDP          CMDPX            65
447900941121     C                   Z-ADD     65            LUNCMD           15 5
448000941121     C                   CALL      'QCMDEXC'
448100941121     C                   PARM                    CMDPX
448200941121     C                   PARM                    LUNCMD
448300941121     C                   OPEN      QSYSPRT
448400941121     C*  Azzero variabili totale
448500941121     C                   Z-ADD     0             STPTPA
448600941121     C                   Z-ADD     0             STPTCL
448700941121     C                   Z-ADD     0             STPTPK
448800941121     C                   Z-ADD     0             STPTVL
448900941121     C                   Z-ADD     0             STPTST
449000970620     C                   Z-ADD     0             STPNSC
449100970305     C                   Z-ADD     0             STPNFA
449200970620     C                   Z-ADD     0             STPTNP
449300970620     C                   Z-ADD     0             STPTNV
449400941121     C                   Z-ADD     0             STPNGC
449500941121     C                   Z-ADD     0             STPNLA
449600941121     C                   Z-ADD     0             STPTNC
449700941121     C                   Z-ADD     0             STPTPK
449800941121     C                   Z-ADD     0             STPTVL
449900941121     C*  Lettura subfile dati di consegna
450000941121     C                   Z-ADD     1             NRR2
450100970926     C                   Z-ADD     1             SAVNR2
450200941121     C     NRR2          CHAIN     LR53S02                            96
450300941121     C     *IN96         DOWEQ     '0'
450400011121     C                   CLEAR                   fiftd000
450500941122     C                   Z-ADD     0             SAVPKG
450600941122     C                   Z-ADD     0             SAVNCL
450700941122     C                   Z-ADD     0             SAVVOL
450800941121     C*  Alloco record FNARB
450900941121     C                   Z-ADD     1             §§                2 0
451000941129     C                   Z-ADD     V2CAAS        KAAS
451100941129     C                   Z-ADD     V2CLNP        KLNP
451200941129     C                   Z-ADD     V2CNRS        KNRS
451300941129     C                   Z-ADD     V2CNSP        KNSP
451400941121     C     KARB1         CHAIN     FNARB01L                           7190
451500941121     C     *IN90         DOWEQ     '1'
451600941121     C                   ADD       1             §§
451700941121     C     §§            IFGT      4
451800941121     C*  Dopo 4 Tentativi mando messaggio
451900941121     C                   CALL      'FNLR53C'
452000941121     C                   PARM                    KPJBA
452100941121     C                   END
452200941121     C     KARB1         CHAIN     FNARB01L                           7190
452300941121     C                   END
452400971120     C*  reperisco AZORG della linea di arrivo
452500971120     C     ARBLNA        CHAIN     AZORG01L                           74
452600020717     C  N74              MOVEL     ORGDE3        OG143
452700971120     C                   MOVEL     'N'           WAGDT3            1
452800941122     C*  Gestisco rottura numero documento
452900031008     C     WSVNDC        IFNE      VNMDOC
453000031008     C                   Z-ADD     0             WNSS              3 0
453100031008     C                   EXSR      NEWDOC
453200031008     C                   END
453300941122     C*  Giro la data consegna
453400950602     C                   ADD       1             WNSS              3 0
453500941222     C                   MOVEL     VH2DDC        ANNO
453600941222     C                   MOVE      VH2DDC        MMGG              4 0
453700941222     C                   MOVEL     MMGG          MM1               2 0
453800941222     C                   MOVE      V02DTC        MM2               2 0
453900941222     C     MM1           IFEQ      12
454000941222     C     MM2           ANDEQ     1
454100941222     C                   ADD       1             ANNO
454200941222     C                   END
454300941222     C     MM1           IFEQ      1
454400941222     C     MM2           ANDEQ     12
454500941222     C                   SUB       1             ANNO
454600941222     C                   END
454700941122     C     V02DTC        IFEQ      0
454800941122     C                   Z-ADD     DDC(I)        G02DAT
454900941122     C                   ELSE
455000941222     C                   MOVEL     V02DTC        G02DAT
455100941222     C                   MOVE      ANNO          G02DAT
455200941122     C                   END
455300941122     C                   MOVE      *ZEROS        G02INV
455400941122     C                   MOVEL     *BLANKS       G02ERR
455500941122     C                   CALL      'XSRDA8'
455600941122     C                   PARM                    WLBDA8
455700941122     C                   Z-ADD     G02INV        DATCON
455800941122     C* Preparo l'ora avventa/mancata consegna
455900941122     C     V02HCO        IFNE      0
456000941122     C                   MOVEL     V02HCO        ORACON            4 0
456100941122     C                   MOVE      V02MCO        ORACON
456200941122     C                   ELSE
456300941122     C                   Z-ADD     HRC(I)        ORACON
456400941122     C                   END
456500941122     C*  Aggiorno campi stampa con dati di dettaglio spedizione
456600941122     C                   Z-ADD     ARBAAS        STPAAS
456700941122     C                   Z-ADD     ARBLNP        STPLNP
456800941122     C                   Z-ADD     ARBNRS        STPNRS
456900941122     C                   Z-ADD     ARBNSP        STPNSP
457000941123     C                   MOVE      ARBMGS        GG                2 0
457100941123     C                   MOVEL     ARBMGS        MM                2 0
457200941122     C                   MOVE      MM            STPGMS
457300941122     C                   MOVEL     GG            STPGMS
457400941130     C                   MOVEL     ARBRSD        STPRSD           29
457500951025     C                   Z-ADD     VH2DDT        ARBDDC
457600141212     C*
457700941122     C*  Mancata consegna
457800941122     C     V02CCN        IFNE      *BLANKS
457900941122     C                   EXSR      NOCONS
458000941212     C                   EXSR      STPCON
458100941122     C*  Aggiorno archivio conteggi padroncini
458200941122     C                   EXSR      UPDFTD
458300130722     c                   exsr      VerCorAR5
458400941122     C                   ELSE
458500941122     C*  Se no ritorno all'incasso aggiorno dati consegna
458600941122     C     V2CCON        IFEQ      'NO'
458700941122     C*  Consegna parziale
458800970304     C     V02CPZ        IFEQ      'P'
458900941123     C                   EXSR      CONPAR
459000941122     C                   ELSE
459100941122     C*  Consegna normale
459200941122     C                   EXSR      NOPARZ
459300941122     C                   END
459400941123     C                   EXSR      CONDAT
459500141212      *
459600141212ab   C*  Controllo se immesso codice particolarità
459700141212ab   C                   CLEAR                   DS7R
459800141212ab   C     V2CGMA        IFNE      *BLANKS                                      --- 03 ---
459900141212ab   C                   Z-ADD     1             X
460000141212ab   C     V2CGMA        LOOKUP    C7R(X)                                 95
460100141212ab   C     *IN95         IFEQ      '1'
460200141212ab   C                   MOVEL     D7R(X)        DS7R
460300141212ab   C                   END
460400141212ab   C                   END                                                    --- 03 ---
460500141212ab   C*  SOLO se particolarità di consegna con PINCODE
460600141212ab    *  x Pro-memoria di aver Forzato scrive una RA (61)
460700141212ab   C                   if        §7RpinCODE ='S' and
460800141212ab   c                             v2hForza = 'F'
460900141212ab   c                   exsr      write_RA_61
461000141212ab   c                   end
461100141212     C*
461200130722      *consegna parziale o totale se distinta pda cerca coordinate geo e
461300130722      * le trascrive su FIAR5
461400130722     c                   if        wpda = 'S'
461500141202ab    ***
461600141202ab   c                   exsr      Found_CET
461700141202ab    ***
461800141202ab   c                   if        ecco_il_CET = 'S'
461900140609     c                   exsr      srdocum
462000130722     c                   endif
462100130722     c                   exsr      AggCorAR5
462200130722     c                   else
462300130722     c                   exsr      VerCorAR5
462400130722     c                   endif
462500941122     C                   END
462600941130     C*
462700941130     C                   EXSR      STPCON
462800941122     C*  Incassi
462900941122     C     V2CNRA        IFNE      0
463000941122     C                   EXSR      AGGINC
463100941122     C                   END
463200941219     C*  Controllo se devo gestire non incasso
463300941219     C     V2CICA        IFEQ      'S'
463400941219     C     ARBICA        ANDNE     'Y'
463500941219     C                   MOVEL     'Y'           ARBICA
463600941219     C                   END
463700941219     C     V2CICC        IFEQ      'S'
463800941219     C     ARBICC        ANDNE     'Y'
463900941219     C                   MOVEL     'Y'           ARBICC
464000941219     C                   END
464100941130     C*  Se i flag di incasso sono uguali a *blanks impsto 'S'
464200941130     C     ARBICC        IFEQ      *BLANKS
464300990913     C     ARBACC        ANDEQ     'S'
464400941130     C                   MOVEL     'S'           ARBICC
464500941130     C                   END
464600941130     C     ARBICA        IFEQ      *BLANKS
464700990913     C     ARBACA        ANDEQ     'S'
464800941130     C                   MOVEL     'S'           ARBICA
464900941130     C                   END
465000941122     C*
465100941129     C                   MOVEL     ARBICC        COMICC
465200941122     C                   MOVEL     *BLANKS       ARBFT1
465300941123     C                   MOVEL     *BLANKS       ARBDT1
465400020826     C                   Z-ADD     dutpou        ARBIPP
465500020826     C                   Z-ADD     dutcou        ARBCUC
465600941122     C                   MOVE      'S'           ARBFDC
465700941122     C                   Z-ADD     *ZEROS        ARBNMI
465800950602     C     ARBNSS        IFEQ      0
465900970304     C     V02CPZ        ANDNE     'P'
466000970311     C     V2CIDD        ANDNE     'P'
466100950602     C                   Z-ADD     WNSS          ARBNSS
466200950602     C                   END
466300941123     C                   UPDATE    FNARB000
466400941122     C*  Aggiorno archivio conteggi padroncini
466500020524     C*    CNTPDR        IFEQ      'S'
466600941122     C                   EXSR      UPDFTD
466700020524     C*                  ELSE
466800020524     C     CNTPDR        IFEQ      'N'
466900970926     C                   EXSR      CHKSTP
467000941122     C                   END
467100941122     C*  Se c'è bolla mamma la aggiorno
467200941122     C     VH2AAP        IFNE      *ZEROS
467300941122     C                   EXSR      SPEMAM
467400941122     C                   END
467500941122     C*
467600941122     C                   END
467700140306     C*aggiorna tipo rekord GEN  spedizione esitata con PDA o no
467800140306     c                   exsr      AggGenAR5
467900941122     C*  Note Consegna
468000941122     C     V2CNT1        IFNE      *BLANKS
468100941122     C                   EXSR      NOTCON
468200941122     C                   END
468300970121     C*  Note MANCATA CONSEGNA
468400970121     C     V2CNM1        IFNE      *BLANKS
468500970121     C                   EXSR      NOTNM1
468600970121     C                   END
468700970121     C     V2CNM2        IFNE      *BLANKS
468800970121     C                   EXSR      NOTNM2
468900970121     C                   END
469000070313     C*  cancella eventuale FIARP se è rimasto
469100070313     c     karp          chain     fiarp01l
469200070313     c                   if        %found(fiarp01l)
469300070313     c                   delete    fiarp000
469400070313     C                   Endif
469500941122     C*  Nuovo record
469600941122     C                   ADD       1             NRR2
469700941122     C     NRR2          CHAIN     LR53S02                            96
469800941122     C                   END
469900031008     c                   do        5             i
470000031008     c                   if        ndc(i) > 0
470100031008     C                   EXSR      NEWDOCb
470200031008     c                   endif
470300031008     c                   enddo
470400941122     C                   EXSR      GESTOT
470500941122     C                   CLOSE     QSYSPRT
470600971001     C*  MEMORIZZAZIONE STOP PADRONCINI A FINE CICLO
470700971001     C     YY            IFGT      *ZEROS
470800971001     C                   Z-ADD     SAVNR2        NRR2
470900971001     C                   EXSR      ELASTP
471000971001     C                   Z-ADD     NRR2          SAVNR2
471100971001     C                   CLEAR                   NST
471200971001     C                   CLEAR                   SPS
471300971001     C                   MOVE      *ZEROS        YY                3 0
471400971001     C                   END
471500941122     C*
471600941122     C                   COMMIT
471700090804      * operazioni dopo commit
471800090804     C*  verifica record spia per variazione bolla
471900090804     c                   exsr      srback2
472000090804     C*  cancella record spia per variazione bolla
472100090804     c     *loval        setll     wfvarb1l
472200090804     c                   do        *hival
472300090804     c                   read      wfvarb1l
472400090804     c                   if        %eof(wfvarb1l)
472500090804     c                   leave
472600090804     C                   Endif
472700090804     c                   delete    wfvarb00
472800090804     C                   Enddo
472900941122     C                   ENDSR
473000941122      *---------------------------------------------------------*
473100941122      *  Nuovo Documento
473200941122      *---------------------------------------------------------*
473300941122     C     NEWDOC        BEGSR
473400941122     C*
473500970227     C*  Se non è la prima volta e ci sono delle bolle con la
473600970227     C*  data consegna = a quella della distinta richiamo pgm
473700031008     C     WSVNDC        IFNE      0
473800031008     C     WNBOL         IFNE      0
473900970227     C                   MOVEL     'S'           WRISTA            1
474000970227     C                   MOVEL     *BLANKS       SA2TLA            1
474100970227     C                   MOVEL     'C'           SA2TRC            1
474200970228     C                   Z-ADD     CONDIS        SA2DTS            8 0
474300970227     C                   Z-ADD     D53LNA        SA2LNA            3 0
474400970227     C                   CALL      'FNLSA2R3'
474500970227     C                   PARM                    SA2TLA            1
474600970227     C                   PARM                    SA2TRC            1
474700970227     C                   PARM                    SA2DTS            8 0
474800970227     C                   PARM                    SA2LNA            3 0
474900031008     C                   END
475000970926     C*  MEMORIZZO STOP PADRONCINI
475100971001     C     YY            IFGT      *ZEROS
475200970926     C                   Z-ADD     SAVNR2        NRR2
475300970926     C                   EXSR      ELASTP
475400970926     C                   END
475500971110     C                   Z-ADD     NRR2          SAVNR2
475600031008     C                   END
475700970926     C                   CLEAR                   NST
475800970926     C                   CLEAR                   SPS
475900970926     C                   MOVE      *ZEROS        YY                3 0
476000031008     c                   z-add     1             i                 2 0
476100031008     c     arbndc        lookup    ndc(i)                                 95
476200031008     C     DDC(I)        IFEQ      *ZEROS
476300031008     C     *IN95         OREQ      '0'
476400100210     C                   Z-ADD     WOGGI         DDC(I)
476500031008     C                   END
476600031008     C*  Giro data consegna x distinta
476700031008     C                   MOVE      DDC(I)        G02DAT
476800031008     C                   MOVEL     *BLANK        G02ERR
476900031008     C                   CALL      'XSRDA8'
477000031008     C                   PARM                    WLBDA8
477100031008     C                   Z-ADD     G02INV        CONDIS            8 0
477200031008     C*
477300031008     C                   MOVE      DTD(I)        ANNO1
477400031008     C                   MOVEL     DTD(I)        MMGG
477500031008     C                   MOVE      MMGG          MM1
477600031008     C                   Z-ADD     0             WNBOL             4 0
477700031008     C                   MOVE      VNMDOC        WSVNDC
477800031008     c                   endsr
477900031008     c*-----------------------------------------------------------------
478000031008     c     newdocb       begsr
478100031008     c*-----------------------------------------------------------------
478200970227     C*  data e ora di consegna nuovo documento
478300941122     C*  Sfleggo FLFVV
478400000821     C                   MOVEL     KPJBU         WPJBU           256
478500031024      * per estero è a zero
478600031024     c                   if        d53fgs = 0
478700031024     c                   z-add     d53lna        kfgs
478800031024     c                   else
478900031016     c                   z-add     d53fgs        kfgs
479000031024     c                   end
479100031016     C*                  Z-ADD     ARBifp        Kfgs
479200031008     C                   Z-ADD     ndc(i)        KNFV
479300000626     C                   Z-ADD     4             KNPG
479400070316      * se trovo chiudo il dst
479500070316     C                   Z-ADD     ndc(i)        KNFV6
479600070410     C     Kdst          CHAIN     fidst09L                           9594
479700070316     C     *IN95         IFEQ      '0'
479800070410     C                   move      'S'           dstFCF
479900070410     C                   move      FPP(I)        dstFPP
480000070410     C                   move      PEP(I)        dstPEP
480100070316     c                   update    fidst000
480200070316     c                   endif
480300120418     c* crea la fase di chiusura distinta su AS400
480400120418     c                   eval      dsfatb = ' '
480500120418     c                   eval      dsffgs = dstfgs
480600120418     c                   eval      dsfnpg = dstnpg
480700120418     c                   eval      dsfnfv = dstnfv
480800120418     c                   eval      dsftrd = 'CDC'
480900120418     c                   time                    DSFDTORIN
481000120418     C                   TIME                    dtorin           14 0
481100120418     C                   MOVEl     dtorin        orin              6 0
481200120418     C                   MOVE      orin          DSFDTORIN
481300120418     C                   MOVE      dtorin        G02DAT
481400120418     C                   MOVEL     *BLANK        G02ERR
481500120418     C                   CALL      'XSRDA8'
481600120418     C                   PARM                    WLBDA8
481700120418     C                   movel     G02INV        DSFDTORIN
481800120418     c                   eval      DSFPDAAS4 = 'A'
481900120418     c                   eval      DSFdati = knmus
482000120418     c                   write     fidsf00
482100000821      *                    ------         ----
482200020823     C     KFVV          CHAIN     FNFVV01L                           95
482300941122     C     *IN95         IFEQ      '0'
482400941122     C                   MOVE      'S'           FVVFCF
482500941122     C                   MOVE      FPP(I)        FVVFPP
482600941122     C                   MOVE      PEP(I)        FVVPEP
482700941123     C                   UPDATE    FNFVV000
482800000526     C                   MOVEL     FVVDS         FVVDS2
482900000821      *
483000000821      *  Sottometto Trasmissione Spunte CAT 4
483100041021     C*                  CLEAR                   DSLR01
483200041021     C*                  Z-ADD     FVVFGS        DLRFGS                         FIL.GESTIONE
483300041021     C*                  Z-ADD     FVVNPG        DLRNPG                         CATEGORIA
483400041021     C*                  Z-ADD     FVVNFV        DLRNFV                         NUM.FOGLIO
483500041021     C*                  MOVE(P)   '0'           W52ALF           52            "PARNUM"
483600041021     C*                  MOVEL(P)  W52ALF        KPJBU
483700041021     C*                  MOVEL     DSLR01        KPJBU
483800041021     C*                  MOVE      'LR96'        KCOAZ
483900041021     C*                  CALL      'BCH10'
484000041021     C*                  PARM                    KPJBA
484100000821      *
484200000619     C                   Z-ADD     8             KNPG
484300000821      *                    ------         ----
484400020823     C     KFVV          CHAIN     FNFVV01L                           95
484500000526     C     *IN95         IFEQ      '0'
484600000526     C                   MOVEL     FVVDS2        FVVDS
484700000619     C                   Z-ADD     8             FVVNPG
484800000526     C                   UPDATE    FNFVV000
484900010601      *  Sottometto Trasmissione Spunte CAT 8
485000010601     C                   CLEAR                   DSLR01
485100041021     C*                  Z-ADD     FVVFGS        DLRFGS                         FIL.GESTIONE
485200041021     C*                  Z-ADD     FVVNPG        DLRNPG                         CATEGORIA
485300041021     C*                  Z-ADD     FVVNFV        DLRNFV                         NUM.FOGLIO
485400041021     C*                  MOVE(P)   '0'           W52ALF           52            "PARNUM"
485500041021     C*                  MOVEL(P)  W52ALF        KPJBU
485600041021     C*                  MOVEL     DSLR01        KPJBU
485700041021     C*                  MOVE      'LR96'        KCOAZ
485800041021     C*                  CALL      'BCH10'
485900041021     C*                  PARM                    KPJBA
486000010601     C*
486100000526     C                   MOVEL     FVVDS2        FVVDS
486200000821      *
486300000821      *
486400941122     C                   END
486500000526     C                   END
486600000821     C                   MOVEL     WPJBU         KPJBU
486700000626     C                   Z-ADD     4             KNPG
486800000821      *                    ------         ----
486900941122     C*  Aggiorno archivio conteggi padroncini
487000020524     C*    CNTPDR        IFEQ      'S'
487100941122     C                   EXSR      UPDFTT
487200020524     C*                  END
487300031008
487400941122     C                   ENDSR
487500031008      *---------------------------------------------------------*
487600031008      *  pretesta
487700031008      *---------------------------------------------------------*
487800031008     C     pretesta      BEGSR
487900031008     C*  Stampo dati testata
488000031008     C                   MOVEL     CMAT          TPPRE            10
488100031013     C                   MOVEL     CMAT          TPPRE2           10
488200031013     C                   MOVEL     CMAT          TPPRE3           10
488300031013     C                   MOVEL     CMAT          TPPRE4           10
488400031013     C                   MOVEL     CMAT          TPPRE5           10
488500031008     C     FPP(1)        IFEQ      'P'
488600031008     C                   MOVEL     CPOM          TPPRE            10
488700031008     C                   END
488800031008     C                   Z-ADD     NDC(1)        STPNDC
488900031008     C                   Z-ADD     DTD(1)        STPDDC
489000031008     C                   Z-ADD     ARBPDC        STPPDR
489100031008     C                   MOVEL     D53DSF        DESPAD
489200031008     c                   setoff                                       737677
489300031008     c                   setoff                                       78
489400031008     c                   if        ndc(2) > 0
489500031013     C     FPP(2)        IFEQ      'P'
489600031013     C                   MOVEL     CPOM          TPPRE2
489700031013     C                   END
489800031008     c                   seton                                        73
489900031008     c                   endif
490000031008     c                   if        ndc(3) > 0
490100031013     C     FPP(3)        IFEQ      'P'
490200031013     C                   MOVEL     CPOM          TPPRE3
490300031013     C                   END
490400031008     c                   seton                                        76
490500031008     c                   endif
490600031008     c                   if        ndc(4) > 0
490700031013     C     FPP(4)        IFEQ      'P'
490800031013     C                   MOVEL     CPOM          TPPRE4
490900031013     C                   END
491000031008     c                   seton                                        77
491100031008     c                   endif
491200031008     c                   if        ndc(5) > 0
491300031013     C     FPP(5)        IFEQ      'P'
491400031013     C                   MOVEL     CPOM          TPPRE5
491500031013     C                   END
491600031008     c                   seton                                        78
491700031008     c                   endif
491800031008     C                   EXCEPT    TESTA
491900031008     C*
492000031008     C                   ENDSR
492100941122      *---------------------------------------------------------*
492200941122      *  No Consegna
492300941122      *---------------------------------------------------------*
492400941122     C     NOCONS        BEGSR
492500941122     C*
492600951205     C*  Se evento immesso da utente è un lasciato avviso devo
492700951206     C*  ristampare la bolle
492800070405     C                   EXSR      AVVSTB
492900951205     C*
493000960205     C     V2CEVP        IFEQ      *BLANKS
493100960205     C                   MOVEL     V02CCN        V2CEVP
493200960205     C                   END
493300941125     C                   Z-ADD     1             X
493400040224     C*    V02CCN        LOOKUP    C2A(X)                                 95
493500040224     C     V2cevp        LOOKUP    C2A(X)                                 95
493600951205     C                   MOVEL     D2A(X)        DS2A
493700070406     C                   MOVEL     D2z(X)        DS2z
493800941130     C                   MOVEL     §2ADES        STPDCO           20
493900941123     C                   MOVEL     §2AFTC        STPTMC
494000941206     C                   MOVEL     V02CCN        STPCMC
494100941122     C*  Incremento totali stampa
494200941122     C                   ADD       1             STPNSC
494300941122     C                   ADD       V02NCL        STPTNC
494400941122     C                   ADD       V02PKG        STPTNP
494500941122     C                   ADD       V02VOL        STPTNV
494600941122     C*  Totale non fatte, giacenti, lasc. avviso
494700941122     C     §2AFTC        IFEQ      'N'
494800941130     C                   ADD       1             STPNFA
494900941122     C                   ELSE
495000941122     C     §2AFTC        IFEQ      'G'
495100941122     C                   ADD       1             STPNGC
495200941122     C                   ELSE
495300941122     C     §2AFTC        IFEQ      'A'
495400941122     C                   ADD       1             STPNLA
495500941122     C                   END
495600941122     C                   END
495700941122     C                   END
495800941122     C*  Controllo se addizionare numero tentativi consegna
495900941122     C     §2ANTC        IFEQ      'S'
496000941122     C                   ADD       1             ARBNTC
496100941122     C                   END
496200951228     C*  Se forzato l.avviso metto 2 nei tentavi consegna
496300951228     C     V2CF08        IFEQ      '1'
496400990419     C     V2C4NT        ADD       1             ARBNTC
496500951228     C                   END
496600941122     C*  Controllo se devo mettere blocco su bolla
496700941122     C     §2AAGB        IFEQ      'N'
496800950113     C     V2CEVP        OREQ      'RIC'
496900941122     C                   MOVEL     *BLANKS       ARBCMC
497000941122     C                   ELSE
497100960306     C                   MOVE      V2CEVP        ARBCMC
497200941122     C                   END
497300941122     C*  Se vale come giacenza metto blocco
497400941122     C     §2AFTG        IFEQ      'S'
497500950113     C     V2CEVP        ANDNE     'RIC'
497600941122     C                   MOVE      §2AFTC        ARBFBC
497700941122     C                   END
497800941122     C*
497900950113     C     V2CEVP        IFNE      'RIC'
498000941122     C                   MOVE      §2AFTC        ARBTMC
498100951205     C                   ELSE
498200951205     C                   MOVE      WSVFTC        ARBTMC
498300950113     C                   END
498400140207      *scrittura record spia per invio sms/mail al destinatario in caso
498500140207      *di lasciato avviso al 1° tentativo di consegna
498600140207      *N.B deve essere richiamato prima dell'aggiornamento di arb
498700140218     C     V2CEVP        IFEQ      'RIC'
498800140207     c                   exsr      srspiasms
498900140207     c                   end
499000001229     C*PREPARO CAMPI PER NOTE EVENTI
499100001229      *
499200941122     C                   Z-ADD     0             ARBNGD
499300020902     C                   Z-ADD     0             ARBifp
499400131112     C                   Z-ADD     arbndc        wARBNDC           7 0
499500020902     C                   Z-ADD     0             ARBNDC
499600941122     C                   Z-ADD     0             ARBSTP
499700941122     C                   Z-ADD     0             ARBNSS
499800941122     C                   MOVEL     *BLANKS       ARBFDC
499900071003     C* se ho la data consegna richiesta proposta da telefonata non blocco
500000071003     c* la bolla per lasciato avviso
500100101020      * ?asteriscato altrimenti venivano R.B le variazioni inserite da fiarp a fine chiusura
500200101020      * ?i campi vengono puliti dal pgm di assegnazione automatico prima di rimettere a giro le boll
500300110418      * ?TOLTI ASTERISCHI RIMANEVANO BLOCCATE BOLLE CON LASCIATO AVVISO E RIMANDO ALLO SVINCOLO
500400110418     c                   if        v2harp = '1' and arbdcr > 0 and
500500110418     c                             arbfbc = 'A'
500600110418     c                   clear                   arbfbc
500700110418     c                   clear                   arbcmc
500800110418     c                   end
500900941123     C                   UPDATE    FNARB000
501000941122     C*  Controllo se devo gestire scrittura eventi
501100131120     c                   clear                   scriviCEP         1
501200941122     C     §2AEVB        IFEQ      'S'
501300950113     C                   MOVEL     V2CEVP        EVBCEV
501400941122     C                   EXSR      EVENTI
501500941122     C                   END
501600131108     c* verifica se ricevuto doppio evento CET/CEP
501700131120     c                   if        scrivicep = 'S'
501800131108     c                   exsr      eventicep
501900131120     c                   endif
502000070319     c*verifica se inserire il blocco alla consegna se esiste una c.a. aperta
502100070319     c*solo se in organigramma è abilitato il flag per la gestione
502200070319     c                   if        §oggeot = 'S'
502300070319     c                   exsr      insbloc
502400070319     c                   endif
502500110328      * scrive spia file fiilv00f solo per distinte PDA
502600110328     c                   if        wpda  = 'S'
502700110328     c                   move      'S'           wnocons           1
502800110328     c                   exsr      scrivispia
502900110328     c                   endif
503000950615     C*
503100950919     C*  Se non fatta chiudo l'anomalia con causale 'CH'
503200950915     C                   EXSR      CLOANM
503300950919     C*
503400941122     C                   ENDSR
503500950615     C*-----------------------------------------------------*
503600950615     C*  Chiudo eventuali anomalie su dettaglio segnacolli
503700950615     C*-----------------------------------------------------*
503800950915     C     CLOANM        BEGSR
503900960418     C*
504000960418     C                   CLEAR                   DSLR33
504100960418     C                   MOVE      '4'           D33FSC
504200961106     C                   Z-ADD     OGGI          D33DCH
504300960418     C                   MOVEL     'CH'          D33CCH
504400960426     C                   MOVEL     V02CCA        D33CCA
504500960418     C*
504600000207     C     HARLNP        IFNE      0
504700990519     C                   Z-ADD     HARAAS        KAA4
504800950915     C                   Z-ADD     HARLNP        KLNP
504900950915     C                   Z-ADD     HARNRS        KNRS
505000950915     C                   Z-ADD     HARNSP        KNSP
505100960418     C*
505200960418     C                   Z-ADD     1             II                1 0
505300960418     C     II            DOWLE     2
505400960418     C     II            IFEQ      1
505500960418     C*  Vado a chiudere le anomalie di fuori posto
505600950915     C                   Z-ADD     135           KCAA
505700960418     C                   ELSE
505800960418     C*  Vado a chiudere le anomalie di bolla in consegna su altra disti.
505900960418     C                   Z-ADD     150           KCAA
506000960418     C                   END
506100990519     C     KANM1         CHAIN     FNANM01L                           34
506200950915     C     *IN34         DOWEQ     '0'
506300960418     C                   Z-ADD     ANMNR1        D33NRR
506400960418     C                   CALL      'FNLR33R'
506500960418     C                   PARM                    DSLR33
506600990519     C     KANM1         READE     FNANM01L                               34
506700960418     C                   ENDDO
506800960418     C                   ADD       1             II
506900960418     C                   ENDDO
507000000207     C                   END
507100950615     C*
507200950915     C                   ENDSR
507300941122      *---------------------------------------------------------*
507400941122      *  Consegna Parziale
507500941122      *---------------------------------------------------------*
507600941123     C     CONPAR        BEGSR
507700941122     C*
507800950410     C                   MOVEL     'N'           WMDDTI            1
507900950331     C                   MOVEL     'N'           WFBS
508000021107     C                   clear                   wpesparz
508100021107     C                   clear                   wvolparz
508200941201     C                   Z-ADD     V02PKG        SAVPKG
508300941201     C                   Z-ADD     V02NCL        SAVNCL
508400941201     C                   Z-ADD     V02VOL        SAVVOL
508500941122     C                   Z-ADD     V2CPDS        NRR7
508600941122     C                   Z-ADD     0             V02NCL
508700941122     C     NRR7          CHAIN     LR53S07                            95
508800941122     C     *IN95         DOWEQ     '0'
508900941122     C     SFHAAS        ANDEQ     HARAAS
509000941122     C     SFHLNP        ANDEQ     HARLNP
509100941122     C     SFHNRS        ANDEQ     HARNRS
509200941122     C     SFHNSP        ANDEQ     HARNSP
509300941129     C     SFHSEL        IFNE      '1'
509400941129     C                   Z-ADD     SFHAAS        KAAS1
509500941129     C                   Z-ADD     SFHLNP        KLNP1
509600941129     C                   Z-ADD     SFHNRS        KNRS1
509700941129     C                   Z-ADD     SFHNSP        KNSP1
509800941129     C                   Z-ADD     SFHSCN        KNSC
509900941129     C     KART2         CHAIN     FNART01L                           94
510000941122     C     *IN94         IFEQ      '0'
510100941122     C                   Z-ADD     DATCON        ARTDCM
510200950214     C                   MOVEL     *BLANKS       ARTDAB
510300961113     C                   MOVEL     ' '           ARTFTR
510400941122     C* Se manca data arrivo imposto = Data consegna
510500941122     C     ARTDAM        IFEQ      0
510600970620     C     V02CCA        ANDNE     '5'
510700950331     C                   MOVEL     'S'           WFBS              1
510800941122     C                   Z-ADD     DATCON        ARTDAM
510900941122     C                   Z-ADD     85            ARTNAP
511000941122     C                   END
511100961219     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
511200961219     C* ARTDTR  solo x estero
511300961219     C     ARTDTR        IFEQ      0
511400961219     C     V02CCA        ANDEQ     '5'
511500970114     C     ARBLNA        ANDNE     ARBLNP
511600020717     C**** ORGFL1        ANDEQ     'E'
511700020717     C     §OgNTW        ifeq      'FED'
511800020717     C     §OgNTW        oreq      'EEX'
511900020717     C     §OgNTW        oreq      'EUP'
512000961219     C                   Z-ADD     DATCON        ARTDTR
512100020717     C                   End
512200961219     C                   END
512300941122     C                   UPDATE    FNART000
512400941122     C* Sistemo eventuali anomalie del collo consegnato
512500941122?    C                   EXSR      GESANM
512600021107      *
512700021107      * somma i colli consegnati
512800941122     C                   ADD       1             V02NCL
512900021107      *
513000021107      *  Se si tratta di un unica spedizione oppure se multicollo
513100021107      *   e tutti i colli sono passati x VDL memorizza il peso reale
513200021107      *    da sottrarre in seguito per il calcolo del parziale
513300050208     C     arbncp        ifeq      arbncl
513400021107     C                   add       artPUC        wpesparz
513500021107     C                   end
513600021107      *
513700050208     C     arbncr        ifeq      arbncl
513800021107     C                   add       artVUC        wvolparz
513900021107     C                   endif
514000021107      *
514100941122     C                   END
514200941130     C                   END
514300941129     C                   ADD       1             NRR7
514400941129     C     NRR7          CHAIN     LR53S07                            95
514500941122     C                   END
514600941206     C*
514700021107     C*  Ricalcolo il peso / volume
514800021107     C*
514900021107      *  Se si tratta di un unica spedizione oppure se multicollo
515000021107      *   e tutti i colli sono passati x VDL
515100021107      *    imposta il peso calcolato con precisione dal VDL.
515200021107     c                   if        wpesparz > 0
515300021107     c                   z-add     wpesparz      V02pkg
515400021107     c                   else
515500021107     C**** ARBPKF        MULT      V02NCL        APPOG            18 3
515600021107     C     V02PKG        MULT      V02NCL        APPOG            18 3
515700040211     C***  APPOG         DIV(H)    ARBNCL        V02PKG
515800040211     C     APPOG         DIV(H)    SAVNCL        V02PKG
515900021107     c                   end
516000021107     C*
516100021107     C*
516200021107      *  Se si tratta di un unica spedizione oppure se multicollo
516300021107      *   e tutti i colli sono passati x VDL
516400021107      *    imposta il volume calcolato con precisione dal VDL.
516500021107     c                   if        wvolparz > 0
516600021107     c                   z-add     wvolparz      V02vol
516700021107     c                   else
516800941207     C     V02VOL        MULT      V02NCL        APPOG            18 3
516900040211     C***  APPOG         DIV(H)    ARBNCL        V02VOL
517000040211     C     APPOG         DIV(H)    SAVNCL        V02VOL
517100021107     c                   end
517200941201     C*
517300941201     C*  Incremento totale colli non consegnati
517400941201     C                   SUB       V02NCL        SAVNCL
517500941201     C                   SUB       V02PKG        SAVPKG
517600941201     C                   SUB       V02VOL        SAVVOL
517700941201     C                   ADD       SAVNCL        STPTNC
517800941201     C                   ADD       SAVPKG        STPTNP
517900941201     C                   ADD       SAVVOL        STPTNV
518000941122     C*
518100941122     C*  Chain tab. eventi
518200941123     C                   MOVEL     V02CPZ        JEY               3
518300941122     C                   SETOFF                                       95
518400941122     C                   Z-ADD     1             X
518500941123     C     JEY           LOOKUP    C2A(X)                                 95
518600941123     C                   MOVEL     D2A(X)        DS2A
518700070406     C                   MOVEL     D2z(X)        DS2z
518800941122     C*
518900941122     C* Se c'è 'S' in tentata consegna vado addizionare 1
519000941130     C     §2ANTC        IFEQ      'S'
519100941202     C                   ADD       1             ARBNTC
519200941122     C                   END
519300941122     C* Se evento da memorizzare lo memorizzo
519400131120     c                   clear                   scriviCEP
519500941122     C     §2AEVB        IFEQ      'S'
519600941130     C                   MOVEL     *BLANKS       EVBCEV
519700941130     C                   MOVEL     V02CPZ        EVBCEV
519800001229     C* PREPARO CAMPI NOTE EVENTI
519900941122?    C                   EXSR      EVENTI
520000941122     C                   END
520100131108     c* verifica se ricevuto doppio evento CET/CEP
520200131120     c                   if        scrivicep = 'S'
520300131108     c                   exsr      eventicep
520400131120     c                   endif
520500070319     C* Incremento contatore consegne parziali
520600941122     C                   ADD       1             STPTPA
520700941122     C*
520800941122     C                   MOVE      §2AFTC        ARBTMC
520900941122     C                   Z-ADD     0             ARBNGD
521000941122     C                   Z-ADD     0             ARBDCM
521100941122     C                   Z-ADD     0             ARBHMC
521200941122     C                   Z-ADD     0             ARBSTP
521300941122     C                   Z-ADD     0             ARBNSS
521400941122     C* Se prima consegna parziale aggiorno la data
521500941122     C     ARBDCP        IFEQ      0
521600941122     C                   Z-ADD     DATCON        ARBDCP
521700941122     C                   END
521800070319     c*verifica se inserire il blocco alla consegna se esiste una c.a. aperta
521900070319     c*solo se in organogramma è abilitato il flag per la gestione
522000070319     c                   if        §oggeot = 'S'
522100070319     c                   exsr      insbloc
522200070319     c                   endif
522300110328      * scrive spia file fiilv00f solo per distinte PDA
522400110207     c                   if        wpda  = 'S'
522500110328     c                   clear                   wnocons
522600110207     c                   exsr      scrivispia
522700110207     c                   endif
522800950919     C*  Se non fatta chiudo l'anomalia con causale 'CH'
522900950919     C                   EXSR      CLOANM
523000941122     C*
523100941122     C                   ENDSR
523200951205      *---------------------------------------------------------*
523300951205      *  Controllo se devo ristampare la bolla
523400951205      *---------------------------------------------------------*
523500951205     C     AVVSTB        BEGSR
523600951205     C*
523700951205     C                   Z-ADD     1             X
523800951205     C     V02CCN        LOOKUP    C2A(X)                                 95
523900951205     C                   MOVEL     D2A(X)        DS2A
524000070406     C                   MOVEL     D2z(X)        DS2z
524100951205     C                   MOVEL     §2AFTC        WSVFTC            1
524200951205     C*
524300951205     C                   ENDSR
524400941122      *---------------------------------------------------------*
524500941122      *  Consegna Normale
524600941122      *---------------------------------------------------------*
524700941122     C     NOPARZ        BEGSR
524800941122     C*
524900950331     C                   MOVEL     'N'           WMDDTI            1
525000950410     C                   MOVEL     'N'           WFBS
525100941122     C                   Z-ADD     DATCON        ARBDCM
525200941122     C                   Z-ADD     ORACON        ARBHMC
525300941122     C                   Z-ADD     V02STP        ARBSTP
525400941129     C                   Z-ADD     HARAAS        KAAS1
525500941129     C                   Z-ADD     HARLNP        KLNP1
525600941129     C                   Z-ADD     HARNRS        KNRS1
525700941129     C                   Z-ADD     HARNSP        KNSP1
525800090622      *cerca ed eventualmente elimina eventi particolari immessi in automatico con
525900090622      * data o ora maggiori rispetto alla consegna
526000090622?    C                   EXSR      DelEvePar
526100970304      *---------------------------------------------------------*
526200970304     C*  Aggiorno i Segnacolli: se non ho aperto pratica IDD
526300970304     C*                         leggo tutti i segnacolli in
526400970304     C*                         base al numero spedizione e
526500970304     C*                         li aggiorno.
526600970805     C     V2CIDD        IFEQ      ' '
526700980722     C                   ADD       1             ARBNTC
526800970620     C     KART1         CHAIN     FNART01L                           72
526900970620     C     *IN72         IFEQ      '1'
527000021107     C********           Z-ADD     ARBPKF        SAVPKG
527100970620     C                   Z-ADD     ARBNCL        SAVNCL
527200021107     C* Calcolo Volume
527300970620     C                   EXSR      CALVOL
527400970620     C                   Z-ADD     WVOL          SAVVOL
527500021107     C* Calcolo peso
527600021107     C                   exsr      calPES
527700021107     C                   Z-ADD     WPes          SAVPKG
527800970620     C                   END
527900970620     C     *IN72         DOWEQ     '0'
528000941129     C     ARTDCM        IFEQ      0
528100941122     C                   Z-ADD     DATCON        ARTDCM
528200950214     C                   MOVEL     *BLANKS       ARTDAB
528300941220     C     ARBDCP        IFNE      0
528400961113     C                   MOVEL     ' '           ARTFTR
528500941220     C                   END
528600941122     C* Se manca data arrivo imposto = data consegna
528700941122     C     ARTDAM        IFEQ      0
528800970620     C     V02CCA        ANDNE     '5'
528900941122     C                   Z-ADD     DATCON        ARTDAM
529000941122     C                   Z-ADD     85            ARTNAP
529100950329     C                   MOVEL     'S'           WMDDTI
529200941122     C                   END
529300961219     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
529400961219     C* ARTDTR solo x estero
529500961219     C     ARTDTR        IFEQ      0
529600961219     C     V02CCA        ANDEQ     '5'
529700970114     C     ARBLNA        ANDNE     ARBLNP
529800020717     C**** ORGFL1        ANDEQ     'E'
529900020717     C     §OgNTW        ifeq      'FED'
530000020717     C     §OgNTW        oreq      'EEX'
530100020717     C     §OgNTW        oreq      'EUP'
530200961219     C                   Z-ADD     DATCON        ARTDTR
530300020717     c                   end
530400961219     C                   END
530500941123     C                   UPDATE    FNART000
530600941122     C* Sistemo eventuali anomalie del collo consegnato
530700941122?    C                   EXSR      GESANM
530800941123     C                   END
530900941122     C     KART1         READE     FNART01L                               72
531000941122     C                   END
531100970304     C*---------------------------------------------------------*
531200970304     C*                       - se ho aperto pratica IDD leggo
531300970304     C*                         subfile segnacolli e mi posiz.
531400970304     C*                         x nr.spedizione/nr.segnacollo
531500970304     C*                         e li aggiorno.
531600970805     C                   ELSE
531700970805     C                   EXSR      CHNART
531800970805     C                   END
531900970304     C*---------------------------------------------------------*
532000120814     C* Scrive record per segnalare bolla con obbligo restituzione BAM o particolarità simile
532100960116     C     V2C1BF        IFNE      *BLANKS
532200120814      * imposta se c'è linea partenza bolla mamma altrimenti linea partenza bolla
532300120814      * per indicare dove deve essere restituita la "bolla firmata"
532400120814     c                   clear                   darbflrx
532500120814     c                   if        vh2lpp <> 0
532600120814     c                   move      vh2lpp        arblnpx
532700120814     c                   else
532800120814     c                   move      v2clnp        arblnpx
532900120814     c                   endif
533000120814     c                   movel     darbflrx      arbflr
533100960116     C                   Z-ADD     D53FGS        ARBFGS
533200960116     C                   Z-ADD     0             ARBDST
533300960116     C                   WRITE     FNARBX00
533400960116     C                   END
533500961219     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
533600961219     C* ARTDTR solo x estero
533700961219     C     ARBDT3        IFEQ      0
533800961219     C     V02CCA        ANDEQ     '5'
533900970114     C     ARBLNA        ANDNE     ARBLNP
534000020717     C**** ORGFL1        ANDEQ     'E'
534100020717     C     §OgNTW        ifeq      'FED'
534200020717     C     §OgNTW        oreq      'EEX'
534300020717     C     §OgNTW        oreq      'EUP'
534400060203     C     §OgNTW        oreq      'DPD'
534500961219     C                   MOVEL     'S'           WAGDT3
534600961219     C                   Z-ADD     DATCON        ARBDT3
534700020717     C                   End
534800961219     C                   END
534900970227     C*  Se data consegna della bolla è diversa dalla da quella
535000970227     C*  indicata in distinta richiamo pgm x scrittura FNSTA
535100970227     C     DATCON        IFNE      CONDIS
535200970227     C                   MOVEL     'S'           WRISTA            1
535300970227     C                   MOVEL     *BLANKS       SA2TLA            1
535400970227     C                   MOVEL     'C'           SA2TRC            1
535500970227     C                   Z-ADD     DATCON        SA2DTS            8 0
535600970227     C                   Z-ADD     D53LNA        SA2LNA            3 0
535700970227     C                   CALL      'FNLSA2R3'
535800970227     C                   PARM                    SA2TLA            1
535900970227     C                   PARM                    SA2TRC            1
536000970227     C                   PARM                    SA2DTS            8 0
536100970227     C                   PARM                    SA2LNA            3 0
536200970227     C*  altrimenti aggiorno contatore bolle
536300970227     C                   ELSE
536400970227     C                   ADD       1             WNBOL
536500970227     C                   END
536600070319     c*elimina eventuali blocchi dalla bolla
536700070319     c*solo se in organigramma è abilitato il flag per la gestione
536800070319     c                   if        §oggeot = 'S'
536900070319     c                   exsr      elibloc
537000070319     c                   endif
537100110328      * scrive spia file fiilv00f solo per distinte PDA
537200110207     c                   if        wpda  = 'S'
537300110328     c                   clear                   wnocons
537400110207     c                   exsr      scrivispia
537500110207     c                   endif
537600070319     c
537700941122     C*
537800941122     C                   ENDSR
537900970304      *---------------------------------------------------------*
538000970304      *  Pratica IDD
538100970304      *---------------------------------------------------------*
538200970304     C     CHNART        BEGSR
538300970304     C*
538400021107     C*****              Z-ADD     ARBPKF        SAVPKG
538500970304     C                   Z-ADD     ARBNCL        SAVNCL
538600021107     C* Calcolo Volume
538700970304     C                   EXSR      CALVOL
538800970304     C                   Z-ADD     WVOL          SAVVOL
538900021107     C* Calcolo peso
539000021107     C                   exsr      calPES
539100021107     C                   Z-ADD     WPes          SAVPKG
539200970304     C* Leggo subfile segnacolli e aggiorno dett.colli
539300970304     C                   Z-ADD     V2CPDS        NRR7
539400970304     C     NRR7          CHAIN     LR53S07                            95
539500970304     C     *IN95         DOWEQ     '0'
539600970304     C     SFHAAS        ANDEQ     HARAAS
539700970304     C     SFHLNP        ANDEQ     HARLNP
539800970304     C     SFHNRS        ANDEQ     HARNRS
539900970304     C     SFHNSP        ANDEQ     HARNSP
540000970304     C                   Z-ADD     SFHAAS        KAAS1
540100970304     C                   Z-ADD     SFHLNP        KLNP1
540200970304     C                   Z-ADD     SFHNRS        KNRS1
540300970304     C                   Z-ADD     SFHNSP        KNSP1
540400970304     C                   Z-ADD     SFHSCN        KNSC
540500970304     C     KART2         CHAIN     FNART01L                           72
540600970311     C*  Aggiorno dati consegna se tutti consegnati o colli chiusi con IDD
540700970311     C     V2CIDD        IFEQ      'T'
540800970311     C     SFHSEL        OREQ      '1'
540900970311     C     ARTDCM        IFEQ      0
541000970304     C                   Z-ADD     DATCON        ARTDCM
541100970304     C                   MOVEL     *BLANKS       ARTDAB
541200970304     C     ARBDCP        IFNE      0
541300970304     C                   MOVEL     ' '           ARTFTR
541400970304     C                   END
541500970304     C* Se manca data arrivo imposto = data consegna
541600970304     C     ARTDAM        IFEQ      0
541700970320     C     V2CIDD        ANDEQ     'T'
541800970320     C     SFHSEL        ANDNE     '1'
541900970304     C                   Z-ADD     DATCON        ARTDAM
542000970304     C                   Z-ADD     85            ARTNAP
542100970304     C                   MOVEL     'S'           WMDDTI
542200970304     C                   END
542300970304     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
542400970304     C* ARTDTR solo x estero
542500970304     C     ARTDTR        IFEQ      0
542600970304     C     ARBLNA        ANDNE     ARBLNP
542700020717     C**** ORGFL1        ANDEQ     'E'
542800020717     C     §OgNTW        ifeq      'FED'
542900020717     C     §OgNTW        oreq      'EEX'
543000020717     C     §OgNTW        oreq      'EUP'
543100970304     C                   Z-ADD     DATCON        ARTDTR
543200020717     C                   End
543300970304     C                   END
543400970304     C* Se selezionato imposto 5 in artfl1
543500970304     C     SFHSEL        IFEQ      '1'
543600970304     C                   MOVEL     '5'           ARTFL1
543700970305     C                   MOVEL     ' '           ARTFTR
543800970311     C                   END
543900970311     C*
544000970304     C                   UPDATE    FNART000
544100970304     C* Sistemo eventuali anomalie del collo consegnato
544200970304?    C                   EXSR      GESANM
544300970304     C                   END
544400970311     C*
544500970311     C                   END
544600970304     C* Leggo record subfile successivo
544700970304     C                   ADD       1             NRR7
544800970304     C     NRR7          CHAIN     LR53S07                            95
544900970304     C                   END
545000970311     C*
545100970311     C*  Chain tab. eventi
545200970311     C                   Z-ADD     1             X
545300970311     C     V2CEVD        LOOKUP    C2A(X)                                 95
545400970311     C                   MOVEL     D2A(X)        DS2A
545500070406     C                   MOVEL     D2z(X)        DS2z
545600970311     C*
545700970311     C* Se c'è 'S' in tentata consegna vado addizionare 1
545800970311     C     §2ANTC        IFEQ      'S'
545900970311     C                   ADD       1             ARBNTC
546000970311     C                   END
546100970311     C* Se evento da memorizzare lo memorizzo
546200970311     C     §2AEVB        IFEQ      'S'
546300970311     C                   MOVEL     *BLANKS       EVBCEV
546400970311     C                   MOVEL     V2CEVD        EVBCEV
546500970311?    C                   EXSR      EVENTI
546600970311     C                   END
546700970311     C* Incremento contatotre consegne parziali
546800970311     C                   ADD       1             STPTPA
546900970311     C*
547000970311     C     V2CIDD        IFEQ      'P'
547100970311     C                   MOVE      §2AFTC        ARBTMC
547200970311     C                   Z-ADD     0             ARBNGD
547300970311     C                   Z-ADD     0             ARBDCM
547400970311     C                   Z-ADD     0             ARBHMC
547500970311     C                   Z-ADD     0             ARBSTP
547600970311     C                   Z-ADD     0             ARBNSS
547700970311     C* Se prima consegna parziale aggiorno la data
547800970311     C     ARBDCP        IFEQ      0
547900970311     C                   Z-ADD     DATCON        ARBDCP
548000970311     C                   END
548100970311     C*  Se non fatta chiudo l'anomalia con causale 'CH'
548200970311     C                   EXSR      CLOANM
548300970311     C                   END
548400970304     C*
548500970304     C                   ENDSR
548600941206      *---------------------------------------------------------*
548700941206      *  Determinazione del volume da utilizzare
548800941206      *---------------------------------------------------------*
548900941206     C     CALVOL        BEGSR
549000941206     C*  determino volume da utilizzare
549100941206     C     ARBFVF        IFEQ      'T'
549200941206     C                   Z-ADD     ARBVLF        WVOL
549300941206     C                   ELSE
549400050208     C     ARBNCR        ifeq      ARBNCL
549500941206     C                   Z-ADD     ARBVLC        WVOL
549600941206     C                   ELSE
549700941206     C     ARBVLC        IFGE      ARBVLF
549800941206     C                   Z-ADD     ARBVLC        WVOL
549900941206     C                   ELSE
550000941206     C                   Z-ADD     ARBVLF        WVOL
550100941206     C                   ENDIF
550200941206     C                   ENDIF
550300941206     C                   ENDIF
550400941206     C*
550500941206     C                   ENDSR
550600070319      *---------------------------------------------------------*
550700070319      *  Determinazione se bolla da bloccare alla consegna
550800070319      *---------------------------------------------------------*
550900070319     c     insbloc       begsr
551000070319      * con C.A. aperta
551100070319     c                   clear                   fidn12ds
551200070319     c                   eval      i12aas = arbaas
551300070319     c                   eval      i12lnp = arblnp
551400070319     c                   eval      i12nrs = arbnrs
551500070319     c                   eval      i12nsp = arbnsp
551600070319     c                   eval      i12fel = 'E'
551700070319     c                   eval      i12fan = 'E'
551800070319     c                   eval      i12fch = 'E'
551900070319     c                   call      'FIDN12R'
552000070319     c                   parm                    fidn12ds
552100070319      * se c'è almeno una C.A. aperta blocco la bolla
552200070406     c                   if        o12nca > 0  or §2zsos = 'S'
552300070319     C                   move      'L'           KTRC
552400070319     c                   clear                   dsbl4l
552500070319     C     Kar4          chain     fiar401l                           94
552600070319     c  n94              movel(p)  ar4not        dsbl4l
552700070319     c                   move      'S'           §b4lbge
552800070319     c                   movel     dsbl4l        ar4not
552900070319     C                   if        *in94
553000070319     C                   move      'L'           ar4trc
553100070319     C                   z-add     klnp          ar4LNP
553200070319     C                   z-add     kaas          ar4AAS
553300070319     C                   z-add     knrs          ar4NRS
553400070319     C                   z-add     knsp          ar4NSP
553500070319     C                   write     fiar4000
553600070319     C                   else
553700070319     C                   update    fiar4000
553800070319     C                   end
553900070319     C*
554000070319     c                   end
554100070319     c
554200070319     C                   endsr
554300070319      *---------------------------------------------------------*
554400070319      *  elimina blocchi alla consegna da bolla data x consegnata
554500070319      *---------------------------------------------------------*
554600070319     c     elibloc       begsr
554700070321     C                   clear                   dsbl4l
554800070319     C                   move      'L'           KTRC
554900070320     C     Kar4          chain     fiar401l
555000070320     C                   if        %found(fiar401l)
555100070321     C                   movel     ar4not        dsbl4l
555200070321     c                   move      *blank        §b4lbas
555300070321     c                   move      *blank        §b4lbge
555400080131     c*                  move      *blank        §b4ltca
555500070321     c                   move      *blank        §b4ltfd
555600070321     C                   movel     dsbl4l        ar4not
555700070321     c                   update    fiar4000
555800070320     C                   endif
555900070319     c
556000070319     C                   endsr
556100130722      *---------------------------------------------------------*
556200130722      *  Verifica se presente record con coordinate GEO
556300130722      *---------------------------------------------------------*
556400130722     c     VerCorAR5     begsr
556500130723     C                   clear                   dar5cog
556600130722     C                   move      'COG'         trek5
556700130722      * per non consegna oppure consegna senza PDA verifica presenza record AR5-COG
556800130722      * se esiste e ancora da trasmettere a sede cancella
556900130722      * altrimenti aggiorna con tutti blank nelle coordinate
557000130722     C     Kar5          chain     fiar501l
557100130722     C                   if        %found(fiar501l)
557200130722     C                   if        ar5ft2 = *blank
557300130722     c                   delete    fiar5000
557400130722     c                   else
557500140404     C                   movel     ar5uni        dar5cog
557600140404     c                   move      *blank        ar5ft2
557700140404     c                   z-add     0             ar5dt2
557800140404     c                   move      *blank        §ar5xlo
557900140404     c                   move      *blank        §ar5yla
558000140404     C                   movel(p)  dar5cog       ar5uni
558100130722     c                   update    fiar5000
558200130722     c                   endif
558300130722     C                   endif
558400130722     c
558500130722     C                   endsr
558600130722      *---------------------------------------------------------*
558700140609      * verifico flag consegna documento
558800130722      *---------------------------------------------------------*
558900140609     c     srdocum       begsr
559000140609     c                   if        §pctritdoc = 'N'
559100140609     c     kpdc          chain     fipdc03t
559200140609     c                   if        %found(fipdc03t)
559300141021      *
559400141021     c                   if        pdcdocum = *blank
559500141021     c                   clear                   §pctritdoc
559600141021     c                   leavesr
559700141021     c                   endif
559800141021      *
559900140609     c                   movel(p)  pdcdocum      kke1
560000140609     c                   eval      KCOD1 =  'DOC'
560100140609     c     ktbe          chain     tntbe01l
560200140609     c                   if        %found(tntbe01l)
560300140610     c                   movel     tbeuni        ddoc
560400140609     c* se fotocopia c/ass o fattura non scrivo RA in caso di non consegna
560500140609     c* documento
560600140610     c                   if        comflg = ' S  S ' or
560700140610     c                             comflg = '    S ' or
560800140609     c                             comflg = ' S    '
560900140609     c                   clear                   §pctritdoc
561000140609     c                   end
561100140609     c                   end
561200140609     c                   end
561300140609     c                   end
561400140609     C                   endsr
561500140609      *---------------------------------------------------------*
561600140609      *  aggiorna scrive record con coordinate GEO
561700140609      *---------------------------------------------------------*
561800140609     c     AggCorAR5     begsr
561900130722     C                   clear                   dar5cog
562000130722     C                   move      'COG'         trek5
562100130722      * per non consegna oppure consegna senza PDA verifica presenza record AR5-COG
562200130722      * se esiste e ancora da trasmettere a sede cancella
562300130722      * altrimenti aggiorna con tutti blank nelle coordinate
562400130722     C     Kar5          chain     fiar501l
562500130722     C                   if        %found(fiar501l)
562600140404     C                   move      ar5uni        dar5cog
562700140404     c                   clear                   ar5ft2
562800140404     c                   clear                   ar5dt2
562900140415     c                   if        §pctxlo <> *blank and §pctxlo <> '00000000'
563000140404     c                   move      §PCTXLO       §ar5XLO
563100140404     c                   move      §PCTYLA       §ar5YLA
563200140415     c                   else
563300140415     c                   clear                   §ar5xlo
563400140415     c                   clear                   §ar5yla
563500140415     c                   endif
563600140404     c                   move      §PCTritdoc    §ar5docd
563700140404     C                   movel(p)  dar5cog       ar5uni
563800140404     c                   update    fiar5000
563900140415      *
564000130722     c                   else
564100140415      *
564200140415      *se non ci sono ne coordinate ne flag documento esce non scrive il fiar5
564300140415     c                   if        §pctritdoc = *blank and
564400140415     c                             (§pctxlo = *blank or §pctxlo = '00000000')
564500140415     c                   leavesr
564600140415     c                   else
564700140404     c                   clear                   dar5cog
564800130722     c                   clear                   ar5prg
564900140415      *se coordinate = 0 le abblenca
565000140415     c                   if        §pctxlo = '00000000'
565100140415     c                   clear                   §ar5xlo
565200140415     c                   clear                   §ar5yla
565300140415     c                   else
565400130723     c                   move      §PCTXLO       §ar5XLO
565500130723     c                   move      §PCTYLA       §ar5YLA
565600140415     c                   endif
565700140225     c                   move      §PCTritdoc    §ar5docd
565800130723     C                   move      'COG'         ar5trd
565900130730     C                   movel(p)  dar5cog       ar5uni
566000130722     c                   eval      ar5ft1 = 'T'
566100130722     c                   eval      ar5dt1 = 99999999
566200130722     c                   eval      ar5ft3 = 'T'
566300130722     c                   eval      ar5dt3 = 99999999
566400130722     c                   clear                   ar5ft2
566500130722     c                   clear                   ar5dt2
566600130730     c                   move      oggi          ar5dac
566700130722     C                   TIME                    ar5ORc
566800130722     c                   eval      ar5aas = V2CAAS
566900130722     c                   eval      ar5lnp = V2CLNP
567000130722     c                   eval      ar5nrs = V2CNRS
567100130722     c                   eval      ar5nsp = V2CNSP
567200130722     c                   write     fiar5000
567300140415     C                   endif
567400130722     C                   endif
567500140404     c* in uscita dalla routine verifico se il flag per la consegna documenti è = 'N'
567600140404      * richiamo routne per scrivere richiesta assistenza
567700140404     c                   if        §ar5docd  = 'N'
567800140404     c                   clear                   fidna6ds
567900140404     c                   eval      ida06mad = ' 88'
568000140404     c                   eval      ida06tor = 'S'
568100140404     c                   eval      ida06ogg = %editc(ar5lnp:'X') +
568200140404     c                             %editc(ar5nrs:'X') + %editc(ar5nsp:'X') +
568300140404     c                             %editc(ar5aas:'X')
568400140404     c                   eval      ida06ute = uteute
568500140404     c                   eval      ida06fil = utefil
568600140404     c                   eval      ida06cdt = '999'
568700140404     c                   eval      ida06no2 = 'AUT: ' + %editc(vh2pdr:'X') +
568800140407     c                             ' DIST: ' + %editc(vnmdoc:'X')
568900140410     c                   eval      ida06rsc = 'Automatica da PDA'
569000150610     c                   eval      IDA06PRT = '0'
569100150610     c                   eval      IDA06RPR = 'RAC'
569200150610     c                   eval      IDA06RPG = 'RAC'
569300140404     c                   call      'FIDNA6R'
569400140404     c                   parm                    fidna6ds
569500140404     c                   endif
569600130722     c
569700130722     C                   endsr
569800140306      *---------------------------------------------------------*
569900140306      *  aggiorna record GEN di fiar5 con spedizione su pda
570000140306      *---------------------------------------------------------*
570100140306     c     AggGenAR5     begsr
570200140306     C                   move      'GEN'         trek5
570300140306      * imposta campo per dichiarare se spedizione esitata con o senza pda
570400140306     C     Kar5          chain     fiar501l
570500140306     C                   if        %found(fiar501l)
570600140324     C                   movel     ar5uni        dar5gen
570700140326     c                   move      'NO'          wAR5PDACO
570800140805     c                   if        wpda = 'S'
570900140326     c                   move      *blank        WAR5PDACO
571000140306     c                   endif
571100140326      * aggiorno solo se variato
571200140326     c                   if        war5pdaco <> §ar5pdaco
571300140326     c                   move      war5pdaco     §ar5pdaco
571400140306     C                   movel(p)  dar5gen       ar5uni
571500140324     c                   clear                   ar5ft2
571600140324     c                   clear                   ar5dt2
571700140306     c                   update    fiar5000
571800140326     C                   endif
571900140306     C                   endif
572000140306     c
572100140306     C                   endsr
572200110207      *---------------------------------------------------------*
572300110207      *  scrive file spia per immagini ldv
572400110207      *---------------------------------------------------------*
572500110207     c     scrivispia    begsr
572600141202      **
572700141202ab   c                   exsr      Found_CET
572800141202ab    ***
572900141202ab   c                   if        ecco_il_CET = 'S'
573000110328     c*                  if        §PCTLDVFIR = *blank and v2sda = 'L'
573100110207     C                   clear                   fiilv000
573200110221     C                   z-add     d53fgs        ilvfgs
573300110208     C                   z-add     vnmdoc        ilvndc
573400110208     C                   z-add     v2caas        ilvaas
573500110208     C                   z-add     v2clnp        ilvlnp
573600110208     C                   z-add     v2cnrs        ilvnrs
573700110208     C                   z-add     v2cnsp        ilvnsp
573800110214     C*                  time                    ilvtimd
573900110214     C                   TIME                    Wilvtimd         14 0
574000110214     C                   MOVEl     Wilvtimd      wilvtimh          6 0
574100110214     C                   MOVE      Wilvtimh      ilvtimd
574200110214     C                   MOVE      Wilvtimd      G02DAT
574300110214     C                   MOVEL     *BLANK        G02ERR
574400110214     C                   CALL      'XSRDA8'
574500110214     C                   PARM                    WLBDA8
574600110214     C                   movel     G02INV        ilvtimd
574700110207     C                   movel     v02ris        ilvriserv
574800110207     C                   movel     v2cnt1        ilvnotaut
574900110328     C                   move      v2sda         ilvscan
575000110328     c                   move      wnocons       ilvnocdst
575100110328     C                   move      §PCTldvfir    ilvfirpda
575200110207     C                   movel     §PCTFLGSPC    ilvflgspc
575300110207     c                   move      '00'          ilvcol
575400120528     c                   eval      dilvlibero = ilvlibero
575500120528     C                   move      ORACON        §ilvhmp
575600120528     c                   eval      ilvlibero = dilvlibero
575700110207     c                   write     fiilv000
575800110328     C*                  endif
575900110207     C                   endif
576000110207     c
576100110207     C                   endsr
576200120302      *---------------------------------------------------------*
576300120302      *  richiamo scrittura file multispedizione multiassegno   *
576400120302      *---------------------------------------------------------*
576500120302     C     scrivisped    BEGSR
576600120302     c                   clear                   fncss000
576700120305     c     kspevid       chain     fncss01l
576800120518     c                   if        %found(fncss01l)
576900120518     c     nrrcss        chain     fncss00f
577000120518     c                   end
577100130118     c*                  eval      cssfgs = v2clna
577200130118     c                   if        d53fgs = 0
577300130118     c                   z-add     d53lna        cssfgs
577400130118     c                   else
577500130118     c                   z-add     d53fgs        cssfgs
577600130118     c                   end
577700120302     c                   eval      cssndc = vnmdoc
577800120305     c                   movel(p)  v3copz        cssopz
577900120305     c                   clear                   csskey
578000120302     c                   eval      cssvca = v3cvca
578100120302     c                   eval      cssaas = v3caas
578200120302     c                   eval      csslnp = v3clnp
578300120302     c                   eval      cssnrs = v3cnrs
578400120302     c                   eval      cssnsp = v3cnsp
578500120306     C                   IF        VH2LPP <> 0
578600120302     c                   eval      cssaao = vh2aap
578700120302     c                   eval      csslno = vh2lpp
578800120302     c                   eval      cssnro = vh2nrp
578900120302     c                   eval      cssnso = vh2nsp
579000120306     C                   ELSE
579100120306     c                   eval      cssaao = v3caas
579200120306     c                   eval      csslno = v3clnp
579300120306     c                   eval      cssnro = v3cnrs
579400120306     c                   eval      cssnso = v3cnsp
579500120306     C                   END
579600120302     c                   eval      csstic = v3ctic
579700120302     c                   if        v02asp = 0
579800120302     c                   eval      csscas = v3cimp
579900120302     c                   else
580000120302     c                   eval      csscas = v02asp
580100120302     c                   end
580200120302     c                   eval      cssccm = §clitm
580300120302     c                   eval      csspru = knmus
580400120518     c                   eval      cssmit = v3cmit
580500120518     c                   if        v3cmit = ' '
580600120518     c                   eval      cssmit = vh2rsm
580700120518     c                   end
580800120302     c                   time                    cssdatora
580900120518     c                   if        not %found(fncss01l)
581000120302     c                   write     fncss000
581100120518     c                   else
581200120518     c                   update    fncss000
581300120305     c                   end
581400120302     C*
581500120302     C                   ENDSR
581600120228      *---------------------------------------------------------*
581700120228      *  Richiamo pgm per gestione multiassegno multispedizione
581800120228      *---------------------------------------------------------*
581900120308     c     srinsert      begsr
582000120228     c                   clear                   exopz
582100120228     c                   clear                   tnsc30ds
582200120307     c* inserimento assegni multipli
582300120312     c                   if        d53fgs = 0
582400120312     c                   z-add     d53lna        $cssfgs
582500120312     c                   else
582600120312     c                   z-add     d53fgs        $cssfgs
582700120312     c                   end
582800120615     c                   z-add     ndc(i)        NDCI              7 0
582900120312     c     kdst41        setll     fncss04l
583000120228     c                   do        *hival
583100120312     c     kdst4         reade     fncss04l
583200120312     c                   if        %eof(fncss04l)
583300120228     c                   leave
583400120228     c                   endif
583500120312     c                   if        $cssopz <> exopz  and
583600120312     c                             $csskey = ' '
583700120307     c                   eval      sc30cmti = 'F'
583800120312     c                   eval      sc30FGSI = $cssfgs
583900120312     c                   eval      sc30NDCI = $cssndc
584000120312     c                   eval      sc30OPZI = $cssopz
584100120305     c                   eval      SC30TPCI = 'I'
584200120228     c                   eval      kpjbu  = tnsc30ds
584300120228     c                   call      'TNSC30R'
584400120305     c                   parm                    kpjba
584500120305     c                   eval      tnsc30ds = kpjbu
584600120307     c* per errore o nessun aggiornamento (f12) cancello i record
584700120307     c* per ricominciare
584800120305     c                   if        sc30msgo <> ' '
584900120306     c                             or sc30erro = 'L'
585000120308     c                   clear                   tnsc30ds
585100120308     c                   eval      sc30cmti = 'F'
585200120312     c                   eval      sc30AASI = $cssaao
585300120312     c                   eval      sc30LNPI = $csslno
585400120312     c                   eval      sc30NRSI = $cssnro
585500120312     c                   eval      sc30NSPI = $cssnso
585600120312     c                   eval      sc30AANI = $cssaas
585700120312     c                   eval      sc30LPNI = $csslnp
585800120312     c                   eval      sc30NRNI = $cssnrs
585900120312     c                   eval      sc30NSNI = $cssnsp
586000120308     c                   eval      SC30TPCI = 'A'
586100120308     c                   eval      kpjbu  = tnsc30ds
586200120308     c                   call      'TNSC30R'
586300120308     c                   parm                    kpjba
586400120308     c                   eval      kpjbu = tnsc30ds
586500120308     c                   if        sc30msgo <> ' '
586600120308     C                   MOVEL     sc30msgo      $MSG
586700120308     c                   else
586800120307     C                   eval      $MSG = 'Aggiornamento non effettuato'
586900120306     c                   end
587000120308     C                   SETON                                        40  28
587100120308     c                   leavesr
587200120308     c                   end
587300120308     c* se ok
587400120312     c                   eval      exopz = $cssopz
587500120308     c                   endif
587600120313     c* aggiorno il record del sfl della spedizione inserita
587700120312     c                   if        $cssopz = exopz
587800120312     c                   eval      comaas = $cssaas
587900120312     c                   eval      comlnp = $csslnp
588000120312     c                   eval      comnrs = $cssnrs
588100120312     c                   eval      comnsp = $cssnsp
588200120308     c                   clear                   comkey
588300120308     c                   exsr      sraggsfl
588400120308     c                   end
588500120228     c                   enddo
588600120228     C                   endsr
588700120308      *---------------------------------------------------------*
588800120308      *  Richiamo pgm per gestione multiassegno multispedizione
588900120308      *---------------------------------------------------------*
589000120308     c     aggiosped     begsr
589100120308     c* richiamo il pgm per modifica se presente ancora la sped.
589200120308     c     kspevid       chain     fncss01l
589300120308     c                   if        %found(fncss01l) and
589400120308     c                             £csskey <> ' '
589500120308     c                   clear                   tnsc30ds
589600120308     c                   eval      sc30cmti = 'F'
589700120308     C                   IF        VH2LPP = 0
589800120308     c                   eval      sc30AASI = v3caas
589900120308     c                   eval      sc30LNPI = V3CLNP
590000120308     c                   eval      sc30NRSI = V3CNRS
590100120308     c                   eval      sc30NSPI = V3CNSP
590200120308     c                   else
590300120308     c                   eval      sc30AASI = VH2AAP
590400120308     c                   eval      sc30LNPI = VH2LPP
590500120308     c                   eval      sc30NRSI = VH2NRP
590600120308     c                   eval      sc30NSPI = VH2NSP
590700120308     c                   end
590800120308     c                   eval      sc30AANI = v3caas
590900120308     c                   eval      sc30LPNI = V3CLNP
591000120308     c                   eval      sc30NRNI = V3CNRS
591100120308     c                   eval      sc30NSNI = V3CNSP
591200120308     c                   eval      SC30TPCI = 'M'
591300120308     c                   eval      kpjbu  = tnsc30ds
591400120308     c                   call      'TNSC30R'
591500120308     c                   parm                    kpjba
591600120308     c                   eval      tnsc30ds = kpjbu
591700120308     c* errore nell'aggiornamento
591800120308     c                   if        sc30msgo <> ' '
591900120308     C                   SETON                                        40  28
592000120308     C                   MOVEL     sc30msgo      $MSG
592100120308     c                   leavesr
592200120308     c                   end
592300120313     c* se annullato aggiorno tutti i record del sfl con lo stesso legame
592400120313     c                   if        sc30erro = 'Q'
592500120308     c                   eval      comkey = £csskey
592600120308     c                   clear                   comaas
592700120308     c                   clear                   comlnp
592800120308     c                   eval      comnrs = *hival
592900120308     c                   clear                   comnsp
593000120308     c                   clear                   v02as1
593100120308     C                   SETON                                        28
593200120308     c                   clear                   $msg
593300120308     c                   exsr      sraggsfl
593400120313     c                   end
593500120308     c                   end
593600120308     C                   endsr
593700120308      *---------------------------------------------------------*
593800120308      *  Richiamo pgm per gestione multiassegno multispedizione
593900120308      *---------------------------------------------------------*
594000120308     c     sraggsfl      begsr
594100120308     c* aggiorno il sfl e richiamo eventuali manutenzioni
594200120308     c                   do        *hival        nr                4 0
594300120308     C     NR            CHAIN     LR53S03                            96
594400120308     c                   if        *in96
594500120308     c                   leave
594600120308     c                   end
594700120313     c* solo c/assegni
594800120313     c                   if        v3dtpi <> 'C/A'
594900120313     c                   iter
595000120313     c                   end
595100120308     c* aggiorno solo se la stessa sped.
595200120308     c                   if        (v3caas <>comaas and comaas <> 0) or
595300120308     c                             (v3clnp <>comlnp and comlnp <> 0) or
595400120308     c                             (v3cnrs <>comnrs and comnrs <> *hival) or
595500120308     c                             (v3cnsp <>comnsp and comnsp <> 0)
595600120308     c                   iter
595700120308     c                   end
595800120308     c* aggiorno solo se stesso legame
595900120308     c                   if        v3hkey  <> comkey and comkey <> *hival
596000120308     c                   iter
596100120308     c                   end
596200120313     c* aggiorno tutto il sfl dove ci sono legami perchè è stato annullato
596300120313     c* tutto con f12
596400120313     c                   if        comkey = *hival and comlnp = 0 and
596500120313     c                             v3hkey = ' '
596600120313     c                   iter
596700120313     c                   end
596800120313     c*
596900120308     C                   SETOFF                                       404142
597000120308     C                   SETOFF                                       434445
597100120308     C                   SETOFF                                       820503
597200120308     C                   SETOFF                                       4951
597300120308     c                   eval      v3hkey = sc30keyo
597400120308     c* aggiornamento eseguito
597500120308     c                   if        v3hkey <> ' '
597600120308     c                   eval      v02as1 ='9999'
597700120308     c                   else
597800120308     c                   clear                   v02as1
597900120308     c                   end
598000120308     c                   clear                   v3copz
598100120308     c     v3hkey        comp      ' '                                8787
598200120308     c                   seton                                        22
598300120308     c                   update    lr53s03
598400120308     c                   setoff                                       22
598500120308     c                   enddo
598600120308     C                   endsr
598700110207     C**************************************************************************
598800110207     C*  Routine per gestione richiamo interrogazione bolle
598900110207     C**************************************************************************
599000110207     C     richbl        begsr
599100110207      *
599200110207     c                   movel     kpjbu         §kpjbu          256
599300110207      *
599400110207     c                   z-add     v2caas        p36aas
599500110207     c                   z-add     v2clnp        p36lnp
599600110207     c                   z-add     v2cnrs        p36nrs
599700110207     c                   z-add     v2cnsp        p36nsp
599800110207     c                   movel     ' '           p36f03
599900110207     c                   movel     '1'           p36flg
600000110207     c                   movel     dslr36        kpjbu
600100110207     c                   call      'FNLR36R'
600200110207     c                   parm                    kpjba
600300110207     c                   movel     kpjbu         dslr36
600400110207      *
600500110207     c                   movel     §kpjbu        kpjbu
600600110207      *
600700110207     c                   endsr
600800021107      *---------------------------------------------------------*
600900021107      *  Determinazione del peso da utilizzare
601000021107      *---------------------------------------------------------*
601100021107     C     calPES        BEGSR
601200021107      *
601300021107      * se è considerata un'unica spedizione
601400050208     C     arbncp        ifeq      arbncl
601500021107     C                   Z-ADD     arbPKC        wpes
601600021107     C*
601700021107     C                   ELSE
601800021107     C*
601900021107     C     arbPKC        ifge      arbPKF
602000021107     C                   Z-ADD     arbPKC        wpes
602100021107     C                   else
602200021107     C                   Z-ADD     arbPKF        wpes
602300021107     C                   end
602400021107     C*
602500021107     C                   ENDIF
602600021107     C*
602700021107     C                   ENDSR
602800941122      *---------------------------------------------------------*
602900941122      *  Consegna
603000941122      *---------------------------------------------------------*
603100941123     C     CONDAT        BEGSR
603200941122     C*
603300020829     C                   Z-ADD     ARBifp        COMifp
603400020829     C                   Z-ADD     ARBNDC        COMNDC
603500031209     C                   Z-ADD     ARBpdc        COMpdc
603600941122     C                   Z-ADD     ARBDCM        COMDCM
603700990311     C                   Z-ADD     ARBDCP        COMDCP
603800941122     C                   Z-ADD     ARBHMC        COMHMC
603900941122     C*  Aggiorno dati consegna
604000951025     C                   Z-ADD     VH2DDT        COMDDC
604100101028     C*mm                MOVE      V2CCP1        ARBTC1
604200101028     C*mm                MOVE      V2CCP2        ARBTC2
604300970805     C     ARBCCA        IFNE      '5'
604400941122     C                   MOVE      V02CCA        ARBCCA
604500970805     C                   END
604600941123     C                   ADD       V02NCL        STPTCL
604700941123     C                   ADD       V02PKG        STPTPK
604800941123     C                   ADD       V02VOL        STPTVL
604900941122     C*  Gestisco codice riserva
605000131120     c                   clear                   scriviCEP
605100941122     C     V02RIS        IFNE      *BLANK
605200941122     C                   Z-ADD     1             X
605300941122     C     V02RIS        LOOKUP    C2A(X)                                 15
605400941122     C                   MOVEL     D2A(X)        DS2A
605500070406     C                   MOVEL     D2z(X)        DS2z
605600941122     C* Se prevista memorizzazione evento procedo di conseguenza
605700941122     C     §2AEVB        IFEQ      'S'
605800941122     C                   MOVEL     V02RIS        EVBCEV
605900941122?    C                   EXSR      EVENTI
606000941122     C                   END
606100131121     c                   else
606200131121      *ricerca forzata evento CEP
606300131121     c                   exsr      leggiCep
606400941122     C                   END
606500131108     c* verifica se ricevuto doppio evento CET/CEP
606600131120     c                   if        scriviCEP = 'S'
606700131108     c                   exsr      eventicep
606800131120     c                   endif
606900941129     C*  Firmatario
607000941129     C     V2CFIR        IFNE      *BLANKS
607100941129     C                   EXSR      AGGFIR
607200941129     C                   END
607300941129     C* Se non c'è data arrivo forzo data consegna
607400941129     C     ARBDAM        IFEQ      *ZEROS
607500970617     C     V02CCA        ANDNE     '5'
607600941129     C                   Z-ADD     DATCON        ARBDAM
607700950329     C                   END
607800941129     C     ARBDTI        IFEQ      *ZEROS
607900950329     C     WMDDTI        ANDEQ     'S'
608000970620     C     V02CCA        ANDNE     '5'
608100941129     C                   Z-ADD     DATCON        ARBDTI
608200950329     C                   Z-ADD     0202          ARBHTI
608300970620     C* RICHIAMO PGM PER RIELABORARE LA STATISTICA ARRIVI
608400970620     C                   CLEAR                   SA2TLA
608500970620     C                   MOVEL     'S'           WRISTA            1
608600970620     C                   MOVEL     *BLANKS       SA2TLA            1
608700970620     C                   MOVEL     'A'           SA2TRC            1
608800970620     C                   Z-ADD     DATCON        SA2DTS            8 0
608900970620     C                   CLEAR                   SA2LNA
609000970620     C                   CALL      'FNLSA2R3'
609100970620     C                   PARM                    SA2TLA            1
609200970620     C                   PARM                    SA2TRC            1
609300970620     C                   PARM                    SA2DTS            8 0
609400970620     C                   PARM                    SA2LNA            3 0
609500941129     C                   END
609600970620     C*
609700950331     C     WFBS          IFEQ      'S'
609800950410     C     VH2LPP        IFEQ      0
609900950410     C     VH2AAP        ORNE      HARAAS
610000950410     C     VH2LPP        ORNE      HARLNP
610100950410     C     VH2NRP        ORNE      HARNRS
610200950410     C     VH2NSP        ORNE      HARNSP
610300040505     C***                MOVEL     'S'           WRICH1
610400970924     C     ARBFBS        IFEQ      ' '
610500950331     C                   MOVEL     'S'           ARBFBS
610600970924     C                   ELSE
610700970924     C     ARBFBS        IFEQ      'V'
610800040505     C     ARBFBS        oreq      'P'
610900970924     C                   MOVEL     'E'           ARBFBS
611000970924     C                   END
611100970924     C                   END
611200950331     C                   END
611300950410     C                   END
611400941122     C*
611500941122     C                   ENDSR
611600941122      *---------------------------------------------------------*
611700941122      *  Aggiorno dati di incasso
611800941122      *---------------------------------------------------------*
611900941122     C     AGGINC        BEGSR
612000941122     C*
612100941122     C                   Z-ADD     V2CNRA        NRR3
612200941122     C                   MOVE      *BLANKS       WSVTIC
612300941122     C                   MOVE      *ZERO         WSVAS1
612400941122     C                   MOVE      *ZERO         WSVAB1
612500941122     C                   MOVE      *ZERO         WSVCA1
612600941122     C                   MOVE      *ZERO         WSVDT1
612700941122     C*
612800941122     C     NRR3          CHAIN     LR53S03                            95
612900941123     C     *IN95         DOWEQ     '0'                                          --- 01 ---
613000941122     C     NRR3          ANDGE     V2CNRD
613100000421     C* totale
613200991008     C     V3DTPI        IFNE      'C/A'                                        --- 02 ---
613300991007     C     V3DTPI        ANDNE     'Ass'                                        --- 02 ---
613400941213     C     V3CTIC        IFNE      *BLANKS
613500000421     C* ... in fase di aggiornamento dati il programma legge x
613600000421     C* ogni spedizione il subfile degli importi alla rovescio
613700000421     C* (parte cioè dalla riga di totale) quando leggo questa
613800000421     C* mi salvo i dati di incasso se specificati in appositi
613900000421     C* campi di work
614000941122     C                   MOVE      V3CTIC        WSVTIC
614100941122     C                   MOVE      V02AS1        WSVAS1
614200941122     C                   MOVE      V02AB1        WSVAB1
614300941122     C                   MOVE      V02CA1        WSVCA1
614400941122     C                   MOVE      V02DT1        WSVDT1
614500941122     C*  Scrittura record importi
614600941122     C                   EXSR      WRTIMP
614700941213     C                   END
614800991008     C*  Scrittura record importi parziali x totali
614900991011     C                   EXSR      AZZARI
615000941122     C*
615100941123     C                   ELSE                                                   --- 02 ---
615200000421     C* ... sto gestendo l'assegnato o il C/Asseegno
615300941122     C     V3CTIC        IFEQ      *BLANKS
615400000421     C* se non ho specificato su questa riga i dati di incasso
615500000421     C* controllo se li ho specificati a livello di totale
615600941122     C     WSVTIC        ANDNE     *BLANKS
615700000421     C* ... se si sostituisco i dati dell'assegno con quelli
615800000421     C* impostati nel totale per effettuare i ragionamenti
615900000421     C* successivi ma non scrivo dati in stampa ne su Cnass
616000941122     C                   MOVE      WSVTIC        V3CTIC
616100941122     C                   MOVE      WSVAS1        V02AS1
616200941122     C                   MOVE      WSVAB1        V02AB1
616300941122     C                   MOVE      WSVCA1        V02CA1
616400941122     C                   MOVE      WSVDT1        V02DT1
616500941122     C                   ELSE
616600000421     C* ... altrimenti se non c'è o non devo considerare la
616700120302     C* riga di totale scrivo dati in stampa e su Cnass x
616800000421     C* singola riga di C/Assegno o Assegnato
616900941122     C                   EXSR      WRTIMP
617000941122     C                   END
617100000421     C*
617200990924     C*  Verifico se devo aggiornare/scrivere archivio incassi
617300000421     C* (FIARI)
617400990924     C*  ... se impostato incasso parziale devo aggiornarlo
617500990924     C     V02ASP        IFNE      0
617600011108MM+  C     V02SN1        ANDEQ     ' '
617700991027     C     V02TPA        ORNE      ' '
617800000421     C*      oppure se non specificato x assegnati verifico se era
617900000421     C*      stato precedente effettuato un incasso parziale
618000991027     C     V3DTPI        OREQ      'Ass'
618100990924     C     ARBICA        ANDEQ     'R'
618200990924     C     ARBFIP        ANDEQ     'I'
618300000421     C* ... oppure se ho impostato riga totale scrivo sempre FIARI
618400991011     C     WPZTOT        OREQ      'SI'
618500991011     C     V3CTOT        ANDEQ     'SI'
618600991011     C* Se c'è riga totale memorizzo dati altrimenti vado in
618700991011     C* scrittura importo
618800991011     C                   EXSR      WRTARI
618900991011     C                   END
619000941122     C*  C/Assegno
619100941123     C     V3DTPI        IFEQ      'C/A'                                        --- 03 ---
619200941123     C     V02SN1        IFEQ      ' '                                          --- 04 ---
619300941122     C*  Aggiorno Flag
619400941122     C     ARBICC        IFEQ      ' '
619500941122     C                   MOVEL     'S'           ARBICC
619600941122     C                   ELSE
619700941122     C     ARBICC        IFEQ      'S'
619800941122     C                   MOVEL     'Y'           ARBICC
619900941122     C                   ELSE
620000941122     C     ARBICC        IFEQ      'R'
620100941122     C                   MOVEL     'I'           ARBICC
620200941122     C                   ELSE
620300941122     C     ARBICC        IFEQ      'I'
620400941122     C                   MOVEL     'Y'           ARBICC
620500941122     C                   END
620600941122     C                   END
620700941122     C                   END
620800941123     C                   END
620900120302     C* ???
621000941122     C                   MOVE      V02AB1        FTDAB1
621100941122     C                   MOVE      V02CA1        FTDCA1
621200941122     C                   MOVE      V02AS1        FTDAS1
621300941123     C                   MOVE      V3CTIC        FTDTIC
621400941122     C*
621500941122     C*  Chain dati C/Assegno
621600051115     C     KARB1         CHAIN     FIAR901L                           94
621700941122     C*  Se non esiste il C/Assegno sulla figlia lo cerco sulla mamma
621800941122     C     *IN94         IFEQ      '1'
621900941122     C                   Z-ADD     VH2AAP        KAAS2
622000941122     C                   Z-ADD     VH2LPP        KLNP2
622100941122     C                   Z-ADD     VH2NRP        KNRS2
622200941122     C                   Z-ADD     VH2NSP        KNSP2
622300051115     C     KARB2         CHAIN     FIAR901L                           95
622400941122     C                   END
622500941122     C     *IN95         IFEQ      '0'
622600941122     C                   MOVE      V3CTIC        AR9TIC
622700991013     C     V02ASP        IFEQ      0
622800991013     C                   Z-ADD     V3CIMP        FTDCAS
622900991013     C                   ELSE
623000991013     C                   Z-ADD     V02ASP        FTDCAS
623100991013     C                   END
623200991013     C                   MOVE      V3CVCA        FTDVCA
623300051115     C                   UPDATE    FIAR9000
623400941122     C                   END
623500941122     C*  Ritorno all'incasso
623600941122     C                   ELSE
623700941220     C                   MOVEL     'R'           ARBICC
623800941123     C                   END                                                    --- 04 ---
623900941123     C                   END                                                    --- 03 ---
624000941122     C*  Assegnato
624100941122     C     V3DTPI        IFEQ      'Ass'
624200941122     C     V02SN1        IFEQ      ' '
624300941122     C                   MOVE      V02AB1        FTDAB2
624400941122     C                   MOVE      V02CA1        FTDCA2
624500941122     C                   MOVE      V02AS1        FTDAS2
624600941123     C                   MOVE      V3CTIC        FTDMIF
624700941122     C*  Aggiorno Flag
624800941122     C     ARBICA        IFEQ      ' '
624900990913     C     ARBACA        ANDEQ     'S'
625000941206     C     V02TPA        IFNE      'I'
625100941122     C                   MOVEL     'S'           ARBICA
625200941122     C                   ELSE
625300941122     C                   MOVEL     'I'           ARBICA
625400941122     C                   END
625500941122     C                   ELSE
625600941122     C     ARBICA        IFEQ      'S'
625700941122     C                   MOVEL     'Y'           ARBICA
625800941122     C                   ELSE
625900941122     C     ARBICA        IFEQ      'R'
626000941122     C                   MOVEL     'I'           ARBICA
626100941122     C                   ELSE
626200941122     C     ARBICA        IFEQ      'I'
626300941122     C                   MOVEL     'Y'           ARBICA
626400941122     C                   END
626500941122     C                   END
626600941122     C                   END
626700941122     C                   END
626800990924     C* Se la divisa dell'assegnato non è uguale a Lit converto
626900011010     C     V3CVCA        IFNE      §GEDCR
627000990924     C                   CLEAR                   YEURDS
627100990924     C                   MOVEL     V3CVCA        YECDVI
627200990924     C                   Z-ADD     V3CIMP        YECIMI
627300990924     C     V02ASP        IFNE      0
627400990924     C                   Z-ADD     V02ASP        YECIMI
627500990924     C                   END
627600011010     C                   MOVEL     §GEDCR        YECDVO
627700990924     C                   MOVEL     'H'           YECTAR
627800990924     C                   CALL      'YEURCO'
627900990924     C                   PARM                    YEURDS
628000990924     C                   Z-ADD     YECIMO        FTDIFP
628100990924     C                   ELSE
628200990924     C                   Z-ADD     V3CIMP        FTDIFP
628300990924     C     V02ASP        IFNE      0
628400991027     C     V02TPA        ORNE      ' '
628500990924     C                   Z-ADD     V02ASP        FTDIFP
628600990924     C                   END
628700990924     C                   END
628800020725     c                   if        ftdifp <> 0
628900020725     c                   movel     v3cvca        ftddiv
629000020725     c                   else
629100020725     c                   movel     *blanks       ftddiv
629200020725     c                   end
629300941122     C                   MOVE      V3CTIC        ARBMIF
629400941122     C                   MOVE      V02TPA        ARBFIP
629500941122     C     V02TPA        IFEQ      *BLANKS
629600941207     C     V2CCON        ANDEQ     'SI'
629700941122     C                   END
629800941122     C*  Ritorno all'incasso
629900941122     C                   ELSE
630000941220     C                   MOVEL     'R'           ARBICA
630100941122     C                   END
630200941122     C                   END
630300941123     C                   END
630400941122     C*  Lettura successiva
630500941122     C                   SUB       1             NRR3
630600941122     C     NRR3          CHAIN     LR53S03                            95
630700941122     C                   END
630800941122     C*
630900941123     C     NRR2          CHAIN     LR53S02                            95
631000941122     C                   ENDSR
631100941122      *---------------------------------------------------------*
631200941122      *  SCrittura importi
631300941122      *---------------------------------------------------------*
631400941122     C     WRTIMP        BEGSR
631500941122     C*
631600000421     C* Stampa riga importi
631700941207     C     V02SN1        IFEQ      *BLANKS
631800941206     C                   Z-ADD     V3CIMP        STPIMP
631900120302     c     V02ASP        IFGT      0
632000991027     C     V02TPA        ORNE      ' '
632100941206     C                   Z-ADD     V02ASP        STPIMP
632200941206     C                   END
632300941123     C                   MOVE      V3CVCA        STPVAL
632400120314     C*                  MOVE      STPIMP        APPO3             3 0
632500120314     C*    APPO3         COMP      0                                      06
632600941122     C   OF              EXCEPT    TESTA
632700120302     c* singolo assegno
632800120302     c                   if        v3hkey = ' '
632900120302     C     V02AS1        COMP      *BLANKS                            04
633000120302     C                   MOVEL     V3CTIC        STPTIC            2
633100120302     C                   MOVE      V02AS1        STPNAS            4
633200120302     C                   MOVE      V02AB1        STPABI
633300120302     C                   MOVE      V02CA1        STPCAB
633400941122     C                   EXCEPT    DETIMP
633500120302     c                   else
633600120302     c* multiassegni
633700120302     c     v3hkey        setll     tncsm03l
633800120302     c                   do        *hival
633900120302     c     v3hkey        reade     tncsm03l
634000120302     c                   if        %eof(tncsm03l)
634100120302     c                   leave
634200120302     c                   end
634300120302     c                   seton                                        04
634400120313     c                   movel     csmdiv        stpval
634500120313     c                   z-add     csmidc        stpimp
634600120313     C                   MOVEl     CSmNRA        stpnas
634700120302     C                   MOVE      CSmABI        stpABI
634800120302     C                   MOVE      CSmCAB        stpCAB
634900120302     C                   MOVEL     CSmTPA        stptic
635000120302     C                   MOVE      CSmTPI        stptic
635100120302     C   OF              EXCEPT    TESTA
635200120302     C                   EXCEPT    DETIMP
635300120302     C                   ENDdo
635400120302     C                   END
635500120302     C                   END
635600941122     C*
635700941212     C*  Eventuale scrittura CNASS
635800941212     C                   Z-ADD     1             X
635900941212     C     V3CTIC        LOOKUP    C1A(X)                                 94
636000941212     C     *IN94         IFEQ      '1'
636100941212     C                   MOVEL     D1A(X)        DS1A
636200951113     C     V02SN1        IFEQ      ' '
636300941212     C     §1AFCA        ANDNE     'S'
636400060331     C     §1AFCA        ANDNE     'P'
636500020827     C                   Z-ADD     dutpou        ASSFLE
636600941212     C     VH2AAP        IFEQ      *ZEROS
636700951113     C     V3DTPI        OREQ      'Ass'
636800941212     C                   Z-ADD     V3CLNP        ASSLNP
636900941212     C                   Z-ADD     V3CAAS        ASSAAS
637000941212     C                   Z-ADD     V3CNRS        ASSNRS
637100941212     C                   Z-ADD     V3CNSP        ASSNSP
637200941212     C                   ELSE
637300941212     C                   Z-ADD     VH2LPP        ASSLNP
637400941212     C                   Z-ADD     VH2AAP        ASSAAS
637500941212     C                   Z-ADD     VH2NRP        ASSNRS
637600941212     C                   Z-ADD     VH2NSP        ASSNSP
637700941212     C                   END
637800941212     C*
637900941212     C                   MOVE      §1AFCA        ASSTPA
638000941212     C     §1AFMB        IFEQ      'M'
638100941212     C                   MOVE      'M'           ASSTPI
638200941212     C                   ELSE
638300941212     C                   MOVE      ' '           ASSTPI
638400941212     C                   END
638500941212     C*
638600941212     C                   MOVEL     *BLANKS       ASSTPP
638700941212     C                   MOVEL     *BLANKS       ASSPIA
638800941212     C                   MOVEL     *BLANKS       ASSNOT
638900941212     C                   Z-ADD     *ZEROS        ASSKCC
639000941212     C                   Z-ADD     *ZEROS        ASSKSC
639100941212     C                   Z-ADD     *ZEROS        ASSNDV
639200941212     C                   Z-ADD     *ZEROS        ASSDDV
639300941212     C                   MOVE      *BLANK        ASSFTR
639400941212     C                   MOVEL     V3DTPI        ASSTPD
639500991122     C     ASSTPD        IFNE      'A'
639600991122     C     ASSTPD        ANDNE     'C'
639700991122     C                   MOVEL     'T'           ASSTPD
639800991122     C                   END
639900960730     C                   Z-ADD     V3CIMP        ASSCAS
640000941212     C     V02ASP        IFGT      0
640100990924     C                   Z-ADD     V02ASP        ASSCAS
640200941212     C                   END
640300950223     C                   MOVEL     V3CVCA        ASSVCA
640400941212     C     V3CVCA        IFNE      *BLANKS
640500011010      *FORZATO A 0 X MODIFICHE EURO
640600011010     C                   Z-ADD     0             ASSCMB
640700941212     C                   END
640800120302     C* SINGOLO ASSEGNO
640900120302     C                   IF        V3HKEY = ' '
641000960725     C*  Se non devo impostare nr.assegno scrivo 333333333
641100960725     C     §1ANAS        IFEQ      'N'
641200960725     C                   MOVEL     '3333'        ASSNRA
641300960725     C                   Z-ADD     OGGI          ASSDTE
641400960725     C                   ELSE
641500960725     C                   MOVEL     *BLANKS       ASSNRA
641600960725     C                   MOVEL     V02AS1        ASSNRA
641700941222     C                   MOVE      V02DT1        MM2
641800970113     C                   MOVE      DTD(I)        ANNO1
641900000421     C* Assegno Postdatato
642000951116     C     V3CTIC        IFEQ      'TM'
642100060626     C     V3CTIC        orEQ      'TO'
642200150526     C     v3ctic        oreq      'TP'
642300151005      *
642400000421     C* slitto avanti fino a mesi
642500151005     C**** MM2           IFEQ      1
642600151005     C**** MM1           ANDEQ     10
642700151005     C**** MM2           ORLE      2
642800151005     C**** MM1           ANDEQ     11
642900151005     C**** MM2           ORLE      3
643000151005     C**** MM1           ANDEQ     12
643100151005      *
643200151005     C* ... se sono in ottobre al massimo posso accettare
643300151005     C* assegni postdatati a gennaio (ott. + 3 mesi)
643400151005     C     MM2           IFle      3
643500151005     C     MM1           ANDEQ     10
643600151005     C* ... se sono in novembre posso accettare assegni
643700151005     C* postdatati a gennaio o febbraio (nov. + 3 mesi)
643800151005     C     MM2           ORLE      5
643900151005     C     MM1           ANDEQ     11
644000151005     C* ... se sono in dicembre posso accettare assegni
644100151005     C* postdatati a gennaio, febbraio o marzo (dic. + 3 mesi)
644200151005     C     MM2           ORLE      6
644300151005     C     MM1           ANDEQ     12
644400951116     C                   ADD       1             ANNO1
644500951116     C                   END
644600151005      *
644700951116     C                   ELSE
644800000421     C* .. se no postdatato slitto di un mese avanti
644900951205     C     MM1           IFEQ      12
645000951205     C     MM2           ANDEQ     1
645100951205     C                   ADD       1             ANNO1
645200951205     C                   END
645300000421     C* .. o indietro
645400941222     C     MM1           IFEQ      1
645500941222     C     MM2           ANDEQ     12
645600950102     C                   SUB       1             ANNO1
645700941222     C                   END
645800951116     C                   END
645900151005      **
646000941212     C                   MOVEL     V02DT1        G02DAT
646100950102     C                   MOVE      ANNO1         G02DAT
646200941212     C                   MOVEL     ' '           G02ERR
646300941212     C                   CALL      'XSRDA8'
646400941212     C                   PARM                    WLBDA8
646500941212     C                   MOVE      G02INV        ASSDTE
646600960725     C                   END
646700960725     C*  Scrivo 33333 - 33333 x abi cab se non obbligatorio
646800960725     C     §1AABI        IFEQ      'N'
646900960725     C                   Z-ADD     1000          ASSABI
647000960725     C                   Z-ADD     1000          ASSCAB
647100960725     C                   ELSE
647200941212     C                   Z-ADD     V02AB1        ASSABI
647300941212     C                   Z-ADD     V02CA1        ASSCAB
647400960725     C                   END
647500120302     C* MULTIASSEGNI
647600120302     c                   else
647700120302     C                   CLEAR                   assdte
647800120302     C                   CLEAR                   assabi
647900120302     C                   CLEAR                   asscab
648000120302     c                   eval      assnra = v3hkey
648100120302     C                   END
648200941212     C                   WRITE     CNASS000
648300941212     C                   END
648400941212     C                   END
648500941212     C*
648600941122     C                   ENDSR
648700991011      *---------------------------------------------------------*
648800991011      *  Scrittura file incassi
648900991011      *---------------------------------------------------------*
649000991011     C     WRTARI        BEGSR
649100991011     C*
649200991011     C* Se sto gestendo un Assegnato che ha già avuto degli incassi
649300991011     C* parziali eseguo chain + update
649400991011     C     V3DTPI        IFEQ      'Ass'
649500991011     C* Se c'è importo totale incassato in un altra valuta converto
649600000421     C* importo assegnato nella valuta della riga di totale
649700991018     C     V3CTOT        IFEQ      'SI'
649800991011     C     WPZTOT        ANDEQ     'SI'
649900991011     C     V3CVCA        ANDNE     WVTA
650000991011     C                   CLEAR                   YEURDS
650100991011     C     V02ASP        IFEQ      0
650200991011     C                   Z-ADD     V3CIMP        YECIMI
650300991011     C                   ELSE
650400991011     C                   Z-ADD     V02ASP        YECIMI
650500991011     C                   END
650600991011     C                   MOVEL     V3CVCA        YECDVI
650700991011     C                   MOVEL     WVTA          YECDVO
650800991011     C                   MOVEL     'H'           YECTAR
650900991011     C                   CALL      'YEURCO'
651000991011     C                   PARM                    YEURDS
651100991011     C                   Z-ADD     YECIMO        V02ASP
651200991011     C                   MOVEL     WVTA          V3CVCA
651300991011     C                   END
651400991011     C                   Z-ADD     V3CAAS        KAAS
651500991011     C                   Z-ADD     V3CLNP        KLNP
651600991011     C                   Z-ADD     V3CNRS        KNRS
651700991011     C                   Z-ADD     V3CNSP        KNSP
651800991011     C                   MOVEL     'A'           KTIP
651900991011     C                   MOVEL     V3CVCA        KDIV
652000991011     C     KARI          CHAIN     FIARI01L                           44
652100991011     C* Se non ho trovato record lo azzero e imposto dati
652200991011     C* key
652300991011     C     *IN44         IFEQ      '1'
652400991011     C                   CLEAR                   FIARI000
652500991011     C                   Z-ADD     V3CAAS        ARIAAS
652600991011     C                   Z-ADD     V3CLNP        ARILNP
652700991011     C                   Z-ADD     V3CNRS        ARINRS
652800991011     C                   Z-ADD     V3CNSP        ARINSP
652900991011     C                   MOVEL     'A'           ARITIP
653000991011     C                   MOVEL     V3CVCA        ARIDIV
653100991011     C                   END
653200991011     C* Per assegnato verifico se devo memorizzare importo
653300991011     C* incassato o da incassare
653400991027     C     V02ASP        IFEQ      0
653500991027     C     V02TPA        ANDEQ     ' '
653600991011     C                   Z-ADD     V3CIMP        ARIIMP
653700991011     C                   ELSE
653800991011     C                   Z-ADD     V02ASP        ARIIMP
653900991011     C                   END
654000991011     C   44              WRITE     FIARI000
654100991011     C  N44              UPDATE    FIARI000
654200991116     C                   Z-ADD     ARIIMP        WIMA
654300991011     C* C/Assegno: sempre write
654400991011     C                   ELSE
654500991011     C* Se c'è importo totale incassato in un altra valuta calcolo
654600000421     C* C/Assegno x differenza x evitare problemi di arrotondamenti
654700000421     C* (è più semplice effettuarlo sul C/Ass. perchè è l'ultima riga
654800000421     C* del sbf degli importi che leggo --> sbf letto alla rovescia
654900000421     C* e anche perchè non devo gestire incassi parziali)
655000991018     C     V3CTOT        IFEQ      'SI'
655100991011     C     WPZTOT        ANDEQ     'SI'
655200991011     C     V3CVCA        ANDNE     WVTA
655300991011     C     WTOT          SUB       WIMA          V02ASP
655400991011     C                   MOVEL     WVTA          V3CVCA
655500991011     C                   END
655600991011     C* Per C/Assegno verifico se il C/Assegno è sulla mamma
655700991011     C* o sulla figlia
655800991011     C     VH2AAP        IFEQ      *ZEROS
655900991011     C                   Z-ADD     V3CLNP        ARILNP
656000991011     C                   Z-ADD     V3CAAS        ARIAAS
656100991011     C                   Z-ADD     V3CNRS        ARINRS
656200991011     C                   Z-ADD     V3CNSP        ARINSP
656300991011     C                   ELSE
656400991011     C                   Z-ADD     VH2LPP        ARILNP
656500991011     C                   Z-ADD     VH2AAP        ARIAAS
656600991011     C                   Z-ADD     VH2NRP        ARINRS
656700991011     C                   Z-ADD     VH2NSP        ARINSP
656800991011     C                   END
656900991011     C                   MOVEL     'C'           ARITIP
657000991011     C                   MOVEL     V3CVCA        ARIDIV
657100991118     C     V02ASP        IFGT      0
657200991011     C                   Z-ADD     V02ASP        ARIIMP
657300991118     C                   ELSE
657400991118     C                   Z-ADD     V3CIMP        ARIIMP
657500991118     C                   END
657600010412MM   C                   Z-ADD     ARIAAS        KAAS
657700010412MM   C                   Z-ADD     ARILNP        KLNP
657800010412MM   C                   Z-ADD     ARINRS        KNRS
657900010412MM   C                   Z-ADD     ARINSP        KNSP
658000010412MM   C                   MOVEL     ARITIP        KTIP
658100010412MM   C                   MOVEL     ARIDIV        KDIV
658200010412     C     KARI          CHAIN     FIARI01L                           44
658300010412     C   44              WRITE     FIARI000
658400010412     C  N44              UPDATE    FIARI000
658500991011     C                   END
658600991011     C*
658700991011     C                   ENDSR
658800991011      *---------------------------------------------------------*
658900991011      *  AZZARI: Verifico se devo srivere dati incasso x totale
659000991011      *---------------------------------------------------------*
659100991011     C     AZZARI        BEGSR
659200991011     C*
659300991011     C                   Z-ADD     0             WIMA             13 3
659400991011     C                   MOVEL     *BLANKS       WVTA              3
659500991011     C                   Z-ADD     0             WTOT
659600991011     C                   MOVEL     *BLANKS       WPZTOT            2
659700991011     C* Verifico se impostato incasso parziale su riga di totale
659800991116     C     V02SN1        IFNE      'N'
659900991011     C* Se si imposto variabile totale e valuta relativa
660000991011     C                   MOVEL     V3CVCA        WVTA
660100991011     C                   MOVEL     'SI'          WPZTOT
660200991116     C* Imposto totale
660300991116     C     V02ASP        IFNE      0
660400991116     C                   Z-ADD     V02ASP        WTOT
660500991116     C                   ELSE
660600991116     C                   Z-ADD     V3CIMP        WTOT
660700991011     C                   END
660800991116     C                   END
660900991011     C*
661000991011     C                   ENDSR
661100941130      *---------------------------------------------------------*
661200941130      *  SCrittura importi
661300941130      *---------------------------------------------------------*
661400941130     C     STPCON        BEGSR
661500941130     C*
661600941130     C                   MOVE      V02NCL        STPNCL
661700941130     C                   MOVE      V02PKG        STPPKG
661800941130     C                   MOVE      V02VOL        STPVOL
661900941130     C                   MOVE      V02CPZ        STPCPZ
662000941130     C                   MOVEL     V02RIS        STPRIS
662100941130     C*  Stampa
662200031008     c                   if        pretes = *blank
662300031008     c                   exsr      pretesta
662400031008     c                   move      'X'           pretes            1
662500031008     c                   endif
662600941130     C   OF              EXCEPT    TESTA
662700941130     C                   EXCEPT    DETCON
662800941130     C*
662900941130     C                   ENDSR
663000941122      *---------------------------------------------------------*
663100941122      *  Aggiorno dati firmatario
663200941122      *---------------------------------------------------------*
663300941122     C     AGGFIR        BEGSR
663400941122     C*
663500941122     C                   MOVE      '1'           KTRC
663600060214     C     KAR4          CHAIN     FiAR401L                           94
663700970220     C     '°':' '       XLATE     V2CFIR        AR4NOT
663800941122     C                   MOVE      *BLANKS       AR4FTR
663900941122     C                   MOVE      *ZEROS        AR4DTR
664000941122     C     *IN94         IFEQ      '1'
664100941122     C                   MOVE      '1'           AR4TRC
664200941228     C                   Z-ADD     KLNP          AR4LNP
664300941228     C                   Z-ADD     KAAS          AR4AAS
664400941228     C                   Z-ADD     KNRS          AR4NRS
664500941228     C                   Z-ADD     KNSP          AR4NSP
664600060214     C                   WRITE     FiAR4000
664700941122     C                   ELSE
664800060214     C                   UPDATE    FiAR4000
664900941122     C                   END
665000941122     C*
665100941122     C                   ENDSR
665200941122      *---------------------------------------------------------*
665300941122      *  Note Consegna
665400941122      *---------------------------------------------------------*
665500941122     C     NOTCON        BEGSR
665600941122     C*
665700941130     C                   MOVE      '0'           KTRC
665800060214     C     KAR4          CHAIN     FiAR401L                           94
665900970220     C     '°':' '       XLATE     V2CNT1        AR4NOT
666000941122     C                   MOVE      *BLANKS       AR4FTR
666100941122     C                   MOVE      *ZEROS        AR4DTR
666200941130     C     *IN94         IFEQ      '1'
666300941122     C                   MOVE      '0'           AR4TRC
666400941228     C                   Z-ADD     KLNP          AR4LNP
666500941228     C                   Z-ADD     KAAS          AR4AAS
666600941228     C                   Z-ADD     KNRS          AR4NRS
666700941228     C                   Z-ADD     KNSP          AR4NSP
666800060214     C                   WRITE     FiAR4000
666900941122     C                   ELSE
667000060214     C                   UPDATE    FiAR4000
667100941122     C                   END
667200941122     C*
667300941122     C                   ENDSR
667400970121      *---------------------------------------------------------*
667500970121      *  Note MANCATA Consegna
667600970121      *---------------------------------------------------------*
667700970121     C     NOTNM1        BEGSR
667800970121     C*
667900970121     C                   MOVE      V2CTR1        KTRC
668000060214     C     KAR4          CHAIN     FiAR401L                           94
668100970220     C     '°':' '       XLATE     V2CNM1        AR4NOT
668200970121     C                   MOVE      *BLANKS       AR4FTR
668300970121     C                   MOVE      *ZEROS        AR4DTR
668400970121     C     *IN94         IFEQ      '1'
668500970121     C                   MOVE      V2CTR1        AR4TRC
668600970121     C                   Z-ADD     KLNP          AR4LNP
668700970121     C                   Z-ADD     KAAS          AR4AAS
668800970121     C                   Z-ADD     KNRS          AR4NRS
668900970121     C                   Z-ADD     KNSP          AR4NSP
669000060214     C                   WRITE     FiAR4000
669100970121     C                   ELSE
669200060214     C                   UPDATE    FiAR4000
669300970121     C                   END
669400970121     C*
669500970121     C                   ENDSR
669600970121      *---------------------------------------------------------*
669700970121      *  Note MANCATA Consegna
669800970121      *---------------------------------------------------------*
669900970121     C     NOTNM2        BEGSR
670000970121     C*
670100970121     C                   MOVE      V2CTR2        KTRC
670200060214     C     KAR4          CHAIN     FiAR401L                           94
670300970220     C     '°':' '       XLATE     V2CNM2        AR4NOT
670400970121     C                   MOVE      *BLANKS       AR4FTR
670500970121     C                   MOVE      *ZEROS        AR4DTR
670600970121     C     *IN94         IFEQ      '1'
670700970121     C                   MOVE      V2CTR2        AR4TRC
670800970121     C                   Z-ADD     KLNP          AR4LNP
670900970121     C                   Z-ADD     KAAS          AR4AAS
671000970121     C                   Z-ADD     KNRS          AR4NRS
671100970121     C                   Z-ADD     KNSP          AR4NSP
671200060214     C                   WRITE     FiAR4000
671300970121     C                   ELSE
671400060214     C                   UPDATE    FiAR4000
671500970121     C                   END
671600970121     C*
671700970121     C                   ENDSR
671800941122      *---------------------------------------------------------*
671900941122      *  Aggiorno spedizione mamma
672000941122      *---------------------------------------------------------*
672100941122     C     SPEMAM        BEGSR
672200941122     C*
672300941122     C                   Z-ADD     VH2AAP        KAAS2
672400941122     C                   Z-ADD     VH2LPP        KLNP2
672500941122     C                   Z-ADD     VH2NRP        KNRS2
672600941122     C                   Z-ADD     VH2NSP        KNSP2
672700941122     C     KARB2         CHAIN     FNARB01L                           95
672800941216     C     *IN95         IFEQ      '0'
672900941122     C     ARBDCM        IFEQ      0
673000990311     C                   Z-ADD     COMDCP        ARBDCP
673100990311     C                   Z-ADD     COMDCM        ARBDCM
673200020829     C                   Z-ADD     COMifp        ARBifp
673300031209     C                   Z-ADD     COMpdc        ARBpdc
673400020829     C                   Z-ADD     COMNDC        ARBNDC
673500941228     C                   Z-ADD     COMDDC        ARBDDC
673600941122     C                   Z-ADD     COMHMC        ARBHMC
673700941230     C                   Z-ADD     0             ARBDT1
673800941230     C                   MOVEL     *BLANKS       ARBFT1
673900941122     C                   END
674000941123     C                   MOVE      COMICC        ARBICC
674100941122     C                   MOVEL     'Y'           ARBICA
674200941122     C                   Z-ADD     *ZEROS        ARBNMI
674300020826     C                   Z-ADD     dutpou        ARBIPP
674400020826     C                   Z-ADD     dutcou        ARBCUC
674500941122     C                   MOVE      *BLANKS       ARBFBC
674600941122     C                   MOVE      'S'           ARBFDC
674700950602     C                   Z-ADD     0             ARBNSS
674800941122     C* Se non c'è data arrivo merce forzo data consegna
674900941122     C     ARBDAM        IFEQ      *ZEROS
675000941122     C                   Z-ADD     DATCON        ARBDAM
675100950329     C                   END
675200941122     C     ARBDTI        IFEQ      *ZEROS
675300950329     C     WMDDTI        ANDEQ     'S'
675400941122     C                   Z-ADD     DATCON        ARBDTI
675500950329     C                   Z-ADD     0202          ARBHTI
675600941122     C                   END
675700950331     C     WFBS          IFEQ      'S'
675800950410     C     VH2AAP        ANDEQ     HARAAS
675900950410     C     VH2LPP        ANDEQ     HARLNP
676000950410     C     VH2NRP        ANDEQ     HARNRS
676100950410     C     VH2NSP        ANDEQ     HARNSP
676200970924     C     ARBFBS        IFEQ      ' '
676300970924     C                   MOVEL     'S'           ARBFBS
676400970924     C                   ELSE
676500970924     C     ARBFBS        IFEQ      'V'
676600040505     C     ARBFBS        oreq      'P'
676700970924     C                   MOVEL     'E'           ARBFBS
676800970924     C                   END
676900970924     C                   END
677000040505     C***                MOVEL     'S'           WRICH1
677100950331     C                   END
677200961219     C* Se bolla chiusa con codice consegna anomala 5 aggiorno
677300961219     C* ARTDTR solo x estero
677400961219     C     WAGDT3        IFEQ      'S'
677500961219     C     ARBDT3        ANDEQ     0
677600961219     C                   Z-ADD     DATCON        ARBDT3
677700961219     C                   END
677800941122     C                   UPDATE    FNARB000
677900941122     C                   END
678000941122     C*
678100941122     C                   ENDSR
678200941122     C*------------------------------------------------------
678300941122     C* Aggior. record totale distinta tassazione padroncini
678400941122     C*------------------------------------------------------
678500941122     C     UPDFTT        BEGSR
678600941122     C*  Procedo all'aggiornamento archivio conteggi
678700941122     C                   MOVE      ARBPDC        KPDR
678800941122     C                   MOVE      'C'           KTSR
678900941122     C                   MOVE      NDC(I)        KNDC
679000011121     C     KFTT          CHAIN     fiftt01L                           91
679100011121     C   91              CLEAR                   fiftt000
679200941122     C                   MOVE      ARBPDC        FTTPDR
679300140225     C                   MOVE      fttflr        dftt01
679400060120     c                   if        arbifp > 0
679500060120     C                   z-add     ARBifp        fttfgs
679600060120     c                   else
679700060120     C                   z-add     d53fgs        fttfgs
679800060120     c                   endif
679900060120     c                   if        fttfgs = 0
680000060120     c                   movel     fttpdr        fttfgs
680100060120     c                   end
680200941122     C                   MOVE      'C'           FTTTSR
680300031008     C                   MOVE      ndc(i)        FTTNDC
680400031009     C                   MOVE      Dtd(I)        G02DAT
680500031009     C                   MOVEL     *BLANK        G02ERR
680600031009     C                   CALL      'XSRDA8'
680700031009     C                   PARM                    WLBDA8
680800031009     C                   Z-ADD     G02INV        fttddc
680900051222     C                   Z-ADD     oggi          fttdtr
681000941122     C                   MOVE      SET(I)        FTTSET
681100941122     C                   MOVE      SNT(I)        FTTSNT
681200020826     C                   MOVE      dutcou        FTTCUC
681300140225     C                   MOVE      dutpou        §FTTpou
681400140225     c                   movel     dftt01        fttflr
681500941122     C                   MOVE      FPP(I)        FTTFPP
681600941122     C                   MOVE      PEP(I)        FTTPEP
681700030204     C                   move      Car(I)        FTTlvl
681800941122     C*
681900941122     C*  Se in distinta non esistono spedizioni consegnate non metto
682000941122     C*  il blocco
682100941122     C                   MOVE      'B'           FTTFVL
682200941122     C     SET(I)        IFEQ      SNT(I)
682300941122     C                   MOVE      ' '           FTTFVL
682400941122     C                   END
682500941202     C*
682600011121      * In base alla data del documento aggancio la tab.GEDCN per
682700011121      * impostare la divisa della valorizzazione dei conteggi.
682800011121     c                   if        fttddc <= §geddtfv
682900011121     c                   movel     §geditl       fttdiv
683000011121     c                   else
683100011121     c                   movel     §gedeur       fttdiv
683200011121     c                   end
683300011121      *
683400011121     C   91              WRITE     fiftt000
683500011121     C  N91              UPDATE    fiftt000
683600941122     C*
683700941122     C                   ENDSR
683800941122     C*---------------------------------------------------------
683900941122     C* Aggior. record dettaglio distinta tassazione padroncini
684000941122     C*--------------------------------------------------------
684100941122     C     UPDFTD        BEGSR
684200941122     C*
684300941122     C*  Procedo all'aggiornamento archivio conteggi
684400060203     C     KFTD          CHAIN     fiftd22L                           91
684500941122     C                   MOVEL     *BLANKS       FTDATB
684600941122     C                   MOVE      ARBPDC        FTDPDR
684700041022     c                   if        arbifp > 0
684800020829     C                   z-add     ARBifp        ftdfgs
684900041022     c                   else
685000041022     C                   z-add     d53fgs        ftdfgs
685100041022     c                   endif
685200060119     c                   if        ftdfgs = 0
685300060119     c                   movel     ftdpdr        ftdfgs
685400060119     c                   end
685500941122     C                   MOVE      'C'           FTDTSR
685600941122     C                   MOVE      V2CLNP        FTDLNP
685700941122     C                   MOVE      V2CAAS        FTDAAS
685800941122     C                   MOVE      V2CNRS        FTDNRS
685900941122     C                   MOVE      V2CNSP        FTDNSP
686000941122     C                   MOVE      NDC(I)        FTDNDC
686100031009     C                   MOVE      arbddc        FTDDDC
686200941122     C                   MOVE      ARBRSD        FTDRSC
686300950111ES   C                   MOVE      ARBCAD        FTDCAP
686400950111ES   C                   MOVE      ARBFIN        FTDFIN
686500950118ES   C                   Z-ADD     1             FTDSET
686600941122     C*  Solo incasso: tipo record 50
686700941122     C     V2CCON        IFEQ      'NO'
686800941122     C                   MOVE      ' '           FTDSIC
686900941207     C                   MOVE      STPNCL        FTDNCL
687000941207     C                   MOVE      STPPKG        FTDPKL
687100941207     C                   MOVE      STPVOL        FTDVLU
687200941122     C                   ELSE
687300941122     C                   MOVE      'S'           FTDSIC
687400941122     C                   END
687500941122     C*  Mancata consegna: 'N' in riconsegna giacenza
687600941122     C     V02CCN        IFNE      *BLANKS
687700941122     C     V02CCN        ANDNE     'P  '
687800941122     C                   MOVE      'N'           FTDFGC
687900941130     C                   MOVE      §2AFTC        FTDTMC
688000941130     C                   MOVE      V02CCN        FTDCMC
688100941122     C                   ELSE
688200941122     C                   MOVE      ' '           FTDFGC
688300941122     C*  Controllo se bolla senza dettaglio segnacolli
688400941202     C     V02CPZ        IFNE      'P'
688500941122     C     SAVNCL        IFNE      0
688600941122     C     SAVPKG        ORNE      0
688700941122     C     SAVVOL        ORNE      0
688800941122     C                   MOVE      ' '           FTDFGC
688900941122     C                   MOVE      SAVNCL        FTDNCL
689000941123     C                   MOVE      SAVPKG        FTDPKL
689100941123     C                   MOVE      SAVVOL        FTDVLU
689200941122     C                   END
689300941130     C                   ELSE
689400941130     C                   MOVEL     V02CPZ        FTDCMC
689500941130     C                   END
689600941122     C                   END
689700941122     C*
689800941122     C                   MOVE      V02STP        FTDSTP
689900950602     C                   MOVE      WNSS          FTDNSS
690000941122     C*  Mancata consegna: non valorizzo data consegna
690100941122     C     V02CCN        IFEQ      *BLANKS
690200941122     C     V02CCN        OREQ      'P  '
690300941122     C                   MOVE      DATCON        FTDDCM
690400941122     C                   MOVE      ORACON        FTDHMC
690500941122     C                   ELSE
690600941122     C                   MOVE      *ZERO         FTDDCM
690700941122     C                   MOVE      *ZERO         FTDHMC
690800941122     C                   END
690900941122     C                   MOVEL     V02RIS        FTDCRC
691000941122     C*
691100011121     C   91              WRITE     fiftd000
691200011121     C  N91              UPDATE    fiftd000
691300941122     C*
691400941122     C                   ENDSR
691500970926     C*---------------------------------------------------------
691600970926     C* Aggior. record dettaglio trasmissione in sede stop
691700970926     C*--------------------------------------------------------
691800970926     C     ELASTP        BEGSR
691900000421     C*
692000970926     C     NRR2          CHAIN     LR53S02                            96
692100970926    1C     *IN96         DOWEQ     '0'
692200970926     C     VNMDOC        ANDEQ     WSVNDC
692300970926    2C     V2CCON        IFEQ      'NO'
692400970926     C     V2CDCM        ANDGT     0
692500970926     C     V02STP        ANDGT     0
692600971001     C     V2CRBL        ANDNE     'R'
692700970926    3C     V2CBSD        IFNE      'N'
692800970926     C     V2CBFI        ORNE      'N'
692900970926     C                   Z-ADD     1             WW
693000970926     C     V02STP        LOOKUP    NST(WW)                                95
693100970926     C                   Z-ADD     SPS(WW)       WSET
693200970926     C                   EXSR      UPDSTP
693300970926    3C                   END
693400970926    2C                   END
693500970926     C                   ADD       1             NRR2
693600970926     C     NRR2          CHAIN     LR53S02                            96
693700970926    1C                   ENDDO
693800970926     C*
693900970926     C                   ENDSR
694000950215     C*---------------------------------------------------------
694100950215     C* Aggior. record dettaglio trasmissione in sede stop
694200950215     C*--------------------------------------------------------
694300970926     C     CHKSTP        BEGSR
694400970926     C*
694500970926     C*  Controllo il codice bolla
694600970926     C                   CLEAR                   DS3A
694700970926     C                   Z-ADD     1             X
694800970926     C     ARBCBO        LOOKUP    C3A(X)                                 32
694900970926     C   32              MOVEL     D3A(X)        DS3A
695000971001     C*
695100971001     C                   MOVE      §3ABSD        V2CBSD
695200971001     C                   MOVE      §3ABFI        V2CBFI
695300971001     C                   MOVEL     §3ARBL        V2CRBL
695400971001     C                   Z-ADD     ARBDCM        V2CDCM
695500971007     C     NRR2          CHAIN     LR53S02                            96
695600990423     C                   MOVEL     V2IN09        *IN09
695700000522     C                   MOVEL     V2IN24        *IN24
695800971001     C                   UPDATE    LR53S02
695900971001     C*
696000970926    1C     V2CCON        IFEQ      'NO'
696100970926     C     ARBDCM        ANDGT     0
696200971001     C* Escludo recupero perchè non sono previste in C/E competenze di
696300971001     C* stop sulle spedizioni di recupero
696400971001     C     §3ARBL        ANDNE     'R'
696500970926    2C     §3ABSD        IFNE      'N'
696600970926     C     §3ABFI        ORNE      'N'
696700970926     C*
696800970926    3C     D53CMG        IFEQ      'S'
696900000615     C                   Z-ADD     99999         WSET
697000970926     C                   EXSR      UPDSTP
697100970926   X3C                   ELSE
697200970926    4C     V02STP        IFGT      *ZEROS
697300970926     C                   Z-ADD     1             WW                3 0
697400970926     C     V02STP        LOOKUP    NST(WW)                                95
697500970926     C   95              ADD       1             SPS(WW)
697600080903    5C  N95YY            IFLT      200
697700970926     C                   ADD       1             YY
697800970926     C                   Z-ADD     V02STP        NST(YY)
697900970926     C                   Z-ADD     1             SPS(YY)
698000970926    5C                   END
698100970926    4C                   END
698200970926    3C                   END
698300970926    2C                   END
698400970926    1C                   END
698500950215     C*
698600950215     C                   ENDSR
698700970926     C*---------------------------------------------------------
698800970926     C* Aggior. record dettaglio trasmissione in sede stop
698900970926     C*--------------------------------------------------------
699000970926     C     UPDSTP        BEGSR
699100970926     C*
699200970926     C                   CLEAR                   FNSTP100
699300000615     C* SE NO CONS.A MAGAZZINO E NUMERO SPED.=99999 SCRIVO 99998
699400000615     C* (99999 E'USATO SOLO PER LE CONSEGNE A MAGAZZINO)
699500971028     C     D53CMG        IFEQ      *BLANKS
699600000615     C     WSET          ANDEQ     99999
699700000615     C                   Z-ADD     99998         FTPSET
699800971028     C                   ELSE
699900970926     C                   Z-ADD     WSET          FTPSET
700000971028     C                   END
700100970926     C                   MOVE      'C'           FTPTSR
700200970926     C                   MOVE      V2CLNP        FTPLNP
700300970926     C                   MOVE      V2CAAS        FTPAAS
700400970926     C                   MOVE      V2CNRS        FTPNRS
700500970926     C                   MOVE      V2CNSP        FTPNSP
700600970926    3C                   SELECT
700700970926     C     §3ABSD        WHENEQ    *BLANKS
700800970926     C     §3ABFI        ANDEQ     *BLANKS
700900970926     C                   MOVEL     'E'           FTPINV
701000970926     C     §3ABSD        WHENEQ    *BLANKS
701100970926     C                   MOVEL     'S'           FTPINV
701200970926     C     §3ABFI        WHENEQ    *BLANKS
701300970926     C                   MOVEL     'F'           FTPINV
701400970926    3C                   ENDSL
701500970926     C                   WRITE     FNSTP100
701600970926     C*
701700970926     C                   ENDSR
701800941123     C*---------------------------------------------------------
701900941123     C*  Totali stampa
702000941123     C*--------------------------------------------------------
702100941123     C     GESTOT        BEGSR
702200941123     C*
702300941123     C*  Riporto totali in stampa
702400941123     C                   Z-ADD     V05TSC        STPTSC
702500941123     C                   Z-ADD     V05STP        STPTST
702600000505     C                   Z-ADD     V05ADR        STPADR
702700000505     C                   Z-ADD     V05NDR        STPNDR
702800941206     C     STPLI1        IFGT      51
702900941123     C                   EXCEPT    STAMPA
703000941123     C                   END
703100941123     C                   EXCEPT    TOTAL
703200941123     C*  Totale da esigere in divisa
703300991007     C     V05TCE        IFNE      *ZEROS
703400990924     C     V05TAE        ORNE      *ZEROS
703500060419     C     V05pos        ORNE      *ZEROS
703600990924     C     V05TAV        ORNE      *ZEROS
703700991007     C     V05TDE        ORNE      *ZEROS
703800991007     C                   Z-ADD     V05TCE        STPEES
703900991007     C                   ADD       V05TAE        STPEES
704000060419     C                   ADD       V05pos        STPEES
704100990924     C                   Z-ADD     V05TCE        STPECO
704200060419     C                   ADD       V05pos        STPECO
704300990924     C                   Z-ADD     V05TAE        STPEAN
704400991007     C                   Z-ADD     V05TDE        STPEAD
704500991007     C                   Z-ADD     V05TAV        STPDAD
704600990924     C                   EXCEPT    TOTEU
704700941123     C                   END
704800970227     C*  Se non è la prima volta e ci sono delle bolle con la
704900970227     C*  data consegna = a quella della distinta richiamo pgm
705000970227     C     WSVNDC        IFNE      0
705100970227     C     WNBOL         ANDNE     0
705200970227     C                   MOVEL     'S'           WRISTA            1
705300970227     C                   MOVEL     *BLANKS       SA2TLA            1
705400970227     C                   MOVEL     'C'           SA2TRC            1
705500970228     C                   Z-ADD     CONDIS        SA2DTS            8 0
705600970227     C                   Z-ADD     D53LNA        SA2LNA            3 0
705700970227     C                   CALL      'FNLSA2R3'
705800970227     C                   PARM                    SA2TLA            1
705900970227     C                   PARM                    SA2TRC            1
706000970227     C                   PARM                    SA2DTS            8 0
706100970227     C                   PARM                    SA2LNA            3 0
706200970227     C                   END
706300941123     C*
706400941123     C                   ENDSR
706500941123     C*---------------------------------------------------------
706600941123     C* MEMORIZZAZIONE EVENTI BOLLE
706700941123     C*----------------------------------------------------------
706800941123?    C     EVENTI        BEGSR
706900941123     C*
707000941123     C                   MOVEL     *BLANKS       EVBATB
707100941123     C                   Z-ADD     OGGI          EVBDTV
707200941123     C                   TIME                    EVBORV
707300020905     C*                  Z-ADD     D53LNA        EVBFLE
707400020905     C                   Z-ADD     dutpou        EVBFLE
707500020826     C                   Z-ADD     dutcou        EVBCDU
707600941123     C* Sulla bolla metto i riferimenti della mamma se esiste legame
707700941123     C     VH2AAP        IFEQ      *ZEROS
707800941123     C                   Z-ADD     V2CLNP        EVBLNP
707900941123     C                   Z-ADD     V2CAAS        EVBAAS
708000941123     C                   Z-ADD     V2CNRS        EVBNRS
708100941123     C                   Z-ADD     V2CNSP        EVBNSP
708200941123     C                   ELSE
708300941123     C                   Z-ADD     VH2LPP        EVBLNP
708400941123     C                   Z-ADD     VH2AAP        EVBAAS
708500941123     C                   Z-ADD     VH2NRP        EVBNRS
708600941123     C                   Z-ADD     VH2NSP        EVBNSP
708700941123     C                   END
708800941123     C                   Z-ADD     DATCON        EVBDEV
708900941123     C                   Z-ADD     ORACON        EVBHEV
709000941123     C                   MOVEL     *BLANKS       EVBFTR
709100941123     C                   Z-ADD     0             EVBDTR
709200131112     c                   clear                   devb01
709300131205      * imposta flag per sapere chi ha inserito l'evento  se cassiere spc o autista
709400131205      * se la distinta è pda altrimenti sempre A = cassiere
709500131205     c                   if        wpda = 'S'
709600131205     c                   if        v02ccn <> *blank and
709700131205     c                             v02ccn <> vh2ccn
709800131205     c                   move      'A'           §notagg
709900131205     c                   else
710000131205     c                   if        vh2spc = 'S'
710100131205     c                   move      'S'           §notagg
710200131205     c                   else
710300131205     c                   move      'P'           §notagg
710400131205     c                   endif
710500131205     c                   endif
710600131205     c                   else
710700131205     c                   move      'A'           §notagg
710800131205     c                   endif
710900131112     C                   move      ARBPDC        §NOTPDC
711000131112     c                   if        arbndc > 0
711100131112     C                   move      ARBNDC        §NOTNDC
711200131112     c                   else
711300131112     c                   move      warbndc       §notndc
711400131112     c                   endif
711500131121      *leggo eventuale evento CEP
711600131121     c                   move      'X'           DaEventi          1
711700131121     c                   exsr      leggiCEP
711800131121     c                   clear                   DaEventi
711900131120     c                   if        scriviCEP = 'S'
712000131120     c                   move      'U'           §notprg
712100131120     c                   else
712200131120     c                   move      *blank        §notprg
712300131120     c                   endif
712400131106     C                   MOVEL     Devb01        EVBNOT
712500990803     C                   Z-ADD     0             EVBDTC
712600941123     C*
712700990803     C                   WRITE     FNEVB000
712800070406      * verifico se codice evento richiede gestione sospensione bolla dalla
712900070406      * consegna ESEMPIO: RS riserva speciale
713000070406     c                   if        §oggeot = 'S'
713100070406     c                   if        §2zsos = 'S'
713200070406     c                   exsr      insbloc
713300070406     c                   endif
713400070406     c                   endif
713500070312      * verifico se evento proposto abilita scrittura estensione evento
713600070312     C                   Z-ADD     1             Yz                3 0
713700140114     c                   if        v2cevp <> *blank
713800070312     C     V2cevp        LOOKUP    C2A(Yz)                                95
713900140114     c                   else
714000140114     C     evbcev        LOOKUP    C2A(Yz)                                95
714100140114     c                   endif
714200070312     C     *IN95         IFEQ      '1'                                          --- 04 ---
714300070312     C                   MOVEL     D2Z(Yz)       DS2Z
714400070312     C                   endif                                                  --- 04 ---
714500140207      *scrittura estensione evento per l.Avv. solo se filiale abilitata
714600070319     c                   if        §oggeot = 'S'
714700070312     c                   if        §2zev2 = 'S'
714800070312     c                   exsr      eventi2
714900070312     c                   endif
715000070319     c                   endif
715100941123     C*
715200941123?    C                   ENDSR
715300140207     C*---------------------------------------------------------
715400140207      *scrittura record spia per invio sms/mail al destinatario in caso
715500140207      *di lasciato avviso al 1° tentativo di consegna
715600140207     C*----------------------------------------------------------
715700140207?    C     srspiasms     begsr
715800140207     c                   clear                   trtcp2ds
715900140207     c                   eval      t2chi= 'C'
716000140207     c                   z-add     arbifp        t2fgs
716100140207     c                   z-add     arbndc        t2ndc
716200140207     C                   Z-ADD     arbLNP        t2lnp
716300140207     C                   Z-ADD     arbAAS        t2aas
716400140207     C                   Z-ADD     arbNRS        t2nrs
716500140207     C                   Z-ADD     arbNSP        t2nsp
716600140207     C                   Z-ADD     DATCON        t2DEV
716700140207     C                   Z-ADD     ORACON        t2HEV
716800140207     c                   call      'TRTCP2R'
716900140207     c                   parm                    trtcp2ds
717000140207?    C                   ENDSR
717100140113     C*---------------------------------------------------------
717200140113     C* scrittura estensione EVENTI
717300140113     C*----------------------------------------------------------
717400140113?    C     eventi2       begsr
717500140113      *
717600140207     c                   clear                   fnevb2ds
717700140113     C                   move      evblnp        evb2lnp
717800140113     C                   move      evbaas        evb2aas
717900140113     C                   move      evbnrs        evb2nrs
718000140113     C                   move      evbnsp        evb2nsp
718100140113     C                   move      evbcev        evb2cev
718200140113     C                   move      evbdtv        evb2dtv
718300140113     C                   move      evborv        evb2orv
718400140113     C                   move      evbfle        evb2fle
718500140113     C                   move      evbdev        evb2DEV
718600140113     C                   move      evbhev        evb2HEV
718700140113     C                   move      §2ztel        evb2TEL
718800140113     C                   move      *blank        evb2FLR
718900140113     C                   move      knmus         evb2ute
719000140113     C* se ho la data consegna richiesta proposta da telefonata non metto
719100140113     c* il flag da telefonare
719200140113     c                   if        v2harp = '1'
719300140113     c                   clear                   evb2tel
719400140113     c                   end
719500140212     c* se non devo telefonare e se i campi del firmatario o delle note sono
719600140212      * vuoti, non scrivo il record
719700140212     c                   if        evb2tel = ' ' and
719800140212     c                             v2cfir = *blank and
719900140212     c                             v2cnt1 = *blank
720000140212     c                   leavesr
720100140212     c                   end
720200140113      * se sono con bolla consegnata i presenza di firmatario inserisco un ulteriore record
720300140113      * se è presente anche la nota
720400140113     c                   if        v2cfir <> *blank
720500140113     C                   movel     v2cfir        evb2dati
720600140113     C                   move      'FIR'         evb2trd
720700140113     C                   move      v02ris        evb2ris
720800140113     C                   WRITE     FNEVB200
720900140113     c                   if        v2cnt1 <> *blank
721000140113     C                   movel     v2cnt1        evb2dati
721100140113     C                   move      'NOT'         evb2trd
721200140113     C                   WRITE     FNEVB200
721300140113     c                   endif
721400140113     c                   else
721500140113      * lasciato avviso se presente nota inserisco tipo record
721600140113     c                   if        v2cnt1 <> *blank
721700140113     C                   move      'NOT'         evb2trd
721800140113     C                   movel     v2cnt1        evb2dati
721900140113     c                   endif
722000140113     C                   WRITE     FNEVB200
722100140113     c                   endif
722200140113      *
722300140113     c                   endsr
722400131121      *________________________________________________________________
722500131121     c     leggiCep      begsr
722600131121      *________________________________________________________________
722700131121     c** verificop se esiste Rekord CEP abbinato per segnare l'evento che sto scrivendo
722800131121      * e allo stesso tempo memorizzo se devo scrivere l'evento con il motivo del CEP
722900131121     c                   move      'CEP'         ttrd
723000131121     c     kcet          setgt     fipct02l
723100131121     c     kcet          readpe    fipct02l
723200131121     c                   if        not %eof(fipct02l)
723300131121     c                   clear                   ds2acep
723400131121     c                   movel     pctdati       fipctcetds
723500141202      ****
723600131121     c                   if        §pctcmc <> *blank
723700131121     c                   z-add     1             ww
723800131121     c     §pctcmc       lookup    c2a(ww)                                85
723900131121     c   85              movel     d2a(ww)       ds2acep
724000131121     c                   if        w§2aevb = 'S'
724100131121     c                   move      'S'           scriviCEP
724200131121      *forza riempimento campi se non sono passato già dagli eventi quindi
724300131121      * sono in situazione di evento CEP ma CET consegnato e senza riserva
724400131121     c                   if        DaEventi = *blank
724500131121     C                   MOVEL     *BLANKS       EVBATB
724600131121     C                   Z-ADD     OGGI          EVBDTV
724700131121     C                   TIME                    EVBORV
724800131121     C*                  Z-ADD     D53LNA        EVBFLE
724900131121     C                   Z-ADD     dutpou        EVBFLE
725000131121     C                   Z-ADD     dutcou        EVBCDU
725100131121     C* Sulla bolla metto i riferimenti della mamma se esiste legame
725200131121     C     VH2AAP        IFEQ      *ZEROS
725300131121     C                   Z-ADD     V2CLNP        EVBLNP
725400131121     C                   Z-ADD     V2CAAS        EVBAAS
725500131121     C                   Z-ADD     V2CNRS        EVBNRS
725600131121     C                   Z-ADD     V2CNSP        EVBNSP
725700131121     C                   ELSE
725800131121     C                   Z-ADD     VH2LPP        EVBLNP
725900131121     C                   Z-ADD     VH2AAP        EVBAAS
726000131121     C                   Z-ADD     VH2NRP        EVBNRS
726100131121     C                   Z-ADD     VH2NSP        EVBNSP
726200131121     C                   END
726300131121     c                   endif
726400131121     c                   endif
726500131121     c                   endif
726600131121     c                   endif
726700131121     c                   endsr
726800131108     C*---------------------------------------------------------
726900131108     C* scrittura EVENTI generati da doppio tentativo di consegna
727000131108     C*----------------------------------------------------------
727100131108?    C     eventicep     begsr
727200131120      *
727300131108     C                   move      §pctcmc       EVBcev
727400131108     C                   movel     *blank        evbatb
727500131118     C                   move      evborv        woramin
727600131108     c     woramin       subdur    1:*mn         woramin
727700131108     C                   move      woramin       evborv
727800131108     C                   Z-ADD     pctfgs        evbfle
727900131108     C                   Z-ADD     dutcou        evbcdu
728000131108     C                   movel     pctdtorin     evbdev
728100131118     C                   move      pctdtorin     wevbhev           6 0
728200131118     C                   movel     wevbhev       evbhev
728300131108     C                   MOVEL     *BLANKS       evbftr
728400131108     C                   Z-ADD     0             evbdtr
728500131108     c                   clear                   devb01
728600131108     C                   move      ARBPDC        §NOTPDC
728700131112     c                   if        arbndc > 0
728800131108     C                   move      ARBNDC        §NOTNDC
728900131112     c                   else
729000131112     c                   move      warbndc       §notndc
729100131112     c                   endif
729200131108     c                   move      'P'           §notprg
729300131213     c                   move      'P'           §notagg
729400131108     C                   MOVEL     Devb01        EVBNOT
729500131108     C                   Z-ADD     0             EVBDTC
729600131121     C* se motivo di lasciato avviso e non ho ancora avuto un ric per la bolla lo forzo
729700131121     c                   if        w§2aftc = 'A'
729800131121     c     kevbric       setll     fnevb04l                               86
729900131121     c  n86              move      'RIC'         evbcev
730000131121     c                   endif
730100131121      *
730200131108     C                   WRITE     FNEVB000
730300131108      *
730400131108     c                   endsr
730500090622     C*---------------------------------------------------------
730600090622     C* Cancellazione eventuali eventi particolari inseriti con data ora maggiori della consegna
730700090622     C*---------------------------------------------------------
730800090622?    C     delevepar     BEGSR
730900090622     c     kevbpar       setll     fnevb01l
731000090622     c                   do        *hival
731100090622     c     kevbpar2      reade     fnevb01l
731200090622     c                   if        %eof(fnevb01l)
731300090622     c                   leave
731400090622     c                   endif
731500090622     c                   z-add     1             m                 3 0
731600090622     c     evbcev        lookup    c2a(m)                                 67
731700090622     c   67              movel     d2z(m)        ds2z
731800090622     c                   if        §2zcep <> *blank
731900090622     c                   delete    fnevb000
732000090622     c                   endif
732100090622     c                   enddo
732200090622     c                   endsr
732300941123     C*---------------------------------------------------------
732400941123     C* GESTIONE ANOMALIE COLLI CONSEGNATI
732500941123     C*---------------------------------------------------------
732600941123?    C     GESANM        BEGSR
732700941123     C*
732800960418     C                   CLEAR                   DSLR33
732900960418     C                   Z-ADD     ARTDCM        D33DCH
733000960418     C                   MOVEL     '4'           D33FSC
733100960418     C                   MOVEL     'CO'          D33CCH
733200960426     C                   MOVEL     V02CCA        D33CCA
733300960418     C*
733400941129     C                   Z-ADD     ARTFLS        KFLS
733500941129     C                   Z-ADD     ARTLNA        KLNA
733600941130     C                   Z-ADD     ARTNRS        KNRS1
733700941123     C                   Z-ADD     ARTNSC        KSCN
733800990519     C     KANM          CHAIN     FNANM02L                           94
733900941123     C     *IN94         DOWEQ     '0'
734000960418     C                   Z-ADD     ANMNR2        D33NRR
734100960418     C                   CALL      'FNLR33R'
734200960418     C                   PARM                    DSLR33
734300990519     C     KANM          READE     FNANM02L                               94
734400960418     C                   ENDDO
734500941123     C*
734600941123?    C                   ENDSR
734700941123     C*-----------------------------------------------------*
734800941123     C*  Operazioni iniziali
734900941123     C*-----------------------------------------------------*
735000941123     C     DEFCAM        BEGSR
735100941123     C*
735200970227     C                   MOVEL     *BLANKS       WRISTA
735300941213     C                   Z-ADD     D53LNA        V1CLNA            3 0
735400941123     C                   Z-ADD     ND1           NDC
735500941123     C                   MOVEL     'N'           WAGGIO            1
735600941123     C                   MOVEL     ' '           WFINE             1
735700941124     C                   MOVEL     '2'           WTPVID            1
735800941123     C                   MOVEL     'N'           WTRIMP            1
735900060303     C                   MOVEL     'N'           WTRpre            1
736000941124     C                   MOVEL     D53DSF        V2DRSC
736100941130     C                   MOVEL     D53DSF        DESPAD
736200941130     C                   MOVEL     D53WRT        CNTPDR
736300941124     C                   Z-ADD     1             NRR2
736400941124     C                   Z-ADD     1             NRR3
736500941124     C                   Z-ADD     1             NRR6
736600941124     C                   Z-ADD     1             NRR7
736700941124     C                   Z-ADD     1             NR2KEY
736800941124     C                   Z-ADD     1             NR3KEY
736900950221     C* SE NON DEVO FARE CONTEGGI PADR.
737000950221     C     CNTPDR        IFEQ      'N'
737100950221     C*----------------------------------------------------*
737200970926     C                   OPEN      FNSTP10F
737300971001     C* EFFETTUO UNA CHAIN FITTIZIA DI FNSTP10F PER GIUSTIFICARE
737400971001     C* L'APERTURA IN UPDATE
737500971001     C                   READ(N)   FNSTP10F                               96
737600950221     C                   ENDIF
737700070319     C*  reperisco AZORG della linea di arrivo per abilitazioni
737800070319     c                   clear                   og148
737900070405     c                   clear                   og146
738000070319     C     v1cLNA        CHAIN     AZORG01L                           74
738100070319     C  N74              MOVEL     ORGDE8        og148                           L.ARRIVO
738200070405     C  N74              MOVEL     ORGDE6        og146                           L.ARRIVO
738300070424     c                   if        §ogdda <> *blank
738400070424     c                   move      §ogdda        ogddan            8 0
738500070424     c                   endif
738600950221      *
738700941123     C                   ENDSR
738800941123     C*-----------------------------------------------------*
738900941123     C*  Operazioni iniziali
739000941123     C*-----------------------------------------------------*
739100941123     C     *INZSR        BEGSR
739200941123     C*
739300941123     C* Apertura controllata dei files
739400020823     C                   OPEN      FNFVV01L                             16
739500070410     C  N16              OPEN      fidst09l                             16
739600941124     C  N16              OPEN      CNASS00F                             16
739700990519     C  N16              OPEN      FNANM02L                             16
739800990519     C  N16              OPEN      FNANM01L                             16
739900990803     C  N16              OPEN      FNEVB01L                             16
740000070312     C  N16              OPEN      FNEVB20f                             16
740100011121     C  N16              OPEN      fiftt01L                             16
740200060203     C  N16              OPEN      fiftd22L                             16
740300941124     C  N16              OPEN      FNARB01L                             16
740400070313     C  N16              OPEN      Fiarp01L                             16
740500060214     C  N16              OPEN      FiAR401L                             16
740600051115     C  N16              OPEN      FIAR901L                             16
740700941124     C  N16              OPEN      FNART01L                             16
740800960116     C  N16              OPEN      FNARBX0F                             16
740900980402     C  N16              OPEN      FNCDS02L                             16
741000051027     C  N16              OPEN      FIARS01L                             16
741100990924     C  N16              OPEN      FIARI01L                             16
741200941123     C   16              GOTO      FINE                                         OPEN vado a fine
741300950619     C*  Richiamo PARUT
741400950619     C                   Z-ADD     1             CODUT
741500950619     C                   CALL      'X§PARUT'
741600950619     C                   PARM                    UTEDSE
741700950619      * Trasformo data contabilizz. da 6 a 8
741800950619     C                   MOVEL     DTU(10)       W20               2 0
741900950619     C     W20           IFGT      50
742000950619     C     1900          ADD       W20           W40               4 0
742100950619     C                   ELSE
742200950619     C     2000          ADD       W20           W40
742300950619     C                   END
742400950619     C                   MOVE      DTU(10)       §DTCON            8 0
742500950619     C                   MOVEL     W40           §DTCON
742600020822     c*
742700020822     c     *dtaara       define    §azute        azuteds
742800020822     c     *dtaara       define    §datiute      ddatiute
742900020822     C                   in(E)     *dtaara
743000020822     C                   IF        %Error  or  RSUT = *blanks
743100020822     C                   call      'TIBS34R'
743200020822     C                   parm                    Tibs34Ds
743300020822     C                   in        *dtaara
743400020822     c                   ENDIF
743500941123     C*----------------------------------------------------*
743600941123     C*  Definizione chiavi
743700080808     c     kar5          klist
743800080808     c                   kfld                    V2CAAS
743900080808     c                   kfld                    V2CLNP
744000080808     c                   kfld                    V2CNRS
744100080808     c                   kfld                    V2CNSP
744200080808     c                   kfld                    trek5             3
744300120302     c     kspevid       klist
744400120302     c                   kfld                    V3CAAS
744500120302     c                   kfld                    V3CLNP
744600120302     c                   kfld                    V3CNRS
744700120302     c                   kfld                    V3CNSP
744800080808
744900080806     C     Kcet          KLIST
745000100128     C                   KFLD                    d53fgs
745100080806     C                   KFLD                    vnmdoc6           6 0
745200080808     C                   KFLD                    w53pdc
745300080806     C                   KFLD                    TTRD
745400080806     C                   KFLD                    V2CAAS
745500080806     C                   KFLD                    V2CLNP
745600080806     C                   KFLD                    V2CNRS
745700080806     C                   KFLD                    V2CNSP
745800080806     C                   move      'CET'         TTRD              3
745900140609     C     Kpdc          KLIST
746000140609     C                   KFLD                    d53fgs
746100140609     C                   KFLD                    vnmdoc6           6 0
746200140609     C                   KFLD                    w53pdc
746300140609     C                   KFLD                    V2CAAS
746400140609     C                   KFLD                    V2CLNP
746500140609     C                   KFLD                    V2CNRS
746600140609     C                   KFLD                    V2CNSP
746700140609     C     Kevbric       KLIST
746800131121     C                   KFLD                    evbAAS
746900131121     C                   KFLD                    evbLNP
747000131121     C                   KFLD                    evbNRS
747100131121     C                   KFLD                    evbNSP
747200131121     C                   KFLD                    wevbric
747300131121     C                   move      'RIC'         wevbric           3
747400941123     C     KARB1         KLIST
747500941123     C                   KFLD                    KAAS
747600941123     C                   KFLD                    KLNP
747700941123     C                   KFLD                    KNRS
747800941123     C                   KFLD                    KNSP
747900990924     C*
748000990924     C     KARI          KLIST
748100990924     C                   KFLD                    KAAS
748200990924     C                   KFLD                    KLNP
748300990924     C                   KFLD                    KNRS
748400990924     C                   KFLD                    KNSP
748500990924     C                   KFLD                    KTIP
748600990924     C                   KFLD                    KDIV
748700941123     C*
748800941123     C     KABI1         KLIST
748900941123     C                   KFLD                    KABI
749000941123     C                   KFLD                    KCAB
749100941123     C*
749200941123     C     KART1         KLIST
749300941129     C                   KFLD                    KAAS1
749400941129     C                   KFLD                    KLNP1
749500941129     C                   KFLD                    KNRS1
749600941129     C                   KFLD                    KNSP1
749700941129     C*
749800941129     C     KART2         KLIST
749900941129     C                   KFLD                    KAAS1
750000941129     C                   KFLD                    KLNP1
750100941129     C                   KFLD                    KNRS1
750200941129     C                   KFLD                    KNSP1
750300941130     C                   KFLD                    KNSC
750400941123     C*
750500941123     C     KGCA          KLIST
750600941123     C                   KFLD                    KAAS
750700941123     C                   KFLD                    KLNP
750800941123     C                   KFLD                    KNRS
750900941123     C                   KFLD                    KNSP
751000941123     C*
751100941123     C     KARB2         KLIST
751200941123     C                   KFLD                    KAAS2
751300941123     C                   KFLD                    KLNP2
751400941123     C                   KFLD                    KNRS2
751500941123     C                   KFLD                    KNSP2
751600941123     C*
751700941123     C     KAR4          KLIST
751800941123     C                   KFLD                    KAAS
751900941123     C                   KFLD                    KLNP
752000941123     C                   KFLD                    KNRS
752100941123     C                   KFLD                    KNSP
752200941123     C                   KFLD                    KTRC
752300941123     C*
752400941123     C     KFVV          KLIST
752500941123     C                   KFLD                    KNPG
752600941123     C                   KFLD                    KNFV
752700020902     C                   KFLD                    kfgs
752800070316     C     Kdst          KLIST
752900070410     C                   KFLD                    kfgs
753000070316     C                   KFLD                    KNPG
753100070316     C                   KFLD                    KNFV6             6 0
753200120305     C     Kdst4         KLIST
753300120305     C                   KFLD                    $cssfgs
753400120615     C                   KFLD                    NDCI
753500120312     C     Kdst41        KLIST
753600120312     C                   KFLD                    $cssfgs
753700120615     C                   KFLD                    NDCI
753800120312     C                   KFLD                    exopz
753900941229     C*
754000941123     C                   Z-ADD     4             KNPG
754100941123     C*
754200941123     C     KTAB1         KLIST
754300941123     C                   KFLD                    KKUT
754400941123     C                   KFLD                    KCOD
754500941123     C*
754600941123     C     KTAB2         KLIST
754700941123     C                   KFLD                    KKUT
754800941123     C                   KFLD                    KCOD
754900941123     C                   KFLD                    KKEY
755000990923     C*
755100990923     C     KTBE          KLIST
755200990924     C                   KFLD                    KCOD1
755300990923     C                   KFLD                    KKE1
755400941123     C*
755500080709     C     KTBE2         KLIST
755600080709     C                   KFLD                    KCOD1
755700080709     C                   KFLD                    KKE1
755800080709     C                   KFLD                    KKE2
755900080709     C*
756000941123     C     KFTT          KLIST
756100941123     C                   KFLD                    KPDR
756200941123     C                   KFLD                    KTSR
756300941123     C                   KFLD                    KNDC
756400941123     C*
756500941123     C     KFTD          KLIST
756600941123     C                   KFLD                    KAAS
756700941123     C                   KFLD                    KLNP
756800941123     C                   KFLD                    KNRS
756900941123     C                   KFLD                    KNSP
757000950113     C                   KFLD                    KTSR
757100941214     C                   KFLD                    KPDR
757200941214     C                   KFLD                    KNDC
757300941123     C*
757400941123     C     KANM          KLIST
757500941123     C                   KFLD                    KFLS
757600941123     C                   KFLD                    KLNA
757700941130     C                   KFLD                    KNRS1
757800941123     C                   KFLD                    KSCN
757900950915     C     KANM1         KLIST
758000990519     C                   KFLD                    KAA4
758100950915     C                   KFLD                    KLNP
758200950915     C                   KFLD                    KNRS
758300950915     C                   KFLD                    KNSP
758400950915     C                   KFLD                    KCAA
758500941123     C*
758600941123     C     KEVB2         KLIST
758700990803     C                   KFLD                    KAAS
758800941123     C                   KFLD                    KLNP
758900941123     C                   KFLD                    KNRS
759000941123     C                   KFLD                    KNSP
759100941123     C*
759200941123     C     KEVB1         KLIST
759300990803     C                   KFLD                    KAAS
759400941123     C                   KFLD                    KLNP
759500941123     C                   KFLD                    KNRS
759600941123     C                   KFLD                    KNSP
759700941123     C                   KFLD                    KDEV
759800941123     C                   KFLD                    KHEV
759900090622     C*
760000090622     C     kevbpar       KLIST
760100090622     C                   KFLD                    KAAS1
760200090622     C                   KFLD                    KLNP1
760300090622     C                   KFLD                    KNRS1
760400090622     C                   KFLD                    KNSP1
760500090622     C                   KFLD                    ARBDCM
760600090622     C                   KFLD                    ARBHMC
760700090622     C*
760800090622     C     kevbpar2      KLIST
760900090622     C                   KFLD                    KAAS1
761000090622     C                   KFLD                    KLNP1
761100090622     C                   KFLD                    KNRS1
761200090622     C                   KFLD                    KNSP1
761300090622      *
761400980402     C     KARS0         KLIST
761500980402     C                   KFLD                    KFLS
761600980402     C                   KFLD                    KLNA
761700980402     C                   KFLD                    KNRS1
761800980402     C                   KFLD                    KNSC
761900980402     C                   KFLD                    KTRC
762000060323      *
762100060323     C     Kass          KLIST
762200060323     C                   KFLD                    incfle
762300060323     C                   KFLD                    inclnp
762400060323     C                   KFLD                    incaas
762500060323     C                   KFLD                    incnrs
762600060323     C                   KFLD                    incnsp
762700980402      *
762800980402     C     KCDS0         KLIST
762900980402     C                   KFLD                    KNDC
763000980402     C                   KFLD                    KAAS1
763100980402     C                   KFLD                    KLNP1
763200980402     C                   KFLD                    KNRS1
763300980402     C                   KFLD                    KNSP1
763400030318     c* quadratura giornaliera
763500030318     C     Kqig          KLIST
763600030318     C                   KFLD                    qigann
763700030318     C                   KFLD                    dutpou
763800030318     C                   KFLD                    oggi
763900030318     c                   clear                   qigann
764000050516     C     Kapd          KLIST
764100050516     C                   KFLD                    apdtip
764200050516     C                   KFLD                    w53pdc            7 0
764300050516     c                   movel     'A'           apdtip
764400070309     C     Karp          KLIST
764500070309     C                   KFLD                    v2cAAS
764600070309     C                   KFLD                    v2cLNP
764700070309     C                   KFLD                    v2cNRS
764800070309     C                   KFLD                    v2cNSP
764900071106     C     Kvarb1        KLIST
765000071106     C                   KFLD                    v2cAAS
765100071106     C                   KFLD                    v2cLNP
765200071106     C                   KFLD                    v2cNRS
765300071106     C                   KFLD                    v2cNSP
765400071026     C     Kvarb         KLIST
765500071026     C                   KFLD                    d48AAS
765600071026     C                   KFLD                    d48LNP
765700071026     C                   KFLD                    d48NRS
765800071026     C                   KFLD                    d48NSP
765900071030     C     Kvar          KLIST
766000071030     C                   KFLD                    d48AAS
766100071030     C                   KFLD                    d48LNP
766200071030     C                   KFLD                    d48NRS
766300071030     C                   KFLD                    d48NSP
766400071026     C     Kwf           KLIST
766500071026     C                   KFLD                    varbAAS
766600071026     C                   KFLD                    varbLNP
766700071026     C                   KFLD                    varbNRS
766800071026     C                   KFLD                    varbNSP
766900071026     C                   KFLD                    varbdim
767000090804     C     Kwf1          KLIST
767100090804     C                   KFLD                    varbAAS
767200090804     C                   KFLD                    varbLNP
767300090804     C                   KFLD                    varbNRS
767400090804     C                   KFLD                    varbNSP
767500080714     C     Kbrv          KLIST
767600080714     C                   KFLD                    NPG8
767700080714     C                   KFLD                    wdisbrv           6 0
767800100128     C                   KFLD                    d53fgs
767900080714     C                   KFLD                    artfls
768000080714     C                   KFLD                    artlna
768100080714     C                   KFLD                    artnrs
768200080714     C                   KFLD                    artnsc
768300080714     C                   z-add     8             NPG8              1 0
768400080715     C     Kdsf          KLIST
768500080715     C                   KFLD                    NPG4              1 0
768600080715     C                   KFLD                    wDSFndc           6 0
768700100118     C                   KFLD                    d53fgs
768800080715     C                   KFLD                    fase              3
768900080715     C                   move      'CRI'         fase
769000080715     C                   z-add     4             NPG4
769100080723     C     Kced          KLIST
769200100128     C                   KFLD                    d53fgs
769300080805     C                   KFLD                    wdisbrv
769400101115     C                   KFLD                    w53pdc
769500080805     C                   KFLD                    TRDCED
769600080723     C                   KFLD                    artAAS
769700080723     C                   KFLD                    artLNP
769800080723     C                   KFLD                    artNRS
769900080723     C                   KFLD                    artNSP
770000080723     C                   KFLD                    artNSC
770100080723     C                   move      'CED'         trdced            3
770200141212ab   C*
770300141212ab   c     Key_TGD11     klist
770400141212ab   c                   kfld                    tgDTOR
770500141212ab   c                   kfld                    tgDOGG
770600141212ab   c                   kfld                    tgDMAD
770700141212ab    *
770800141212ab   c     Key_TGR01     klist
770900141212ab   c                   kfld                    tgRANT
771000141212ab   c                   kfld                    tgRNUT
771100141212ab   c                   kfld                    tgRPDT
771200141212ab   c                   kfld                    tgRTRI
771300941123     C*----------------------------------------------------*
771400941123     C*  Definizione delle variabili
771500000526     C     *LIKE         DEFINE    FVVDS         FVVDS2
771600980402     C     *LIKE         DEFINE    ARBAAS        KAAS2
771700941123     C     *LIKE         DEFINE    ARBLNP        KLNP2
771800941123     C     *LIKE         DEFINE    ARBNRS        KNRS2
771900941123     C     *LIKE         DEFINE    ARBNSP        KNSP2
772000990924     C     *LIKE         DEFINE    ARITIP        KTIP
772100990924     C     *LIKE         DEFINE    ARIDIV        KDIV
772200941129     C     *LIKE         DEFINE    ARBAAS        KAAS1
772300941129     C     *LIKE         DEFINE    ARBLNP        KLNP1
772400941129     C     *LIKE         DEFINE    ARBNRS        KNRS1
772500941129     C     *LIKE         DEFINE    ARBNSP        KNSP1
772600941123     C     *LIKE         DEFINE    ARBAAS        KAAS
772700941123     C     *LIKE         DEFINE    ARBLNP        KLNP
772800941123     C     *LIKE         DEFINE    ARBNRS        KNRS
772900941123     C     *LIKE         DEFINE    ARBNSP        KNSP
773000941123     C     *LIKE         DEFINE    AR4TRC        KTRC
773100941123     C     *LIKE         DEFINE    ARBAAS        KAAP
773200941123     C     *LIKE         DEFINE    ARBLNP        KLPP
773300941123     C     *LIKE         DEFINE    ARBNRS        KNRP
773400941123     C     *LIKE         DEFINE    ARBVLF        WVOL
773500021107     C     *like         define    ARBVLF        wvolparz
773600021107     C     *like         define    arbpkF        wpes
773700021107     C     *like         define    arbpkF        wpesparz
773800941123     C     *LIKE         DEFINE    FVVNPG        KNPG
773900941123     C     *LIKE         DEFINE    FVVNFV        KNFV
774000020902     C     *LIKE         DEFINE    FVVfgs        Kfgs
774100941123     C     *LIKE         DEFINE    ABIABI        KABI
774200941123     C     *LIKE         DEFINE    ABICAB        KCAB
774300941123     C     *LIKE         DEFINE    TBLKUT        KKUT
774400941123     C     *LIKE         DEFINE    TBLCOD        KCOD
774500941123     C     *LIKE         DEFINE    TBLKEY        KKEY
774600990923     C     *LIKE         DEFINE    TBEKE1        KKE1
774700080709     C     *LIKE         DEFINE    TBEKE2        KKE2
774800941123     C     *LIKE         DEFINE    TBLKUT        §KUT
774900941123     C     *LIKE         DEFINE    TBLCOD        §COD
775000990924     C     *LIKE         DEFINE    TBECOD        KCOD1
775100941123     C     *LIKE         DEFINE    EVBDEV        KDEV
775200941123     C     *LIKE         DEFINE    EVBHEV        KHEV
775300941123     C     *LIKE         DEFINE    ANMFLS        KFLS
775400950918     C     *LIKE         DEFINE    ANMCAA        KCAA
775500950620     C     *LIKE         DEFINE    ANMLNA        KLNA
775600941123     C     *LIKE         DEFINE    ANMSCN        KSCN
775700941129     C     *LIKE         DEFINE    ARTNSC        KNSC
775800990519     C     *LIKE         DEFINE    ANMAAS        KAA4
775900941123     C     *LIKE         DEFINE    FTTPDR        KPDR
776000941123     C     *LIKE         DEFINE    FTTNDC        KNDC
776100941123     C     *LIKE         DEFINE    FTTTSR        KTSR
776200970926     C     *LIKE         DEFINE    FTTSET        WSET
776300941123     C     *LIKE         DEFINE    NRR6          CONT
776400941123     C     *LIKE         DEFINE    NRR6          TOTCAR
776500941123     C     *LIKE         DEFINE    NRR6          KJ
776600941123     C     *LIKE         DEFINE    NRR6          TOTSFH
776700941123     C     *LIKE         DEFINE    NRR6          POSIZ
776800941123     C     *LIKE         DEFINE    NRR6          SAVREC
776900941123     C     *LIKE         DEFINE    NRR6          SAVNRR
777000941123     C     *LIKE         DEFINE    NRR6          NRR2
777100970926     C     *LIKE         DEFINE    NRR6          SAVNR2
777200941123     C     *LIKE         DEFINE    NRR6          NRR3
777300941123     C     *LIKE         DEFINE    ARBDCM        OGGI
777400941123     C     *LIKE         DEFINE    ARBDCM        WOGGI
777500941123     C     *LIKE         DEFINE    ARBDCM        DATCON
777600941123     C     *LIKE         DEFINE    ARBDCM        DTCONS
777700941123     C     *LIKE         DEFINE    ARBDCM        DTSPED
777800941123     C     *LIKE         DEFINE    ARBDCM        WDTGIA
777900941123     C     *LIKE         DEFINE    ARBDCM        WDTASS
778000941123     C     *LIKE         DEFINE    ARBDCM        WDTAVV
778100941123     C     *LIKE         DEFINE    ARBNDC        WSVNDC
778200941123     C     *LIKE         DEFINE    ARBICC        COMICC
778300941128     C     *LIKE         DEFINE    ARBAAS        ANNO
778400941123     C     *LIKE         DEFINE    V2CNT1        NOTA
778500941123     C     *LIKE         DEFINE    V02DT1        DAT4
778600941123     C     *LIKE         DEFINE    §2ZTIC        WTPGIA
778700941123     C     *LIKE         DEFINE    ARBPKF        SAVPKG
778800941123     C     *LIKE         DEFINE    ARBNCL        SAVNCL
778900941123     C     *LIKE         DEFINE    ARBVLF        SAVVOL
779000020829     C     *LIKE         DEFINE    ARBifp        COMifp
779100031209     C     *LIKE         DEFINE    ARBpdc        COMpdc
779200020829     C     *LIKE         DEFINE    ARBNDC        COMNDC
779300941123     C     *LIKE         DEFINE    ARBDCM        COMDCM
779400990311     C     *LIKE         DEFINE    ARBDCP        COMDCP
779500941123     C     *LIKE         DEFINE    ARBHMC        COMHMC
779600941123     C     *LIKE         DEFINE    ARBDDC        COMDDC
779700941123     C     *LIKE         DEFINE    V3CTIC        WSVTIC
779800941123     C     *LIKE         DEFINE    V02AS1        WSVAS1
779900941123     C     *LIKE         DEFINE    V02AB1        WSVAB1
780000941123     C     *LIKE         DEFINE    V02CA1        WSVCA1
780100941123     C     *LIKE         DEFINE    V02DT1        WSVDT1
780200941123     C     *LIKE         DEFINE    ARBNDC        STPNDC
780300941123     C     *LIKE         DEFINE    ARBDDC        STPDDC
780400941123     C     *LIKE         DEFINE    ARBPDC        STPPDR
780500941123     C     *LIKE         DEFINE    D53DSF        DESPAD
780600941123     C     *LIKE         DEFINE    ARBLNP        STPLNP
780700941123     C     *LIKE         DEFINE    ARBAAS        STPAAS
780800941123     C     *LIKE         DEFINE    ARBNRS        STPNRS
780900941123     C     *LIKE         DEFINE    ARBNSP        STPNSP
781000941123     C     *LIKE         DEFINE    ARBMGS        STPGMS
781100941123     C     *LIKE         DEFINE    ARBCMC        STPCMC
781200941123     C     *LIKE         DEFINE    ARBTMC        STPTMC
781300941123     C     *LIKE         DEFINE    ARBNCL        STPNCL
781400941123     C     *LIKE         DEFINE    ARBPKF        STPPKG
781500941123     C     *LIKE         DEFINE    ARBVLF        STPVOL
781600941123     C     *LIKE         DEFINE    V02CPZ        STPCPZ
781700941123     C     *LIKE         DEFINE    V02RIS        STPRIS
781800941123     C     *LIKE         DEFINE    ABIABI        STPABI
781900941123     C     *LIKE         DEFINE    ABICAB        STPCAB
782000941123     C     *LIKE         DEFINE    V3CIMP        STPIMP
782100941123     C     *LIKE         DEFINE    V3CVCA        STPVAL
782200941123     C     *LIKE         DEFINE    ARBNCL        STPTCL
782300941123     C     *LIKE         DEFINE    ARBPKF        STPTPK
782400941123     C     *LIKE         DEFINE    ARBVLF        STPTVL
782500941123     C     *LIKE         DEFINE    ARBSTP        STPTST
782600000505     C     *LIKE         DEFINE    V05ADR        STPADR
782700000505     C     *LIKE         DEFINE    V05NDR        STPNDR
782800941123     C     *LIKE         DEFINE    ARBNCL        STPTNC
782900941123     C     *LIKE         DEFINE    ARBPKF        STPTNP
783000941123     C     *LIKE         DEFINE    ARBVLF        STPTNV
783100941123     C     *LIKE         DEFINE    V05TSC        STPTSC
783200941123     C     *LIKE         DEFINE    V05TSC        STPTPA
783300941123     C     *LIKE         DEFINE    V05TSC        STPNSC
783400941123     C     *LIKE         DEFINE    V05TSC        STPNFA
783500941123     C     *LIKE         DEFINE    V05TSC        STPNGC
783600941123     C     *LIKE         DEFINE    V05TSC        STPNLA
783700991007     C     *LIKE         DEFINE    V05TCE        STPEES
783800990924     C     *LIKE         DEFINE    V05TCE        STPECO
783900990924     C     *LIKE         DEFINE    V05TAE        STPEAN
784000991007     C     *LIKE         DEFINE    V05TAV        STPDAD
784100991007     C     *LIKE         DEFINE    V05TDE        STPEAD
784200990924     C     *LIKE         DEFINE    D53WRT        CNTPDR
784300941207     C     *LIKE         DEFINE    V3CAAS        WSVAAS
784400941207     C     *LIKE         DEFINE    V3CLNP        WSVLNP
784500941207     C     *LIKE         DEFINE    V3CNRS        WSVNRS
784600941207     C     *LIKE         DEFINE    V3CNSP        WSVNSP
784700991007     C     *LIKE         DEFINE    V3CNSP        WTOTSP
784800991007     C     *LIKE         DEFINE    V3CIMP        WASS
784900991007     C     *LIKE         DEFINE    V3CIMP        WCAS
785000991007     C     *LIKE         DEFINE    V3CIMP        WTOT
785100991007     C     *LIKE         DEFINE    V3CIMP        WIMP
785200991007     C     *LIKE         DEFINE    V3CVCA        WDIV
785300991007     C     *LIKE         DEFINE    V3CVCA        WVCA
785400991007     C     *LIKE         DEFINE    V3CVCA        WVTO
785500991007     C     *LIKE         DEFINE    V3CVCA        WVAL
785600991007     C     *LIKE         DEFINE    V3CTIC        WTCA
785700991007     C     *LIKE         DEFINE    V3CTIC        WTIC
785800991007     C     *LIKE         DEFINE    V3CTIC        WTTO
785900991007     C     *LIKE         DEFINE    §1AFCA        WFCA
786000991007     C     *LIKE         DEFINE    §1AFCA        WFCC
786100991007     C     *LIKE         DEFINE    §1AFCA        WFCT
786200991007     C     *LIKE         DEFINE    §1AFCA        WCON
786300991007     C     *LIKE         DEFINE    §1AFMB        WFMA
786400991007     C     *LIKE         DEFINE    §1AFMB        WFMC
786500991007     C     *LIKE         DEFINE    §1AFMB        WFMT
786600991007     C     *LIKE         DEFINE    §1AFMB        WMIT
786700991007     C     *LIKE         DEFINE    V02SN1        WSNA
786800991007     C     *LIKE         DEFINE    V02SN1        WSNC
786900991007     C     *LIKE         DEFINE    V02SN1        WSNT
787000941123     C*----------------------------------------------------*
787100941123     C* Giro data del giorno
787200941123     C                   TIME                    WHHDAT           14 0
787300941123     C                   MOVEL     WHHDAT        TIMES             6 0
787400941123     C                   MOVE      WHHDAT        G02DAT
787500941123     C                   MOVEL     *BLANK        G02ERR
787600941123     C                   CALL      'XSRDA8'
787700941123     C                   PARM                    WLBDA8
787800941123     C                   Z-ADD     G02INV        OGGI
787900941123     C                   Z-ADD     G02DAT        WOGGI
788000950206     C                   MOVEL     G02DAT        WOGGI6            6 0
788100950206     C                   MOVE      G02DAT        AA2               2 0
788200950206     C                   MOVE      AA2           WOGGI6
788300941123     C*----------------------------------------------------*
788400941123     C                   Z-ADD     1             KKUT
788500941123     C*----------------------------------------------------*
788600990923     C*  tabelle GED x reperimento divisa corrente
788700000211     C                   MOVEL     'GED'         KCOD1
788800990923     C                   MOVEL     *BLANKS       KKE1
788900990923     C                   MOVEL     '1'           KKE1
789000990923     C     KTBE          CHAIN     TNTBE01L                           96
789100990923     C     *IN96         IFEQ      '0'
789200990924     C                   MOVEL     TBEUNI        DGED
789300990923     C                   END
789400941123     C*  Caricamento tabelle
789500941123     C                   Z-ADD     0             X                 3 0
789600941123     C                   MOVEL     'CV'          KCOD
789700941123     C     KTAB1         CHAIN     TABEL00F                           96
789800941123     C     *IN96         DOWEQ     '0'
789900080903     C     X             ANDLT     200
790000941123     C     TBLFLG        IFEQ      ' '
790100941123     C                   ADD       1             X
790200941123     C                   MOVEL     TBLKEY        CCV(X)
790300941123     C                   MOVEL     TBLUNI        DSCV
790400990923     C                   MOVEL     §CVDVA        FLV(X)
790500941123     C                   END
790600941123     C     KTAB1         READE     TABEL00F                               96
790700941123     C                   END
790800951019     C     *IN96         IFEQ      '0'
790900000211     C                   MOVEL     MSG(1)        DSMSG
791000000211     C                   MOVEL     KCOD          KCODM
791100000211     C                   MOVEL     DSMSG         MSG(1)
791200100825     c                   exsr      sndmail
791300951019     C                   END
791400941123     C*
791500000505     C                   CLEAR                   DS1A
791600000505     C                   Z-ADD     0             Z                 3 0
791700941123     C                   Z-ADD     0             X                 3 0
791800941123     C                   MOVEL     '1A'          KCOD
791900941123     C     KTAB1         CHAIN     TABEL00F                           96
792000941123     C     *IN96         DOWEQ     '0'
792100080903     C     X             ANDLT     200
792200941123     C     TBLFLG        IFEQ      ' '
792300000505     C                   MOVEL     TBLUNI        DS1A
792400941123     C                   ADD       1             X
792500941123     C                   MOVEL     TBLKEY        C1A(X)
792600941123     C                   MOVEL     TBLUNI        D1A(X)
792700941123     C                   END
792800941123     C     KTAB1         READE     TABEL00F                               96
792900941123     C                   END
793000951019     C     *IN96         IFEQ      '0'
793100000211     C                   MOVEL     MSG(1)        DSMSG
793200000211     C                   MOVEL     KCOD          KCODM
793300000211     C                   MOVEL     DSMSG         MSG(1)
793400100825     c                   exsr      sndmail
793500951019     C                   END
793600941123     C*
793700941123     C                   Z-ADD     0             X                 3 0
793800941123     C                   MOVEL     '2A'          KCOD
793900941123     C     KTAB1         CHAIN     TABEL00F                           96
794000941123     C     *IN96         DOWEQ     '0'
794100080903     C     X             ANDLT     200
794200941123     C     TBLFLG        IFEQ      ' '
794300941123     C                   ADD       1             X
794400941123     C                   MOVEL     TBLKEY        C2A(X)
794500941123     C                   MOVEL     TBLUNI        D2A(X)
794600941123     C                   END
794700951019     C     KTAB1         READE     TABEL00F                               96
794800941123     C                   END
794900951019     C     *IN96         IFEQ      '0'
795000000211     C                   MOVEL     MSG(1)        DSMSG
795100000211     C                   MOVEL     KCOD          KCODM
795200000211     C                   MOVEL     DSMSG         MSG(1)
795300100825     c                   exsr      sndmail
795400951019     C                   END
795500941123     C*
795600941123     C                   MOVEL     '2Z'          KCOD
795700941123     C     KTAB1         CHAIN     TABEL00F                           96
795800941123     C     *IN96         DOWEQ     '0'
795900941123     C     TBLFLG        IFEQ      ' '
796000941123     C                   MOVEL     TBLKEY        COD2Z             3
796100941123     C                   Z-ADD     1             X
796200941123     C     COD2Z         LOOKUP    C2A(X)                                 95
796300941123     C     *IN95         IFEQ      '1'
796400941124     C                   MOVEL     TBLUNI        D2Z(X)
796500941123     C                   END
796600941123     C                   END
796700941123     C     KTAB1         READE     TABEL00F                               96
796800941123     C                   END
796900951019     C     *IN96         IFEQ      '0'
797000000211     C                   MOVEL     MSG(1)        DSMSG
797100000211     C                   MOVEL     KCOD          KCODM
797200000211     C                   MOVEL     DSMSG         MSG(1)
797300100825     c                   exsr      sndmail
797400951019     C                   END
797500941123     C*
797600941123     C                   Z-ADD     0             X                 3 0
797700941123     C                   MOVEL     '3A'          KCOD
797800941123     C     KTAB1         CHAIN     TABEL00F                           96
797900941123     C     *IN96         DOWEQ     '0'
798000080903     C     X             ANDLT     200
798100941123     C     TBLFLG        IFEQ      ' '
798200941123     C                   ADD       1             X
798300941123     C                   MOVEL     TBLKEY        C3A(X)
798400941123     C                   MOVEL     TBLUNI        D3A(X)
798500941123     C                   END
798600941123     C     KTAB1         READE     TABEL00F                               96
798700941123     C                   END
798800951019     C     *IN96         IFEQ      '0'
798900000211     C                   MOVEL     MSG(1)        DSMSG
799000000211     C                   MOVEL     KCOD          KCODM
799100000211     C                   MOVEL     DSMSG         MSG(1)
799200100825     c                   exsr      sndmail
799300951019     C                   END
799400941123     C*
799500941123     C                   Z-ADD     0             X                 3 0
799600941123     C                   MOVEL     '7R'          KCOD
799700941123     C     KTAB1         CHAIN     TABEL00F                           96
799800941123     C     *IN96         DOWEQ     '0'
799900080903     C     X             ANDLT     200
800000941123     C     TBLFLG        IFEQ      ' '
800100941123     C                   ADD       1             X
800200941123     C                   MOVEL     TBLKEY        C7R(X)
800300941123     C                   MOVEL     TBLUNI        D7R(X)
800400941123     C                   END
800500941123     C     KTAB1         READE     TABEL00F                               96
800600941123     C                   END
800700951019     C     *IN96         IFEQ      '0'
800800000211     C                   MOVEL     MSG(1)        DSMSG
800900000211     C                   MOVEL     KCOD          KCODM
801000000211     C                   MOVEL     DSMSG         MSG(1)
801100100825     c                   exsr      sndmail
801200951019     C                   END
801300941123     C*
801400000505     C                   Z-ADD     0             Z                 3 0
801500941123     C                   Z-ADD     0             X                 3 0
801600000505     C                   CLEAR                   DS7O
801700941123     C                   MOVEL     '7O'          KCOD
801800941123     C     KTAB1         CHAIN     TABEL00F                           96
801900941123     C     *IN96         DOWEQ     '0'
802000080903     C     X             ANDLT     200
802100941123     C     TBLFLG        IFEQ      ' '
802200000505     C                   MOVEL     TBLUNI        DS7O
802300000512     C     §7OFPT        IFEQ      'B'
802400000512     C     §7OFPT        OREQ      *BLANK
802500941123     C                   ADD       1             X
802600941123     C                   MOVEL     TBLKEY        C7O(X)
802700941123     C                   MOVEL     TBLUNI        D7O(X)
802800000512     C                   END
802900941123     C                   END
803000941123     C     KTAB1         READE     TABEL00F                               96
803100941123     C                   END
803200951019     C     *IN96         IFEQ      '0'
803300000211     C                   MOVEL     MSG(1)        DSMSG
803400000211     C                   MOVEL     KCOD          KCODM
803500000211     C                   MOVEL     DSMSG         MSG(1)
803600100825     c                   exsr      sndmail
803700951019     C                   END
803800941123     C*
803900941123     C                   Z-ADD     0             X                 3 0
804000000505     C                   Z-ADD     0             Z                 3 0
804100000505     C                   CLEAR                   DS1P
804200941123     C                   MOVEL     '1P'          KCOD
804300941123     C     KTAB1         CHAIN     TABEL00F                           96
804400941123     C     *IN96         DOWEQ     '0'
804500080903     C     X             ANDLT     200
804600941123     C     TBLFLG        IFEQ      ' '
804700000505     C                   MOVEL     TBLUNI        DS1P
804800000512     C     §1PUAR        IFEQ      'S'
804900941123     C                   ADD       1             X
805000941123     C                   MOVEL     TBLKEY        C1P(X)
805100941123     C                   MOVEL     TBLUNI        D1P(X)
805200000512     C                   END
805300941123     C                   END
805400941123     C     KTAB1         READE     TABEL00F                               96
805500941123     C                   END
805600951019     C     *IN96         IFEQ      '0'
805700000211     C                   MOVEL     MSG(1)        DSMSG
805800000211     C                   MOVEL     KCOD          KCODM
805900000211     C                   MOVEL     DSMSG         MSG(1)
806000100825     c                   exsr      sndmail
806100951019     C                   END
806200960227     C*
806300960227     C                   Z-ADD     0             X                 3 0
806400960227     C                   MOVEL     '7U'          KCOD
806500960227     C     KTAB1         CHAIN     TABEL00F                           96
806600960227     C     *IN96         DOWEQ     '0'
806700000211     C     X             ANDLT     500
806800960227     C     TBLFLG        IFEQ      ' '
806900050221     C     §7ustb        andeq     'S'
807000960227     C                   ADD       1             X
807100960227     C                   MOVEL     TBLUNI        DS7U
807200960228     C                   MOVEL     TBLKEY        WC7U              7
807300960228     C                   MOVEL     WC7U          C7U(X)
807400960227     C                   MOVEL     §7USTB        F7U(X)
807500960227     C                   END
807600960227     C     KTAB1         READE     TABEL00F                               96
807700960227     C                   END
807800960227     C     *IN96         IFEQ      '0'
807900000211     C                   MOVEL     MSG(1)        DSMSG
808000000211     C                   MOVEL     KCOD          KCODM
808100000211     C                   MOVEL     DSMSG         MSG(1)
808200100825     c                   exsr      sndmail
808300960227     C                   END
808400941123     C*
808500941123     C                   Z-ADD     0             X                 3 0
808600941123     C                   MOVEL     '7M'          KCOD
808700941123     C     KTAB1         CHAIN     TABEL00F                           96
808800941123     C     *IN96         DOWEQ     '0'
808900080903     C     X             ANDLT     200
809000941123     C     TBLFLG        IFEQ      ' '
809100941123     C                   ADD       1             X
809200941123     C                   MOVEL     TBLKEY        C7M(X)
809300941123     C                   END
809400941123     C     KTAB1         READE     TABEL00F                               96
809500941123     C                   END
809600951019     C     *IN96         IFEQ      '0'
809700000211     C                   MOVEL     MSG(1)        DSMSG
809800000211     C                   MOVEL     KCOD          KCODM
809900000211     C                   MOVEL     DSMSG         MSG(1)
810000100825     c                   exsr      sndmail
810100951019     C                   END
810200941123     C*
810300000211     C                   MOVEL     'GEI'         KCOD1
810400990923     C                   MOVEL     *BLANKS       KKE1
810500991007     C                   MOVEL     §GEDCR        KKE1
810600990923     C                   CLEAR                   DGEI
810700990924     C     KTBE          CHAIN     TNTBE01L                           96
810800941123     C     *IN96         IFEQ      '0'
810900990923     C                   MOVEL     TBEUNI        DGEI
811000941123     C                   END
811100941123     C*
811200011121     C*  tabelle GEDCN x reperimento divisa conteggi padroncini/tariffe
811300011121     C                   movel     'GED'         kcod1
811400011121     C                   movel     *blanks       kke1
811500011121     C                   movel     'CN'          kke1
811600011121     C                   clear                   dgedcn
811700011121     C     ktbe          chain     tntbe01l                           96
811800011121     C     *in96         ifeq      '0'
811900011121     C                   movel     tbeuni        dgedcn
812000011121     C                   end
812100020418     C*  tabelle dgd x reperimento limiti chiusura spedizioni
812200020418     C                   MOVEL     'DGD'         KCOD1
812300020418     C                   MOVEL     *BLANKS       KKE1
812400020418     C                   MOVEL     '1'           KKE1
812500020418     C     KTBE          CHAIN     TNTBE01L                           96
812600020418     C     *IN96         IFEQ      '0'
812700020418     C                   MOVEL     TBEUNI        DDGD
812800020418     C                   END
812900011121     C*
813000040330     C*tabelle vpo x reperimento limite massimo inserimento data cons.rich.
813100040330     C                   MOVEL     'VPO'         KCOD1
813200040330     C                   MOVEL     *BLANKS       KKE1
813300040330     C                   MOVEL     'VPO'         KKE1
813400040330     C     KTBE          CHAIN     TNTBE01L                           96
813500040330     C     *IN96         IFEQ      '0'
813600040330     C                   MOVEL     TBEUNI        Dvpo
813700040330     C                   END
813800040330     c                   move      oggi          dataiso
813900040330     c     dataiso       adddur    §vpodcr:*d    dataiso
814000040330     c                   move      dataiso       wvpodcr           8 0
814100040330     C*
814200941123     C                   ENDSR
814300100825      *----------------------------------------------------------------
814400100825     c     sndmail       begsr
814500100825      *----------------------------------------------------------------
814600120305     c                   eval      emlto = 'CedAlert@brt.it'
814700100825     c                   eval      emlcc = *blank
814800100825     c                   eval      emogg = 'ATTENZIONE Chiusura distinta '
814900100825     C                   eval      emmsg = msg(1)  + ' ' +
815000100825     c                             msg(2) + ' ' + msg(3)
815100100825     C                   CALL      'TIS701C1'
815200100825     C                   PARM                    emlto           253
815300100825     C                   PARM                    emlcc           253
815400100825     C                   PARM                    emogg            44
815500100825     C                   PARM                    emmsg          5000
815600100825     C                   PARM                    emesito           1
815700100825      *
815800100825     C                   ENDSR
815900941121     C*-----------------------------------------------------
816000941121     OQSYSPRT   E            TESTA            01
816100941121     O                       STT(1)              66
816200991115     O                       STT(2)             132
816300021016     O                       rsut                20
816400021016     O                                           21 '/'
816500941213     O                       D53LNA              24
816600941130     O                       WOGGI6             123 '  /  /  '
816700941121     O                       PAGE          Z    132
816800941130     O          E            TESTA            02
816900941130     O                       TIMES              123 '  .  .  '
817000941130     O          E            TESTA            03
817100941130     O                                           10 'DISTINTA: '
817200941121     O                       STPNDC        Z     18
817300941121     O                                         +  1 'DEL'
817400941122     O                       STPDDC            +  1 '  /  /    '
817500960607     O                                         +  1 'AUTOTRASPORTATORE:'
817600941130     O                       STPPDR        Z   +  1
817700941121     O                       DESPAD            +  2
817800941227     O                                         +  2 'TIPO PRESTAZIONE:'
817900941227     O                       TPPRE             +  1
818000031008     O          E    73      TESTA       1
818100031008     O                                           10 'DISTINTA: '
818200031008     O                       ndc(2)        Z     18
818300031008     O                                         +  1 'DEL'
818400031008     O                       dtd(2)            +  1 '  /  /    '
818500031008     O                                         +  1 '                  '
818600031008     O                       STPPDR        Z   +  1
818700031008     O                       DESPAD            +  2
818800031008     O                                         +  2 '                 '
818900031013     O                       tppre2            +  1
819000031008     O          E    76      TESTA       1
819100031008     O                                           10 'DISTINTA: '
819200031008     O                       ndc(3)        Z     18
819300031008     O                                         +  1 'DEL'
819400031008     O                       dtd(3)            +  1 '  /  /    '
819500031008     O                                         +  1 '                  '
819600031008     O                       STPPDR        Z   +  1
819700031008     O                       DESPAD            +  2
819800031008     O                                         +  2 '                 '
819900031013     O                       tppre3            +  1
820000031008     O          E    77      TESTA       1
820100031008     O                                           10 'DISTINTA: '
820200031008     O                       ndc(4)        Z     18
820300031008     O                                         +  1 'DEL'
820400031008     O                       dtd(4)            +  1 '  /  /    '
820500031008     O                                         +  1 '                  '
820600031008     O                       STPPDR        Z   +  1
820700031008     O                       DESPAD            +  2
820800031008     O                                         +  2 '                 '
820900031013     O                       tppre4            +  1
821000031008     O          E    78      TESTA       1
821100031008     O                                           10 'DISTINTA: '
821200031008     O                       ndc(5)        Z     18
821300031008     O                                         +  1 'DEL'
821400031008     O                       dtd(5)            +  1 '  /  /    '
821500031008     O                                         +  1 '                  '
821600031008     O                       STPPDR        Z   +  1
821700031008     O                       DESPAD            +  2
821800031008     O                                         +  2 '                 '
821900031013     O                       tppre5            +  1
822000031008     O          E            TESTA       2
822100941121     O                       STT(3)              66
822200941121     O                       STT(4)             132
822300031008     O          E            TESTA       1  1
822400941121     O                       STT(5)              66
822500941121     O                       STT(6)             132
822600941130     O          E            DETCON         1
822700941206     O                       STPLNP               3
822800941206     O                       STPNRS        Z      6
822900941121     O                       STPNSP        Z     14
823000941121     O                       STPGMS              21 '  /  /'
823100941121     O                       STPAAS              25
823200941206     O                       STPCMC         B    29
823300941206     O                       STPTMC         B    31
823400941206     O                       STPDCO         B    52
823500941121     O                       STPRSD              82
823600941121     O                       STPNCL        Z    110
823700941121     O                       STPPKG        2    119
823800941121     O                       STPVOL        2    126
823900941121     O                       STPCPZ             129
824000941121     O                       STPRIS             132
824100941121     O          E            DETIMP         1
824200941202     O                       STPTIC              50
824300941202     O               04                          58 'Ass.Nr.'
824400941202     O                       STPNAS              62
824500941202     O               04                          71 'ABI/CAB'
824600941202     O                       STPABI        Z     76
824700941202     O               04                          77 '/'
824800941202     O                       STPCAB        Z     82
824900941121     O                       STPIMP        2    100
825000120314     O*              06                         100 '   '
825100991008     O                       STPVAL             104
825200941123     O          E            STAMPA           01
825300941121     O                       STT(1)              66
825400941121     O                       STT(2)             132
825500021016     O                       rsut                20
825600021016     O                                           21 '/'
825700941213     O                       D53LNA              24
825800941123     O                       WOGGI6             118 '  /  /  '
825900941121     O                       PAGE          Z    132
826000941121     O          E            STAMPA           02
826100941121     O                       TIMES              118 '  .  .  '
826200941121     O          E            TOTAL       2
826300960607     O                                           18 'AUTOTRASPORTATORE:'
826400941121     O                       VIDDES            +  1
826500941121     O                                         +  1 'OPER.:'
826600020826     O                       dutpou            +  1
826700020826     O                       dutcou            +  1
826800941121     O          E            TOTAL       1
826900941121     O                                           10 'DISTINTA: '
827000941121     O                                           34 'DATA CONS.'
827100941121     O                                           45 'ORA CONS.'
827200941121     O          E            TOTAL       1
827300941121     O                       NDC(1)        Z      8
827400941122     O                       DTD(1)              21 '  /  /    '
827500941122     O                       DDC(1)              32 '  /  /    '
827600941121     O                       HRC(1)              43 '  :  '
827700941121     O          E            TOTAL       1
827800941121     O                       NDC(2)        Z      8
827900941122     O                       DTD(2)              21 '  /  /    '
828000941122     O                       DDC(2)              32 '  /  /    '
828100941121     O                       HRC(2)              43 '  :  '
828200941121     O          E            TOTAL       1
828300941121     O                       NDC(3)        Z      8
828400941122     O                       DTD(3)              21 '  /  /    '
828500941122     O                       DDC(3)              32 '  /  /    '
828600941121     O                       HRC(3)              43 '  :  '
828700941121     O          E            TOTAL       1
828800941121     O                       NDC(4)        Z      8
828900941122     O                       DTD(4)              21 '  /  /    '
829000941122     O                       DDC(4)              32 '  /  /    '
829100941121     O                       HRC(4)              43 '  :  '
829200941121     O          E            TOTAL       1
829300941121     O                       NDC(5)        Z      8
829400941122     O                       DTD(5)              21 '  /  /    '
829500941122     O                       DDC(5)              32 '  /  /    '
829600941121     O                       HRC(5)              43 '  :  '
829700941121     O          E            TOTAL       1
829800941130     O                                           22 'SPEDIZ.CONSEGNATE....:'
829900941130     O                       STPTSC        Z     26
830000941130     O                                           44 'DI CUI PARZIALI.:'
830100941130     O                       STPTPA        Z     47
830200941121     O                                           89 'NR.COLLI:'
830300941121     O                       STPTCL        Z     94
830400941121     O                                          100 'PESO:'
830500941121     O                       STPTPK        3    108
830600941121     O                                          116 'VOLUME:'
830700941121     O                       STPTVL        3    122
830800941121     O                                          128 'STOP:'
830900941121     O                       STPTST        Z    132
831000000505     O          E            TOTAL       1
831100000508     O                                           22 'SPEDIZ.CON ADR CONS..:'
831200000505     O                       STPADR        Z     26
831300000508     O                                           54 'SPEDIZ.CON ADR NON CONS.'
831400000508     O                       STPNDR        Z     59
831500941130     O          E            TOTAL       1  1
831600941130     O                                           22 'SPEDIZ.NON CONSEGNATE:'
831700941130     O                       STPNSC        Z     26
831800941130     O                                           44 'DI CUI NON FATTE:'
831900941130     O                       STPNFA        Z     47
832000941121     O                                           57 'GIACENTI:'
832100941121     O                       STPNGC        Z     60
832200941121     O                                           75 'LASC.AVVISO:'
832300941121     O                       STPNLA        Z     78
832400941121     O                                           89 'NR.COLLI:'
832500941121     O                       STPTNC        Z     94
832600941121     O                                          100 'PESO:'
832700941122     O                       STPTNP        3    108
832800941121     O                                          116 'VOLUME:'
832900941122     O                       STPTNV        3    122
833000990924     O          E            TOTEU       1
833100991007     O                                           16 'TOT.INC.IN EURO:'
833200991007     O                       STPEES        2     30
833300060419     O                                           47 'DI CUI CON.+POS:'
833400991007     O                       STPECO        2     61
833500991007     O                                           69 'ASS.NS:'
833600991008     O                       STPEAN        2     83
833700991007     O                                           96 'ASS.DIRETTI:'
833800991008     O                       STPEAD        2    110
833900991007     O                                          121 'ALTRE DIV:'
834000991008     O                       STPDAD        2    132
834100011010** STT
834200941121                                    ***  C H I U S U R A     D I S
834300941130 T I N T A    C O N S E G N A  ***      FNLR53R           PAG.
834400941130LNP     SPEDIZIONE        COD. E DESCR. CONSEGNA     DESTINATARIO
834500941121                     INCASSATO         COLLI   PESO   VOLUME PRZ R
834600941121------------------------------------------------------------------
834700941121------------------------------------------------------------------
834800920221**  CMDP
834900940415OVRPRTF FILE(QSYSPRT) FORMTYPE(CHIU_DIS) SAVE(*YES) HOLD(*YES)
835000930305**  ERRORI
835100941116Codice mancata consegna inesistente                                    01
835200941116Codice mancata consegna non valido                                     02
835300941116Già effettuata consegna parziale impossibile aprire giacenza           03
835400941116Se merce non consegnata impossibile immettere consegna con anomalie !  04
835500941116Se merce non consegnata impossibile immettere consegna con riserva !   05
835600990923Importo incassato obbligatorio se modificata divisa incasso !          06
835700990423Attenzione ! Modificata mancata consegna                               07
835800991007Nelle righe precedenti presente tipo incasso non congruente con divisa 08
835900941116Cod.giacenza generica incompatibile con particolarità consegna         09
836000000616Codice consegna anomala inesistente o non utilizzabile                 10
836100941116Codice riserva inesistente o errato                                    11
836200941117Obbligatorio firmatario bolla                                          12
836300091221Ora consegna obbligatoria                                              13
836400941117Ora consegna errata !!                                                 14
836500990923Importo parziale incassato ammesso solo per incasso Assegnato          15
836600941117Spedizione non consegnata !! Impossibile immettere firmatario          16
836700941117Data consegna anteriore data spedizione                                17
836800941117Data consegna errata                                                   18
836900941117Attenzione ! Spedizione con data consegna inferiori alla data distinta 19
837000020418Data consegna inferiore oltre il limite ammesso rispetto a oggi        20
837100941117Data consegna superiore data del giorno                                21
837200941117Codice consegna particolare non utilizzabile                           22
837300000522Codice consegna particolare inesistente o non utilizzabile             23
837400941117Attenzione !! Variazione bolla non effettuata per conflitto            24
837500990928Incasso parziale ammesso solo per assegnato !                          25
837600990923Importo incassato superiore al totale da incassare                     26
837700990928Divisa di incasso inesistente                                          27
837800941117Immettere data documento rimborso                                      28
837900941117Data documento rimborso errata                                         29
838000941117Per immettere importo rimborso specificare causale relativa            30
838100941117Per immettere numero documento rimborso specificare causale relativa   31
838200941117Per immettere data documento rimborso specificare causale relativa     32
838300941117Per immettere note rimborso specificare causale relativa               33
838400941118Impossibile gestire incasso sulla riga di totale                       34
838500060419Impossibile immettere ABI/CAB per tipo incasso in contante o pos       35
838600060419Impossibile immettere numero assegno per tipo incasso contante o pos   36
838700060419Impossibile immettere data assegno per tipo incasso contante o pos     37
838800941118Tipo incasso inesistente                                               38
838900941118Tipo incasso valido solo per C/Assegno                                 39
839000990923Tipo incasso non utilizzabile con divisa indicata                      40
839100941118Cliente non abilitato alla gestione del tipo incasso indicato          41
839200120418Numero assegno obbligatorio o errato per il tipo incasso indicato      42
839300941118ABI/CAB Obbligatori                                                    43
839400941121Data assegno errata                                                    44
839500941118ABI/CAB Errato                                                         45
839600991115Imp. incassato valido solo x assegnati o C/A inc.in divisa alternativa 46
839700991007Differenza importi da incassare e incassati in contanti troppo alta !! 47
839800941121Incasso parziale superiore al importo totale assegnato !!              48
839900941121Immettere tipo incasso parziale  !!!                                   49
840000941121Tipo incasso parziale non ammesso !!                                   50
840100941121Tipo incasso parziale inesistente                                      51
840200150601Se tipo incasso C/A è TM/TO/TP, l'assegno deve essere postdatato       52
840300941122Lasciato avviso già emesso in precedenza impossibile reimetterlo       53
840400941125Giorno di chiusura non valido                                          54
840500941128Codice mancata consegna non gestibile da utente !!                     55
840600950208Collo senza data arrivo premi Invio poi F7 per forzare                 56
840700941128No consegnati tutti i colli o nessuno !                                57
840800941128Nessuna consegna parziale da annullare !                               58
840900150601Assegno postdatato utilizzabile solo con tipo incasso TM/TO/TP         59
841000971120Impossibile inserire assegni a noi intestati postdatati                60
841100110419Modificare esito incasso deve essere uguale a N oppure blank           61
841200941205Impossibile gestire consegna parziale: non ci sono colli da consegnare 62
841300950620Puoi ancora modificare la tassazione invece di chiedere N.Accredtito   63
841400970120Obbligatorio immettere descrizione motivo mancata consegna             64
841500970123Impossibile immettere note mancata consegna: spedizione già consegnata 65
841600970311Indicare evento consegna parziale con IDD                              66
841700970304Impossibile gestire pratica IDD: non ci sono colli da consegnare       67
841800970304Nessuna pratica IDD da annullare !                                     68
841900970304Indicare i colli da chiudere con IDD                                   69
842000970311Indicare codice evento IDD                                             70
842100970311Tutti i colli mancanti obbligatorio indicare IDD totale                71
842200140423Codice mancata consegna non valido incongruente con particolarità      72
842300970805Se merce consegnata parzialmente impossibile attivare pratica IDD      73
842400130408Incasso non destinato a mittente o divisa non gestita con multiassegno 74
842500120228Incasso già gestito come multiplo assegno o spedizione                 75
842600120228Incasso non effettuato impossibile gestire opzione                     76
842700141128Spedizione consegnabile solo con PINCODE da PDA !!!                    77
842800141201Codice evento non ammesso Spedizione SENZA il PINCODE !!!              78
842900100205                                                                       79
843000100205                                                                       80
843100100205                                                                       81
843200100205                                                                       82
843300131204Ora di consegna a video non può essere inferiore al primo tentativo    83
843400011010Ora consegna errata ! PER DISTINTA POMERIDIANA IL MINIMO SONO LE 13,00 84
843500011108ATTENZIONE !! LA DATA D'INCASSO E' OLTRE LA DATA SCADENZA DELLA DIVISA 85
843600011108LA SPEDIZIONE NON È INCASSATA I CAMPI DI INCASSO DEVONO ESSERE VUOTI   86
843700020610Non è possibile impostare il numero di STOP a (0) !!!!                 87
843800030318Non è possibile continuare. Esiste già quadratura giornaliera          88
843900031202Evento non inseribile prima di 2 tentativi di consegna                 89
844000040213Data consegna richiesta errata o non inserita                          90
844100040213Data consegna richiesta deve essere superiore alla data odierna        91
844200040330Data consegna richiesta superiore al limite ammesso                    92
844300051215Motivo immesso o proposto non valido la bolla ha Consegna TASSATIVA    93
844400060320Il tipo incasso POS è ammesso solo per consegne a magazziono           94
844500070313Data/ora evento reale errata o non inserita                            95
844600080204Ora/minuti evento errata verificare (08:00 - 19:59)                    96
844700070321Tipo nota non gestibile filiale non abilitata                          97
844800070412Data evento reale non può essere maggiore di oggi                      98
844900070412Data evento reale non può essere inferiore alla spedizione             99
845000070412Data evento reale non corretta fuori dai limiti aziendali              00
845100070517Attenzione è presente una data concordata con il destinatario          01
845200080709Manca Tabella Y4O x incasso POS chiamare la sede Uff.Banche            02
845300080717Incasso dichiarato in contante su PDA ma diverso da originale          03
845400080717Incasso dichiarato con assegno su PDA ma diverso da originale          04
845500090220Presente NOTA AUTISTA visualizzarla per proseguire                     05
845600090622Codice ABI 07601 non ammesso per tipo incasso "BP"                     06
845700091221Attenzione tipo servizio H10:30 orario oltre le 10:30 !!!              07
845800091221Attenzione tipo servizio Priority consegna fuori affidabilità !!!      08
845900130416Tipo incasso non utilizzabile variare il tipo incasso per proseguire   09
846000011010**
846100011010ERRORE IN CHIUSURA DISTINTA
846200011010**
846300011010ATTENZIONE ! La tabella XX utilizzata nella chiusura distinta ha r
846400011010aggiunto la capienza massima ! Correggere il programma FNLR53R inc
846500011010rementando la relativa schiera.

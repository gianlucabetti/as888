000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200940610     H*F UDATER *----------------------------------------------------*
000300111115     H*                                                              *
000400911028     H*             - SELEZIONE GIACENZE IN ARRIVO -                 *
000500940912     H*                                                              *
000600911028     H*  01 SI E' COLLEGATA UNA 2° LIVELLO                           *
000700900418     H*--------------------------------------------------------------*
000800940912     FFNLG80D   CF   E             WORKSTN
000900940914     F                                     SFILE(LG80S01:NRR)
001000050504     FFNART01L  IF   E           K DISK
001100970602     FFNARB01L  UF   E           K DISK
001200950104     F                                     RENAME(FNARB000:FNARB001)
001300940912     FFNLBL01L  IF   E           K DISK
001400050214     Ftigcp21L  IF   E           K DISK
001500110928     Ffnfvv01L  IF   E           K DISK
001600990803     FFNEVB05L  IF A E           K DISK
001700080603     Ffiar401l  IF   E           K DISK
001800130610     Ffiar501l  IF   E           K DISK
001801151201     FTIIDC02L  IF A E           K DISK
001900911010     FTABEL00F  IF   E           K DISK
002000000526     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002100940921     F*--------------------------------------------------------------*
002200020503     D £1              S              3  0 DIM(30)
002300020503     D £6              S              3  0 DIM(30)
002400980216     D C2A             S              3    DIM(500)
002500980216     D D2A             S             75    DIM(500)
002600980216     D G2A             S              1  0 DIM(500)
002700980216     D TP2             S              1    DIM(500)
002800980216     D FLG             S              1    DIM(500)
002900980216     D TPG             S              1    DIM(500)
003000940914     D C7Q             S              2    DIM(50)
003100970529     D C7R             S              2    DIM(50)
003200000526     D FIP             S              3  0 DIM(50)
003300130613     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
003400040806     D K7U             S              7s 0 DIM(999)
003500940921     D MULT            DS            89    OCCURS(50)
003600970529     D MULTR           DS            89    OCCURS(50)
003700040323     D MULTw           DS            89    OCCURS(50)
003701151201     D TRUL33DS      E DS
003800940921     D DS2A          E DS
003900950104     D DS2Z          E DS
004000990729     D DS2G          E DS
004100040806     D DS7U          E DS
004200940914     D DS7Q          E DS
004300940921     D  TIP                   78     82
004400940921     D                                     DIM(5)
004500940921     D  DSP                   83     87
004600940921     D                                     DIM(5)
004700021024      * tabella 7w estensione 7q
004800021024     D DS7W          E DS
004900021024     D  wIP                    1      5
005000021024     D                                     DIM(5)
005100021024     D  wSP                    6     10
005200021024     D                                     DIM(5)
005300021024     D  wca                   11     15
005400021024     D                                     DIM(5)
005500110722     D  tmt                   25     39
005600110722     D                                     DIM(5)
005700970529     D DS7R          E DS
005800970529     D  TGI                   22     26
005900970529     D                                     DIM(5)
006000970529     D  EVE                   32     46
006100970529     D                                     DIM(5)
006200970530     D  TIN                   47     51
006300970530     D                                     DIM(5)
006400970530     D  VIN                   52     56  0
006500970529     D                                     DIM(5)
006600911115     D*------- RECORD TABELLA  "CB"
006700900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
006800000526     D OG143         E DS
006900900418     D CNCR80        E DS
007000900418     D KPJBA         E DS
007100970530     D FNLR53        E DS                  EXTNAME(FNLR53DS3)
007200970530     D FNLG05        E DS                  EXTNAME(FNLG05DS)
007300111115     D DSLR01        E DS
007400020503     d trul06ds      e ds
007500020503     d  lin                    1     90  0 dim(30)
007600941017     D FNLG82        E DS                  EXTNAME(FNLG82DS)
007700040716     d fidn12ds      e ds
007800110921     d fnlr98ds      e ds
007900080603     d dsbl4a        e ds
008000130610     D Dar5gen       E DS
008100141029     D fnlry09ds2    E DS
008200940913     D WLBDA8          DS
008300940913     D  G02DAT                 1      8  0
008400940913     D  G02INV                 9     16  0
008500940913     D  G02ERR                17     17
008600940913     D  G02TGI                18     22  0
008700911010      *
008800911010     D DATPAR          DS
008900940913     D  GIODAT                 1      8  0
009000940913     D  GIOINV                 9     16  0
009100940913     D  GIOTGI                17     21  0
009200911010     D*
009300911021     D                 DS
009400911010     D  §SEI                   1      6
009500911010     D  §PRF                   1      3
009600911010     D  §FLE                   4      6  0
009700940913     D GFES8           DS
009800940913     D  DATADA                 1      8  0
009900940913     D  DATAA                  9     16  0
010000940913     D  GGF                   17     21  0
010100040113     d Wdata8          DS
010200040113     d  dadata                 1      8  0
010300040113     d  adata                  9     16  0
010400040113     d  GioLav                17     21  0
010500040113      *
010600130219     d fnarbds       e ds                  extname(fnarb00f)
010601160224      * DS per richiamo pgm FNLR36R
010602160224     D DSLR36          DS
010603160224     D  P36AAS                14     17  0
010604160224     D  P36LNP                18     20  0
010605160224     D  P36NRS                21     22  0
010606160224     D  P36NSP                23     29  0
010607160224     D  P36F03                30     30
010608160224     D  P36FLG                31     31
010700021025     d param80         ds
010800021025     d  fldlnpa                       3
010900021025     d  fldaasa                       4
011000021025     d  fldnrsa                       2
011100021025     d  fldnspa                       7
011200021025     d  fldcca                        1
011300021029     d  fldgga                        2
011400050505     d savfbc          s                   like(arbfbc)
011401151201     d w0090           s              9  0
011402151201     d kprg            s                   like(idcprg)
011403151201     d $Finenum        s               n   inz(*off)
011405151201     D Esito           s              1a
011409151201     d annocur         s              4s 0
011500940920     D*---------------------------------------------------------------*
011600940920     D*  Costanti per campo messaggio
011700960119     D COST1           C                   CONST('MSG   ')
011800940920     D COST2           C                   CONST('F.D.  ')
011900040113     D COST3           C                   CONST('C.R.  ')
012000050504     D COST4           C                   CONST('PARZ  ')
012001151201      *
012002151201     D trul33R         pr                  extpgm('TRUL33R')
012003151201     D  kpjba                              likeds(kpjba)
012100940912     I*--------------------------------------------------------------*
012200000526     IAZORG01L  AA
012300000526     I                             P    4    5 0ORGFIL
012400000526     I                                  7    7  ORGFAG
012500000526     I                               3776 3800  ORGDE3
012600911115     C*---------------------------------------------------------------*
012700940912     C*  Caricamento videata subfile iniziale
012800940914     C                   EXSR      INZ01
012900940912     C*
013000911010     C     *INKC         DOWEQ     '0'
013100940912     C                   EXSR      GESS01
013200940912     C                   END
013300940912     C*
013400900418     C                   SETON                                        LR
013500900418     C/EJECT
013600940912     C*------------------------------------------------------------*
013700940912     C*  Gestione prima videata
013800940912     C*------------------------------------------------------------*
013900940912     C     GESS01        BEGSR
014000940912     C*
014100940912     C                   WRITE     LG80Z01
014200940912     C                   EXFMT     LG80C01
014300940913     C                   SETOFF                                       28
014400940921     C                   MOVEL     'N'           WSELEZ
014500940912     C* Fine Lavoro
014600940912     C     *INKC         IFEQ      '1'
014700941003     C                   MOVEL     'U'           D05TRI
014800941003     C                   MOVEL     'S'           WSELEZ            1
014900941003     C                   MOVE      KAAS          D05AAS
015000941003     C                   MOVE      KLNP          D05LNP
015100941003     C                   MOVE      KNRS          D05NRS
015200941003     C                   MOVE      KNSP          D05NSP
015300941003     C                   MOVE      'S'           D05RIC
015400941003     C                   MOVEL     FNLG05        KPJBU
015500941003     C                   CALL      'FNLG05C'
015600941003     C                   PARM                    KPJBA
015700940912     C                   GOTO      FINVD1
015800940912     C                   END
015900940913     C* Ritorno
016000940913     C     *INKL         IFEQ      '1'
016100940914     C                   SETOFF                                       02
016200940914     C                   MOVEL     'S'           WGCAPE            1
016300940914     C                   EXSR      INZ01
016400940913     C                   GOTO      FINVD1
016500940913     C                   END
016600940912     C* Cambio Filiale
016700940912     C     *INKS         IFEQ      '1'
016800940912     C                   Z-ADD     V1CFLE        V2CFLE
016900940914     C                   SETOFF                                       402802
017000940914     C                   EXSR      GESD02
017100940914     C                   MOVEL     'S'           WGCAPE            1
017200940914     C  NKL              EXSR      INZ01
017300940912     C                   GOTO      FINVD1
017400940912     C                   END
017500110928     c*verifico se periodo trazioni ridotte se è inserito il numero di distinta
017600110928     c*stand by per parcheggiare bolle G8 o simile x clienti senza gestione reso
017700110928     c                   if        *in66
017800111115     C* ricerca
017900111115     C     '?'           SCAN      V1CNDsba                               31
018000111115     C     *IN31         IFEQ      '1'
018100111115     C                   CLEAR                   DSLR01
018200111115     C                   Z-ADD     oggi          DLRADA
018300111115     C                   Z-ADD     V1Cfle        DLRFGS
018400111115     C                   MOVEL     'FNLG80R'     DLRPGM
018500111115     C                   MOVEL     'S'           DLRRIC
018600111115     C                   MOVEL     '2'           DLRTFV
018700111115     C                   Z-ADD     4             DLRNPG
018800111115     C                   MOVEL     DSLR01        KPJBU
018900111115     C                   CALL      'FNLR02R'
019000111115     C                   PARM                    KPJBA
019100111115     C                   MOVEL     KPJBU         DSLR01
019200111115     C                   MOVE      *ZEROS        V1CNDsba
019300111115     C                   MOVE      DLRNFV        V1CNDsba
019400111115     C                   END
019500111115     C* controlli
019600111115     C                   TESTN                   V1CNDsba             31
019700111115     c                   if        not *in31
019800111115     C                   SETON                                        28
019900111115     C                   eval      $MSG = 'Distinta errata'
020000111115     C                   Z-ADD     *ZEROS        V1cndsb
020100111115     C                   ELSE
020200111115     C                   MOVE      V1CNDsba      V1cndsb           7 0
020300111115     C                   END
020400111115     C                   END
020500111115     C*  Controllo il numero di distinta se diverso da zero
020600111115     C*  deve esistere in archivio flarb00 o flarb50
020700111115     C*
020800110928     c                   if        v1cndsb > 0
020900110928     c                   z-add     v1cfle        fgs
021000110928     c                   z-add     v1cndsb       nfv
021100110928     c     kfvv          chain     fnfvv01l
021200110928     c                   if        not %found(fnfvv01l) or fvvfcf <> *blank
021300110928     c                             or fvvatb <> *blank
021400110928     c                   seton                                        28
021500110928     C                   MOVEL     ERR(3)        $MSG
021600110928     c                   goto      finvd1
021700110928     c                   endif
021800110928     c                   endif
021900940912     C* Causali
022000940912     C     *INKB         IFEQ      '1'
022100960216     C                   MOVEL     'N'           WRICAR            1
022200940914     C                   SETON                                        02
022300940912     C                   MOVEL     'N'           WGCAPE
022400940914     C                   EXSR      INZ01
022500960216     C     WRICAR        IFEQ      'S'
022600960216     C                   SETOFF                                       02
022700960216     C                   MOVEL     'S'           WGCAPE            1
022800960216     C                   EXSR      INZ01
022900960216     C                   SETON                                        28
023000960216     C                   MOVEL     ERR(1)        $MSG
023100960216     C                   END
023200940912     C                   GOTO      FINVD1
023300940912     C                   END
023400940912     C* Controlli
023500940912     C                   EXSR      CTRD1
023600940921     C* Se ho selezionato una o più spedizioni ricarico il subfile
023700940921     C     WSELEZ        IFEQ      'S'
023800940921     C                   EXSR      INZ01
023900940921     C                   END
024000940912     C*
024100940912     C     FINVD1        ENDSR
024200940913     C*------------------------------------------------------------*
024300940913     C*  Gestione seconda videata
024400940913     C*------------------------------------------------------------*
024500940913     C     GESD02        BEGSR
024600940913     C*
024700940913     C*  Fino a che non do CMD12 o cod.filiale esatto loop seconda videata
024800940913     C     *INKL         DOUEQ     '1'
024900940913     C     *IN28         OREQ      '0'
025000940913     C                   EXFMT     LG80D02
025100940913     C                   SETOFF                                       2840
025200940913     C* Ritorno
025300940913     C     *INKL         IFEQ      '0'
025400940913     C                   SETOFF                                       32
025500940914     C     V2CFLE        IFNE      0
025600940914     C     V2CFLE        LOOKUP    £1                                     32
025700940913     C                   END
025800940913     C     *IN32         IFEQ      '0'
025900940913     C                   SETON                                        2840
026000940913     C                   MOVEL     ERR(2)        $MSG
026100940913     C                   ELSE
026200940913     C                   Z-ADD     V2CFLE        V1CFLE
026300940914     C                   EXSR      CAR£6
026400940913     C                   END
026500940913     C                   END
026600940913     C*
026700940913     C                   END
026800940913     C*
026900940913     C                   ENDSR
027000940912     C*------------------------------------------------------------*
027100940912     C*  Controlli prima videata
027200940912     C*------------------------------------------------------------*
027300940912     C     CTRD1         BEGSR
027400940912     C*
027500940912     C                   READC     LG80S01                                29
027600940912     C     *IN29         DOWEQ     '0'
027700940914     C     V1CSEL        IFEQ      '1'
027800140923     C     V1CSEL        oreq      '2'
027801151202     C     V1CSEL        oreq      'R'
027802160224     C     V1CSEL        oreq      '7'
027803160224      * visualizzazione bolle
027804160224     C     v1csel        ifeq      '7'
027808160224     c                   exsr      richbl
027809160224     c                   move      *blank        v1csel
027810160224     c                   update    lg80s01
027811160224     c                   goto      nextr
027812160224     c                   endif
027813160224      * controlli
027900110722     c                   if        v1cnoge = '*'
028000110722     c                   eval      $msg = 'Codice motivo giacenza non a-
028100110722     c                             mmesso con questa particolarità'
028200110722     c                   seton                                        28
028300110722     c                   goto      nextr
028400110722     c                   endif
028500140923     C     V1CSEL        IFEQ      '1'
028600140923     c                   if        v1hdis <> *blank
028700140923     c                   eval      $msg = 'Presente disposizione di consegna -
028800141008     c                             in sospeso eseguire prima opzione 2'
028900140923     c                   seton                                        28
029000140923     c                   goto      nextr
029100140923     c                   endif
029200140923     c                   endif
029300140923     C     V1CSEL        IFEQ      '2'
029400140923     c                   if        v1hdis = *blank
029500140924     c                   eval      $msg = 'Opzione 2 non possibile non presen-
029600140924     c                             ti disposizioni di consegna'
029700140923     c                   seton                                        28
029800140923     c                   goto      nextr
029900140923     c                   endif
030000140923     c                   endif
030001151201      * per eseguire reso diretto nuova opzione R
030002151201     C     V1CSEL        IFEQ      'R'
030004151201     c                   if        v1cdis <> 'R'
030005151201     c                   eval      $msg = 'Richiesto Reso diretto in bolla -
030006151201     c                             senza disposizione di reso'
030007151201     c                   seton                                        28
030008151201     c                   goto      nextr
030009151201     c                   endif
030010151201     c                   endif
030100940914     C                   CLEAR                   FNLG05
030200940912     C* CONTROLLO SE LA SPEDIZIONE HA UNA MAMMA : IN QUESTO CASO SE LA MAMMA
030300940912     C* ERA GIACENTE RIAPRO LA GIACENZA DELLA MAMMA
030400940912     C                   MOVE      V1CAAS        KAAS
030500940912     C                   MOVE      V1CLNP        KLNP
030600940912     C                   MOVE      V1CNRS        KNRS
030700940912     C                   MOVE      V1CNSP        KNSP
030800940912     C     KLBL          CHAIN     FNLBL01L                           30
030900940912     C     *IN30         IFEQ      '0'
031000940912     C     LBLLAN        ANDEQ     LBLLAP
031100940912     C* VERIFICO CHE LA BOLLA MAMMA ESISTA
031200940912     C                   MOVE      LBLAAP        KAAS
031300940912     C                   MOVE      LBLLPP        KLNP
031400940912     C                   MOVE      LBLNRP        KNRS
031500940912     C                   MOVE      LBLNSP        KNSP
031600950102     C                   MOVEL     V1CCMC        D05CMC
031700040324     C                   MOVEL     VhCCMC        D05CMCh
031800950613     C                   MOVE      V1CDDC        D05DXD
031900940920     C                   MOVE      LBLAAN        D05AAF
032000940914     C                   MOVE      LBLLPN        D05LNF
032100940914     C                   MOVE      LBLNRN        D05NRF
032200940914     C                   MOVE      LBLNSN        D05NSF
032300940912     C                   END
032400050214     C     KGCA          CHAIN     tigcp21l                           35
032500940912     C* CONTROLLO SE LA SPEDIZIONE NON FOSSE GIA' GIACENTE
032600940912     C     *IN35         IFEQ      '0'
032700940914     C                   MOVE      GCPAGC        D05AGC
032800940914     C                   MOVE      GCPFGC        D05FGC
032900940914     C                   MOVE      GCPNGC        D05NGC
033000940914     C                   MOVE      GCPFRG        D05FRG
033100941017     C                   MOVE      GCPAGC        D82AGC
033200941017     C                   MOVE      GCPFGC        D82FGC
033300941017     C                   MOVE      GCPNGC        D82NGC
033400941017     C                   MOVE      GCPFRG        D82FRG
033500940912     C                   END
033600140923      *scelta opzione 2
033700140923     C     V1CSEL        IFEQ      '2'
033800141029     c                   clear                   fnlry09ds2
033900140923     c                   eval      ilry09tch = 'E'
034000140923     c                   eval      ilry09aas = KAAS
034100140923     c                   eval      ilry09lnp = KLNP
034200140923     c                   eval      ilry09nrs = KNRS
034300140923     c                   eval      ilry09nsp = KNSP
034400140923     c                   exsr      verdispo
034500140923     C                   MOVEL     'S'           WSELEZ
034600140923     c                   goto      nextr
034700140923     c                   endif
034800941017     C* Se devo aprire giacenza richiamo il pgm. FNLG05R se non devo
034900941017     C* aprire giacenza ma devo eseguire disposizioni richiamo FNLG20R
035000941017     C     V1CDIS        IFEQ      ' '
035100960402     C     *IN35         OREQ      '0'
035200960605     C     V1CMSG        OREQ      COST1
035300050419     C     KARB          CHAIN(n)  FNARB01L                           34
035400070301      * verifico se dopo che ho caricato il sfl è stata modificata la
035500070301      * DCR in questo caso non tratto il rek ma passo al successivo
035600070507      * il controllo viene effettuato solo sel il flag tipo dcr è <>'P'
035700950124     C                   MOVEL     'S'           WSELEZ            1
035800950104     C                   MOVE      D82MOB        D05MOB
035900950104     C                   MOVE      D82PRB        D05PRB
036000950104     C                   MOVE      D82MOE        D05MOE
036100950104     C                   MOVE      D82PRE        D05PRE
036200950104     C                   MOVE      D82PSS        D05PSS
036300030716     c                   eval      d05mdb4 = d82mdb4
036400030716     c                   eval      d05mdb5 = d82mdb5
036500030716     c                   eval      d05psl = d82psl
036600030716     c                   eval      d05bsl = d82bsl
036700030716     c                   eval      d05prb4 = d82prb4
036800030716     c                   eval      d05prb5 = d82prb5
036900941003     C                   MOVEL     SAVTRI        D05TRI
037000940914     C                   MOVE      KAAS          D05AAS
037100940914     C                   MOVE      KLNP          D05LNP
037200940914     C                   MOVE      KNRS          D05NRS
037300940914     C                   MOVE      KNSP          D05NSP
037400940920     C                   MOVE      'S'           D05RIC
037500050316     C                   MOVE      'S'           D05comm
037600040114      * se codice mancata consegna uguale a codice di dft lo passo a pgm
037700040303     c                   if        v1ccmc = §2gmdr or v1ccmc = §2gadr or
037800040303     c                             v1ccmc = '016' or v1ccmc = '023'
037900040114     c                   move      v1ccmc        d05cmc
038000040114     c                   endif
038100040324     c                   move      vhccmc        d05cmch
038200040114
038300940914     C                   MOVEL     FNLG05        KPJBU
038400940920     C                   CALL      'FNLG05C'
038500940912     C                   PARM                    KPJBA
038600941003     C                   MOVEL     ' '           SAVTRI            1
038700941017     C                   ELSE
038800950104     C     KARB          CHAIN     FNARB01L                           34
038900950104     C     *IN34         IFEQ      '0'
039000050505     C                   MOVEL     ARBFBC        savfbc
039101160218      *se reso con dispo aggiorno a vuoto per non sporcare flag altrimenti
039102160218      * non viene gestito da assegnazione automatica
039103160218      *
039104160218     c                   if        v1cdis = 'R' and v1csel = '1'
039105160218     C                   EXCEPT    liber
039106160218     c                   else
039107160218     C                   MOVEL     ' '           ARBFBC
039200950104     C                   EXCEPT    AGGBLO
039201160218     C                   Endif
039300950104     C                   END
039400021030     C                   MOVEL     'S'           WSELEZ            1
039500021025     c                   if        v1cdis = 'C'
039600021025     c                   movel     arblnp        fldlnpa
039700021025     c                   movel     arbaas        fldaasa
039800021025     c                   movel     arbnrs        fldnrsa
039900021025     c                   movel     arbnsp        fldnspa
040000021025     c                   movel     v1ccca        fldcca
040100021029     c                   movel     arbgga        fldgga
040200021025     c                   movel(p)  param80       kpjbu
040300021025     c                   call      'FNLG80R1'
040400021025     c                   parm                    kpjba
040500021025     c                   else
040600111115     c                   if        v1cdis = 'Y'
040700111115     c                   if        *in66 and v1cndsb = 0
040800111115     c                   eval      $msg = 'Per gestire bolle da ins-
040900111115     c                             erire in dist.inserire il numero'
041000111115     c                   seton                                        28
041100111115     C                   MOVEL     'N'           WSELEZ
041200111115     c                   goto      nextr
041300111115     c                   endif
041400170517     c*M                 if        *in66 and v1cndsb > 0
041500170517     C*m   KARB          CHAIN     FNARB01L                           34
041600170517     C*m   *IN34         IFEQ      '0'
041700170517     C*m                 MOVEL     ARBFBC        savfbc
041800170517     C*m                 MOVEL     ' '           ARBFBC
041900170517     C*m                 EXCEPT    AGGBLO
042000170517     C*m                 END
042100110921     c                   clear                   fnlr98ds
042200110921     c                   z-add     arbaas        i98aas
042300110921     c                   z-add     arblnp        i98lnp
042400110921     c                   z-add     arbnsp        i98nsp
042500110921     c                   z-add     arbnrs        i98nrs
042600110921     c                   z-add     arbifp        i98fgs
042700110921     c                   z-add     4             i98npg
042800110921     c                   z-add     v1cndsb       i98ndc
042900110921     c                   move      'M'           i98delins
043000110921     c                   move      'N'           i98video
043100110922     c                   z-add     v1cfle        i98fgs
043200110921     c                   movel     fnlr98ds      kpjbu
043300110921     c                   call      'FNLR98C'
043400110921     c                   parm                    kpjba
043500110921     c                   movel     kpjbu         fnlr98ds
043600110921      *esito negativo riblocco la bolla
043700110921     c                   if        o98esito <> '0'
043800110921     C     KARB          CHAIN     FNARB01L                           34
043900110921     C                   MOVEL     savfbc        arbfbc
044000110921     C  N34              EXCEPT    AGGBLO
044100111116     c                   eval      $msg = o98msgerr
044200111116     c                   seton                                        28
044300111116     C                   MOVEL     'N'           WSELEZ
044400111116     c                   goto      nextr
044500110921     c                   endif
044600170517     c*m                 endif
044700110921     c                   else
044701151201     c                   if        v1cdis = 'R' and v1csel = '1'
044702151201     c                   exsr      repnum
044703151201     c                   eval      idcprg = kprg
044704151216     c                   move      ora           idcinsdata
044705151216     c                   movel     oggi          idcinsdata
044707151201     c                   eval      idcmodacc = 'B'
044708151201     c                   eval      idctipocli= 'M'
044709151201     c                   eval      IDCWAAS   = kaas
044710151201     c                   eval      IDCWLNP   = klnp
044711151201     c                   eval      IDCWNRS   = knrs
044712151201     c                   eval      IDCWNSP   = knsp
044713151201     c                   eval      IDCAAS    = kaas
044714151201     c                   eval      IDCLNP    = klnp
044715151201     c                   eval      IDCNRS    = knrs
044716151201     c                   eval      IDCNSP    = knsp
044717151202     c                   eval      IDCFGS    = v1cfle
044718151201     c                   eval      IDCTIPODIS = '5'
044719151201     c                   eval      IDCNEWLNA=*zero
044720151201     c                   eval      IDCFLA   ='R'
044721151201     c                   eval      IDCELAFLG='S'
044722151201     c                   eval      IDCELAUTE=knmus
044723151201     c                   write     tiidc000
044724151203     C                   MOVEL     'S'           WSELEZ
044725151203     c                   goto      nextr
044726151201     c                   else
044800941017     C                   MOVEL     'A'           D82GES
044900941017     C                   MOVEL     'N'           D82GIA
045000950118     C                   MOVE      V1CFLE        D82FGS
045100950118     C                   MOVE      KAAS          D82AAS
045200941017     C                   MOVE      KLNP          D82LNP
045300941017     C                   MOVE      KNRS          D82NRS
045400941017     C                   MOVE      KNSP          D82NSP
045500941017     C                   MOVE      V1CDIS        D82DIS
045600970529     C                   MOVE      '0'           D82F12
045700941017     C                   MOVEL     FNLG82        KPJBU
045800941017     C                   CALL      'FNLG20R'
045900941017     C                   PARM                    KPJBA
046000970204     C                   MOVEL     KPJBU         FNLG82
046100970203     C*  Se premuto F12 riblocco la bolla
046200970203     C     D82F12        IFEQ      '1'
046300970204     C     KARB          CHAIN     FNARB01L                           34
046400050505     C                   MOVEL     savfbc        arbfbc
046500970204     C  N34              EXCEPT    AGGBLO
046600940912     C                   END
046601151201     c                   endif
046700110921     c                   endif
046800970530     C*
046900021025     c                   endif
047000970203     C                   END
047100941017     C*
047200941017     C                   END
047300940912     C*
047400970606     C     NEXTR         TAG
047500940912     C                   READC     LG80S01                                29
047600940912     C                   END
047700940912     C*
047800940912     C                   ENDSR
047801160224     C**************************************************************************
047802160224     C*  Routine per gestione richiamo interrogazione bolle
047803160224     C**************************************************************************
047804160224     C     richbl        begsr
047805160224      *
047806160224     c                   movel     kpjbu         §kpjbu          256
047807160224      *
047808160224     c                   z-add     v1caas        p36aas
047809160224     c                   z-add     v1clnp        p36lnp
047810160224     c                   z-add     v1cnrs        p36nrs
047811160224     c                   z-add     v1cnsp        p36nsp
047812160224     c                   movel     ' '           p36f03
047813160224     c                   movel     '1'           p36flg
047814160224     c                   movel     dslr36        kpjbu
047815160224     c                   call      'FNLR36R'
047816160224     c                   parm                    kpjba
047817160224     c                   movel     kpjbu         dslr36
047818160224      *
047819160224     c                   movel     §kpjbu        kpjbu
047820160224      *
047821160224     c                   endsr
047900940912     C*---------------------------------------------------------------*
048000911017     C* CARICAMENTO SUBFILE
048100940913     C*---------------------------------------------------------------*
048200940913     C     INZ01         BEGSR
048300940913     C* Pulizia Subfile
048400141014     c                   setoff                                       64
048500940913     C                   EXSR      PULIZ
048600130613      *reperisco giorni standard  e
048700130613      * imposto il valore standard  ma se è specificato
048800130613      * un numero di giorni diverso sulla particolarità calcolo con quello
048900130613     C                   Z-ADD     1             X
049000130613     C     'F  '         LOOKUP    C2A(X)                                 32
049100130613     c   32              z-add     g2a(x)        giodft            2 0
049200130613     c  n32              seton                                        28
049300130613     C  n32              MOVEL     ERR(4)        $MSG
049400130613     c  n32              goto      esci
049500940913     C* Verifico se la filiale ne controlla altre
049600040113 1   C                   DO        30            I                 2 0
049700040113 2   c                   if        £6(i) = 0
049800020503     c                   leave
049900040113e2   c                   endif
050000940914     C                   MOVE      £6(I)         KLNA
050100940913     C* Caricamento delle bolle
050200130215     C/EXEC SQL
050300130215     C+ Declare C1 Cursor for
050400130219     c+ SELECT *  FROM fnarb00f WHERE arblna = :klna and
050500130219     c+ arbdcm = 0 and
050600130219     c+  ((arbgma in (SELECT tblkey FROM tabel00f
050700130219     c+ WHERE substr(tbluni, 65, 1)= 'R' and tblcod ='7R' and tblkut = 1)
050800130219     c+ and arbfbc = ' ')  or arbcmc <> ' '
050900130215     c+ or arbffd = 'S' and arbfbc = ' '
051000130215     c+ or arbdcr > 0  and arbfbc = ' ' )
051100130219     c+ ORDER BY arblna, arbaas, arblnp, arbnrs, arbnsp
051200130215     C/END-EXEC
051300130215      *
051400130215     C/EXEC SQL
051500130215     C+ OPEN C1
051600130215     C/END-EXEC
051700130215      *
051800130419     c                   do        *hival
051900130215     C/EXEC SQL
052000130215     C+ Fetch Next From c1 Into :fnarbds
052100130215     C/END-EXEC
052200130419     C                   if        SqlCod = 100 OR SqlCod < 0
052300130419     c                   leave
052400130419     c                   endif
052401151201      *verifico già dalla lettura se la bolla che sto leggendo ha delle dispo
052402151201      * di reso inserite in questo caso non carico la bolla nel sfl
052403151201      *
052404151201     c                   exsr      verdispoR
052405151201     c                   if        olry09err = *blank
052406151201     c                   if        olry09esdi = '1' and
052407151201     c                             olry09reso = '1'
052408151201     c                   iter
052409151201     c                   endif
052410151201     c                   endif
052500061020      * forzatura in caso di presenza di data consegna richiesta e flag P
052600061020      * "PRIMA DEL" viene forzato a 0 il campo ARBDCR per permettere il
052700061020      * corretto svolgimento di eventuali particolarità con tempi di attesa
052800061020      * da lasciato avviso ad esempi particolarità C4 e C6
052900061020      * ATTENZIONE il campo non viene aggiornato fisicamente
053000130215     c                   clear                   scritta           1
053100130215     c                   clear                   parz              1
053200061020     c                   if        arbdcr > oggi and arbtcr = 'P'
053300061020     c                   z-add     0             arbdcr
053400061020     c                   endif
053500061020
053600000526     C     ARBLNP        LOOKUP    FIP                                    99
053700040113 3   C     *IN99         IFEQ      *OFF
053800970530     C                   MOVEL     *BLANKS       WTRGMA            1
053900970530     C                   MOVEL     *BLANKS       WEVGMA            3
054000970530     C                   MOVEL     *BLANKS       WTPGMA            1
054100040806     C                   MOVEL     'N'           WTREVE
054200950104     C                   CLEAR                   DS7Q
054300990203     C* Verifico che non esistano CA aperte per proporre la sped.
054400990203     C* fra le giacenze da aprire
054500990203     C                   EXSR      CTRDAN
054600940913     C* Verifico che non sia bolla di rientro
054700040113 4   C     ARBFBR        IFNE      'S'
054800940922     C     ARBNDC        ANDEQ     0
054900990203     C     WDANNI        ANDEQ     'N'
055000950104     C* Controllo se devo cercare evento di attesa apertura
055100040113 5   C     ARBGGA        IFNE      *BLANKS
055200950104     C                   Z-ADD     1             Y
055300950104     C     ARBGGA        LOOKUP    C7Q(Y)                                 32
055400040113 6   C     *IN32         IFEQ      '1'
055500950104     C     Y             OCCUR     MULT
055600950104     C                   MOVE      MULT          DS7Q
055700021024     C     Y             OCCUR     MULTw
055800040323     C                   movel     MULTw         DS7w
055900040113 7   C     §7Q2EV        IFNE      *BLANKS
056000960119     C                   MOVE      §7Q2EV        WCEV              3
056100960220     C                   Z-ADD     1             X
056200950104     C     §7Q2EV        LOOKUP    C2A(X)                                 32
056300040113 8   C     *IN32         IFEQ      '1'
056400960626     C                   EXSR      CTREVE
056500960626     C  N33              MOVEL     'S'           WTREVE            1
056600040113e8   C                   END
056700040113e7   C                   END
056800040113e6   C                   END
056900040113e5   C                   END
057000950104     C*
057100970530     C* Controlli per particolarità giacenza
057200040113 5   C     WTREVE        IFEQ      'N'
057300970530     C     ARBGMA        ANDNE     *BLANKS
057400040113e5   C                   EXSR      CTRGMA
057500970530     C                   END
057600970530     C*
057700040113 5   C     WTREVE        IFEQ      'N'
057800940920     C* Verifico se sto esaminando un fermo deposito
057900130605     C* Gestione particolarità consegna
058000130614     C     WTRGMA        ifEQ      'S'
058100130614 7   C     ARBGMA        ifne      *BLANKS
058200130605     C                   EXSR      GESGMA
058300130605x7   C                   Endif
058400130614     c                   else
058500040113 6   C     ARBFFD        IFEQ      'S'
058600130611 6   C     ARBFFD        oreq      'M'
058700130611     C     ARBCMC        ANDEQ     *BLANKS
058800970530     C                   EXSR      GESFFD
058900130614x6   C                   else
059000130614     C* Controllo se cod.mancata cons. prevede giac. posticipata
059100130614     C                   EXSR      CALGGA
059200040113e7   C                   END
059300130614e7   C                   endif
059400040223      *se arrivo da chiusura distinta quindi evento di lasciato avviso
059500040223      * con data consegna richiesta
059600050502     c                   if        arbtmc = 'A' and arbdcr > 0
059700040223     c                             and scritta = ' ' and arbdcr > oggi
059800050502     c                             and arbtcr <> 'P' and
059900050502     c                             (arbama > 1 OR arbdam >= oggi)
060000050502     c
060100040223     c                   exsr      datarich
060200040223     c                   endif
060300950104     C*
060400040113x5   C                   ELSE
060500040113 6   C     GIOINV        IFLT      OGGI
060600950104     C     WGCAPE        ANDEQ     'S'
060700950104     C     GIOINV        ORGE      OGGI
060800950104     C     WGCAPE        ANDEQ     'N'
060900040323     C     arbtmc        ANDEQ     'G'
061000950104     C     *IN31         OREQ      '1'
061100950104     C                   EXSR      WTR
061200040113e6   C                   END
061300040113e5   C                   END
061400040113e4   C                   END
061500040113e3   C                   END
061600040114     c                   if        arbdcr > 0 and arbtcr <> 'P'
061700040114     c                             and scritta = ' ' and arbdcr > oggi
061800050502     c                             and arbtmc <> 'A' and
061900050502     c                             (arbama > 1 OR arbdam >= oggi)
062000040113     c                   exsr      datarich
062100040113     c                   end
062200050504     c* particolarità consegna: NO CONSEGNA PARZIALE
062300050504     c* se la bolla non è mai stata consegnata perchè parziale e non è
062400050504     c* già giacente la propongo come reso  se trascorsi 7 gg dalla
062500050504     c* arrivo del 1° collo
062600050504     c                   if        scritta = ' ' and arbndc = 0 and
062700050504     c                             arbdcm = 0 and arbdcp = 0 and
062800050504     c                             arbcmc = ' ' and arbfbc = ' '
062900050504     c                             and §7R8cp = 'N' and arbgma <> ' '
063000050530     c                             and wgcape = 'S' and wdanni = 'N'
063100050504     c                   exsr      srparz
063200050504     c                   end
063300950104     C*
063400130215 e2  C                   END
063500130215     C/EXEC SQL
063600130215     C+ Close c1
063700130215     C/END-EXEC
063800040113 e1  C                   END
063900130613     c     esci          tag
064000911028     C     NRR           IFEQ      0
064100911028     C                   ADD       1             NRR
064200940914     C   KB              SETOFF                                       02
064300960216     C   KB              MOVEL     'S'           WRICAR
064400940914     C   KL              SETON                                        02
064500940914     C                   WRITE     LG80S01
064600911028     C                   ELSE
064700940913     C                   SETOFF                                       28
064800940913     C                   SETON                                        20
064900911028     C                   END
065000940914     C*
065100911010     C                   ENDSR
065200970530     C*----------------------------------------------------------*
065300050504     C* srparz  particolarità consegna : no parziale
065400970530     C*----------------------------------------------------------*
065500050504     C     srparz        BEGSR
065600050504     c* verifico se è una consegna parziale dai segnacolli
065700050504     c     karb          setll     fnart01l
065800050504     c                   do        *hival
065900050504     c     karb          reade     fnart01l
066000050504     c                   if        %eof(fnart01l)
066100050504     c                   leave
066200050504     c                   end
066300050504     c                   if        artdam = 0
066400050504     c                   eval      parz = '1'
066500050504     c                   leave
066600050504     c                   end
066700050504     c                   enddo
066800050504      *controllo differenza fra la data arrivo 1° collo e oggi
066900050530     c                   if        parz = '1' and arbdam > 0
067000050506     c                   move      arbdam        dadata
067100050506     c                   move      oggi          adata
067200050504     c                   CALL      'XSRLAV8'
067300050504     c                   PARM                    Wdata8
067400050504     c                   if        §7r8gg <> *blanks
067500050504     c                   move      §7r8gg        w7r8gg
067600050504     c                   else
067700050504     c                   clear                   w7r8gg            2 0
067800050504     c                   end
067900050504     c                   if        giolav <> 0 and giolav >= w7r8gg
068000050504     c                   exsr      wtr
068100050504     c                   endif
068200050504     c                   endif
068300050504     C*
068400050504     C                   ENDSR
068500050504     C*----------------------------------------------------------*
068600050504     C* GESFFD: GESTIONE FERMO DEPOSITO
068700050504     C*----------------------------------------------------------*
068800050504     C     GESFFD        BEGSR
068900970530     C*
069000130610      * rekord GEN riferimento data inserimento fermo deposito
069100130610     C                   clear                   dar5gen
069200130610     C                   move      'GEN'         trcar5
069300130610     C     Kfiar5        CHAIN     FiAR501L
069400130610     C                   IF        %found(fiar501l)
069500130610     C                   MOVEL     ar5uni        dar5gen
069600130610     C                   endif
069700130610     c                   z-add     0             nostandard        2 0
069800130613     c                   z-add     giodft        numgio            2 0
069900130610     c                   if        arbgma <> *blank and
070000130610     c                             (§7rggfd <> *blank and §7rggfd <> *zero)
070100130610     c                   move      §7rggfd       nostandard
070200130610     c                   move      §7rggfd       numgio
070300130610     c                   endif
070400130613     c                   add       1             numgio
070500130610      *
070600040419      *se è un f.d. con giacenza aperta e la fase è 35 non la ripropongo da
070700040419      * aprire
070800050210      * a meno che non siano passati 2 giorni dall'esecuzione delle disposizioni
070900050210      *
071000050214     C     KGCA          CHAIN     tigcp21L                           35
071100040419     C     *IN35         IFEQ      '0'
071200040419     c     gcpfas        andeq     35
071300050214     c                   move      gcpdeda       dadata
071400050210     c                   move      oggi          adata
071500050210     c                   CALL      'XSRLAV8'
071600050210     c                   PARM                    Wdata8
071700130610     c                   if        giolav > numgio and wgcape ='S'
071800050210     c                   exsr      wtr
071900050210     c                   else
072000040419     c                   goto      endrut
072100050210     c                   endif
072200050210
072300040419     c                   else
072400970530     C     WGCAPE        IFEQ      'S'
072500130610     c                   move      arbdti        arbdtiw           8 0
072600130610     c                   if        §ar5dfd <> *blank
072700130610     c                   move      §ar5dfd       war5dfd           8 0
072800130610     c                   if        war5dfd > arbdtiw
072900130610     c                   move      war5dfd       arbdtiw           8 0
073000130610     c                   endif
073100130610     c                   endif
073200130610     C     ARBDTIw       ifne      0
073300130610     C                   Z-ADD     ARBDTIw       DATADA
073400970530     C                   Z-ADD     OGGI          DATAA
073500990906     C                   CALL      'XGGFEST'
073600970530     C                   PARM                    GFES8
073700970530     C*
073800970530     C                   Z-ADD     DATADA        G02INV
073900970530     C                   MOVE      '3'           G02ERR
074000970530     C                   CALL      'XSRDA8'
074100970530     C                   PARM                    WLBDA8
074200130610     c                   if        nostandard > 0
074300130610     C     G02TGI        ADD       nostandard    GIOTGI
074400130610     c                   else
074500130613     C     G02TGI        ADD       giodft        GIOTGI
074600130610     C*  Se il collo è arrivato dopo le 18 aggiungo uno al giorno
074700130610     C     ARBHTI        IFGT      1800
074800130610     C                   ADD       1             GIOTGI
074900130610     C                   END
075000130610     c                   endif
075100970530     C                   ADD       GGF           GIOTGI
075200970530     C*
075300970530     C                   CALL      'XSRGI8'
075400970530     C                   PARM                    DATPAR
075500970530     C*
075600970530     C     GIOINV        IFLT      OGGI
075700970530     C                   EXSR      WTR
075800970530     C                   END
075900970530     C                   END
076000970530     C                   END
076100970530     C*
076200040419     c                   endif
076300040419     C     endrut        ENDSR
076400970530     C*----------------------------------------------------------*
076500970530     C* GESGMA: GESTIONE PARTICOLARITA' CONSEGNA
076600970530     C*----------------------------------------------------------*
076700970530     C     GESGMA        BEGSR
076800970530     C*
076900970530     C* Calcolo gg. attesa evento solo se ARBCMC <> evento giacenza
077000970530     C     ARBCMC        IFNE      WEVGMA
077100970530     C                   Z-ADD     1             X
077200970530     C     ARBCMC        LOOKUP    C2A(X)                                 32
077300970530     C     *IN32         IFEQ      '1'
077400970530     C     G2A(X)        ANDNE     0
077500970530     C     TP2(X)        ANDEQ     'A'
077600970530     C                   MOVEL     C2A(X)        WCEV
077700970530     C                   EXSR      CTREVE
077800970530     C     *IN33         IFEQ      '1'
077900970530     C                   Z-ADD     0             GIOINV
078000970530     C                   END
078100970530     C                   END
078200970530     C*
078300970530     C                   ELSE
078400970530     C                   Z-ADD     0             GIOINV
078500970530     C                   END
078600970530     C*
078700970530     C* LASCIATO AVVISO + L.AVVISO --> CALCOLO GIORNI DA 1° TENTATIVO
078800970530     C     WTPGMA        IFEQ      'A'
078900970530     C     TP2(X)        ANDEQ     'A'
079000051222      * se presente per doppio lasciato avviso una data cons.rich maggiore
079100051222      * della udate forzo per il confronto la dcr in modo da demandare
079200051222      * il controllo x l'apertura della giacenza alla routine specifica
079300051222     c                   if        arbdcr > oggi
079400051222     c                   z-add     arbdcr        wdtgma
079500051222     c                   endif
079600970530     C                   Z-ADD     WDTGMA        GIOINV
079700970530     C                   END
079800970530     C* LASCIATO AVVISO + GIACEZA --> CALCOLO GIORNI DA 1° TENTATIVO
079900970530     C     WTPGMA        IFEQ      'A'
080000970530     C     TP2(X)        ANDEQ     'G'
080100970530     C                   Z-ADD     WDTGMA        GIOINV
080200970530     C                   END
080300970530     C* GIACENZA + L.AVVISO --> CALCOLO GIORNI DAL PRIMO TENTATIVO
080400970530     C* SOLO SE DATA PARTICOLARITA' CONSEGNA > DATA LASCIATO AVVISO
080500970530     C     WTPGMA        IFEQ      'G'
080600970530     C     TP2(X)        ANDEQ     'A'
080700970530     C     WDTGMA        ANDGT     GIOINV
080800970530     C                   Z-ADD     WDTGMA        GIOINV
080900970530     C                   END
081000970530     C* GIACENZA + GIACENZA ---> RENDO SUBITO
081100970602     C     WTPGMA        IFEQ      'G'
081200970602     C     TP2(X)        ANDEQ     'G'
081300970602     C     ARBCMC        IFNE      WEVGMA
081400970602     C                   Z-ADD     0             GIOINV
081500970602     C                   ELSE
081600970602     C                   Z-ADD     WDTGMA        GIOINV
081700970602     C                   END
081800970602     C                   END
081900970530     C*
082000970530     C* CALCOLO se aprire o no giacenza
082100970530     C     GIOINV        IFLT      OGGI
082200970530     C     WGCAPE        ANDEQ     'S'
082300041117     C     GIOINV        orge      OGGI
082400041117     C     WGCAPE        ANDEQ     'N'
082500970530     C                   EXSR      WTR
082600970530     C                   END
082700970530     C*
082800970530     C                   ENDSR
082900970530     C*----------------------------------------------------------*
083000970530     C* CALGGA: CALCOLO ATTESA EVENTO GIACENZA
083100970530     C*----------------------------------------------------------*
083200970530     C     CALGGA        BEGSR
083300970530     C*
083400040323     c                   if        arbcmc <> *blank
083500970530     C                   Z-ADD     1             X
083600970530     C     ARBCMC        LOOKUP    C2A(X)                                 32
083700970530     C     *IN32         IFEQ      '1'
083800970530     C     G2A(X)        ANDNE     0
083900970530     C     FLG(X)        ANDEQ     '§'
084000970530     C     *IN32         OREQ      '1'
084100970530     C     G2A(X)        ANDNE     0
084200970530     C     TP2(X)        ANDEQ     'A'
084300051222     C     arbdcr        andle     OGGI
084400970530     C                   MOVEL     C2A(X)        WCEV
084500970530     C                   EXSR      CTREVE
084600970530     C     GIOINV        IFLT      OGGI
084700970530     C     WGCAPE        ANDEQ     'S'
084800970530     C     GIOINV        ORGE      OGGI
084900970530     C     WGCAPE        ANDEQ     'N'
085000040323     C     arbtmc        ANDEQ     'G'
085100970530     C     *IN33         OREQ      '1'
085200040323     C     arbtmc        ANDEQ     'G'
085300970530     C                   EXSR      WTR
085400970530     C                   END
085500970530     C                   ELSE
085600970530     C                   Z-ADD     OGGI          GIOINV
085700970530     C     WGCAPE        IFEQ      'S'
085800050830     c     arbdcr        andlt     oggi
085900040325     C     WGCAPE        orEQ      'S'
086000040325     c     arbdcr        andge     oggi
086100040325     c     arbtcr        andeq     'P'
086200050830     C     WGCAPE        orEQ      'S'
086300050830     c     arbdcr        andeq     oggi
086400050830     c     arbtcr        andne     'D'
086500970530     C                   EXSR      WTR
086600970530     C                   END
086700970530     C                   END
086800970530     C*
086900040323     C                   endif
087000970530     C                   ENDSR
087100040113     C*----------------------------------------------------------*
087200040113     C* datarich  apertura giacenza per consegna richiesta oltre limite
087300040113     C*----------------------------------------------------------*
087400040113     C     datarich      BEGSR
087500040510     c                   if        wgcape = 'S' and arbfbr <> 'S'
087600040421     c                   if        arbgga <> *blank and §7q4d1 = 'R'and
087700040421     c                             arbntc = 0
087800040421     c                   goto      bypass
087900040421     c                   end
088000110527     c                   if        arbgga <> *blank and §7wca1 = 'S'
088100110527     c*                  if        arbgga <> *blank and §7wca1 = 'S' or
088200110527     c*                            arbgga <> *blank and §7w7ca1 = 'S'
088300040421     c                   goto      bypass
088400040421     c                   end
088500040113     C*
088600040113      *controllo differenza fra le date il numero dei giorni lavorativi
088700150311      *in caso che non sia stato fatto un tentativo di consegna non elaboro la
088800150311      *differenza da udate ma da arbdam(arrivo ultimo collo)
088900150311     c                   move      oggi          dadata
089000150311     c                   if        arbdam > oggi
089100150311     c                   move      arbdam        dadata
089200150311     c                   endif
089300150311      *
089400040114     c                   move      arbdcr        adata
089500040113     c                   CALL      'XSRLAV8'
089600040113     c                   PARM                    Wdata8
089700060307     c                   z-add     §2gldr        limit             2 0
089800060307      * verifico se il cliente ha un limite di apertura diverso dallo
089900060307      * standard
090000060307      *tenta con KSC
090100060307     c                   movel     arbksc        kkey7u
090200060309     c     key7u         chain     tabel00f
090300060307     c                   if        %found(tabel00f)
090400060307     c                   movel     tbluni        ds7u
090500060307     c                   if        §7uldcr > '00'
090600060307     c                   move      §7uldcr       limit
090700060307     c                   endif
090800060307     c                   else
090900060307      *tenta con CCM
091000060309     c                   movel     arbccm        kkey7u
091100060309     c     key7u         chain     tabel00f
091200060307     c                   if        %found(tabel00f)
091300060307     c                   movel     tbluni        ds7u
091400060307     c                   if        §7uldcr > '00'
091500060307     c                   move      §7uldcr       limit
091600060307     c                   endif
091700060307     c                   endif
091800060307     c                   endif
091900060307      *
092000060307     c                   if        giolav <> 0 and giolav > limit
092100040114     c                   move      'X'           richiesta         1
092200040113     c                   exsr      wtr
092300070419     c                   else
092400070419      * se data inferiore ai limiti per la giacenza abblenco i campi per
092500070419      * poter ritrattare la bolla alla consegna sbloccandola
092600070419     c     karbb         chain     fnarb01l
092700070419     c                   if        %found(fnarb01l)
092800070419     c                   clear                   arbfbc
092900070419     c                   clear                   arbcmc
093000070419     c                   update    fnarb001
093100040113     c                   endif
093200070419     c                   endif
093300040113     C*
093400040325     c                   endif
093500040421     C     bypass        ENDSR
093600950104     C*----------------------------------------------------------*
093700950104     C* Controllo esistenza evento
093800950104     C*----------------------------------------------------------*
093900950104     C     CTREVE        BEGSR
094000950104     C* Se la bolla è una figlia mi posiziono sugli eventi con la
094100950104     C* spedizione mamma
094200950104     C                   Z-ADD     ARBAAS        KAAS
094300950104     C                   Z-ADD     ARBLNP        KLNP
094400950104     C                   Z-ADD     ARBNRS        KNRS
094500950104     C                   Z-ADD     ARBNSP        KNSP
094600950104     C     KLBL          CHAIN     FNLBL01L                           30
094700950104     C     *IN30         IFEQ      '0'
094800950104     C     LBLLAN        ANDEQ     LBLLAP
094900950104     C* VERIFICO CHE LA BOLLA MAMMA ESISTA
095000990803     C                   MOVE      LBLAAP        KAAS
095100950104     C                   MOVE      LBLLPP        KLNP
095200950104     C                   MOVE      LBLNRP        KNRS
095300950104     C                   MOVE      LBLNSP        KNSP
095400950104     C                   END
095500950104     C* Posizionamento su file degli eventi x reperire data
095600950104     C* tentata consegna
095700950104     C                   Z-ADD     *HIVAL        KDEV
095800070117     C                   clear                   gioinv
095900990803     C     KEVB1         SETLL     FNEVB05L
096000990803     C     KEVB2         READPE    FNEVB05L                               33
096100960119     C  N33WCEV          COMP      EVBCEV                             3333
096200950104     C     *IN33         IFEQ      '0'
096300950104     C                   Z-ADD     EVBDEV        DATADA
096400950104     C                   Z-ADD     0             GGF
096500950104     C     G2A(X)        IFNE      0
096600950104     C                   Z-ADD     OGGI          DATAA
096700990906     C                   CALL      'XGGFEST'
096800950104     C                   PARM                    GFES8
096900950104     C                   END
097000950104     C*
097100950104     C                   Z-ADD     DATADA        G02INV
097200950104     C                   MOVE      '3'           G02ERR
097300950104     C                   CALL      'XSRDA8'
097400950104     C                   PARM                    WLBDA8
097500950104     C     G02TGI        ADD       G2A(X)        GIOTGI
097600950104     C                   ADD       GGF           GIOTGI
097700950104     C                   CALL      'XSRGI8'
097800950104     C                   PARM                    DATPAR
097900950104     C                   END
098000950104     C*
098100950104     C*
098200950104     C                   ENDSR
098300990203     C*----------------------------------------------------------*
098400990203     C* Controllo che non esista una CA aperta
098500990203     C*----------------------------------------------------------*
098600990203     C     CTRDAN        BEGSR
098700990203     C* Se la bolla è una figlia cerco la pratica CA sulla mamma
098800990203     C                   MOVEL     'N'           WDANNI            1
098900990203     C                   Z-ADD     ARBAAS        KAAS
099000990203     C                   Z-ADD     ARBLNP        KLNP
099100990203     C                   Z-ADD     ARBNRS        KNRS
099200990203     C                   Z-ADD     ARBNSP        KNSP
099300990203     C     KLBL          CHAIN     FNLBL01L                           30
099400990203     C     *IN30         IFEQ      '0'
099500990203     C     LBLLAN        ANDEQ     LBLLAP
099600990203     C* VERIFICO CHE LA BOLLA MAMMA ESISTA
099700990203     C                   MOVE      LBLAAP        KAAS
099800990203     C                   MOVE      LBLLPP        KLNP
099900990203     C                   MOVE      LBLNRP        KNRS
100000990203     C                   MOVE      LBLNSP        KNSP
100100990203     C                   END
100200990203     C* Posizionamento su file danni
100300990203     C                   Z-ADD     *HIVAL        KDEV
100400050214      * ricerca CA SIA COME MAMMA CHE COME FIGLIA                            ALL
100500040716     c                   clear                   fidn12ds
100600040716     c                   eval      i12aas = kaas
100700040716     c                   eval      i12lnp = klnp
100800040716     c                   eval      i12nrs = knrs
100900040716     c                   eval      i12nsp = knsp
101000040716     c                   eval      i12fel = 'E'
101100040716     c                   eval      i12fan = 'E'
101200040716     c                   eval      i12fch = 'E'
101300040716     c                   call      'FIDN12R'
101400040716     c                   parm                    fidn12ds
101500040716      **
101600040716      * se c'è almeno una C.A. aperta
101700040716      **
101800040716     c     o12nca        ifgt      0
101900990203     C                   MOVEL     'S'           WDANNI
102000000405     C* ... Se non c' è una pratica di danno aperta non consento
102100000405     C*     comunque di riaprire giacenza se c'è stata una prec.
102200000405     C*     chiusura per apertura anomalia
102300000405     C                   ELSE
102400050214     C     KGCA          CHAIN     tigcp21L                           35
102500000405     C     *IN35         IFEQ      '0'
102600000405     C     GCPCFG        ANDEQ     '015'
102700000405     C                   MOVEL     'S'           WDANNI
102800990203     C                   END
102900000405     C                   END
103000990203     C*
103100990203     C                   ENDSR
103200970530     C*----------------------------------------------------------*
103300970530     C* Controllo esistenza evento PARTICOLARITA' CONSEGNA
103400970530     C*----------------------------------------------------------*
103500970530     C     GMAEVE        BEGSR
103600970530     C* Se la bolla è una figlia mi posiziono sugli eventi con la
103700970530     C* spedizione mamma
103800970530     C                   Z-ADD     ARBAAS        KAAS
103900970530     C                   Z-ADD     ARBLNP        KLNP
104000970530     C                   Z-ADD     ARBNRS        KNRS
104100970530     C                   Z-ADD     ARBNSP        KNSP
104200970530     C     KLBL          CHAIN     FNLBL01L                           30
104300970530     C     *IN30         IFEQ      '0'
104400970530     C     LBLLAN        ANDEQ     LBLLAP
104500970530     C* VERIFICO CHE LA BOLLA MAMMA ESISTA
104600990803     C                   MOVE      LBLAAP        KAAS
104700970530     C                   MOVE      LBLLPP        KLNP
104800970530     C                   MOVE      LBLNRP        KNRS
104900970530     C                   MOVE      LBLNSP        KNSP
105000970530     C                   END
105100970530     C* Posizionamento su file degli eventi x reperire data
105200970530     C* tentata consegna
105300970530     C                   Z-ADD     *HIVAL        KDEV
105400990803     C     KEVB1         SETLL     FNEVB05L
105500990803     C     KEVB2         READPE    FNEVB05L                               33
105600970530     C     *IN33         DOWEQ     '0'
105700970530     C     WEVGMA        ANDNE     EVBCEV
105800990803     C     KEVB2         READPE    FNEVB05L                               33
105900970530     C                   END
106000970530     C* SE TROVATO EVENTO PARTICOLARITA' GIACENZA  CALCOLO
106100970530     C* DATA FINALE
106200970530     C     *IN33         IFEQ      '0'
106300970530     C                   SETOFF                                       33
106400970530     C                   Z-ADD     1             X
106500970530     C     EVBCEV        LOOKUP    C2A(X)                                 32
106600970530     C                   MOVEL     TP2(X)        WTPGMA            1
106700970530     C                   END
106800970530     C     *IN33         IFEQ      '0'
106900970530     C                   Z-ADD     EVBDEV        DATADA
107000970530     C                   Z-ADD     0             GGF
107100970530     C     G2A(X)        IFNE      0
107200970530     C                   Z-ADD     OGGI          DATAA
107300990906     C                   CALL      'XGGFEST'
107400970530     C                   PARM                    GFES8
107500970530     C                   END
107600970530     C*
107700970530     C                   Z-ADD     DATADA        G02INV
107800970530     C                   MOVE      '3'           G02ERR
107900970530     C                   CALL      'XSRDA8'
108000970530     C                   PARM                    WLBDA8
108100970530     C                   Z-ADD     0             GIOINV
108200970530     C     G02TGI        ADD       G2A(X)        GIOTGI
108300970530     C                   ADD       GGF           GIOTGI
108400970530     C                   CALL      'XSRGI8'
108500970530     C                   PARM                    DATPAR
108600970530     C                   Z-ADD     GIOINV        WDTGMA            8 0
108700970530     C                   MOVEL     'S'           WTRGMA
108800970530     C                   END
108900970530     C*
109000970530     C*
109100970530     C                   ENDSR
109200960626     C*----------------------------------------------------------*
109300960626     C* Controllo esistenza evento PARTICOLARITA'
109400960626     C*----------------------------------------------------------*
109500960626     C     EVEPAR        BEGSR
109600960626     C*
109700960626     C                   MOVEL     'N'           WDOPOA
109800960626     C* Se non ho trovato evento di attesa mi posiziono sull'ultima
109900960626     C* apertura e controllo se ce n'è uno dopo questa
110000960626     C     WTREVE        IFEQ      'N'
110100960626     C                   Z-ADD     0             KDEV
110200960626     C* Se una riapertura controllo se è già stato immesso un
110300960626     C* evento di attesa successivo ad essa
110400960626     C     *IN35         IFEQ      '0'
110500960626     C                   Z-ADD     GCPDUR        KDEV
110600960626     C                   END
110700960626     C*  Mi posiziono sugli eventi successivi alla data consegna per
110800960626     C*  verificare se ci sono degli eventi di attesa
110900990803     C     KEVB1         SETLL     FNEVB05L
111000990803     C     KEVB2         READE     FNEVB05L                               33
111100960626     C     *IN33         DOWEQ     '0'
111200960626     C     WCEV          ANDNE     EVBCEV
111300990803     C  N33KEVB2         READE     FNEVB05L                               33
111400960626     C                   END
111500960626     C* se dopo l'ultima apertura è stato emesso un evento di giac.
111600960626     C* non devo più accettarne un altro
111700960626     C     WCEV          IFEQ      EVBCEV
111800960626     C                   MOVEL     'S'           WDOPOA            1
111900960626     C                   END
112000960626     C*
112100960626     C                   END
112200960626     C*
112300960626     C                   ENDSR
112400970530     C*----------------------------------------------------------*
112500970530     C* CREO EVENTO ATTESA 032 E LO COMUNICO A WURTH
112600970530     C*----------------------------------------------------------*
112700970530     C     CTRG09        BEGSR
112800970530     C*
112900970530     C* Decodifico fermo deposito
113000970530     C                   Z-ADD     1             FF                3 0
113100970530     C     '032'         LOOKUP    C2A(FF)                                36
113200970530     C     *IN36         IFEQ      '1'
113300970530     C                   MOVEL     D2A(FF)       §2ADES
113400970530     C                   END
113500970530     C* Richiamo FNLR53r3
113600970530     C                   CLEAR                   FNLR53
113700970530     C                   MOVEL     *BLANKS       D53RIC
113800970530     C                   MOVEL     'N'           D53VIS
113900970530     C                   Z-ADD     ARBAAS        D53AAS
114000970530     C                   Z-ADD     ARBLNP        D53LNP
114100970530     C                   Z-ADD     ARBNRS        D53NRS
114200970530     C                   Z-ADD     ARBNSP        D53NSP
114300970530     C                   MOVEL     'G'           D53TMC
114400970530     C                   MOVEL     '032'         D53CMC
114500970530     C                   MOVEL     §2ADES        D53DMC
114600970530     C                   MOVEL     ARBGMA        D53GMA
114700970530     C                   MOVEL     *BLANKS       D53EXC
114800970530     C                   MOVEL     *BLANKS       D53NT1
114900970530     C                   MOVEL     *BLANKS       D53NT2
115000970530     C                   MOVEL     'G'           D53TR1
115100970530     C                   MOVEL     'H'           D53TR2
115200970530     C                   MOVEL     FNLR53        KPJBU
115300970530     C                   CALL      'FNLR53R3'
115400970530     C                   PARM                    KPJBA
115500970530     C                   MOVEL     KPJBU         FNLR53
115600970530     C*  Se non ci sono errori aggiorno campi subfile
115700970530     C     D53ERR        IFEQ      *BLANKS
115800970530     C     D53F12        ANDEQ     *BLANKS
115900970602     C                   MOVEL     'S'           WSELEZ            1
116000970530     C                   CALL      'FNLR53R4'
116100970530     C                   PARM                    KPJBA
116200970602     C* Provvedo aggiornamento FNARB
116300970602     C                   MOVE      V1CAAS        KAAS
116400970602     C                   MOVE      V1CLNP        KLNP
116500970602     C                   MOVE      V1CNRS        KNRS
116600970602     C                   MOVE      V1CNSP        KNSP
116700050419     C     KARB          CHAIN(n)  FNARB01L                           30
116800970602     C                   Z-ADD     OGGI          ARBDDC
116900970602     C                   Z-ADD     1             ARBNTC
117000970602     C                   MOVEL     'G09'         ARBCMC
117100970602     C     §2AFTG        IFEQ      'S'
117200970602     C                   MOVE      §2AFTC        ARBFBC
117300970602     C                   END
117400970606     C                   MOVEL     'G'           ARBTMC
117500970602     C                   UPDATE    FNARB001
117600970602     C* Verifico se bolla figlia
117700970602     C                   MOVE      V1CAAS        KAAS
117800970602     C                   MOVE      V1CLNP        KLNP
117900970602     C                   MOVE      V1CNRS        KNRS
118000970602     C                   MOVE      V1CNSP        KNSP
118100970602     C     KLBL          CHAIN     FNLBL01L                           30
118200970602     C     *IN30         IFEQ      '0'
118300970602     C     LBLLAN        ANDEQ     LBLLAP
118400970602     C                   MOVE      LBLAAP        EVBAAS
118500970602     C                   MOVE      LBLLPP        EVBLNP
118600970602     C                   MOVE      LBLNRP        EVBNRS
118700970602     C                   MOVE      LBLNSP        EVBNSP
118800970602     C                   ELSE
118900970602     C                   Z-ADD     V1CLNP        EVBLNP
119000970602     C                   Z-ADD     V1CAAS        EVBAAS
119100970602     C                   Z-ADD     V1CNRS        EVBNRS
119200970602     C                   Z-ADD     V1CNSP        EVBNSP
119300970602     C                   END
119400970602     C* Scrivo evento G09
119500970602     C                   MOVEL     'G09'         EVBCEV
119600970602     C                   MOVEL     *BLANKS       EVBATB
119700970602     C                   Z-ADD     OGGI          EVBDTV
119800970602     C                   TIME                    EVBORV
119900970602     C                   Z-ADD     V1CFLE        EVBFLE
120000970602     C                   Z-ADD     999           EVBCDU
120100970602     C* Sulla bolla metto i riferimenti della mamma se esiste legame
120200970602     C                   Z-ADD     OGGI          EVBDEV
120300970602     C                   MOVEL     EVBORV        EVBHEV
120400970602     C                   MOVEL     *BLANKS       EVBFTR
120500970602     C                   Z-ADD     0             EVBDTR
120600990803     C                   MOVEL     *BLANKS       EVBNOT
120700990803     C                   Z-ADD     0             EVBDTC
120800970602     C*
120900990803     C                   WRITE     FNEVB000
121000970530     C*
121100970602     C                   END
121200970602     C*
121300970530     C                   ENDSR
121400940913     C*----------------------------------------------------------*
121500940913     C* Caricamento tabella £6 e £1
121600940913     C*----------------------------------------------------------*
121700940914     C     CAR£6         BEGSR
121800940913     C* Caricamento £6
121900020503     c                   clear                   trul06ds
122000020503     c                   eval      d06cod = '£6'
122100020503     c                   movel     v1cfle        d06key
122200020503     c                   movel     trul06ds      kpjbu
122300020503     c                   call      'TRUL06R'
122400020503     c                   parm                    kpjba
122500020503     c                   movel     kpjbu         trul06ds
122600020503     c                   movea     lin           £6
122700940913     C                   ENDSR
122800911017     C*-----------------------------*
122900911017     C* SCRITTURA RECORD NEL SUBFILE
123000911017     C*-----------------------------*
123100911010     C     WTR           BEGSR
123200940921     C*
123300080603     c                   exsr      vediddt
123400110722     c                   clear                   v1cnoge
123500050413     c                   clear                   v1cmsg
123600040114     c                   move      '1'           scritta
123700940921     C                   MOVEL     *BLANKS       V1CDIS
123800950104     C                   MOVEL     TPG(X)        WTPGIA            1
123900940914     C                   SETOFF                                       03
124000051107     C***  ARBFFD        IFEQ      'S'
124100051107     C     ARBFFD        IFne      ' '
124200950224     C     ARBCMC        ANDEQ     *BLANKS
124300940920     C                   MOVEL     COST2         V1CMSG
124400940920     C                   SETON                                        03
124500940922     C                   END
124600040113     C* Controllo se devo proporre C.R.
124700040324     C* ed eventuali motivi di forzati o di default
124800040114     c                   if        richiesta <> *blank
124900040113     C                   MOVEL     COST3         V1CMSG
125000040323     c                   if        arbtmc = 'A'
125100040302     c                   select
125200040302     c                   when      arbcmc = 'A16'
125300040302     c                   move      '016'         v1ccmc
125400040302     c                   when      arbcmc = 'A23'
125500040302     c                   move      '023'         v1ccmc
125600040302     c                   other
125700040213     c                   movel     §2gadr        v1ccmc
125800040302     c                   endsl
125900040302     c                   endif
126000040323     c                   if        arbtmc <> 'A'
126100040114     C                   MOVEL     §2gmdr        V1Ccmc
126200040212     c                   endif
126300040114     c                   move      *blank        richiesta
126400040113     C                   SETON                                        03
126500040224     c                   else
126600040324      * per i motivi a16 e a23 forza
126700040324      * rispettivi motivi di giacenza altrimenti imposta motivo da ARB
126800040324     c                   select
126900040324     c                   when      arbcmc = 'A16'
127000040324     c                   move      '016'         v1ccmc
127100040324     c                   when      arbcmc = 'A23'
127200040324     c                   move      '023'         v1ccmc
127300040324     c                   other
127400040324     c                   move      arbcmc        v1ccmc
127500040324     c                   endsl
127600040113     C                   END
127700040324     c                   move      arbcmc        vhccmc
127800070426      * imposta Hidden il campo DCR se è una data da considerare flag <> P
127900070426     c                   if        arbtcr = 'P'
128000070426     c                   clear                   vhcdcr
128100070426     c                   else
128200070301     c                   move      arbdcr        vhcdcr
128300070426     c                   endif
128400940921     C*  Controllo se devo visualizzare messaggio o gestire esecuz.disp.
128500940914     C     ARBGGA        IFNE      *BLANKS
128600960403     C                   MOVE      ARBAAS        KAAS
128700960403     C                   MOVE      ARBLNP        KLNP
128800960403     C                   MOVE      ARBNRS        KNRS
128900960403     C                   MOVE      ARBNSP        KNSP
129000960403     C     KLBL          CHAIN     FNLBL01L                           30
129100960403     C     *IN30         IFEQ      '0'
129200960403     C     LBLLAN        ANDEQ     LBLLAP
129300960403     C* VERIFICO CHE LA BOLLA MAMMA ESISTA
129400960403     C                   MOVE      LBLAAP        KAAS
129500960403     C                   MOVE      LBLLPP        KLNP
129600960403     C                   MOVE      LBLNRP        KNRS
129700960403     C                   MOVE      LBLNSP        KNSP
129800960403     C                   END
129900050214     C     KGCA          CHAIN     tigcp21L                           35
130000960403     C* CONTROLLO SE LA SPEDIZIONE NON FOSSE GIA' GIACENTE
130100960403     C     *IN35         IFEQ      '1'
130200110722     C                   Z-ADD     1             Y
130300110722     c                   if        arbcmc <> *blank
130400110722     c     arbcmc        lookup    tmt(y)                                 32
130500110722     c   32              move      '*'           v1cnoge
130600110722     c                   endif
130700940921     C*  Controllo se devo gestire esecuzione disposiz.
130800940921     C                   Z-ADD     1             Y                 2 0
130900940921     C     WTPGIA        LOOKUP    TIP(Y)                                 32
131000940921     C     *IN32         IFEQ      '1'
131100950118     C     WTPGIA        ANDNE     ' '
131200130611     C     ARBNTC        ifNE      0
131300051107     C***  ARBFFD        ORNE      'S'
131400051107     C     ARBFFD        OReq      ' '
131500940921     C                   MOVEL     DSP(Y)        V1CDIS
131600970606     C                   END
131700940927     C                   ELSE
131800940927     C                   Z-ADD     1             Y                 2 0
131900940927     C     'T'           LOOKUP    TIP(Y)                                 32
132000940927     C     *IN32         IFEQ      '1'
132100130611     C     ARBNTC        ifNE      0
132200051107     C***  ARBFFD        ORNE      'S'
132300051107     C     ARBFFD        OReq      ' '
132400940927     C                   MOVEL     DSP(Y)        V1CDIS
132500940927     C                   END
132600970606     C                   END
132700021024     c                   if        v1cdis = *blank
132800021024     C*  Controllo se devo gestire esecuzione disposiz.
132900021024     C                   Z-ADD     1             Y                 2 0
133000021024     C     WTPGIA        LOOKUP    WIP(Y)                                 32
133100021024     C     *IN32         IFEQ      '1'
133200021024     C     WTPGIA        ANDNE     ' '
133300021024     C                   MOVEL     wSP(Y)        V1CDIS
133400021025     C                   MOVEL     wca(Y)        V1Ccca
133500021024     C                   ELSE
133600021024     C                   Z-ADD     1             Y                 2 0
133700021024     C     'T'           LOOKUP    WIP(Y)                                 32
133800021024     C     *IN32         IFEQ      '1'
133900021024     C                   MOVEL     wSP(Y)        V1CDIS
134000021025     C                   MOVEL     wca(Y)        V1Ccca
134100021024     C                   END
134200021024     C                   END
134300021024     c                   endif
134400940921     C                   END
134500940921     C*  Controllo se devo emettere messaggio
134600940922     C     §7Q2MF        IFEQ      'A'
134700940921     C     §7Q2MS        ANDNE     *BLANKS
134800960626     C*  Verifico se esiste un evento di attesa dopo apert.
134900960626     C                   EXSR      EVEPAR
135000950105     C     WTREVE        IFEQ      'N'
135100960626     C     WDOPOA        ANDEQ     'N'
135200950105     C     GIOINV        ORGE      OGGI
135300960626     C     WTREVE        ANDEQ     'S'
135400940921     C                   MOVEL     COST1         V1CMSG
135500940921     C                   SETON                                        03
135600940921     C                   END
135700950105     C                   END
135800940920     C                   END
135900940920     C*
136000960403     C                   END
136100050504     c* consegna parziale
136200050504     c                   if        parz = '1'
136300050504     c                   eval      v1cdis=§7R8di
136400050504     C                   MOVEL     COST4         V1CMSG
136500050504     C                   SETON                                        03
136600050504     c                   end
136700960403     C*
136800990729     C* Se sono nel periodo di esclusione dei rientri automatici
136900040806     C* non emetto il record del subfile per la selezione
137000040806      * prima però verifico se il cliente vuole il reso anche in questo
137100040806      * periodo
137200040806     c                   setoff                                       88
137300040806     C     V1CDIS        IFeq      'R'
137400040806     C     OGGI          andge     §2GDPI
137500040806     C     OGGI          andle     §2GDPF
137600110921     c                   seton                                        66
137700040806     C     arbksc        lookup    k7u                                    88
137800040806      * se non l'ho trovato con KSC provo con CCM
137900040806     c  n88arbccm        ifne      *zeros
138000040806     C     arbccm        lookup    k7u                                    88
138100040806     c                   endif
138200110921      * forzo scrittura per spedizioni da rendere ma che in periodo di trazioni
138300110921      * ridotte devono essere forzate in distinta stand by
138400110921     c                   if        not *in88
138500111115     c                   move      'Y'           v1cdis
138600110921     c                   endif
138700040806     c                   endif
138800040806      * in ogni caso devo proporre l'apertura solo se la bolla non è in dist
138900040806     c     arbndc        ifeq      0
139000040806      *
139100990729     C     V1CDIS        IFNE      'R'
139200990729     C     OGGI          ORLT      §2GDPI
139300990729     C     OGGI          ORGT      §2GDPF
139400040806     C     *in88         OReq      '1'
139500940913     C                   Z-ADD     ARBLNP        V1CLNP
139600940913     C                   Z-ADD     ARBAAS        V1CAAS
139700940914     C                   Z-ADD     ARBNRS        V1CNRS
139800940913     C                   Z-ADD     ARBNSP        V1CNSP
139900940913     C                   MOVEL     ARBRSM        V1CRSM
140000940913     C                   MOVEL     ARBRSD        V1CRSD
140100940914     C                   Z-ADD     0             G02DAT
140200940914     C                   Z-ADD     ARBDDC        G02INV
140300940914     C                   MOVE      '3'           G02ERR
140400940914     C                   CALL      'XSRDA8'
140500940914     C                   PARM                    WLBDA8
140600940914     C                   Z-ADD     G02DAT        V1CDDC
140700140923     c                   clear                   v1hdis
140800140923     c                   setoff                                       65
140900141029     c                   clear                   fnlry09ds2
141000151204     c                   eval      ilry09tch = 'T'
141100140923     c                   eval      ilry09fgs = v1cfle
141200140923     c                   eval      ilry09aas = arbaas
141300140923     c                   eval      ilry09lnp = arblnp
141400140923     c                   eval      ilry09nrs = arbnrs
141500140923     c                   eval      ilry09nsp = arbnsp
141600140923     c                   exsr      verdispo
141700141029     c                   if        OLRY09ESDI = '0'
141800911011     C                   ADD       1             NRR               4 0
141900940914     C                   WRITE     LG80S01
142000141029     c                   endif
142100990729     C                   END
142200990729     C*
142300040806      * endif arbndc=0
142400040806     c                   endif
142500040806      *
142600900509     C                   ENDSR
142700140923     c*-----------------------------------------------------------------
142800140923     c     verdispo      begsr
142900140923     c*-----------------------------------------------------------------
143000140923     c                   eval      ilry09fgs = v1cfle
143100141029     c                   movel     fnlry09ds2    kpjbu
143200140923     c                   call      'FNLRY09R'
143300141013     c                   parm                    kpjba
143400141029     c                   movel     kpjbu         fnlry09ds2
143500140923     c                   if        olry09err = *blank
143600141029     c                   if        olry09esal = '1'
143700141014     c                   seton                                        6564
143800140923     c                   move      '1'           v1hdis
143900140923     c                   endif
144000140923     c                   endif
144100140923     C                   ENDSR
144101151201     c*-----------------------------------------------------------------
144102151201     c     verdispoR     begsr
144103151201     c*-----------------------------------------------------------------
144104151201     c                   clear                   fnlry09ds2
144105151204     c                   eval      ilry09tch = 'T'
144106151201     c                   eval      ilry09fgs = v1cfle
144107151201     c                   eval      ilry09aas = arbaas
144108151201     c                   eval      ilry09lnp = arblnp
144109151201     c                   eval      ilry09nrs = arbnrs
144110151201     c                   eval      ilry09nsp = arbnsp
144111151201     c                   movel     fnlry09ds2    kpjbu
144112151201     c                   call      'FNLRY09R'
144113151201     c                   parm                    kpjba
144114151201     c                   movel     kpjbu         fnlry09ds2
144121151201     C                   ENDSR
144200080603     c*-----------------------------------------------------------------
144300080603     c     vediddt       begsr
144400080603     c*-----------------------------------------------------------------
144500080603      * Flag DDT SI  ar4 trek A
144600080603     c                   clear                   v1cddt
144700080603     c                   clear                   dsbl4a
144800080603     c                   move      'A'           trek4
144900080603     c     kar4          chain     fiar401l
145000080603     c                   if        %found(fiar401l)
145100080603     c                   movel     AR4NOT        dsbl4a
145200080603     c                   if        §B4ABM = 'S' or §B4ABM = 'P' or
145300080603     c                             §B4ABM = 'C' or §B4ABM = 'Q'
145400080603     c                   eval      v1cddt = 'S'
145500080603     C                   endif
145600080603     C                   endif
145700080603     c                   endsr
145800911017     C*----------------*
145900911010     C* PULIZIA SUBFILE
146000911017     C*----------------*
146100911010     C     PULIZ         BEGSR
146200940913     C                   SETOFF                                       2021
146300940914     C                   WRITE     LG80C01
146400911010     C                   Z-ADD     0             NRR
146500940913     C                   SETON                                        21
146600940913     C                   MOVEL     *BLANKS       V1CSEL
146700940913     C                   MOVE      *ZEROS        V1CLNP
146800940913     C                   MOVE      *ZEROS        V1CAAS
146900940913     C                   MOVE      *ZEROS        V1CNRS
147000940913     C                   MOVE      *ZEROS        V1CNSP
147100940913     C                   MOVE      *BLANKS       V1CRSM
147200940913     C                   MOVE      *BLANKS       V1CRSD
147300940913     C                   MOVE      *ZEROS        V1CDDC
147400940913     C                   MOVE      *BLANKS       V1CCMC
147500040324     C                   MOVE      *BLANKS       VhCCMC
147600070301     C                   MOVE      *zeros        VhCdcr
147700970529     C                   ENDSR
147800970529     C*------------------------------------------------------------*
147900970530     C*  CTRGMA: Controllo codice particolarità consegna
148000970529     C*------------------------------------------------------------*
148100970530     C     CTRGMA        BEGSR
148200970606     C*
148300970529     C* Controllo se particolarità consegna prevede attesa xx
148400970529     C* giorni e comunicazione al cliente
148500970529     C                   Z-ADD     1             SS                3 0
148600970530     C     ARBGMA        LOOKUP    C7R(SS)                                36
148700970529     C     *IN36         IFEQ      '1'
148800970529     C     SS            OCCUR     MULTR
148900970529     C                   MOVEL     MULTR         DS7R
149000970529     C* Se è previsto l'invio al cliente del motivo mancata consegna
149100970529     C* do il tempo di spedirlo
149200970530     C                   MOVEA     TIN           WTPINV            5
149300970529     C* Reperisco eventi di attesa da l.avviso no standard
149400970529     C     §7R3EV        IFNE      *BLANKS
149500050105     C     §7r3pu        andne     *BLANKS
149600970530     C                   MOVEL     §7R3EV        WEVGMA
149700970530     C                   EXSR      GMAEVE
149800970530     C     WTRGMA        IFEQ      'S'
149900970530     C                   GOTO      FINGMA
150000970529     C                   END
150100970529     C                   END
150200970529     C* Reperisco eventi attesa da cod.evento proposto x riconsegnare
150300970529     C     §7R4EV        IFNE      *BLANKS
150400050105     C     §7r4pu        andne     *BLANKS
150500970530     C                   MOVEL     §7R4EV        WEVGMA
150600970530     C                   EXSR      GMAEVE
150700970530     C     WTRGMA        IFEQ      'S'
150800970530     C                   GOTO      FINGMA
150900970529     C                   END
151000970529     C                   END
151100970529     C* Reperisco eventi attesa prima di aprire giacenza
151200970529     C     §7R5EV        IFNE      *BLANKS
151300050105     C     §7r5pu        andne     *BLANKS
151400970530     C                   MOVEL     §7R5EV        WEVGMA
151500970530     C                   EXSR      GMAEVE
151600970530     C     WTRGMA        IFEQ      'S'
151700970530     C                   GOTO      FINGMA
151800970529     C                   END
151900970529     C                   END
152000970529     C* Reperisco eventi attesa prima di aprire giacenza
152100970529     C                   MOVEA     EVE           WEVE             15
152200970529     C     WEVE          IFNE      *BLANKS
152300050105     C     §7r6pu        andne     *BLANKS
152400970529     C                   DO        5             SS
152500970530     C                   MOVEL     EVE(SS)       WEVGMA
152600970530     C                   EXSR      GMAEVE
152700970530     C     WTRGMA        IFEQ      'S'
152800970530     C                   GOTO      FINGMA
152900970529     C                   END
153000970529     C                   END
153100970529     C                   END
153200970530     C*
153300970530     C                   END
153400970529     C*
153500970530     C     FINGMA        ENDSR
153501151201      /free
153502151201        //----------------------------------------------------------------------
153503151201        // Reperimento progressivo richiesta da numeratore 650
153504151201        //----------------------------------------------------------------------
153505151201        begsr RepNum;
153506151201
153507151201          annocur  = %subdt(%date():*years);
153508151201          clear esito;
153509151201          $finenum=*off;
153510151201          clear trul33ds;
153511151201
153512151201          I33TLa = 'L';
153513151201          I33Ope = *ZEROS;
153514151201          I33CNu = 651;
153515151201          I33Num = 1;
153516151201
153517151201          KPJBU = TRUL33DS;
153518151201
153519151201          dow $finenum=*off   ;
153520151201             Trul33R(kpjba);
153521151201
153522151201             TRUL33DS = KPJBU;
153523151201
153524151201             if O33Err <> *zeros;
153525151201               Esito = '2';
153526151201               $finenum=*on;
153527151201             else;
153528151201               w0090=o33nri;
153529151201               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
153530151201               setll kprg tiidc02l;
153531151201        // Se numero già presente nel file TIIDC ne cerco un altro
153532151201               if not %equal;
153533151201                  $finenum=*on;
153534151201               endif;
153535151201             endif;
153536151201          enddo;
153537151201
153538151201          endsr;
153539151201      /end-free
153600940913     C*------------------------------------------------------------*
153700940913     C*  Operazioni iniziali
153800940913     C*------------------------------------------------------------*
153900940913     C     *INZSR        BEGSR
154000940913     C*
154100940913     C     *ENTRY        PLIST
154200940913     C                   PARM                    KPJBA
154300941017     C                   MOVEL     KPJBU         FNLG82
154400940913     C                   Z-ADD     1             CODUT
154500940913     C                   CALL      'X§PARUT'
154600940913     C                   PARM                    UTEDSE
154700940913     C*
154800940914     C                   MOVEL     RAGUT         V1CRSU
154900940913     C                   MOVEL     REC80         CNCR80
155000940913     C                   MOVEL     KNMUS         §SEI              6
155100020730     C                   Z-ADD     SimPou        §FLE
155200020730     C     SimTpp        COMP      '2'                                    01
155300940913     C*---------------------------------------------------------------*
155400940913     C*  KLIST
155500130610     C     Kfiar5        KLIST
155600130610     C                   KFLD                    KAAS
155700130610     C                   KFLD                    KLNP
155800130610     C                   KFLD                    KNRS
155900130610     C                   KFLD                    KNSP
156000130610     C                   KFLD                    trcar5            3
156100130610     C*
156200990203     C     KDCT          KLIST
156300990203     C                   KFLD                    KAAS
156400990203     C                   KFLD                    KLNP
156500990203     C                   KFLD                    KNRS
156600990203     C                   KFLD                    KNSP
156700940913     C     KLBL          KLIST
156800940913     C                   KFLD                    KAAS
156900940913     C                   KFLD                    KLNP
157000940913     C                   KFLD                    KNRS
157100940913     C                   KFLD                    KNSP
157200940913     C*
157300940913     C     KGCA          KLIST
157400940913     C                   KFLD                    KAAS
157500940913     C                   KFLD                    KLNP
157600940913     C                   KFLD                    KNRS
157700940913     C                   KFLD                    KNSP
157800940927     C                   KFLD                    KFRG
157900940927     C                   Z-ADD     0             KFRG
158000940913     C*
158100940913     C     KTBL1         KLIST
158200940913     C                   KFLD                    KKUT
158300940913     C                   KFLD                    KCOD
158400940913     C*
158500940913     C     KTBL2         KLIST
158600940913     C                   KFLD                    KKUT
158700940913     C                   KFLD                    KCOD
158800940913     C                   KFLD                    KKEY
158900940913     C*
159000060307     C     Key7u         Klist
159100060307     C                   Kfld                    KKUT
159200060307     C                   Kfld                    KCOD7u            2
159300060307     C                   KFLD                    KKEY7u            8
159400060307     C                   move      '7U'          Kcod7u
159500060307     C                   move      'Q'           KKEY7u
159600060307     c*
159700940913     C     KEVB1         KLIST
159800990803     C                   KFLD                    KAAS
159900940913     C                   KFLD                    KLNP
160000940913     C                   KFLD                    KNRS
160100940913     C                   KFLD                    KNSP
160200940913     C                   KFLD                    KDEV
160300940913     C*
160400990803     C     KEVB2         KLIST
160500990803     C                   KFLD                    KAAS
160600940913     C                   KFLD                    KLNP
160700940913     C                   KFLD                    KNRS
160800940913     C                   KFLD                    KNSP
160900950104     C*
161000950104     C     KARB          KLIST
161100950104     C                   KFLD                    KAAS
161200950104     C                   KFLD                    KLNP
161300950104     C                   KFLD                    KNRS
161400950104     C                   KFLD                    KNSP
161500070419     C     KARBb         KLIST
161600070419     C                   KFLD                    arbAAS
161700070419     C                   KFLD                    arbLNP
161800070419     C                   KFLD                    arbNRS
161900070419     C                   KFLD                    arbNSP
162000080603     c     kar4          klist
162100080603     c                   kfld                    arbAAS
162200080603     c                   kfld                    arbLNP
162300080603     c                   kfld                    arbNRS
162400080603     c                   kfld                    arbNSP
162500080603     c                   kfld                    trek4             1
162600110928     c     kfvv          klist
162700110928     c                   kfld                    npg
162800110928     c                   kfld                    nfv
162900110928     c                   kfld                    fgs
163000110928     c                   z-add     4             npg
163100940913     C*---------------------------------------------------------------*
163200940913     C*  Like DEFN
163300110928     C     *LIKE         DEFINE    fvvnfv        nfv
163400110928     C     *LIKE         DEFINE    fvvnpg        npg
163500110928     C     *LIKE         DEFINE    fvvfgs        fgs
163600940913     C     *LIKE         DEFINE    ARBAAS        KAAS
163700940913     C     *LIKE         DEFINE    ARBLNP        KLNP
163800940913     C     *LIKE         DEFINE    ARBLNA        KLNA
163900940913     C     *LIKE         DEFINE    ARBNRS        KNRS
164000940913     C     *LIKE         DEFINE    ARBNSP        KNSP
164100940927     C     *LIKE         DEFINE    GCPFRG        KFRG
164200940913     C     *LIKE         DEFINE    TBLKUT        KKUT
164300940913     C     *LIKE         DEFINE    TBLCOD        KCOD
164400940913     C     *LIKE         DEFINE    TBLKEY        KKEY
164500940913     C     *LIKE         DEFINE    EVBDEV        KDEV
164600940913     C*---------------------------------------------------------------*
164700941221     C                   CLEAR                   WLBDA8
164800950105     C                   CLEAR                   DATPAR
164900940914     C                   Z-ADD     1             KKUT
165000940913     C                   TIME                    WHHDAT           14 0
165001151216     C                   MOVEl     WHHDAT        ora               6 0
165100941221     C                   MOVE      WHHDAT        G02DAT
165200941221     C                   MOVEL     ' '           G02ERR
165300941221     C                   CALL      'XSRDA8'
165400941221     C                   PARM                    WLBDA8
165500941221     C                   Z-ADD     G02INV        OGGI              8 0
165600940913     C                   MOVEL     'S'           WGCAPE
165700940913     C                   Z-ADD     §FLE          V1CFLE
165800941003     C                   MOVEL     'P'           SAVTRI
165900940913     C*---------------------------------------------------------------*
166000940927     C*  Caricamento tabella 2A E 2Z
166100980216     C                   Z-ADD     0             X                 4 0
166200940913     C                   MOVEL     '2A'          KCOD
166300940914     C     KTBL1         CHAIN     TABEL                              31
166400940913     C     *IN31         DOWEQ     '0'
166500970602     C     X             ANDLT     200
166600940913     C                   MOVEL     TBLUNI        DS2A
166700950118     C     TBLFLG        IFEQ      ' '
166800940913     C                   ADD       1             X
166900940913     C                   MOVEL     TBLKEY        C2A(X)
167000970530     C                   MOVE      §2ADES        D2A(X)
167100970530     C                   MOVE      §2AGLA        G2A(X)
167200941228     C                   MOVEL     §2AFTC        TP2(X)
167300960119     C                   MOVEL     §2AFLG        FLG(X)
167400940913     C                   END
167500940914     C     KTBL1         READE     TABEL                                  31
167600940913     C                   END
167700950104     C*  Carico tabella correlata 2Z
167800950104     C                   MOVEL     '2Z'          KCOD
167900950104     C     KTBL1         CHAIN     TABEL                              31
168000950104     C     *IN31         DOWEQ     '0'
168100950104     C                   MOVEL     TBLUNI        DS2A
168200950104     C     TBLFLG        IFEQ      ' '
168300980216     C                   Z-ADD     1             X                 4 0
168400950104     C                   MOVEL     TBLKEY        WCD2A             3
168500950104     C     WCD2A         LOOKUP    C2A(X)                                 32
168600950104     C     *IN32         IFEQ      '1'
168700950104     C                   MOVEL     TBLUNI        DS2Z
168800950104     C                   MOVE      §2ZTIG        TPG(X)
168900950104     C                   END
169000950104     C                   END
169100950104     C     KTBL1         READE     TABEL                                  31
169200950104     C                   END
169300040806      *
169400040806     C*  Caricamento tabella 7U clienti che vogliono il reso anche nel
169500040806     C*                         periodo delle trazioni ridotte
169600040806     C                   Z-ADD     0             X                 4 0
169700040806     C                   MOVEL     '7U'          KCOD
169800040806     C     KTBL1         CHAIN     TABEL                              31
169900040806     C     *IN31         DOWEQ     '0'
170000040806     C     X             ANDLT     999
170100040806     C                   MOVE      TBLkey        tipoQ             1
170200040806     C                   MOVEL     TBLUNI        DS7U
170300040806     C     TBLFLG        IFEQ      ' '
170400040806     C     tipoQ         andeq     'Q'
170500040806     C     §7USRE        andeq     'S'
170600040806     C                   ADD       1             X
170700040806     c                   movel     tblkey        alfa7             7
170800040806     C                   movel     alfa7         K7u(X)
170900040806     C                   END
171000040806     C     KTBL1         READE     TABEL                                  31
171100040806     C                   END
171200940913     C*---------------------------------------------------------------*
171300940914     C*  Caricamento tabella 7Q
171400980216     C                   Z-ADD     0             X                 4 0
171500940914     C                   MOVEL     '7Q'          KCOD
171600940914     C     KTBL1         CHAIN     TABEL                              31
171700940914     C     *IN31         DOWEQ     '0'
171800940914     C     X             ANDLT     50
171900940914     C                   MOVEL     TBLUNI        DS7Q
172000940921     C     TBLFLG        IFEQ      ' '
172100940914     C                   ADD       1             X
172200940914     C                   MOVEL     TBLKEY        C7Q(X)
172300940921     C     X             OCCUR     MULT
172400940921     C                   MOVEL     DS7Q          MULT
172500940914     C                   END
172600940914     C     KTBL1         READE     TABEL                                  31
172700940914     C                   END
172800021024     C*---------------------------------------------------------------*
172900021024     C*  Caricamento tabella 7W correlata 7Q
173000021024     C                   Z-ADD     0             X                 4 0
173100021024     C                   MOVEL     '7W'          KCOD
173200021024     c                   do        50            x
173300021024     c                   movel     c7q(x)        kkey
173400021024     C     KTBL2         CHAIN     TABEL                              31
173500021024     C     *IN31         ifeq      '0'
173600021024     C                   MOVEL     TBLUNI        ds7w
173700021024     c                   else
173800021024     c                   clear                   ds7w
173900021024     c                   endif
174000021024     C     X             OCCUR     MULTw
174100021024     C                   MOVEL     ds7w          MULTw
174200021024     C                   END
174300970529     C*---------------------------------------------------------------*
174400970529     C*  Caricamento tabella 7R
174500980216     C                   Z-ADD     0             X                 4 0
174600970529     C                   MOVEL     '7R'          KCOD
174700970529     C     KTBL1         CHAIN     TABEL                              31
174800970529     C     *IN31         DOWEQ     '0'
174900970529     C     X             ANDLT     50
175000970529     C                   MOVEL     TBLUNI        DS7R
175100970529     C     TBLFLG        IFEQ      ' '
175200970529     C                   ADD       1             X
175300970529     C                   MOVEL     TBLKEY        C7R(X)
175400970530     C     X             OCCUR     MULTR
175500970529     C                   MOVEL     DS7R          MULTR
175600970529     C                   END
175700970529     C     KTBL1         READE     TABEL                                  31
175800970529     C                   END
175900940914     C*---------------------------------------------------------------*
176000940914     C* Caricamento £1
176100020503     c                   clear                   trul06ds
176200020503     c                   eval      d06cod = '£1'
176300020503     c                   movel     v1cfle        d06key
176400020503     c                   movel     trul06ds      kpjbu
176500020503     c                   call      'TRUL06R'
176600020503     c                   parm                    kpjba
176700020503     c                   movel     kpjbu         trul06ds
176800020503     c                   movea     lin           £1
176900990729     C* Caricamento 2G
177000990729     C                   Z-ADD     0             Y
177100990729     C                   MOVEL     '2G'          KCOD
177200990729     C                   MOVEL     *BLANKS       KKEY
177300990729     C                   MOVEL     '1'           KKEY
177400990729     C     KTBL2         CHAIN     TABEL                              31
177500990729     C     *IN31         IFEQ      '0'
177600990729     C     TBLFLG        ANDEQ     ' '
177700990729     C                   MOVEL     TBLUNI        DS2G
177800990729     C                   END
177900940914     C*---------------------------------------------------------------*
178000940914     C*  Caricamento tabella £6
178100940914     C                   EXSR      CAR£6
178200000526     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
178300000526     C                   CLEAR                   FIP
178400000526     C     *LOVAL        SETLL     AZORG01L
178500000526     C                   DO        *HIVAL
178600000526     C                   READ      AZORG01L                               95
178700000526     C   95              LEAVE
178800000526     C                   MOVEL     ORGDE3        OG143
178900020729     C     §OgNTW        IFEQ      'PPT'
179000000526     C                   Z-ADD     1             X
179100000526     C     *ZEROS        LOOKUP    FIP(X)                                 23
179200000526     C   23              Z-ADD     ORGFIL        FIP(X)
179300000526     C                   END
179400000526     C                   END
179500940913     C                   ENDSR
179600950104     OFNARB001  E            AGGBLO
179700950104     O                       ARBFBC
179800970606     OFNARB001  E            LIBER
179900990204**
180000940912Nessuna bolla giacente
180100990204Punto operativo errato
180200110928Numero distinta Stand-By inserito non corretto verificare
180300130613Non trovato evento standard per gestione fermo deposito avvisare CED

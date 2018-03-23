000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941227     H* FNLR49R *----------------------------------------------------*
000300960405     H*         - ELABORAZIONE  I.D.D.
000400000000     H*--------------------------------------------------------------*
000500960325     FAZORG01L  IF   E           K DISK
000600960327     FTABEL00F  IF   E           K DISK
000700960325     FFNFVV02L  IF   E           K DISK
000800990513     FFNANM04L  IF   E           K DISK
000900960508     FAZCLN01L  IF   E           K DISK
001000960508     FAZCAE02L  IF   E           K DISK
001100961030     FFNFVA02L  IF   E           K DISK
001200941227     FFNLR49D   CF   E             WORKSTN
001300020906
001400020906     d x               s              3  0
001500020906
001600960926     D L1              S              3  0 DIM(30)                              P.O.GESTITE
001700960926     D L6S             S              3  0 DIM(30)                              P.O.GEST DA AL
001800050830     D EST             S              3  0 DIM(30)                              P.O.GEST DA AL
001900960325     D K78             S              1    DIM(78)                              DI COMODO
002000960325     D SKI             S              1    DIM(15)                              COMODO X ABBL
002100960405     D IMA             S              9  0 DIM(50)                              IMA NO CHIUSI
002200960405     D IDA             S              9  0 DIM(50)                              IDA NO CHIUSI
002300960405     D IDD             S              3  0 DIM(30)                              IDD NO FATTI
002400050830     D MSG             S             78    DIM(14) CTDATA PERRCD(1)             MSG DI ERRORE
002500911015     D*
002600960327     D DS7A2         E DS
002700960409     D DS7AF         E DS
002800960508     D DS1Y          E DS
002900050504     D DVPO          E DS
003000960327     D CNCR80        E DS
003100960325     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003200960325     D* ds per pgm trul06r - caricamento £x
003300960325     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003400960325     D  LIN                    1     90  0
003500960926     D                                     DIM(30)                              P.O. COMODO
003600961016     D* DS PER FNLV56R - RICERCA OK IDA DEL TERMINAL DI ARRIVO
003700961016     D DSLV56        E DS                  EXTNAME(FNLV56DS)
003800960327     D* DS PER PASSAGGIO DATI A STAMPA IDD - FNLR35R
003900960327     D DSLR34        E DS                  EXTNAME(FNLR34DS)
004000971114     D DSLV55        E DS                  EXTNAME(FNLV55DS)
004100960422     D OG148         E DS
004200050805     D OG143         E DS
004300960422     D KPJBA         E DS
004400121115     d xgiolavds     e ds
004500050504     D* DS PER TIBS02R - GESTIONE TNTBE00F
004600050504     D DSBS02        E DS                  EXTNAME(TIBS02DS)
004700170220     d Trul09ds      e ds
004800170220     d  T09                   19    108  0 dim(30)
004900170220     d
005000960405     D                 DS
005100960405     D  KNPG                   1      1  0
005200960405     D  FVVFGS                 2      4  0
005300960405     D  FVVNFV                 5      9  0
005400960405     D  W0090                  1      9  0
005500921027     D WLBDAT          DS
005600960325     D  G02DAT                 1      8  0
005700960325     D  G02INV                 9     16  0
005800960325     D  G02ERR                17     17
005900960325     D  G02TGI                18     22  0
006000960508     D WGIDAT          DS
006100960508     D  GI8DAT                 1      8  0
006200960508     D  GI8INV                 9     16  0
006300960508     D  GI8TGI                17     21  0
006400960508     D                 DS
006500960508     D  CLNMAT                 1     31
006600960508     D  MAT                    1     31
006700960508     D                                     DIM(31)                              FESTIV.MATTINO
006800961209     D*
006900961209     D* DEFINIZIONE COSTANTI
007000961209     D CPRF            C                   CONST('ISP99     ')
007100900702     C****************************************************************
007200900702     C*  RIEPILOGO INDICATORI
007300900702     C***************************************************************
007400960325     C* 02    - GIA' CONFERMATO IDD  CON DATA UGUALE A UDATE
007500960926     C* 05    - SPROTEGGO P.O. IN GESTIONE
007600960325     C* 06    - NON ABILITO TASTO FUNZIONALE F18
007700960325     C* 40/41 - ERRORE
007800900702     C*****************************************************************
007900921027     C*
008000921027     C     FOR01         TAG
008100941227     C                   EXFMT     LR49D01
008200960325     C                   SETOFF                                       2890
008300960325     C                   SETOFF                                       4041
008400960325     C* F3 - FINE LAVORO
008500921027     C  NKC              DO
008600960926     C* F18 - CAMBIO P.O. IN GESTIONE
008700090406     c                   if        *inks
008800090406     C                   SETON                                        05
008900090406     c                   goto      for01
009000090406     c                   endif
009100090406     C* CONTROLLO LA DATA
009200090406     C                   Z-ADD     V1CDEL        G02DAT
009300090406     C                   MOVEL     *BLANK        G02ERR
009400090406     C                   CALL      'XSRDA8'
009500090406     C                   PARM                    WLBDAT
009600090406    1C     G02ERR        IFEQ      '1'
009700090406     C                   MOVEL     MSG(4)        V1CMSG
009800090406     C                   SETON                                        412890
009900090406     C                   GOTO      for01
010000090406    1C                   END
010100090406     C*
010200090406     C                   Z-ADD     G02DAT        V1CDEL
010300090406     C                   Z-ADD     G02INV        WDEL08            8 0
010400090406     C                   Z-ADD     G02TGI        WTGI
010500090406     c
010600090406     c* Carico la tabella L6s in base alla data elaborazione
010700090406     c                   EXSR      CARTAB
010800090406     c
010900090406     C* 05 ON - CONTROLLO P.O. IN  GESTIONE
011000090406     C   05              EXSR      CTRFGS
011100090406     C   05
011200090406     CAN 90              GOTO      FOR01
011300960325     C*
011400090406     C* controlli  anche in base al tasto funzionale premuto
011500960325     C                   EXSR      CTRD01
011600960325     C   90              GOTO      FOR01
011700050805     C*
011800960325     C**
011900950608     C* PER ENTER RIEMETTO LA VIDEATA
012000960327     C  NKA
012100960327     CANNKF              GOTO      FOR01
012200950608     C**
012300960327     C                   CLEAR                   DSLR34
012400960403     C                   MOVEL     'S'           D34GES
012500960403     C                   MOVEL     V1CFGS        D34FGS
012600960327     C                   MOVEL     'A'           D34PAR
012700960509     C                   MOVEL     'D'           D34IE
012800960327     C                   MOVEL     'S'           D34APR
012900960327     C                   MOVEL     V1CDEL        D34ALG
013000960327     C                   MOVEL     WDEL08        D34ALA
013100960327     C                   MOVEL     'I'           D34INC
013200960327     C                   MOVEL     §7ALID        D34LID
013300960327     C**
013400960327     C   KA              MOVEL     'N'           D34ELA                         F1  PROVVIS
013500000823     C     *INKF         IFEQ      *ON
013600000823     C                   MOVEL     'S'           D34ELA                         F6  DEFINIT
013700000823     C* FORZATURA CODA DI ELABRAZIONE
013800111025     C***                MOVEL     'KSKEDULA'    KQEBI
013900111025     C***                MOVEL     'KSKEDULA'    KQIEB
014000000823     C                   ENDIF
014100030717     C*
014200030717     C* VALORIZZO NUOVI CAMPI DELLA DS VALIDI UNICAMENTE COME SELEZIONE IN FASE DI STAMPA
014300030717     C* (ESEGUO UNA VALORIZZAZIONE NEUTRA OVVERO COME SE NN FOSSERO STATE RICHIESTE SELEZIONI)
014400030717     C                   Z-ADD     *ZEROS        D34POS
014500030717     C                   Z-ADD     99            D34NRS
014600921027     C*
014700960403     C                   MOVEL     DSLR34        KPJBU
014800960327     C                   MOVEL     'LR35'        KCOAZ
014900960327     C                   CALL      'BCH10'
015000921027     C                   PARM                    KPJBA
015100921027     C*
015200921027     C                   END
015300050805     c* chiudo trul06
015400050805     C                   CLEAR                   DSul06
015500050805     C                   MOVEL     'C'           D06TLA
015600050805     c                   movel     dsul06        kpjbu
015700050805     C                   CALL      'TRUL06R'
015800050805     C                   PARM                    kpjba
015900921027     C*
016000921027     C                   SETON                                        LR
016100960325     C*
016200960325     C* CONTROLLO DATI -----------------------------------------------*
016300960325     C     CTRD01        BEGSR
016400980220     C* SE RICHIESTO IDD DEFINITIVO ERRORE SE IL P.O. NON FA L'IDD
016500980220     C     *INKF         IFEQ      *ON
016600980220     C     WOGNID        ANDEQ     'N'
016700980220     C                   MOVEL     MSG(5)        V1CMSG
016800980220     C                   SETON                                        412890
016900980220     C                   GOTO      ENDCT1
017000980220     C                   END
017100050504     C* SE RICHIESTO IDD DEFINITIVO ERRORE SE ora non ancora passata
017200050504     C                   IF        *inkf and §vpohid>0
017300050504     C                   time                    wTIME             6 0
017400050504     C                   MOVEL     WTIMe         WORA              4 2
017500050504     c                   if        wOra<§vpohid
017600050504     C                   MOVEL     MSG(13)       V1CMSG
017700050504     c                   eval      %subst (v1cmsg:53:5) = (%editc(§vpohid: '2'))
017800050504     C                   SETON                                        412890
017900050504     C                   GOTO      ENDCT1
018000050504     C                   END
018100050504     C                   END
018200960325     C*
018300961209     C* SE PROFILO "ISP99" E PUNTO OPERATIVO <> 101 IMPEDISCO IL LANCIO
018400961209     C*   DELL'I.D.D. DEFINITIVO
018500961209     C     V1CFGS        IFNE      101                                          *BO-RESI
018600961209     C     KNMUS         ANDEQ     CPRF
018700961209     C     *INKF         ANDEQ     *ON
018800961209     C                   MOVEL     MSG(12)       V1CMSG
018900961209     C                   SETON                                        409028
019000961209     C                   GOTO      ENDCT1
019100961209     C                   ENDIF
019200961209     C*
019300960325     C* VERIFICO CON I TASTI FUNZIONALE
019400960325     C* IMPOSSIBILILE IDD PROVISSORIO SE GIA' CONFERMATO
019500960325    1C     *INKA         IFEQ      *ON
019600990513     C     WDEL08        ANDLE     WDID
019700960325     C                   MOVEL     MSG(6)        V1CMSG
019800960325     C                   SETON                                        412890
019900960325     C                   GOTO      ENDCT1
020000960325    1C                   ENDIF
020100960325     C* IMPOSSBILILE IDD DEFINITIVO  SE GIA' CONFERMATO
020200960325    1C     *INKF         IFEQ      *ON
020300990513     C     WDEL08        ANDLE     WDID
020400960325     C                   MOVEL     MSG(7)        V1CMSG
020500960325     C                   SETON                                        412890
020600960325     C                   GOTO      ENDCT1
020700960325    1C                   ENDIF
020800050830     c
020900050830     c* Impossibile IDD definitivo se p.o. estero
021000050830    1C     *INKF         IFEQ      *ON
021100050830     c     v1cfgs        lookup    EST                                    30
021200050830     c                   if        *in30
021300050830     C                   MOVEL     MSG(14)       V1CMSG
021400050830     C                   SETON                                        412890
021500050830     C                   GOTO      ENDCT1
021600050830    1C                   ENDIF
021700050830    1C                   ENDIF
021800960325     C*
021900960405     C*
022000960405     C* SE CAMBIA DATA RICONTROLLO TUTTO
022100960405    2C     WDEL08        IFNE      SAVDEL
022200960405     C                   CLEAR                   IMA
022300960405     C                   CLEAR                   IDA
022400960405     C                   CLEAR                   IDD
022500960405     C                   MOVEL     WDEL08        SAVDEL
022600960405    2C                   ENDIF
022700121115    c
022800121115     c* Per trovare il terminal di arrivo uso la data IDD  - 1 gg lavorativo
022900121115     c* cerco la data - 1 gg lavorativo
023000121115     c                   clear                   xgiolavds
023100121115     c                   eval      ixgldata=wdel08
023200121115     c                   eval      ixglfil =v1cfgs
023300121115     c                   eval      ixglpa  ='A'
023400121115     c                   eval      ixglsub ='S'
023500121115     c                   eval      ixglgg  =1
023600121115     c                   eval      ixgllav ='S'
023700121115     c
023800121115     c                   call      'XGIOLAV'
023900121115     c                   parm                    xgiolavds
024000121115     c
024100121115     c                   if        oxglerr<>' ' or oxgldata=0
024200121115     c                   eval      oxgldata=wdel08
024300121115     c                   endif
024400121115
024500960508     C**
024600090406     C** VERIFICO SE SONO TERMINAL DI ARRIVO
024700961015     C                   MOVE      ' '           WTEROR            1
024800961015     C                   MOVE      ' '           WTEREC            1
024900971114     C                   CLEAR                   DSLV55
025000971114     C                   MOVEL     'A'           D55TPT
025100971114     C                   MOVEL     'L'           D55TLA
025200971114     C                   MOVEL     V1CFGS        D55LIN
025300121115     C****               MOVEL     WDEL08        D55DRF
025400121115     C                   MOVEL     oxgldata      D55DRF
025500971114     C                   CALL      'FNLV55R'
025600971114     C                   PARM                    DSLV55
025700971114     C     D55ERR        IFNE      ' '
025800971114     C                   MOVEL     D55LIN        D55TFA
025900971114     C                   ENDIF
026000971114     C**
026100971114    1C     D55TFA        IFEQ      V1CFGS
026200961015     C                   MOVE      'S'           WTEROR
026300961015     C                   MOVE      'S'           WTEREC
026400960508   X1C                   ELSE
026500960508     C     KCAE          SETLL     AZCAE02L
026600960508     C     KCAE          READE     AZCAE02L                               30
026700960508    2C     *IN30         DOWEQ     *OFF
026800961015     C     WTEREC        ANDEQ     ' '
026900960508     C*
027000960508     C* TROVATA ECCEZIONE
027100121115    4C***  WDEL08        IFLE      CAEDSC
027200121115     C**** WDEL08        ANDGE     CAEDDE
027300121115    4C     oxgldata      IFLE      CAEDSC
027400121115     C     oxgldata      ANDGE     CAEDDE
027500960509     C     CAETFE        ANDEQ     V1CFGS
027600961015     C                   MOVE      'S'           WTEREC
027700960508    4C                   END
027800960508     C*
027900960508     C     KCAE          READE     AZCAE02L                               30
028000960508    2C                   ENDDO
028100960508    1C                   END
028200050805     c
028300050805     c                   movel     d55tfa        fgstfa            3 0
028400050805     c
028500960509     C**
028600960509     C** CONTROLLO SE IL MIO TERMINAL DI ARRIVO HA FATTO L'IDA
028700960509     C** SE NON SONO IO IL TERMINAL DI ME STESSO
028800961015     C     WTEROR        IFNE      'S'
028900960509     C                   EXSR      IDATER
029000960517     C   90              GOTO      ENDCT1
029100960509     C                   ENDIF
029200960405     C**
029300960405     C** VERIFICO SE CHIUSO IDA
029400960405     C                   EXSR      CTRIDA
029500960403     C   90              GOTO      ENDCT1
029600960509     C**
029700960509     C**  VERIFICO SE CHIUSO IMA
029800960509     C                   MOVEL     3             KNPG
029900960509     C                   EXSR      CTRIMA
030000960509     C   90              GOTO      ENDCT1
030100960405     C**
030200960405     C** SE SONO SIMFEL VERIFICO SE I 2 HANNO FATTO IDD
030300960423     C   KFV1CFGS        IFEQ      SIMFEL
030400960405     C                   EXSR      CTRIDD
030500960405     C                   ENDIF
030600960405     C   90              GOTO      ENDCT1
030700960403     C*
030800960403     C* SE CONTROLLO
030900960325     C     ENDCT1        ENDSR
031000960403     C*
031100960405     C* CONTROLLO I FOGLI IMA ----------------------------------------*
031200960405     C     CTRIMA        BEGSR
031300960403     C                   MOVEL     WDEL08        KDFV
031400960403     C                   CLEAR                   WTROV
031500050805     c**
031600050805     C**
031700960403     C                   MOVEL     V1CFGS        KFGS
031800960403     C     KFVV2         CHAIN     FNFVV02L                           30
031900960403     C*
032000960403    2C     *IN30         DOWEQ     *OFF
032100960405     C* TROVATO FOGLIO IMA NON ANNULLATO
032200960403    3C     FVVSPG        IFEQ      'A'
032300960403     C     FVVATB        ANDEQ     ' '
032400960403     C*
032500960403     C                   MOVEL     'S'           WTROV             1
032600960403     C* FOGLIO APERTO
032700960403    4C     FVVFCF        IFNE      'S'
032800960403     C                   EXSR      MSGAPE
032900960403    4C                   ENDIF
033000960403    3C                   ENDIF
033100050805     C**
033200960403     C  N90KFVV2         READE     FNFVV02L                               30
033300960403    2C                   ENDDO
033400960403     C*
033500960403     C* FOGLIO IMA NO  TROVATO IN DATA DI CONFERMA IDD
033600960405    3C  N90WTROV         IFEQ      ' '
033700960405     C                   CLEAR                   FVVNFV
033800960405     C                   MOVEL     V1CFGS        FVVFGS
033900960403     C                   EXSR      MSGMAN
034000960403    2C                   ENDIF
034100960403     C*
034200960403     C                   ENDSR
034300960405     C*
034400960405     C* CONTROLLO I FOGLI IDA ----------------------------------------*
034500960405     C     CTRIDA        BEGSR
034600960508     C                   MOVE      *ZEROS        II                3 0
034700960405     C                   MOVEL     WDEL08        KDFV
034800960509     C     CHKFVV        TAG
034900960405     C                   CLEAR                   WTROV2
035000960405     C                   MOVEL     2             KNPG
035100960405     C*
035200960405     C* PER ORA ALMENO, IL CONTROLLO DELL'IMA E' OGNUNO IL PROPRIO
035300960405     C* SE NON 'C'E O E' APERTO
035400960405     C                   MOVEL     V1CFGS        KFGS
035500960405     C     KFVV2         CHAIN     FNFVV02L                           30
035600960405     C*
035700960405    2C     *IN30         DOWEQ     *OFF
035800961030     C     *IN90         ANDEQ     *OFF
035900960405     C* TROVATO FOGLIO NON ANNULLATO
036000960405    3C     FVVATB        IFEQ      ' '
036100960405     C*
036200960405    4C     KNPG          IFEQ      2
036300960405     C                   MOVEL     'S'           WTROV2            1
036400121115     c                   endif
036500960405     C* FOGLIO APERTO
036600960405    4C     FVVFCF        IFNE      'S'
036700961030     C     KFVA          SETLL     FNFVA02L                               32
036800961030     C* EMETTO MSG DI FOGLIO APERTO SOLO SE HA ALMENO UN FVP ABBINATO
036900961030    5C     *IN32         IFEQ      *ON
037000960405     C                   EXSR      MSGAPE
037100961030    5C                   ENDIF
037200960405    4C                   ENDIF
037300960405    3C                   ENDIF
037400960405     C*
037500960405     C  N90KFVV2         READE     FNFVV02L                               30
037600960405    2C                   ENDDO
037700960405     C*
037800960405     C*
037900960405     C* FOGLIO IMA NO  TROVATO IN DATA DI CONFERMA IDD
038000960509    1C  N90WTROV2        IFEQ      ' '
038100960508     C* VERIFICO SE GIORNO PRECEDENTE FESTIVO
038200960508     C                   EXSR      CTRFES
038300960509    2C     WFES          IFEQ      'F'
038400960508     C* . . . . . . .     E' FESTIVO ->CERCO FOGLIO IDA CON QUESTA DATA
038500960508     C                   MOVE      WDELP         KDFV
038600960509     C                   GOTO      CHKFVV
038700960509   X2C                   ELSE
038800960508     C* . . . . . . . NON E' FESTIVO ->ERRORE BLOCCANTE SE SONO TER-ARR
038900961015    3C     WTEREC        IFEQ      'S'
039000961015    3C     WTEROR        OREQ      'S'
039100960508     C                   CLEAR                   FVVNFV
039200960508     C                   MOVEL     V1CFGS        FVVFGS
039300960508     C                   EXSR      MSGMAN
039400960509    3C                   ENDIF
039500960508     C*
039600960509    2C                   ENDIF
039700960509    1C                   ENDIF
039800960405     C                   ENDSR
039900960325     C*
040000960405     C* PREPARO MESSAGGIO "IXA MANCANTE" ----------------------------*
040100960325     C     MSGMAN        BEGSR
040200960405     C*
040300960403     C     KNPG          IFEQ      3
040400960405     C     W0090         LOOKUP    IMA                                    31
040500960403     C                   ELSE
040600960405     C     W0090         LOOKUP    IDA                                    31
040700960403     C                   ENDIF
040800960325     C*
040900960325    1C     *IN31         IFEQ      *OFF
041000961015     C     WTEROR        OREQ      'S'
041100961015     C     KNPG          ANDNE     3
041200961015     C     WTEREC        OREQ      'S'
041300960509     C     KNPG          ANDNE     3
041400960325     C                   Z-ADD     1             K                 3 0
041500960403     C     KNPG          IFEQ      3
041600960405     C     WNOVE         LOOKUP    IMA(K)                                 32
041700960405     C   32              Z-ADD     W0090         IMA(K)
041800960403     C                   ELSE
041900960405     C     WNOVE         LOOKUP    IDA(K)                                 32
042000960405     C   32              Z-ADD     W0090         IDA(K)
042100960403     C                   ENDIF
042200960325     C*
042300960325     C                   MOVEA     MSG(8)        K78
042400960405     C                   MOVEL     FVVFGS        W003A             3
042500960926     C                   MOVEA     W003A         K78(44)
042600960403     C     KNPG          IFEQ      3
042700960403     C                   MOVEL     'M'           K78(9)
042800960403     C                   ELSE
042900960403     C                   MOVEL     'D'           K78(9)
043000961015     C     WTEROR        IFEQ      'S'
043100961015     C     WTEREC        OREQ      'S'
043200960926     C                   MOVEA     *BLANKS       K78(47)
043300960509     C                   END
043400960403     C                   ENDIF
043500960325     C                   MOVEA     K78           V1CMSG
043600960325     C                   SETON                                        412890
043700960325    1C                   ENDIF
043800960403     C*
043900960325     C                   ENDSR
044000960325     C*
044100960325     C* PREPARA MESSAGGIO DI F.IMA APERTO----------------------------*
044200960325     C     MSGAPE        BEGSR
044300960405     C*
044400960405     C     KNPG          IFEQ      3
044500960405     C     W0090         LOOKUP    IMA                                    31
044600960405     C                   ELSE
044700960405     C     W0090         LOOKUP    IDA                                    31
044800960405     C                   ENDIF
044900960509     C*
045000960509     C* SE MESSAGGIO PER IDA APERTO E' BLOCCANTE SE SONO TER-ARR,
045100960509     C* ALTRIMENTI MSG FORZABILE
045200960325    1C     *IN31         IFEQ      *OFF
045300961015     C     WTEROR        OREQ      'S'
045400960509     C     KNPG          ANDNE     3
045500961015     C     WTEREC        OREQ      'S'
045600961015     C     KNPG          ANDNE     3
045700960325     C                   Z-ADD     1             K
045800960405     C**
045900960405     C     KNPG          IFEQ      3
046000960405     C     WNOVE         LOOKUP    IMA(K)                                 32
046100960405     C   32              Z-ADD     W0090         IMA(K)
046200960405     C                   ELSE
046300960405     C     WNOVE         LOOKUP    IDA(K)                                 32
046400960405     C   32              Z-ADD     W0090         IDA(K)
046500960405     C                   ENDIF
046600960325     C*
046700960325     C                   MOVEA     MSG(9)        K78
046800960325     C                   Z-ADD     11            I                 2 0
046900960325     C                   MOVEL     FVVNFV        W005A             5
047000960325     C                   MOVEA     W005A         SKI(I)
047100960325     C                   EXSR      ABBL
047200960325     C                   MOVEA     SKI(I)        K78(12)
047300960405     C                   MOVEL     FVVFGS        W003A             3
047400960926     C                   MOVEA     W003A         K78(52)
047500960403     C     KNPG          IFEQ      3
047600960403     C                   MOVEL     'M'           K78(9)
047700960403     C                   ELSE
047800960403     C                   MOVEL     'D'           K78(9)
047900961015     C     WTEREC        IFEQ      'S'
048000961015     C     WTEROR        OREQ      'S'
048100960926     C                   MOVEA     *BLANKS       K78(55)
048200960509     C                   END
048300960403     C                   ENDIF
048400960325     C                   MOVEA     K78           V1CMSG
048500960325     C                   SETON                                        412890
048600960325     C                   SETON                                        30
048700960325    1C                   ENDIF
048800960325     C*
048900960325     C                   ENDSR
049000960405     C*
049100960405     C* CONTROLLO SE HANNO FATTO IDD ---------------------------------*
049200960405     C     CTRIDD        BEGSR
049300960405     C                   Z-ADD     1             X
049400960405     C*
049500960405    1C     L1(X)         DOWGT     0
049600960405     C     *IN90         ANDEQ     *OFF
049700960405     C* NON CONTROLLO ME STESSO
049800960405    2C     L1(X)         IFNE      V1CFGS
049900960422     C* NON CONTROLLO SE IN ORGANIGRAMMA C'E' SCRITTO CHE NON FA IDD
050000960422     C                   Z-ADD     L1(X)         W0030             3 0
050100960422     C     W0030         CHAIN     AZORG01L                           30
050200960422     C  N30              MOVEL     ORGDE8        OG148
050300960422     C   30              CLEAR                   OG148
050400960422    3C     §OGNID        IFEQ      ' '
050500960405     C*
050600960405     C                   Z-ADD     95            KCAA
050700960405     C                   Z-ADD     L1(X)         KLNA
050800990513     C     KANM4         SETLL     FNANM04L                               30
050900960405     C*
051000960422    4C     *IN30         IFEQ      *OFF
051100960405     C     KLNA          LOOKUP    L6S                                    31
051200051111     c* Escludo p.o. esteri
051300051111     C  n31KLNA          LOOKUP    EST                                    31
051400960405     C* VEDO SE HO GIA' DATO IL MESSAGGIO
051500960405     C  N31KLNA          LOOKUP    IDD                                    31
051600960422    5C     *IN31         IFEQ      *OFF
051700960405     C*
051800960405     C* ERRORE SE NON C'E' OK IDD E NON E' GESTITA DA ALTRE
051900960405     C                   Z-ADD     1             K
052000960405     C     WTRE          LOOKUP    IDD(K)                                 32
052100960405     C   32              MOVEL     L1(X)         IDD(K)
052200960405     C*
052300960405     C                   MOVEA     MSG(10)       K78
052400960405     C                   MOVEL     L1(X)         W003A             3
052500960926     C                   MOVEA     W003A         K78(52)
052600960405     C                   MOVEA     K78           V1CMSG
052700960405     C                   SETON                                        412890
052800960422    5C                   ENDIF
052900960422    4C                   ENDIF
053000960405    3C                   ENDIF
053100960405    2C                   ENDIF
053200960405     C*
053300960405     C                   ADD       1             X
053400960405    1C                   ENDDO
053500960405     C*
053600960405     C                   ENDSR
053700960405     C*
053800960405     C* CONTROLLO SE IL MIO TERMINAL DI ARRIVO HA FATTO L'IDA --------*
053900960405     C     IDATER        BEGSR
054000960405     C                   Z-ADD     1             X
054100961016     C*
054200961016     C                   CLEAR                   DSLV56
054300990513     C                   MOVE      WDEL08        D56DAO
054400961016     C                   MOVE      WTGI          D56TGI
054500050805     C                   MOVE      fgsTFA        D56FE0
054600961016     C                   MOVEL     DSLV56        KPJBU
054700961016     C                   CALL      'FNLV56R'
054800961016     C                   PARM                    KPJBA
054900961016     C                   MOVEL     KPJBU         DSLV56
055000960405     C*
055100961016     C     D56ERR        IFEQ      'N'
055200961016     C*  NON TROVATO OK IDA DEL TERMINAL:ERRORE BLOCCANTE
055300960405     C                   MOVEA     MSG(11)       K78
055400050805     C                   MOVEL     fgsTFA        W003A             3
055500960509     C                   MOVEA     W003A         K78(62)
055600960405     C                   MOVEA     K78           V1CMSG
055700960405     C                   SETON                                        412890
055800960405     C                   ENDIF
055900960405     C*
056000960405     C                   ENDSR
056100960325     C*
056200960926      * CONTROLLO IL P.O. IN IN GESTIONE -----------------------------*
056300960325     C     CTRFGS        BEGSR
056400960325     C                   SETOFF                                       90
056500960325     C*
056600170220     c     v1cfgs        lookup    t09                                    30
056700170220     C***  V1CFGS        LOOKUP    L1                                     30
056800960325     C     *IN30         IFEQ      *OFF
056900960325     C     V1CFGS        OREQ      0
057000960325     C                   MOVEL     MSG(2)        V1CMSG
057100960325     C                   CLEAR                   V1DFGS
057200960325     C                   SETON                                        409028
057300960325     C                   GOTO      ENDCTF
057400960325     C                   ENDIF
057500960325     C* DECODIFICA
057600960325     C     V1CFGS        CHAIN     AZORG                              31
057700960325     C  N31              MOVEL     ORGDES        V1DFGS
057800980220     C* MEMORIZZO SE P.O. FA L'IDD
057900980220     C  N31              MOVEL     ORGDE8        OG148
058000980220     C   31              CLEAR                   OG148
058100980220     C                   MOVEL     §OGNID        WOGNID            1
058200960409     C*
058300960409     C                   MOVEL     '7A'          COD
058400960409     C                   MOVEL(P)  V1CFGS        KEY
058500960409     C     KTAB          CHAIN     TABEL                              31
058600960409     C     *IN31         IFEQ      *OFF
058700960409     C                   MOVEL     TBLUNI        DS7AF
058800960409     C                   MOVE      §7ADID        WDID
058900960409     C                   SETON                                        02
059000960409     C                   ELSE
059100960409     C                   CLEAR                   WDID
059200960409     C* ERRORE DATA ULTIMA ESECUZIONE IDD NON REPERIBILE
059300960409     C                   MOVEL     MSG(1)        V1CMSG
059400960918     C                   SETON                                        2890
059500960918     C                   GOTO      ENDCTF
059600960409     C                   END
059700960325     C*
059800960926     C* GESTITA DA ALTRO P.O.
059900960325     C     V1CFGS        LOOKUP    L6S                                    90
060000960325     C     *IN90         IFEQ      *ON
060100960325     C                   SETON                                        4028
060200960325     C                   MOVEL     MSG(3)        V1CMSG
060300960325     C                   GOTO      ENDCTF
060400960325     C                   ENDIF
060500960325     C*
060600960325     C     ENDCTF        ENDSR
060700960325     C* ABBLANCA ZERO NON SIGNIFICATIVI -----------------------------*
060800960325     C     ABBL          BEGSR
060900960325     C*
061000960325     C     SKI(I)        DOWEQ     '0'
061100960325     C     I             ANDLT     15
061200960325     C                   MOVEL     ' '           SKI(I)
061300960325     C                   ADD       1             I
061400960325     C                   ENDDO
061500960325     C*
061600960325     C                   ENDSR
061700050805     C*
061800960508      * CONTROLLO SE GIORNO FESTIVO ----------------------------------*
061900960508     C     CTRFES        BEGSR
062000960508     C                   MOVE      *BLANKS       WFES              1
062100960508     C                   CLEAR                   WDELP
062200960508     C                   ADD       1             II
062300960508     C* DETERMINO GIORNO PRECEDENTE
062400960508     C                   CLEAR                   WGIDAT
062500960508     C     WTGI          SUB       II            GI8TGI
062600960509     C                   CALL      'XSRGI8'
062700960508     C                   PARM                    WGIDAT
062800960508     C                   Z-ADD     GI8INV        WDELP
062900960508     C* VERIFICO SE FESTIVO LEGGENDO CALENDARIO BARTOLINI
063000960508     C                   MOVEL     GI8INV        KANN
063100960508     C                   MOVE      GI8INV        W0040             4 0
063200960508     C                   MOVEL     W0040         KMES
063300960508     C                   MOVE      W0040         GG                2 0
063400960508     C                   CLEAR                   KTFP
063500050805     C                   MOVE      fgsTFA        KTFA
063600960508     C     KCLN          CHAIN     AZCLN01L                           31
063700960508    1C     *IN31         IFEQ      *OFF
063800960508     C     MAT(GG)       ANDNE     *BLANKS
063900960508     C                   MOVEL     '1Y'          COD
064000960508     C                   MOVEL(P)  MAT(GG)       KEY
064100960508     C     KTAB          CHAIN     TABEL                              31
064200960508    2C     *IN31         IFEQ      *OFF
064300960508     C                   MOVEL     TBLUNI        DS1Y
064400960508    3C     §1YFEI        IFEQ      'S'
064500960508     C                   MOVEL     'F'           WFES
064600960508    3C                   END
064700960508    2C                   END
064800960508    1C                   END
064900960508     C*
065000960508     C                   ENDSR
065100960325     C*--------------------------------------------------------------*
065200960325     C     *INZSR        BEGSR
065300960325     C* KLIST
065400960325     C     KFVV2         KLIST
065500960325     C                   KFLD                    KNPG
065600960325     C                   KFLD                    KDFV
065700960325     C                   KFLD                    KFGS
065800961030     C     KFVA          KLIST
065900961030     C                   KFLD                    FVVNPG
066000961030     C                   KFLD                    FVVNFV
066100020917     c                   Kfld                    FvvFgs
066200960325     C     KFVV21        KLIST
066300960325     C                   KFLD                    KNPG
066400960325     C                   KFLD                    KDFV
066500960327     C     KTAB          KLIST
066600960327     C                   KFLD                    CODUT
066700960327     C                   KFLD                    COD
066800960327     C                   KFLD                    KEY
066900960405     C     KANM4         KLIST
067000960405     C                   KFLD                    KCAA
067100990513     C                   KFLD                    WDEL08
067200960405     C                   KFLD                    KLNA
067300960508     C     KCLN          KLIST
067400960508     C                   KFLD                    KTFP
067500960508     C                   KFLD                    KTFA
067600960508     C                   KFLD                    KANN
067700960508     C                   KFLD                    KMES
067800960508     C     KCAE          KLIST
067900960508     C                   KFLD                    KEPA
068000960508     C                   KFLD                    V1CFGS
068100970717     C     KCAE1         KLIST
068200970717     C                   KFLD                    CAEEPA
068300970717     C                   KFLD                    CAETFA
068400970717     C                   KFLD                    CAETFP
068500970717     C                   KFLD                    WDEL08
068600960508     C                   MOVEL     'A'           KEPA
068700960325     C*
068800960327     C     *LIKE         DEFINE    TBLCOD        COD
068900960327     C     *LIKE         DEFINE    TBLKEY        KEY
069000960325     C     *LIKE         DEFINE    FVVDFV        KDFV
069100960325     C     *LIKE         DEFINE    V1CDEL        SAVDEL
069200960325     C     *LIKE         DEFINE    FVVFGS        KFGS
069300960405     C     *LIKE         DEFINE    ANMLNA        KLNA
069400960405     C     *LIKE         DEFINE    ANMCAA        KCAA
069500960409     C     *LIKE         DEFINE    §7ADID        WDID
069600960509     C     *LIKE         DEFINE    WDEL08        WDELP
069700960508     C     *LIKE         DEFINE    G02TGI        WTGI
069800960508     C     *LIKE         DEFINE    CLNANN        KANN
069900960508     C     *LIKE         DEFINE    CLNMES        KMES
070000960508     C     *LIKE         DEFINE    CLNTFP        KTFP
070100960508     C     *LIKE         DEFINE    CLNTFA        KTFA
070200960508     C     *LIKE         DEFINE    CAEEPA        KEPA
070300960325     C**
070400960405     C                   MOVEL     *ZEROS        WNOVE             9 0
070500960405     C                   MOVEL     *ZEROS        WTRE              3 0
070600960325     C**
070700960325     C*--------------------------------------------------------------*
070800960325     C*
070900960325     C     *ENTRY        PLIST
071000960325     C                   PARM                    KPJBA
071100960325     C                   Z-ADD     1             CODUT
071200960325     C                   CALL      'X§PARUT'
071300960325     C                   PARM                    UT§DSE
071400960325     C                   MOVEL     REC80         CNCR80
071500960325     C                   MOVEL     RAGUT         RSUT             20
071600960325     C*
071700020429     C                   Z-ADD     SIMPOU        V1CFGS
071800170220     C**   SIMTPP        IFEQ      '2'
071900170220     C**   SIMTPP        OREQ      *BLANKS
072000170220     C**                 SETON                                        06
072100170220     C**                 ENDIF
072200170220     c* il cambio filiale gestione lo do a chi è terminal di arrivo non partenza
072300170220     c*
072400170220     c                   clear                   trul09ds
072500170220     c                   eval      i09mod='TFA'
072600170220     c                   eval      i09tfa=simpou
072700170220     c                   call      'TRUL09R'
072800170220     c                   parm                    trul09ds
072900170220     c
073000170220     c                   if        o09err<>' ' or t09(1)=000
073100170220     c                   seton                                        06
073200170220     c                   endif
073300170220     c* se ce ne è solo uno ed è uguale a se stesso non F18
073400170220     c                   if        t09(2)=000 and t09(1)=simpou
073500170220     c                   seton                                        06
073600170220     c                   endif
073700960325     C*
073800961209     C* SE PROFILO "ISP99" COME PUNTO OPERATIVO IMPOSTO "101"
073900961209     C     KNMUS         IFEQ      CPRF
074000961209     C                   Z-ADD     101           V1CFGS
074100961209     C                   ENDIF
074200961209     C*
074300960325     C     V1CFGS        CHAIN     AZORG01L                           31
074400960325     C  N31              MOVEL     ORGDES        V1DFGS
074500980220     C* MEMORIZZO SE P.O. FA L'IDD
074600980220     C  N31              MOVEL     ORGDE8        OG148
074700980220     C   31              CLEAR                   OG148
074800980220     C                   MOVEL     §OGNID        WOGNID            1
074900960325     C*
075000960325     C*---------------------------------------------------------------*
075100960325     C* GIRO UDATE
075200960325     C                   Z-ADD     UDATE         G02DAT
075300960325     C                   MOVEL     *BLANK        G02ERR
075400960325     C                   CALL      'XSRDA8'
075500960325     C                   PARM                    WLBDAT
075600960325     C                   Z-ADD     G02INV        DATEU             6 0
075700960325     C                   Z-ADD     G02DAT        V1CDEL
075800960325     C*
075900960325     C* DOPO ELABORAZIONE DEFINITIVA NON VISUALIZZO F1  ED F6
076000960409     C*
076100960409     C                   MOVEL     '7A'          COD
076200960409     C                   MOVEL(P)  V1CFGS        KEY
076300960409     C     KTAB          CHAIN     TABEL                              31
076400960409     C     *IN31         IFEQ      *OFF
076500960409     C                   MOVEL     TBLUNI        DS7AF
076600960409     C                   MOVE      §7ADID        WDID
076700960409     C                   SETON                                        02
076800960409     C                   ELSE
076900960409     C                   CLEAR                   WDID
077000960409     C* ERRORE DATA ULTIMA ESECUZIONE IDD NON REPERIBILE
077100960409     C                   MOVEL     MSG(1)        V1CMSG
077200960409     C                   SETON                                        28
077300960409     C                   END
077400960327     C**
077500960327     C* CARICO LA DS7A2
077600960327     C**
077700960327     C                   MOVEL     '7A'          COD
077800960327     C                   MOVEL(P)  '2'           KEY
077900960327     C     KTAB          CHAIN     TABEL                              31
078000960327     C     *IN31         IFEQ      *OFF
078100960327     C     TBLFLG        ANDEQ     ' '
078200960327     C                   MOVEL     TBLUNI        DS7A2
078300960327     C                   ELSE
078400960327     C                   CLEAR                   DS7A2
078500960327     C                   ENDIF
078600050504      * Aggancio tabella "VPO" per ora conferma idd
078700050504     C                   CLEAR                   dsbs02
078800050504     c                   clear                   dvpo
078900050504     C                   MOVEL     'C'           T02MOD
079000050504     C                   MOVEL     KNSIF         T02SIF
079100050504     C                   MOVEL     'VPO'         T02COD
079200050504     c                   movel(P)  'VPO'         t02ke1
079300050504     C                   CALL      'TIBS02R'
079400050504     C                   PARM                    KPJBA
079500050504     C                   PARM                    dsbs02
079600050504    2C                   IF        T02ERR = *BLANKS
079700050504     C                   MOVEL     T02uni        dvpo
079800050504    2C                   ENDIF
079900050805     C***
080000050805     C* CARICO TABELLA P.O.  GESTITI £1
080100050805     C***
080200050805     C                   CLEAR                   L1
080300050805     C                   CLEAR                   DSUL06
080400050805     C                   MOVE      '£1'          D06COD
080500050805     C                   MOVEL     SIMFEL        D06KEY
080600050805     C                   MOVEL     DSUL06        KPJBU
080700050805     C                   CALL      'TRUL06R'
080800050805     C                   PARM                    KPJBA
080900050805     C                   MOVEL     KPJBU         DSUL06
081000050805     C                   MOVEA     LIN           L1
081100960327     C*
081200960325     C                   ENDSR
081300050805     c*
081400050805     c* Carico L6 in base alla data elaborazione --------------------
081500050805     c     CARTAB        BEGSR
081600050805     C***
081700050805     C* CARICO TUTTE I P.O. GESTITI DA ALTRI (DA £6)
081800050805     C***
081900050805     C                   CLEAR                   L6S
082000050805     C                   CLEAR                   DSUL06
082100050805     C                   MOVE      '£6'          D06COD
082200050805     C**!!!              MOVEL     'S'           D06ESC
082300050805     c                   eval      d06esc = 'G'
082400050805     c                   movel     simfel        d06key
082500050805     c                   move      'S'           d06key
082600050805     c                   movel     Wdel08        d06drf
082700050805     C                   MOVEL     DSUL06        KPJBU
082800050805     C                   CALL      'TRUL06R'
082900050805     C                   PARM                    KPJBA
083000050805     C                   MOVEL     KPJBU         DSUL06
083100050805     C                   MOVEA     LIN           L6S
083200050830     c***                z-add     1             L                 3 0
083300050830     c***  000           lookup    L6S(L)                                 30
083400050805     c
083500050805     c
083600050830     c* Carico a parte anche i p.o. esteri
083700050830     c                   z-add     1             L                 3 0
083800050805     C     *LOVAL        SETLL     AZORG01L
083900050805     C                   READ      AZORG01L                               30
084000050805     C*
084100050805    2C     *IN30         DOWEQ     *OFF
084200050805     c     L             andle     30
084300050805    3C     ORGFAG        IFEQ      'F'
084400050805     C     ORGFAG        OREQ      'A'
084500050805     C* P.O. NON ANNULLATO
084600050805    4C     ORGFVA        IFEQ      ' '
084700050805     C                   MOVEL     ORGDE3        OG143
084800050805     C*
084900050805    5C     §OGNTW        IFEQ      'EEX'                                        ESTERA
085000050805     C     §OGNTW        OREQ      'EUP'
085100050805     C     §OGNTW        OREQ      'FED'
085200050805     C     §OGNTW        OREQ      'DPD'                                        DPD
085300050805     C*
085400050805     C* VEDO SE HA COME TERMINAL DI PARTENZA LA FIL.GESTIONE
085500050805     C                   CLEAR                   DSLV55
085600050805     C                   MOVEL     'P'           D55TPT
085700050805     C                   MOVEL     ORGFIL        D55LIN
085800050805     C                   MOVEL     wdel08        D55DRF
085900050805     C                   CALL      'FNLV55R'
086000050805     C                   PARM                    DSLV55
086100050805    6C     D55ERR        IFNE      ' '
086200050805     C                   MOVEL     D55LIN        D55TFP
086300050805    6C                   ENDIF
086400050805    6C     D55TFP        IFEQ      simfel
086500050830     C                   Z-ADD     ORGFIL        EST(L)
086600050805     C                   ADD       1             L
086700050805    6C                   ENDIF
086800050805    5C                   ENDIF
086900050805    4C                   ENDIF
087000050805    3C                   ENDIF
087100050805     C*
087200050805     C                   READ      AZORG01L                               30
087300050805    2C                   ENDDO
087400050805     c                   ENDSR
087500961209** MSG
087600960409Impossibile lanciare l'elaborazione IDD: telefonare in sede                    1
087700960926Punto operativo in gestione errato
087800960926Punto operativo errato: in gestione ad altro P.O.                              3
087900960325Data elaborazione errata
088000980220Impossibile eseguire IDD DEFINITIVO  per il punto operativo richiesto
088100960325Impossibile eseguire IDD PROVVISORIO : gia' confermato nella data richiesta    6
088200960325Impossibile eseguire IDD DEFINITIVO : gia' confermato nella data richiesta
088300960926Foglio IMA mancante per il punto operativo xxx : ENTER PER FORZARE            8
088400020918Foglio IMA       non chiuso per il punto operativo xxx : ENTER PER FORZARE
088500960926Manca l'elaborazione I.D.D. per il punto operativo xxx : ENTER PER FORZARE
088600960508Manca l'elaborazione dell'IDA del proprio terminal di arrivo xxx
088700961209Profilo non abilitato al lancio dell'IDD DEFINITIVO per il p.o. immesso !!    12
088800050504Impossibile eseguire IDD definitivo prima delle ore xx,xx                     13
088900050830Impossibile eseguire IDD definitivo per P.O. estero                           14

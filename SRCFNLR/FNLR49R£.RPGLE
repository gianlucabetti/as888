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
004700960405     D                 DS
004800960405     D  KNPG                   1      1  0
004900960405     D  FVVFGS                 2      4  0
005000960405     D  FVVNFV                 5      9  0
005100960405     D  W0090                  1      9  0
005200921027     D WLBDAT          DS
005300960325     D  G02DAT                 1      8  0
005400960325     D  G02INV                 9     16  0
005500960325     D  G02ERR                17     17
005600960325     D  G02TGI                18     22  0
005700960508     D WGIDAT          DS
005800960508     D  GI8DAT                 1      8  0
005900960508     D  GI8INV                 9     16  0
006000960508     D  GI8TGI                17     21  0
006100960508     D                 DS
006200960508     D  CLNMAT                 1     31
006300960508     D  MAT                    1     31
006400960508     D                                     DIM(31)                              FESTIV.MATTINO
006500961209     D*
006600961209     D* DEFINIZIONE COSTANTI
006700961209     D CPRF            C                   CONST('ISP99     ')
006800900702     C****************************************************************
006900900702     C*  RIEPILOGO INDICATORI
007000900702     C***************************************************************
007100960325     C* 02    - GIA' CONFERMATO IDD  CON DATA UGUALE A UDATE
007200960926     C* 05    - SPROTEGGO P.O. IN GESTIONE
007300960325     C* 06    - NON ABILITO TASTO FUNZIONALE F18
007400960325     C* 40/41 - ERRORE
007500900702     C*****************************************************************
007600921027     C*
007700921027     C     FOR01         TAG
007800941227     C                   EXFMT     LR49D01
007900960325     C                   SETOFF                                       2890
008000960325     C                   SETOFF                                       4041
008100960325     C* F3 - FINE LAVORO
008200921027     C  NKC              DO
008300960926     C* F18 - CAMBIO P.O. IN GESTIONE
008400090406     c                   if        *inks
008500090406     C                   SETON                                        05
008600090406     c                   goto      for01
008700090406     c                   endif
008800090406     C* CONTROLLO LA DATA
008900090406     C                   Z-ADD     V1CDEL        G02DAT
009000090406     C                   MOVEL     *BLANK        G02ERR
009100090406     C                   CALL      'XSRDA8'
009200090406     C                   PARM                    WLBDAT
009300090406    1C     G02ERR        IFEQ      '1'
009400090406     C                   MOVEL     MSG(4)        V1CMSG
009500090406     C                   SETON                                        412890
009600090406     C                   GOTO      for01
009700090406    1C                   END
009800090406     C*
009900090406     C                   Z-ADD     G02DAT        V1CDEL
010000090406     C                   Z-ADD     G02INV        WDEL08            8 0
010100090406     C                   Z-ADD     G02TGI        WTGI
010200090406     c
010300090406     c* Carico la tabella L6s in base alla data elaborazione
010400090406     c                   EXSR      CARTAB
010500090406     c
010600090406     C* 05 ON - CONTROLLO P.O. IN  GESTIONE
010700090406     C   05              EXSR      CTRFGS
010800090406     C   05
010900090406     CAN 90              GOTO      FOR01
011000960325     C*
011100090406     C* controlli  anche in base al tasto funzionale premuto
011200960325     C                   EXSR      CTRD01
011300960325     C   90              GOTO      FOR01
011400050805     C*
011500960325     C**
011600950608     C* PER ENTER RIEMETTO LA VIDEATA
011700960327     C  NKA
011800960327     CANNKF              GOTO      FOR01
011900950608     C**
012000960327     C                   CLEAR                   DSLR34
012100960403     C                   MOVEL     'S'           D34GES
012200960403     C                   MOVEL     V1CFGS        D34FGS
012300960327     C                   MOVEL     'A'           D34PAR
012400960509     C                   MOVEL     'D'           D34IE
012500960327     C                   MOVEL     'S'           D34APR
012600960327     C                   MOVEL     V1CDEL        D34ALG
012700960327     C                   MOVEL     WDEL08        D34ALA
012800960327     C                   MOVEL     'I'           D34INC
012900960327     C                   MOVEL     §7ALID        D34LID
013000960327     C**
013100960327     C   KA              MOVEL     'N'           D34ELA                         F1  PROVVIS
013200000823     C     *INKF         IFEQ      *ON
013300000823     C                   MOVEL     'S'           D34ELA                         F6  DEFINIT
013400000823     C* FORZATURA CODA DI ELABRAZIONE
013500111025     C***                MOVEL     'KSKEDULA'    KQEBI
013600111025     C***                MOVEL     'KSKEDULA'    KQIEB
013700000823     C                   ENDIF
013800030717     C*
013900030717     C* VALORIZZO NUOVI CAMPI DELLA DS VALIDI UNICAMENTE COME SELEZIONE IN FASE DI STAMPA
014000030717     C* (ESEGUO UNA VALORIZZAZIONE NEUTRA OVVERO COME SE NN FOSSERO STATE RICHIESTE SELEZIONI)
014100030717     C                   Z-ADD     *ZEROS        D34POS
014200030717     C                   Z-ADD     99            D34NRS
014300921027     C*
014400960403     C                   MOVEL     DSLR34        KPJBU
014500960327     C                   MOVEL     'LR35'        KCOAZ
014600960327     C                   CALL      'BCH10'
014700921027     C                   PARM                    KPJBA
014800921027     C*
014900921027     C                   END
015000050805     c* chiudo trul06
015100050805     C                   CLEAR                   DSul06
015200050805     C                   MOVEL     'C'           D06TLA
015300050805     c                   movel     dsul06        kpjbu
015400050805     C                   CALL      'TRUL06R'
015500050805     C                   PARM                    kpjba
015600921027     C*
015700921027     C                   SETON                                        LR
015800960325     C*
015900960325     C* CONTROLLO DATI -----------------------------------------------*
016000960325     C     CTRD01        BEGSR
016100980220     C* SE RICHIESTO IDD DEFINITIVO ERRORE SE IL P.O. NON FA L'IDD
016200980220     C     *INKF         IFEQ      *ON
016300980220     C     WOGNID        ANDEQ     'N'
016400980220     C                   MOVEL     MSG(5)        V1CMSG
016500980220     C                   SETON                                        412890
016600980220     C                   GOTO      ENDCT1
016700980220     C                   END
016800050504     C* SE RICHIESTO IDD DEFINITIVO ERRORE SE ora non ancora passata
016900050504     C                   IF        *inkf and §vpohid>0
017000050504     C                   time                    wTIME             6 0
017100050504     C                   MOVEL     WTIMe         WORA              4 2
017200050504     c                   if        wOra<§vpohid
017300050504     C                   MOVEL     MSG(13)       V1CMSG
017400050504     c                   eval      %subst (v1cmsg:53:5) = (%editc(§vpohid: '2'))
017500050504     C                   SETON                                        412890
017600050504     C                   GOTO      ENDCT1
017700050504     C                   END
017800050504     C                   END
017900960325     C*
018000961209     C* SE PROFILO "ISP99" E PUNTO OPERATIVO <> 101 IMPEDISCO IL LANCIO
018100961209     C*   DELL'I.D.D. DEFINITIVO
018200961209     C     V1CFGS        IFNE      101                                          *BO-RESI
018300961209     C     KNMUS         ANDEQ     CPRF
018400961209     C     *INKF         ANDEQ     *ON
018500961209     C                   MOVEL     MSG(12)       V1CMSG
018600961209     C                   SETON                                        409028
018700961209     C                   GOTO      ENDCT1
018800961209     C                   ENDIF
018900961209     C*
019000960325     C* VERIFICO CON I TASTI FUNZIONALE
019100960325     C* IMPOSSIBILILE IDD PROVISSORIO SE GIA' CONFERMATO
019200960325    1C     *INKA         IFEQ      *ON
019300990513     C     WDEL08        ANDLE     WDID
019400960325     C                   MOVEL     MSG(6)        V1CMSG
019500960325     C                   SETON                                        412890
019600960325     C                   GOTO      ENDCT1
019700960325    1C                   ENDIF
019800960325     C* IMPOSSBILILE IDD DEFINITIVO  SE GIA' CONFERMATO
019900960325    1C     *INKF         IFEQ      *ON
020000990513     C     WDEL08        ANDLE     WDID
020100960325     C                   MOVEL     MSG(7)        V1CMSG
020200960325     C                   SETON                                        412890
020300960325     C                   GOTO      ENDCT1
020400960325    1C                   ENDIF
020500050830     c
020600050830     c* Impossibile IDD definitivo se p.o. estero
020700050830    1C     *INKF         IFEQ      *ON
020800050830     c     v1cfgs        lookup    EST                                    30
020900050830     c                   if        *in30
021000050830     C                   MOVEL     MSG(14)       V1CMSG
021100050830     C                   SETON                                        412890
021200050830     C                   GOTO      ENDCT1
021300050830    1C                   ENDIF
021400050830    1C                   ENDIF
021500960325     C*
021600960405     C*
021700960405     C* SE CAMBIA DATA RICONTROLLO TUTTO
021800960405    2C     WDEL08        IFNE      SAVDEL
021900960405     C                   CLEAR                   IMA
022000960405     C                   CLEAR                   IDA
022100960405     C                   CLEAR                   IDD
022200960405     C                   MOVEL     WDEL08        SAVDEL
022300960405    2C                   ENDIF
022400121115    c
022500121115     c* Per trovare il terminal di arrivo uso la data IDD  - 1 gg lavorativo
022600121115     c* cerco la data - 1 gg lavorativo
022700121115     c                   clear                   xgiolavds
022800121115     c                   eval      ixgldata=wdel08
022900121115     c                   eval      ixglfil =v1cfgs
023000121115     c                   eval      ixglpa  ='A'
023100121115     c                   eval      ixglsub ='S'
023200121115     c                   eval      ixglgg  =1
023300121115     c                   eval      ixgllav ='S'
023400121115     c
023500121115     c                   call      'XGIOLAV'
023600121115     c                   parm                    xgiolavds
023700121115     c
023800121115     c                   if        oxglerr<>' ' or oxgldata=0
023900121115     c                   eval      oxgldata=wdel08
024000121115     c                   endif
024100121115
024200960508     C**
024300090406     C** VERIFICO SE SONO TERMINAL DI ARRIVO
024400961015     C                   MOVE      ' '           WTEROR            1
024500961015     C                   MOVE      ' '           WTEREC            1
024600971114     C                   CLEAR                   DSLV55
024700971114     C                   MOVEL     'A'           D55TPT
024800971114     C                   MOVEL     'L'           D55TLA
024900971114     C                   MOVEL     V1CFGS        D55LIN
025000121115     C****               MOVEL     WDEL08        D55DRF
025100121115     C                   MOVEL     oxgldata      D55DRF
025200971114     C                   CALL      'FNLV55R'
025300971114     C                   PARM                    DSLV55
025400971114     C     D55ERR        IFNE      ' '
025500971114     C                   MOVEL     D55LIN        D55TFA
025600971114     C                   ENDIF
025700971114     C**
025800971114    1C     D55TFA        IFEQ      V1CFGS
025900961015     C                   MOVE      'S'           WTEROR
026000961015     C                   MOVE      'S'           WTEREC
026100960508   X1C                   ELSE
026200960508     C     KCAE          SETLL     AZCAE02L
026300960508     C     KCAE          READE     AZCAE02L                               30
026400960508    2C     *IN30         DOWEQ     *OFF
026500961015     C     WTEREC        ANDEQ     ' '
026600960508     C*
026700960508     C* TROVATA ECCEZIONE
026800121115    4C***  WDEL08        IFLE      CAEDSC
026900121115     C**** WDEL08        ANDGE     CAEDDE
027000121115    4C     oxgldata      IFLE      CAEDSC
027100121115     C     oxgldata      ANDGE     CAEDDE
027200960509     C     CAETFE        ANDEQ     V1CFGS
027300961015     C                   MOVE      'S'           WTEREC
027400960508    4C                   END
027500960508     C*
027600960508     C     KCAE          READE     AZCAE02L                               30
027700960508    2C                   ENDDO
027800960508    1C                   END
027900050805     c
028000050805     c                   movel     d55tfa        fgstfa            3 0
028100050805     c
028200960509     C**
028300960509     C** CONTROLLO SE IL MIO TERMINAL DI ARRIVO HA FATTO L'IDA
028400960509     C** SE NON SONO IO IL TERMINAL DI ME STESSO
028500961015     C     WTEROR        IFNE      'S'
028600960509     C                   EXSR      IDATER
028700960517     C   90              GOTO      ENDCT1
028800960509     C                   ENDIF
028900960405     C**
029000960405     C** VERIFICO SE CHIUSO IDA
029100960405     C                   EXSR      CTRIDA
029200960403     C   90              GOTO      ENDCT1
029300960509     C**
029400960509     C**  VERIFICO SE CHIUSO IMA
029500960509     C                   MOVEL     3             KNPG
029600960509     C                   EXSR      CTRIMA
029700960509     C   90              GOTO      ENDCT1
029800960405     C**
029900960405     C** SE SONO SIMFEL VERIFICO SE I 2 HANNO FATTO IDD
030000960423     C   KFV1CFGS        IFEQ      SIMFEL
030100960405     C                   EXSR      CTRIDD
030200960405     C                   ENDIF
030300960405     C   90              GOTO      ENDCT1
030400960403     C*
030500960403     C* SE CONTROLLO
030600960325     C     ENDCT1        ENDSR
030700960403     C*
030800960405     C* CONTROLLO I FOGLI IMA ----------------------------------------*
030900960405     C     CTRIMA        BEGSR
031000960403     C                   MOVEL     WDEL08        KDFV
031100960403     C                   CLEAR                   WTROV
031200050805     c**
031300050805     C**
031400960403     C                   MOVEL     V1CFGS        KFGS
031500960403     C     KFVV2         CHAIN     FNFVV02L                           30
031600960403     C*
031700960403    2C     *IN30         DOWEQ     *OFF
031800960405     C* TROVATO FOGLIO IMA NON ANNULLATO
031900960403    3C     FVVSPG        IFEQ      'A'
032000960403     C     FVVATB        ANDEQ     ' '
032100960403     C*
032200960403     C                   MOVEL     'S'           WTROV             1
032300960403     C* FOGLIO APERTO
032400960403    4C     FVVFCF        IFNE      'S'
032500960403     C                   EXSR      MSGAPE
032600960403    4C                   ENDIF
032700960403    3C                   ENDIF
032800050805     C**
032900960403     C  N90KFVV2         READE     FNFVV02L                               30
033000960403    2C                   ENDDO
033100960403     C*
033200960403     C* FOGLIO IMA NO  TROVATO IN DATA DI CONFERMA IDD
033300960405    3C  N90WTROV         IFEQ      ' '
033400960405     C                   CLEAR                   FVVNFV
033500960405     C                   MOVEL     V1CFGS        FVVFGS
033600960403     C                   EXSR      MSGMAN
033700960403    2C                   ENDIF
033800960403     C*
033900960403     C                   ENDSR
034000960405     C*
034100960405     C* CONTROLLO I FOGLI IDA ----------------------------------------*
034200960405     C     CTRIDA        BEGSR
034300960508     C                   MOVE      *ZEROS        II                3 0
034400960405     C                   MOVEL     WDEL08        KDFV
034500960509     C     CHKFVV        TAG
034600960405     C                   CLEAR                   WTROV2
034700960405     C                   MOVEL     2             KNPG
034800960405     C*
034900960405     C* PER ORA ALMENO, IL CONTROLLO DELL'IMA E' OGNUNO IL PROPRIO
035000960405     C* SE NON 'C'E O E' APERTO
035100960405     C                   MOVEL     V1CFGS        KFGS
035200960405     C     KFVV2         CHAIN     FNFVV02L                           30
035300960405     C*
035400960405    2C     *IN30         DOWEQ     *OFF
035500961030     C     *IN90         ANDEQ     *OFF
035600960405     C* TROVATO FOGLIO NON ANNULLATO
035700960405    3C     FVVATB        IFEQ      ' '
035800960405     C*
035900960405    4C     KNPG          IFEQ      2
036000960405     C                   MOVEL     'S'           WTROV2            1
036100121115     c                   endif
036200960405     C* FOGLIO APERTO
036300960405    4C     FVVFCF        IFNE      'S'
036400961030     C     KFVA          SETLL     FNFVA02L                               32
036500961030     C* EMETTO MSG DI FOGLIO APERTO SOLO SE HA ALMENO UN FVP ABBINATO
036600961030    5C     *IN32         IFEQ      *ON
036700960405     C                   EXSR      MSGAPE
036800961030    5C                   ENDIF
036900960405    4C                   ENDIF
037000960405    3C                   ENDIF
037100960405     C*
037200960405     C  N90KFVV2         READE     FNFVV02L                               30
037300960405    2C                   ENDDO
037400960405     C*
037500960405     C*
037600960405     C* FOGLIO IMA NO  TROVATO IN DATA DI CONFERMA IDD
037700960509    1C  N90WTROV2        IFEQ      ' '
037800960508     C* VERIFICO SE GIORNO PRECEDENTE FESTIVO
037900960508     C                   EXSR      CTRFES
038000960509    2C     WFES          IFEQ      'F'
038100960508     C* . . . . . . .     E' FESTIVO ->CERCO FOGLIO IDA CON QUESTA DATA
038200960508     C                   MOVE      WDELP         KDFV
038300960509     C                   GOTO      CHKFVV
038400960509   X2C                   ELSE
038500960508     C* . . . . . . . NON E' FESTIVO ->ERRORE BLOCCANTE SE SONO TER-ARR
038600961015    3C     WTEREC        IFEQ      'S'
038700961015    3C     WTEROR        OREQ      'S'
038800960508     C                   CLEAR                   FVVNFV
038900960508     C                   MOVEL     V1CFGS        FVVFGS
039000960508     C                   EXSR      MSGMAN
039100960509    3C                   ENDIF
039200960508     C*
039300960509    2C                   ENDIF
039400960509    1C                   ENDIF
039500960405     C                   ENDSR
039600960325     C*
039700960405     C* PREPARO MESSAGGIO "IXA MANCANTE" ----------------------------*
039800960325     C     MSGMAN        BEGSR
039900960405     C*
040000960403     C     KNPG          IFEQ      3
040100960405     C     W0090         LOOKUP    IMA                                    31
040200960403     C                   ELSE
040300960405     C     W0090         LOOKUP    IDA                                    31
040400960403     C                   ENDIF
040500960325     C*
040600960325    1C     *IN31         IFEQ      *OFF
040700961015     C     WTEROR        OREQ      'S'
040800961015     C     KNPG          ANDNE     3
040900961015     C     WTEREC        OREQ      'S'
041000960509     C     KNPG          ANDNE     3
041100960325     C                   Z-ADD     1             K                 3 0
041200960403     C     KNPG          IFEQ      3
041300960405     C     WNOVE         LOOKUP    IMA(K)                                 32
041400960405     C   32              Z-ADD     W0090         IMA(K)
041500960403     C                   ELSE
041600960405     C     WNOVE         LOOKUP    IDA(K)                                 32
041700960405     C   32              Z-ADD     W0090         IDA(K)
041800960403     C                   ENDIF
041900960325     C*
042000960325     C                   MOVEA     MSG(8)        K78
042100960405     C                   MOVEL     FVVFGS        W003A             3
042200960926     C                   MOVEA     W003A         K78(44)
042300960403     C     KNPG          IFEQ      3
042400960403     C                   MOVEL     'M'           K78(9)
042500960403     C                   ELSE
042600960403     C                   MOVEL     'D'           K78(9)
042700961015     C     WTEROR        IFEQ      'S'
042800961015     C     WTEREC        OREQ      'S'
042900960926     C                   MOVEA     *BLANKS       K78(47)
043000960509     C                   END
043100960403     C                   ENDIF
043200960325     C                   MOVEA     K78           V1CMSG
043300960325     C                   SETON                                        412890
043400960325    1C                   ENDIF
043500960403     C*
043600960325     C                   ENDSR
043700960325     C*
043800960325     C* PREPARA MESSAGGIO DI F.IMA APERTO----------------------------*
043900960325     C     MSGAPE        BEGSR
044000960405     C*
044100960405     C     KNPG          IFEQ      3
044200960405     C     W0090         LOOKUP    IMA                                    31
044300960405     C                   ELSE
044400960405     C     W0090         LOOKUP    IDA                                    31
044500960405     C                   ENDIF
044600960509     C*
044700960509     C* SE MESSAGGIO PER IDA APERTO E' BLOCCANTE SE SONO TER-ARR,
044800960509     C* ALTRIMENTI MSG FORZABILE
044900960325    1C     *IN31         IFEQ      *OFF
045000961015     C     WTEROR        OREQ      'S'
045100960509     C     KNPG          ANDNE     3
045200961015     C     WTEREC        OREQ      'S'
045300961015     C     KNPG          ANDNE     3
045400960325     C                   Z-ADD     1             K
045500960405     C**
045600960405     C     KNPG          IFEQ      3
045700960405     C     WNOVE         LOOKUP    IMA(K)                                 32
045800960405     C   32              Z-ADD     W0090         IMA(K)
045900960405     C                   ELSE
046000960405     C     WNOVE         LOOKUP    IDA(K)                                 32
046100960405     C   32              Z-ADD     W0090         IDA(K)
046200960405     C                   ENDIF
046300960325     C*
046400960325     C                   MOVEA     MSG(9)        K78
046500960325     C                   Z-ADD     11            I                 2 0
046600960325     C                   MOVEL     FVVNFV        W005A             5
046700960325     C                   MOVEA     W005A         SKI(I)
046800960325     C                   EXSR      ABBL
046900960325     C                   MOVEA     SKI(I)        K78(12)
047000960405     C                   MOVEL     FVVFGS        W003A             3
047100960926     C                   MOVEA     W003A         K78(52)
047200960403     C     KNPG          IFEQ      3
047300960403     C                   MOVEL     'M'           K78(9)
047400960403     C                   ELSE
047500960403     C                   MOVEL     'D'           K78(9)
047600961015     C     WTEREC        IFEQ      'S'
047700961015     C     WTEROR        OREQ      'S'
047800960926     C                   MOVEA     *BLANKS       K78(55)
047900960509     C                   END
048000960403     C                   ENDIF
048100960325     C                   MOVEA     K78           V1CMSG
048200960325     C                   SETON                                        412890
048300960325     C                   SETON                                        30
048400960325    1C                   ENDIF
048500960325     C*
048600960325     C                   ENDSR
048700960405     C*
048800960405     C* CONTROLLO SE HANNO FATTO IDD ---------------------------------*
048900960405     C     CTRIDD        BEGSR
049000960405     C                   Z-ADD     1             X
049100960405     C*
049200960405    1C     L1(X)         DOWGT     0
049300960405     C     *IN90         ANDEQ     *OFF
049400960405     C* NON CONTROLLO ME STESSO
049500960405    2C     L1(X)         IFNE      V1CFGS
049600960422     C* NON CONTROLLO SE IN ORGANIGRAMMA C'E' SCRITTO CHE NON FA IDD
049700960422     C                   Z-ADD     L1(X)         W0030             3 0
049800960422     C     W0030         CHAIN     AZORG01L                           30
049900960422     C  N30              MOVEL     ORGDE8        OG148
050000960422     C   30              CLEAR                   OG148
050100960422    3C     §OGNID        IFEQ      ' '
050200960405     C*
050300960405     C                   Z-ADD     95            KCAA
050400960405     C                   Z-ADD     L1(X)         KLNA
050500990513     C     KANM4         SETLL     FNANM04L                               30
050600960405     C*
050700960422    4C     *IN30         IFEQ      *OFF
050800960405     C     KLNA          LOOKUP    L6S                                    31
050900051111     c* Escludo p.o. esteri
051000051111     C  n31KLNA          LOOKUP    EST                                    31
051100960405     C* VEDO SE HO GIA' DATO IL MESSAGGIO
051200960405     C  N31KLNA          LOOKUP    IDD                                    31
051300960422    5C     *IN31         IFEQ      *OFF
051400960405     C*
051500960405     C* ERRORE SE NON C'E' OK IDD E NON E' GESTITA DA ALTRE
051600960405     C                   Z-ADD     1             K
051700960405     C     WTRE          LOOKUP    IDD(K)                                 32
051800960405     C   32              MOVEL     L1(X)         IDD(K)
051900960405     C*
052000960405     C                   MOVEA     MSG(10)       K78
052100960405     C                   MOVEL     L1(X)         W003A             3
052200960926     C                   MOVEA     W003A         K78(52)
052300960405     C                   MOVEA     K78           V1CMSG
052400960405     C                   SETON                                        412890
052500960422    5C                   ENDIF
052600960422    4C                   ENDIF
052700960405    3C                   ENDIF
052800960405    2C                   ENDIF
052900960405     C*
053000960405     C                   ADD       1             X
053100960405    1C                   ENDDO
053200960405     C*
053300960405     C                   ENDSR
053400960405     C*
053500960405     C* CONTROLLO SE IL MIO TERMINAL DI ARRIVO HA FATTO L'IDA --------*
053600960405     C     IDATER        BEGSR
053700960405     C                   Z-ADD     1             X
053800961016     C*
053900961016     C                   CLEAR                   DSLV56
054000990513     C                   MOVE      WDEL08        D56DAO
054100961016     C                   MOVE      WTGI          D56TGI
054200050805     C                   MOVE      fgsTFA        D56FE0
054300961016     C                   MOVEL     DSLV56        KPJBU
054400961016     C                   CALL      'FNLV56R'
054500961016     C                   PARM                    KPJBA
054600961016     C                   MOVEL     KPJBU         DSLV56
054700960405     C*
054800961016     C     D56ERR        IFEQ      'N'
054900961016     C*  NON TROVATO OK IDA DEL TERMINAL:ERRORE BLOCCANTE
055000960405     C                   MOVEA     MSG(11)       K78
055100050805     C                   MOVEL     fgsTFA        W003A             3
055200960509     C                   MOVEA     W003A         K78(62)
055300960405     C                   MOVEA     K78           V1CMSG
055400960405     C                   SETON                                        412890
055500960405     C                   ENDIF
055600960405     C*
055700960405     C                   ENDSR
055800960325     C*
055900960926      * CONTROLLO IL P.O. IN IN GESTIONE -----------------------------*
056000960325     C     CTRFGS        BEGSR
056100960325     C                   SETOFF                                       90
056200960325     C*
056300960325     C     V1CFGS        LOOKUP    L1                                     30
056400960325     C     *IN30         IFEQ      *OFF
056500960325     C     V1CFGS        OREQ      0
056600960325     C                   MOVEL     MSG(2)        V1CMSG
056700960325     C                   CLEAR                   V1DFGS
056800960325     C                   SETON                                        409028
056900960325     C                   GOTO      ENDCTF
057000960325     C                   ENDIF
057100960325     C* DECODIFICA
057200960325     C     V1CFGS        CHAIN     AZORG                              31
057300960325     C  N31              MOVEL     ORGDES        V1DFGS
057400980220     C* MEMORIZZO SE P.O. FA L'IDD
057500980220     C  N31              MOVEL     ORGDE8        OG148
057600980220     C   31              CLEAR                   OG148
057700980220     C                   MOVEL     §OGNID        WOGNID            1
057800960409     C*
057900960409     C                   MOVEL     '7A'          COD
058000960409     C                   MOVEL(P)  V1CFGS        KEY
058100960409     C     KTAB          CHAIN     TABEL                              31
058200960409     C     *IN31         IFEQ      *OFF
058300960409     C                   MOVEL     TBLUNI        DS7AF
058400960409     C                   MOVE      §7ADID        WDID
058500960409     C                   SETON                                        02
058600960409     C                   ELSE
058700960409     C                   CLEAR                   WDID
058800960409     C* ERRORE DATA ULTIMA ESECUZIONE IDD NON REPERIBILE
058900960409     C                   MOVEL     MSG(1)        V1CMSG
059000960918     C                   SETON                                        2890
059100960918     C                   GOTO      ENDCTF
059200960409     C                   END
059300960325     C*
059400960926     C* GESTITA DA ALTRO P.O.
059500960325     C     V1CFGS        LOOKUP    L6S                                    90
059600960325     C     *IN90         IFEQ      *ON
059700960325     C                   SETON                                        4028
059800960325     C                   MOVEL     MSG(3)        V1CMSG
059900960325     C                   GOTO      ENDCTF
060000960325     C                   ENDIF
060100960325     C*
060200960325     C     ENDCTF        ENDSR
060300960325     C* ABBLANCA ZERO NON SIGNIFICATIVI -----------------------------*
060400960325     C     ABBL          BEGSR
060500960325     C*
060600960325     C     SKI(I)        DOWEQ     '0'
060700960325     C     I             ANDLT     15
060800960325     C                   MOVEL     ' '           SKI(I)
060900960325     C                   ADD       1             I
061000960325     C                   ENDDO
061100960325     C*
061200960325     C                   ENDSR
061300050805     C*
061400960508      * CONTROLLO SE GIORNO FESTIVO ----------------------------------*
061500960508     C     CTRFES        BEGSR
061600960508     C                   MOVE      *BLANKS       WFES              1
061700960508     C                   CLEAR                   WDELP
061800960508     C                   ADD       1             II
061900960508     C* DETERMINO GIORNO PRECEDENTE
062000960508     C                   CLEAR                   WGIDAT
062100960508     C     WTGI          SUB       II            GI8TGI
062200960509     C                   CALL      'XSRGI8'
062300960508     C                   PARM                    WGIDAT
062400960508     C                   Z-ADD     GI8INV        WDELP
062500960508     C* VERIFICO SE FESTIVO LEGGENDO CALENDARIO BARTOLINI
062600960508     C                   MOVEL     GI8INV        KANN
062700960508     C                   MOVE      GI8INV        W0040             4 0
062800960508     C                   MOVEL     W0040         KMES
062900960508     C                   MOVE      W0040         GG                2 0
063000960508     C                   CLEAR                   KTFP
063100050805     C                   MOVE      fgsTFA        KTFA
063200960508     C     KCLN          CHAIN     AZCLN01L                           31
063300960508    1C     *IN31         IFEQ      *OFF
063400960508     C     MAT(GG)       ANDNE     *BLANKS
063500960508     C                   MOVEL     '1Y'          COD
063600960508     C                   MOVEL(P)  MAT(GG)       KEY
063700960508     C     KTAB          CHAIN     TABEL                              31
063800960508    2C     *IN31         IFEQ      *OFF
063900960508     C                   MOVEL     TBLUNI        DS1Y
064000960508    3C     §1YFEI        IFEQ      'S'
064100960508     C                   MOVEL     'F'           WFES
064200960508    3C                   END
064300960508    2C                   END
064400960508    1C                   END
064500960508     C*
064600960508     C                   ENDSR
064700960325     C*--------------------------------------------------------------*
064800960325     C     *INZSR        BEGSR
064900960325     C* KLIST
065000960325     C     KFVV2         KLIST
065100960325     C                   KFLD                    KNPG
065200960325     C                   KFLD                    KDFV
065300960325     C                   KFLD                    KFGS
065400961030     C     KFVA          KLIST
065500961030     C                   KFLD                    FVVNPG
065600961030     C                   KFLD                    FVVNFV
065700020917     c                   Kfld                    FvvFgs
065800960325     C     KFVV21        KLIST
065900960325     C                   KFLD                    KNPG
066000960325     C                   KFLD                    KDFV
066100960327     C     KTAB          KLIST
066200960327     C                   KFLD                    CODUT
066300960327     C                   KFLD                    COD
066400960327     C                   KFLD                    KEY
066500960405     C     KANM4         KLIST
066600960405     C                   KFLD                    KCAA
066700990513     C                   KFLD                    WDEL08
066800960405     C                   KFLD                    KLNA
066900960508     C     KCLN          KLIST
067000960508     C                   KFLD                    KTFP
067100960508     C                   KFLD                    KTFA
067200960508     C                   KFLD                    KANN
067300960508     C                   KFLD                    KMES
067400960508     C     KCAE          KLIST
067500960508     C                   KFLD                    KEPA
067600960508     C                   KFLD                    V1CFGS
067700970717     C     KCAE1         KLIST
067800970717     C                   KFLD                    CAEEPA
067900970717     C                   KFLD                    CAETFA
068000970717     C                   KFLD                    CAETFP
068100970717     C                   KFLD                    WDEL08
068200960508     C                   MOVEL     'A'           KEPA
068300960325     C*
068400960327     C     *LIKE         DEFINE    TBLCOD        COD
068500960327     C     *LIKE         DEFINE    TBLKEY        KEY
068600960325     C     *LIKE         DEFINE    FVVDFV        KDFV
068700960325     C     *LIKE         DEFINE    V1CDEL        SAVDEL
068800960325     C     *LIKE         DEFINE    FVVFGS        KFGS
068900960405     C     *LIKE         DEFINE    ANMLNA        KLNA
069000960405     C     *LIKE         DEFINE    ANMCAA        KCAA
069100960409     C     *LIKE         DEFINE    §7ADID        WDID
069200960509     C     *LIKE         DEFINE    WDEL08        WDELP
069300960508     C     *LIKE         DEFINE    G02TGI        WTGI
069400960508     C     *LIKE         DEFINE    CLNANN        KANN
069500960508     C     *LIKE         DEFINE    CLNMES        KMES
069600960508     C     *LIKE         DEFINE    CLNTFP        KTFP
069700960508     C     *LIKE         DEFINE    CLNTFA        KTFA
069800960508     C     *LIKE         DEFINE    CAEEPA        KEPA
069900960325     C**
070000960405     C                   MOVEL     *ZEROS        WNOVE             9 0
070100960405     C                   MOVEL     *ZEROS        WTRE              3 0
070200960325     C**
070300960325     C*--------------------------------------------------------------*
070400960325     C*
070500960325     C     *ENTRY        PLIST
070600960325     C                   PARM                    KPJBA
070700960325     C                   Z-ADD     1             CODUT
070800960325     C                   CALL      'X§PARUT'
070900960325     C                   PARM                    UT§DSE
071000960325     C                   MOVEL     REC80         CNCR80
071100960325     C                   MOVEL     RAGUT         RSUT             20
071200960325     C*
071300020429     C                   Z-ADD     SIMPOU        V1CFGS
071400020429     C     SIMTPP        IFEQ      '2'
071500020429     C     SIMTPP        OREQ      *BLANKS
071600020429     C                   SETON                                        06
071700020429     C                   ENDIF
071800960325     C*
071900961209     C* SE PROFILO "ISP99" COME PUNTO OPERATIVO IMPOSTO "101"
072000961209     C     KNMUS         IFEQ      CPRF
072100961209     C                   Z-ADD     101           V1CFGS
072200961209     C                   ENDIF
072300961209     C*
072400960325     C     V1CFGS        CHAIN     AZORG01L                           31
072500960325     C  N31              MOVEL     ORGDES        V1DFGS
072600980220     C* MEMORIZZO SE P.O. FA L'IDD
072700980220     C  N31              MOVEL     ORGDE8        OG148
072800980220     C   31              CLEAR                   OG148
072900980220     C                   MOVEL     §OGNID        WOGNID            1
073000960325     C*
073100960325     C*---------------------------------------------------------------*
073200960325     C* GIRO UDATE
073300960325     C                   Z-ADD     UDATE         G02DAT
073400960325     C                   MOVEL     *BLANK        G02ERR
073500960325     C                   CALL      'XSRDA8'
073600960325     C                   PARM                    WLBDAT
073700960325     C                   Z-ADD     G02INV        DATEU             6 0
073800960325     C                   Z-ADD     G02DAT        V1CDEL
073900960325     C*
074000960325     C* DOPO ELABORAZIONE DEFINITIVA NON VISUALIZZO F1  ED F6
074100960409     C*
074200960409     C                   MOVEL     '7A'          COD
074300960409     C                   MOVEL(P)  V1CFGS        KEY
074400960409     C     KTAB          CHAIN     TABEL                              31
074500960409     C     *IN31         IFEQ      *OFF
074600960409     C                   MOVEL     TBLUNI        DS7AF
074700960409     C                   MOVE      §7ADID        WDID
074800960409     C                   SETON                                        02
074900960409     C                   ELSE
075000960409     C                   CLEAR                   WDID
075100960409     C* ERRORE DATA ULTIMA ESECUZIONE IDD NON REPERIBILE
075200960409     C                   MOVEL     MSG(1)        V1CMSG
075300960409     C                   SETON                                        28
075400960409     C                   END
075500960327     C**
075600960327     C* CARICO LA DS7A2
075700960327     C**
075800960327     C                   MOVEL     '7A'          COD
075900960327     C                   MOVEL(P)  '2'           KEY
076000960327     C     KTAB          CHAIN     TABEL                              31
076100960327     C     *IN31         IFEQ      *OFF
076200960327     C     TBLFLG        ANDEQ     ' '
076300960327     C                   MOVEL     TBLUNI        DS7A2
076400960327     C                   ELSE
076500960327     C                   CLEAR                   DS7A2
076600960327     C                   ENDIF
076700050504      * Aggancio tabella "VPO" per ora conferma idd
076800050504     C                   CLEAR                   dsbs02
076900050504     c                   clear                   dvpo
077000050504     C                   MOVEL     'C'           T02MOD
077100050504     C                   MOVEL     KNSIF         T02SIF
077200050504     C                   MOVEL     'VPO'         T02COD
077300050504     c                   movel(P)  'VPO'         t02ke1
077400050504     C                   CALL      'TIBS02R'
077500050504     C                   PARM                    KPJBA
077600050504     C                   PARM                    dsbs02
077700050504    2C                   IF        T02ERR = *BLANKS
077800050504     C                   MOVEL     T02uni        dvpo
077900050504    2C                   ENDIF
078000050805     C***
078100050805     C* CARICO TABELLA P.O.  GESTITI £1
078200050805     C***
078300050805     C                   CLEAR                   L1
078400050805     C                   CLEAR                   DSUL06
078500050805     C                   MOVE      '£1'          D06COD
078600050805     C                   MOVEL     SIMFEL        D06KEY
078700050805     C                   MOVEL     DSUL06        KPJBU
078800050805     C                   CALL      'TRUL06R'
078900050805     C                   PARM                    KPJBA
079000050805     C                   MOVEL     KPJBU         DSUL06
079100050805     C                   MOVEA     LIN           L1
079200960327     C*
079300960325     C                   ENDSR
079400050805     c*
079500050805     c* Carico L6 in base alla data elaborazione --------------------
079600050805     c     CARTAB        BEGSR
079700050805     C***
079800050805     C* CARICO TUTTE I P.O. GESTITI DA ALTRI (DA £6)
079900050805     C***
080000050805     C                   CLEAR                   L6S
080100050805     C                   CLEAR                   DSUL06
080200050805     C                   MOVE      '£6'          D06COD
080300050805     C**!!!              MOVEL     'S'           D06ESC
080400050805     c                   eval      d06esc = 'G'
080500050805     c                   movel     simfel        d06key
080600050805     c                   move      'S'           d06key
080700050805     c                   movel     Wdel08        d06drf
080800050805     C                   MOVEL     DSUL06        KPJBU
080900050805     C                   CALL      'TRUL06R'
081000050805     C                   PARM                    KPJBA
081100050805     C                   MOVEL     KPJBU         DSUL06
081200050805     C                   MOVEA     LIN           L6S
081300050830     c***                z-add     1             L                 3 0
081400050830     c***  000           lookup    L6S(L)                                 30
081500050805     c
081600050805     c
081700050830     c* Carico a parte anche i p.o. esteri
081800050830     c                   z-add     1             L                 3 0
081900050805     C     *LOVAL        SETLL     AZORG01L
082000050805     C                   READ      AZORG01L                               30
082100050805     C*
082200050805    2C     *IN30         DOWEQ     *OFF
082300050805     c     L             andle     30
082400050805    3C     ORGFAG        IFEQ      'F'
082500050805     C     ORGFAG        OREQ      'A'
082600050805     C* P.O. NON ANNULLATO
082700050805    4C     ORGFVA        IFEQ      ' '
082800050805     C                   MOVEL     ORGDE3        OG143
082900050805     C*
083000050805    5C     §OGNTW        IFEQ      'EEX'                                        ESTERA
083100050805     C     §OGNTW        OREQ      'EUP'
083200050805     C     §OGNTW        OREQ      'FED'
083300050805     C     §OGNTW        OREQ      'DPD'                                        DPD
083400050805     C*
083500050805     C* VEDO SE HA COME TERMINAL DI PARTENZA LA FIL.GESTIONE
083600050805     C                   CLEAR                   DSLV55
083700050805     C                   MOVEL     'P'           D55TPT
083800050805     C                   MOVEL     ORGFIL        D55LIN
083900050805     C                   MOVEL     wdel08        D55DRF
084000050805     C                   CALL      'FNLV55R'
084100050805     C                   PARM                    DSLV55
084200050805    6C     D55ERR        IFNE      ' '
084300050805     C                   MOVEL     D55LIN        D55TFP
084400050805    6C                   ENDIF
084500050805    6C     D55TFP        IFEQ      simfel
084600050830     C                   Z-ADD     ORGFIL        EST(L)
084700050805     C                   ADD       1             L
084800050805    6C                   ENDIF
084900050805    5C                   ENDIF
085000050805    4C                   ENDIF
085100050805    3C                   ENDIF
085200050805     C*
085300050805     C                   READ      AZORG01L                               30
085400050805    2C                   ENDDO
085500050805     c                   ENDSR
085600961209** MSG
085700960409Impossibile lanciare l'elaborazione IDD: telefonare in sede                    1
085800960926Punto operativo in gestione errato
085900960926Punto operativo errato: in gestione ad altro P.O.                              3
086000960325Data elaborazione errata
086100980220Impossibile eseguire IDD DEFINITIVO  per il punto operativo richiesto
086200960325Impossibile eseguire IDD PROVVISORIO : gia' confermato nella data richiesta    6
086300960325Impossibile eseguire IDD DEFINITIVO : gia' confermato nella data richiesta
086400960926Foglio IMA mancante per il punto operativo xxx : ENTER PER FORZARE            8
086500020918Foglio IMA       non chiuso per il punto operativo xxx : ENTER PER FORZARE
086600960926Manca l'elaborazione I.D.D. per il punto operativo xxx : ENTER PER FORZARE
086700960508Manca l'elaborazione dell'IDA del proprio terminal di arrivo xxx
086800961209Profilo non abilitato al lancio dell'IDD DEFINITIVO per il p.o. immesso !!    12
086900050504Impossibile eseguire IDD definitivo prima delle ore xx,xx                     13
087000050830Impossibile eseguire IDD definitivo per P.O. estero                           14

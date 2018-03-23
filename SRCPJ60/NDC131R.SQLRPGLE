000100981217     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300980608     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000400000601B0218H*PARMS DYNUSRPRF(*OWNER) RDB(*NONE)
000500980608     H DECEDIT('0,') DATEDIT(*DMY.)
000600980608     H*----------------------------------------------------*
000700980608     H*
000800981217    >H*            Stampa mastrini: selezioni
000900980615     H*           ----------------------------
001000950316     H*
001100940830     F*----------------------------------------------------*
001200950508     FANMPE01W  IF   E           K DISK
001300980608     D*-------------
001400980608    >D* ds dei file che servono per l'sql
001500980608    >D ANRCO         E DS                  extname(ANRCO00F)
001600980703     D*-------------
001700940830     D* Passaggio Parametri
001800940830     D KPJBA         E DS
001900940830     D*-------------
002000941129     D* Dati di ambiente ottenuti da XSOC
002100941129     D SOC001        E DS                  EXTNAME(XSOC001DS)
002200941129     D*-------------
002300941129     D* DS Interna per dati di output di XSOC
002400941129     D XSOCDS          DS          1000
002500950315     D*-------------
002600950315     D* Parametri per stampa prima pagina
002700960527     D ANA968DS      E DS                  INZ
002800950315     D*-------------
002900950131     D* Parametri del PGM di reperimento schiera selezioni
003000960527     D ANA969DS      E DS                  INZ
003100950131     D  S969         E                     EXTFLD(SEL969)
003200950131     D                                     DIM(5000)                            parametri
003300950131     D  O969         E                     EXTFLD(ORD969)
003400950131     D                                     DIM(2000)                               "
003500950315     D*-------------
003600950203     D* Parametri per programma di controllo
003700960527     D ANA971DS      E DS                  INZ
003800950315     D*-------------
003900950621     D* Parametri di lancio
004000981217    >D NDC131DS      E DS                  INZ
004100950621     D*-------------
004200981218     D* Parametri passati dal PGM
004300981218     D C132DS        E DS                  EXTNAME(NDC132DS)
004400981218     D*-------------
004500940830     D* Reperimento nome PGM
004600940830     D STATUS         SDS           333
004700940830     D  DSPGM            *PROC
004800950315     D*-------------
004900950201     D* Schiera selezione file
005000950316     D SKK             DS
005100950316    >D  SK                     1    320
005200950206     D                                     DIM(4) CTDATA PERRCD(1)              schiere
005300950316    >D                 DS
005400950316    >D  SK1                    1     80
005500950315     D                                     DIM(80) CTDATA PERRCD(80)            schiera 1 sql
005600950316    >D  SK1X                   1     80
005700950315     D*-------------
005800950131     D* Stringa per SQL
005900950131     D SQ              DS          5000
006000950131     D* Selezioni
006100950131     D SELX            DS
006200950131     D  SEL                    1   5000
006300950131     D                                     DIM(5000)                            parametri
006400950131     D* Ordinamenti
006500950131     D ORDX            DS
006600950131     D  ORD                    1   2000
006700950131     D                                     DIM(2000)                               "
006800950203     D* tracciato
006900950203     D                 DS
007000950203     D  TRC                    1    100
007100950203     D                                     DIM(10)                              tracciati  anreo00f
007200950203     D  TRCX                   1    100
007300950202     D* campi ordinamento
007400950202     D                 DS
007500950202     D  FLD                    1    100
007600950203     D                                     DIM(10)                              campi ord. anreo00f
007700950202     D  FLDX                   1    100
007800950202     D* tipi rottura
007900950202     D                 DS
008000950202     D  ROT                    1     10
008100950202     D                                     DIM(10)                              tipi rottura
008200950202     D  ROTX                   1     10
008300950206     D* descrizioni campi
008400950206     D                 DS
008500950206     D  TIT                    1    200
008600950206     D                                     DIM(10)                              titoli x totali
008700950206     D  TITX                   1    200
008800980608     D*-------------
008900980615     D* Parametri per XDATCD
009000980615     D XCod            S              2  0
009100980615     D XDat            S               D
009200980615     D*-------------
009300980703     D* Costante
009400980703     D FETCH           C                   CONST('FOR FETCH ONLY')
009500980703     D*-------------
009600940830     C*----------------------------------------------------*
009700940830     C*                MAIN LINE PROGRAM
009800940830     C*----------------------------------------------------*
009900940830     C*
010000940830     C* inizializzazione variabili
010100940830     C                   EXSR      INZVAR
010200950131     C*
010300950131     C* Reperimento schiera per SQL
010400950131     C                   EXSR      REPSQL
010500941129     C*
010600950131     C* Esecuzione SQL
010700950131     C                   EXSR      EXESQL
010800940830     C*
010900940830     C* fine programma
011000940830     C                   EXSR      ENDPGM
011100940830     C*
011200940830     C/EJECT
011300940830     C************************************************************
011400940830     C* FINE PROGRAMMA
011500940830     C************************************************************
011600940830     C     ENDPGM        BEGSR
011700980608     C*
011800980615     C* Ripasso la ds dei parametri
011900981217     C                   MOVEL     NDC131DS      KPJBU
012000980615     C*
012100940830     C                   SETON                                            LR
012200940905     C                   RETURN
012300940830     C*
012400940830     C                   ENDSR
012500950127     C/EJECT
012600940830     C************************************************************
012700950131     C* REPERIMENTO SCHIERA PER SQL
012800940830     C************************************************************
012900950131     C     REPSQL        BEGSR
013000950131     C*
013100950315     C* Imposta la società di elaborazione del programma
013200950315     C*
013300950201     C     SQ            CAT       SKK:0         SQ
013400950215     C                   MOVE      MPESOC        WRKSOC            3
013500950316     C*
013600950316    >C* Controllare la posizione della società nella schiera SK1
013700981218    >C                   MOVEA     WRKSOC        SK1(19)
013800950316    >C     SQ            CAT       SK1X:1        SQ
013900950131     C*
014000950131     C* Reperisco selezioni/ordinamenti
014100950131     C*
014200950215     C                   MOVE      MPESOC        SOC969
014300950208     C                   Z-ADD     MPEREG        NUM969
014400950508     C                   Z-ADD     LAN968        LAN969
014500950131     C                   CALL      'ANA969R'
014600950131     C                   PARM                    KPJBA
014700960527     C                   PARM                    ANA969DS
014800941129     C*
014900950131     C     ERR969        IFNE      *ON
015000950131     C                   MOVEA     S969          SEL
015100950131     C                   MOVEA     O969          ORD
015200950131     C* Se esistono altre selezioni aggiungo 'AND' per l'sql
015300950131     C     SELX          IFNE      *BLANKS
015400950131     C     SQ            CAT       'AND':1       SQ
015500950131     C                   ENDIF
015600950131     C*
015700950131     C     SQ            CAT       SELX:1        SQ
015800950131     C     SQ            CAT       ORDX:1        SQ
015900950629     C     SQ            CAT       FETCH:1       SQ
016000950131     C                   ENDIF
016100950202     C*
016200950206     C* Riempio schiera campi ordinamento, tipi rottura e titoli
016300950202     C*
016400950206     C                   MOVEL     TRC969        TRCX
016500950202     C                   MOVEL     FLD969        FLDX
016600950202     C                   MOVEL     ROT969        ROTX
016700950206     C                   MOVEL     DEB969        TITX
016800950201     C*
016900940905     C                   ENDSR
017000940905     C/EJECT
017100940905     C******************************************************
017200950131     C* ESECUZIONE SQL
017300940905     C******************************************************
017400950131     C     EXESQL        BEGSR
017500950131     C*
017600950131     C/EXEC SQL
017700950131     C+     PREPARE S1 FROM :SQ
017800950131     C/END-EXEC
017900950131     C*
018000950131     C/EXEC SQL
018100950131     C+     DECLARE A1 CURSOR FOR S1
018200950131     C/END-EXEC
018300950131     C*
018400950131     C/EXEC SQL
018500950131     C+     OPEN A1
018600950131     C/END-EXEC
018700950131     C*
018800950201     C/EXEC SQL
018900981217    >C+     FETCH A1 INTO :ANRCO
019000950201     C/END-EXEC
019100950718     C*
019200030801D1414X***  SQLCOD        DOWEQ     0
019300030801  "  C     SQLCOD        DowGE     0
019400030801D1414C     SQLCOD        andNE     100
019500950201     C*
019600980608     C* Elaborazione dettaglio
019700980608     C                   EXSR      ELADET
019800980608     C*
019900950201     C/EXEC SQL
020000981217    >C+     FETCH A1 INTO :ANRCO
020100950201     C/END-EXEC
020200950202     C*
020300950202  E11C                   ENDDO
020400981218     C*
020500981218     C* a fine elaborazione chiama pgm di stampa x totali e legenda causali
020600981218     C                   reset                   c132ds
020700981218     C                   movel     soc131        soc132
020800981218     C                   movel     uni131        uni132
020900981218     C                   movel     ctb131        ctb132
021000981218     C                   movel     dtc131        dtc132
021100981218     C                   movel     seo131        seo132
021200981218     C                   movel     dti131        dti132
021300981218     C                   movel     dtf131        dtf132
021400981218     C*                  move      tpr131        tpr132
021500981218     C                   move      def131        def132
021600981218     C                   move      pro131        pro132
021700981218     C                   move      ges131        ges132
021800981218     C                   move      det131        det132
021900981218     C                   move      nmo131        cnm132
022000981218     C                   move      slm131        slm132
022100981218     C                   move      slp131        slp132
022200981218     C                   move      cau131        cau132
022300981218     C                   move      *on           fin132
022400981218     C                   move      *on           lr132
022500041012>>>>>C                   EVAL      Ycd132 = Ycd131
022600981218     C*
022700981218     C                   movel     c132ds        kpjbu
022800981218     C                   call      'NDC132R'
022900981218     C                   parm                    kpjba
023000981218     C*
023100950131     C/EXEC SQL
023200950131     C+     CLOSE A1
023300950131     C/END-EXEC
023400980608     C*
023500940905     C                   ENDSR
023600940905     C/EJECT
023700950131     C******************************************************
023800980608     C* ELABORAZIONE DETTAGLIO
023900950131     C******************************************************
024000980608     C     ELADET        BEGSR
024100980608     C*
024200981218     C* passa i dati al pgm di stampa
024300981218     C                   reset                   c132ds
024400981218     C                   movel     soc131        soc132
024500981218     C                   movel     uni131        uni132
024600981218     C                   movel     ctb131        ctb132
024700981218     C                   movel     rcokcc        kcc132
024800981218     C                   movel     rcoksc        ksc132
024900981218     C                   movel     dtc131        dtc132
025000981218     C                   movel     seo131        seo132
025100981218     C                   movel     dti131        dti132
025200981218     C                   movel     dtf131        dtf132
025300981218     C*                  move      tpr131        tpr132
025400981218     C                   move      def131        def132
025500981218     C                   move      pro131        pro132
025600981218     C                   move      ges131        ges132
025700981218     C                   move      det131        det132
025800981218     C                   move      nmo131        cnm132
025900981218     C                   move      slm131        slm132
026000981218     C                   move      slp131        slp132
026100981218     C                   move      cau131        cau132
026200981218     C                   move      *off          fin132
026300981218     C                   move      *off          lr132
026400041012>>>>>C                   EVAL      Ycd132 = Ycd131
026500981218     C*
026600981218     C                   movel     c132ds        kpjbu
026700981218     C                   call      'NDC132R'
026800981218     C                   parm                    kpjba
026900981218     C*
027000980622     C*
027100950131     C                   ENDSR
027200950131     C/EJECT
027300940927     C************************************************************
027400940927     C* OPERAZIONI INIZIALI
027500940927     C************************************************************
027600940927     C     *INZSR        BEGSR
027700940927     C*
027800940830     C* Reperimento parametri
027900940830     C*
028000940830     C     *ENTRY        PLIST
028100940830     C                   PARM                    KPJBA
028200950208     C                   PARM                    TITOLO           50
028300980608     C*
028400940830     C                   ENDSR
028500941129     C/EJECT
028600941129     C************************************************************
028700941129     C* Reperimento dati società
028800941129     C************************************************************
028900941129     C     REPSOC        BEGSR
029000941129     C*
029100950629     C                   CALLb     'XSOC'
029200941129     C                   PARM                    TIPXSC            6
029300941129     C                   PARM                    SOCXSC            3
029400941129     C                   PARM                    CDSXSC            9 0
029500941129     C                   PARM                    MODXSC            3
029600941129     C                   PARM      *BLANKS       RTNXSC            1
029700941129     C                   PARM                    XSOCDS
029800941129     C                   PARM                    KPJBA
029900980608     C*
030000941129     C                   ENDSR
030100940830     C/EJECT
030200940830     C************************************************************
030300940830     C* INIZIALIZZAZIONE VARIABILI
030400940830     C************************************************************
030500940830     C     INZVAR        BEGSR
030600940830     C*
030700960527     C                   MOVEL     KPJBU         ANA968DS
030800950509     C*
030900950509     C* Reperimento parametri da ANMPE di lavoro
031000950509     C*
031100950621     C                   Z-ADD     LAN968        MPEPRGLAN
031200950509     C                   MOVE      SOC968        MPESOC
031300950509     C     K2MPE1        CHAIN     ANMPE01W                           21
031400950509     C     *IN21         IFEQ      *OFF
031500981217    >C                   MOVEL     MPEPAR        NDC131DS
031600950509     C                   ELSE
031700950509     C                   EXSR      ENDPGM
031800950509     C                   ENDIF
031900950509     C*
032000950509     C* Reperimento dati società
032100950509     C*
032200950509     C                   MOVEL     'SOC001'      TIPXSC
032300950509     C                   MOVEL     MPESOC        SOCXSC
032400950509     C                   EXSR      REPSOC
032500950509     C     RTNXSC        IFNE      '1'
032600950509     C                   MOVEL     XSOCDS        SOC001
032700950509     C                   ELSE
032800950509     C                   EXSR      ENDPGM
032900950509     C                   ENDIF
033000980615     C*
033100950509     C                   ENDSR
033200940830     C/EJECT
033300940830     C************************************************************
033400940830     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
033500940830     C************************************************************
033600940830     C     DEFCAM        BEGSR
033700950203     C*
033800950315     C                   MOVE      *BLANKS       WRKTRC           10
033900950315     C                   MOVE      *BLANKS       WRKFLD           10
034000961017     C                   MOVE      *BLANKS       WRKRGC            4
034100980608     C*
034200980608     C* KLIST per CHAIN su ANMPE
034300980608     C*
034400950508     C     K2MPE1        KLIST                                                        *
034500950621     C                   KFLD                    MPEPRGLAN                            *
034600950508     C                   KFLD                    MPESOC                               *
034700950127     C*
034800950127     C                   ENDSR
034900950202     C/EJECT
035000980608**
035100981218select * from ANRCO00F
035200980608**
035300981218where RCOSocieta='XXX' and RCOKsc <> '        '

000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940921     H* FNLS58R *----------------------------------------------------*
000300900601     H*         - PROSPETTO NOLI
000400000000     H*--------------------------------------------------------------*
000500940921     FFNLS58D   CF   E             WORKSTN
000600940125     FTABEL00F  IF   E           K DISK
000700940921     FFNFGV01L  IF   E           K DISK
000800000619     FFNFGW01L  IF   E           K DISK
000900000619     FAZORG01L  IF   E           K DISK
001000020809     D*
001100000707     D L1              S              3  0 DIM(30)
001200020809     D*
001300940125     D WLBDAT          DS
001400940921     D  G02DAT                 1      8  0
001500940921     D  G02INV                 9     16  0
001600940921     D  G02ERR                17     17
001700940921     D  G02TGI                18     22  0
001800940125     D WGIDAT          DS
001900940921     D  GIODAT                 1      8  0
002000940921     D  GIOINV                 9     16  0
002100940921     D  GIOTGI                17     21  0
002200020809     D*
002300020809     D KPJBA         E DS
002400020809     D UT§DSE0F      E DS
002500020809     D CNCR80        E DS
002600000619     D OG143         E DS
002700000619     D DS5A          E DS
002800950307     D DSLS01        E DS                  INZ
002900020809     D TRUL06DS      E DS                  inz
003000000619     D  LIN                    1     90  0
003100000619     D                                     DIM(30)
003200030627     D* DS PER TRUL90R - RICHIESTA STAMPANTI
003300030627     D TRUL90DS      E DS                  inz
003400020809     D*
003500020809     D COD             S                   LIKE(TBLCOD)
003600020809     D KEY             S                   LIKE(TBLKEY)
003700000000     C*---------------------------------------------------------------*
003800000000     C     *ENTRY        PLIST
003900000000     C                   PARM                    KPJBA
004000940921     C*
004100000000     C                   Z-ADD     1             CODUT
004200000619     C                   CALL      'X§PARUT'
004300020809     C                   PARM                    UT§DSE0F
004400940921     C                   MOVEL     RAGUT         VIDRSU
004500000619     C                   MOVEL     REC80         CNCR80
004600000619     C* CARICO L1
004700020809     C                   CLEAR                   TRUL06DS
004800000619     C                   MOVE      '£1'          D06COD
004900000619     C                   MOVEL     SIMFEL        D06KEY
005000020809     C                   MOVEL     TRUL06DS      KPJBU
005100000619     C                   CALL      'TRUL06R'
005200000619     C                   PARM                    KPJBA
005300020809     C                   MOVEL     KPJBU         TRUL06DS
005400000619     C                   MOVEA     LIN           L1
005500900511     C*---------------------------------------------------------------*
005600940125     C     KTAB          KLIST
005700940125     C                   KFLD                    CODUT
005800940921     C                   KFLD                    COD
005900940921     C                   KFLD                    KEY
006000020809     C     K02FGV01      klist
006100020809     C                   kfld                    VIDnfv
006200020809     C                   kfld                    SIMfel
006300020809     ***C*
006400020809     ***C     *LIKE         DEFINE    TBLCOD        COD
006500020809     ***C     *LIKE         DEFINE    TBLKEY        KEY
006600940125     C*---------------------------------------------------------------*
006700970430     C* RICHIESTA STAMPANTI
006800030627     C                   MOVEL     'S'           D90RSE
006900030627     C                   CALL      'TRUL90R'
007000970430     C                   PARM                    KPJBA
007100030627     C                   Parm                    TRUL90DS
007200030627     C                   MOVEL     D90MDE        DS1MDE
007300030627     C                   MOVEL     D90PRE        DS1PRE
007400970430     C* NOME PROGRAMMA STAMPA ETICHETTE FV
007500030627     C                   MOVEL     D90PSF        DS1PGM
007600970430     C**
007700970430     C* F3 - FINE
007800030627     C     D90F3         IFEQ      '1'
007900970430     C                   GOTO      FINE
008000970430     C                   ENDIF
008100970430     C*
008200940921     C* GIRO LA DATA DEL GIORNO
008300940921     C                   TIME                    WTIME            14 0
008400940921     C                   MOVE      WTIME         WDATE             8 0
008500940921     C                   Z-ADD     WDATE         G02DAT
008600940921     C                   Z-ADD     *ZERO         G02INV
008700940921     C                   MOVEL     *BLANKS       G02ERR
008800940921     C                   CALL      'XSRDA8'
008900940921     C                   PARM                    WLBDAT
009000940921     C                   Z-ADD     G02INV        DATEU             8 0
009100900517     C*
009200950307     C                   MOVEL     'N'           DS1FL4
009300950411     C*
009400000620     C                   Z-ADD     0             VIDFGS
009500020430     C*                    MOVELKNMUS     REM     3
009600000620     C                   SETON                                        35
009700000626     C*
009800000626     C                   MOVEL     'S'           VIDETI
009900000620      *
010000000620      *  UTENTE 2° LIVELLO
010100000620      *  NB - IMPOSTO IL P.O. IN GESTIONE UGUALE A SIMFEL SE NON E' POSTE
010200000620      *
010300020430     C*          REM       IFEQ 'REM'
010400020430     C*                    MOVELKNMUS     WORK06  6
010500020430     C*                    MOVE WORK06    VIDFGS
010600020430     C*                    MOVE WORK06    WRKUTE  30
010700020430     C     SIMTPP        IFEQ      '2'
010800020430     C     SIMTPP        OREQ      *BLANKS
010900020430     C                   Z-ADD     SIMPOU        VIDFGS
011000020430     C                   Z-ADD     SIMPOU        WRKUTE            3 0
011100000620     C     VIDFGS        CHAIN     AZORG01L                           31
011200000620     C     *IN31         IFEQ      *OFF
011300000620     C                   MOVEL     ORGDE3        OG143
011400020809     ***C     §OGPT         IFNE      'S'
011500020809     C                   IF        §OGntw <> 'PPT'
011600000620     C                   Z-ADD     SIMFEL        VIDFGS
011700000620     C                   ENDIF
011800000620     C                   ENDIF
011900000620      *
012000000620      *  UTENTE 1° LIVELLO
012100000620      *
012200000620     C                   ELSE
012300000620     C                   Z-ADD     SIMFEL        VIDFGS
012400000707     C                   Z-ADD     SIMFEL        WRKUTE
012500000620     C                   SETOFF                                       35
012600000620     C                   ENDIF
012700000707      *
012800000707      *  CONTROLLO SE UTENTE E' POSTE PER ABILITARE F10
012900000707      *
013000000707     C     WRKUTE        CHAIN     AZORG01L                           31
013100000707     C     *IN31         IFEQ      *OFF
013200000707     C                   MOVEL     ORGDE3        OG143
013300020809     ***C     §OGPT         IFEQ      'S'
013400020809     C                   IF        §OGntw = 'PPT'
013500000707     C                   SETON                                        53
013600000707     C                   ENDIF
013700000707     C                   ENDIF
013800000620      *
013900950411     C     INIZIO        TAG
014000000619     C                   SETOFF                                       51
014100000619      *
014200000619      * INIZIALIZZO VARIABILI VIDEO - - - - - - - - - - - - - - - - - - -
014300000619      *
014400900613     C                   Z-ADD     0             VIDDFD
014500900601     C                   Z-ADD     0             VIDDFA
014600950307     C                   Z-ADD     0             DS1AAL
014700950307     C                   Z-ADD     0             DS1ADA
014800900608     C                   Z-ADD     0             VIDNFV
014900901017     C                   MOVEL     '2'           VIDTTB
015000901017     C                   MOVEL     'A'           VIDTFV
015100911119     C                   MOVEL     'N'           VIDSVC
015200910322     C                   MOVEL     'S'           VIDSTP
015300000711     C   53              MOVEL     'N'           VIDETI
015400000711     C  N53              MOVEL     'S'           VIDETI
015500000619      *
015600000619      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
015700000619      *
015800941027     C                   WRITE     LS58T01
015900900509     C     FOR01         TAG
016000940921     C                   EXFMT     LS58D01
016100940921     C*
016200900509     C** CMD03 - FINE LAVORO
016300900509     C   KC              GOTO      FINE
016400000707     C   10
016500000707     CANN53              GOTO      FOR01
016600000619     C*
016700000619     C** CMD18 - CAMBIO P.O. IN GESTIONE (ABILITATO SOLO SIMFEL - 35=OFF)
016800000619     C     *IN18         IFEQ      *ON
016900000619     C     *IN35         IFEQ      *OFF
017000000619     C                   SETON                                        51
017100000619     C                   ENDIF
017200000619     C                   GOTO      FOR01
017300000619     C                   ENDIF
017400900524     C* CONTROLLO
017500900524     C                   EXSR      CONTR
017600000707     C  NKF
017700000707     CANN10
017800900524     COR 90              GOTO      FOR01
017900900511     C* ELABORAZIONE
018000000711     C  N10              MOVE      'N'           DS1FL1
018100000711     C   10              MOVE      'S'           DS1FL1
018200950307     C                   Z-ADD     *ZERO         DS1LNP
018300950307     C                   Z-ADD     VIDDFD        DS1GDA
018400950307     C                   Z-ADD     VIDDFA        DS1GAL
018500950307     C                   MOVEL     VIDTTB        DS1FTM
018600950307     C                   MOVEL     VIDTFV        DS1TFV
018700950307     C                   Z-ADD     VIDNFV        DS1NFV
018800000711     C  N10              MOVEL     VIDSVC        DS1FL2
018900000711     C   10              MOVEL     'N'           DS1FL2
019000950307     C                   MOVEL     VIDSTP        DS1FL3
019100000711     C  N10              MOVEL     VIDETI        DS1FL6
019200000711     C   10              MOVEL     'N'           DS1FL6
019300000619     C                   Z-ADD     VIDFGS        DS1FGS
019400950307     C                   MOVEL     DSLS01        KPJBU
019500940921     C                   MOVEL     'LS59'        KCOAZ
019600900523     C                   CALL      'BCH10'
019700900523     C                   PARM                    KPJBA
019800900523     C*
019900950411     C                   GOTO      INIZIO
020000000000     C     FINE          TAG
020100000000     C                   SETON                                        LR
020200940921     C**************************************************************************
020300940921     C*    CONTROLLI FORMATO1
020400940921     C**************************************************************************
020500900524     C     CONTR         BEGSR
020600940921     C*
020700900524     C                   SETOFF                                       90
020800000619     C* CONTROLLO P.O. IN GESTIONE
020900000619     C* Controllo se presente in L1
021000000619    1C     VIDFGS        IFNE      0
021100000619     C     VIDFGS        LOOKUP    L1                                     31
021200000619     C  N31              SETON                                        52  90
021300000619     C   90              GOTO      ENDCTR
021400000707     C     VIDFGS        CHAIN     AZORG01L                           31
021500000619      * Controllo se è SIMFEL o P.O. Poste
021600000619    2C     VIDFGS        IFNE      SIMFEL
021700000619    3C     *IN31         IFEQ      *OFF
021800000619     C                   MOVEL     ORGDE3        OG143
021900020809    4***C     §OGPT         IFNE      'S'
022000020809     C                   IF        §OGntw <> 'PPT'
022100000619     C                   SETON                                        52  90
022200000619     C                   GOTO      ENDCTR
022300000619    4C                   ENDIF
022400000619     C                   ELSE
022500000619   X3C                   SETON                                        52  90
022600000619     C                   GOTO      ENDCTR
022700000619    3C                   ENDIF
022800000707   X2C                   ELSE
022900000707    3C   10*IN31         IFEQ      *OFF
023000000707     C                   MOVEL     ORGDE3        OG143
023100020809    4***C     §OGPT         IFNE      'S'
023200020809     C                   IF        §OGntw <> 'PPT'
023300000707     C                   SETON                                        52  90
023400000707     C                   GOTO      ENDCTR
023500000707    4C                   ENDIF
023600000707    3C                   ENDIF
023700000619    2C                   ENDIF
023800000619     C                   ELSE
023900000619      * SE = 0 ERRORE
024000000619   X1C                   SETON                                        52  90
024100000619     C                   GOTO      ENDCTR
024200000619    1C                   ENDIF
024300900613     C* O NUMERO FOGLIO VIAGGIO A DATA DAL
024400900613     C     VIDNFV        IFEQ      0
024500940125     C     VIDDFD        ANDEQ     0
024600900613     C                   SETON                                        4390
024700900613     C                   GOTO      ENDCTR
024800900613     C                   END
024900900613     C* NON SI POSSONO IMMETTERE ENTRAMBI
025000900613     C     VIDNFV        IFNE      0
025100940125     C     VIDDFD        IFNE      0
025200940125     C     VIDDFA        ORNE      0
025300900613     C                   SETON                                        4490
025400900613     C                   GOTO      ENDCTR
025500900613     C                   END
025600940125     C                   END
025700900613     C*
025800900613     C* NUMERO FOGLIO VIAGGIO
025900900613     C     VIDNFV        IFNE      0
026000020809     ***C     VIDNFV        CHAIN     FNFGV01L                           45
026100020809     C     K02FGV01      chain     FNFGV01L                           45
026200900626     C  N45FGVATB        IFNE      *BLANKS
026300900626     C                   SETON                                        45
026400900626     C                   END
026500900613     C   45              SETON                                        90
026600900613     C   45              GOTO      ENDCTR
026700900613     C                   END
026800000619      * ... DEVE CORRISPONDERE A P.O. IN GESTIONE
026900000619    1C     VIDNFV        IFNE      0
027000020809     ***C     VIDNFV        CHAIN     FNFGW01L                           31
027100020809     C     K02FGV01      chain     FNFGW01L                           31
027200000619    2C     *IN31         IFEQ      *OFF
027300020809     c     FGWatb        andeq     *blanks
027400000619     C     FGWFGS        ANDNE     0
027500000619    3C     VIDFGS        IFNE      FGWFGS
027600000619     C                   SETON                                        55  90
027700000619     C                   GOTO      ENDCTR
027800000619    3C                   ENDIF
027900000619   X2C                   ELSE
028000000619    3C     VIDFGS        IFNE      FGVLNP
028100000619     C                   SETON                                        55  90
028200000619     C                   GOTO      ENDCTR
028300000619    3C                   ENDIF
028400000619    2C                   ENDIF
028500000619    1C                   ENDIF
028600900613     C* DATA BORDERO' DAL
028700900613     C     VIDDFD        IFNE      0
028800940921     C                   MOVEL     VIDDFD        G02DAT
028900940921     C                   MOVEL     *ZERO         G02INV
029000940125     C                   MOVEL     *BLANK        G02ERR
029100940921     C                   CALL      'XSRDA8'
029200940125     C                   PARM                    WLBDAT
029300940125IF  1C     G02ERR        IFEQ      '1'
029400940125     C                   SETON                                        40  90
029500940125     C                   GOTO      ENDCTR
029600940125E   1C                   ENDIF
029700940921     C                   Z-ADD     G02DAT        VIDDFD
029800950307     C                   Z-ADD     G02INV        DS1ADA
029900940125     C                   END
030000900524     C*
030100900613     C* DATA BORDERO' AL
030200900601     C     VIDDFA        IFNE      0
030300940921     C                   MOVEL     VIDDFA        G02DAT
030400940921     C                   MOVEL     *ZERO         G02INV
030500940125     C                   MOVEL     *BLANK        G02ERR
030600940921     C                   CALL      'XSRDA8'
030700940125     C                   PARM                    WLBDAT
030800940125IF  1C     G02ERR        IFEQ      '1'
030900940125     C                   SETON                                        41  90
031000940125     C                   GOTO      ENDCTR
031100940125E   1C                   ENDIF
031200940921     C                   MOVEL     G02DAT        VIDDFA
031300950307     C                   Z-ADD     G02INV        DS1AAL
031400940125     C*
031500900524     C* DATA DAL NO MAGGIORE DATA AL
031600950307     C     DS1ADA        IFGT      DS1AAL
031700900524     C                   SETON                                        4290
031800900524     C                   GOTO      ENDCTR
031900900524     C                   END
032000900524     C*
032100900524     C                   END
032200910322     C*
032300910322     C* EFFETTUARE ALMENO UNA SCELTA DI STAMPA
032400910322     C     VIDSTP        IFEQ      'N'
032500940125     C     VIDSVC        ANDEQ     'N'
032600970421     C     VIDETI        ANDEQ     'N'
032700910322     C                   SETON                                        4690
032800910322     C                   GOTO      ENDCTR
032900910322     C                   END
033000940125     C*
033100940125     C* CONTROLLO SE CI SONO ANCORA I DATI DA ELABORARE
033200940125     C* 1) SE HO RICHIESTO LA STAMPA DEL PROSPETTO
033300940125     C* 2) SE NON HO RICHIESTO SOLO I FOGLI APERTI
033400940125     C* 3) SE IL FOGLIO RICHIESTO E' CHIUSO
033500940125     C     VIDSTP        IFEQ      'S'
033600940125     C*
033700940125     C     VIDNFV        IFEQ      0
033800940125     C     VIDTFV        ANDNE     'A'
033900940125     C*
034000940125     C     VIDNFV        ORGT      0
034100940125     C     FGVFCF        ANDNE     ' '
034200940125     C*
034300940125     C* REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA FV
034400950116     C                   MOVEL     'FNLRA7R'     PGM
034500940125     C                   MOVEL     *BLANKS       DAT
034600940125     C                   MOVEL     *BLANKS       ERR
034700940125     C*
034800940921     C                   CALL      'TRUL49C'                            99
034900940125     C                   PARM                    PGM              10
035000000103     C                   PARM                    DAT               8
035100940125     C                   PARM                    ERR               1
035200940125     C*
035300940125     C* PROGRAMMA IN USO DATA ULTIMO UTILIZZO = UDATE
035400940125IF  1C     ERR           IFEQ      '1'
035500940921     C     *IN99         OREQ      *ON
035600940125     C                   Z-ADD     DATEU         G02INV
035700940125     C                   ELSE
035800950203     C                   CLEAR                   G02INV
035900940125     C                   MOVE      DAT           G02INV
036000940125E   1C                   ENDIF
036100940125     C*
036200940125     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
036300940125     C* PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
036400940125     C* POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
036500940125     C*
036600940125     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
036700940125IF  1C     G02INV        IFNE      *ZEROS
036800940125     C                   Z-ADD     *ZEROS        G02DAT
036900940125     C                   Z-ADD     *ZEROS        G02TGI
037000940125     C                   MOVEL     '3'           G02ERR
037100940921     C                   CALL      'XSRDA8'
037200940125     C                   PARM                    WLBDAT
037300940125     C*
037400940125     C                   MOVEL     '5A'          COD
037500940125     C                   MOVEL     *BLANKS       KEY
037600940125     C                   MOVEL     '1'           KEY
037700940125     C     KTAB          CHAIN     TABEL00F                           30
037800940125IF  2C     *IN30         IFEQ      *ON
037900940125     C                   MOVEL     *BLANKS       DS5A
038000940125X   2C                   ELSE
038100940125     C                   MOVEL     TBLUNI        DS5A
038200940125E   2C                   ENDIF
038300940125     C*
038400950116     C                   SUB       §5AFGV        G02TGI
038500940125     C*
038600940125     C                   Z-ADD     *ZEROS        GIODAT
038700940125     C                   Z-ADD     *ZEROS        GIOINV
038800940125     C                   Z-ADD     G02TGI        GIOTGI
038900940125     C*
039000940921     C                   CALL      'XSRGI8'
039100940125     C                   PARM                    WGIDAT
039200940125     C*
039300940921     C                   Z-ADD     GIOINV        PRIDAT            8 0
039400940125E   1C                   ENDIF
039500940125     C*
039600940125     C* DATA DAL MAGGIORE DATA CALCOLATA
039700940131     C     VIDNFV        IFEQ      0
039800950307IF  1C     DS1ADA        IFLE      PRIDAT
039900940125     C                   SETON                                        4790
040000940125     C                   GOTO      ENDCTR
040100940125E   1C                   ENDIF
040200940131     C                   ELSE
040300940131     C* CONFRONTO CON LA DATA FOGLIO VIAGGIO
040400940131IF  1C     FGVDFV        IFLE      PRIDAT
040500940131     C                   SETON                                        4790
040600940131     C                   GOTO      ENDCTR
040700940131E   1C                   ENDIF
040800940131     C                   ENDIF
040900940125     C*
041000940125E   1C                   ENDIF
041100940125E   1C                   ENDIF
041200940921     C*
041300900524     C     ENDCTR        ENDSR

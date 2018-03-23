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
003200970430     D* DS PER TRUL01R - RICHIESTA STAMPANTI
003300020809     D TRUL01DS      E DS                  inz
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
006800970430     C                   MOVEL     'S'           D01RSE
006900020809     C                   MOVEL     TRUL01DS      KPJBU
007000970430     C                   CALL      'TRUL01R'
007100970430     C                   PARM                    KPJBA
007200020809     C                   MOVEL     KPJBU         TRUL01DS
007300970430     C                   MOVEL     D01MDE        DS1MDE
007400970430     C                   MOVEL     D01PRE        DS1PRE
007500970430     C* NOME PROGRAMMA STAMPA ETICHETTE FV
007600970430     C                   MOVEL     D01PSF        DS1PGM
007700970430     C**
007800970430     C* F3 - FINE
007900970430     C     D01F3         IFEQ      '1'
008000970430     C                   GOTO      FINE
008100970430     C                   ENDIF
008200970430     C*
008300940921     C* GIRO LA DATA DEL GIORNO
008400940921     C                   TIME                    WTIME            14 0
008500940921     C                   MOVE      WTIME         WDATE             8 0
008600940921     C                   Z-ADD     WDATE         G02DAT
008700940921     C                   Z-ADD     *ZERO         G02INV
008800940921     C                   MOVEL     *BLANKS       G02ERR
008900940921     C                   CALL      'XSRDA8'
009000940921     C                   PARM                    WLBDAT
009100940921     C                   Z-ADD     G02INV        DATEU             8 0
009200900517     C*
009300950307     C                   MOVEL     'N'           DS1FL4
009400950411     C*
009500000620     C                   Z-ADD     0             VIDFGS
009600020430     C*                    MOVELKNMUS     REM     3
009700000620     C                   SETON                                        35
009800000626     C*
009900000626     C                   MOVEL     'S'           VIDETI
010000000620      *
010100000620      *  UTENTE 2° LIVELLO
010200000620      *  NB - IMPOSTO IL P.O. IN GESTIONE UGUALE A SIMFEL SE NON E' POSTE
010300000620      *
010400020430     C*          REM       IFEQ 'REM'
010500020430     C*                    MOVELKNMUS     WORK06  6
010600020430     C*                    MOVE WORK06    VIDFGS
010700020430     C*                    MOVE WORK06    WRKUTE  30
010800020430     C     SIMTPP        IFEQ      '2'
010900020430     C     SIMTPP        OREQ      *BLANKS
011000020430     C                   Z-ADD     SIMPOU        VIDFGS
011100020430     C                   Z-ADD     SIMPOU        WRKUTE            3 0
011200000620     C     VIDFGS        CHAIN     AZORG01L                           31
011300000620     C     *IN31         IFEQ      *OFF
011400000620     C                   MOVEL     ORGDE3        OG143
011500020809     ***C     §OGPT         IFNE      'S'
011600020809     C                   IF        §OGntw <> 'PPT'
011700000620     C                   Z-ADD     SIMFEL        VIDFGS
011800000620     C                   ENDIF
011900000620     C                   ENDIF
012000000620      *
012100000620      *  UTENTE 1° LIVELLO
012200000620      *
012300000620     C                   ELSE
012400000620     C                   Z-ADD     SIMFEL        VIDFGS
012500000707     C                   Z-ADD     SIMFEL        WRKUTE
012600000620     C                   SETOFF                                       35
012700000620     C                   ENDIF
012800000707      *
012900000707      *  CONTROLLO SE UTENTE E' POSTE PER ABILITARE F10
013000000707      *
013100000707     C     WRKUTE        CHAIN     AZORG01L                           31
013200000707     C     *IN31         IFEQ      *OFF
013300000707     C                   MOVEL     ORGDE3        OG143
013400020809     ***C     §OGPT         IFEQ      'S'
013500020809     C                   IF        §OGntw = 'PPT'
013600000707     C                   SETON                                        53
013700000707     C                   ENDIF
013800000707     C                   ENDIF
013900000620      *
014000950411     C     INIZIO        TAG
014100000619     C                   SETOFF                                       51
014200000619      *
014300000619      * INIZIALIZZO VARIABILI VIDEO - - - - - - - - - - - - - - - - - - -
014400000619      *
014500900613     C                   Z-ADD     0             VIDDFD
014600900601     C                   Z-ADD     0             VIDDFA
014700950307     C                   Z-ADD     0             DS1AAL
014800950307     C                   Z-ADD     0             DS1ADA
014900900608     C                   Z-ADD     0             VIDNFV
015000901017     C                   MOVEL     '2'           VIDTTB
015100901017     C                   MOVEL     'A'           VIDTFV
015200911119     C                   MOVEL     'N'           VIDSVC
015300910322     C                   MOVEL     'S'           VIDSTP
015400000711     C   53              MOVEL     'N'           VIDETI
015500000711     C  N53              MOVEL     'S'           VIDETI
015600000619      *
015700000619      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
015800000619      *
015900941027     C                   WRITE     LS58T01
016000900509     C     FOR01         TAG
016100940921     C                   EXFMT     LS58D01
016200940921     C*
016300900509     C** CMD03 - FINE LAVORO
016400900509     C   KC              GOTO      FINE
016500000707     C   10
016600000707     CANN53              GOTO      FOR01
016700000619     C*
016800000619     C** CMD18 - CAMBIO P.O. IN GESTIONE (ABILITATO SOLO SIMFEL - 35=OFF)
016900000619     C     *IN18         IFEQ      *ON
017000000619     C     *IN35         IFEQ      *OFF
017100000619     C                   SETON                                        51
017200000619     C                   ENDIF
017300000619     C                   GOTO      FOR01
017400000619     C                   ENDIF
017500900524     C* CONTROLLO
017600900524     C                   EXSR      CONTR
017700000707     C  NKF
017800000707     CANN10
017900900524     COR 90              GOTO      FOR01
018000900511     C* ELABORAZIONE
018100000711     C  N10              MOVE      'N'           DS1FL1
018200000711     C   10              MOVE      'S'           DS1FL1
018300950307     C                   Z-ADD     *ZERO         DS1LNP
018400950307     C                   Z-ADD     VIDDFD        DS1GDA
018500950307     C                   Z-ADD     VIDDFA        DS1GAL
018600950307     C                   MOVEL     VIDTTB        DS1FTM
018700950307     C                   MOVEL     VIDTFV        DS1TFV
018800950307     C                   Z-ADD     VIDNFV        DS1NFV
018900000711     C  N10              MOVEL     VIDSVC        DS1FL2
019000000711     C   10              MOVEL     'N'           DS1FL2
019100950307     C                   MOVEL     VIDSTP        DS1FL3
019200000711     C  N10              MOVEL     VIDETI        DS1FL6
019300000711     C   10              MOVEL     'N'           DS1FL6
019400000619     C                   Z-ADD     VIDFGS        DS1FGS
019500950307     C                   MOVEL     DSLS01        KPJBU
019600940921     C                   MOVEL     'LS59'        KCOAZ
019700900523     C                   CALL      'BCH10'
019800900523     C                   PARM                    KPJBA
019900900523     C*
020000950411     C                   GOTO      INIZIO
020100000000     C     FINE          TAG
020200000000     C                   SETON                                        LR
020300940921     C**************************************************************************
020400940921     C*    CONTROLLI FORMATO1
020500940921     C**************************************************************************
020600900524     C     CONTR         BEGSR
020700940921     C*
020800900524     C                   SETOFF                                       90
020900000619     C* CONTROLLO P.O. IN GESTIONE
021000000619     C* Controllo se presente in L1
021100000619    1C     VIDFGS        IFNE      0
021200000619     C     VIDFGS        LOOKUP    L1                                     31
021300000619     C  N31              SETON                                        52  90
021400000619     C   90              GOTO      ENDCTR
021500000707     C     VIDFGS        CHAIN     AZORG01L                           31
021600000619      * Controllo se è SIMFEL o P.O. Poste
021700000619    2C     VIDFGS        IFNE      SIMFEL
021800000619    3C     *IN31         IFEQ      *OFF
021900000619     C                   MOVEL     ORGDE3        OG143
022000020809    4***C     §OGPT         IFNE      'S'
022100020809     C                   IF        §OGntw <> 'PPT'
022200000619     C                   SETON                                        52  90
022300000619     C                   GOTO      ENDCTR
022400000619    4C                   ENDIF
022500000619     C                   ELSE
022600000619   X3C                   SETON                                        52  90
022700000619     C                   GOTO      ENDCTR
022800000619    3C                   ENDIF
022900000707   X2C                   ELSE
023000000707    3C   10*IN31         IFEQ      *OFF
023100000707     C                   MOVEL     ORGDE3        OG143
023200020809    4***C     §OGPT         IFNE      'S'
023300020809     C                   IF        §OGntw <> 'PPT'
023400000707     C                   SETON                                        52  90
023500000707     C                   GOTO      ENDCTR
023600000707    4C                   ENDIF
023700000707    3C                   ENDIF
023800000619    2C                   ENDIF
023900000619     C                   ELSE
024000000619      * SE = 0 ERRORE
024100000619   X1C                   SETON                                        52  90
024200000619     C                   GOTO      ENDCTR
024300000619    1C                   ENDIF
024400900613     C* O NUMERO FOGLIO VIAGGIO A DATA DAL
024500900613     C     VIDNFV        IFEQ      0
024600940125     C     VIDDFD        ANDEQ     0
024700900613     C                   SETON                                        4390
024800900613     C                   GOTO      ENDCTR
024900900613     C                   END
025000900613     C* NON SI POSSONO IMMETTERE ENTRAMBI
025100900613     C     VIDNFV        IFNE      0
025200940125     C     VIDDFD        IFNE      0
025300940125     C     VIDDFA        ORNE      0
025400900613     C                   SETON                                        4490
025500900613     C                   GOTO      ENDCTR
025600900613     C                   END
025700940125     C                   END
025800900613     C*
025900900613     C* NUMERO FOGLIO VIAGGIO
026000900613     C     VIDNFV        IFNE      0
026100020809     ***C     VIDNFV        CHAIN     FNFGV01L                           45
026200020809     C     K02FGV01      chain     FNFGV01L                           45
026300900626     C  N45FGVATB        IFNE      *BLANKS
026400900626     C                   SETON                                        45
026500900626     C                   END
026600900613     C   45              SETON                                        90
026700900613     C   45              GOTO      ENDCTR
026800900613     C                   END
026900000619      * ... DEVE CORRISPONDERE A P.O. IN GESTIONE
027000000619    1C     VIDNFV        IFNE      0
027100020809     ***C     VIDNFV        CHAIN     FNFGW01L                           31
027200020809     C     K02FGV01      chain     FNFGW01L                           31
027300000619    2C     *IN31         IFEQ      *OFF
027400020809     c     FGWatb        andeq     *blanks
027500000619     C     FGWFGS        ANDNE     0
027600000619    3C     VIDFGS        IFNE      FGWFGS
027700000619     C                   SETON                                        55  90
027800000619     C                   GOTO      ENDCTR
027900000619    3C                   ENDIF
028000000619   X2C                   ELSE
028100000619    3C     VIDFGS        IFNE      FGVLNP
028200000619     C                   SETON                                        55  90
028300000619     C                   GOTO      ENDCTR
028400000619    3C                   ENDIF
028500000619    2C                   ENDIF
028600000619    1C                   ENDIF
028700900613     C* DATA BORDERO' DAL
028800900613     C     VIDDFD        IFNE      0
028900940921     C                   MOVEL     VIDDFD        G02DAT
029000940921     C                   MOVEL     *ZERO         G02INV
029100940125     C                   MOVEL     *BLANK        G02ERR
029200940921     C                   CALL      'XSRDA8'
029300940125     C                   PARM                    WLBDAT
029400940125IF  1C     G02ERR        IFEQ      '1'
029500940125     C                   SETON                                        40  90
029600940125     C                   GOTO      ENDCTR
029700940125E   1C                   ENDIF
029800940921     C                   Z-ADD     G02DAT        VIDDFD
029900950307     C                   Z-ADD     G02INV        DS1ADA
030000940125     C                   END
030100900524     C*
030200900613     C* DATA BORDERO' AL
030300900601     C     VIDDFA        IFNE      0
030400940921     C                   MOVEL     VIDDFA        G02DAT
030500940921     C                   MOVEL     *ZERO         G02INV
030600940125     C                   MOVEL     *BLANK        G02ERR
030700940921     C                   CALL      'XSRDA8'
030800940125     C                   PARM                    WLBDAT
030900940125IF  1C     G02ERR        IFEQ      '1'
031000940125     C                   SETON                                        41  90
031100940125     C                   GOTO      ENDCTR
031200940125E   1C                   ENDIF
031300940921     C                   MOVEL     G02DAT        VIDDFA
031400950307     C                   Z-ADD     G02INV        DS1AAL
031500940125     C*
031600900524     C* DATA DAL NO MAGGIORE DATA AL
031700950307     C     DS1ADA        IFGT      DS1AAL
031800900524     C                   SETON                                        4290
031900900524     C                   GOTO      ENDCTR
032000900524     C                   END
032100900524     C*
032200900524     C                   END
032300910322     C*
032400910322     C* EFFETTUARE ALMENO UNA SCELTA DI STAMPA
032500910322     C     VIDSTP        IFEQ      'N'
032600940125     C     VIDSVC        ANDEQ     'N'
032700970421     C     VIDETI        ANDEQ     'N'
032800910322     C                   SETON                                        4690
032900910322     C                   GOTO      ENDCTR
033000910322     C                   END
033100940125     C*
033200940125     C* CONTROLLO SE CI SONO ANCORA I DATI DA ELABORARE
033300940125     C* 1) SE HO RICHIESTO LA STAMPA DEL PROSPETTO
033400940125     C* 2) SE NON HO RICHIESTO SOLO I FOGLI APERTI
033500940125     C* 3) SE IL FOGLIO RICHIESTO E' CHIUSO
033600940125     C     VIDSTP        IFEQ      'S'
033700940125     C*
033800940125     C     VIDNFV        IFEQ      0
033900940125     C     VIDTFV        ANDNE     'A'
034000940125     C*
034100940125     C     VIDNFV        ORGT      0
034200940125     C     FGVFCF        ANDNE     ' '
034300940125     C*
034400940125     C* REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA FV
034500950116     C                   MOVEL     'FNLRA7R'     PGM
034600940125     C                   MOVEL     *BLANKS       DAT
034700940125     C                   MOVEL     *BLANKS       ERR
034800940125     C*
034900940921     C                   CALL      'TRUL49C'                            99
035000940125     C                   PARM                    PGM              10
035100000103     C                   PARM                    DAT               8
035200940125     C                   PARM                    ERR               1
035300940125     C*
035400940125     C* PROGRAMMA IN USO DATA ULTIMO UTILIZZO = UDATE
035500940125IF  1C     ERR           IFEQ      '1'
035600940921     C     *IN99         OREQ      *ON
035700940125     C                   Z-ADD     DATEU         G02INV
035800940125     C                   ELSE
035900950203     C                   CLEAR                   G02INV
036000940125     C                   MOVE      DAT           G02INV
036100940125E   1C                   ENDIF
036200940125     C*
036300940125     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
036400940125     C* PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
036500940125     C* POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
036600940125     C*
036700940125     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
036800940125IF  1C     G02INV        IFNE      *ZEROS
036900940125     C                   Z-ADD     *ZEROS        G02DAT
037000940125     C                   Z-ADD     *ZEROS        G02TGI
037100940125     C                   MOVEL     '3'           G02ERR
037200940921     C                   CALL      'XSRDA8'
037300940125     C                   PARM                    WLBDAT
037400940125     C*
037500940125     C                   MOVEL     '5A'          COD
037600940125     C                   MOVEL     *BLANKS       KEY
037700940125     C                   MOVEL     '1'           KEY
037800940125     C     KTAB          CHAIN     TABEL00F                           30
037900940125IF  2C     *IN30         IFEQ      *ON
038000940125     C                   MOVEL     *BLANKS       DS5A
038100940125X   2C                   ELSE
038200940125     C                   MOVEL     TBLUNI        DS5A
038300940125E   2C                   ENDIF
038400940125     C*
038500950116     C                   SUB       §5AFGV        G02TGI
038600940125     C*
038700940125     C                   Z-ADD     *ZEROS        GIODAT
038800940125     C                   Z-ADD     *ZEROS        GIOINV
038900940125     C                   Z-ADD     G02TGI        GIOTGI
039000940125     C*
039100940921     C                   CALL      'XSRGI8'
039200940125     C                   PARM                    WGIDAT
039300940125     C*
039400940921     C                   Z-ADD     GIOINV        PRIDAT            8 0
039500940125E   1C                   ENDIF
039600940125     C*
039700940125     C* DATA DAL MAGGIORE DATA CALCOLATA
039800940131     C     VIDNFV        IFEQ      0
039900950307IF  1C     DS1ADA        IFLE      PRIDAT
040000940125     C                   SETON                                        4790
040100940125     C                   GOTO      ENDCTR
040200940125E   1C                   ENDIF
040300940131     C                   ELSE
040400940131     C* CONFRONTO CON LA DATA FOGLIO VIAGGIO
040500940131IF  1C     FGVDFV        IFLE      PRIDAT
040600940131     C                   SETON                                        4790
040700940131     C                   GOTO      ENDCTR
040800940131E   1C                   ENDIF
040900940131     C                   ENDIF
041000940125     C*
041100940125E   1C                   ENDIF
041200940125E   1C                   ENDIF
041300940921     C*
041400900524     C     ENDCTR        ENDSR

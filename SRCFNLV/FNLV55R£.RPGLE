000100950130     H DECEDIT('0,') DATEDIT(*DMY.)
000200061207
000300061207      *---------------------------------------------------------------*
000400061207      *  -- ATTENZIONE --- COMPILARE V5R1M0
000500061207      * NB: Ricordarsi di non modificare mai FNLV55R ma creare sempre un *
000600061207      *     FNLV55R perchè i clienti scaricano e utilizzano il sorgente     *
000700061207      *     e NON AGGIUNGERE FILE/RICHIAMI                                  *
000800061207      *---------------------------------------------------------------*
000900061207
001000970514     F* FNLV55R *-----------------------------------------------------*
001100060705     F*       REPERIMENTO:
001200100617     F*       A/P/_       - TERMINAL DI ARRIVO E/O DI PARTENZA
001300100617     F*         6         - P.O. GESTIONE DA £6
001400100617     F*         O         - P.O. Ottico
001500100617     F*       D/P/E       - TERMINAL DI ARRIVO PER DEFLUENZA E/O DI PARTENZA
001600950130     F*---------------------------------------------------------------*
001700061207
001800970514     FAZCAE01L  IF   E           K DISK
001900020529     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
002000040701     D***                 CMD     1   1 36               CHKBJ
002100970514     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
002200970514     D DSLV55        E DS                  EXTNAME(FNLV55DS)
002300970424     C**************************************************************************
002400970424     C     *ENTRY        PLIST
002500970514     C                   PARM                    DSLV55
002600970424     C*
002700970514     C* TIPO LANCIO, D55TLA "C" -           CHIUSO CON LR
002800970514     C* TIPO LANCIO, D55TLA "L" - ELABORO E CHIUDO CON LR
002900970514     C* TIPO LANCIO, D55TLA " " - ELABORO E CHIUDO IN RETRN
003000970514    1C     D55TLA        IFNE      'C'
003100970514     C* Pulizia dei campi di output
003200970514     C                   CLEAR                   D55ERR
003300970514     C                   CLEAR                   D55MSG
003400970514     C                   CLEAR                   D55TFP
003500970514     C                   CLEAR                   D55TFA
003600970918     C                   CLEAR                   D55ORP
003700970918     C                   CLEAR                   D55ORR
003800970516     C* ERRORE SE NON PASSATA LA LINEA PER CUI DETERMINARE IL TERMINAL
003900970516     C     D55LIN        IFEQ      *ZEROS
004000970516     C                   MOVEL     '1'           D55ERR
004100970516     C                   MOVEL     MSG(1)        D55MSG
004200970516     C                   GOTO      FINE
004300970516     C                   END
004400970702     C* ERRORE SE NON PASSATA LA DATA DI RIFERIMENTO
004500970702     C     D55DRF        IFEQ      *ZEROS
004600970702     C                   MOVEL     '2'           D55ERR
004700970703     C                   MOVEL     MSG(2)        D55MSG
004800970702     C                   GOTO      FINE
004900970702     C                   END
005000970514     C*
005100970514     C**************************************
005200970514     C* REPERIMENTO DEL TERMINAL DI PARTENZA
005300970514     C**************************************
005400970514    2C     D55TPT        IFEQ      'P'
005500970514     C     D55TPT        OREQ      ' '
005600100617     C     D55TPT        OREQ      'E'
005700100617     c
005800970702     C                   MOVEL     'P'           KEPA
005900970702     C                   Z-ADD     D55LIN        KTFP
006000970702     C                   MOVE      *ZEROS        KTFA
006100100617     c                   eval      kdrf=d55DRF
006200970702     C                   EXSR      RICTER
006300970702     C     WTER          IFEQ      *ZEROS
006400970702     C                   Z-ADD     D55LIN        D55TFP
006500990517     C                   MOVEL     '3'           D55ERR
006600990517     C                   MOVEL     MSG(3)        D55MSG
006700970702     C                   ELSE
006800970702     C                   Z-ADD     WTER          D55TFP
006900970702     C                   END
007000970514    2C                   END
007100970514     C*
007200970514     C**************************************
007300970514     C* REPERIMENTO DEL TERMINAL DI ARRIVO
007400970514     C**************************************
007500970514    2C     D55TPT        IFEQ      'A'
007600970514     C     D55TPT        OREQ      *BLANKS
007700970514     C*
007800970702     C                   CLEAR                   WTER
007900970702     C* CERCO IL TERMINAL SPECIFICO SOLO SE MI HANNO PASSATO LA LNP
008000970702     C* ALTRIMENTI CERCO QUELLO DI DEFAULT
008100970702    3C     D55LNP        IFGT      *ZEROS
008200000711     C* VERIFICO SE C'E' AZORG00F
008300040701    4C***        *IN99     IFEQ *OFF
008400040701     C***                  MOVELCMD,1     COMMAN 80
008500040701     C***                  Z-ADD36        LUNG   155
008600040701     C***                  CALL 'QCMDEXC'              98
008700040701     C***                  PARM           COMMAN
008800040701     C***                  PARM           LUNG
008900040701     C***     N98          OPEN AZORG01L
009000040701     C***                  SETON                     99
009100040701    4C***                  ENDIF
009200000711     C**
009300040701    4C***        *IN98     IFEQ *OFF
009400000711     C**
009500040701     C***                  CLEAROG143
009600040701     C***        D55LNP    CHAINAZORG01L             31
009700040701    5C***  N31   ORGFVA    IFNE ' '
009800040701     C***        ORGFAG    ORNE 'A'
009900040701     C***        ORGFAG    ANDNE'F'
010000040701     C***                  SETON                     31
010100040701    5C***                  ENDIF
010200040701     C***  N31             MOVELORGDE3    OG143
010300040701     C***   31             CLEAROG143
010400040701   X4C***                  ELSE
010500000711     C* 98 ON - NON TROVATO AZORG00F
010600000711     C*  TESTO FISSO 180
010700040701    5C***        D55LNP    IFEQ 180
010800040701     C***                  MOVEL'S'       §OGPT
010900040701     C***                  ELSE
011000040701     C***                  CLEAR§OGPT
011100040701    5C***                  ENDIF
011200040701    4C***                  ENDIF
011300000711     C**
011400970702     C* Per prima cosa determino il terminal di partenza di d55lnp
011500970702     C                   MOVEL     'P'           KEPA
011600970702     C                   Z-ADD     D55LNP        KTFP
011700970702     C                   MOVE      *ZEROS        KTFA
011800100617     c                   eval      kdrf=d55DRF
011900970702     C                   EXSR      RICTER
012000970702     C*
012100970702     C                   MOVEL     'A'           KEPA
012200970702    4C     WTER          IFEQ      *ZEROS
012300970702     C                   Z-ADD     D55LNP        KTFP
012400970702   X4C                   ELSE
012500970702     C                   Z-ADD     WTER          KTFP
012600970702    4C                   END
012700970702     C                   Z-ADD     D55LIN        KTFA
012800970702     C*
012900100617     c                   eval      kdrf=d55DRF
013000970702     C                   EXSR      RICTER
013100970702     C*
013200970702    3C                   END
013300970702     C* TROVATA ECCEZIONE TER-ARR
013400970702    3C     WTER          IFGT      *ZEROS
013500970702     C                   Z-ADD     WTER          D55TFA
013600970918     C                   MOVEL     'E'           D55ORR
013700970702   X3C                   ELSE
013800970702     C* SE NON HO TROVATO IL TERMINAL DI ARRIVO O SE NON MI E' STATA
013900970702     C* PASSATA LA LINEA DI PARTENZA PER CUI CONTROLLARE LE ECCEZIONI
014000970702     C* CERCO IL TERMINAL DI ARRIVO DI DEFAULT
014100970702     C                   MOVEL     'A'           KEPA
014200970702     C                   MOVE      *ZEROS        KTFP
014300970702     C                   Z-ADD     D55LIN        KTFA
014400100617     c                   eval      kdrf=d55DRF
014500970702     C                   EXSR      RICTER
014600970702    4C     WTER          IFEQ      *ZEROS
014700970702     C                   Z-ADD     D55LIN        D55TFA
014800990517     C                   SELECT
014900990517     C     D55ERR        WHENEQ    *BLANKS
015000990517     C                   MOVEL     '4'           D55ERR
015100990517     C                   MOVEL     MSG(4)        D55MSG
015200990517     C     D55ERR        WHENEQ    '3'
015300990517     C                   MOVEL     '5'           D55ERR
015400990517     C                   MOVEL     MSG(5)        D55MSG
015500990517     C                   ENDSL
015600970702   X4C                   ELSE
015700970702     C                   Z-ADD     WTER          D55TFA
015800970702    4C                   END
015900970702    3C                   END
016000970514    2C                   END
016100100617     C***************************************************
016200100617     C* REPERIMENTO DEL TERMINAL DI ARRIVO per Defluenza
016300100617     C***************************************************
016400100617    2C     D55TPT        IFEQ      'D'
016500100617     C     D55TPT        OREQ      'E'
016600100617     C*
016700100617     c* Se esiste il pgm xgiolavc calcolo la data -1 gg lavorativo
016800100617     c*  per la ricerca del terminal
016900100617     c                   clear                   dataDEF           8 0
017000100617     c                   clear                   dataRIF           8 0
017100100617     c                   clear                   lineaARR          3 0
017200100617     c                   eval      datarif=d55DRF
017300100617     c                   eval      lineaARR=d55lin
017400100617     c
017500100617     c                   call(e)   'XGIOLAVC'
017600100617     c                   parm                    dataRIF
017700100617     c                   parm                    lineaARR
017800100617     c                   parm                    dataDEF
017900100617     c
018000100617     c                   if        %error or dataDEF=0
018100100617     c                   eval      dataDEF=d55DRF
018200100617     c                   endif
018300100617     c
018400100617     C                   CLEAR                   WTER
018500100617     C* CERCO IL TERMINAL SPECIFICO SOLO SE MI HANNO PASSATO LA LNP
018600100617     C* ALTRIMENTI CERCO QUELLO DI DEFAULT
018700100617    3C     D55LNP        IFGT      *ZEROS
018800100617     C                   MOVEL     'P'           KEPA
018900100617     C                   Z-ADD     D55LNP        KTFP
019000100617     C                   MOVE      *ZEROS        KTFA
019100100617     c                   eval      kdrf=DataDEF
019200100617     C                   EXSR      RICTER
019300100617     C*
019400100617     C                   MOVEL     'A'           KEPA
019500100617    4C     WTER          IFEQ      *ZEROS
019600100617     C                   Z-ADD     D55LNP        KTFP
019700100617   X4C                   ELSE
019800100617     C                   Z-ADD     WTER          KTFP
019900100617    4C                   END
020000100617     C                   Z-ADD     D55LIN        KTFA
020100100617     C*
020200100617     c                   eval      kdrf=DataDEF
020300100617     C                   EXSR      RICTER
020400100617     C*
020500100617    3C                   END
020600100617     C* TROVATA ECCEZIONE TER-ARR
020700100617    3C     WTER          IFGT      *ZEROS
020800100617     C                   Z-ADD     WTER          D55TFA
020900100617     C                   MOVEL     'E'           D55ORR
021000100617   X3C                   ELSE
021100100617     C* SE NON HO TROVATO IL TERMINAL DI ARRIVO O SE NON MI E' STATA
021200100617     C* PASSATA LA LINEA DI PARTENZA PER CUI CONTROLLARE LE ECCEZIONI
021300100617     C* CERCO IL TERMINAL DI ARRIVO DI DEFAULT
021400100617     C                   MOVEL     'A'           KEPA
021500100617     C                   MOVE      *ZEROS        KTFP
021600100617     C                   Z-ADD     D55LIN        KTFA
021700100617     c                   eval      kdrf=dataDEF
021800100617     C                   EXSR      RICTER
021900100617    4C     WTER          IFEQ      *ZEROS
022000100617     C                   Z-ADD     D55LIN        D55TFA
022100100617     C                   SELECT
022200100617     C     D55ERR        WHENEQ    *BLANKS
022300100617     C                   MOVEL     '4'           D55ERR
022400100617     C                   MOVEL     MSG(4)        D55MSG
022500100617     C     D55ERR        WHENEQ    '3'
022600100617     C                   MOVEL     '5'           D55ERR
022700100617     C                   MOVEL     MSG(5)        D55MSG
022800100617     C                   ENDSL
022900100617   X4C                   ELSE
023000100617     C                   Z-ADD     WTER          D55TFA
023100100617    4C                   END
023200100617    3C                   END
023300100617    2C                   END
023400020529     C**************************************
023500020529     C* REPERIMENTO DEL P.O. GESTIONE DA £6
023600020529     C**************************************
023700020529    2C     D55TPT        IFEQ      '6'
023800020529     C                   MOVEL     '6'           KEPA
023900020529     C                   MOVE      *ZEROS        KTFP
024000020529     C                   Z-ADD     D55LIN        KTFA
024100100617     c                   eval      kdrf=d55DRF
024200020529     C                   EXSR      RICTER
024300020529    3C     WTER          IFEQ      *ZEROS
024400020529     C                   Z-ADD     D55LIN        D55TFA
024500020529    4C     D55ERR        IFEQ      *BLANKS
024600020529     C                   MOVEL     '6'           D55ERR
024700020529     C                   MOVEL     MSG(6)        D55MSG
024800020529    4C                   ENDIF
024900020529   X3C                   ELSE
025000020529     C                   Z-ADD     WTER          D55TFA
025100020529    3C                   END
025200020529    2C                   ENDIF
025300060705     C**************************************
025400060705     C* REPERIMENTO DEL P.O. Ottico
025500060705     C**************************************
025600060705    2C     D55TPT        IFEQ      'O'
025700060705     C                   MOVEL     'O'           KEPA
025800060705     C                   MOVE      d55lin        KTFP
025900060705     C                   clear                   KTFA
026000100617     c                   eval      kdrf=d55DRF
026100060705     C                   EXSR      RICTER
026200060705     c
026300060705    3C     WTER          IFEQ      *ZEROS
026400060705     c* Prendo il terminal di partenza STD se non trovato
026500060705     C                   MOVEL     'P'           KEPA
026600060705     C                   MOVE      d55lin        KTFP
026700060705     C                   clear                   KTFA
026800100617     c                   eval      kdrf=d55DRF
026900060705     C                   EXSR      RICTER
027000060705     c
027100060705    4C     wter          IFEQ      0
027200060705     C                   Z-ADD     D55LIN        D55TFP
027300060705     C                   MOVEL     '3'           D55ERR
027400060705     C                   MOVEL     MSG(3)        D55MSG
027500060705   x4c                   else
027600060705     C                   Z-ADD     WTER          D55TFP
027700060705    4C                   ENDIF
027800060705     c
027900060705   X3C                   ELSE
028000060705     c* Trovato record  "O" --> eccezione su terminal di partenza
028100060705     c*                         per ottico
028200060705     C                   Z-ADD     WTER          D55TFP
028300060705    3C                   END
028400060705    2C                   ENDIF
028500970514     C*
028600970514     C     FINE          TAG
028700970424     C*
028800970424    1C                   ENDIF
028900970424     C* CHIUSURA PGM
029000970514    1C     D55TLA        IFEQ      ' '
029100970424     C                   RETURN
029200970424     C                   ELSE
029300970424     C                   SETON                                        LR
029400970424    1C                   ENDIF
029500970702     C**************************************************************************
029600970702     C* REPERIMENTO TERMINAL
029700970702     C**************************************************************************
029800970702     C     RICTER        BEGSR
029900970702     C*
030000970702     C                   CLEAR                   WTER
030100970702     C*
030200970702     C     KCAE1         SETLL     AZCAE01L                               33
030300970702     C   33KCAE1         READE     AZCAE01L                               34
030400970702     C  N33KCAE1A        READPE    AZCAE01L                               34
030500970702    3C     *IN34         DOWEQ     *OFF
030600970702     C     CAEATB        ANDNE     *BLANKS
030700970702     C     KCAE1A        READPE    AZCAE01L                               34
030800970702    3C                   ENDDO
030900970702    3C     *IN34         IFEQ      *OFF
031000100617     C     CAEDSC        ANDLT     KDRF
031100970702     C                   SETON                                        34
031200970702    3C                   END
031300970702     C* 34 OFF --> TROVATO IL TERMINAL
031400970702     C     *IN34         IFEQ      *OFF
031500970702     C                   MOVE      CAETFE        WTER
031600970702     C                   END
031700970702     C                   ENDSR
031800000000     C**************************************************************************
031900000000     C* R O U T I N E    I N I Z I A L E
032000000000     C**************************************************************************
032100000000     C     *INZSR        BEGSR
032200950130     C*
032300970514     C     KCAE1         KLIST
032400970514     C                   KFLD                    KEPA
032500970514     C                   KFLD                    KTFP
032600970702     C                   KFLD                    KTFA
032700100617     C                   KFLD                    kdrf
032800970514     C     KCAE1A        KLIST
032900970514     C                   KFLD                    KEPA
033000970514     C                   KFLD                    KTFP
033100970702     C                   KFLD                    KTFA
033200970430     C*
033300970430     C* DEFINIZIONE DI ALCUNI CAMPI
033400970514     C     *LIKE         DEFINE    CAEEPA        KEPA
033500970514     C     *LIKE         DEFINE    CAETFP        KTFP
033600970702     C     *LIKE         DEFINE    CAETFP        WTER
033700970702     C     *LIKE         DEFINE    CAETFA        KTFA
033800100617     C     *LIKE         DEFINE    CAEDDE        KDRF
033900970430     C*
034000000000     C                   ENDSR
034100020529**
034200970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA LINEA PER CUI CALCOLARLO
034300970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA DATA DI RIFERIMENTO
034400990517NON TROVATO TERMINAL DI PARTENZA DI D55LIN:ASSUNTO D55LIN
034500990517NON TROVATO TERMINAL DI ARRIVO   DI D55LIN:ASSUNTO D55LIN
034600990517NON TROVATI TERMINAL DI PARTENZA E DI ARRIVO: ASSUNTO D55LIN
034700020529NON TROVATO P.O. GESTIONE DI D55LIN:ASSUNTO D55LIN

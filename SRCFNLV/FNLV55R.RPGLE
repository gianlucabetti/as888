000100950130     H DECEDIT('0,') DATEDIT(*DMY.)
000200061207
000300061207      *---------------------------------------------------------------*
000400140124      *  -- ATTENZIONE --- COMPILARE *TGTRLS il più vecchio possibile
000500140117      * NB: Ricordarsi di non modificare mai FNLV55R ma creare sempre un
000600140117      *     FNLV55R perchè i clienti scaricano e utilizzano il sorgente
000700140117      *     e NON AGGIUNGERE FILE/RICHIAMI
000800061207      *---------------------------------------------------------------*
000900061207
001000970514     F* FNLV55R *-----------------------------------------------------*
001100060705     F*       REPERIMENTO:
001200100617     F*       A/P/_       - TERMINAL DI ARRIVO E/O DI PARTENZA
001300100617     F*         6         - P.O. GESTIONE DA £6
001400140117     F*         O         - P.O. Ottico
001500140117     F*         J         - legame contabile
001600100617     F*       D/P/E       - TERMINAL DI ARRIVO PER DEFLUENZA E/O DI PARTENZA
001700950130     F*---------------------------------------------------------------*
001800061207
001900970514     FAZCAE01L  IF   E           K DISK
002000020529     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
002100040701     D***                 CMD     1   1 36               CHKBJ
002200970514     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
002300970514     D DSLV55        E DS                  EXTNAME(FNLV55DS)
002400970424     C**************************************************************************
002500970424     C     *ENTRY        PLIST
002600970514     C                   PARM                    DSLV55
002700970424     C*
002800970514     C* TIPO LANCIO, D55TLA "C" -           CHIUSO CON LR
002900970514     C* TIPO LANCIO, D55TLA "L" - ELABORO E CHIUDO CON LR
003000970514     C* TIPO LANCIO, D55TLA " " - ELABORO E CHIUDO IN RETRN
003100970514    1C     D55TLA        IFNE      'C'
003200970514     C* Pulizia dei campi di output
003300970514     C                   CLEAR                   D55ERR
003400970514     C                   CLEAR                   D55MSG
003500970514     C                   CLEAR                   D55TFP
003600970514     C                   CLEAR                   D55TFA
003700970918     C                   CLEAR                   D55ORP
003800970918     C                   CLEAR                   D55ORR
003900970516     C* ERRORE SE NON PASSATA LA LINEA PER CUI DETERMINARE IL TERMINAL
004000970516     C     D55LIN        IFEQ      *ZEROS
004100970516     C                   MOVEL     '1'           D55ERR
004200970516     C                   MOVEL     MSG(1)        D55MSG
004300970516     C                   GOTO      FINE
004400970516     C                   END
004500970702     C* ERRORE SE NON PASSATA LA DATA DI RIFERIMENTO
004600970702     C     D55DRF        IFEQ      *ZEROS
004700970702     C                   MOVEL     '2'           D55ERR
004800970703     C                   MOVEL     MSG(2)        D55MSG
004900970702     C                   GOTO      FINE
005000970702     C                   END
005100970514     C*
005200970514     C**************************************
005300970514     C* REPERIMENTO DEL TERMINAL DI PARTENZA
005400970514     C**************************************
005500970514    2C     D55TPT        IFEQ      'P'
005600970514     C     D55TPT        OREQ      ' '
005700100617     C     D55TPT        OREQ      'E'
005800100617     c
005900970702     C                   MOVEL     'P'           KEPA
006000970702     C                   Z-ADD     D55LIN        KTFP
006100970702     C                   MOVE      *ZEROS        KTFA
006200100617     c                   eval      kdrf=d55DRF
006300970702     C                   EXSR      RICTER
006400970702     C     WTER          IFEQ      *ZEROS
006500970702     C                   Z-ADD     D55LIN        D55TFP
006600990517     C                   MOVEL     '3'           D55ERR
006700990517     C                   MOVEL     MSG(3)        D55MSG
006800970702     C                   ELSE
006900970702     C                   Z-ADD     WTER          D55TFP
007000970702     C                   END
007100970514    2C                   END
007200970514     C*
007300970514     C**************************************
007400970514     C* REPERIMENTO DEL TERMINAL DI ARRIVO
007500970514     C**************************************
007600970514    2C     D55TPT        IFEQ      'A'
007700970514     C     D55TPT        OREQ      *BLANKS
007800970514     C*
007900970702     C                   CLEAR                   WTER
008000970702     C* CERCO IL TERMINAL SPECIFICO SOLO SE MI HANNO PASSATO LA LNP
008100970702     C* ALTRIMENTI CERCO QUELLO DI DEFAULT
008200970702    3C     D55LNP        IFGT      *ZEROS
008300000711     C* VERIFICO SE C'E' AZORG00F
008400040701    4C***        *IN99     IFEQ *OFF
008500040701     C***                  MOVELCMD,1     COMMAN 80
008600040701     C***                  Z-ADD36        LUNG   155
008700040701     C***                  CALL 'QCMDEXC'              98
008800040701     C***                  PARM           COMMAN
008900040701     C***                  PARM           LUNG
009000040701     C***     N98          OPEN AZORG01L
009100040701     C***                  SETON                     99
009200040701    4C***                  ENDIF
009300000711     C**
009400040701    4C***        *IN98     IFEQ *OFF
009500000711     C**
009600040701     C***                  CLEAROG143
009700040701     C***        D55LNP    CHAINAZORG01L             31
009800040701    5C***  N31   ORGFVA    IFNE ' '
009900040701     C***        ORGFAG    ORNE 'A'
010000040701     C***        ORGFAG    ANDNE'F'
010100040701     C***                  SETON                     31
010200040701    5C***                  ENDIF
010300040701     C***  N31             MOVELORGDE3    OG143
010400040701     C***   31             CLEAROG143
010500040701   X4C***                  ELSE
010600000711     C* 98 ON - NON TROVATO AZORG00F
010700000711     C*  TESTO FISSO 180
010800040701    5C***        D55LNP    IFEQ 180
010900040701     C***                  MOVEL'S'       §OGPT
011000040701     C***                  ELSE
011100040701     C***                  CLEAR§OGPT
011200040701    5C***                  ENDIF
011300040701    4C***                  ENDIF
011400000711     C**
011500970702     C* Per prima cosa determino il terminal di partenza di d55lnp
011600970702     C                   MOVEL     'P'           KEPA
011700970702     C                   Z-ADD     D55LNP        KTFP
011800970702     C                   MOVE      *ZEROS        KTFA
011900100617     c                   eval      kdrf=d55DRF
012000970702     C                   EXSR      RICTER
012100970702     C*
012200970702     C                   MOVEL     'A'           KEPA
012300970702    4C     WTER          IFEQ      *ZEROS
012400970702     C                   Z-ADD     D55LNP        KTFP
012500970702   X4C                   ELSE
012600970702     C                   Z-ADD     WTER          KTFP
012700970702    4C                   END
012800970702     C                   Z-ADD     D55LIN        KTFA
012900970702     C*
013000100617     c                   eval      kdrf=d55DRF
013100970702     C                   EXSR      RICTER
013200970702     C*
013300970702    3C                   END
013400970702     C* TROVATA ECCEZIONE TER-ARR
013500970702    3C     WTER          IFGT      *ZEROS
013600970702     C                   Z-ADD     WTER          D55TFA
013700970918     C                   MOVEL     'E'           D55ORR
013800970702   X3C                   ELSE
013900970702     C* SE NON HO TROVATO IL TERMINAL DI ARRIVO O SE NON MI E' STATA
014000970702     C* PASSATA LA LINEA DI PARTENZA PER CUI CONTROLLARE LE ECCEZIONI
014100970702     C* CERCO IL TERMINAL DI ARRIVO DI DEFAULT
014200970702     C                   MOVEL     'A'           KEPA
014300970702     C                   MOVE      *ZEROS        KTFP
014400970702     C                   Z-ADD     D55LIN        KTFA
014500100617     c                   eval      kdrf=d55DRF
014600970702     C                   EXSR      RICTER
014700970702    4C     WTER          IFEQ      *ZEROS
014800970702     C                   Z-ADD     D55LIN        D55TFA
014900990517     C                   SELECT
015000990517     C     D55ERR        WHENEQ    *BLANKS
015100990517     C                   MOVEL     '4'           D55ERR
015200990517     C                   MOVEL     MSG(4)        D55MSG
015300990517     C     D55ERR        WHENEQ    '3'
015400990517     C                   MOVEL     '5'           D55ERR
015500990517     C                   MOVEL     MSG(5)        D55MSG
015600990517     C                   ENDSL
015700970702   X4C                   ELSE
015800970702     C                   Z-ADD     WTER          D55TFA
015900970702    4C                   END
016000970702    3C                   END
016100970514    2C                   END
016200100617     C***************************************************
016300100617     C* REPERIMENTO DEL TERMINAL DI ARRIVO per Defluenza
016400100617     C***************************************************
016500100617    2C     D55TPT        IFEQ      'D'
016600100617     C     D55TPT        OREQ      'E'
016700100617     C*
016800100617     c* Se esiste il pgm xgiolavc calcolo la data -1 gg lavorativo
016900100617     c*  per la ricerca del terminal
017000100617     c                   clear                   dataDEF           8 0
017100100617     c                   clear                   dataRIF           8 0
017200100617     c                   clear                   lineaARR          3 0
017300100617     c                   eval      datarif=d55DRF
017400100617     c                   eval      lineaARR=d55lin
017500100617     c
017600100617     c                   call(e)   'XGIOLAVC'
017700100617     c                   parm                    dataRIF
017800100617     c                   parm                    lineaARR
017900100617     c                   parm                    dataDEF
018000100617     c
018100100617     c                   if        %error or dataDEF=0
018200100617     c                   eval      dataDEF=d55DRF
018300100617     c                   endif
018400100617     c
018500100617     C                   CLEAR                   WTER
018600100617     C* CERCO IL TERMINAL SPECIFICO SOLO SE MI HANNO PASSATO LA LNP
018700100617     C* ALTRIMENTI CERCO QUELLO DI DEFAULT
018800100617    3C     D55LNP        IFGT      *ZEROS
018900100617     C                   MOVEL     'P'           KEPA
019000100617     C                   Z-ADD     D55LNP        KTFP
019100100617     C                   MOVE      *ZEROS        KTFA
019200100617     c                   eval      kdrf=DataDEF
019300100617     C                   EXSR      RICTER
019400100617     C*
019500100617     C                   MOVEL     'A'           KEPA
019600100617    4C     WTER          IFEQ      *ZEROS
019700100617     C                   Z-ADD     D55LNP        KTFP
019800100617   X4C                   ELSE
019900100617     C                   Z-ADD     WTER          KTFP
020000100617    4C                   END
020100100617     C                   Z-ADD     D55LIN        KTFA
020200100617     C*
020300100617     c                   eval      kdrf=DataDEF
020400100617     C                   EXSR      RICTER
020500100617     C*
020600100617    3C                   END
020700100617     C* TROVATA ECCEZIONE TER-ARR
020800100617    3C     WTER          IFGT      *ZEROS
020900100617     C                   Z-ADD     WTER          D55TFA
021000100617     C                   MOVEL     'E'           D55ORR
021100100617   X3C                   ELSE
021200100617     C* SE NON HO TROVATO IL TERMINAL DI ARRIVO O SE NON MI E' STATA
021300100617     C* PASSATA LA LINEA DI PARTENZA PER CUI CONTROLLARE LE ECCEZIONI
021400100617     C* CERCO IL TERMINAL DI ARRIVO DI DEFAULT
021500100617     C                   MOVEL     'A'           KEPA
021600100617     C                   MOVE      *ZEROS        KTFP
021700100617     C                   Z-ADD     D55LIN        KTFA
021800100617     c                   eval      kdrf=dataDEF
021900100617     C                   EXSR      RICTER
022000100617    4C     WTER          IFEQ      *ZEROS
022100100617     C                   Z-ADD     D55LIN        D55TFA
022200100617     C                   SELECT
022300100617     C     D55ERR        WHENEQ    *BLANKS
022400100617     C                   MOVEL     '4'           D55ERR
022500100617     C                   MOVEL     MSG(4)        D55MSG
022600100617     C     D55ERR        WHENEQ    '3'
022700100617     C                   MOVEL     '5'           D55ERR
022800100617     C                   MOVEL     MSG(5)        D55MSG
022900100617     C                   ENDSL
023000100617   X4C                   ELSE
023100100617     C                   Z-ADD     WTER          D55TFA
023200100617    4C                   END
023300100617    3C                   END
023400100617    2C                   END
023500020529     C**************************************
023600020529     C* REPERIMENTO DEL P.O. GESTIONE DA £6
023700020529     C**************************************
023800020529    2C     D55TPT        IFEQ      '6'
023900020529     C                   MOVEL     '6'           KEPA
024000020529     C                   MOVE      *ZEROS        KTFP
024100020529     C                   Z-ADD     D55LIN        KTFA
024200100617     c                   eval      kdrf=d55DRF
024300020529     C                   EXSR      RICTER
024400140117
024500020529    3C     WTER          IFEQ      *ZEROS
024600020529     C                   Z-ADD     D55LIN        D55TFA
024700020529    4C     D55ERR        IFEQ      *BLANKS
024800020529     C                   MOVEL     '6'           D55ERR
024900020529     C                   MOVEL     MSG(6)        D55MSG
025000020529    4C                   ENDIF
025100020529   X3C                   ELSE
025200020529     C                   Z-ADD     WTER          D55TFA
025300020529    3C                   END
025400020529    2C                   ENDIF
025500060705     C**************************************
025600060705     C* REPERIMENTO DEL P.O. Ottico
025700060705     C**************************************
025800060705    2C     D55TPT        IFEQ      'O'
025900060705     C                   MOVEL     'O'           KEPA
026000060705     C                   MOVE      d55lin        KTFP
026100060705     C                   clear                   KTFA
026200100617     c                   eval      kdrf=d55DRF
026300060705     C                   EXSR      RICTER
026400060705     c
026500060705    3C     WTER          IFEQ      *ZEROS
026600060705     c* Prendo il terminal di partenza STD se non trovato
026700060705     C                   MOVEL     'P'           KEPA
026800060705     C                   MOVE      d55lin        KTFP
026900060705     C                   clear                   KTFA
027000100617     c                   eval      kdrf=d55DRF
027100060705     C                   EXSR      RICTER
027200060705     c
027300060705    4C     wter          IFEQ      0
027400060705     C                   Z-ADD     D55LIN        D55TFP
027500060705     C                   MOVEL     '3'           D55ERR
027600060705     C                   MOVEL     MSG(3)        D55MSG
027700060705   x4c                   else
027800060705     C                   Z-ADD     WTER          D55TFP
027900060705    4C                   ENDIF
028000060705     c
028100060705   X3C                   ELSE
028200060705     c* Trovato record  "O" --> eccezione su terminal di partenza
028300060705     c*                         per ottico
028400060705     C                   Z-ADD     WTER          D55TFP
028500060705    3C                   END
028600060705    2C                   ENDIF
028700140117     C**************************************
028800140117     C* REPERIMENTO DEL P.O. legame contabile
028900140117     C**************************************
029000140117    2C     D55TPT        IFEQ      'J'
029100140117     C                   MOVEL     'J'           KEPA
029200140117     C                   clear                   KTFP
029300140117     C                   MOVE      d55lin        KTFA
029400140117     c                   eval      kdrf=d55DRF
029500140117     C                   EXSR      RICTER
029600140117     c
029700140117    3C     WTER          IFEQ      *ZEROS
029800140117     C                   Z-ADD     D55LIN        D55TFA
029900140117    4C     D55ERR        IFEQ      *BLANKS
030000140117     C                   MOVEL     '6'           D55ERR
030100140117     C                   MOVEL     MSG(6)        D55MSG
030200140117    4C                   ENDIF
030300140117   X3C                   ELSE
030400140117     C                   Z-ADD     WTER          D55TFA
030500140117    3C                   END
030600140117    2C                   ENDIF
030700140117     C**************************************
030800970514     C*
030900970514     C     FINE          TAG
031000970424     C*
031100970424    1C                   ENDIF
031200970424     C* CHIUSURA PGM
031300970514    1C     D55TLA        IFEQ      ' '
031400970424     C                   RETURN
031500970424     C                   ELSE
031600970424     C                   SETON                                        LR
031700970424    1C                   ENDIF
031800970702     C**************************************************************************
031900970702     C* REPERIMENTO TERMINAL
032000970702     C**************************************************************************
032100970702     C     RICTER        BEGSR
032200970702     C*
032300970702     C                   CLEAR                   WTER
032400970702     C*
032500970702     C     KCAE1         SETLL     AZCAE01L                               33
032600970702     C   33KCAE1         READE     AZCAE01L                               34
032700970702     C  N33KCAE1A        READPE    AZCAE01L                               34
032800970702    3C     *IN34         DOWEQ     *OFF
032900970702     C     CAEATB        ANDNE     *BLANKS
033000970702     C     KCAE1A        READPE    AZCAE01L                               34
033100970702    3C                   ENDDO
033200970702    3C     *IN34         IFEQ      *OFF
033300100617     C     CAEDSC        ANDLT     KDRF
033400970702     C                   SETON                                        34
033500970702    3C                   END
033600970702     C* 34 OFF --> TROVATO IL TERMINAL
033700970702     C     *IN34         IFEQ      *OFF
033800970702     C                   MOVE      CAETFE        WTER
033900970702     C                   END
034000970702     C                   ENDSR
034100000000     C**************************************************************************
034200000000     C* R O U T I N E    I N I Z I A L E
034300000000     C**************************************************************************
034400000000     C     *INZSR        BEGSR
034500950130     C*
034600970514     C     KCAE1         KLIST
034700970514     C                   KFLD                    KEPA
034800970514     C                   KFLD                    KTFP
034900970702     C                   KFLD                    KTFA
035000100617     C                   KFLD                    kdrf
035100970514     C     KCAE1A        KLIST
035200970514     C                   KFLD                    KEPA
035300970514     C                   KFLD                    KTFP
035400970702     C                   KFLD                    KTFA
035500970430     C*
035600970430     C* DEFINIZIONE DI ALCUNI CAMPI
035700970514     C     *LIKE         DEFINE    CAEEPA        KEPA
035800970514     C     *LIKE         DEFINE    CAETFP        KTFP
035900970702     C     *LIKE         DEFINE    CAETFP        WTER
036000970702     C     *LIKE         DEFINE    CAETFA        KTFA
036100100617     C     *LIKE         DEFINE    CAEDDE        KDRF
036200970430     C*
036300000000     C                   ENDSR
036400020529**
036500970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA LINEA PER CUI CALCOLARLO
036600970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA DATA DI RIFERIMENTO
036700990517NON TROVATO TERMINAL DI PARTENZA DI D55LIN:ASSUNTO D55LIN
036800990517NON TROVATO TERMINAL DI ARRIVO   DI D55LIN:ASSUNTO D55LIN
036900990517NON TROVATI TERMINAL DI PARTENZA E DI ARRIVO: ASSUNTO D55LIN
037000020529NON TROVATO P.O. GESTIONE DI D55LIN:ASSUNTO D55LIN

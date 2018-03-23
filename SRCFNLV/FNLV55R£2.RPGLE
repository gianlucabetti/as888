000100950130     H DECEDIT('0,') DATEDIT(*DMY.)
000101061207
000102061207      *---------------------------------------------------------------*
000103061207      *  -- ATTENZIONE --- COMPILARE V5R1M0
000104061207      *---------------------------------------------------------------*
000106061207
000200970514     F* FNLV55R *-----------------------------------------------------*
000300060705     F*       REPERIMENTO:
000301060705     F*                   - TERMINAL DI ARRIVO E/O DI PARTENZA
000400060705     F*                   - P.O. GESTIONE DA £6
000401060705     F*                   - P.O. Ottico
000500950130     F*---------------------------------------------------------------*
000501061207
000600970514     FAZCAE01L  IF   E           K DISK
000800020529     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
000900040701     D***                 CMD     1   1 36               CHKBJ
001000970514     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
001100970514     D DSLV55        E DS                  EXTNAME(FNLV55DS)
001300970424     C**************************************************************************
001400970424     C     *ENTRY        PLIST
001500970514     C                   PARM                    DSLV55
001600970424     C*
001700970514     C* TIPO LANCIO, D55TLA "C" -           CHIUSO CON LR
001800970514     C* TIPO LANCIO, D55TLA "L" - ELABORO E CHIUDO CON LR
001900970514     C* TIPO LANCIO, D55TLA " " - ELABORO E CHIUDO IN RETRN
002000970514    1C     D55TLA        IFNE      'C'
002100970514     C* Pulizia dei campi di output
002200970514     C                   CLEAR                   D55ERR
002300970514     C                   CLEAR                   D55MSG
002400970514     C                   CLEAR                   D55TFP
002500970514     C                   CLEAR                   D55TFA
002600970918     C                   CLEAR                   D55ORP
002700970918     C                   CLEAR                   D55ORR
002800970516     C* ERRORE SE NON PASSATA LA LINEA PER CUI DETERMINARE IL TERMINAL
002900970516     C     D55LIN        IFEQ      *ZEROS
003000970516     C                   MOVEL     '1'           D55ERR
003100970516     C                   MOVEL     MSG(1)        D55MSG
003200970516     C                   GOTO      FINE
003300970516     C                   END
003400970702     C* ERRORE SE NON PASSATA LA DATA DI RIFERIMENTO
003500970702     C     D55DRF        IFEQ      *ZEROS
003600970702     C                   MOVEL     '2'           D55ERR
003700970703     C                   MOVEL     MSG(2)        D55MSG
003800970702     C                   GOTO      FINE
003900970702     C                   END
004000970514     C*
004100970514     C**************************************
004200970514     C* REPERIMENTO DEL TERMINAL DI PARTENZA
004300970514     C**************************************
004400970514    2C     D55TPT        IFEQ      'P'
004500970514     C     D55TPT        OREQ      ' '
004600970702     C                   MOVEL     'P'           KEPA
004700970702     C                   Z-ADD     D55LIN        KTFP
004800970702     C                   MOVE      *ZEROS        KTFA
004900970702     C                   EXSR      RICTER
005000970702     C     WTER          IFEQ      *ZEROS
005100970702     C                   Z-ADD     D55LIN        D55TFP
005200990517     C                   MOVEL     '3'           D55ERR
005300990517     C                   MOVEL     MSG(3)        D55MSG
005400970702     C                   ELSE
005500970702     C                   Z-ADD     WTER          D55TFP
005600970702     C                   END
005700970514    2C                   END
005800970514     C*
005900970514     C**************************************
006000970514     C* REPERIMENTO DEL TERMINAL DI ARRIVO
006100970514     C**************************************
006200970514    2C     D55TPT        IFEQ      'A'
006300970514     C     D55TPT        OREQ      *BLANKS
006400970514     C*
006500970702     C                   CLEAR                   WTER
006600970702     C* CERCO IL TERMINAL SPECIFICO SOLO SE MI HANNO PASSATO LA LNP
006700970702     C* ALTRIMENTI CERCO QUELLO DI DEFAULT
006800970702    3C     D55LNP        IFGT      *ZEROS
006900000711     C* VERIFICO SE C'E' AZORG00F
007000040701    4C***        *IN99     IFEQ *OFF
007100040701     C***                  MOVELCMD,1     COMMAN 80
007200040701     C***                  Z-ADD36        LUNG   155
007300040701     C***                  CALL 'QCMDEXC'              98
007400040701     C***                  PARM           COMMAN
007500040701     C***                  PARM           LUNG
007600040701     C***     N98          OPEN AZORG01L
007700040701     C***                  SETON                     99
007800040701    4C***                  ENDIF
007900000711     C**
008000040701    4C***        *IN98     IFEQ *OFF
008100000711     C**
008200040701     C***                  CLEAROG143
008300040701     C***        D55LNP    CHAINAZORG01L             31
008400040701    5C***  N31   ORGFVA    IFNE ' '
008500040701     C***        ORGFAG    ORNE 'A'
008600040701     C***        ORGFAG    ANDNE'F'
008700040701     C***                  SETON                     31
008800040701    5C***                  ENDIF
008900040701     C***  N31             MOVELORGDE3    OG143
009000040701     C***   31             CLEAROG143
009100040701   X4C***                  ELSE
009200000711     C* 98 ON - NON TROVATO AZORG00F
009300000711     C*  TESTO FISSO 180
009400040701    5C***        D55LNP    IFEQ 180
009500040701     C***                  MOVEL'S'       §OGPT
009600040701     C***                  ELSE
009700040701     C***                  CLEAR§OGPT
009800040701    5C***                  ENDIF
009900040701    4C***                  ENDIF
010000000711     C**
010100970702     C* Per prima cosa determino il terminal di partenza di d55lnp
010200000606     C* Solo se non è p.o. poste
010300040701    4C***        §OGPT     IFNE 'S'
010400970702     C                   MOVEL     'P'           KEPA
010500970702     C                   Z-ADD     D55LNP        KTFP
010600970702     C                   MOVE      *ZEROS        KTFA
010700970702     C                   EXSR      RICTER
010800040701    4C***                  ENDIF
010900970702     C*
011000970702     C                   MOVEL     'A'           KEPA
011100970702    4C     WTER          IFEQ      *ZEROS
011200970702     C                   Z-ADD     D55LNP        KTFP
011300970702   X4C                   ELSE
011400970702     C                   Z-ADD     WTER          KTFP
011500970702    4C                   END
011600970702     C                   Z-ADD     D55LIN        KTFA
011700970702     C*
011800970702     C                   EXSR      RICTER
011900970702     C*
012000970702    3C                   END
012100970702     C* TROVATA ECCEZIONE TER-ARR
012200970702    3C     WTER          IFGT      *ZEROS
012300970702     C                   Z-ADD     WTER          D55TFA
012400970918     C                   MOVEL     'E'           D55ORR
012500970702   X3C                   ELSE
012600970702     C* SE NON HO TROVATO IL TERMINAL DI ARRIVO O SE NON MI E' STATA
012700970702     C* PASSATA LA LINEA DI PARTENZA PER CUI CONTROLLARE LE ECCEZIONI
012800970702     C* CERCO IL TERMINAL DI ARRIVO DI DEFAULT
012900970702     C                   MOVEL     'A'           KEPA
013000970702     C                   MOVE      *ZEROS        KTFP
013100970702     C                   Z-ADD     D55LIN        KTFA
013200970702     C                   EXSR      RICTER
013300970702    4C     WTER          IFEQ      *ZEROS
013400970702     C                   Z-ADD     D55LIN        D55TFA
013500990517     C                   SELECT
013600990517     C     D55ERR        WHENEQ    *BLANKS
013700990517     C                   MOVEL     '4'           D55ERR
013800990517     C                   MOVEL     MSG(4)        D55MSG
013900990517     C     D55ERR        WHENEQ    '3'
014000990517     C                   MOVEL     '5'           D55ERR
014100990517     C                   MOVEL     MSG(5)        D55MSG
014200990517     C                   ENDSL
014300970702   X4C                   ELSE
014400970702     C                   Z-ADD     WTER          D55TFA
014500970702    4C                   END
014600970702    3C                   END
014700970514    2C                   END
014800020529     C**************************************
014900020529     C* REPERIMENTO DEL P.O. GESTIONE DA £6
015000020529     C**************************************
015100020529    2C     D55TPT        IFEQ      '6'
015200020529     C                   MOVEL     '6'           KEPA
015300020529     C                   MOVE      *ZEROS        KTFP
015400020529     C                   Z-ADD     D55LIN        KTFA
015500020529     C                   EXSR      RICTER
015600020529    3C     WTER          IFEQ      *ZEROS
015700020529     C                   Z-ADD     D55LIN        D55TFA
015800020529    4C     D55ERR        IFEQ      *BLANKS
015900020529     C                   MOVEL     '6'           D55ERR
016000020529     C                   MOVEL     MSG(6)        D55MSG
016100020529    4C                   ENDIF
016200020529   X3C                   ELSE
016300020529     C                   Z-ADD     WTER          D55TFA
016400020529    3C                   END
016500020529    2C                   ENDIF
016501060705     C**************************************
016502060705     C* REPERIMENTO DEL P.O. Ottico
016503060705     C**************************************
016504060705    2C     D55TPT        IFEQ      'O'
016505060705     C                   MOVEL     'O'           KEPA
016506060705     C                   MOVE      d55lin        KTFP
016507060705     C                   clear                   KTFA
016508060705     C                   EXSR      RICTER
016509060705     c
016510060705    3C     WTER          IFEQ      *ZEROS
016511060705     c* Prendo il terminal di partenza STD se non trovato
016512060705     C                   MOVEL     'P'           KEPA
016513060705     C                   MOVE      d55lin        KTFP
016514060705     C                   clear                   KTFA
016515060705     C                   EXSR      RICTER
016516060705     c
016517060705    4C     wter          IFEQ      0
016518060705     C                   Z-ADD     D55LIN        D55TFP
016519060705     C                   MOVEL     '3'           D55ERR
016520060705     C                   MOVEL     MSG(3)        D55MSG
016522060705   x4c                   else
016523060705     C                   Z-ADD     WTER          D55TFP
016524060705    4C                   ENDIF
016525060705     c
016526060705   X3C                   ELSE
016527060705     c* Trovato record  "O" --> eccezione su terminal di partenza
016528060705     c*                         per ottico
016529060705     C                   Z-ADD     WTER          D55TFP
016530060705    3C                   END
016531060705    2C                   ENDIF
016600970514     C*
016700970514     C     FINE          TAG
016800970424     C*
016900970424    1C                   ENDIF
017000970424     C* CHIUSURA PGM
017100970514    1C     D55TLA        IFEQ      ' '
017200970424     C                   RETURN
017300970424     C                   ELSE
017400970424     C                   SETON                                        LR
017500970424    1C                   ENDIF
017600970702     C**************************************************************************
017700970702     C* REPERIMENTO TERMINAL
017800970702     C**************************************************************************
017900970702     C     RICTER        BEGSR
018000970702     C*
018100970702     C                   CLEAR                   WTER
018200970702     C*
018300970702     C     KCAE1         SETLL     AZCAE01L                               33
018400970702     C   33KCAE1         READE     AZCAE01L                               34
018500970702     C  N33KCAE1A        READPE    AZCAE01L                               34
018600970702    3C     *IN34         DOWEQ     *OFF
018700970702     C     CAEATB        ANDNE     *BLANKS
018800970702     C     KCAE1A        READPE    AZCAE01L                               34
018900970702    3C                   ENDDO
019000970702    3C     *IN34         IFEQ      *OFF
019100970702     C     CAEDSC        ANDLT     D55DRF
019200970702     C                   SETON                                        34
019300970702    3C                   END
019400970702     C* 34 OFF --> TROVATO IL TERMINAL
019500970702     C     *IN34         IFEQ      *OFF
019600970702     C                   MOVE      CAETFE        WTER
019700970702     C                   END
019800970702     C                   ENDSR
019900000000     C**************************************************************************
020000000000     C* R O U T I N E    I N I Z I A L E
020100000000     C**************************************************************************
020200000000     C     *INZSR        BEGSR
020300950130     C*
020400970514     C     KCAE1         KLIST
020500970514     C                   KFLD                    KEPA
020600970514     C                   KFLD                    KTFP
020700970702     C                   KFLD                    KTFA
020800970514     C                   KFLD                    D55DRF
020900970514     C     KCAE1A        KLIST
021000970514     C                   KFLD                    KEPA
021100970514     C                   KFLD                    KTFP
021200970702     C                   KFLD                    KTFA
021300970430     C*
021400970430     C* DEFINIZIONE DI ALCUNI CAMPI
021500970514     C     *LIKE         DEFINE    CAEEPA        KEPA
021600970514     C     *LIKE         DEFINE    CAETFP        KTFP
021700970702     C     *LIKE         DEFINE    CAETFP        WTER
021800970702     C     *LIKE         DEFINE    CAETFA        KTFA
021900970430     C*
022000000000     C                   ENDSR
022100020529**
022200970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA LINEA PER CUI CALCOLARLO
022300970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA DATA DI RIFERIMENTO
022400990517NON TROVATO TERMINAL DI PARTENZA DI D55LIN:ASSUNTO D55LIN
022500990517NON TROVATO TERMINAL DI ARRIVO   DI D55LIN:ASSUNTO D55LIN
022600990517NON TROVATI TERMINAL DI PARTENZA E DI ARRIVO: ASSUNTO D55LIN
022700020529NON TROVATO P.O. GESTIONE DI D55LIN:ASSUNTO D55LIN

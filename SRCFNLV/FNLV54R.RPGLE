000100950130     H DECEDIT('0,') DATEDIT(*DMY.)
000200030224     F* FNLV54R *-----------------------------------------------------*
000300970514     F*       REPERIMENTO TERMINAL DI ARRIVO E/O DI PARTENZA
000400020529     F*       O REPERIMENTO P.O. GESTIONE DA £6
000500950130     F*---------------------------------------------------------------*
000600970514     FAZCAE01L  IF   E           K DISK
000700030224     FAZORG01L  IF   E           K DISK
000800030224
000900020529     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
001000030224
001100030224     d kazorg          s                   Like(orgfil)
001200030224
001300030224      * DS PER PASSAGGIO PARAMETRI
001400030224     D FNLV54DS      E DS
001500000606     D OG143         E DS
001600970424     C**************************************************************************
001700970424     C     *ENTRY        PLIST
001800030224     C                   PARM                    fnlv54ds
001900970424     C*
002000030226     C* TIPO LANCIO, I54TLA "C" -           CHIUSO CON LR
002100030226     C* TIPO LANCIO, I54TLA "L" - ELABORO E CHIUDO CON LR
002200030226     C* TIPO LANCIO, I54TLA " " - ELABORO E CHIUDO IN RETRN
002300030226    1C     I54TLA        IFNE      'C'
002400970514     C* Pulizia dei campi di output
002500030226     C                   CLEAR                   O54ERR
002600030226     C                   CLEAR                   O54MSG
002700030226     C                   CLEAR                   O54TFP
002800030226     C                   CLEAR                   O54TFA
002900030226     C                   CLEAR                   O54ORP
003000030226     C                   CLEAR                   O54ORR
003100030226     c                   clear                   O54aslnp
003200030226     c                   clear                   O54aslin
003300030226     c                   clear                   O54astfp
003400030226     c                   clear                   O54astfa
003500970516     C* ERRORE SE NON PASSATA LA LINEA PER CUI DETERMINARE IL TERMINAL
003600030226     C     I54LIN        IFEQ      *ZEROS
003700030226     C                   MOVEL     '1'           O54ERR
003800030226     C                   MOVEL     MSG(1)        O54MSG
003900970516     C                   GOTO      FINE
004000970516     C                   END
004100970702     C* ERRORE SE NON PASSATA LA DATA DI RIFERIMENTO
004200030226     C     I54DRF        IFEQ      *ZEROS
004300030226     C                   MOVEL     '2'           O54ERR
004400030226     C                   MOVEL     MSG(2)        O54MSG
004500970702     C                   GOTO      FINE
004600970702     C                   END
004700970514     C*
004800970514     C**************************************
004900970514     C* REPERIMENTO DEL TERMINAL DI PARTENZA
005000970514     C**************************************
005100030226    2C     I54TPT        IFEQ      'P'
005200030226     C     I54TPT        OREQ      ' '
005300970702     C                   MOVEL     'P'           KEPA
005400030226     C                   Z-ADD     I54LIN        KTFP
005500970702     C                   MOVE      *ZEROS        KTFA
005600970702     C                   EXSR      RICTER
005700970702     C     WTER          IFEQ      *ZEROS
005800030226     C                   Z-ADD     I54LIN        O54TFP
005900030226     C                   MOVEL     '3'           O54ERR
006000030226     C                   MOVEL     MSG(3)        O54MSG
006100970702     C                   ELSE
006200030226     C                   Z-ADD     WTER          O54TFP
006300970702     C                   END
006400970514    2C                   END
006500970514     C*
006600970514     C**************************************
006700970514     C* REPERIMENTO DEL TERMINAL DI ARRIVO
006800970514     C**************************************
006900030226    2C     I54TPT        IFEQ      'A'
007000030226     C     I54TPT        OREQ      *BLANKS
007100970514     C*
007200970702     C                   CLEAR                   WTER
007300970702     C* CERCO IL TERMINAL SPECIFICO SOLO SE MI HANNO PASSATO LA LNP
007400970702     C* ALTRIMENTI CERCO QUELLO DI DEFAULT
007500030226    3C     I54LNP        IFGT      *ZEROS
007600000711     C**
007700040701     C***                CLEAR                   OG143
007800040701     C***  I54LNP        CHAIN     AZORG01L                           31
007900040701    5C**N31ORGFVA        IFNE      ' '
008000040701     C***  ORGFAG        ORNE      'A'
008100040701     C***  ORGFAG        ANDNE     'F'
008200040701     C***                SETON                                        31
008300040701    5C***                ENDIF
008400040701     C**N31              MOVEL     ORGDE3        OG143
008500040701     C***31              CLEAR                   OG143
008600000711     C**
008700030226     C* Per prima cosa determino il terminal di partenza di I54lnp
008800000606     C* Solo se non è p.o. poste
008900040701    4C***  §OGntw        IFNE      'PPT'
009000970702     C                   MOVEL     'P'           KEPA
009100030226     C                   Z-ADD     I54LNP        KTFP
009200970702     C                   MOVE      *ZEROS        KTFA
009300970702     C                   EXSR      RICTER
009400040701    4C***                ENDIF
009500970702     C*
009600970702     C                   MOVEL     'A'           KEPA
009700970702    4C     WTER          IFEQ      *ZEROS
009800030226     C                   Z-ADD     I54LNP        KTFP
009900970702   X4C                   ELSE
010000970702     C                   Z-ADD     WTER          KTFP
010100970702    4C                   END
010200030226     C                   Z-ADD     I54LIN        KTFA
010300970702     C*
010400970702     C                   EXSR      RICTER
010500970702     C*
010600970702    3C                   END
010700970702     C* TROVATA ECCEZIONE TER-ARR
010800970702    3C     WTER          IFGT      *ZEROS
010900030226     C                   Z-ADD     WTER          O54TFA
011000030226     C                   MOVEL     'E'           O54ORR
011100970702   X3C                   ELSE
011200970702     C* SE NON HO TROVATO IL TERMINAL DI ARRIVO O SE NON MI E' STATA
011300970702     C* PASSATA LA LINEA DI PARTENZA PER CUI CONTROLLARE LE ECCEZIONI
011400970702     C* CERCO IL TERMINAL DI ARRIVO DI DEFAULT
011500970702     C                   MOVEL     'A'           KEPA
011600970702     C                   MOVE      *ZEROS        KTFP
011700030226     C                   Z-ADD     I54LIN        KTFA
011800970702     C                   EXSR      RICTER
011900970702    4C     WTER          IFEQ      *ZEROS
012000030226     C                   Z-ADD     I54LIN        O54TFA
012100990517     C                   SELECT
012200030226     C     O54ERR        WHENEQ    *BLANKS
012300030226     C                   MOVEL     '4'           O54ERR
012400030226     C                   MOVEL     MSG(4)        O54MSG
012500030226     C     O54ERR        WHENEQ    '3'
012600030226     C                   MOVEL     '5'           O54ERR
012700030226     C                   MOVEL     MSG(5)        O54MSG
012800990517     C                   ENDSL
012900970702   X4C                   ELSE
013000030226     C                   Z-ADD     WTER          O54TFA
013100970702    4C                   END
013200970702    3C                   END
013300970514    2C                   END
013400020529     C**************************************
013500020529     C* REPERIMENTO DEL P.O. GESTIONE DA £6
013600020529     C**************************************
013700030226    2C     I54TPT        IFEQ      '6'
013800020529     C                   MOVEL     '6'           KEPA
013900020529     C                   MOVE      *ZEROS        KTFP
014000030226     C                   Z-ADD     I54LIN        KTFA
014100020529     C                   EXSR      RICTER
014200020529    3C     WTER          IFEQ      *ZEROS
014300030226     C                   Z-ADD     I54LIN        O54TFA
014400030226    4C     O54ERR        IFEQ      *BLANKS
014500030226     C                   MOVEL     '6'           O54ERR
014600030226     C                   MOVEL     MSG(6)        O54MSG
014700020529    4C                   ENDIF
014800020529   X3C                   ELSE
014900030226     C                   Z-ADD     WTER          O54TFA
015000020529    3C                   END
015100020529    2C                   ENDIF
015200030224
015300030224      * Cerco OGAS
015400030226     c                   If        o54err = *Blanks
015500030224
015600030226     c                   If        i54lnp > *Zeros
015700030226     c                   Eval      kazorg = i54lnp
015800030224     c                   Exsr      sr_azorg
015900030226     c                   Eval      o54aslnp = §ogas
016000030224     c                   EndIf
016100030224
016200030226     c                   Eval      kazorg = i54lin
016300030224     c                   Exsr      sr_azorg
016400030226     c                   Eval      o54aslin = §ogas
016500030224
016600030226     c                   If        o54tfp > *Zeros
016700030226     c                   Eval      kazorg = o54tfp
016800030224     c                   Exsr      sr_azorg
016900030226     c                   Eval      o54astfp = §ogas
017000030224     c                   EndIf
017100030224
017200030226     c                   If        o54tfa > *Zeros
017300030226     c                   Eval      kazorg = o54tfa
017400030224     c                   Exsr      sr_azorg
017500030226     c                   Eval      o54astfa = §ogas
017600030224     c                   EndIf
017700030224
017800030224     c                   EndIf
017900030224
018000970514     C*
018100970514     C     FINE          TAG
018200970424     C*
018300970424    1C                   ENDIF
018400970424     C* CHIUSURA PGM
018500030226    1C     I54TLA        IFEQ      ' '
018600970424     C                   RETURN
018700970424     C                   ELSE
018800970424     C                   SETON                                        LR
018900970424    1C                   ENDIF
019000970702     C**************************************************************************
019100970702     C* REPERIMENTO TERMINAL
019200970702     C**************************************************************************
019300970702     C     RICTER        BEGSR
019400970702     C*
019500970702     C                   CLEAR                   WTER
019600970702     C*
019700970702     C     KCAE1         SETLL     AZCAE01L                               33
019800970702     C   33KCAE1         READE     AZCAE01L                               34
019900970702     C  N33KCAE1A        READPE    AZCAE01L                               34
020000970702    3C     *IN34         DOWEQ     *OFF
020100970702     C     CAEATB        ANDNE     *BLANKS
020200970702     C     KCAE1A        READPE    AZCAE01L                               34
020300970702    3C                   ENDDO
020400970702    3C     *IN34         IFEQ      *OFF
020500030226     C     CAEDSC        ANDLT     I54DRF
020600970702     C                   SETON                                        34
020700970702    3C                   END
020800970702     C* 34 OFF --> TROVATO IL TERMINAL
020900970702     C     *IN34         IFEQ      *OFF
021000970702     C                   MOVE      CAETFE        WTER
021100970702     C                   END
021200970702     C                   ENDSR
021300030224      *------------------------------------------------------------------------*
021400030224      * Chain Azorg
021500030224      *------------------------------------------------------------------------*
021600030224     c     sr_azorg      begsr
021700030224
021800030224     c                   Clear                   og143
021900030224     c     kazorg        Chain     Azorg01l
022000030224     c                   If        %Found(azorg01l) and Orgfva = *Blanks
022100030224     c                             and (orgfag = 'A' or orgfag = 'F')
022200030224     c                   Movel     orgde3        og143
022300030224     c                   endif
022400030224
022500030224     c                   endsr
022600000000     C**************************************************************************
022700000000     C* R O U T I N E    I N I Z I A L E
022800000000     C**************************************************************************
022900000000     C     *INZSR        BEGSR
023000950130     C*
023100970514     C     KCAE1         KLIST
023200970514     C                   KFLD                    KEPA
023300970514     C                   KFLD                    KTFP
023400970702     C                   KFLD                    KTFA
023500030226     C                   KFLD                    I54DRF
023600970514     C     KCAE1A        KLIST
023700970514     C                   KFLD                    KEPA
023800970514     C                   KFLD                    KTFP
023900970702     C                   KFLD                    KTFA
024000970430     C*
024100970430     C* DEFINIZIONE DI ALCUNI CAMPI
024200970514     C     *LIKE         DEFINE    CAEEPA        KEPA
024300970514     C     *LIKE         DEFINE    CAETFP        KTFP
024400970702     C     *LIKE         DEFINE    CAETFP        WTER
024500970702     C     *LIKE         DEFINE    CAETFA        KTFA
024600970430     C*
024700000000     C                   ENDSR
024800000711**
024900970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA LINEA PER CUI CALCOLARLO
025000970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA DATA DI RIFERIMENTO
025100030226NON TROVATO TERMINAL DI PARTENZA DI I54LIN:ASSUNTO I54LIN
025200030226NON TROVATO TERMINAL DI ARRIVO   DI I54LIN:ASSUNTO I54LIN
025300030226NON TROVATI TERMINAL DI PARTENZA E DI ARRIVO: ASSUNTO I54LIN
025400030226NON TROVATO P.O. GESTIONE DI I54LIN:ASSUNTO I54LIN

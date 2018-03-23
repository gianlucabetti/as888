000100950130     H DECEDIT('0,') DATEDIT(*DMY.)
000200030224     F* FNLV54R *-----------------------------------------------------*
000300970514     F*       REPERIMENTO TERMINAL DI ARRIVO E/O DI PARTENZA
000400020529     F*       O REPERIMENTO P.O. GESTIONE DA £6
000500000711     F*         SE NON TROVO AZORG00F  TESTO FISSO 180 PER LE POSTE
000600000711     F*         ATTENZIONE SE SOI CREA UNA NUOVA FILIALE POSTE
000700950130     F*---------------------------------------------------------------*
000800970514     FAZCAE01L  IF   E           K DISK
000900030224     FAZORG01L  IF   E           K DISK
001000030224
001100020529     D MSG             S             78    DIM(6) CTDATA PERRCD(1)
001200030224
001300030224     d kazorg          s                   Like(orgfil)
001400030224
001500030224      * DS PER PASSAGGIO PARAMETRI
001600030224     D FNLV54DS      E DS
001700000606     D OG143         E DS
001800970424     C**************************************************************************
001900970424     C     *ENTRY        PLIST
002000030224     C                   PARM                    fnlv54ds
002100970424     C*
002200030226     C* TIPO LANCIO, I54TLA "C" -           CHIUSO CON LR
002300030226     C* TIPO LANCIO, I54TLA "L" - ELABORO E CHIUDO CON LR
002400030226     C* TIPO LANCIO, I54TLA " " - ELABORO E CHIUDO IN RETRN
002500030226    1C     I54TLA        IFNE      'C'
002600970514     C* Pulizia dei campi di output
002700030226     C                   CLEAR                   O54ERR
002800030226     C                   CLEAR                   O54MSG
002900030226     C                   CLEAR                   O54TFP
003000030226     C                   CLEAR                   O54TFA
003100030226     C                   CLEAR                   O54ORP
003200030226     C                   CLEAR                   O54ORR
003300030226     c                   clear                   O54aslnp
003400030226     c                   clear                   O54aslin
003500030226     c                   clear                   O54astfp
003600030226     c                   clear                   O54astfa
003700970516     C* ERRORE SE NON PASSATA LA LINEA PER CUI DETERMINARE IL TERMINAL
003800030226     C     I54LIN        IFEQ      *ZEROS
003900030226     C                   MOVEL     '1'           O54ERR
004000030226     C                   MOVEL     MSG(1)        O54MSG
004100970516     C                   GOTO      FINE
004200970516     C                   END
004300970702     C* ERRORE SE NON PASSATA LA DATA DI RIFERIMENTO
004400030226     C     I54DRF        IFEQ      *ZEROS
004500030226     C                   MOVEL     '2'           O54ERR
004600030226     C                   MOVEL     MSG(2)        O54MSG
004700970702     C                   GOTO      FINE
004800970702     C                   END
004900970514     C*
005000970514     C**************************************
005100970514     C* REPERIMENTO DEL TERMINAL DI PARTENZA
005200970514     C**************************************
005300030226    2C     I54TPT        IFEQ      'P'
005400030226     C     I54TPT        OREQ      ' '
005500970702     C                   MOVEL     'P'           KEPA
005600030226     C                   Z-ADD     I54LIN        KTFP
005700970702     C                   MOVE      *ZEROS        KTFA
005800970702     C                   EXSR      RICTER
005900970702     C     WTER          IFEQ      *ZEROS
006000030226     C                   Z-ADD     I54LIN        O54TFP
006100030226     C                   MOVEL     '3'           O54ERR
006200030226     C                   MOVEL     MSG(3)        O54MSG
006300970702     C                   ELSE
006400030226     C                   Z-ADD     WTER          O54TFP
006500970702     C                   END
006600970514    2C                   END
006700970514     C*
006800970514     C**************************************
006900970514     C* REPERIMENTO DEL TERMINAL DI ARRIVO
007000970514     C**************************************
007100030226    2C     I54TPT        IFEQ      'A'
007200030226     C     I54TPT        OREQ      *BLANKS
007300970514     C*
007400970702     C                   CLEAR                   WTER
007500970702     C* CERCO IL TERMINAL SPECIFICO SOLO SE MI HANNO PASSATO LA LNP
007600970702     C* ALTRIMENTI CERCO QUELLO DI DEFAULT
007700030226    3C     I54LNP        IFGT      *ZEROS
007800000711     C**
007900000606     C                   CLEAR                   OG143
008000030226     C     I54LNP        CHAIN     AZORG01L                           31
008100000711    5C  N31ORGFVA        IFNE      ' '
008200000606     C     ORGFAG        ORNE      'A'
008300000606     C     ORGFAG        ANDNE     'F'
008400000606     C                   SETON                                        31
008500000711    5C                   ENDIF
008600000606     C  N31              MOVEL     ORGDE3        OG143
008700000711     C   31              CLEAR                   OG143
008800000711     C**
008900030226     C* Per prima cosa determino il terminal di partenza di I54lnp
009000000606     C* Solo se non è p.o. poste
009100030224    4C     §OGntw        IFNE      'PPT'
009200970702     C                   MOVEL     'P'           KEPA
009300030226     C                   Z-ADD     I54LNP        KTFP
009400970702     C                   MOVE      *ZEROS        KTFA
009500970702     C                   EXSR      RICTER
009600000606    4C                   ENDIF
009700970702     C*
009800970702     C                   MOVEL     'A'           KEPA
009900970702    4C     WTER          IFEQ      *ZEROS
010000030226     C                   Z-ADD     I54LNP        KTFP
010100970702   X4C                   ELSE
010200970702     C                   Z-ADD     WTER          KTFP
010300970702    4C                   END
010400030226     C                   Z-ADD     I54LIN        KTFA
010500970702     C*
010600970702     C                   EXSR      RICTER
010700970702     C*
010800970702    3C                   END
010900970702     C* TROVATA ECCEZIONE TER-ARR
011000970702    3C     WTER          IFGT      *ZEROS
011100030226     C                   Z-ADD     WTER          O54TFA
011200030226     C                   MOVEL     'E'           O54ORR
011300970702   X3C                   ELSE
011400970702     C* SE NON HO TROVATO IL TERMINAL DI ARRIVO O SE NON MI E' STATA
011500970702     C* PASSATA LA LINEA DI PARTENZA PER CUI CONTROLLARE LE ECCEZIONI
011600970702     C* CERCO IL TERMINAL DI ARRIVO DI DEFAULT
011700970702     C                   MOVEL     'A'           KEPA
011800970702     C                   MOVE      *ZEROS        KTFP
011900030226     C                   Z-ADD     I54LIN        KTFA
012000970702     C                   EXSR      RICTER
012100970702    4C     WTER          IFEQ      *ZEROS
012200030226     C                   Z-ADD     I54LIN        O54TFA
012300990517     C                   SELECT
012400030226     C     O54ERR        WHENEQ    *BLANKS
012500030226     C                   MOVEL     '4'           O54ERR
012600030226     C                   MOVEL     MSG(4)        O54MSG
012700030226     C     O54ERR        WHENEQ    '3'
012800030226     C                   MOVEL     '5'           O54ERR
012900030226     C                   MOVEL     MSG(5)        O54MSG
013000990517     C                   ENDSL
013100970702   X4C                   ELSE
013200030226     C                   Z-ADD     WTER          O54TFA
013300970702    4C                   END
013400970702    3C                   END
013500970514    2C                   END
013600020529     C**************************************
013700020529     C* REPERIMENTO DEL P.O. GESTIONE DA £6
013800020529     C**************************************
013900030226    2C     I54TPT        IFEQ      '6'
014000020529     C                   MOVEL     '6'           KEPA
014100020529     C                   MOVE      *ZEROS        KTFP
014200030226     C                   Z-ADD     I54LIN        KTFA
014300020529     C                   EXSR      RICTER
014400020529    3C     WTER          IFEQ      *ZEROS
014500030226     C                   Z-ADD     I54LIN        O54TFA
014600030226    4C     O54ERR        IFEQ      *BLANKS
014700030226     C                   MOVEL     '6'           O54ERR
014800030226     C                   MOVEL     MSG(6)        O54MSG
014900020529    4C                   ENDIF
015000020529   X3C                   ELSE
015100030226     C                   Z-ADD     WTER          O54TFA
015200020529    3C                   END
015300020529    2C                   ENDIF
015400030224
015500030224      * Cerco OGAS
015600030226     c                   If        o54err = *Blanks
015700030224
015800030226     c                   If        i54lnp > *Zeros
015900030226     c                   Eval      kazorg = i54lnp
016000030224     c                   Exsr      sr_azorg
016100030226     c                   Eval      o54aslnp = §ogas
016200030224     c                   EndIf
016300030224
016400030226     c                   Eval      kazorg = i54lin
016500030224     c                   Exsr      sr_azorg
016600030226     c                   Eval      o54aslin = §ogas
016700030224
016800030226     c                   If        o54tfp > *Zeros
016900030226     c                   Eval      kazorg = o54tfp
017000030224     c                   Exsr      sr_azorg
017100030226     c                   Eval      o54astfp = §ogas
017200030224     c                   EndIf
017300030224
017400030226     c                   If        o54tfa > *Zeros
017500030226     c                   Eval      kazorg = o54tfa
017600030224     c                   Exsr      sr_azorg
017700030226     c                   Eval      o54astfa = §ogas
017800030224     c                   EndIf
017900030224
018000030224     c                   EndIf
018100030224
018200970514     C*
018300970514     C     FINE          TAG
018400970424     C*
018500970424    1C                   ENDIF
018600970424     C* CHIUSURA PGM
018700030226    1C     I54TLA        IFEQ      ' '
018800970424     C                   RETURN
018900970424     C                   ELSE
019000970424     C                   SETON                                        LR
019100970424    1C                   ENDIF
019200970702     C**************************************************************************
019300970702     C* REPERIMENTO TERMINAL
019400970702     C**************************************************************************
019500970702     C     RICTER        BEGSR
019600970702     C*
019700970702     C                   CLEAR                   WTER
019800970702     C*
019900970702     C     KCAE1         SETLL     AZCAE01L                               33
020000970702     C   33KCAE1         READE     AZCAE01L                               34
020100970702     C  N33KCAE1A        READPE    AZCAE01L                               34
020200970702    3C     *IN34         DOWEQ     *OFF
020300970702     C     CAEATB        ANDNE     *BLANKS
020400970702     C     KCAE1A        READPE    AZCAE01L                               34
020500970702    3C                   ENDDO
020600970702    3C     *IN34         IFEQ      *OFF
020700030226     C     CAEDSC        ANDLT     I54DRF
020800970702     C                   SETON                                        34
020900970702    3C                   END
021000970702     C* 34 OFF --> TROVATO IL TERMINAL
021100970702     C     *IN34         IFEQ      *OFF
021200970702     C                   MOVE      CAETFE        WTER
021300970702     C                   END
021400970702     C                   ENDSR
021500030224      *------------------------------------------------------------------------*
021600030224      * Chain Azorg
021700030224      *------------------------------------------------------------------------*
021800030224     c     sr_azorg      begsr
021900030224
022000030224     c                   Clear                   og143
022100030224     c     kazorg        Chain     Azorg01l
022200030224     c                   If        %Found(azorg01l) and Orgfva = *Blanks
022300030224     c                             and (orgfag = 'A' or orgfag = 'F')
022400030224     c                   Movel     orgde3        og143
022500030224     c                   endif
022600030224
022700030224     c                   endsr
022800000000     C**************************************************************************
022900000000     C* R O U T I N E    I N I Z I A L E
023000000000     C**************************************************************************
023100000000     C     *INZSR        BEGSR
023200950130     C*
023300970514     C     KCAE1         KLIST
023400970514     C                   KFLD                    KEPA
023500970514     C                   KFLD                    KTFP
023600970702     C                   KFLD                    KTFA
023700030226     C                   KFLD                    I54DRF
023800970514     C     KCAE1A        KLIST
023900970514     C                   KFLD                    KEPA
024000970514     C                   KFLD                    KTFP
024100970702     C                   KFLD                    KTFA
024200970430     C*
024300970430     C* DEFINIZIONE DI ALCUNI CAMPI
024400970514     C     *LIKE         DEFINE    CAEEPA        KEPA
024500970514     C     *LIKE         DEFINE    CAETFP        KTFP
024600970702     C     *LIKE         DEFINE    CAETFP        WTER
024700970702     C     *LIKE         DEFINE    CAETFA        KTFA
024800970430     C*
024900000000     C                   ENDSR
025000000711**
025100970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA LINEA PER CUI CALCOLARLO
025200970702IMPOSSIBILE DETERMINARE IL TERMINAL: MANCA LA DATA DI RIFERIMENTO
025300030226NON TROVATO TERMINAL DI PARTENZA DI I54LIN:ASSUNTO I54LIN
025400030226NON TROVATO TERMINAL DI ARRIVO   DI I54LIN:ASSUNTO I54LIN
025500030226NON TROVATI TERMINAL DI PARTENZA E DI ARRIVO: ASSUNTO I54LIN
025600030226NON TROVATO P.O. GESTIONE DI I54LIN:ASSUNTO I54LIN

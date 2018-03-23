000100020716     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* AUTOMATISMO DUPLICA TRAINI - RICHIESTA PARAMETRI
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL55D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL55DVM:MSGULT)
001000000000     F*---TNTLT01LIF  E           K        DISK
001100000000     D*------------------------------------------------------------------------*
001200000000     D* SCHIERE
001300000000     D*------------------------------------------------------------------------*
001400000000     D*-------------------
001500000000     D* MESSAGGI, COMANDI E FUNZIONI
001600000000     D*-------------------
001700000000     D MSG             S             78    DIM(25) CTDATA PERRCD(1)             *MESSAGGI
001800000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
001900000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002000000000     D*-------------------
002100000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002200000000     D*-------------------
002300000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002400000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002500000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002600000000     D*-------------------
002700000000     D* TABELLA DESCRIZIONI A VIDEO
002800000000     D*-------------------
002900000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003000000000     D*-------------------
003100000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003200000000     D*-------------------
003300000000     D KEYS            S              1    DIM(40)
003400000000     D*------------------------------------------------------------------------*
003500000000     D* INPUT
003600000000     D*------------------------------------------------------------------------*
003700000000     D*---------------------
003800000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003900000000     D*---------------------
004000000000     D DSFUN         E DS
004100000000     D  FUNRIG                 1     40
004200000000     D*---------------------
004300000000     D* DS PER GESTIONE TASTI FUNZIONALI
004400000000     D*---------------------
004500000000     D DSKEYS        E DS
004600000000     D  KEYRIG                 1     40
004700000000     D*---------------------
004800000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004900000000     D*---------------------
005000000000     D                SDS
005100000000     D  USER                 254    263
005200000000     D*---------------------
005300000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005400000000     D*---------------------
005500000000     D DEVDS1          DS
005600000000     D  KEY                  369    369
005700000000     D  CURSOR               370    371
005800000000     D*---------------------
005900000000     D* ARCHITETTURA
006000000000     D*---------------------
006100000000     D KPJBA         E DS
006200000000     D*---------------------
006300000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
006400000000     D*---------------------
006500000000     D DSMSG         E DS
006600020716
006700020716     d DdatiUte      e ds
006800020716     d AzuteDs       e ds                  ExtName(Azute00f)
006900020716     d Tibs34Ds      e ds                  Inz
007000020716
007100000000     D*---------------------
007200000000     D* DS PARAMETRI DI LANCIO
007300000000     D*------------------
007400000000     D PARAM           DS                  INZ
007500000000     D  PARTR1                 1      7  0
007600000000     D  PARTR2                 8     14  0
007700000000     D  PARDDE                15     22  0
007800000000     D  PARDSC                23     30  0
007900000000     D  PARNUM                31     37  0
008000000000     D  PARDD2                38     45  0
008100000000     D  PARDS2                46     53  0
008200000000     D  PARESI                54     54
008300000000     D  PARSIM                55     55
008400000000     D  PARMAN                56     56
008500980605     D  PARTE1                57     63  0
008600980605     D  PARTE2                64     70  0
008700980605     D  PARTE3                71     77  0
008800980605     D  PARTE4                78     84  0
008900980605     D  PARTE5                85     91  0
009000980605     D  PARTE6                92     98  0
009100980605     D  PARTE7                99    105  0
009200980605     D  PARTE8               106    112  0
009300980605     D  PARTE9               113    119  0
009400980605     D  PARTE0               120    126  0
009500980605     D  PARTEA               127    133  0
009600980605     D  PARTEB               134    140  0
009700000000     D*---------------------
009800000000     D* DS CONTROLLO DATA (8)
009900000000     D*------------------
010000000000     D WLBDA8          DS                  INZ
010100000000     D  G08DAT                 1      8  0
010200000000     D  G08INV                 9     16  0
010300000000     D  G08ERR                17     17
010400000000     D  G08TGI                18     22  0
010500000000     C*------------------------------------------------------------------------*
010600000000     C* MAIN LINE
010700000000     C*------------------------------------------------------------------------*
010800000000     C*
010900000000     C* CICLO FINO A CHE:
011000000000DO  1C     $FINE         DOUEQ     '1'
011100000000     C*
011200000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
011300000000     C*
011400000000     C* CONTROLLA GESTIONE VIDEO
011500000000IF  2C     $GES          IFEQ      'V1'
011600000000     C                   EXSR      GESV1                                        *PARAMETRI
011700000000E   2C                   ENDIF
011800000000E   1C                   ENDDO
011900000000     C*
012000000000     C                   SETON                                        LR
012100000000     C*------------------------------------------------------------------------*
012200000000     C* GESV1 - GESTIONE VIDEO UNO
012300000000     C*------------------------------------------------------------------------*
012400000000     C     GESV1         BEGSR
012500000000     C*
012600000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
012700000000     C                   EXSR      CARV1                                        *CARICA VIDEO
012800000000E   1C                   ENDIF
012900000000     C*
013000000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
013100000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
013200000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
013300000000E   1C                   ENDIF
013400000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
013500000000     C*
013600000000     C                   EXFMT     TL55DV1                                      *PARAMETRI
013700000000     C*
013800000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013900020717
014000020717      * Se Profilo non OK devo uscire dal pgm
014100020717     c                   If        *In76
014200020717     c                   Eval      $err = '1'
014300020717     c                   Eval      $fine = '1'
014400020717     c                   EndIf
014500000000     C*
014600000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
014700000000SL  2C                   SELECT
014800000000     C     KEY           WHENEQ    ENTER
014900000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
015000000000     C     KEY           WHENEQ    F6
015100000000     C                   EXSR      F06GES                                       *CONFERMA
015200000000     C     KEY           WHENEQ    F22
015300000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
015400000000     C                   OTHER
015500000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
015600000000E   2C                   ENDSL
015700000000E   1C                   ENDIF
015800000000     C*
015900000000     C                   ENDSR
016000000000     C*------------------------------------------------------------------------*
016100000000     C* CARV1 - CARICA VIDEO UNO
016200000000     C*------------------------------------------------------------------------*
016300000000     C     CARV1         BEGSR
016400000000     C*
016500000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016600000000     C*
016700000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016800000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016900000000     C*
017000000000     C                   WRITE     TL55DVT                                      *TESTATA
017100000000     C                   WRITE     TL55DVF                                      *FUNZIONI
017200000000     C*
017300000000     C                   ENDSR
017400000000     C*------------------------------------------------------------------------*
017500000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
017600000000     C*------------------------------------------------------------------------*
017700000000     C     ENTV1         BEGSR
017800000000     C*
017900000000     C* CONTROLLA I CAMPI IMMESSI
018000000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
018100000000     C*
018200000000     C                   ENDSR
018300000000     C*------------------------------------------------------------------------*
018400000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
018500000000     C*------------------------------------------------------------------------*
018600000000     C     F06GES        BEGSR
018700000000     C*
018800000000     C* CONTROLLA I CAMPI IMMESSI
018900000000     C                   EXSR      DECODI
019000000000     C*
019100000000     C* LANCIA IL PROGRAMMA DI DUPLICA
019200000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
019300000000     C                   Z-ADD     V1CTR1        PARTR1                         *DA NUMERO TRAINO
019400000000     C                   Z-ADD     V1CTR2        PARTR2                         * A NUMERO TRAINO
019500000000     C                   Z-ADD     DDEV1C        PARDDE                         *CON DATA DECORRENZA
019600000000     C                   Z-ADD     DSCV1C        PARDSC                         *CON DATA SCADENZA
019700000000     C                   Z-ADD     V1CNUM        PARNUM                         *A PARTIRE DAL NUMER
019800000000     C                   Z-ADD     DD2V1C        PARDD2                         *NEW DATA DECORRENZA
019900000000     C                   Z-ADD     DS2V1C        PARDS2                         *NEW DATA SCADENZA
020000000000     C                   MOVEL     V1CESI        PARESI                         *FLAG SOVRASCRITTURA
020100000000     C                   MOVEL     V1CSIM        PARSIM                         *FLAG SIMULAZIONE
020200000000     C                   MOVEL     V1CMAN        PARMAN                         *FLAG MANTENIMENTO
020300980605     C                   Z-ADD     V1CTE1        PARTE1                         *TRAINO ESCLUSO
020400980605     C                   Z-ADD     V1CTE2        PARTE2                         *TRAINO ESCLUSO
020500980605     C                   Z-ADD     V1CTE3        PARTE3                         *TRAINO ESCLUSO
020600980605     C                   Z-ADD     V1CTE4        PARTE4                         *TRAINO ESCLUSO
020700980605     C                   Z-ADD     V1CTE5        PARTE5                         *TRAINO ESCLUSO
020800980605     C                   Z-ADD     V1CTE6        PARTE6                         *TRAINO ESCLUSO
020900980605     C                   Z-ADD     V1CTE7        PARTE7                         *TRAINO ESCLUSO
021000980605     C                   Z-ADD     V1CTE8        PARTE8                         *TRAINO ESCLUSO
021100980605     C                   Z-ADD     V1CTE9        PARTE9                         *TRAINO ESCLUSO
021200980605     C                   Z-ADD     V1CTE0        PARTE0                         *TRAINO ESCLUSO
021300980605     C                   Z-ADD     V1CTEA        PARTEA                         *TRAINO ESCLUSO
021400980605     C                   Z-ADD     V1CTEB        PARTEB                         *TRAINO ESCLUSO
021500000000     C*
021600000000     C                   MOVEL     'TL56'        KCOAZ
021700000000     C                   MOVEL     PARAM         KPJBU
021800000000     C                   CALL      'BCH10'
021900000000     C                   PARM                    KPJBA
022000000000     C*
022100000000     C                   MOVEL     MSG(25)       $MSG                           ELABORAZIONE LANCIAT
022200000000     C                   EXSR      CARMSG
022300000000E   1C                   ENDIF
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
022800000000     C*------------------------------------------------------------------------*
022900000000     C     F22GES        BEGSR
023000000000     C*
023100000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
023200000000     C                   Z-ADD     80            LENGH            15 5
023300000000     C                   CALL      'QCAEXEC'
023400000000     C                   PARM                    CMD(1)
023500000000     C                   PARM                    LENGH
023600000000     C*
023700000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
023800000000     C*
023900000000     C                   ENDSR
024000000000     C*------------------------------------------------------------------------*
024100000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
024200000000     C*------------------------------------------------------------------------*
024300000000     C     DECODI        BEGSR
024400000000     C*
024500000000     C* REIMPOSTA INDICATORI DI ERRORE
024600000000     C                   SETOFF                                       606162
024700000000     C                   SETOFF                                       636465
024800000000     C                   SETOFF                                       666768
024900000000     C                   SETOFF                                       69
025000000000     C*--------------------
025100000000     C* SIMULAZIONE LANCIO
025200000000     C*--------------------
025300000000IF  1C     $ERR          IFEQ      '0'
025400000000IF  2C     V1CSIM        IFEQ      'S'
025500000000     C     V1CSIM        OREQ      'N'
025600000000X   2C                   ELSE
025700000000     C                   SETON                                        68
025800000000     C                   MOVEL     '1'           $ERR
025900000000     C                   MOVEL     MSG(19)       $MSG
026000000000     C                   EXSR      CARMSG
026100000000E   2C                   ENDIF
026200000000E   1C                   ENDIF
026300000000     C*--------------------
026400000000     C* FLAG SOVRASCRITTURA
026500000000     C*--------------------
026600000000IF  1C     $ERR          IFEQ      '0'
026700000000IF  2C     V1CESI        IFEQ      'S'
026800000000     C     V1CESI        OREQ      'N'
026900000000X   2C                   ELSE
027000000000     C                   SETON                                        67
027100000000     C                   MOVEL     '1'           $ERR
027200000000     C                   MOVEL     MSG(18)       $MSG
027300000000     C                   EXSR      CARMSG
027400000000E   2C                   ENDIF
027500000000E   1C                   ENDIF
027600000000     C***
027700000000     C* TRAINI DA CUI COPIARE
027800000000     C***
027900000000     C*--------------------
028000000000     C* DA NUMERO TRAINO
028100000000     C*--------------------
028200000000IF  1C     $ERR          IFEQ      '0'
028300000000IF  2C     V1CTR1        IFEQ      *ZEROS
028400000000     C                   SETON                                        60
028500000000     C                   MOVEL     '1'           $ERR
028600000000     C                   MOVEL     MSG(7)        $MSG
028700000000     C                   EXSR      CARMSG
028800000000E   2C                   ENDIF
028900000000E   1C                   ENDIF
029000000000     C*--------------------
029100000000     C* A NUMERO TRAINO
029200000000     C*--------------------
029300000000IF  1C     $ERR          IFEQ      '0'
029400000000IF  2C     V1CTR2        IFEQ      *ZEROS
029500000000     C                   SETON                                        61
029600000000     C                   MOVEL     '1'           $ERR
029700000000     C                   MOVEL     MSG(7)        $MSG
029800000000     C                   EXSR      CARMSG
029900000000E   2C                   ENDIF
030000000000E   1C                   ENDIF
030100000000     C* TRAINO A >= TRAINO DA
030200000000IF  1C     $ERR          IFEQ      '0'
030300000000IF  2C     V1CTR2        IFLT      V1CTR1
030400000000     C                   SETON                                        61
030500000000     C                   MOVEL     '1'           $ERR
030600000000     C                   MOVEL     MSG(8)        $MSG
030700000000     C                   EXSR      CARMSG
030800000000E   2C                   ENDIF
030900000000E   1C                   ENDIF
031000000000     C*--------------------
031100000000     C* DATA DECORRENZA
031200000000     C*--------------------
031300000000     C                   Z-ADD     *ZEROS        DDEV1C
031400000000IF  1C     $ERR          IFEQ      '0'
031500000000IF  2C     V1CDDE        IFGT      *ZEROS
031600000000     C                   Z-ADD     V1CDDE        WDA8
031700000000     C                   Z-ADD     *ZEROS        DA8W
031800000000     C                   EXSR      CHKDAT
031900000000     C                   Z-ADD     WDA8          V1CDDE
032000000000     C                   Z-ADD     DA8W          DDEV1C
032100000000IF  3C     $ERR          IFEQ      '1'
032200000000     C                   SETON                                        62
032300000000     C                   MOVEL     MSG(9)        $MSG
032400000000     C                   EXSR      CARMSG
032500000000E   3C                   ENDIF
032600000000E   2C                   ENDIF
032700000000E   1C                   ENDIF
032800000000     C*--------------------
032900000000     C* DATA SCADENZA
033000000000     C*--------------------
033100000000     C                   Z-ADD     *ZEROS        DSCV1C
033200000000IF  1C     $ERR          IFEQ      '0'
033300000000IF  2C     V1CDSC        IFGT      *ZEROS
033400000000     C                   Z-ADD     V1CDSC        WDA8
033500000000     C                   Z-ADD     *ZEROS        DA8W
033600000000     C                   EXSR      CHKDAT
033700000000     C                   Z-ADD     WDA8          V1CDSC
033800000000     C                   Z-ADD     DA8W          DSCV1C
033900000000IF  3C     $ERR          IFEQ      '1'
034000000000     C                   SETON                                        63
034100000000     C                   MOVEL     MSG(9)        $MSG
034200000000     C                   EXSR      CARMSG
034300000000E   3C                   ENDIF
034400000000E   2C                   ENDIF
034500000000E   1C                   ENDIF
034600000000     C* DECORRENZA O SCADENZA DA INSERIRE
034700000000IF  1C     $ERR          IFEQ      '0'
034800000000IF  2C     V1CDDE        IFEQ      *ZEROS
034900000000     C     V1CDSC        ANDEQ     *ZEROS
035000000000     C                   SETON                                        6263
035100000000     C                   MOVEL     '1'           $ERR
035200000000     C                   MOVEL     MSG(10)       $MSG
035300000000     C                   EXSR      CARMSG
035400000000E   2C                   ENDIF
035500000000E   1C                   ENDIF
035600000000     C*--------------------
035700000000     C* TRAINO  DA  ESISTENTE
035800000000     C*--------------------
035900000000IF  1C*---       $ERR      IFEQ '0'
036000000000     C*---                 MOVEL'N'       WRECOK
036100000000     C*---                 Z-ADDV1CTR1    KLTTRN
036200000000     C*---       KLTTRN    SETLLTNTLT01L
036300000000     C*---       KLTTRN    READETNTLT01L                 99
036400000000DO  2C*---       *IN99     DOWEQ*OFF
036500000000     C*---       WRECOK    ANDEQ'N'
036600000000     C*-DECORRENZA
036700000000IF  3C*---       DDEV1C    IFGT *ZEROS
036800000000IF  4C*---       TLTDDE    IFEQ DDEV1C
036900000000     C*---                 MOVEL'S'       WRECOK
037000000000E   4C*---                 ENDIF
037100000000E   3C*---                 ENDIF
037200000000     C*-SACEDENZA
037300000000IF  3C*---       DSCV1C    IFGT *ZEROS
037400000000     C*---                 MOVEL'N'       WRECOK
037500000000IF  4C*---       TLTDSC    IFEQ DSCV1C
037600000000     C*---                 MOVEL'S'       WRECOK
037700000000E   4C*---                 ENDIF
037800000000E   3C*---                 ENDIF
037900000000     C*---
038000000000     C*---       KLTTRN    READETNTLT01L                 99
038100000000E   2C*---                 ENDDO
038200000000     C*---
038300000000IF  2C*---       WRECOK    IFEQ 'N'
038400000000     C*---                 SETON                     606263
038500000000     C*---                 MOVEL'1'       $ERR
038600000000     C*---                 MOVELMSG,11    $MSG
038700000000     C*---                 EXSR CARMSG
038800000000E   2C*---                 ENDIF
038900000000E   1C*---                 ENDIF
039000000000     C*--------------------
039100000000     C* TRAINO  A  ESISTENTE
039200000000     C*--------------------
039300000000     C*---
039400000000IF  1C*---       $ERR      IFEQ '0'
039500000000     C*---                 MOVEL'N'       WRECOK
039600000000     C*---                 Z-ADDV1CTR2    KLTTRN
039700000000     C*---       KLTTRN    SETLLTNTLT01L
039800000000     C*---       KLTTRN    READETNTLT01L                 99
039900000000DO  2C*---       *IN99     DOWEQ*OFF
040000000000     C*---       WRECOK    ANDEQ'N'
040100000000     C*-DECORRENZA
040200000000IF  3C*---       DDEV1C    IFGT *ZEROS
040300000000IF  4C*---       TLTDDE    IFEQ DDEV1C
040400000000     C*---                 MOVEL'S'       WRECOK
040500000000E   4C*---                 ENDIF
040600000000E   3C*---                 ENDIF
040700000000     C*-SACEDENZA
040800000000IF  3C*---       DSCV1C    IFGT *ZEROS
040900000000     C*---                 MOVEL'N'       WRECOK
041000000000IF  4C*---       TLTDSC    IFEQ DSCV1C
041100000000     C*---                 MOVEL'S'       WRECOK
041200000000E   4C*---                 ENDIF
041300000000E   3C*---                 ENDIF
041400000000     C*---
041500000000     C*---       KLTTRN    READETNTLT01L                 99
041600000000E   2C*---                 ENDDO
041700000000     C*---
041800000000IF  2C*---       WRECOK    IFEQ 'N'
041900000000     C*---                 SETON                     616263
042000000000     C*---                 MOVEL'1'       $ERR
042100000000     C*---                 MOVELMSG,12    $MSG
042200000000     C*---                 EXSR CARMSG
042300000000E   2C*---                 ENDIF
042400000000E   1C*---                 ENDIF
042500000000     C***
042600000000     C* TRAINI SU CUI COPIARE
042700000000     C***
042800000000     C*--------------------
042900000000     C* MANTENIMENTO STESSA NUMERAZIONE
043000000000     C*--------------------
043100000000IF  1C     $ERR          IFEQ      '0'
043200000000IF  2C     V1CMAN        IFEQ      'S'
043300000000     C     V1CMAN        OREQ      'N'
043400000000X   2C                   ELSE
043500000000     C                   SETON                                        69
043600000000     C                   MOVEL     '1'           $ERR
043700000000     C                   MOVEL     MSG(17)       $MSG
043800000000     C                   EXSR      CARMSG
043900000000E   2C                   ENDIF
044000000000E   1C                   ENDIF
044100000000     C*--------------------
044200000000     C* PRIMO NUMERO TRAINO
044300000000     C*--------------------
044400000000IF  1C     $ERR          IFEQ      '0'
044500000000IF  2C     V1CMAN        IFEQ      'S'                                          *STESSA NUMERAZIONE
044600000000IF  3C     V1CNUM        IFGT      *ZEROS
044700000000     C                   SETON                                        64
044800000000     C                   MOVEL     '1'           $ERR
044900000000     C                   MOVEL     MSG(20)       $MSG
045000000000     C                   EXSR      CARMSG
045100000000E   3C                   ENDIF
045200000000X   2C                   ELSE                                                   *NUOVA NUMERAZIONE
045300000000IF  3C     V1CNUM        IFEQ      *ZEROS
045400000000     C                   SETON                                        64
045500000000     C                   MOVEL     '1'           $ERR
045600000000     C                   MOVEL     MSG(13)       $MSG
045700000000     C                   EXSR      CARMSG
045800000000E   3C                   ENDIF
045900000000E   2C                   ENDIF
046000000000E   1C                   ENDIF
046100000000     C*--------------------
046200000000     C* NUOVA DATA DECORRENZA
046300000000     C*--------------------
046400000000     C                   Z-ADD     *ZEROS        DD2V1C
046500000000IF  1C     $ERR          IFEQ      '0'
046600000000     C                   Z-ADD     V1CDD2        WDA8
046700000000     C                   Z-ADD     *ZEROS        DA8W
046800000000     C                   EXSR      CHKDAT
046900000000     C                   Z-ADD     WDA8          V1CDD2
047000000000     C                   Z-ADD     DA8W          DD2V1C
047100000000IF  2C     $ERR          IFEQ      '1'
047200000000     C                   SETON                                        65
047300000000     C                   MOVEL     MSG(14)       $MSG
047400000000     C                   EXSR      CARMSG
047500000000E   2C                   ENDIF
047600000000E   1C                   ENDIF
047700000000     C*--------------------
047800000000     C* NUOVA DATA SCADENZA
047900000000     C*--------------------
048000000000     C                   Z-ADD     *ZEROS        DS2V1C
048100000000IF  1C     $ERR          IFEQ      '0'
048200000000     C                   Z-ADD     V1CDS2        WDA8
048300000000     C                   Z-ADD     *ZEROS        DA8W
048400000000     C                   EXSR      CHKDAT
048500000000     C                   Z-ADD     WDA8          V1CDS2
048600000000     C                   Z-ADD     DA8W          DS2V1C
048700000000IF  2C     $ERR          IFEQ      '1'
048800000000     C                   SETON                                        66
048900000000     C                   MOVEL     MSG(15)       $MSG
049000000000     C                   EXSR      CARMSG
049100000000E   2C                   ENDIF
049200000000E   1C                   ENDIF
049300000000     C* SCADENZA MAGGIORE O UGUALE DECORRENZA
049400000000IF  1C     $ERR          IFEQ      '0'
049500000000IF  2C     DS2V1C        IFLT      DD2V1C
049600000000     C                   SETON                                        6566
049700000000     C                   MOVEL     '1'           $ERR
049800000000     C                   MOVEL     MSG(16)       $MSG
049900000000     C                   EXSR      CARMSG
050000000000E   2C                   ENDIF
050100000000E   1C                   ENDIF
050200000000     c*
050300000000     C                   ENDSR
050400000000     C*------------------------------------------------------------------------*
050500000000     C* CHKDAT - CONTROLLA LA DATA
050600000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
050700000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
050800000000     C*------------------------------------------------------------------------*
050900000000     C     CHKDAT        BEGSR
051000000000     C*
051100000000     C     *LIKE         DEFINE    V1CDDE        WDA8
051200000000     C     *LIKE         DEFINE    V1CDDE        DA8W
051300000000     C*
051400000000     C                   Z-ADD     *ZEROS        DA8W
051500000000     C*
051600000000     C                   Z-ADD     WDA8          G08DAT
051700000000     C                   Z-ADD     *ZEROS        G08INV
051800000000     C                   MOVEL     '0'           G08ERR
051900000000     C                   CALL      'XSRDA8'
052000000000     C                   PARM                    WLBDA8
052100000000     C                   Z-ADD     G08DAT        WDA8
052200000000IF  1C     G08ERR        IFEQ      '1'
052300000000     C                   MOVEL     '1'           $ERR
052400000000     C                   ELSE
052500000000     C                   Z-ADD     G08INV        DA8W
052600000000E   1C                   ENDIF
052700000000     C*
052800000000     C                   ENDSR
052900000000     C*------------------------------------------------------------------------*
053000000000     C* IMPVID - IMPOSTAZIONI VIDEO
053100000000     C*------------------------------------------------------------------------*
053200000000     C     IMPVID        BEGSR
053300000000     C*
053400000000     C                   MOVEL     'S'           V1CSIM                         *SIMULAZIONE LANCIO
053500000000     C                   MOVEL     'N'           V1CESI                         *SOVRASCRITTURA
053600000000     C                   MOVEL     'N'           V1CMAN                         *MANTENIMENTO NUMER.
053700000000     C*
053800000000     C                   ENDSR
053900000000     C*------------------------------------------------------------------------*
054000000000     C* CARTAB - CARICA LE TABELLE
054100000000     C*------------------------------------------------------------------------*
054200000000     C     CARTAB        BEGSR
054300000000     C                   ENDSR
054400000000     C*------------------------------------------------------------------------*
054500000000     C* *INZSR - ROUTINE INIZIALE
054600000000     C*------------------------------------------------------------------------*
054700000000     C     *INZSR        BEGSR
054800000000     C*--------------------
054900000000     C* RICEVIMENTO PARAMETRI
055000000000     C*--------------------
055100000000     C     *ENTRY        PLIST
055200000000     C                   PARM                    KPJBA
055300000000     C*--------------------
055400000000     C* VARIABILI RIFERITE AL DATA BASE
055500000000     C*--------------------
055600000000     C*---       *LIKE     DEFN TLTTRN    KLTTRN           *TNTLT00F
055700000000     C*---       *LIKE     DEFN TLTDDE    KLTDDE
055800000000     C*--------------------
055900000000     C* CHIAVI DI LETTURA
056000000000     C*--------------------
056100000000     C* LETTURA TNTLT01L - COMPLETA
056200000000     C*---       KEYTLT    KLIST
056300000000     C*---                 KFLD           KLTTRN           *NUMERO TRAINO
056400000000     C*---                 KFLD           KLTDDE           *DATA DECORRENZA
056500000000     C*--------------------
056600000000     C* VARIABILI  N O N  RIFERITE AL DATA BASE
056700000000     C*--------------------
056800000000     C     *LIKE         DEFINE    V1CDDE        DDEV1C                         *DATE DA CUI COPIARE
056900000000     C     *LIKE         DEFINE    V1CDSC        DSCV1C
057000000000     C     *LIKE         DEFINE    V1CDD2        DD2V1C                         *NUOVE DATE
057100000000     C     *LIKE         DEFINE    V1CDS2        DS2V1C
057200000000     C*--------------------
057300000000     C* VARIABILI DI CONTROLLO
057400000000     C*--------------------
057500000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
057600000000     C***
057700000000     C* IMPOSTAZIONI VIDEO
057800000000     C***
057900000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
058000000000     C*
058100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
058200000000     C                   EXSR      INZSTD
058300000000     C*
058400000000     C* CARICA LE TABELLE
058500000000     C                   EXSR      CARTAB
058600000000     C*
058700000000     C* IMPOSTAZIONI VIDEO
058800000000     C                   EXSR      IMPVID
058900000000     C*
059000000000     C* EMETTE VIDEATA
059100000000     C                   EXSR      CARV1
059200000000     C*
059300000000     C                   ENDSR
059400000000     C*------------------------------------------------------------------------*
059500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
059600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
059700000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
059800000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
059900000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
060000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
060100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
060200000000     C*------------------------------------------------------------------------*
060300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
060400000000     C*------------------------------------------------------------------------*
060500000000     C     INZSTD        BEGSR
060600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
060700000000     C                   CALL      'XKEYS'
060800000000     C                   PARM                    DSKEYS
060900000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
061000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
061100000000     C     XMSGPA        PLIST
061200000000     C                   PARM                    DSMSMS
061300000000     C                   PARM                    DSMSPG
061400000000     C                   PARM                    DSMSKE
061500000000     C                   PARM                    DSMSER
061600000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
061700000000     C                   MOVEL     'TNTL55R'     DSMSPG                         *NOME PGM CHIAMANTE
061800000000     C                   MOVEL     'TNTL55R'     VMCQUE                         *CODA MESSAGGI DESTI
061900000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
062000000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
062100000000     C* INIZIALIZZO FLAG CONTROLLO PGM
062200000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
062300000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
062400000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
062500000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
062600000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
062700000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
062800000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
062900000000     C* LANCIO ROUTINE PER VERIFICARE SE
063000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
063100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
063200020716
063300020716     c     *dtaara       define    §azute        azuteds
063400020716     c     *dtaara       define    §datiute      ddatiute
063500020716
063600020716     c                   in(E)     *dtaara
063700020716     c                   if        %Error or rsut = *blanks
063800020716     c                   clear                   tibs34ds
063900020716     c                   call      'TIBS34R'
064000020716     c                   parm                    Tibs34Ds
064100020716     c                   in        *dtaara
064200020716     c                   endif
064300020717
064400020717      * Controllo il profilo utente
064500020717     c                   ExSr      CtrUte
064600020717
064700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
064800000000     C                   BITOFF    '01234567'    POSMI1            1
064900000000     C                   BITOFF    '01234567'    POSMI2            1
065000000000     C                   BITOFF    '01234567'    POSMA1            1
065100000000     C                   BITOFF    '01234567'    POSMA2            1
065200000000     C                   BITON     '34'          POSMI1            1            *18
065300000000     C                   BITON     '7'           POSMI2            1            *01
065400000000     C                   BITON     '34'          POSMA1            1            *18
065500000000     C                   BITON     '0'           POSMA2            1            *80
065600000000     C                   MOVE      POSMI2        POSMIN            2
065700000000     C                   MOVEL     POSMI1        POSMIN
065800000000     C                   MOVE      POSMA2        POSMAX            2
065900000000     C                   MOVEL     POSMA1        POSMAX
066000000000     C                   ENDSR
066100020717      *------------------------------------------------------------------------*
066200020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
066300020717      *------------------------------------------------------------------------*
066400020717     c     CtrUte        BegSr
066500020717
066600020717     c                   Eval      *In76 = *Off
066700020717
066800020717      * Controllo se il profilo è ok da tibs34
066900020717     c                   If        DutErr = 'E'
067000020717     c                   eval      $Msg = DutMsg
067100020717     c                   ExSr      CarMsg
067200020717     c                   eval      *In76 = *On
067300020717     c                   endIf
067400020717
067500020717      * Controllo se il profilo di sede
067600020717     c                   If        DutLpo <> 'S'
067700020717     c                   Eval      $Msg = Msg(21)
067800020717     c                   ExSr      CarMsg
067900020717     c                   Eval      *In76 = *On
068000020717     c                   EndIf
068100020717
068200020717     c                   endSr
068300000000     C*------------------------------------------------------------------------*
068400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
068500000000     C*------------------------------------------------------------------------*
068600000000     C     FUNSTD        BEGSR
068700000000     C                   SELECT
068800000000     C     KEY           WHENEQ    F3
068900000000     C                   EXSR      F03STD
069000000000     C     KEY           WHENEQ    F12
069100000000     C                   EXSR      F12STD
069200000000     C     KEY           WHENEQ    F24
069300000000     C                   EXSR      F24STD
069400000000     C                   OTHER
069500000000     C                   MOVEL     MSG(2)        $MSG
069600000000     C                   EXSR      CARMSG
069700000000     C                   ENDSL
069800000000     C                   ENDSR
069900000000     C*------------------------------------------------------------------------*
070000000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
070100000000     C*------------------------------------------------------------------------*
070200000000     C     F03STD        BEGSR
070300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
070400000000     C                   MOVEL     '1'           $F03
070500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
070600000000     C                   MOVEL     '1'           $FINE
070700000000     C* PULISCE IL MESSAGGIO
070800000000     C                   ENDSR
070900000000     C*------------------------------------------------------------------------*
071000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
071100000000     C*------------------------------------------------------------------------*
071200000000     C     F12STD        BEGSR
071300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
071400000000     C                   MOVEL     '1'           $FINE
071500000000     C                   ENDSR
071600000000     C*------------------------------------------------------------------------*
071700000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
071800000000     C*------------------------------------------------------------------------*
071900000000     C     F24STD        BEGSR
072000000000     C*
072100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
072200000000     C                   EXSR      OUTFUN
072300000000     C*
072400000000     C                   ENDSR
072500000000     C*------------------------------------------------------------------------*
072600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
072700000000     C*          CODA MESSAGGI DEL PGM
072800000000     C*------------------------------------------------------------------------*
072900000000     C     CARMSG        BEGSR
073000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
073100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
073200000000     C     MSGULT        IFEQ      0
073300000000     C                   MOVEL     '1'           *IN40
073400000000     C                   WRITE     TL55DVK
073500000000     C                   MOVEL     '0'           *IN40
073600000000     C                   ENDIF
073700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
073800000000     C                   MOVEL     $MSG          DSMSMS
073900000000     C                   CALL      'XMSG'        XMSGPA
074000000000     C     DSMSER        IFEQ      0
074100000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
074200000000     C                   ENDIF
074300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
074400000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
074500000000     C                   WRITE     TL55DVM                                      *SFL MESS
074600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
074700000000     C                   MOVEL     *BLANKS       $MSG
074800000000     C                   ENDSR
074900000000     C*------------------------------------------------------------------------*
075000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
075100000000     C*------------------------------------------------------------------------*
075200000000     C     OUTMSG        BEGSR
075300000000     C* VISUALIZZAZIONE MESSAGGIO
075400000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
075500000000     C                   WRITE     TL55DVK                                      *CONTR MESS
075600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
075700000000     C                   Z-ADD     *ZEROS        MSGULT
075800000000     C                   ENDSR
075900000000     C*------------------------------------------------------------------------*
076000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
076100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
076200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
076300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
076400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
076500000000     C*------------------------------------------------------------------------*
076600000000     C     CARFUN        BEGSR
076700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
076800000000     C     1             DO        31            I                 5 0          --- 1 -->
076900000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
077000000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
077100000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
077200020716     c                   If        DutLpo = 'S'
077300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
077400000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
077500000000     C                   ELSE                                                   *INTERROG.
077600000000     C                   MOVEL     FUNASI        FAT(I)
077700000000     C                   ENDIF
077800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
077900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
078000000000     C                   MOVEL     FUNAFG        FAT(I)
078100000000     C                   ELSE                                                   *INTERROG.
078200000000     C                   MOVEL     FUNAFI        FAT(I)
078300000000     C                   ENDIF
078400000000     C                   ENDIF                                                  <-- 2 ---
078500000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
078600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
078700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
078800000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
078900000000     C                   ELSE
079000000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
079100000000     C                   ENDIF
079200000000     C                   ENDIF
079300000000     C                   ENDDO                                                  <-- 1 ---
079400000000     C                   ENDSR
079500000000     C*------------------------------------------------------------------------*
079600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
079700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
079800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
079900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
080000000000     C*------------------------------------------------------------------------*
080100000000     C     OUTFUN        BEGSR
080200000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
080300000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
080400000000     C                   ENDDO
080500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
080600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
080700000000     C     1             DO        31            I
080800000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
080900000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
081000000000     C                   ADD       1             FUNNAT
081100000000     C                   ENDIF
081200000000     C                   ENDDO
081300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
081400000000     C     FUNNAT        IFGT      8
081500000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
081600000000     C                   ENDIF
081700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
081800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
081900000000     C                   Z-ADD     8             FUNVIS            5 0
082000000000     C     FAT(23)       IFEQ      'S'
082100000000     C                   SUB       1             FUNVIS
082200000000     C                   ENDIF
082300000000     C     FAT(24)       IFEQ      'S'
082400000000     C                   SUB       1             FUNVIS
082500000000     C                   ENDIF
082600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
082700000000     C                   Z-ADD     *ZEROS        W                 5 0
082800000000     C                   ADD       1             FUNULT
082900000000     C     FUNULT        IFGT      31
083000000000     C                   Z-ADD     1             FUNULT
083100000000     C                   ENDIF
083200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
083300000000     C     FUNULT        DO        31            I                              --- 1 -->
083400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
083500000000     C     W             IFLT      FUNVIS                                       --- 2 -->
083600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
083700000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
083800000000     C     FDE(I)        ANDNE     *BLANKS
083900000000     C     I             IFEQ      23                                           *TOLGO F23
084000000000     C     I             OREQ      24                                           *E F24
084100000000     C                   ELSE
084200000000     C                   ADD       1             W
084300000000     C                   MOVEL     FDE(I)        FUV(W)
084400000000     C                   ENDIF
084500000000     C                   ENDIF                                                  <-- 3 ---
084600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
084700000000     C                   Z-ADD     I             FUNULT
084800000000     C                   ENDIF                                                  <-- 2 ---
084900000000     C                   ENDDO                                                  <-- 1 ---
085000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
085100000000     C                   MOVEL     FUV(1)        VFD001
085200000000     C                   MOVEL     FUV(2)        VFD002
085300000000     C                   MOVEL     FUV(3)        VFD003
085400000000     C                   MOVEL     FUV(4)        VFD004
085500000000     C                   MOVEL     FUV(5)        VFD005
085600000000     C                   MOVEL     FUV(6)        VFD006
085700000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
085800000000     C     FAT(24)       ANDEQ     'N'
085900000000     C                   MOVEL     FUV(7)        VFD007
086000000000     C                   MOVEL     FUV(8)        VFD008
086100000000     C                   ELSE                                                   <-- 1 -->
086200000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
086300000000     C     FAT(24)       ANDEQ     'S'
086400000000     C                   MOVEL     FDE(23)       VFD007
086500000000     C                   MOVEL     FDE(24)       VFD008
086600000000     C                   ELSE                                                   <-- 2 -->
086700000000     C                   MOVEL     FUV(7)        VFD007
086800000000     C     FAT(23)       IFEQ      'S'
086900000000     C                   MOVEL     FDE(23)       VFD008
087000000000     C                   ELSE
087100000000     C                   MOVEL     FDE(24)       VFD008
087200000000     C                   ENDIF
087300000000     C                   ENDIF                                                  <-- 2 ---
087400000000     C                   ENDIF                                                  <-- 1 ---
087500000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
087600000000     C                   ENDSR
087700000000     C*------------------------------------------------------------------------*
087800000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
087900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
088000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
088100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
088200000000     C*------------------------------------------------------------------------*
088300000000     C     CONFUN        BEGSR
088400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
088500000000     C     1             DO        31            I                              --- 1 -->
088600000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
088700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
088800000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
088900000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
089000000000     C                   EXSR      CARMSG
089100000000     C                   MOVEL     '1'           $ERR
089200000000     C                   ENDIF                                                  <-- 3 ---
089300000000     C                   ENDIF                                                  <-- 2 ---
089400000000     C                   ENDDO                                                  <-- 1 ---
089500000000     C                   ENDSR
089600000000     O*------------------------------------------------------------------------*
089700000000** MSG - (MESSAGGI)
089800000000TNTL55R- Tasto funzionale non valido.                                          1
089900000000TNTL55R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
090000000000TNTL55R- Opzione non valida.                                                   3
090100000000TNTL55R- Opzione abilitata ma non gestita. Avvertire CED.                      4
090200000000TNTL55R- Inizio elenco.                                                        5
090300000000TNTL55R- Fine elenco.                                                          6
090400000000TNTL55R- Inserire il numero traino                                             7
090500000000TNTL55R- Il numero traino A deve essere maggiore o uguale al numero traino DA  8
090600000000TNTL55R- Data errata                                                           9
090700000000TNTL55R- Inserire la data decorrenza o la data scadenza o tutte e due         10
090800000000TNTL55R- Traino DA inesistente                                                11
090900000000TNTL55R- Traino A inesistente                                                 12
091000000000TNTL55R- Inserire il primo numero traino dal quale iniziare a copiare         13
091100000000TNTL55R- Inserire la nuova data decorrenza                                    14
091200000000TNTL55R- Inserire la nuova data scadenza                                      15
091300000000TNTL55R- La data scadenza deve essere maggiore o uguale alla data decorrenza  16
091400000000TNTL55R- 'S' per mantentere la stessa numerazione dei traini da copiare       17
091500000000TNTL55R- 'S' per sovrascrivere i dati nuovi ai vecchi se già esistenti        18
091600000000TNTL55R- 'S' per stampare elenco traini da duplicare con eventuali anomalie   19
091700000000TNTL55R- Per avere una nuova numerazione occorre richiederla                  20
091800020717TNTL55R- Profilo non abilitato alla funzione                                  21
091900000000TNTL55R- 22
092000000000TNTL55R- 23
092100000000TNTL55R- 24
092200000000TNTL55R- Elaborazione lanciata ...                                            25
092300000000** CMD  (COMANDI)
092400000000WRKSPLF
092500000000
092600000000
092700000000
092800000000
092900000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
093000000000F1    -                   -    NN     NN
093100000000F2    -                   -    NN     NN
093200000000F3    -F3=Fine            -    SS     SS
093300000000F4    -                   -    NN     NN
093400000000F5    -                   -    NN     NN
093500000000F6    -F6=Conferma        -    SS     SS
093600000000F7    -                   -    NN     NN
093700000000F8    -                   -    NN     NN
093800000000F9    -                   -    NN     NN
093900000000F10   -                   -    NN     NN
094000000000F11   -                   -    NN     NN
094100000000F12   -F12=Ritorno        -    SS     SS
094200000000F13   -                   -    NN     NN
094300000000F14   -                   -    NN     NN
094400000000F15   -                   -    NN     NN
094500000000F16   -                   -    NN     NN
094600000000F17   -                   -    NN     NN
094700000000F18   -                   -    NN     NN
094800000000F19   -                   -    NN     NN
094900000000F20   -                   -    NN     NN
095000000000F21   -                   -    NN     NN
095100000000F22   -F22=Gestione stampe-    EE     EE
095200000000F23   -                   -    NN     NN
095300000000F24   -F24=Altri tasti    -    NN     NN
095400000000ENTER -                   -    SS     SS
095500000000ROLLUP-                   -    NN     NN
095600000000ROLLDN-                   -    NN     NN
095700000000HELP  -                   -    NN     NN
095800000000PRINT -                   -    NN     NN
095900000000BSPACE-                   -    NN     NN
096000000000CLEAR -                   -    NN     NN

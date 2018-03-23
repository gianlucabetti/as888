000100020716     H DECEDIT('0,') DATEDIT(*DMY.)
000200960517     H*------------------------------------------------------------------------*
000300960517     H* FORZATURA SCADENZA TRAINI - RICHIESTA PARAMETRI                        *
000400960517     H*------------------------------------------------------------------------*
000500960517     F*------------------------------------------------------------------------*
000600960517     F* DATA BASE
000700960517     F*------------------------------------------------------------------------*
000800960517     FTNTL53D   CF   E             WORKSTN INFDS(DEVDS1)
000900960517     F                                     SFILE(TL53DVM:MSGULT)
001000960517     D*------------------------------------------------------------------------*
001100960517     D* SCHIERE
001200960517     D*------------------------------------------------------------------------*
001300960517     D*-------------------
001400960517     D* MESSAGGI, COMANDI E FUNZIONI
001500960517     D*-------------------
001600960517     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001700960517     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
001800960517     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001900960517     D*-------------------
002000960517     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002100960517     D*-------------------
002200960517     D FFU             S              6    DIM(31)                              *FUNZIONE
002300960517     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002400960517     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002500960517     D*-------------------
002600960517     D* TABELLA DESCRIZIONI A VIDEO
002700960517     D*-------------------
002800960517     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002900960517     D*-------------------
003000960517     D* TABELLA TASTI FUNZIONALI PREMUTI
003100960517     D*-------------------
003200960517     D KEYS            S              1    DIM(40)
003300960517     D*------------------------------------------------------------------------*
003400960517     D* INPUT
003500960517     D*------------------------------------------------------------------------*
003600960517     D*---------------------
003700960517     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003800960517     D*---------------------
003900960517     D DSFUN         E DS
004000960517     D  FUNRIG                 1     40
004100960517     D*---------------------
004200960517     D* DS PER GESTIONE TASTI FUNZIONALI
004300960517     D*---------------------
004400960517     D DSKEYS        E DS
004500960517     D  KEYRIG                 1     40
004600960517     D*---------------------
004700960517     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004800960517     D*---------------------
004900960517     D                SDS
005000960517     D  USER                 254    263
005100960517     D*---------------------
005200960517     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005300960517     D*---------------------
005400960517     D DEVDS1          DS
005500960517     D  KEY                  369    369
005600960517     D  CURSOR               370    371
005700960517     D*---------------------
005800960517     D* ARCHITETTURA
005900960517     D*---------------------
006000960517     D KPJBA         E DS
006100960517     D*---------------------
006200960517     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
006300960517     D*---------------------
006400960517     D DSMSG         E DS
006500020716
006600020716     d DdatiUte      e ds
006700020716     d AzuteDs       e ds                  ExtName(Azute00f)
006800020716     d Tibs34Ds      e ds                  Inz
006900020716
007000960517     D*---------------------
007100960517     D* DS PARAMETRI DI LANCIO
007200960517     D*------------------
007300960517     D PARAM           DS                  INZ
007400960517     D  PARTR1                 1      7  0
007500960517     D  PARTR2                 8     14  0
007600960517     D  PARDDE                15     22  0
007700960517     D  PARDSC                23     30  0
007800960517     D  PARDS2                31     38  0
007900960517     D  PARSIM                39     39
008000980605     D  PARTE1                40     46  0
008100980605     D  PARTE2                47     53  0
008200980605     D  PARTE3                54     60  0
008300980605     D  PARTE4                61     67  0
008400980605     D  PARTE5                68     74  0
008500980605     D  PARTE6                75     81  0
008600980605     D  PARTE7                82     88  0
008700980605     D  PARTE8                89     95  0
008800980605     D  PARTE9                96    102  0
008900980605     D  PARTE0               103    109  0
009000980605     D  PARTEA               110    116  0
009100980605     D  PARTEB               117    123  0
009200960517     D*---------------------
009300960517     D* DS CONTROLLO DATA (8)
009400960517     D*------------------
009500960517     D WLBDA8          DS                  INZ
009600960517     D  G08DAT                 1      8  0
009700960517     D  G08INV                 9     16  0
009800960517     D  G08ERR                17     17
009900960517     D  G08TGI                18     22  0
010000960517     C*------------------------------------------------------------------------*
010100960517     C* MAIN LINE
010200960517     C*------------------------------------------------------------------------*
010300960517     C*
010400960517     C* CICLO FINO A CHE:
010500960517DO  1C     $FINE         DOUEQ     '1'
010600960517     C*
010700960517     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010800960517     C*
010900960517     C* CONTROLLA GESTIONE VIDEO
011000960517IF  2C     $GES          IFEQ      'V1'
011100960517     C                   EXSR      GESV1                                        *PARAMETRI
011200960517E   2C                   ENDIF
011300960517E   1C                   ENDDO
011400960517     C*
011500960517     C                   SETON                                        LR
011600960517     C*------------------------------------------------------------------------*
011700960517     C* GESV1 - GESTIONE VIDEO UNO
011800960517     C*------------------------------------------------------------------------*
011900960517     C     GESV1         BEGSR
012000960517     C*
012100960517IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
012200960517     C                   EXSR      CARV1                                        *CARICA VIDEO
012300960517E   1C                   ENDIF
012400960517     C*
012500960517IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012600960517     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012700960517     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012800960517E   1C                   ENDIF
012900960517     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
013000960517     C*
013100960517     C                   EXFMT     TL53DV1                                      *PARAMETRI
013200960517     C*
013300960517     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013400020717
013500020717      * Se Profilo non OK devo uscire dal pgm
013600020717     c                   If        *In76
013700020717     c                   Eval      $err = '1'
013800020717     c                   Eval      $fine = '1'
013900020717     c                   EndIf
014000960517     C*
014100960517IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
014200960517SL  2C                   SELECT
014300960517     C     KEY           WHENEQ    ENTER
014400960517     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
014500960517     C     KEY           WHENEQ    F6
014600960517     C                   EXSR      F06GES                                       *CONFERMA
014700960517     C     KEY           WHENEQ    F22
014800960517     C                   EXSR      F22GES                                       *GESTIONE STAMPE
014900960517     C                   OTHER
015000960517     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
015100960517E   2C                   ENDSL
015200960517E   1C                   ENDIF
015300960517     C*
015400960517     C                   ENDSR
015500960517     C*------------------------------------------------------------------------*
015600960517     C* CARV1 - CARICA VIDEO UNO
015700960517     C*------------------------------------------------------------------------*
015800960517     C     CARV1         BEGSR
015900960517     C*
016000960517     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016100960517     C*
016200960517     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016300960517     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016400960517     C*
016500960517     C                   WRITE     TL53DVT                                      *TESTATA
016600960517     C                   WRITE     TL53DVF                                      *FUNZIONI
016700960517     C*
016800960517     C                   ENDSR
016900960517     C*------------------------------------------------------------------------*
017000960517     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
017100960517     C*------------------------------------------------------------------------*
017200960517     C     ENTV1         BEGSR
017300960517     C*
017400960517     C* CONTROLLA I CAMPI IMMESSI
017500960517     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
017600960517     C*
017700960517     C                   ENDSR
017800960517     C*------------------------------------------------------------------------*
017900960517     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
018000960517     C*------------------------------------------------------------------------*
018100960517     C     F06GES        BEGSR
018200960517     C*
018300960517     C* CONTROLLA I CAMPI IMMESSI
018400960517     C                   EXSR      DECODI
018500960517     C*
018600960517     C* LANCIA IL PROGRAMMA DI DUPLICA
018700960517IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
018800960517     C                   Z-ADD     V1CTR1        PARTR1                         *DA NUMERO TRAINO
018900960517     C                   Z-ADD     V1CTR2        PARTR2                         * A NUMERO TRAINO
019000960517     C                   Z-ADD     DDEV1C        PARDDE                         *CON DATA DECORRENZA
019100960517     C                   Z-ADD     DSCV1C        PARDSC                         *CON DATA SCADENZA
019200960517     C                   Z-ADD     DS2V1C        PARDS2                         *NUOVA DATA SCADENZA
019300960517     C                   MOVEL     V1CSIM        PARSIM                         *SIMULAZIONE LANCIO
019400980605     C                   Z-ADD     V1CTE1        PARTE1                         *TRAINO ESCLUSO
019500980605     C                   Z-ADD     V1CTE2        PARTE2                         *TRAINO ESCLUSO
019600980605     C                   Z-ADD     V1CTE3        PARTE3                         *TRAINO ESCLUSO
019700980605     C                   Z-ADD     V1CTE4        PARTE4                         *TRAINO ESCLUSO
019800980605     C                   Z-ADD     V1CTE5        PARTE5                         *TRAINO ESCLUSO
019900980605     C                   Z-ADD     V1CTE6        PARTE6                         *TRAINO ESCLUSO
020000980605     C                   Z-ADD     V1CTE7        PARTE7                         *TRAINO ESCLUSO
020100980605     C                   Z-ADD     V1CTE8        PARTE8                         *TRAINO ESCLUSO
020200980605     C                   Z-ADD     V1CTE9        PARTE9                         *TRAINO ESCLUSO
020300980605     C                   Z-ADD     V1CTE0        PARTE0                         *TRAINO ESCLUSO
020400980605     C                   Z-ADD     V1CTEA        PARTEA                         *TRAINO ESCLUSO
020500980605     C                   Z-ADD     V1CTEB        PARTEB                         *TRAINO ESCLUSO
020600980605     C*
020700960517     C                   MOVEL     'TL54'        KCOAZ
020800960517     C                   MOVEL     PARAM         KPJBU
020900960517     C                   CALL      'BCH10'
021000960517     C                   PARM                    KPJBA
021100960517     C                   MOVEL     MSG(20)       $MSG                           ELABORAZIONE LANCIAT
021200960517     C                   EXSR      CARMSG
021300960517E   1C                   ENDIF
021400960517     C*
021500960517     C                   ENDSR
021600960517     C*------------------------------------------------------------------------*
021700960517     C* F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
021800960517     C*------------------------------------------------------------------------*
021900960517     C     F22GES        BEGSR
022000960517     C*
022100960517     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
022200960517     C                   Z-ADD     80            LENGH            15 5
022300960517     C                   CALL      'QCAEXEC'
022400960517     C                   PARM                    CMD(1)
022500960517     C                   PARM                    LENGH
022600960517     C*
022700960517     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
022800960517     C*
022900960517     C                   ENDSR
023000960517     C*------------------------------------------------------------------------*
023100960517     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023200960517     C*------------------------------------------------------------------------*
023300960517     C     DECODI        BEGSR
023400960517     C*
023500960517     C* REIMPOSTA INDICATORI DI ERRORE
023600960517     C                   SETOFF                                       606162
023700960517     C                   SETOFF                                       636668
023800960517     C*--------------------
023900960517     C* SIMULAZIONE LANCIO
024000960517     C*--------------------
024100960517IF  1C     $ERR          IFEQ      '0'
024200960517IF  2C     V1CSIM        IFEQ      'S'
024300960517     C     V1CSIM        OREQ      'N'
024400960517X   2C                   ELSE
024500960517     C                   SETON                                        68
024600960517     C                   MOVEL     '1'           $ERR
024700960517     C                   MOVEL     MSG(11)       $MSG
024800960517     C                   EXSR      CARMSG
024900960517E   2C                   ENDIF
025000960517E   1C                   ENDIF
025100960517     C***
025200960517     C* TRAINI DA MODIFICARE
025300960517     C***
025400960517     C*--------------------
025500960517     C* DA NUMERO TRAINO
025600960517     C*--------------------
025700960517IF  1C     $ERR          IFEQ      '0'
025800960517IF  2C     V1CTR1        IFEQ      *ZEROS
025900960517     C                   SETON                                        60
026000960517     C                   MOVEL     '1'           $ERR
026100960517     C                   MOVEL     MSG(7)        $MSG
026200960517     C                   EXSR      CARMSG
026300960517E   2C                   ENDIF
026400960517E   1C                   ENDIF
026500960517     C*--------------------
026600960517     C* A NUMERO TRAINO
026700960517     C*--------------------
026800960517IF  1C     $ERR          IFEQ      '0'
026900960517IF  2C     V1CTR2        IFEQ      *ZEROS
027000960517     C                   SETON                                        61
027100960517     C                   MOVEL     '1'           $ERR
027200960517     C                   MOVEL     MSG(7)        $MSG
027300960517     C                   EXSR      CARMSG
027400960517E   2C                   ENDIF
027500960517E   1C                   ENDIF
027600960517     C* TRAINO A >= TRAINO DA
027700960517IF  1C     $ERR          IFEQ      '0'
027800960517IF  2C     V1CTR2        IFLT      V1CTR1
027900960517     C                   SETON                                        61
028000960517     C                   MOVEL     '1'           $ERR
028100960517     C                   MOVEL     MSG(8)        $MSG
028200960517     C                   EXSR      CARMSG
028300960517E   2C                   ENDIF
028400960517E   1C                   ENDIF
028500960517     C*--------------------
028600960517     C* DATA DECORRENZA
028700960517     C*--------------------
028800960517     C                   Z-ADD     *ZEROS        DDEV1C
028900960517IF  1C     $ERR          IFEQ      '0'
029000960517IF  2C     V1CDDE        IFGT      *ZEROS
029100960517     C                   Z-ADD     V1CDDE        WDA8
029200960517     C                   Z-ADD     *ZEROS        DA8W
029300960517     C                   EXSR      CHKDAT
029400960517     C                   Z-ADD     WDA8          V1CDDE
029500960517     C                   Z-ADD     DA8W          DDEV1C
029600960517IF  3C     $ERR          IFEQ      '1'
029700960517     C                   SETON                                        62
029800960517     C                   MOVEL     MSG(9)        $MSG
029900960517     C                   EXSR      CARMSG
030000960517E   3C                   ENDIF
030100960517E   2C                   ENDIF
030200960517E   1C                   ENDIF
030300960517     C*--------------------
030400960517     C* DATA SCADENZA
030500960517     C*--------------------
030600960517     C                   Z-ADD     *ZEROS        DSCV1C
030700960517IF  1C     $ERR          IFEQ      '0'
030800960517IF  2C     V1CDSC        IFGT      *ZEROS
030900960517     C                   Z-ADD     V1CDSC        WDA8
031000960517     C                   Z-ADD     *ZEROS        DA8W
031100960517     C                   EXSR      CHKDAT
031200960517     C                   Z-ADD     WDA8          V1CDSC
031300960517     C                   Z-ADD     DA8W          DSCV1C
031400960517IF  3C     $ERR          IFEQ      '1'
031500960517     C                   SETON                                        63
031600960517     C                   MOVEL     MSG(9)        $MSG
031700960517     C                   EXSR      CARMSG
031800960517E   3C                   ENDIF
031900960517E   2C                   ENDIF
032000960517E   1C                   ENDIF
032100960517     C* DECORRENZA O SCADENZA DA INSERIRE
032200960517IF  1C     $ERR          IFEQ      '0'
032300960517IF  2C     V1CDDE        IFEQ      *ZEROS
032400960517     C     V1CDSC        ANDEQ     *ZEROS
032500960517     C                   SETON                                        6263
032600960517     C                   MOVEL     '1'           $ERR
032700960517     C                   MOVEL     MSG(10)       $MSG
032800960517     C                   EXSR      CARMSG
032900960517E   2C                   ENDIF
033000960517E   1C                   ENDIF
033100960517     C*--------------------
033200960517     C* NUOVA DATA SCADENZA
033300960517     C*--------------------
033400960517     C                   Z-ADD     *ZEROS        DS2V1C
033500960517IF  1C     $ERR          IFEQ      '0'
033600960517     C                   Z-ADD     V1CDS2        WDA8
033700960517     C                   Z-ADD     *ZEROS        DA8W
033800960517     C                   EXSR      CHKDAT
033900960517     C                   Z-ADD     WDA8          V1CDS2
034000960517     C                   Z-ADD     DA8W          DS2V1C
034100960517IF  2C     $ERR          IFEQ      '1'
034200960517     C                   SETON                                        66
034300960522     C                   MOVEL     MSG(12)       $MSG
034400960517     C                   EXSR      CARMSG
034500960517E   2C                   ENDIF
034600960517E   1C                   ENDIF
034700960523     C*
034800960523     C* NUOVA SCADENZA <> VECCHIA SCADENZA
034900960523IF  1C     $ERR          IFEQ      '0'
035000960523IF  2C     DS2V1C        IFEQ      DSCV1C
035100960523     C                   MOVEL     '1'           $ERR
035200960522     C                   SETON                                        66
035300960523     C                   MOVEL     MSG(13)       $MSG
035400960522     C                   EXSR      CARMSG
035500960523E   2C                   ENDIF
035600960523E   1C                   ENDIF
035700960523     C*
035800960523     C* NUOVA SCADENZA > VECCHIA DECORRENZA
035900960523IF  1C     $ERR          IFEQ      '0'
036000000822IF  2C     DS2V1C        IFLT      DDEV1C
036100960523     C                   MOVEL     '1'           $ERR
036200960523     C                   SETON                                        66
036300960523     C                   MOVEL     MSG(14)       $MSG
036400960523     C                   EXSR      CARMSG
036500960523E   2C                   ENDIF
036600960523E   1C                   ENDIF
036700960517     c*
036800960517     C                   ENDSR
036900960517     C*------------------------------------------------------------------------*
037000960517     C* CHKDAT - CONTROLLA LA DATA
037100960517     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
037200960517     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
037300960517     C*------------------------------------------------------------------------*
037400960517     C     CHKDAT        BEGSR
037500960517     C*
037600960517     C     *LIKE         DEFINE    V1CDDE        WDA8
037700960517     C     *LIKE         DEFINE    V1CDDE        DA8W
037800960517     C*
037900960517     C                   Z-ADD     *ZEROS        DA8W
038000960517     C*
038100960517     C                   Z-ADD     WDA8          G08DAT
038200960517     C                   Z-ADD     *ZEROS        G08INV
038300960517     C                   MOVEL     '0'           G08ERR
038400960517     C                   CALL      'XSRDA8'
038500960517     C                   PARM                    WLBDA8
038600960517     C                   Z-ADD     G08DAT        WDA8
038700960517IF  1C     G08ERR        IFEQ      '1'
038800960517     C                   MOVEL     '1'           $ERR
038900960517     C                   ELSE
039000960517     C                   Z-ADD     G08INV        DA8W
039100960517E   1C                   ENDIF
039200960517     C*
039300960517     C                   ENDSR
039400960517     C*------------------------------------------------------------------------*
039500960517     C* IMPVID - IMPOSTAZIONI VIDEO
039600960517     C*------------------------------------------------------------------------*
039700960517     C     IMPVID        BEGSR
039800960517     C*
039900960517     C                   MOVEL     'S'           V1CSIM                         *SIMULAZIONE LANCIO
040000960517     C*
040100960517     C                   ENDSR
040200960517     C*------------------------------------------------------------------------*
040300960517     C* CARTAB - CARICA LE TABELLE
040400960517     C*------------------------------------------------------------------------*
040500960517     C     CARTAB        BEGSR
040600960517     C                   ENDSR
040700960517     C*------------------------------------------------------------------------*
040800960517     C* *INZSR - ROUTINE INIZIALE
040900960517     C*------------------------------------------------------------------------*
041000960517     C     *INZSR        BEGSR
041100960517     C*--------------------
041200960517     C* RICEVIMENTO PARAMETRI
041300960517     C*--------------------
041400960517     C     *ENTRY        PLIST
041500960517     C                   PARM                    KPJBA
041600960517     C*--------------------
041700960517     C* VARIABILI  N O N  RIFERITE AL DATA BASE
041800960517     C*--------------------
041900960517     C     *LIKE         DEFINE    V1CDDE        DDEV1C                         *DEPOSITI
042000960517     C     *LIKE         DEFINE    V1CDSC        DSCV1C
042100960517     C     *LIKE         DEFINE    V1CDS2        DS2V1C
042200960517     C*--------------------
042300960517     C* VARIABILI DI CONTROLLO
042400960517     C*--------------------
042500960517     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
042600960517     C***
042700960517     C* IMPOSTAZIONI VIDEO
042800960517     C***
042900960517     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
043000960517     C*
043100960517     C* ESEGUE OPERAZIONI INIZIALI STANDARD
043200960517     C                   EXSR      INZSTD
043300960517     C*
043400960517     C* CARICA LE TABELLE
043500960517     C                   EXSR      CARTAB
043600960517     C*
043700960517     C* IMPOSTAZIONI VIDEO
043800960517     C                   EXSR      IMPVID
043900960517     C*
044000960517     C* EMETTE VIDEATA
044100960517     C                   EXSR      CARV1
044200960517     C*
044300960517     C                   ENDSR
044400960517     C*------------------------------------------------------------------------*
044500960517     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
044600960517     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
044700960517     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
044800960517     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
044900960517     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
045000960517     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
045100960517     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
045200960517     C*------------------------------------------------------------------------*
045300960517     C* INZSTD - ROUTINE INIZIALE STANDARD
045400960517     C*------------------------------------------------------------------------*
045500960517     C     INZSTD        BEGSR
045600960517     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
045700960517     C                   CALL      'XKEYS'
045800960517     C                   PARM                    DSKEYS
045900960517     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
046000960517     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
046100960517     C     XMSGPA        PLIST
046200960517     C                   PARM                    DSMSMS
046300960517     C                   PARM                    DSMSPG
046400960517     C                   PARM                    DSMSKE
046500960517     C                   PARM                    DSMSER
046600960517     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
046700960517     C                   MOVEL     'TNTL53R'     DSMSPG                         *NOME PGM CHIAMANTE
046800960517     C                   MOVEL     'TNTL53R'     VMCQUE                         *CODA MESSAGGI DESTI
046900960517     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
047000960517     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
047100960517     C* INIZIALIZZO FLAG CONTROLLO PGM
047200960517     C                   MOVEL     '0'           $ERR              1            *ERRORE
047300960517     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
047400960517     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
047500960517     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
047600960517     C                   MOVEL     '0'           $FINE             1            *FINE PGM
047700960517     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
047800960517     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
047900960517     C* LANCIO ROUTINE PER VERIFICARE SE
048000960517     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
048100960517     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
048200020716
048300020716     c     *dtaara       define    §azute        azuteds
048400020716     c     *dtaara       define    §datiute      ddatiute
048500020716
048600020716     c                   in(E)     *dtaara
048700020716     c                   if        %Error or rsut = *blanks
048800020716     c                   clear                   tibs34ds
048900020716     c                   call      'TIBS34R'
049000020716     c                   parm                    Tibs34Ds
049100020716     c                   in        *dtaara
049200020716     c                   endif
049300020717
049400020717      * Controllo il profilo utente
049500020717     c                   ExSr      CtrUte
049600020717
049700960517     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
049800960517     C                   BITOFF    '01234567'    POSMI1            1
049900960517     C                   BITOFF    '01234567'    POSMI2            1
050000960517     C                   BITOFF    '01234567'    POSMA1            1
050100960517     C                   BITOFF    '01234567'    POSMA2            1
050200960517     C                   BITON     '34'          POSMI1            1            *18
050300960517     C                   BITON     '7'           POSMI2            1            *01
050400960517     C                   BITON     '34'          POSMA1            1            *18
050500960517     C                   BITON     '0'           POSMA2            1            *80
050600960517     C                   MOVE      POSMI2        POSMIN            2
050700960517     C                   MOVEL     POSMI1        POSMIN
050800960517     C                   MOVE      POSMA2        POSMAX            2
050900960517     C                   MOVEL     POSMA1        POSMAX
051000960517     C                   ENDSR
051100020717      *------------------------------------------------------------------------*
051200020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
051300020717      *------------------------------------------------------------------------*
051400020717     c     CtrUte        BegSr
051500020717
051600020717     c                   Eval      *In76 = *Off
051700020717
051800020717      * Controllo se il profilo è ok da tibs34
051900020717     c                   If        DutErr = 'E'
052000020717     c                   eval      $Msg = DutMsg
052100020717     c                   ExSr      CarMsg
052200020717     c                   eval      *In76 = *On
052300020717     c                   endIf
052400020717
052500020717      * Controllo se il profilo di sede
052600020717     c                   If        DutLpo <> 'S'
052700020717     c                   Eval      $Msg = Msg(15)
052800020717     c                   ExSr      CarMsg
052900020717     c                   Eval      *In76 = *On
053000020717     c                   EndIf
053100020717
053200020717     c                   endSr
053300960517     C*------------------------------------------------------------------------*
053400960517     C* FUNSTD - LANCIA SUBROUTINE STANDARD
053500960517     C*------------------------------------------------------------------------*
053600960517     C     FUNSTD        BEGSR
053700960517     C                   SELECT
053800960517     C     KEY           WHENEQ    F3
053900960517     C                   EXSR      F03STD
054000960517     C     KEY           WHENEQ    F12
054100960517     C                   EXSR      F12STD
054200960517     C     KEY           WHENEQ    F24
054300960517     C                   EXSR      F24STD
054400960517     C                   OTHER
054500960517     C                   MOVEL     MSG(2)        $MSG
054600960517     C                   EXSR      CARMSG
054700960517     C                   ENDSL
054800960517     C                   ENDSR
054900960517     C*------------------------------------------------------------------------*
055000960517     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
055100960517     C*------------------------------------------------------------------------*
055200960517     C     F03STD        BEGSR
055300960517     C* IMPOSTO FLAG DI PREMUTO TASTO F03
055400960517     C                   MOVEL     '1'           $F03
055500960517     C* IMPOSTO FLAG DI FINE PROGRAMMA
055600960517     C                   MOVEL     '1'           $FINE
055700960517     C* PULISCE IL MESSAGGIO
055800960517     C                   ENDSR
055900960517     C*------------------------------------------------------------------------*
056000960517     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
056100960517     C*------------------------------------------------------------------------*
056200960517     C     F12STD        BEGSR
056300960517     C* IMPOSTO FLAG DI FINE PROGRAMMA
056400960517     C                   MOVEL     '1'           $FINE
056500960517     C                   ENDSR
056600960517     C*------------------------------------------------------------------------*
056700960517     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
056800960517     C*------------------------------------------------------------------------*
056900960517     C     F24STD        BEGSR
057000960517     C*
057100960517     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
057200960517     C                   EXSR      OUTFUN
057300960517     C*
057400960517     C                   ENDSR
057500960517     C*------------------------------------------------------------------------*
057600960517     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
057700960517     C*          CODA MESSAGGI DEL PGM
057800960517     C*------------------------------------------------------------------------*
057900960517     C     CARMSG        BEGSR
058000960517     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
058100960517     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
058200960517     C     MSGULT        IFEQ      0
058300960517     C                   MOVEL     '1'           *IN40
058400960517     C                   WRITE     TL53DVK
058500960517     C                   MOVEL     '0'           *IN40
058600960517     C                   ENDIF
058700960517     C* INVIA MESSAGGIO ALLA CODA DEL PGM
058800960517     C                   MOVEL     $MSG          DSMSMS
058900960517     C                   CALL      'XMSG'        XMSGPA
059000960517     C     DSMSER        IFEQ      0
059100960517     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
059200960517     C                   ENDIF
059300960517     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
059400960517     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
059500960517     C                   WRITE     TL53DVM                                      *SFL MESS
059600960517     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
059700960517     C                   MOVEL     *BLANKS       $MSG
059800960517     C                   ENDSR
059900960517     C*------------------------------------------------------------------------*
060000960517     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
060100960517     C*------------------------------------------------------------------------*
060200960517     C     OUTMSG        BEGSR
060300960517     C* VISUALIZZAZIONE MESSAGGIO
060400960517     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
060500960517     C                   WRITE     TL53DVK                                      *CONTR MESS
060600960517     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
060700960517     C                   Z-ADD     *ZEROS        MSGULT
060800960517     C                   ENDSR
060900960517     C*------------------------------------------------------------------------*
061000960517     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
061100960517     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
061200960517     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
061300960517     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
061400960517     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
061500960517     C*------------------------------------------------------------------------*
061600960517     C     CARFUN        BEGSR
061700960517     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
061800960517     C     1             DO        31            I                 5 0          --- 1 -->
061900960517     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
062000960517     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
062100960517     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
062200020716     c                   If        DutLpo = 'S'
062300960517     C     $TGES         IFEQ      'G'                                          *GESTIONE
062400960517     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
062500960517     C                   ELSE                                                   *INTERROG.
062600960517     C                   MOVEL     FUNASI        FAT(I)
062700960517     C                   ENDIF
062800960517     C                   ELSE                                                   <-- 2 --> *FILIALE
062900960517     C     $TGES         IFEQ      'G'                                          *GESTIONE
063000960517     C                   MOVEL     FUNAFG        FAT(I)
063100960517     C                   ELSE                                                   *INTERROG.
063200960517     C                   MOVEL     FUNAFI        FAT(I)
063300960517     C                   ENDIF
063400960517     C                   ENDIF                                                  <-- 2 ---
063500960517     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
063600960517     C                   MOVEL     USER          WEDP              3            *PROFILO
063700960517     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
063800960517     C                   MOVEL     'S'           FAT(I)                         *ABILITO
063900960517     C                   ELSE
064000960517     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
064100960517     C                   ENDIF
064200960517     C                   ENDIF
064300960517     C                   ENDDO                                                  <-- 1 ---
064400960517     C                   ENDSR
064500960517     C*------------------------------------------------------------------------*
064600960517     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
064700960517     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
064800960517     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
064900960517     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
065000960517     C*------------------------------------------------------------------------*
065100960517     C     OUTFUN        BEGSR
065200960517     C     1             DO        8             I                              *PULIZIA FUNZIONI A
065300960517     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
065400960517     C                   ENDDO
065500960517     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
065600960517     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
065700960517     C     1             DO        31            I
065800960517     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
065900960517     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
066000960517     C                   ADD       1             FUNNAT
066100960517     C                   ENDIF
066200960517     C                   ENDDO
066300960517     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
066400960517     C     FUNNAT        IFGT      8
066500960517     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
066600960517     C                   ENDIF
066700960517     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
066800960517     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
066900960517     C                   Z-ADD     8             FUNVIS            5 0
067000960517     C     FAT(23)       IFEQ      'S'
067100960517     C                   SUB       1             FUNVIS
067200960517     C                   ENDIF
067300960517     C     FAT(24)       IFEQ      'S'
067400960517     C                   SUB       1             FUNVIS
067500960517     C                   ENDIF
067600960517     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
067700960517     C                   Z-ADD     *ZEROS        W                 5 0
067800960517     C                   ADD       1             FUNULT
067900960517     C     FUNULT        IFGT      31
068000960517     C                   Z-ADD     1             FUNULT
068100960517     C                   ENDIF
068200960517     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
068300960517     C     FUNULT        DO        31            I                              --- 1 -->
068400960517     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
068500960517     C     W             IFLT      FUNVIS                                       --- 2 -->
068600960517     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
068700960517     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
068800960517     C     FDE(I)        ANDNE     *BLANKS
068900960517     C     I             IFEQ      23                                           *TOLGO F23
069000960517     C     I             OREQ      24                                           *E F24
069100960517     C                   ELSE
069200960517     C                   ADD       1             W
069300960517     C                   MOVEL     FDE(I)        FUV(W)
069400960517     C                   ENDIF
069500960517     C                   ENDIF                                                  <-- 3 ---
069600960517     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
069700960517     C                   Z-ADD     I             FUNULT
069800960517     C                   ENDIF                                                  <-- 2 ---
069900960517     C                   ENDDO                                                  <-- 1 ---
070000960517     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
070100960517     C                   MOVEL     FUV(1)        VFD001
070200960517     C                   MOVEL     FUV(2)        VFD002
070300960517     C                   MOVEL     FUV(3)        VFD003
070400960517     C                   MOVEL     FUV(4)        VFD004
070500960517     C                   MOVEL     FUV(5)        VFD005
070600960517     C                   MOVEL     FUV(6)        VFD006
070700960517     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
070800960517     C     FAT(24)       ANDEQ     'N'
070900960517     C                   MOVEL     FUV(7)        VFD007
071000960517     C                   MOVEL     FUV(8)        VFD008
071100960517     C                   ELSE                                                   <-- 1 -->
071200960517     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
071300960517     C     FAT(24)       ANDEQ     'S'
071400960517     C                   MOVEL     FDE(23)       VFD007
071500960517     C                   MOVEL     FDE(24)       VFD008
071600960517     C                   ELSE                                                   <-- 2 -->
071700960517     C                   MOVEL     FUV(7)        VFD007
071800960517     C     FAT(23)       IFEQ      'S'
071900960517     C                   MOVEL     FDE(23)       VFD008
072000960517     C                   ELSE
072100960517     C                   MOVEL     FDE(24)       VFD008
072200960517     C                   ENDIF
072300960517     C                   ENDIF                                                  <-- 2 ---
072400960517     C                   ENDIF                                                  <-- 1 ---
072500960517     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
072600960517     C                   ENDSR
072700960517     C*------------------------------------------------------------------------*
072800960517     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
072900960517     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
073000960517     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
073100960517     C*                   - ERRORE SE FUNZIONE NON ATTIVA
073200960517     C*------------------------------------------------------------------------*
073300960517     C     CONFUN        BEGSR
073400960517     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
073500960517     C     1             DO        31            I                              --- 1 -->
073600960517     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
073700960517     C* CONTROLLO SE IL TASTO E' ATTIVO
073800960517     C     FAT(I)        IFNE      'S'                                          --- 3 -->
073900960517     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
074000960517     C                   EXSR      CARMSG
074100960517     C                   MOVEL     '1'           $ERR
074200960517     C                   ENDIF                                                  <-- 3 ---
074300960517     C                   ENDIF                                                  <-- 2 ---
074400960517     C                   ENDDO                                                  <-- 1 ---
074500960517     C                   ENDSR
074600960517     O*------------------------------------------------------------------------*
074700960517** MSG - (MESSAGGI)
074800960517TNTL53R- Tasto funzionale non valido.                                          1
074900960517TNTL53R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
075000960517TNTL53R- Opzione non valida.                                                   3
075100960517TNTL53R- Opzione abilitata ma non gestita. Avvertire CED.                      4
075200960517TNTL53R- Inizio elenco.                                                        5
075300960517TNTL53R- Fine elenco.                                                          6
075400960517TNTL53R- Inserire il numero traino                                             7
075500960517TNTL53R- Il numero traino A deve essere maggiore o uguale al numero traino DA  8
075600960517TNTL53R- Data errata                                                           9
075700960517TNTL53R- Inserire la data decorrenza o la data scadenza o tutte e due         10
075800960517TNTL53R- 'S' per stampare l'elenco traini da modificare, 'N' per aggiornare   11
075900960522TNTL53R- Inserire la nuova data scadenza                                      12
076000960522TNTL53R- Inserire la nuova data scadenza diversa da quella esistente          13
076100000822TNTL53R- La nuova data scadenza deve essere maggiore o uguale della decorrenza14
076200020717TNTL53R- Profilo non abilitato alla funzione                                  15
076300960517TNTL53R- 16
076400960517TNTL53R- 17
076500960517TNTL53R- 18
076600960517TNTL53R- 19
076700960517TNTL53R- Elaborazione lanciata ...                                            20
076800960517** CMD  (COMANDI)
076900960517WRKSPLF
077000960517
077100960517
077200960517
077300960517
077400960517** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
077500960517F1    -                   -    NN     NN
077600960517F2    -                   -    NN     NN
077700960517F3    -F3=Fine            -    SS     SS
077800960517F4    -                   -    NN     NN
077900960517F5    -                   -    NN     NN
078000960517F6    -F6=Conferma        -    SS     SS
078100960517F7    -                   -    NN     NN
078200960517F8    -                   -    NN     NN
078300960517F9    -                   -    NN     NN
078400960517F10   -                   -    NN     NN
078500960517F11   -                   -    NN     NN
078600960517F12   -F12=Ritorno        -    SS     SS
078700960517F13   -                   -    NN     NN
078800960517F14   -                   -    NN     NN
078900960517F15   -                   -    NN     NN
079000960517F16   -                   -    NN     NN
079100960517F17   -                   -    NN     NN
079200960517F18   -                   -    NN     NN
079300960517F19   -                   -    NN     NN
079400960517F20   -                   -    NN     NN
079500960517F21   -                   -    NN     NN
079600960517F22   -F22=Gestione stampe-    EE     EE
079700960517F23   -                   -    NN     NN
079800960517F24   -F24=Altri tasti    -    NN     NN
079900960517ENTER -                   -    SS     SS
080000960517ROLLUP-                   -    NN     NN
080100960517ROLLDN-                   -    NN     NN
080200960517HELP  -                   -    NN     NN
080300960517PRINT -                   -    NN     NN
080400960517BSPACE-                   -    NN     NN
080500960517CLEAR -                   -    NN     NN

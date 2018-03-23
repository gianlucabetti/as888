000100020716     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - DUPLICA UN TRAINO (E TUTTI GLI ARCHIVI CORRELATI)    *
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL21D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL21DVM:MSGULT)
001000000000     FTNTLT01L  UF A E           K DISK
001100000000     FTNTLR01L  UF A E           K DISK
001200000000     FTNTLO01L  UF A E           K DISK
001300000000     FTNTLM01L  UF A E           K DISK
001400010725     FTITLL01L  UF A E           K DISK
001500000000     FTNTLN01L  UF A E           K DISK
001600110510     FTNTLS01L  UF A E           K DISK
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D*-------------------
002100000000     D* MESSAGGI E FUNZIONI
002200000000     D*-------------------
002300000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002400000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002500000000     D*-------------------
002600000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002700000000     D*-------------------
002800000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002900000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003000000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003100000000     D*-------------------
003200000000     D* TABELLA DESCRIZIONI A VIDEO
003300000000     D*-------------------
003400000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003500000000     D*-------------------
003600000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003700000000     D*-------------------
003800000000     D KEYS            S              1    DIM(40)
003900000000     D*------------------------------------------------------------------------*
004000000000     D* INPUT
004100000000     D*------------------------------------------------------------------------*
004200000000     D*-------------------
004300000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004400000000     D*-------------------
004500000000     D DSFUN         E DS
004600000000     D  FUNRIG                 1     40
004700000000     D*-------------------
004800000000     D* DS PER GESTIONE TASTI FUNZIONALI
004900000000     D*-------------------
005000000000     D DSKEYS        E DS
005100000000     D  KEYRIG                 1     40
005200000000     D*-------------------
005300000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005400000000     D*-------------------
005500000000     D                SDS
005600000000     D  USER                 254    263
005700000000     D*-------------------
005800000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005900000000     D*-------------------
006000000000     D DEVDS1          DS
006100000000     D  KEY                  369    369
006200000000     D  CURSOR               370    371
006300000000     D*---------------------
006400000000     D* ARCHITETTURA
006500000000     D*---------------------
006600000000     D KPJBA         E DS
006700000000     D*---------------------
006800000000     D* DS ESTERNA DI PROCEDURA
006900000000     D*---------------------
007000000000     D DS00          E DS                  EXTNAME(TNTL00DS)
007100000000     D*-------------------
007200000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007300000000     D*-------------------
007400000000     D DSMSG         E DS
007500020716
007600020716     d DdatiUte      e ds
007700020716     d AzuteDs       e ds                  ExtName(Azute00f)
007800020716     d Tibs34Ds      e ds                  Inz
007900020716
008000000000     D*------------------
008100000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
008200000000     D*------------------
008300000000     D WLBDA8          DS
008400000000     D  G08DAT                 1      8  0
008500000000     D  G08INV                 9     16  0
008600000000     D  G08ERR                17     17
008700000000     D  G08TGI                18     22  0
008800000000     C*------------------------------------------------------------------------*
008900000000     C* MAIN LINE
009000000000     C*------------------------------------------------------------------------*
009100000000     C*
009200000000     C                   WRITE     TL21DVT                                      *TESTATA
009300000000DO  1C     $FINE         DOUEQ     '1'
009400000000     C*
009500000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009600000000IF  2C     $GES          IFEQ      'V1'
009700000000     C                   EXSR      GESV1
009800000000E   2C                   ENDIF
009900000000     C*
010000000000E   1C                   ENDDO
010100000000     C*
010200000000IF  1C     $F03          IFEQ      '1'
010300000000     C                   MOVEL     '1'           D00F03
010400000000E   1C                   ENDIF
010500000000     C* FINE PGM
010600000000     C                   MOVEL     DS00          KPJBU
010700000000     C                   SETON                                        LR
010800000000     C*------------------------------------------------------------------------*
010900000000     C* GESV1 - GESTIONE VIDEO UNO
011000000000     C*------------------------------------------------------------------------*
011100000000     C     GESV1         BEGSR
011200000000     C*
011300000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
011400000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011500000000E   1C                   ENDIF
011600000000     C*
011700000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011800000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011900000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012000000000E   1C                   ENDIF
012100000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012200000000     C*
012300000000     C                   EXFMT     TL21DV1                                      *PARAMETRI
012400000000     C*
012500000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012600000000     C*
012700000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012800000000SL  2C                   SELECT
012900000000     C     KEY           WHENEQ    ENTER
013000000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013100000000     C     KEY           WHENEQ    F6
013200000000     C                   EXSR      F06GES
013300000000     C                   OTHER
013400000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013500000000E   2C                   ENDSL
013600000000E   1C                   ENDIF
013700000000     C*
013800000000     C                   ENDSR
013900000000     C*------------------------------------------------------------------------*
014000000000     C* CARV1 - CARICA VIDEO UNO
014100000000     C*------------------------------------------------------------------------*
014200000000     C     CARV1         BEGSR
014300000000     C*
014400000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014500000000     C*
014600000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014700000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014800000000     C*
014900000000     C*---                 WRITETL21DVT                    *TESTATA
015000000000     C*---                 WRITETL21DVF                    *FUNZIONI
015100000000     C*
015200000000     C                   ENDSR
015300000000     C*------------------------------------------------------------------------*
015400000000     C* ENTV1 - GESTIONE TASTO ENTER
015500000000     C*------------------------------------------------------------------------*
015600000000     C     ENTV1         BEGSR
015700000000     C*
015800000000     C* CONTROLLA I CAMPI IMMESSI
015900000000     C                   EXSR      DECODI
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
016400000000     C*------------------------------------------------------------------------*
016500000000     C     F06GES        BEGSR
016600000000     C*
016700000000     C* CONTROLLA I CAMPI IMMESSI
016800000000     C                   EXSR      DECODI
016900000000     C*
017000000000     C* RILEGGE IL TRAINO DA CUI COPIARE E DUPLICA I DATI
017100000000IF  1C     $ERR          IFEQ      '0'
017200000000     C                   Z-ADD     D00TRN        KLTTRN                         *TRAINO
017300000000     C                   Z-ADD     D00DDE        KLTDDE                         *DECORRENZA
017400000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
017500000000IF  2C     *IN99         IFEQ      *OFF
017600000000     C*--------------------
017700000000     C* F E R M A T E
017800000000     C*--------------------
017900000000     C                   Z-ADD     D00TRN        KLRTRN                         *TRAINO
018000000000     C                   Z-ADD     D00DDE        KLRDDE                         *DECORRENZA
018100000000     C     KEYTLR        SETLL     TNTLR01L
018200000000     C     KEYTLR        READE(N)  TNTLR01L                               99
018300000000DO  3C     *IN99         DOWEQ     *OFF
018400000000IF  4C     TLRATB        IFEQ      *BLANKS                                      *NO ANNULLATI
018500000000     C                   Z-ADD     V1CTR2        TLRTRN                         *NUMERO TRAINO
018600000000     C                   Z-ADD     DD2V1C        TLRDDE                         *DATA DECORRENZA
018700000000     C                   Z-ADD     DATCOR        TLRDIR                         *DATA INSERIMENTO
018800000000     C                   Z-ADD     *ZEROS        TLRDUV                         *DATA VARIAZIONE
018900000000     C                   MOVEL     *BLANKS       TLRFTR                         *FLAG TRASMISSIONE
019000000000     C                   Z-ADD     *ZEROS        TLRDTR                         *DATA TRASMISSIONE
019100000000     C                   WRITE     TNTLR000
019200000000E   4C                   ENDIF
019300000000     C     KEYTLR        READE(N)  TNTLR01L                               99
019400000000E   3C                   ENDDO
019500000000     C*--------------------
019600000000     C* O R A R I
019700000000     C*--------------------
019800000000     C                   Z-ADD     D00TRN        KLOTRN                         *TRAINO
019900000000     C                   Z-ADD     D00DDE        KLODDE                         *DECORRENZA
020000000000     C     KEYTLO        SETLL     TNTLO01L
020100000000     C     KEYTLO        READE(N)  TNTLO01L                               99
020200000000DO  3C     *IN99         DOWEQ     *OFF
020300000000IF  4C     TLOATB        IFEQ      *BLANKS                                      *NO ANNULLATI
020400000000     C                   Z-ADD     V1CTR2        TLOTRN                         *NUMERO TRAINO
020500000000     C                   Z-ADD     DD2V1C        TLODDE                         *DATA DECORRENZA
020600000000     C                   Z-ADD     DATCOR        TLODIR                         *DATA INSERIMENTO
020700000000     C                   Z-ADD     *ZEROS        TLODUV                         *DATA VARIAZIONE
020800000000     C                   MOVEL     *BLANKS       TLOFTR                         *FLAG TRASMISSIONE
020900000000     C                   Z-ADD     *ZEROS        TLODTR                         *DATA TRASMISSIONE
021000000000     C                   WRITE     TNTLO000
021100000000E   4C                   ENDIF
021200000000     C     KEYTLO        READE(N)  TNTLO01L                               99
021300000000E   3C                   ENDDO
021400000000     C*--------------------
021500000000     C* M E T R I C U B I
021600000000     C*--------------------
021700000000     C                   Z-ADD     D00TRN        KLMTRN                         *TRAINO
021800000000     C                   Z-ADD     D00DDE        KLMDDE                         *DECORRENZA
021900000000     C     KEYTLM        SETLL     TNTLM01L
022000000000     C     KEYTLM        READE(N)  TNTLM01L                               99
022100000000DO  3C     *IN99         DOWEQ     *OFF
022200000000IF  4C     TLMATB        IFEQ      *BLANKS                                      *NO ANNULLATI
022300000000     C                   Z-ADD     V1CTR2        TLMTRN                         *NUMERO TRAINO
022400000000     C                   Z-ADD     DD2V1C        TLMDDE                         *DATA DECORRENZA
022500000000     C                   Z-ADD     DATCOR        TLMDIR                         *DATA INSERIMENTO
022600000000     C                   Z-ADD     *ZEROS        TLMDUV                         *DATA VARIAZIONE
022700000000     C                   MOVEL     *BLANKS       TLMFTR                         *FLAG TRASMISSIONE
022800000000     C                   Z-ADD     *ZEROS        TLMDTR                         *DATA TRASMISSIONE
022900000000     C                   WRITE     TNTLM000
023000000000E   4C                   ENDIF
023100000000     C     KEYTLM        READE(N)  TNTLM01L                               99
023200000000E   3C                   ENDDO
023300000000     C*--------------------
023400000000     C* L I S T I N I
023500000000     C*--------------------
023600000000     C                   Z-ADD     D00TRN        KLLTRN                         *TRAINO
023700000000     C                   Z-ADD     D00DDE        KLLDDE                         *DECORRENZA
023800010725     C     KEYTLL        SETLL     TITLL01L
023900010725     C     KEYTLL        READE(N)  TITLL01L                               99
024000000000DO  3C     *IN99         DOWEQ     *OFF
024100000000IF  4C     TLLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
024200000000     C                   Z-ADD     V1CTR2        TLLTRN                         *NUMERO TRAINO
024300000000     C                   Z-ADD     DD2V1C        TLLDDE                         *DATA DECORRENZA
024400000000     C                   Z-ADD     DATCOR        TLLDIR                         *DATA INSERIMENTO
024500000000     C                   Z-ADD     *ZEROS        TLLDUV                         *DATA VARIAZIONE
024600000000     C                   MOVEL     *BLANKS       TLLFTR                         *FLAG TRASMISSIONE
024700000000     C                   Z-ADD     *ZEROS        TLLDTR                         *DATA TRASMISSIONE
024800010725     C                   WRITE     TITLL000
024900000000E   4C                   ENDIF
025000010725     C     KEYTLL        READE(N)  TITLL01L                               99
025100000000E   3C                   ENDDO
025200000000     C*--------------------
025300000000     C* N O T E
025400000000     C*--------------------
025500000000     C                   Z-ADD     D00TRN        KLNTRN                         *TRAINO
025600000000     C                   Z-ADD     D00DDE        KLNDDE                         *DECORRENZA
025700000000     C     KEYTLN        SETLL     TNTLN01L
025800000000     C     KEYTLN        READE(N)  TNTLN01L                               99
025900000000DO  3C     *IN99         DOWEQ     *OFF
026000000000IF  4C     TLNATB        IFEQ      *BLANKS                                      *NO ANNULLATI
026100000000     C                   Z-ADD     V1CTR2        TLNTRN                         *NUMERO TRAINO
026200000000     C                   Z-ADD     DD2V1C        TLNDDE                         *DATA DECORRENZA
026300000000     C                   Z-ADD     DATCOR        TLNDIR                         *DATA INSERIMENTO
026400000000     C                   Z-ADD     *ZEROS        TLNDUV                         *DATA VARIAZIONE
026500000000     C                   MOVEL     *BLANKS       TLNFTR                         *FLAG TRASMISSIONE
026600000000     C                   Z-ADD     *ZEROS        TLNDTR                         *DATA TRASMISSIONE
026700000000     C                   WRITE     TNTLN000
026800000000E   4C                   ENDIF
026900000000     C     KEYTLN        READE(N)  TNTLN01L                               99
027000000000E   3C                   ENDDO
027100110510     C*--------------------
027200110510     C* PUNTI  DI  SCAMBIO
027300110510     C*--------------------
027400110510     C                   Z-ADD     D00TRN        KLNTRN                         *TRAINO
027500110510     C                   Z-ADD     D00DDE        KLNDDE                         *DECORRENZA
027600110510     C     KEYTLN        SETLL     TNTLS01L
027700110510     C     KEYTLN        READE(N)  TNTLS01L                               99
027800110510DO  3C     *IN99         DOWEQ     *OFF
027900110510IF  4C     TLNATB        IFEQ      *BLANKS                                      *NO ANNULLATI
028000110510     C                   Z-ADD     V1CTR2        TLSTRN                         *NUMERO TRAINO
028100110510     C                   Z-ADD     DD2V1C        TLSDDE                         *DATA DECORRENZA
028200110510     C                   Z-ADD     DATCOR        TLSDIR                         *DATA INSERIMENTO
028300110510     C                   Z-ADD     *ZEROS        TLSDUV                         *DATA VARIAZIONE
028400110510     C                   MOVEL     *BLANKS       TLSFTR                         *FLAG TRASMISSIONE
028500110510     C                   Z-ADD     *ZEROS        TLSDTR                         *DATA TRASMISSIONE
028600110510     C                   WRITE     TNTLS000
028700110510E   4C                   ENDIF
028800110510     C     KEYTLN        READE(N)  TNTLS01L                               99
028900110510E   3C                   ENDDO
029000000000     C*--------------------
029100000000     C* T R A I N O
029200000000     C*--------------------
029300000000     C                   Z-ADD     V1CTR2        TLTTRN                         *NUMERO TRAINO
029400000000     C                   Z-ADD     DD2V1C        TLTDDE                         *DATA DECORRENZA
029500000000     C                   Z-ADD     DS2V1C        TLTDSC                         *DATA SCADENZA
029600000000     C                   Z-ADD     DATCOR        TLTDIR                         *DATA INSERIMENTO
029700000000     C                   Z-ADD     *ZEROS        TLTDUV                         *DATA VARIAZIONE
029800000000     C                   MOVEL     *BLANKS       TLTFTR                         *FLAG TRASMISSIONE
029900000000     C                   Z-ADD     *ZEROS        TLTDTR                         *DATA TRASMISSIONE
030000000000     C                   WRITE     TNTLT000
030100000000     C*
030200000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
030300000000     C                   MOVEL     MSG(20)       D00MSG
030400000000     C                   MOVEL     '1'           $FINE
030500000000E   2C                   ENDIF
030600000000E   1C                   ENDIF
030700000000     C*
030800000000     C                   ENDSR
030900000000     C*------------------------------------------------------------------------*
031000000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
031100000000     C*------------------------------------------------------------------------*
031200000000     C     DECODI        BEGSR
031300000000     C*
031400000000     C* REIMPOSTA INDICATORI DI ERRORE
031500000000     C                   SETOFF                                       606162
031600000000     C*--------------------
031700000000     C* TRAINO
031800000000     C*--------------------
031900000000IF  1C     $ERR          IFEQ      '0'
032000000000IF  2C     V1CTR2        IFEQ      *ZEROS
032100000000     C                   MOVEL     '1'           $ERR
032200000000     C                   MOVEL     MSG(7)        $MSG
032300000000     C                   EXSR      CARMSG
032400000000     C                   SETON                                        60
032500960424X   2C                   ELSE
032600960424IF  3C     V1CTR2        IFGT      9000000
032700960424     C                   MOVEL     '1'           $ERR
032800960424     C                   MOVEL     MSG(12)       $MSG
032900960424     C                   EXSR      CARMSG
033000960424     C                   SETON                                        60
033100960424E   3C                   ENDIF
033200000000E   2C                   ENDIF
033300000000E   1C                   ENDIF
033400000000     C*--------------------
033500000000     C* DECORRENZA
033600000000     C*--------------------
033700000000IF  1C     $ERR          IFEQ      '0'
033800000000     C                   Z-ADD     V1CDD2        G08DAT
033900000000     C                   Z-ADD     *ZEROS        G08INV
034000000000     C                   MOVEL     '0'           G08ERR
034100000000     C                   CALL      'XSRDA8'
034200000000     C                   PARM                    WLBDA8
034300000000     C                   Z-ADD     G08DAT        V1CDD2
034400000000     C                   Z-ADD     G08INV        DD2V1C
034500000000IF  2C     G08ERR        IFEQ      '1'
034600000000     C                   MOVEL     '1'           $ERR
034700000000     C                   MOVEL     MSG(8)        $MSG
034800000000     C                   EXSR      CARMSG
034900000000     C                   SETON                                        61
035000000000E   2C                   ENDIF
035100000000E   1C                   ENDIF
035200000000     C*--------------------
035300000000     C* SCADENZA
035400000000     C*--------------------
035500000000     C* CORRETTA
035600000000IF  1C     $ERR          IFEQ      '0'
035700000000     C                   Z-ADD     V1CDS2        G08DAT
035800000000     C                   Z-ADD     *ZEROS        G08INV
035900000000     C                   MOVEL     '0'           G08ERR
036000000000     C                   CALL      'XSRDA8'
036100000000     C                   PARM                    WLBDA8
036200000000     C                   Z-ADD     G08DAT        V1CDS2
036300000000     C                   Z-ADD     G08INV        DS2V1C
036400000000IF  2C     G08ERR        IFEQ      '1'
036500000000     C                   MOVEL     '1'           $ERR
036600000000     C                   MOVEL     MSG(8)        $MSG
036700000000     C                   EXSR      CARMSG
036800000000     C                   SETON                                        62
036900000000E   2C                   ENDIF
037000000000E   1C                   ENDIF
037100000000     C* > DELLA DECORRENZA
037200000000IF  1C     $ERR          IFEQ      '0'
037300000000IF  2C     DS2V1C        IFLT      DD2V1C
037400000000     C                   MOVEL     '1'           $ERR
037500000000     C                   MOVEL     MSG(9)        $MSG
037600000000     C                   EXSR      CARMSG
037700000000     C                   SETON                                        62
037800000000E   2C                   ENDIF
037900000000E   1C                   ENDIF
038000970110     C* >= DATA CORRENTE
038100000000IF  1C     $ERR          IFEQ      '0'
038200970110IF  2C     DS2V1C        IFLT      DATCOR
038300000000     C                   MOVEL     '1'           $ERR
038400000000     C                   MOVEL     MSG(10)       $MSG
038500000000     C                   EXSR      CARMSG
038600000000     C                   SETON                                        62
038700000000E   2C                   ENDIF
038800000000E   1C                   ENDIF
038900000000     C*--------------------
039000000000     C* IL NUOVO TRAINO NON SI DEVE SOVRAPPORRE A UNO GIA' IN DECORRENZA
039100000000     C*--------------------
039200000000IF  1C     $ERR          IFEQ      '0'
039300000000     C                   Z-ADD     V1CTR2        KLTTRN
039400000000     C     KLTTRN        SETLL     TNTLT01L
039500000000     C     KLTTRN        READE(N)  TNTLT01L                               99
039600000000DO  2C     *IN99         DOWEQ     *OFF
039700980608IF  3C     DD2V1C        IFGE      TLTDDE                                          *DECORRENZA
039800960320     C     DD2V1C        ANDLE     TLTDSC
039900960320     C     DS2V1C        ORGE      TLTDDE                                          *SCADENZA
040000960320     C     DS2V1C        ANDLE     TLTDSC
040100960321     C     TLTDDE        ORGE      DD2V1C
040200960321     C     TLTDSC        ANDLE     DS2V1C
040300000000     C                   MOVEL     '1'           $ERR
040400000000     C                   LEAVE
040500980608E   3C                   ENDIF
040600000000     C     KLTTRN        READE(N)  TNTLT01L                               99
040700000000E   2C                   ENDDO
040800000000IF  2C     $ERR          IFEQ      '1'
040900960118     C                   MOVEL     MSG(11)       $MSG
041000000000     C                   EXSR      CARMSG
041100000000     C                   SETON                                        606162
041200000000E   2C                   ENDIF
041300000000E   1C                   ENDIF
041400000000     C*
041500000000     C                   ENDSR
041600000000     C*------------------------------------------------------------------------*
041700000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
041800000000     C*------------------------------------------------------------------------*
041900000000     C     IMPVID        BEGSR
042000000000     C*
042100000000     C* CALCOLA LA DATA CORRENTE
042200000000     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
042300000000     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
042400000000     C                   Z-ADD     WNUM8         G08DAT
042500000000     C                   Z-ADD     *ZEROS        G08INV
042600000000     C                   MOVEL     '0'           G08ERR
042700000000     C                   CALL      'XSRDA8'
042800000000     C                   PARM                    WLBDA8
042900000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
043000000000     C*
043100000000     C* LEGGE IL TRAINO
043200000000     C                   Z-ADD     D00TRN        KLTTRN                         *TRAINO
043300000000     C                   Z-ADD     D00DDE        KLTDDE                         *DECORRENZA
043400000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
043500000000IF  1C     *IN99         IFEQ      *OFF
043600000000     C* DATI DA COPIARE
043700000000     C                   Z-ADD     TLTTRN        V1CTRN                         *TRAINO
043800000000     C                   Z-ADD     TLTDDE        G08INV                         *DECORRENZA
043900000000     C                   Z-ADD     *ZEROS        G08DAT
044000000000     C                   MOVEL     '3'           G08ERR
044100000000     C                   CALL      'XSRDA8'
044200000000     C                   PARM                    WLBDA8
044300000000     C                   Z-ADD     G08DAT        V1CDDE
044400000000     C                   Z-ADD     TLTDSC        G08INV                         *SCADENZA
044500000000     C                   Z-ADD     *ZEROS        G08DAT
044600000000     C                   MOVEL     '3'           G08ERR
044700000000     C                   CALL      'XSRDA8'
044800000000     C                   PARM                    WLBDA8
044900000000     C                   Z-ADD     G08DAT        V1CDSC
045000000000     C* DATI DA COPIARE
045100000000     C                   Z-ADD     *ZEROS        V1CTR2                         *TRAINO
045200000000     C                   Z-ADD     *ZEROS        V1CDD2                         *DECORRENZA
045300000000     C                   Z-ADD     *ZEROS        V1CDS2                         *SCADENZA
045400000000E   1C                   ENDIF
045500000000     C*
045600000000     C                   ENDSR
045700000000     C*------------------------------------------------------------------------*
045800000000     C* *INZSR  - ROUTINE INIZIALE
045900000000     C*------------------------------------------------------------------------*
046000000000     C     *INZSR        BEGSR
046100000000     C*--------------------
046200000000     C* RICEVIMENTO PARAMETRI
046300000000     C*--------------------
046400000000     C     *ENTRY        PLIST
046500000000     C                   PARM                    KPJBA
046600000000     C                   MOVEL     KPJBU         DS00
046700000000     C*--------------------
046800000000     C* VARIABILI RIFERITE AL DATA BASE
046900000000     C*--------------------
047000000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
047100000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
047200000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
047300000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
047400000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
047500000000     C     *LIKE         DEFINE    TLODDE        KLODDE
047600000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
047700000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
047800010725     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
047900000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
048000000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
048100000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
048200000000     C*--------------------
048300000000     C* VARIABILI NON RIFERITE AL DATA BASE
048400000000     C*--------------------
048500000000     C     *LIKE         DEFINE    V1CDD2        DD2V1C                         *DEPOSITI
048600000000     C     *LIKE         DEFINE    V1CDS2        DS2V1C
048700000000     C*--------------------
048800000000     C* CHIAVI DI LETTURA
048900000000     C*--------------------
049000000000     C* LETTURA TNTLT01L - COMPLETA
049100000000     C     KEYTLT        KLIST
049200000000     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
049300000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
049400000000     C* LETTURA TNTLR01L - PARZIALE
049500000000     C     KEYTLR        KLIST
049600000000     C                   KFLD                    KLRTRN                         *NUMERO TRAINO
049700000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
049800000000     C* LETTURA TNTLO01L - PARZIALE
049900000000     C     KEYTLO        KLIST
050000000000     C                   KFLD                    KLOTRN                         *NUMERO TRAINO
050100000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
050200000000     C* LETTURA TNTLM01L - PARZIALE
050300000000     C     KEYTLM        KLIST
050400000000     C                   KFLD                    KLMTRN                         *NUMERO TRAINO
050500000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
050600010725     C* LETTURA TITLL01L - PARZIALE
050700000000     C     KEYTLL        KLIST
050800000000     C                   KFLD                    KLLTRN                         *NUMERO TRAINO
050900000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
051000000000     C* LETTURA TNTLN01L - PARZIALE
051100000000     C     KEYTLN        KLIST
051200000000     C                   KFLD                    KLNTRN                         *NUMERO TRAINO
051300000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
051400000000     C***
051500000000     C* IMPOSTAZIONI VIDEO
051600000000     C***
051700000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
051800000000     C                   MOVE      D00OP0        INTGES            2
051900000000IF  1C     INTGES        IFEQ      '02'
052000000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
052100000000X   1C                   ELSE
052200000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
052300000000E   1C                   ENDIF
052400000000     C*
052500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
052600000000     C                   EXSR      INZSTD
052700000000     C*
052800000000     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
052900000000     C                   EXSR      IMPVID
053000000000     C*
053100000000     C* EMETTO PRIMO FORMATO VIDEO
053200000000     C                   EXSR      CARV1
053300000000     C*
053400000000     C                   ENDSR
053500000000     C*------------------------------------------------------------------------*
053600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053800000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
053900000000     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
054000000000     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
054100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
054200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
054300000000     C*------------------------------------------------------------------------*
054400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
054500000000     C*------------------------------------------------------------------------*
054600000000     C     INZSTD        BEGSR
054700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
054800000000     C                   CALL      'XKEYS'
054900000000     C                   PARM                    DSKEYS
055000000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
055100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
055200000000     C     XMSGPA        PLIST
055300000000     C                   PARM                    DSMSMS
055400000000     C                   PARM                    DSMSPG
055500000000     C                   PARM                    DSMSKE
055600000000     C                   PARM                    DSMSER
055700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
055800000000     C                   MOVEL     'TNTL21R'     DSMSPG                         *NOME PGM CHIAMANTE
055900000000     C                   MOVEL     'TNTL21R'     VMCQUE                         *CODA MESSAGGI DESTI
056000000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
056100000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
056200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
056300000000     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
056400000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
056500000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
056600000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
056700000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
056800000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
056900000000     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
057000000000     C* LANCIO ROUTINE PER VERIFICARE SE
057100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
057200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
057300020716
057400020716     c     *dtaara       define    §azute        azuteds
057500020716     c     *dtaara       define    §datiute      ddatiute
057600020716
057700020716     c                   in(E)     *dtaara
057800020716     c                   if        %Error or rsut = *blanks
057900020716     c                   clear                   tibs34ds
058000020716     c                   call      'TIBS34R'
058100020716     c                   parm                    Tibs34Ds
058200020716     c                   in        *dtaara
058300020716     c                   endif
058400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
058500000000     C                   BITOFF    '01234567'    POSMI1            1
058600000000     C                   BITOFF    '01234567'    POSMI2            1
058700000000     C                   BITOFF    '01234567'    POSMA1            1
058800000000     C                   BITOFF    '01234567'    POSMA2            1
058900000000     C                   BITON     '34'          POSMI1            1            *18
059000000000     C                   BITON     '7'           POSMI2            1            *01
059100000000     C                   BITON     '34'          POSMA1            1            *18
059200000000     C                   BITON     '0'           POSMA2            1            *80
059300000000     C                   MOVE      POSMI2        POSMIN            2
059400000000     C                   MOVEL     POSMI1        POSMIN
059500000000     C                   MOVE      POSMA2        POSMAX            2
059600000000     C                   MOVEL     POSMA1        POSMAX
059700000000     C                   ENDSR
059800000000     C*------------------------------------------------------------------------*
059900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
060000000000     C*------------------------------------------------------------------------*
060100000000     C     FUNSTD        BEGSR
060200000000     C                   SELECT
060300000000     C     KEY           WHENEQ    F3
060400000000     C                   EXSR      F03STD
060500000000     C     KEY           WHENEQ    F12
060600000000     C                   EXSR      F12STD
060700000000     C     KEY           WHENEQ    F24
060800000000     C                   EXSR      F24STD
060900000000     C                   OTHER
061000000000     C                   MOVEL     MSG(2)        $MSG
061100000000     C                   EXSR      CARMSG
061200000000     C                   ENDSL
061300000000     C                   ENDSR
061400000000     C*------------------------------------------------------------------------*
061500000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
061600000000     C*------------------------------------------------------------------------*
061700000000     C     F03STD        BEGSR
061800000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
061900000000     C                   MOVEL     '1'           $FINE
062000951212     C* IMPOSTA FLAG DI FINE PROCEDURA
062100951212     C                   MOVEL     '1'           $F03
062200951212     C* PULISCE IL MESSAGGIO
062300951212     C                   MOVEL     *BLANKS       D00MSG
062400000000     C                   ENDSR
062500000000     C*------------------------------------------------------------------------*
062600000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
062700000000     C*------------------------------------------------------------------------*
062800000000     C     F12STD        BEGSR
062900000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
063000000000     C                   MOVEL     '1'           $FINE
063100951212     C* IMPOSTA FLAG DI FINE PROCEDURA
063200951212     C                   MOVEL     '1'           $F12
063300951212     C* PULISCE IL MESSAGGIO
063400951212     C                   MOVEL     *BLANKS       D00MSG
063500000000     C                   ENDSR
063600000000     C*------------------------------------------------------------------------*
063700000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
063800000000     C*------------------------------------------------------------------------*
063900000000     C     F24STD        BEGSR
064000000000     C*
064100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
064200000000     C                   EXSR      OUTFUN
064300000000     C*
064400000000     C                   ENDSR
064500000000     C*------------------------------------------------------------------------*
064600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
064700000000     C*          CODA MESSAGGI DEL PGM
064800000000     C*------------------------------------------------------------------------*
064900000000     C     CARMSG        BEGSR
065000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
065100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
065200000000     C     MSGULT        IFEQ      0
065300000000     C                   MOVEL     '1'           *IN40
065400000000     C                   WRITE     TL21DVK
065500000000     C                   MOVEL     '0'           *IN40
065600000000     C                   ENDIF
065700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
065800000000     C                   MOVEL     $MSG          DSMSMS
065900000000     C                   CALL      'XMSG'        XMSGPA
066000000000     C     DSMSER        IFEQ      0
066100000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
066200000000     C                   ENDIF
066300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
066400000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
066500000000     C                   WRITE     TL21DVM                                      *SFL MESS
066600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
066700000000     C                   MOVEL     *BLANKS       $MSG
066800000000     C                   ENDSR
066900000000     C*------------------------------------------------------------------------*
067000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
067100000000     C*------------------------------------------------------------------------*
067200000000     C     OUTMSG        BEGSR
067300000000     C* VISUALIZZAZIONE MESSAGGIO
067400000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
067500000000     C                   WRITE     TL21DVK                                      *CONTR MESS
067600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
067700000000     C                   Z-ADD     *ZEROS        MSGULT
067800000000     C                   ENDSR
067900000000     C*------------------------------------------------------------------------*
068000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
068100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
068200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
068300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
068400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
068500000000     C*------------------------------------------------------------------------*
068600000000     C     CARFUN        BEGSR
068700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
068800000000     C     1             DO        31            I                 5 0          --- 1 -->
068900000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
069000000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
069100000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
069200020716     c                   If        DutLpo = 'S'
069300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
069400000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
069500000000     C                   ELSE                                                   *INTERROG.
069600000000     C                   MOVEL     FUNASI        FAT(I)
069700000000     C                   ENDIF
069800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
069900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
070000000000     C                   MOVEL     FUNAFG        FAT(I)
070100000000     C                   ELSE                                                   *INTERROG.
070200000000     C                   MOVEL     FUNAFI        FAT(I)
070300000000     C                   ENDIF
070400000000     C                   ENDIF                                                  <-- 2 ---
070500000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
070600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
070700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
070800000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
070900000000     C                   ELSE
071000000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
071100000000     C                   ENDIF
071200000000     C                   ENDIF
071300000000     C                   ENDDO                                                  <-- 1 ---
071400000000     C                   ENDSR
071500000000     C*------------------------------------------------------------------------*
071600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
071700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
071800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
071900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
072000000000     C*------------------------------------------------------------------------*
072100000000     C     OUTFUN        BEGSR
072200000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
072300000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
072400000000     C                   ENDDO
072500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
072600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
072700000000     C     1             DO        31            I
072800000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
072900000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
073000000000     C                   ADD       1             FUNNAT
073100000000     C                   ENDIF
073200000000     C                   ENDDO
073300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
073400000000     C     FUNNAT        IFGT      8
073500000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
073600000000     C                   ENDIF
073700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
073800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
073900000000     C                   Z-ADD     8             FUNVIS            5 0
074000000000     C     FAT(23)       IFEQ      'S'
074100000000     C                   SUB       1             FUNVIS
074200000000     C                   ENDIF
074300000000     C     FAT(24)       IFEQ      'S'
074400000000     C                   SUB       1             FUNVIS
074500000000     C                   ENDIF
074600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
074700000000     C                   Z-ADD     *ZEROS        W                 5 0
074800000000     C                   ADD       1             FUNULT
074900000000     C     FUNULT        IFGT      31
075000000000     C                   Z-ADD     1             FUNULT
075100000000     C                   ENDIF
075200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
075300000000     C     FUNULT        DO        31            I                              --- 1 -->
075400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
075500000000     C     W             IFLT      FUNVIS                                       --- 2 -->
075600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
075700000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
075800000000     C     FDE(I)        ANDNE     *BLANKS
075900000000     C     I             IFEQ      23                                           *TOLGO F23
076000000000     C     I             OREQ      24                                           *E F24
076100000000     C                   ELSE
076200000000     C                   ADD       1             W
076300000000     C                   MOVEL     FDE(I)        FUV(W)
076400000000     C                   ENDIF
076500000000     C                   ENDIF                                                  <-- 3 ---
076600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
076700000000     C                   Z-ADD     I             FUNULT
076800000000     C                   ENDIF                                                  <-- 2 ---
076900000000     C                   ENDDO                                                  <-- 1 ---
077000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
077100000000     C                   MOVEL     FUV(1)        VFD001
077200000000     C                   MOVEL     FUV(2)        VFD002
077300000000     C                   MOVEL     FUV(3)        VFD003
077400000000     C                   MOVEL     FUV(4)        VFD004
077500000000     C                   MOVEL     FUV(5)        VFD005
077600000000     C                   MOVEL     FUV(6)        VFD006
077700000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
077800000000     C     FAT(24)       ANDEQ     'N'
077900000000     C                   MOVEL     FUV(7)        VFD007
078000000000     C                   MOVEL     FUV(8)        VFD008
078100000000     C                   ELSE                                                   <-- 1 -->
078200000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
078300000000     C     FAT(24)       ANDEQ     'S'
078400000000     C                   MOVEL     FDE(23)       VFD007
078500000000     C                   MOVEL     FDE(24)       VFD008
078600000000     C                   ELSE                                                   <-- 2 -->
078700000000     C                   MOVEL     FUV(7)        VFD007
078800000000     C     FAT(23)       IFEQ      'S'
078900000000     C                   MOVEL     FDE(23)       VFD008
079000000000     C                   ELSE
079100000000     C                   MOVEL     FDE(24)       VFD008
079200000000     C                   ENDIF
079300000000     C                   ENDIF                                                  <-- 2 ---
079400000000     C                   ENDIF                                                  <-- 1 ---
079500000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
079600000000     C                   ENDSR
079700000000     C*------------------------------------------------------------------------*
079800000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
079900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
080000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
080100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
080200000000     C*------------------------------------------------------------------------*
080300000000     C     CONFUN        BEGSR
080400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
080500000000     C     1             DO        31            I                              --- 1 -->
080600000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
080700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
080800000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
080900000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
081000000000     C                   EXSR      CARMSG
081100000000     C                   MOVEL     '1'           $ERR
081200000000     C                   ENDIF                                                  <-- 3 ---
081300000000     C                   ENDIF                                                  <-- 2 ---
081400000000     C                   ENDDO                                                  <-- 1 ---
081500000000     C                   ENDSR
081600000000     C*------------------------------------------------------------------------*
081700000000** -MSG- (MESSAGGI)
081800000000TNTL21R- Tasto funzionale non valido.                                          1
081900000000TNTL21R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
082000000000TNTL21R- Opzione non valida.                                                   3
082100000000TNTL21R- Opzione abilitata ma non gestita. Avvertire CED.                      4
082200000000TNTL21R- Inizio elenco.                                                        5
082300000000TNTL21R- Fine elenco.                                                          6
082400000000TNTL21R- Immettere il codice traino                                            7
082500000000TNTL21R- Data errata                                                           8
082600000000TNTL21R- La data scadenza deve essere maggiore della decorrenza                9
082700000000TNTL21R- Il traino non può scadere in un giorno già trascorso                 10
082800960118TNTL21R- Il traino non può sovrapporsi ad uno già in decorrenza               11
082900960424TNTL21R- Il numero traino da duplicare non può essere maggiore di 9.000.000   12
083000000000TNTL21R- 13
083100000000TNTL21R- 14
083200000000TNTL21R- 15
083300000000TNTL21R- 16
083400000000TNTL21R- 17
083500000000TNTL21R- 18
083600000000TNTL21R- 19
083700000000TNTL21R- Il traino e gli archivi correlati sono stati duplicati.              20
083800000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
083900000000F1    -                   -    NN     NN
084000000000F2    -                   -    NN     NN
084100000000F3    -F3=Fine            -    NN     NN
084200000000F4    -                   -    NN     NN
084300000000F5    -                   -    NN     NN
084400000000F6    -F6=Conferma        -    SN     SN
084500000000F7    -                   -    NN     NN
084600000000F8    -                   -    NN     NN
084700000000F9    -                   -    NN     NN
084800000000F10   -                   -    NN     NN
084900000000F11   -                   -    NN     NN
085000000000F12   -F12=Ritorno        -    SS     SS
085100000000F13   -                   -    NN     NN
085200000000F14   -                   -    NN     NN
085300000000F15   -                   -    NN     NN
085400000000F16   -                   -    NN     NN
085500000000F17   -                   -    NN     NN
085600000000F18   -                   -    NN     NN
085700000000F19   -                   -    NN     NN
085800000000F20   -                   -    NN     NN
085900000000F21   -                   -    NN     NN
086000000000F22   -                   -    NN     NN
086100000000F23   -                   -    NN     NN
086200000000F24   -F24=Altri tasti    -    NN     NN
086300000000ENTER -                   -    SS     SS
086400000000ROLLUP-                   -    NN     NN
086500000000ROLLDN-                   -    NN     NN
086600000000HELP  -                   -    NN     NN
086700000000PRINT -                   -    NN     NN
086800000000BSPACE-                   -    NN     NN
086900000000CLEAR -                   -    NN     NN

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
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D*-------------------
002000000000     D* MESSAGGI E FUNZIONI
002100000000     D*-------------------
002200000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002300000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002400000000     D*-------------------
002500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600000000     D*-------------------
002700000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002800000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002900000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003000000000     D*-------------------
003100000000     D* TABELLA DESCRIZIONI A VIDEO
003200000000     D*-------------------
003300000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003400000000     D*-------------------
003500000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003600000000     D*-------------------
003700000000     D KEYS            S              1    DIM(40)
003800000000     D*------------------------------------------------------------------------*
003900000000     D* INPUT
004000000000     D*------------------------------------------------------------------------*
004100000000     D*-------------------
004200000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300000000     D*-------------------
004400000000     D DSFUN         E DS
004500000000     D  FUNRIG                 1     40
004600000000     D*-------------------
004700000000     D* DS PER GESTIONE TASTI FUNZIONALI
004800000000     D*-------------------
004900000000     D DSKEYS        E DS
005000000000     D  KEYRIG                 1     40
005100000000     D*-------------------
005200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005300000000     D*-------------------
005400000000     D                SDS
005500000000     D  USER                 254    263
005600000000     D*-------------------
005700000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005800000000     D*-------------------
005900000000     D DEVDS1          DS
006000000000     D  KEY                  369    369
006100000000     D  CURSOR               370    371
006200000000     D*---------------------
006300000000     D* ARCHITETTURA
006400000000     D*---------------------
006500000000     D KPJBA         E DS
006600000000     D*---------------------
006700000000     D* DS ESTERNA DI PROCEDURA
006800000000     D*---------------------
006900000000     D DS00          E DS                  EXTNAME(TNTL00DS)
007000000000     D*-------------------
007100000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007200000000     D*-------------------
007300000000     D DSMSG         E DS
007400020716
007500020716     d DdatiUte      e ds
007600020716     d AzuteDs       e ds                  ExtName(Azute00f)
007700020716     d Tibs34Ds      e ds                  Inz
007800020716
007900000000     D*------------------
008000000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
008100000000     D*------------------
008200000000     D WLBDA8          DS
008300000000     D  G08DAT                 1      8  0
008400000000     D  G08INV                 9     16  0
008500000000     D  G08ERR                17     17
008600000000     D  G08TGI                18     22  0
008700000000     C*------------------------------------------------------------------------*
008800000000     C* MAIN LINE
008900000000     C*------------------------------------------------------------------------*
009000000000     C*
009100000000     C                   WRITE     TL21DVT                                      *TESTATA
009200000000DO  1C     $FINE         DOUEQ     '1'
009300000000     C*
009400000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009500000000IF  2C     $GES          IFEQ      'V1'
009600000000     C                   EXSR      GESV1
009700000000E   2C                   ENDIF
009800000000     C*
009900000000E   1C                   ENDDO
010000000000     C*
010100000000IF  1C     $F03          IFEQ      '1'
010200000000     C                   MOVEL     '1'           D00F03
010300000000E   1C                   ENDIF
010400000000     C* FINE PGM
010500000000     C                   MOVEL     DS00          KPJBU
010600000000     C                   SETON                                        LR
010700000000     C*------------------------------------------------------------------------*
010800000000     C* GESV1 - GESTIONE VIDEO UNO
010900000000     C*------------------------------------------------------------------------*
011000000000     C     GESV1         BEGSR
011100000000     C*
011200000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
011300000000     C                   EXSR      CARV1                                        *CARICA VIDEO
011400000000E   1C                   ENDIF
011500000000     C*
011600000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011700000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011800000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011900000000E   1C                   ENDIF
012000000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012100000000     C*
012200000000     C                   EXFMT     TL21DV1                                      *PARAMETRI
012300000000     C*
012400000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012500000000     C*
012600000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012700000000SL  2C                   SELECT
012800000000     C     KEY           WHENEQ    ENTER
012900000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013000000000     C     KEY           WHENEQ    F6
013100000000     C                   EXSR      F06GES
013200000000     C                   OTHER
013300000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013400000000E   2C                   ENDSL
013500000000E   1C                   ENDIF
013600000000     C*
013700000000     C                   ENDSR
013800000000     C*------------------------------------------------------------------------*
013900000000     C* CARV1 - CARICA VIDEO UNO
014000000000     C*------------------------------------------------------------------------*
014100000000     C     CARV1         BEGSR
014200000000     C*
014300000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014400000000     C*
014500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014700000000     C*
014800000000     C*---                 WRITETL21DVT                    *TESTATA
014900000000     C*---                 WRITETL21DVF                    *FUNZIONI
015000000000     C*
015100000000     C                   ENDSR
015200000000     C*------------------------------------------------------------------------*
015300000000     C* ENTV1 - GESTIONE TASTO ENTER
015400000000     C*------------------------------------------------------------------------*
015500000000     C     ENTV1         BEGSR
015600000000     C*
015700000000     C* CONTROLLA I CAMPI IMMESSI
015800000000     C                   EXSR      DECODI
015900000000     C*
016000000000     C                   ENDSR
016100000000     C*------------------------------------------------------------------------*
016200000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
016300000000     C*------------------------------------------------------------------------*
016400000000     C     F06GES        BEGSR
016500000000     C*
016600000000     C* CONTROLLA I CAMPI IMMESSI
016700000000     C                   EXSR      DECODI
016800000000     C*
016900000000     C* RILEGGE IL TRAINO DA CUI COPIARE E DUPLICA I DATI
017000000000IF  1C     $ERR          IFEQ      '0'
017100000000     C                   Z-ADD     D00TRN        KLTTRN                         *TRAINO
017200000000     C                   Z-ADD     D00DDE        KLTDDE                         *DECORRENZA
017300000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
017400000000IF  2C     *IN99         IFEQ      *OFF
017500000000     C*--------------------
017600000000     C* F E R M A T E
017700000000     C*--------------------
017800000000     C                   Z-ADD     D00TRN        KLRTRN                         *TRAINO
017900000000     C                   Z-ADD     D00DDE        KLRDDE                         *DECORRENZA
018000000000     C     KEYTLR        SETLL     TNTLR01L
018100000000     C     KEYTLR        READE(N)  TNTLR01L                               99
018200000000DO  3C     *IN99         DOWEQ     *OFF
018300000000IF  4C     TLRATB        IFEQ      *BLANKS                                      *NO ANNULLATI
018400000000     C                   Z-ADD     V1CTR2        TLRTRN                         *NUMERO TRAINO
018500000000     C                   Z-ADD     DD2V1C        TLRDDE                         *DATA DECORRENZA
018600000000     C                   Z-ADD     DATCOR        TLRDIR                         *DATA INSERIMENTO
018700000000     C                   Z-ADD     *ZEROS        TLRDUV                         *DATA VARIAZIONE
018800000000     C                   MOVEL     *BLANKS       TLRFTR                         *FLAG TRASMISSIONE
018900000000     C                   Z-ADD     *ZEROS        TLRDTR                         *DATA TRASMISSIONE
019000000000     C                   WRITE     TNTLR000
019100000000E   4C                   ENDIF
019200000000     C     KEYTLR        READE(N)  TNTLR01L                               99
019300000000E   3C                   ENDDO
019400000000     C*--------------------
019500000000     C* O R A R I
019600000000     C*--------------------
019700000000     C                   Z-ADD     D00TRN        KLOTRN                         *TRAINO
019800000000     C                   Z-ADD     D00DDE        KLODDE                         *DECORRENZA
019900000000     C     KEYTLO        SETLL     TNTLO01L
020000000000     C     KEYTLO        READE(N)  TNTLO01L                               99
020100000000DO  3C     *IN99         DOWEQ     *OFF
020200000000IF  4C     TLOATB        IFEQ      *BLANKS                                      *NO ANNULLATI
020300000000     C                   Z-ADD     V1CTR2        TLOTRN                         *NUMERO TRAINO
020400000000     C                   Z-ADD     DD2V1C        TLODDE                         *DATA DECORRENZA
020500000000     C                   Z-ADD     DATCOR        TLODIR                         *DATA INSERIMENTO
020600000000     C                   Z-ADD     *ZEROS        TLODUV                         *DATA VARIAZIONE
020700000000     C                   MOVEL     *BLANKS       TLOFTR                         *FLAG TRASMISSIONE
020800000000     C                   Z-ADD     *ZEROS        TLODTR                         *DATA TRASMISSIONE
020900000000     C                   WRITE     TNTLO000
021000000000E   4C                   ENDIF
021100000000     C     KEYTLO        READE(N)  TNTLO01L                               99
021200000000E   3C                   ENDDO
021300000000     C*--------------------
021400000000     C* M E T R I C U B I
021500000000     C*--------------------
021600000000     C                   Z-ADD     D00TRN        KLMTRN                         *TRAINO
021700000000     C                   Z-ADD     D00DDE        KLMDDE                         *DECORRENZA
021800000000     C     KEYTLM        SETLL     TNTLM01L
021900000000     C     KEYTLM        READE(N)  TNTLM01L                               99
022000000000DO  3C     *IN99         DOWEQ     *OFF
022100000000IF  4C     TLMATB        IFEQ      *BLANKS                                      *NO ANNULLATI
022200000000     C                   Z-ADD     V1CTR2        TLMTRN                         *NUMERO TRAINO
022300000000     C                   Z-ADD     DD2V1C        TLMDDE                         *DATA DECORRENZA
022400000000     C                   Z-ADD     DATCOR        TLMDIR                         *DATA INSERIMENTO
022500000000     C                   Z-ADD     *ZEROS        TLMDUV                         *DATA VARIAZIONE
022600000000     C                   MOVEL     *BLANKS       TLMFTR                         *FLAG TRASMISSIONE
022700000000     C                   Z-ADD     *ZEROS        TLMDTR                         *DATA TRASMISSIONE
022800000000     C                   WRITE     TNTLM000
022900000000E   4C                   ENDIF
023000000000     C     KEYTLM        READE(N)  TNTLM01L                               99
023100000000E   3C                   ENDDO
023200000000     C*--------------------
023300000000     C* L I S T I N I
023400000000     C*--------------------
023500000000     C                   Z-ADD     D00TRN        KLLTRN                         *TRAINO
023600000000     C                   Z-ADD     D00DDE        KLLDDE                         *DECORRENZA
023700010725     C     KEYTLL        SETLL     TITLL01L
023800010725     C     KEYTLL        READE(N)  TITLL01L                               99
023900000000DO  3C     *IN99         DOWEQ     *OFF
024000000000IF  4C     TLLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
024100000000     C                   Z-ADD     V1CTR2        TLLTRN                         *NUMERO TRAINO
024200000000     C                   Z-ADD     DD2V1C        TLLDDE                         *DATA DECORRENZA
024300000000     C                   Z-ADD     DATCOR        TLLDIR                         *DATA INSERIMENTO
024400000000     C                   Z-ADD     *ZEROS        TLLDUV                         *DATA VARIAZIONE
024500000000     C                   MOVEL     *BLANKS       TLLFTR                         *FLAG TRASMISSIONE
024600000000     C                   Z-ADD     *ZEROS        TLLDTR                         *DATA TRASMISSIONE
024700010725     C                   WRITE     TITLL000
024800000000E   4C                   ENDIF
024900010725     C     KEYTLL        READE(N)  TITLL01L                               99
025000000000E   3C                   ENDDO
025100000000     C*--------------------
025200000000     C* N O T E
025300000000     C*--------------------
025400000000     C                   Z-ADD     D00TRN        KLNTRN                         *TRAINO
025500000000     C                   Z-ADD     D00DDE        KLNDDE                         *DECORRENZA
025600000000     C     KEYTLN        SETLL     TNTLN01L
025700000000     C     KEYTLN        READE(N)  TNTLN01L                               99
025800000000DO  3C     *IN99         DOWEQ     *OFF
025900000000IF  4C     TLNATB        IFEQ      *BLANKS                                      *NO ANNULLATI
026000000000     C                   Z-ADD     V1CTR2        TLNTRN                         *NUMERO TRAINO
026100000000     C                   Z-ADD     DD2V1C        TLNDDE                         *DATA DECORRENZA
026200000000     C                   Z-ADD     DATCOR        TLNDIR                         *DATA INSERIMENTO
026300000000     C                   Z-ADD     *ZEROS        TLNDUV                         *DATA VARIAZIONE
026400000000     C                   MOVEL     *BLANKS       TLNFTR                         *FLAG TRASMISSIONE
026500000000     C                   Z-ADD     *ZEROS        TLNDTR                         *DATA TRASMISSIONE
026600000000     C                   WRITE     TNTLN000
026700000000E   4C                   ENDIF
026800000000     C     KEYTLN        READE(N)  TNTLN01L                               99
026900000000E   3C                   ENDDO
027000000000     C*--------------------
027100000000     C* T R A I N O
027200000000     C*--------------------
027300000000     C                   Z-ADD     V1CTR2        TLTTRN                         *NUMERO TRAINO
027400000000     C                   Z-ADD     DD2V1C        TLTDDE                         *DATA DECORRENZA
027500000000     C                   Z-ADD     DS2V1C        TLTDSC                         *DATA SCADENZA
027600000000     C                   Z-ADD     DATCOR        TLTDIR                         *DATA INSERIMENTO
027700000000     C                   Z-ADD     *ZEROS        TLTDUV                         *DATA VARIAZIONE
027800000000     C                   MOVEL     *BLANKS       TLTFTR                         *FLAG TRASMISSIONE
027900000000     C                   Z-ADD     *ZEROS        TLTDTR                         *DATA TRASMISSIONE
028000000000     C                   WRITE     TNTLT000
028100000000     C*
028200000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
028300000000     C                   MOVEL     MSG(20)       D00MSG
028400000000     C                   MOVEL     '1'           $FINE
028500000000E   2C                   ENDIF
028600000000E   1C                   ENDIF
028700000000     C*
028800000000     C                   ENDSR
028900000000     C*------------------------------------------------------------------------*
029000000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
029100000000     C*------------------------------------------------------------------------*
029200000000     C     DECODI        BEGSR
029300000000     C*
029400000000     C* REIMPOSTA INDICATORI DI ERRORE
029500000000     C                   SETOFF                                       606162
029600000000     C*--------------------
029700000000     C* TRAINO
029800000000     C*--------------------
029900000000IF  1C     $ERR          IFEQ      '0'
030000000000IF  2C     V1CTR2        IFEQ      *ZEROS
030100000000     C                   MOVEL     '1'           $ERR
030200000000     C                   MOVEL     MSG(7)        $MSG
030300000000     C                   EXSR      CARMSG
030400000000     C                   SETON                                        60
030500960424X   2C                   ELSE
030600960424IF  3C     V1CTR2        IFGT      9000000
030700960424     C                   MOVEL     '1'           $ERR
030800960424     C                   MOVEL     MSG(12)       $MSG
030900960424     C                   EXSR      CARMSG
031000960424     C                   SETON                                        60
031100960424E   3C                   ENDIF
031200000000E   2C                   ENDIF
031300000000E   1C                   ENDIF
031400000000     C*--------------------
031500000000     C* DECORRENZA
031600000000     C*--------------------
031700000000IF  1C     $ERR          IFEQ      '0'
031800000000     C                   Z-ADD     V1CDD2        G08DAT
031900000000     C                   Z-ADD     *ZEROS        G08INV
032000000000     C                   MOVEL     '0'           G08ERR
032100000000     C                   CALL      'XSRDA8'
032200000000     C                   PARM                    WLBDA8
032300000000     C                   Z-ADD     G08DAT        V1CDD2
032400000000     C                   Z-ADD     G08INV        DD2V1C
032500000000IF  2C     G08ERR        IFEQ      '1'
032600000000     C                   MOVEL     '1'           $ERR
032700000000     C                   MOVEL     MSG(8)        $MSG
032800000000     C                   EXSR      CARMSG
032900000000     C                   SETON                                        61
033000000000E   2C                   ENDIF
033100000000E   1C                   ENDIF
033200000000     C*--------------------
033300000000     C* SCADENZA
033400000000     C*--------------------
033500000000     C* CORRETTA
033600000000IF  1C     $ERR          IFEQ      '0'
033700000000     C                   Z-ADD     V1CDS2        G08DAT
033800000000     C                   Z-ADD     *ZEROS        G08INV
033900000000     C                   MOVEL     '0'           G08ERR
034000000000     C                   CALL      'XSRDA8'
034100000000     C                   PARM                    WLBDA8
034200000000     C                   Z-ADD     G08DAT        V1CDS2
034300000000     C                   Z-ADD     G08INV        DS2V1C
034400000000IF  2C     G08ERR        IFEQ      '1'
034500000000     C                   MOVEL     '1'           $ERR
034600000000     C                   MOVEL     MSG(8)        $MSG
034700000000     C                   EXSR      CARMSG
034800000000     C                   SETON                                        62
034900000000E   2C                   ENDIF
035000000000E   1C                   ENDIF
035100000000     C* > DELLA DECORRENZA
035200000000IF  1C     $ERR          IFEQ      '0'
035300000000IF  2C     DS2V1C        IFLT      DD2V1C
035400000000     C                   MOVEL     '1'           $ERR
035500000000     C                   MOVEL     MSG(9)        $MSG
035600000000     C                   EXSR      CARMSG
035700000000     C                   SETON                                        62
035800000000E   2C                   ENDIF
035900000000E   1C                   ENDIF
036000970110     C* >= DATA CORRENTE
036100000000IF  1C     $ERR          IFEQ      '0'
036200970110IF  2C     DS2V1C        IFLT      DATCOR
036300000000     C                   MOVEL     '1'           $ERR
036400000000     C                   MOVEL     MSG(10)       $MSG
036500000000     C                   EXSR      CARMSG
036600000000     C                   SETON                                        62
036700000000E   2C                   ENDIF
036800000000E   1C                   ENDIF
036900000000     C*--------------------
037000000000     C* IL NUOVO TRAINO NON SI DEVE SOVRAPPORRE A UNO GIA' IN DECORRENZA
037100000000     C*--------------------
037200000000IF  1C     $ERR          IFEQ      '0'
037300000000     C                   Z-ADD     V1CTR2        KLTTRN
037400000000     C     KLTTRN        SETLL     TNTLT01L
037500000000     C     KLTTRN        READE(N)  TNTLT01L                               99
037600000000DO  2C     *IN99         DOWEQ     *OFF
037700980608IF  3C     DD2V1C        IFGE      TLTDDE                                          *DECORRENZA
037800960320     C     DD2V1C        ANDLE     TLTDSC
037900960320     C     DS2V1C        ORGE      TLTDDE                                          *SCADENZA
038000960320     C     DS2V1C        ANDLE     TLTDSC
038100960321     C     TLTDDE        ORGE      DD2V1C
038200960321     C     TLTDSC        ANDLE     DS2V1C
038300000000     C                   MOVEL     '1'           $ERR
038400000000     C                   LEAVE
038500980608E   3C                   ENDIF
038600000000     C     KLTTRN        READE(N)  TNTLT01L                               99
038700000000E   2C                   ENDDO
038800000000IF  2C     $ERR          IFEQ      '1'
038900960118     C                   MOVEL     MSG(11)       $MSG
039000000000     C                   EXSR      CARMSG
039100000000     C                   SETON                                        606162
039200000000E   2C                   ENDIF
039300000000E   1C                   ENDIF
039400000000     C*
039500000000     C                   ENDSR
039600000000     C*------------------------------------------------------------------------*
039700000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
039800000000     C*------------------------------------------------------------------------*
039900000000     C     IMPVID        BEGSR
040000000000     C*
040100000000     C* CALCOLA LA DATA CORRENTE
040200000000     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
040300000000     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
040400000000     C                   Z-ADD     WNUM8         G08DAT
040500000000     C                   Z-ADD     *ZEROS        G08INV
040600000000     C                   MOVEL     '0'           G08ERR
040700000000     C                   CALL      'XSRDA8'
040800000000     C                   PARM                    WLBDA8
040900000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
041000000000     C*
041100000000     C* LEGGE IL TRAINO
041200000000     C                   Z-ADD     D00TRN        KLTTRN                         *TRAINO
041300000000     C                   Z-ADD     D00DDE        KLTDDE                         *DECORRENZA
041400000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
041500000000IF  1C     *IN99         IFEQ      *OFF
041600000000     C* DATI DA COPIARE
041700000000     C                   Z-ADD     TLTTRN        V1CTRN                         *TRAINO
041800000000     C                   Z-ADD     TLTDDE        G08INV                         *DECORRENZA
041900000000     C                   Z-ADD     *ZEROS        G08DAT
042000000000     C                   MOVEL     '3'           G08ERR
042100000000     C                   CALL      'XSRDA8'
042200000000     C                   PARM                    WLBDA8
042300000000     C                   Z-ADD     G08DAT        V1CDDE
042400000000     C                   Z-ADD     TLTDSC        G08INV                         *SCADENZA
042500000000     C                   Z-ADD     *ZEROS        G08DAT
042600000000     C                   MOVEL     '3'           G08ERR
042700000000     C                   CALL      'XSRDA8'
042800000000     C                   PARM                    WLBDA8
042900000000     C                   Z-ADD     G08DAT        V1CDSC
043000000000     C* DATI DA COPIARE
043100000000     C                   Z-ADD     *ZEROS        V1CTR2                         *TRAINO
043200000000     C                   Z-ADD     *ZEROS        V1CDD2                         *DECORRENZA
043300000000     C                   Z-ADD     *ZEROS        V1CDS2                         *SCADENZA
043400000000E   1C                   ENDIF
043500000000     C*
043600000000     C                   ENDSR
043700000000     C*------------------------------------------------------------------------*
043800000000     C* *INZSR  - ROUTINE INIZIALE
043900000000     C*------------------------------------------------------------------------*
044000000000     C     *INZSR        BEGSR
044100000000     C*--------------------
044200000000     C* RICEVIMENTO PARAMETRI
044300000000     C*--------------------
044400000000     C     *ENTRY        PLIST
044500000000     C                   PARM                    KPJBA
044600000000     C                   MOVEL     KPJBU         DS00
044700000000     C*--------------------
044800000000     C* VARIABILI RIFERITE AL DATA BASE
044900000000     C*--------------------
045000000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
045100000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
045200000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
045300000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
045400000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
045500000000     C     *LIKE         DEFINE    TLODDE        KLODDE
045600000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
045700000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
045800010725     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
045900000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
046000000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
046100000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
046200000000     C*--------------------
046300000000     C* VARIABILI NON RIFERITE AL DATA BASE
046400000000     C*--------------------
046500000000     C     *LIKE         DEFINE    V1CDD2        DD2V1C                         *DEPOSITI
046600000000     C     *LIKE         DEFINE    V1CDS2        DS2V1C
046700000000     C*--------------------
046800000000     C* CHIAVI DI LETTURA
046900000000     C*--------------------
047000000000     C* LETTURA TNTLT01L - COMPLETA
047100000000     C     KEYTLT        KLIST
047200000000     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
047300000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
047400000000     C* LETTURA TNTLR01L - PARZIALE
047500000000     C     KEYTLR        KLIST
047600000000     C                   KFLD                    KLRTRN                         *NUMERO TRAINO
047700000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
047800000000     C* LETTURA TNTLO01L - PARZIALE
047900000000     C     KEYTLO        KLIST
048000000000     C                   KFLD                    KLOTRN                         *NUMERO TRAINO
048100000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
048200000000     C* LETTURA TNTLM01L - PARZIALE
048300000000     C     KEYTLM        KLIST
048400000000     C                   KFLD                    KLMTRN                         *NUMERO TRAINO
048500000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
048600010725     C* LETTURA TITLL01L - PARZIALE
048700000000     C     KEYTLL        KLIST
048800000000     C                   KFLD                    KLLTRN                         *NUMERO TRAINO
048900000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
049000000000     C* LETTURA TNTLN01L - PARZIALE
049100000000     C     KEYTLN        KLIST
049200000000     C                   KFLD                    KLNTRN                         *NUMERO TRAINO
049300000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
049400000000     C***
049500000000     C* IMPOSTAZIONI VIDEO
049600000000     C***
049700000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
049800000000     C                   MOVE      D00OP0        INTGES            2
049900000000IF  1C     INTGES        IFEQ      '02'
050000000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
050100000000X   1C                   ELSE
050200000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
050300000000E   1C                   ENDIF
050400000000     C*
050500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
050600000000     C                   EXSR      INZSTD
050700000000     C*
050800000000     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
050900000000     C                   EXSR      IMPVID
051000000000     C*
051100000000     C* EMETTO PRIMO FORMATO VIDEO
051200000000     C                   EXSR      CARV1
051300000000     C*
051400000000     C                   ENDSR
051500000000     C*------------------------------------------------------------------------*
051600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051800000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
051900000000     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
052000000000     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
052100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
052200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
052300000000     C*------------------------------------------------------------------------*
052400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
052500000000     C*------------------------------------------------------------------------*
052600000000     C     INZSTD        BEGSR
052700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
052800000000     C                   CALL      'XKEYS'
052900000000     C                   PARM                    DSKEYS
053000000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
053100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
053200000000     C     XMSGPA        PLIST
053300000000     C                   PARM                    DSMSMS
053400000000     C                   PARM                    DSMSPG
053500000000     C                   PARM                    DSMSKE
053600000000     C                   PARM                    DSMSER
053700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
053800000000     C                   MOVEL     'TNTL21R'     DSMSPG                         *NOME PGM CHIAMANTE
053900000000     C                   MOVEL     'TNTL21R'     VMCQUE                         *CODA MESSAGGI DESTI
054000000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
054100000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
054200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
054300000000     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
054400000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
054500000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
054600000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
054700000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
054800000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
054900000000     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
055000000000     C* LANCIO ROUTINE PER VERIFICARE SE
055100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
055200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
055300020716
055400020716     c     *dtaara       define    §azute        azuteds
055500020716     c     *dtaara       define    §datiute      ddatiute
055600020716
055700020716     c                   in(E)     *dtaara
055800020716     c                   if        %Error or rsut = *blanks
055900020716     c                   clear                   tibs34ds
056000020716     c                   call      'TIBS34R'
056100020716     c                   parm                    Tibs34Ds
056200020716     c                   in        *dtaara
056300020716     c                   endif
056400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
056500000000     C                   BITOFF    '01234567'    POSMI1            1
056600000000     C                   BITOFF    '01234567'    POSMI2            1
056700000000     C                   BITOFF    '01234567'    POSMA1            1
056800000000     C                   BITOFF    '01234567'    POSMA2            1
056900000000     C                   BITON     '34'          POSMI1            1            *18
057000000000     C                   BITON     '7'           POSMI2            1            *01
057100000000     C                   BITON     '34'          POSMA1            1            *18
057200000000     C                   BITON     '0'           POSMA2            1            *80
057300000000     C                   MOVE      POSMI2        POSMIN            2
057400000000     C                   MOVEL     POSMI1        POSMIN
057500000000     C                   MOVE      POSMA2        POSMAX            2
057600000000     C                   MOVEL     POSMA1        POSMAX
057700000000     C                   ENDSR
057800000000     C*------------------------------------------------------------------------*
057900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
058000000000     C*------------------------------------------------------------------------*
058100000000     C     FUNSTD        BEGSR
058200000000     C                   SELECT
058300000000     C     KEY           WHENEQ    F3
058400000000     C                   EXSR      F03STD
058500000000     C     KEY           WHENEQ    F12
058600000000     C                   EXSR      F12STD
058700000000     C     KEY           WHENEQ    F24
058800000000     C                   EXSR      F24STD
058900000000     C                   OTHER
059000000000     C                   MOVEL     MSG(2)        $MSG
059100000000     C                   EXSR      CARMSG
059200000000     C                   ENDSL
059300000000     C                   ENDSR
059400000000     C*------------------------------------------------------------------------*
059500000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
059600000000     C*------------------------------------------------------------------------*
059700000000     C     F03STD        BEGSR
059800000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
059900000000     C                   MOVEL     '1'           $FINE
060000951212     C* IMPOSTA FLAG DI FINE PROCEDURA
060100951212     C                   MOVEL     '1'           $F03
060200951212     C* PULISCE IL MESSAGGIO
060300951212     C                   MOVEL     *BLANKS       D00MSG
060400000000     C                   ENDSR
060500000000     C*------------------------------------------------------------------------*
060600000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
060700000000     C*------------------------------------------------------------------------*
060800000000     C     F12STD        BEGSR
060900000000     C* IMPOSTA FLAG DI FINE PROGRAMMA
061000000000     C                   MOVEL     '1'           $FINE
061100951212     C* IMPOSTA FLAG DI FINE PROCEDURA
061200951212     C                   MOVEL     '1'           $F12
061300951212     C* PULISCE IL MESSAGGIO
061400951212     C                   MOVEL     *BLANKS       D00MSG
061500000000     C                   ENDSR
061600000000     C*------------------------------------------------------------------------*
061700000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
061800000000     C*------------------------------------------------------------------------*
061900000000     C     F24STD        BEGSR
062000000000     C*
062100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
062200000000     C                   EXSR      OUTFUN
062300000000     C*
062400000000     C                   ENDSR
062500000000     C*------------------------------------------------------------------------*
062600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
062700000000     C*          CODA MESSAGGI DEL PGM
062800000000     C*------------------------------------------------------------------------*
062900000000     C     CARMSG        BEGSR
063000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
063100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
063200000000     C     MSGULT        IFEQ      0
063300000000     C                   MOVEL     '1'           *IN40
063400000000     C                   WRITE     TL21DVK
063500000000     C                   MOVEL     '0'           *IN40
063600000000     C                   ENDIF
063700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
063800000000     C                   MOVEL     $MSG          DSMSMS
063900000000     C                   CALL      'XMSG'        XMSGPA
064000000000     C     DSMSER        IFEQ      0
064100000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
064200000000     C                   ENDIF
064300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
064400000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
064500000000     C                   WRITE     TL21DVM                                      *SFL MESS
064600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
064700000000     C                   MOVEL     *BLANKS       $MSG
064800000000     C                   ENDSR
064900000000     C*------------------------------------------------------------------------*
065000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
065100000000     C*------------------------------------------------------------------------*
065200000000     C     OUTMSG        BEGSR
065300000000     C* VISUALIZZAZIONE MESSAGGIO
065400000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
065500000000     C                   WRITE     TL21DVK                                      *CONTR MESS
065600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
065700000000     C                   Z-ADD     *ZEROS        MSGULT
065800000000     C                   ENDSR
065900000000     C*------------------------------------------------------------------------*
066000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
066100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
066200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
066300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
066400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
066500000000     C*------------------------------------------------------------------------*
066600000000     C     CARFUN        BEGSR
066700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
066800000000     C     1             DO        31            I                 5 0          --- 1 -->
066900000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
067000000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
067100000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
067200020716     c                   If        DutLpo = 'S'
067300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
067400000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
067500000000     C                   ELSE                                                   *INTERROG.
067600000000     C                   MOVEL     FUNASI        FAT(I)
067700000000     C                   ENDIF
067800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
067900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
068000000000     C                   MOVEL     FUNAFG        FAT(I)
068100000000     C                   ELSE                                                   *INTERROG.
068200000000     C                   MOVEL     FUNAFI        FAT(I)
068300000000     C                   ENDIF
068400000000     C                   ENDIF                                                  <-- 2 ---
068500000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
068600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
068700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
068800000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
068900000000     C                   ELSE
069000000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
069100000000     C                   ENDIF
069200000000     C                   ENDIF
069300000000     C                   ENDDO                                                  <-- 1 ---
069400000000     C                   ENDSR
069500000000     C*------------------------------------------------------------------------*
069600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
069700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
069800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
069900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
070000000000     C*------------------------------------------------------------------------*
070100000000     C     OUTFUN        BEGSR
070200000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
070300000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
070400000000     C                   ENDDO
070500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
070600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
070700000000     C     1             DO        31            I
070800000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
070900000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
071000000000     C                   ADD       1             FUNNAT
071100000000     C                   ENDIF
071200000000     C                   ENDDO
071300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
071400000000     C     FUNNAT        IFGT      8
071500000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
071600000000     C                   ENDIF
071700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
071800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
071900000000     C                   Z-ADD     8             FUNVIS            5 0
072000000000     C     FAT(23)       IFEQ      'S'
072100000000     C                   SUB       1             FUNVIS
072200000000     C                   ENDIF
072300000000     C     FAT(24)       IFEQ      'S'
072400000000     C                   SUB       1             FUNVIS
072500000000     C                   ENDIF
072600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
072700000000     C                   Z-ADD     *ZEROS        W                 5 0
072800000000     C                   ADD       1             FUNULT
072900000000     C     FUNULT        IFGT      31
073000000000     C                   Z-ADD     1             FUNULT
073100000000     C                   ENDIF
073200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
073300000000     C     FUNULT        DO        31            I                              --- 1 -->
073400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
073500000000     C     W             IFLT      FUNVIS                                       --- 2 -->
073600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
073700000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
073800000000     C     FDE(I)        ANDNE     *BLANKS
073900000000     C     I             IFEQ      23                                           *TOLGO F23
074000000000     C     I             OREQ      24                                           *E F24
074100000000     C                   ELSE
074200000000     C                   ADD       1             W
074300000000     C                   MOVEL     FDE(I)        FUV(W)
074400000000     C                   ENDIF
074500000000     C                   ENDIF                                                  <-- 3 ---
074600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
074700000000     C                   Z-ADD     I             FUNULT
074800000000     C                   ENDIF                                                  <-- 2 ---
074900000000     C                   ENDDO                                                  <-- 1 ---
075000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
075100000000     C                   MOVEL     FUV(1)        VFD001
075200000000     C                   MOVEL     FUV(2)        VFD002
075300000000     C                   MOVEL     FUV(3)        VFD003
075400000000     C                   MOVEL     FUV(4)        VFD004
075500000000     C                   MOVEL     FUV(5)        VFD005
075600000000     C                   MOVEL     FUV(6)        VFD006
075700000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
075800000000     C     FAT(24)       ANDEQ     'N'
075900000000     C                   MOVEL     FUV(7)        VFD007
076000000000     C                   MOVEL     FUV(8)        VFD008
076100000000     C                   ELSE                                                   <-- 1 -->
076200000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
076300000000     C     FAT(24)       ANDEQ     'S'
076400000000     C                   MOVEL     FDE(23)       VFD007
076500000000     C                   MOVEL     FDE(24)       VFD008
076600000000     C                   ELSE                                                   <-- 2 -->
076700000000     C                   MOVEL     FUV(7)        VFD007
076800000000     C     FAT(23)       IFEQ      'S'
076900000000     C                   MOVEL     FDE(23)       VFD008
077000000000     C                   ELSE
077100000000     C                   MOVEL     FDE(24)       VFD008
077200000000     C                   ENDIF
077300000000     C                   ENDIF                                                  <-- 2 ---
077400000000     C                   ENDIF                                                  <-- 1 ---
077500000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
077600000000     C                   ENDSR
077700000000     C*------------------------------------------------------------------------*
077800000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
077900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
078000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
078100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
078200000000     C*------------------------------------------------------------------------*
078300000000     C     CONFUN        BEGSR
078400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
078500000000     C     1             DO        31            I                              --- 1 -->
078600000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
078700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
078800000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
078900000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
079000000000     C                   EXSR      CARMSG
079100000000     C                   MOVEL     '1'           $ERR
079200000000     C                   ENDIF                                                  <-- 3 ---
079300000000     C                   ENDIF                                                  <-- 2 ---
079400000000     C                   ENDDO                                                  <-- 1 ---
079500000000     C                   ENDSR
079600000000     C*------------------------------------------------------------------------*
079700000000** -MSG- (MESSAGGI)
079800000000TNTL21R- Tasto funzionale non valido.                                          1
079900000000TNTL21R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
080000000000TNTL21R- Opzione non valida.                                                   3
080100000000TNTL21R- Opzione abilitata ma non gestita. Avvertire CED.                      4
080200000000TNTL21R- Inizio elenco.                                                        5
080300000000TNTL21R- Fine elenco.                                                          6
080400000000TNTL21R- Immettere il codice traino                                            7
080500000000TNTL21R- Data errata                                                           8
080600000000TNTL21R- La data scadenza deve essere maggiore della decorrenza                9
080700000000TNTL21R- Il traino non può scadere in un giorno già trascorso                 10
080800960118TNTL21R- Il traino non può sovrapporsi ad uno già in decorrenza               11
080900960424TNTL21R- Il numero traino da duplicare non può essere maggiore di 9.000.000   12
081000000000TNTL21R- 13
081100000000TNTL21R- 14
081200000000TNTL21R- 15
081300000000TNTL21R- 16
081400000000TNTL21R- 17
081500000000TNTL21R- 18
081600000000TNTL21R- 19
081700000000TNTL21R- Il traino e gli archivi correlati sono stati duplicati.              20
081800000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
081900000000F1    -                   -    NN     NN
082000000000F2    -                   -    NN     NN
082100000000F3    -F3=Fine            -    NN     NN
082200000000F4    -                   -    NN     NN
082300000000F5    -                   -    NN     NN
082400000000F6    -F6=Conferma        -    SN     SN
082500000000F7    -                   -    NN     NN
082600000000F8    -                   -    NN     NN
082700000000F9    -                   -    NN     NN
082800000000F10   -                   -    NN     NN
082900000000F11   -                   -    NN     NN
083000000000F12   -F12=Ritorno        -    SS     SS
083100000000F13   -                   -    NN     NN
083200000000F14   -                   -    NN     NN
083300000000F15   -                   -    NN     NN
083400000000F16   -                   -    NN     NN
083500000000F17   -                   -    NN     NN
083600000000F18   -                   -    NN     NN
083700000000F19   -                   -    NN     NN
083800000000F20   -                   -    NN     NN
083900000000F21   -                   -    NN     NN
084000000000F22   -                   -    NN     NN
084100000000F23   -                   -    NN     NN
084200000000F24   -F24=Altri tasti    -    NN     NN
084300000000ENTER -                   -    SS     SS
084400000000ROLLUP-                   -    NN     NN
084500000000ROLLDN-                   -    NN     NN
084600000000HELP  -                   -    NN     NN
084700000000PRINT -                   -    NN     NN
084800000000BSPACE-                   -    NN     NN
084900000000CLEAR -                   -    NN     NN

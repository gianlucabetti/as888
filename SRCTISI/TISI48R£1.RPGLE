000100970603     H*--------------------------------------------------------------------------------------------*
000200000427     H* GESTIONE VERSIONI CAPPARIO - DUPLICA VERSIONE CAPPARIO (CON TUTTI GLI ARCHIVI CORRELATI)
000300000427     H* + RELATIVE ESTENSIONI X POSTE
000400970603     H*--------------------------------------------------------------------------------------------*
000500970603     H DECEDIT('0,') DATEDIT(*DMY.)
000600970603     F*--------------------------------------------------------------------------------------------*
000700970603     F* DATA BASE
000800970603     F*--------------------------------------------------------------------------------------------*
000900970603     FTISI48D   CF   E             WORKSTN INFDS(DEVDS1)
001000970603     F                                     SFILE(SI48DVM:MSGULT)
001100970603     FAZCPR01L  UF A E           K DISK
001200970603     FAZCPC01L  UF A E           K DISK
001300000427     FAZPPC01L  UF A E           K DISK
001400970603     FAZCPE01L  UF A E           K DISK
001500000622     FAZPPE01L  UF A E           K DISK
001600970603     FAZCPT01L  UF A E           K DISK
001700970603     FAZCPL01L  UF A E           K DISK
001800000427     FAZPPL01L  UF A E           K DISK
001900970603     FAZCPS01L  UF A E           K DISK
002000970603     D*--------------------------------------------------------------------------------------------*
002100970603     D* SCHIERE
002200970603     D*--------------------------------------------------------------------------------------------*
002300970603     D*-------------------
002400970603     D* MESSAGGI E FUNZIONI
002500970603     D*-------------------
002600970603     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002700970603     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002800970603     D*-------------------
002900970603     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003000970603     D*-------------------
003100970603     D FFU             S              6    DIM(31)                              *FUNZIONE
003200970603     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003300970603     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003400970603     D*-------------------
003500970603     D* TABELLA DESCRIZIONI A VIDEO
003600970603     D*-------------------
003700970603     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003800970603     D*-------------------
003900970603     D* TABELLA TASTI FUNZIONALI PREMUTI
004000970603     D*-------------------
004100970603     D KEYS            S              1    DIM(40)
004200970603     D*--------------------------------------------------------------------------------------------*
004300970603     D* INPUT
004400970603     D*--------------------------------------------------------------------------------------------*
004500970603     D*-------------------
004600970603     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004700970603     D*-------------------
004800970603     D DSFUN         E DS
004900970603     D  FUNRIG                 1     40
005000970603     D*-------------------
005100970603     D* DS PER GESTIONE TASTI FUNZIONALI
005200970603     D*-------------------
005300970603     D DSKEYS        E DS
005400970603     D  KEYRIG                 1     40
005500970603     D*-------------------
005600970603     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005700970603     D*-------------------
005800970603     D                SDS
005900970603     D  USER                 254    263
006000970603     D*-------------------
006100970603     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006200970603     D*-------------------
006300970603     D DEVDS1          DS
006400970603     D  KEY                  369    369
006500970603     D  CURSOR               370    371
006600970603     D*---------------------
006700970603     D* ARCHITETTURA
006800970603     D*---------------------
006900970603     D KPJBA         E DS
007000970603     D*---------------------
007100970603     D* DS ESTERNA DI PROCEDURA
007200970603     D*---------------------
007300970603     D DSSI00        E DS                  EXTNAME(TISI00DS)
007400970603     D*-------------------
007500020919     D* DS REPERIMENTO DATI UTENTE
007600970603     D*-------------------
007700020919     D TIBS34DS      E DS                                                       *Profili utente
007800020919     D DDATIUTE      E DS                                                       *Dati utente
007900020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008000970603     D*-------------------
008100970603     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
008200970603     D*-------------------
008300970603     D DSMSG         E DS
008400970603     D*------------------
008500970603     D* DS "XSRDA8" - CONTROLLA DATA (8)
008600970603     D*------------------
008700970603     D WLBDA8          DS
008800970603     D  G08DAT                 1      8  0
008900970603     D  G08INV                 9     16  0
009000970603     D  G08ERR                17     17
009100970603     D  G08TGI                18     22  0
009200970603     C*--------------------------------------------------------------------------------------------*
009300970603     C* MAIN LINE
009400970603     C*--------------------------------------------------------------------------------------------*
009500970603     C*
009600970611     C* SE NON CI SONO ERRORI IN ENTRATA
009700970611IF  1C     $FINE         IFEQ      '0'
009800970603     C                   WRITE     SI48DVT                                      *TESTATA
009900970611DO  2C     $FINE         DOUEQ     '1'
010000970603     C*
010100970603     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010200970611IF  3C     $GES          IFEQ      'V1'
010300970603     C                   EXSR      GESV1
010400970611E   3C                   ENDIF
010500970603     C*
010600970611E   2C                   ENDDO
010700970603     C*
010800970611IF  2C     $F03          IFEQ      '1'
010900970603     C                   MOVEL     '1'           D00F03
011000970611E   2C                   ENDIF
011100970611E   1C                   ENDIF
011200970603     C* FINE PGM
011300970603     C                   SETON                                        LR
011400970603     C*--------------------------------------------------------------------------------------------*
011500970603     C* GESV1 - GESTIONE VIDEO UNO
011600970603     C*--------------------------------------------------------------------------------------------*
011700970603     C     GESV1         BEGSR
011800970603     C*
011900970603IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
012000970603     C                   EXSR      CARV1                                        *CARICA VIDEO
012100970603E   1C                   ENDIF
012200970603     C*
012300970603IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012400970603     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012500970603     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012600970603E   1C                   ENDIF
012700970603     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012800970603     C*
012900970603     C                   EXFMT     SI48DV1                                      *PARAMETRI
013000970603     C*
013100970603     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013200970603     C*
013300970603IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013400970603SL  2C                   SELECT
013500970603     C     KEY           WHENEQ    ENTER
013600970603     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013700970603     C     KEY           WHENEQ    F6
013800970603     C                   EXSR      F06GES
013900970603     C                   OTHER
014000970603     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014100970603E   2C                   ENDSL
014200970603E   1C                   ENDIF
014300970603     C*
014400970603     C                   ENDSR
014500970603     C*--------------------------------------------------------------------------------------------*
014600970603     C* CARV1 - CARICA VIDEO UNO
014700970603     C*--------------------------------------------------------------------------------------------*
014800970603     C     CARV1         BEGSR
014900970603     C*
015000970603     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015100970603     C*
015200970603     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015300970603     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015400970603     C*
015500970603     C*---                 WRITESI48DVT                    *TESTATA
015600970603     C*---                 WRITESI48DVF                    *FUNZIONI
015700970603     C*
015800970603     C                   ENDSR
015900970603     C*--------------------------------------------------------------------------------------------*
016000970603     C* ENTV1 - GESTIONE TASTO ENTER
016100970603     C*--------------------------------------------------------------------------------------------*
016200970603     C     ENTV1         BEGSR
016300970603     C*
016400970603     C* CONTROLLA I CAMPI IMMESSI
016500970603     C                   EXSR      DECODI
016600970603     C*
016700970603     C                   ENDSR
016800970603     C*--------------------------------------------------------------------------------------------*
016900970603     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
017000970603     C*--------------------------------------------------------------------------------------------*
017100970603     C     F06GES        BEGSR
017200970603     C*
017300970603     C* CONTROLLA I CAMPI IMMESSI
017400970603     C                   EXSR      DECODI
017500970603     C*
017600970603     C* RILEGGE LA VERSIONE DA CUI COPIARE
017700970603IF  1C     $ERR          IFEQ      '0'
017800970603     C                   Z-ADD     V1CVER        KCRVER
017900970603     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
018000970603IF  2C     *IN99         IFEQ      *OFF
018100970603     C*--------------------
018200970603     C* CAPPARIO
018300970603     C*--------------------
018400970603     C                   Z-ADD     V1CVER        KCCVER                         *VERSIONE
018500970603     C     KEYCPC        SETLL     AZCPC01L
018600970603     C     KEYCPC        READE(N)  AZCPC01L                               99
018700970603DO  3C     *IN99         DOWEQ     *OFF
018800970603IF  4C     CPCATB        IFEQ      *BLANKS                                      *NO ANNULLATI
018900000427     C                   Z-ADD     V1CVE2        CPCVER                         *VERSIONE
019000000622     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
019100970603     C                   Z-ADD     *ZEROS        CPCDTR                         *DATA TRASMISSIONE
019200970603     C                   WRITE     AZCPC000
019300000427     C                   MOVEL     CPCNAR        KPCNAR
019400000427     C                   MOVEL     CPCCAP        KPCCAP
019500000427     C     KEYPPC        CHAIN     AZPPC01L                           98
019600000427     C     *IN98         IFEQ      *OFF
019700000427     C                   Z-ADD     V1CVE2        PPCVER                         *VERSIONE
019800000427     C                   MOVEL     *BLANKS       PPCFTR                         *FLAG TRASMISSIONE
019900000427     C                   Z-ADD     *ZEROS        PPCDTR                         *DATA TRASMISSIONE
020000000427     C                   WRITE     AZPPC000
020100000427     C                   ENDIF
020200970603E   4C                   ENDIF
020300970603     C     KEYCPC        READE(N)  AZCPC01L                               99
020400970603E   3C                   ENDDO
020500970603     C*--------------------
020600970603     C* ECCEZIONI LINEE/ZONE
020700970603     C*--------------------
020800970603     C                   Z-ADD     V1CVER        KCEVER                         *VERSIONE
020900970603     C     KEYCPE        SETLL     AZCPE01L
021000970603     C     KEYCPE        READE(N)  AZCPE01L                               99
021100970603DO  3C     *IN99         DOWEQ     *OFF
021200970603IF  4C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
021300000427     C                   Z-ADD     V1CVE2        CPEVER                         *VERSIONE
021400970603     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
021500970603     C                   Z-ADD     *ZEROS        CPEDTR                         *DATA TRASMISSIONE
021600970603     C                   WRITE     AZCPE000
021700000622     C                   MOVEL     CPENAR        KPENAR
021800000622     C                   MOVEL     CPECAP        KPECAP
021900000622     C                   Z-ADD     CPETFP        KPETFP
022000000622     C     KEYPPE        CHAIN     AZPPE01L                           98
022100000622     C     *IN98         IFEQ      *OFF
022200000622     C                   Z-ADD     V1CVE2        PPEVER                         *VERSIONE
022300000622     C                   MOVEL     *BLANKS       PPEFTR                         *FLAG TRASMISSIONE
022400000622     C                   Z-ADD     *ZEROS        PPEDTR                         *DATA TRASMISSIONE
022500000622     C                   WRITE     AZPPE000
022600000622     C                   ENDIF
022700970603E   4C                   ENDIF
022800970603     C     KEYCPE        READE(N)  AZCPE01L                               99
022900970603E   3C                   ENDDO
023000970603     C*--------------------
023100970603     C* TEMPI CONSEGNA
023200970603     C*--------------------
023300970603     C                   Z-ADD     V1CVER        KCTVER                         *VERSIONE
023400970603     C     KEYCPT        SETLL     AZCPT01L
023500970603     C     KEYCPT        READE(N)  AZCPT01L                               99
023600970603DO  3C     *IN99         DOWEQ     *OFF
023700970603IF  4C     CPTATB        IFEQ      *BLANKS                                      *NO ANNULLATI
023800000427     C                   Z-ADD     V1CVE2        CPTVER                         *VERSIONE
023900970603     C                   MOVEL     *BLANKS       CPTFTR                         *FLAG TRASMISSIONE
024000970603     C                   Z-ADD     *ZEROS        CPTDTR                         *DATA TRASMISSIONE
024100970603     C                   WRITE     AZCPT000
024200970603E   4C                   ENDIF
024300970603     C     KEYCPT        READE(N)  AZCPT01L                               99
024400970603E   3C                   ENDDO
024500970603     C*--------------------
024600970603     C* LOCALITA'
024700970603     C*--------------------
024800970603     C                   Z-ADD     V1CVER        KCLVER                         *VERSIONE
024900970603     C     KEYCPL        SETLL     AZCPL01L
025000970603     C     KEYCPL        READE(N)  AZCPL01L                               99
025100970603DO  3C     *IN99         DOWEQ     *OFF
025200970603IF  4C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
025300000427     C                   Z-ADD     V1CVE2        CPLVER                         *VERSIONE
025400970603     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
025500970603     C                   Z-ADD     *ZEROS        CPLDTR                         *DATA TRASMISSIONE
025600970603     C                   WRITE     AZCPL000
025700000427     C                   MOVEL     CPLNAR        KPLNAR
025800000427     C                   MOVEL     CPLCAP        KPLCAP
025900000427     C                   MOVEL     CPLLOC        KPLLOC
026000000427     C     KEYPPL        CHAIN     AZPPL01L                           98
026100000427     C     *IN98         IFEQ      *OFF
026200000427     C                   Z-ADD     V1CVE2        PPLVER                         *VERSIONE
026300000427     C                   MOVEL     *BLANKS       PPLFTR                         *FLAG TRASMISSIONE
026400000427     C                   Z-ADD     *ZEROS        PPLDTR                         *DATA TRASMISSIONE
026500000427     C                   WRITE     AZPPL000
026600000427     C                   ENDIF
026700970603E   4C                   ENDIF
026800970603     C     KEYCPL        READE(N)  AZCPL01L                               99
026900970603E   3C                   ENDDO
027000970603     C*--------------------
027100970603     C* SINONIMI LOCALITA'
027200970603     C*--------------------
027300970603     C                   Z-ADD     V1CVER        KCSVER                         *VERSIONE
027400970603     C     KEYCPS        SETLL     AZCPS01L
027500970603     C     KEYCPS        READE(N)  AZCPS01L                               99
027600970603DO  3C     *IN99         DOWEQ     *OFF
027700970603IF  4C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
027800000427     C                   Z-ADD     V1CVE2        CPSVER                         *VERSIONE
027900970603     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
028000970603     C                   Z-ADD     *ZEROS        CPSDTR                         *DATA TRASMISSIONE
028100970603     C                   WRITE     AZCPS000
028200970603E   4C                   ENDIF
028300970603     C     KEYCPS        READE(N)  AZCPS01L                               99
028400970603E   3C                   ENDDO
028500970603     C*--------------------
028600970603     C* VERSIONE
028700970603     C*--------------------
028800970603     C                   Z-ADD     V1CVE2        CPRVER                         *VERSIONE
028900970603     C                   Z-ADD     DD2V1C        CPRDDE                         *DATA DECORRENZA
029000970603     C                   Z-ADD     DS2V1C        CPRDSC                         *DATA SCADENZA
029100970603     C                   MOVEL     *BLANKS       CPRFTR                         *FLAG TRASMISSIONE
029200970603     C                   Z-ADD     *ZEROS        CPRDTR                         *DATA TRASMISSIONE
029300970603     C                   WRITE     AZCPR000
029400970603     C*
029500970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
029600970603     C                   MOVEL     MSG(20)       D00MSG
029700970603     C                   MOVEL     '1'           $FINE
029800970603E   2C                   ENDIF
029900970603E   1C                   ENDIF
030000970603     C*
030100970603     C                   ENDSR
030200970603     C*--------------------------------------------------------------------------------------------*
030300970603     C* DECODI - CONTROLLA CAMPI E DECODIFICA
030400970603     C*--------------------------------------------------------------------------------------------*
030500970603     C     DECODI        BEGSR
030600970603     C*
030700970603     C* REIMPOSTA INDICATORI DI ERRORE
030800970603     C                   SETOFF                                       606162
030900970603     C*--------------------
031000970603     C* VERSIONE
031100970603     C*--------------------
031200970603IF  1C     $ERR          IFEQ      '0'
031300970611IF  2C     V1CVE2        IFEQ      *ZEROS
031400970603     C                   MOVEL     '1'           $ERR
031500970603     C                   MOVEL     MSG(7)        $MSG
031600970603     C                   EXSR      CARMSG
031700970603     C                   SETON                                        60
031800970603X   2C                   ELSE
031900970603E   2C                   ENDIF
032000970603E   1C                   ENDIF
032100970603     C*--------------------
032200970603     C* DECORRENZA
032300970603     C*--------------------
032400970603IF  1C     $ERR          IFEQ      '0'
032500970603     C                   Z-ADD     V1CDD2        G08DAT
032600970603     C                   Z-ADD     *ZEROS        G08INV
032700970603     C                   MOVEL     '0'           G08ERR
032800970603     C                   CALL      'XSRDA8'
032900970603     C                   PARM                    WLBDA8
033000970603     C                   Z-ADD     G08DAT        V1CDD2
033100970603     C                   Z-ADD     G08INV        DD2V1C
033200970603IF  2C     G08ERR        IFEQ      '1'
033300970603     C                   MOVEL     '1'           $ERR
033400970603     C                   MOVEL     MSG(8)        $MSG
033500970603     C                   EXSR      CARMSG
033600970603     C                   SETON                                        61
033700970603E   2C                   ENDIF
033800970603E   1C                   ENDIF
033900970603     C*--------------------
034000970603     C* SCADENZA
034100970603     C*--------------------
034200970603     C* CORRETTA
034300970603IF  1C     $ERR          IFEQ      '0'
034400970603     C                   Z-ADD     V1CDS2        G08DAT
034500970603     C                   Z-ADD     *ZEROS        G08INV
034600970603     C                   MOVEL     '0'           G08ERR
034700970603     C                   CALL      'XSRDA8'
034800970603     C                   PARM                    WLBDA8
034900970603     C                   Z-ADD     G08DAT        V1CDS2
035000970603     C                   Z-ADD     G08INV        DS2V1C
035100970603IF  2C     G08ERR        IFEQ      '1'
035200970603     C                   MOVEL     '1'           $ERR
035300970603     C                   MOVEL     MSG(8)        $MSG
035400970603     C                   EXSR      CARMSG
035500970603     C                   SETON                                        62
035600970603E   2C                   ENDIF
035700970603E   1C                   ENDIF
035800970603     C* > DELLA DECORRENZA
035900970603IF  1C     $ERR          IFEQ      '0'
036000970603IF  2C     DS2V1C        IFLT      DD2V1C
036100970603     C                   MOVEL     '1'           $ERR
036200970603     C                   MOVEL     MSG(9)        $MSG
036300970603     C                   EXSR      CARMSG
036400970603     C                   SETON                                        62
036500970603E   2C                   ENDIF
036600970603E   1C                   ENDIF
036700970603     C* >= DATA CORRENTE
036800970603IF  1C     $ERR          IFEQ      '0'
036900970603IF  2C     DS2V1C        IFLT      DATCOR
037000970603     C                   MOVEL     '1'           $ERR
037100970603     C                   MOVEL     MSG(12)       $MSG
037200970603     C                   EXSR      CARMSG
037300970603     C                   SETON                                        62
037400970603E   2C                   ENDIF
037500970603E   1C                   ENDIF
037600970603     C*--------------------
037700970603     C* LA NUOVA VERSIONE NON PUO' SOVRAPPORSI AD UNA GIA' IN DECORRENZA
037800970603     C*--------------------
037900970603     C* NUOVA VERSIONE INESISTENTE
038000970603IF  1C     $ERR          IFEQ      '0'
038100970603     C                   Z-ADD     V1CVE2        KCRVER
038200970603     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
038300970603IF  2C     *IN99         IFEQ      *OFF
038400970603     C                   MOVEL     '1'           $ERR
038500970603     C                   MOVEL     MSG(10)       $MSG
038600970603     C                   EXSR      CARMSG
038700970603     C                   SETON                                        60
038800970603E   2C                   ENDIF
038900970603E   1C                   ENDIF
039000970603     C* NUOVA VERSIONE NON ACCAVALLATA
039100970603IF  1C     $ERR          IFEQ      '0'
039200970603     C     *LOVAL        SETLL     AZCPR01L
039300970603     C                   READ(N)   AZCPR01L                               99
039400970603DO  2C     *IN99         DOWEQ     *OFF
039500970603IF  3C     CPRATB        IFEQ      *BLANKS
039600970603IF  4C     DD2V1C        IFGE      CPRDDE                                          *DECORRENZA
039700970603     C     DD2V1C        ANDLE     CPRDSC
039800970603     C     DS2V1C        ORGE      CPRDDE                                          *SCADENZA
039900970603     C     DS2V1C        ANDLE     CPRDSC
040000970603     C     CPRDDE        ORGE      DD2V1C
040100970603     C     CPRDSC        ANDLE     DS2V1C
040200970603     C                   MOVEL     '1'           $ERR
040300970603     C                   MOVEL     MSG(11)       $MSG
040400970603     C                   EXSR      CARMSG
040500970603     C                   SETON                                        606162
040600970603     C                   LEAVE
040700970603E   4C                   ENDIF
040800970603E   3C                   ENDIF
040900970603     C                   READ(N)   AZCPR01L                               99
041000970603E   2C                   ENDDO
041100970603E   1C                   ENDIF
041200970603     C*
041300970603     C                   ENDSR
041400970603     C*--------------------------------------------------------------------------------------------*
041500970603     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
041600970603     C*--------------------------------------------------------------------------------------------*
041700970603     C     IMPVID        BEGSR
041800970603     C*
041900970603     C* CALCOLA LA DATA CORRENTE
042000970603     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
042100970603     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
042200970603     C                   Z-ADD     WNUM8         G08DAT
042300970603     C                   Z-ADD     *ZEROS        G08INV
042400970603     C                   MOVEL     '0'           G08ERR
042500970603     C                   CALL      'XSRDA8'
042600970603     C                   PARM                    WLBDA8
042700970603     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
042800970603     C*
042900970603     C* LEGGE LA VERSIONE
043000970603     C                   Z-ADD     D00VER        KCRVER                         *VERSIONE
043100970603     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
043200970611IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
043300970611     C* DATI DA COPIARE
043400970603     C                   Z-ADD     CPRVER        V1CVER                         *VERSIONE
043500970603     C                   Z-ADD     CPRDDE        G08INV                         *DECORRENZA
043600970603     C                   Z-ADD     *ZEROS        G08DAT
043700970603     C                   MOVEL     '3'           G08ERR
043800970603     C                   CALL      'XSRDA8'
043900970603     C                   PARM                    WLBDA8
044000970603     C                   Z-ADD     G08DAT        V1CDDE
044100970603     C                   Z-ADD     CPRDSC        G08INV                         *SCADENZA
044200970603     C                   Z-ADD     *ZEROS        G08DAT
044300970603     C                   MOVEL     '3'           G08ERR
044400970603     C                   CALL      'XSRDA8'
044500970603     C                   PARM                    WLBDA8
044600970603     C                   Z-ADD     G08DAT        V1CDSC
044700970611     C* DATI SU CUI COPIARE
044800970603     C                   Z-ADD     *ZEROS        V1CVE2                         *VERSIONE
044900970603     C                   Z-ADD     *ZEROS        V1CDD2                         *DECORRENZA
045000970603     C                   Z-ADD     *ZEROS        V1CDS2                         *SCADENZA
045100970611X   1C                   ELSE                                                   *NON TROVATO
045200970611     C                   MOVEL     '1'           $FINE                          *FINE PGM
045300970611     C                   MOVEL     '1'           D00ERR                         *ERRORE
045400970611     C                   MOVEL     MSG(19)       D00MSG                         *MESSSAGGIO
045500970603E   1C                   ENDIF
045600970603     C*
045700970603     C                   ENDSR
045800020919     C*--------------------------------------------------------------------------------------------*
045900020919     C* REPERISCE I DATI UTENTE
046000020919     C*--------------------------------------------------------------------------------------------*
046100020919     C     REPDATIUTE    BEGSR
046200020919     C*
046300020919     C* INIZIALIZZA VARIABILI DI WRK
046400020919     C                   CLEAR                   TIBS34DS
046500020919     C                   CLEAR                   AZUTEDS
046600020919     C                   CLEAR                   DDATIUTE
046700020919     C*
046800020919     C     *DTAARA       DEFINE    §azute        azuteds
046900020919     C     *DTAARA       DEFINE    §datiute      ddatiute
047000020919     C                   IN(E)     *DTAARA
047100020919if  1C                   IF        %Error
047200020919     c                   EVAL      I34Tla = 'L'
047300020919     C                   CALL      'TIBS34R'
047400020919     C                   PARM                    Tibs34Ds
047500020919     C                   IN        *DTAARA
047600020919e   1C                   ENDIF
047700020919     C*
047800020919     C                   ENDSR
047900970603     C*--------------------------------------------------------------------------------------------*
048000970603     C* *INZSR  - ROUTINE INIZIALE
048100970603     C*--------------------------------------------------------------------------------------------*
048200970603     C     *INZSR        BEGSR
048300970603     C*--------------------
048400970603     C* RICEVIMENTO PARAMETRI
048500970603     C*--------------------
048600970603     C     *ENTRY        PLIST
048700970612     C                   PARM                    KPJBA
048800970612     C                   PARM                    DSSI00
048900970603     C*--------------------
049000970603     C* VARIABILI RIFERITE AL DATA BASE
049100970603     C*--------------------
049200970603     C     *LIKE         DEFINE    CPRVER        KCRVER                         *AZCPR01L
049300970603     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC01L
049400970603     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE01L
049500970603     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT01L
049600970603     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL01L
049700970603     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS01L
049800000427     C     *LIKE         DEFINE    PPCNAR        KPCNAR                         *AZPPC01L
049900000427     C     *LIKE         DEFINE    PPCCAP        KPCCAP                         *AZPPC01L
050000000427     C     *LIKE         DEFINE    PPLNAR        KPLNAR                         *AZPPL01L
050100000427     C     *LIKE         DEFINE    PPLCAP        KPLCAP                         *AZPPL01L
050200000427     C     *LIKE         DEFINE    PPLLOC        KPLLOC                         *AZPPL01L
050300000622     C     *LIKE         DEFINE    PPENAR        KPENAR                         *AZPPE01L
050400000622     C     *LIKE         DEFINE    PPECAP        KPECAP                         *AZPPE01L
050500000622     C     *LIKE         DEFINE    PPETFP        KPETFP                         *AZPPE01L
050600970603     C*--------------------
050700970603     C* VARIABILI NON RIFERITE AL DATA BASE
050800970603     C*--------------------
050900970603     C     *LIKE         DEFINE    V1CDD2        DD2V1C                         *DEPOSITI
051000970603     C     *LIKE         DEFINE    V1CDS2        DS2V1C
051100970603     C*--------------------
051200970603     C* CHIAVI DI LETTURA
051300970603     C*--------------------
051400970603     C* LETTURA AZCPR01L - COMPLETA
051500970603     C     KEYCPR        KLIST
051600970603     C                   KFLD                    KCRVER                         *VERSIONE
051700970603     C* LETTURA AZCPC01L - PARZIALE
051800970603     C     KEYCPC        KLIST
051900970603     C                   KFLD                    KCCVER                         *VERSIONE
052000000427     C* LETTURA AZPPC01L - COMPLETA
052100000427     C     KEYPPC        KLIST
052200000427     C                   KFLD                    KCCVER                         *VERSIONE
052300000427     C                   KFLD                    KPCNAR                         *NAZIONE
052400000427     C                   KFLD                    KPCCAP                         *CAP
052500970603     C* LETTURA AZCPE01L - PARZIALE
052600970603     C     KEYCPE        KLIST
052700970603     C                   KFLD                    KCEVER                         *VERSIONE
052800000622     C* LETTURA AZPPE01L - COMPLETA
052900000622     C     KEYPPE        KLIST
053000000622     C                   KFLD                    KCEVER                         *VERSIONE
053100000622     C                   KFLD                    KPENAR                         *NAZIONE
053200000622     C                   KFLD                    KPECAP                         *CAP
053300000622     C                   KFLD                    KPETFP                         *AS400 DI PARTENZA
053400970603     C* LETTURA AZCPT01L - PARZIALE
053500970603     C     KEYCPT        KLIST
053600970603     C                   KFLD                    KCTVER                         *VERSIONE
053700970603     C* LETTURA AZCPL01L - PARZIALE
053800970603     C     KEYCPL        KLIST
053900970603     C                   KFLD                    KCLVER                         *VERSIONE
054000000427     C* LETTURA AZPPL01L - COMPLETA
054100000427     C     KEYPPL        KLIST
054200000427     C                   KFLD                    KCLVER                         *VERSIONE
054300000427     C                   KFLD                    KPLNAR                         *NAZIONE
054400000427     C                   KFLD                    KPLCAP                         *CAP
054500000427     C                   KFLD                    KPLLOC                         *LOCALITA
054600970603     C* LETTURA AZCPS01L - PARZIALE
054700970603     C     KEYCPS        KLIST
054800970603     C                   KFLD                    KCSVER                         *VERSIONE
054900970603     C***
055000970603     C* IMPOSTAZIONI VIDEO
055100970603     C***
055200970603IF  1C     D00OP0        IFEQ      'R02'
055300970603     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
055400970603X   1C                   ELSE
055500970603     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
055600970603E   1C                   ENDIF
055700970603     C*
055800970603     C* ESEGUE OPERAZIONI INIZIALI STANDARD
055900970603     C                   EXSR      INZSTD
056000970603     C*
056100970603     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
056200970603     C                   EXSR      IMPVID
056300970603     C*
056400970603     C* EMETTO PRIMO FORMATO VIDEO
056500970603     C                   EXSR      CARV1
056600970603     C*
056700970603     C                   ENDSR
056800970603     C*--------------------------------------------------------------------------------------------*
056900970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057000970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057100970603     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
057200970603     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
057300970603     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
057400970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057500970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057600970603     C*--------------------------------------------------------------------------------------------*
057700970603     C* INZSTD - ROUTINE INIZIALE STANDARD
057800970603     C*--------------------------------------------------------------------------------------------*
057900970603     C     INZSTD        BEGSR
058000970603     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
058100970603     C                   CALL      'XKEYS'
058200970603     C                   PARM                    DSKEYS
058300970603     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
058400970603     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
058500970603     C     XMSGPA        PLIST
058600970603     C                   PARM                    DSMSMS
058700970603     C                   PARM                    DSMSPG
058800970603     C                   PARM                    DSMSKE
058900970603     C                   PARM                    DSMSER
059000970603     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
059100970603     C                   MOVEL     'TISI48R'     DSMSPG                         *NOME PGM CHIAMANTE
059200970603     C                   MOVEL     'TISI48R'     VMCQUE                         *CODA MESSAGGI DESTI
059300970603     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
059400970603     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
059500970603     C* INIZIALIZZO FLAG CONTROLLO PGM
059600970603     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
059700970603     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
059800970603     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
059900970603     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
060000970603     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
060100970603     C                   MOVEL     '0'           $FINE             1            *FINE PGM
060200970603     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
060300970603     C* LANCIO ROUTINE PER VERIFICARE SE
060400970603     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
060500970603     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
060600020919     C                   EXSR      REPDATIUTE
060700970603     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
060800970603     C                   BITOFF    '01234567'    POSMI1            1
060900970603     C                   BITOFF    '01234567'    POSMI2            1
061000970603     C                   BITOFF    '01234567'    POSMA1            1
061100970603     C                   BITOFF    '01234567'    POSMA2            1
061200970603     C                   BITON     '34'          POSMI1            1            *18
061300970603     C                   BITON     '7'           POSMI2            1            *01
061400970603     C                   BITON     '34'          POSMA1            1            *18
061500970603     C                   BITON     '0'           POSMA2            1            *80
061600970603     C                   MOVE      POSMI2        POSMIN            2
061700970603     C                   MOVEL     POSMI1        POSMIN
061800970603     C                   MOVE      POSMA2        POSMAX            2
061900970603     C                   MOVEL     POSMA1        POSMAX
062000970603     C                   ENDSR
062100970603     C*--------------------------------------------------------------------------------------------*
062200970603     C* FUNSTD - LANCIA SUBROUTINE STANDARD
062300970603     C*--------------------------------------------------------------------------------------------*
062400970603     C     FUNSTD        BEGSR
062500970603     C                   SELECT
062600970603     C     KEY           WHENEQ    F3
062700970603     C                   EXSR      F03STD
062800970603     C     KEY           WHENEQ    F12
062900970603     C                   EXSR      F12STD
063000970603     C     KEY           WHENEQ    F24
063100970603     C                   EXSR      F24STD
063200970603     C                   OTHER
063300970603     C                   MOVEL     MSG(2)        $MSG
063400970603     C                   EXSR      CARMSG
063500970603     C                   ENDSL
063600970603     C                   ENDSR
063700970603     C*--------------------------------------------------------------------------------------------*
063800970603     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
063900970603     C*--------------------------------------------------------------------------------------------*
064000970603     C     F03STD        BEGSR
064100970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
064200970603     C                   MOVEL     '1'           $FINE
064300970603     C* IMPOSTA FLAG DI FINE PROCEDURA
064400970603     C                   MOVEL     '1'           $F03
064500970603     C* PULISCE IL MESSAGGIO
064600970603     C                   MOVEL     *BLANKS       D00MSG
064700970603     C                   ENDSR
064800970603     C*--------------------------------------------------------------------------------------------*
064900970603     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
065000970603     C*--------------------------------------------------------------------------------------------*
065100970603     C     F12STD        BEGSR
065200970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
065300970603     C                   MOVEL     '1'           $FINE
065400970603     C* IMPOSTA FLAG DI FINE PROCEDURA
065500970603     C                   MOVEL     '1'           $F12
065600970603     C* PULISCE IL MESSAGGIO
065700970603     C                   MOVEL     *BLANKS       D00MSG
065800970603     C                   ENDSR
065900970603     C*--------------------------------------------------------------------------------------------*
066000970603     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
066100970603     C*--------------------------------------------------------------------------------------------*
066200970603     C     F24STD        BEGSR
066300970603     C*
066400970603     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
066500970603     C                   EXSR      OUTFUN
066600970603     C*
066700970603     C                   ENDSR
066800970603     C*--------------------------------------------------------------------------------------------*
066900970603     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
067000970603     C*          CODA MESSAGGI DEL PGM
067100970603     C*--------------------------------------------------------------------------------------------*
067200970603     C     CARMSG        BEGSR
067300970603     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
067400970603     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
067500970603     C     MSGULT        IFEQ      0
067600970603     C                   MOVEL     '1'           *IN40
067700970603     C                   WRITE     SI48DVK
067800970603     C                   MOVEL     '0'           *IN40
067900970603     C                   ENDIF
068000970603     C* INVIA MESSAGGIO ALLA CODA DEL PGM
068100970603     C                   MOVEL     $MSG          DSMSMS
068200970603     C                   CALL      'XMSG'        XMSGPA
068300970603     C     DSMSER        IFEQ      0
068400970603     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
068500970603     C                   ENDIF
068600970603     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
068700970603     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
068800970603     C                   WRITE     SI48DVM                                      *SFL MESS
068900970603     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
069000970603     C                   MOVEL     *BLANKS       $MSG
069100970603     C                   ENDSR
069200970603     C*--------------------------------------------------------------------------------------------*
069300970603     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
069400970603     C*--------------------------------------------------------------------------------------------*
069500970603     C     OUTMSG        BEGSR
069600970603     C* VISUALIZZAZIONE MESSAGGIO
069700970603     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
069800970603     C                   WRITE     SI48DVK                                      *CONTR MESS
069900970603     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
070000970603     C                   Z-ADD     *ZEROS        MSGULT
070100970603     C                   ENDSR
070200970603     C*--------------------------------------------------------------------------------------------*
070300970603     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
070400970603     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
070500970603     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
070600970603     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
070700970603     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
070800970603     C*--------------------------------------------------------------------------------------------*
070900970603     C     CARFUN        BEGSR
071000970603     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
071100970603     C     1             DO        31            I                 5 0          --- 1 -->
071200970603     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
071300970603     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
071400970603     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
071500020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
071600970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
071700970603     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
071800970603     C                   ELSE                                                   *INTERROG.
071900970603     C                   MOVEL     FUNASI        FAT(I)
072000970603     C                   ENDIF
072100970603     C                   ELSE                                                   <-- 2 --> *FILIALE
072200970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
072300970603     C                   MOVEL     FUNAFG        FAT(I)
072400970603     C                   ELSE                                                   *INTERROG.
072500970603     C                   MOVEL     FUNAFI        FAT(I)
072600970603     C                   ENDIF
072700970603     C                   ENDIF                                                  <-- 2 ---
072800970603     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
072900970603     C                   MOVEL     USER          WEDP              3            *PROFILO
073000970603     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
073100970603     C                   MOVEL     'S'           FAT(I)                         *ABILITO
073200970603     C                   ELSE
073300970603     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
073400970603     C                   ENDIF
073500970603     C                   ENDIF
073600970603     C                   ENDDO                                                  <-- 1 ---
073700970603     C                   ENDSR
073800970603     C*--------------------------------------------------------------------------------------------*
073900970603     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
074000970603     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
074100970603     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
074200970603     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
074300970603     C*--------------------------------------------------------------------------------------------*
074400970603     C     OUTFUN        BEGSR
074500970603     C     1             DO        8             I                              *PULIZIA FUNZIONI A
074600970603     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
074700970603     C                   ENDDO
074800970603     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
074900970603     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
075000970603     C     1             DO        31            I
075100970603     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
075200970603     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
075300970603     C                   ADD       1             FUNNAT
075400970603     C                   ENDIF
075500970603     C                   ENDDO
075600970603     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
075700970603     C     FUNNAT        IFGT      8
075800970603     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
075900970603     C                   ENDIF
076000970603     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
076100970603     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
076200970603     C                   Z-ADD     8             FUNVIS            5 0
076300970603     C     FAT(23)       IFEQ      'S'
076400970603     C                   SUB       1             FUNVIS
076500970603     C                   ENDIF
076600970603     C     FAT(24)       IFEQ      'S'
076700970603     C                   SUB       1             FUNVIS
076800970603     C                   ENDIF
076900970603     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
077000970603     C                   Z-ADD     *ZEROS        W                 5 0
077100970603     C                   ADD       1             FUNULT
077200970603     C     FUNULT        IFGT      31
077300970603     C                   Z-ADD     1             FUNULT
077400970603     C                   ENDIF
077500970603     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
077600970603     C     FUNULT        DO        31            I                              --- 1 -->
077700970603     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
077800970603     C     W             IFLT      FUNVIS                                       --- 2 -->
077900970603     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
078000970603     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
078100970603     C     FDE(I)        ANDNE     *BLANKS
078200970603     C     I             IFEQ      23                                           *TOLGO F23
078300970603     C     I             OREQ      24                                           *E F24
078400970603     C                   ELSE
078500970603     C                   ADD       1             W
078600970603     C                   MOVEL     FDE(I)        FUV(W)
078700970603     C                   ENDIF
078800970603     C                   ENDIF                                                  <-- 3 ---
078900970603     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
079000970603     C                   Z-ADD     I             FUNULT
079100970603     C                   ENDIF                                                  <-- 2 ---
079200970603     C                   ENDDO                                                  <-- 1 ---
079300970603     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
079400970603     C                   MOVEL     FUV(1)        VFD001
079500970603     C                   MOVEL     FUV(2)        VFD002
079600970603     C                   MOVEL     FUV(3)        VFD003
079700970603     C                   MOVEL     FUV(4)        VFD004
079800970603     C                   MOVEL     FUV(5)        VFD005
079900970603     C                   MOVEL     FUV(6)        VFD006
080000970603     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
080100970603     C     FAT(24)       ANDEQ     'N'
080200970603     C                   MOVEL     FUV(7)        VFD007
080300970603     C                   MOVEL     FUV(8)        VFD008
080400970603     C                   ELSE                                                   <-- 1 -->
080500970603     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
080600970603     C     FAT(24)       ANDEQ     'S'
080700970603     C                   MOVEL     FDE(23)       VFD007
080800970603     C                   MOVEL     FDE(24)       VFD008
080900970603     C                   ELSE                                                   <-- 2 -->
081000970603     C                   MOVEL     FUV(7)        VFD007
081100970603     C     FAT(23)       IFEQ      'S'
081200970603     C                   MOVEL     FDE(23)       VFD008
081300970603     C                   ELSE
081400970603     C                   MOVEL     FDE(24)       VFD008
081500970603     C                   ENDIF
081600970603     C                   ENDIF                                                  <-- 2 ---
081700970603     C                   ENDIF                                                  <-- 1 ---
081800970603     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
081900970603     C                   ENDSR
082000970603     C*--------------------------------------------------------------------------------------------*
082100970603     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
082200970603     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
082300970603     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
082400970603     C*                   - ERRORE SE FUNZIONE NON ATTIVA
082500970603     C*--------------------------------------------------------------------------------------------*
082600970603     C     CONFUN        BEGSR
082700970603     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
082800970603     C     1             DO        31            I                              --- 1 -->
082900970603     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
083000970603     C* CONTROLLO SE IL TASTO E' ATTIVO
083100970603     C     FAT(I)        IFNE      'S'                                          --- 3 -->
083200970603     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
083300970603     C                   EXSR      CARMSG
083400970603     C                   MOVEL     '1'           $ERR
083500970603     C                   ENDIF                                                  <-- 3 ---
083600970603     C                   ENDIF                                                  <-- 2 ---
083700970603     C                   ENDDO                                                  <-- 1 ---
083800970603     C                   ENDSR
083900970603     O*--------------------------------------------------------------------------------------------*
084000970603** -MSG- (MESSAGGI)
084100970603TISI48R- Tasto funzionale non valido.                                          1
084200970603TISI48R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
084300970603TISI48R- Opzione non valida.                                                   3
084400970603TISI48R- Opzione abilitata ma non gestita. Avvertire CED.                      4
084500970603TISI48R- Inizio elenco.                                                        5
084600970603TISI48R- Fine elenco.                                                          6
084700970611TISI48R- Immettere la versione                                                 7
084800970603TISI48R- Data errata                                                           8
084900970603TISI48R- La data scadenza deve essere maggiore della decorrenza                9
085000970603TISI48R- Versione già inserita                                                10
085100970603TISI48R- La versione non può sovrapporsi ad uno già in decorrenza             11
085200970603TISI48R- La versione non può scadere in un giorno già trascorso               12
085300970603TISI48R- 13
085400970603TISI48R- 14
085500970603TISI48R- 15
085600970603TISI48R- 16
085700970603TISI48R- 17
085800970603TISI48R- 18
085900970723TISI48R- Errori in entrata duplica. Non trovato il record da cui copiare      19
086000970603TISI48R- La versione e gli archivi correlati sono stati duplicati             20
086100970603** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
086200970603F1    -                   -    NN     NN
086300970603F2    -                   -    NN     NN
086400970603F3    -F3=Fine            -    NN     NN
086500970603F4    -                   -    NN     NN
086600970603F5    -                   -    NN     NN
086700970603F6    -F6=Conferma        -    SN     SN
086800970603F7    -                   -    NN     NN
086900970603F8    -                   -    NN     NN
087000970603F9    -                   -    NN     NN
087100970603F10   -                   -    NN     NN
087200970603F11   -                   -    NN     NN
087300970603F12   -F12=Ritorno        -    SS     SS
087400970603F13   -                   -    NN     NN
087500970603F14   -                   -    NN     NN
087600970603F15   -                   -    NN     NN
087700970603F16   -                   -    NN     NN
087800970603F17   -                   -    NN     NN
087900970603F18   -                   -    NN     NN
088000970603F19   -                   -    NN     NN
088100970603F20   -                   -    NN     NN
088200970603F21   -                   -    NN     NN
088300970603F22   -                   -    NN     NN
088400970603F23   -                   -    NN     NN
088500970603F24   -F24=Altri tasti    -    NN     NN
088600970603ENTER -                   -    SS     SS
088700970603ROLLUP-                   -    NN     NN
088800970603ROLLDN-                   -    NN     NN
088900970603HELP  -                   -    NN     NN
089000970603PRINT -                   -    NN     NN
089100970603BSPACE-                   -    NN     NN
089200970603CLEAR -                   -    NN     NN

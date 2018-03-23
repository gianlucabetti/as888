000100970603     H*--------------------------------------------------------------------------------------------*
000200000427     H* GESTIONE VERSIONI CAPPARIO - DUPLICA VERSIONE CAPPARIO (CON TUTTI GLI ARCHIVI CORRELATI)
000300970603     H*--------------------------------------------------------------------------------------------*
000400970603     H DECEDIT('0,') DATEDIT(*DMY.)
000500970603     F*--------------------------------------------------------------------------------------------*
000600970603     F* DATA BASE
000700970603     F*--------------------------------------------------------------------------------------------*
000800970603     FTISI48D   CF   E             WORKSTN INFDS(DEVDS1)
000900970603     F                                     SFILE(SI48DVM:MSGULT)
001000970603     FAZCPR01L  UF A E           K DISK
001100970603     FAZCPC01L  UF A E           K DISK
001200970603     FAZCPE01L  UF A E           K DISK
001300970603     FAZCPT01L  UF A E           K DISK
001400970603     FAZCPL01L  UF A E           K DISK
001500970603     FAZCPS01L  UF A E           K DISK
001600130830     FAZCOS01L  UF A E           K DISK
001700970603     D*--------------------------------------------------------------------------------------------*
001800970603     D* SCHIERE
001900970603     D*--------------------------------------------------------------------------------------------*
002000970603     D*-------------------
002100970603     D* MESSAGGI E FUNZIONI
002200970603     D*-------------------
002300970603     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002400970603     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002500970603     D*-------------------
002600970603     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002700970603     D*-------------------
002800970603     D FFU             S              6    DIM(31)                              *FUNZIONE
002900970603     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003000970603     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003100970603     D*-------------------
003200970603     D* TABELLA DESCRIZIONI A VIDEO
003300970603     D*-------------------
003400970603     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003500970603     D*-------------------
003600970603     D* TABELLA TASTI FUNZIONALI PREMUTI
003700970603     D*-------------------
003800970603     D KEYS            S              1    DIM(40)
003900970603     D*--------------------------------------------------------------------------------------------*
004000970603     D* INPUT
004100970603     D*--------------------------------------------------------------------------------------------*
004200970603     D*-------------------
004300970603     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004400970603     D*-------------------
004500970603     D DSFUN         E DS
004600970603     D  FUNRIG                 1     40
004700970603     D*-------------------
004800970603     D* DS PER GESTIONE TASTI FUNZIONALI
004900970603     D*-------------------
005000970603     D DSKEYS        E DS
005100970603     D  KEYRIG                 1     40
005200970603     D*-------------------
005300970603     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005400970603     D*-------------------
005500970603     D                SDS
005600970603     D  USER                 254    263
005700970603     D*-------------------
005800970603     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005900970603     D*-------------------
006000970603     D DEVDS1          DS
006100970603     D  KEY                  369    369
006200970603     D  CURSOR               370    371
006300970603     D*---------------------
006400970603     D* ARCHITETTURA
006500970603     D*---------------------
006600970603     D KPJBA         E DS
006700970603     D*---------------------
006800970603     D* DS ESTERNA DI PROCEDURA
006900970603     D*---------------------
007000970603     D DSSI00        E DS                  EXTNAME(TISI00DS)
007100970603     D*-------------------
007200020919     D* DS REPERIMENTO DATI UTENTE
007300970603     D*-------------------
007400020919     D TIBS34DS      E DS                                                       *Profili utente
007500020919     D DDATIUTE      E DS                                                       *Dati utente
007600020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007700970603     D*-------------------
007800970603     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007900970603     D*-------------------
008000970603     D DSMSG         E DS
008100970603     D*------------------
008200970603     D* DS "XSRDA8" - CONTROLLA DATA (8)
008300970603     D*------------------
008400970603     D WLBDA8          DS
008500970603     D  G08DAT                 1      8  0
008600970603     D  G08INV                 9     16  0
008700970603     D  G08ERR                17     17
008800970603     D  G08TGI                18     22  0
008900970603     C*--------------------------------------------------------------------------------------------*
009000970603     C* MAIN LINE
009100970603     C*--------------------------------------------------------------------------------------------*
009200970603     C*
009300970611     C* SE NON CI SONO ERRORI IN ENTRATA
009400970611IF  1C     $FINE         IFEQ      '0'
009500970603     C                   WRITE     SI48DVT                                      *TESTATA
009600970611DO  2C     $FINE         DOUEQ     '1'
009700970603     C*
009800970603     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009900970611IF  3C     $GES          IFEQ      'V1'
010000970603     C                   EXSR      GESV1
010100970611E   3C                   ENDIF
010200970603     C*
010300970611E   2C                   ENDDO
010400970603     C*
010500970611IF  2C     $F03          IFEQ      '1'
010600970603     C                   MOVEL     '1'           D00F03
010700970611E   2C                   ENDIF
010800970611E   1C                   ENDIF
010900970603     C* FINE PGM
011000970603     C                   SETON                                        LR
011100970603     C*--------------------------------------------------------------------------------------------*
011200970603     C* GESV1 - GESTIONE VIDEO UNO
011300970603     C*--------------------------------------------------------------------------------------------*
011400970603     C     GESV1         BEGSR
011500970603     C*
011600970603IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
011700970603     C                   EXSR      CARV1                                        *CARICA VIDEO
011800970603E   1C                   ENDIF
011900970603     C*
012000970603IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012100970603     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012200970603     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012300970603E   1C                   ENDIF
012400970603     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012500970603     C*
012600970603     C                   EXFMT     SI48DV1                                      *PARAMETRI
012700970603     C*
012800970603     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012900970603     C*
013000970603IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013100970603SL  2C                   SELECT
013200970603     C     KEY           WHENEQ    ENTER
013300970603     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013400970603     C     KEY           WHENEQ    F6
013500970603     C                   EXSR      F06GES
013600970603     C                   OTHER
013700970603     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013800970603E   2C                   ENDSL
013900970603E   1C                   ENDIF
014000970603     C*
014100970603     C                   ENDSR
014200970603     C*--------------------------------------------------------------------------------------------*
014300970603     C* CARV1 - CARICA VIDEO UNO
014400970603     C*--------------------------------------------------------------------------------------------*
014500970603     C     CARV1         BEGSR
014600970603     C*
014700970603     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014800970603     C*
014900970603     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015000970603     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015100970603     C*
015200970603     C*---                 WRITESI48DVT                    *TESTATA
015300970603     C*---                 WRITESI48DVF                    *FUNZIONI
015400970603     C*
015500970603     C                   ENDSR
015600970603     C*--------------------------------------------------------------------------------------------*
015700970603     C* ENTV1 - GESTIONE TASTO ENTER
015800970603     C*--------------------------------------------------------------------------------------------*
015900970603     C     ENTV1         BEGSR
016000970603     C*
016100970603     C* CONTROLLA I CAMPI IMMESSI
016200970603     C                   EXSR      DECODI
016300970603     C*
016400970603     C                   ENDSR
016500970603     C*--------------------------------------------------------------------------------------------*
016600970603     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
016700970603     C*--------------------------------------------------------------------------------------------*
016800970603     C     F06GES        BEGSR
016900970603     C*
017000970603     C* CONTROLLA I CAMPI IMMESSI
017100970603     C                   EXSR      DECODI
017200970603     C*
017300970603     C* RILEGGE LA VERSIONE DA CUI COPIARE
017400970603IF  1C     $ERR          IFEQ      '0'
017500970603     C                   Z-ADD     V1CVER        KCRVER
017600970603     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
017700970603IF  2C     *IN99         IFEQ      *OFF
017800970603     C*--------------------
017900970603     C* CAPPARIO
018000970603     C*--------------------
018100970603     C                   Z-ADD     V1CVER        KCCVER                         *VERSIONE
018200970603     C     KEYCPC        SETLL     AZCPC01L
018300970603     C     KEYCPC        READE(N)  AZCPC01L                               99
018400970603DO  3C     *IN99         DOWEQ     *OFF
018500970603IF  4C     CPCATB        IFEQ      *BLANKS                                      *NO ANNULLATI
018600000427     C                   Z-ADD     V1CVE2        CPCVER                         *VERSIONE
018700000622     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
018800970603     C                   Z-ADD     *ZEROS        CPCDTR                         *DATA TRASMISSIONE
018900970603     C                   WRITE     AZCPC000
019000970603E   4C                   ENDIF
019100970603     C     KEYCPC        READE(N)  AZCPC01L                               99
019200970603E   3C                   ENDDO
019300970603     C*--------------------
019400970603     C* ECCEZIONI LINEE/ZONE
019500970603     C*--------------------
019600970603     C                   Z-ADD     V1CVER        KCEVER                         *VERSIONE
019700970603     C     KEYCPE        SETLL     AZCPE01L
019800970603     C     KEYCPE        READE(N)  AZCPE01L                               99
019900970603DO  3C     *IN99         DOWEQ     *OFF
020000970603IF  4C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
020100000427     C                   Z-ADD     V1CVE2        CPEVER                         *VERSIONE
020200970603     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
020300970603     C                   Z-ADD     *ZEROS        CPEDTR                         *DATA TRASMISSIONE
020400970603     C                   WRITE     AZCPE000
020500970603E   4C                   ENDIF
020600970603     C     KEYCPE        READE(N)  AZCPE01L                               99
020700970603E   3C                   ENDDO
020800970603     C*--------------------
020900970603     C* TEMPI CONSEGNA
021000970603     C*--------------------
021100970603     C                   Z-ADD     V1CVER        KCTVER                         *VERSIONE
021200970603     C     KEYCPT        SETLL     AZCPT01L
021300970603     C     KEYCPT        READE(N)  AZCPT01L                               99
021400970603DO  3C     *IN99         DOWEQ     *OFF
021500970603IF  4C     CPTATB        IFEQ      *BLANKS                                      *NO ANNULLATI
021600000427     C                   Z-ADD     V1CVE2        CPTVER                         *VERSIONE
021700970603     C                   MOVEL     *BLANKS       CPTFTR                         *FLAG TRASMISSIONE
021800970603     C                   Z-ADD     *ZEROS        CPTDTR                         *DATA TRASMISSIONE
021900970603     C                   WRITE     AZCPT000
022000970603E   4C                   ENDIF
022100970603     C     KEYCPT        READE(N)  AZCPT01L                               99
022200970603E   3C                   ENDDO
022300970603     C*--------------------
022400970603     C* LOCALITA'
022500970603     C*--------------------
022600970603     C                   Z-ADD     V1CVER        KCLVER                         *VERSIONE
022700970603     C     KEYCPL        SETLL     AZCPL01L
022800970603     C     KEYCPL        READE(N)  AZCPL01L                               99
022900970603DO  3C     *IN99         DOWEQ     *OFF
023000970603IF  4C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
023100000427     C                   Z-ADD     V1CVE2        CPLVER                         *VERSIONE
023200970603     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
023300970603     C                   Z-ADD     *ZEROS        CPLDTR                         *DATA TRASMISSIONE
023400970603     C                   WRITE     AZCPL000
023500970603E   4C                   ENDIF
023600970603     C     KEYCPL        READE(N)  AZCPL01L                               99
023700970603E   3C                   ENDDO
023800970603     C*--------------------
023900970603     C* SINONIMI LOCALITA'
024000970603     C*--------------------
024100970603     C                   Z-ADD     V1CVER        KCSVER                         *VERSIONE
024200970603     C     KEYCPS        SETLL     AZCPS01L
024300970603     C     KEYCPS        READE(N)  AZCPS01L                               99
024400970603DO  3C     *IN99         DOWEQ     *OFF
024500970603IF  4C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
024600000427     C                   Z-ADD     V1CVE2        CPSVER                         *VERSIONE
024700970603     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
024800970603     C                   Z-ADD     *ZEROS        CPSDTR                         *DATA TRASMISSIONE
024900970603     C                   WRITE     AZCPS000
025000970603E   4C                   ENDIF
025100970603     C     KEYCPS        READE(N)  AZCPS01L                               99
025200970603E   3C                   ENDDO
025300130830     C*--------------------
025400130830     C* orari servizi
025500130830     C*--------------------
025600130830     C                   Z-ADD     V1CVER        KCLVER                         *VERSIONE
025700130830     C     KEYCPL        SETLL     AZcos01L
025800130830     C     KEYCPL        READE(N)  AZcos01L                               99
025900130830DO  3C     *IN99         DOWEQ     *OFF
026000130830IF  4C     cosATB        IFEQ      *BLANKS                                      *NO ANNULLATI
026100130830     C                   Z-ADD     V1CVE2        cosVER                         *VERSIONE
026200130830     C                   WRITE     AZcos000
026300130830E   4C                   ENDIF
026400130830     C     KEYCPL        READE(N)  AZcos01L                               99
026500130830E   3C                   ENDDO
026600970603     C*--------------------
026700970603     C* VERSIONE
026800970603     C*--------------------
026900970603     C                   Z-ADD     V1CVE2        CPRVER                         *VERSIONE
027000970603     C                   Z-ADD     DD2V1C        CPRDDE                         *DATA DECORRENZA
027100970603     C                   Z-ADD     DS2V1C        CPRDSC                         *DATA SCADENZA
027200970603     C                   MOVEL     *BLANKS       CPRFTR                         *FLAG TRASMISSIONE
027300970603     C                   Z-ADD     *ZEROS        CPRDTR                         *DATA TRASMISSIONE
027400970603     C                   WRITE     AZCPR000
027500970603     C*
027600970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
027700970603     C                   MOVEL     MSG(20)       D00MSG
027800970603     C                   MOVEL     '1'           $FINE
027900970603E   2C                   ENDIF
028000970603E   1C                   ENDIF
028100130830     C*
028200970603     C                   ENDSR
028300970603     C*--------------------------------------------------------------------------------------------*
028400970603     C* DECODI - CONTROLLA CAMPI E DECODIFICA
028500970603     C*--------------------------------------------------------------------------------------------*
028600970603     C     DECODI        BEGSR
028700970603     C*
028800970603     C* REIMPOSTA INDICATORI DI ERRORE
028900970603     C                   SETOFF                                       606162
029000970603     C*--------------------
029100970603     C* VERSIONE
029200970603     C*--------------------
029300970603IF  1C     $ERR          IFEQ      '0'
029400970611IF  2C     V1CVE2        IFEQ      *ZEROS
029500970603     C                   MOVEL     '1'           $ERR
029600970603     C                   MOVEL     MSG(7)        $MSG
029700970603     C                   EXSR      CARMSG
029800970603     C                   SETON                                        60
029900970603X   2C                   ELSE
030000970603E   2C                   ENDIF
030100970603E   1C                   ENDIF
030200970603     C*--------------------
030300970603     C* DECORRENZA
030400970603     C*--------------------
030500970603IF  1C     $ERR          IFEQ      '0'
030600970603     C                   Z-ADD     V1CDD2        G08DAT
030700970603     C                   Z-ADD     *ZEROS        G08INV
030800970603     C                   MOVEL     '0'           G08ERR
030900970603     C                   CALL      'XSRDA8'
031000970603     C                   PARM                    WLBDA8
031100970603     C                   Z-ADD     G08DAT        V1CDD2
031200970603     C                   Z-ADD     G08INV        DD2V1C
031300970603IF  2C     G08ERR        IFEQ      '1'
031400970603     C                   MOVEL     '1'           $ERR
031500970603     C                   MOVEL     MSG(8)        $MSG
031600970603     C                   EXSR      CARMSG
031700970603     C                   SETON                                        61
031800970603E   2C                   ENDIF
031900970603E   1C                   ENDIF
032000970603     C*--------------------
032100970603     C* SCADENZA
032200970603     C*--------------------
032300970603     C* CORRETTA
032400970603IF  1C     $ERR          IFEQ      '0'
032500970603     C                   Z-ADD     V1CDS2        G08DAT
032600970603     C                   Z-ADD     *ZEROS        G08INV
032700970603     C                   MOVEL     '0'           G08ERR
032800970603     C                   CALL      'XSRDA8'
032900970603     C                   PARM                    WLBDA8
033000970603     C                   Z-ADD     G08DAT        V1CDS2
033100970603     C                   Z-ADD     G08INV        DS2V1C
033200970603IF  2C     G08ERR        IFEQ      '1'
033300970603     C                   MOVEL     '1'           $ERR
033400970603     C                   MOVEL     MSG(8)        $MSG
033500970603     C                   EXSR      CARMSG
033600970603     C                   SETON                                        62
033700970603E   2C                   ENDIF
033800970603E   1C                   ENDIF
033900970603     C* > DELLA DECORRENZA
034000970603IF  1C     $ERR          IFEQ      '0'
034100970603IF  2C     DS2V1C        IFLT      DD2V1C
034200970603     C                   MOVEL     '1'           $ERR
034300970603     C                   MOVEL     MSG(9)        $MSG
034400970603     C                   EXSR      CARMSG
034500970603     C                   SETON                                        62
034600970603E   2C                   ENDIF
034700970603E   1C                   ENDIF
034800970603     C* >= DATA CORRENTE
034900970603IF  1C     $ERR          IFEQ      '0'
035000970603IF  2C     DS2V1C        IFLT      DATCOR
035100970603     C                   MOVEL     '1'           $ERR
035200970603     C                   MOVEL     MSG(12)       $MSG
035300970603     C                   EXSR      CARMSG
035400970603     C                   SETON                                        62
035500970603E   2C                   ENDIF
035600970603E   1C                   ENDIF
035700970603     C*--------------------
035800970603     C* LA NUOVA VERSIONE NON PUO' SOVRAPPORSI AD UNA GIA' IN DECORRENZA
035900970603     C*--------------------
036000970603     C* NUOVA VERSIONE INESISTENTE
036100970603IF  1C     $ERR          IFEQ      '0'
036200970603     C                   Z-ADD     V1CVE2        KCRVER
036300970603     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
036400970603IF  2C     *IN99         IFEQ      *OFF
036500970603     C                   MOVEL     '1'           $ERR
036600970603     C                   MOVEL     MSG(10)       $MSG
036700970603     C                   EXSR      CARMSG
036800970603     C                   SETON                                        60
036900970603E   2C                   ENDIF
037000970603E   1C                   ENDIF
037100970603     C* NUOVA VERSIONE NON ACCAVALLATA
037200970603IF  1C     $ERR          IFEQ      '0'
037300970603     C     *LOVAL        SETLL     AZCPR01L
037400970603     C                   READ(N)   AZCPR01L                               99
037500970603DO  2C     *IN99         DOWEQ     *OFF
037600970603IF  3C     CPRATB        IFEQ      *BLANKS
037700970603IF  4C     DD2V1C        IFGE      CPRDDE                                          *DECORRENZA
037800970603     C     DD2V1C        ANDLE     CPRDSC
037900970603     C     DS2V1C        ORGE      CPRDDE                                          *SCADENZA
038000970603     C     DS2V1C        ANDLE     CPRDSC
038100970603     C     CPRDDE        ORGE      DD2V1C
038200970603     C     CPRDSC        ANDLE     DS2V1C
038300970603     C                   MOVEL     '1'           $ERR
038400970603     C                   MOVEL     MSG(11)       $MSG
038500970603     C                   EXSR      CARMSG
038600970603     C                   SETON                                        606162
038700970603     C                   LEAVE
038800970603E   4C                   ENDIF
038900970603E   3C                   ENDIF
039000970603     C                   READ(N)   AZCPR01L                               99
039100970603E   2C                   ENDDO
039200970603E   1C                   ENDIF
039300970603     C*
039400970603     C                   ENDSR
039500970603     C*--------------------------------------------------------------------------------------------*
039600970603     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
039700970603     C*--------------------------------------------------------------------------------------------*
039800970603     C     IMPVID        BEGSR
039900970603     C*
040000970603     C* CALCOLA LA DATA CORRENTE
040100970603     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
040200970603     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
040300970603     C                   Z-ADD     WNUM8         G08DAT
040400970603     C                   Z-ADD     *ZEROS        G08INV
040500970603     C                   MOVEL     '0'           G08ERR
040600970603     C                   CALL      'XSRDA8'
040700970603     C                   PARM                    WLBDA8
040800970603     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
040900970603     C*
041000970603     C* LEGGE LA VERSIONE
041100970603     C                   Z-ADD     D00VER        KCRVER                         *VERSIONE
041200970603     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
041300970611IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
041400970611     C* DATI DA COPIARE
041500970603     C                   Z-ADD     CPRVER        V1CVER                         *VERSIONE
041600970603     C                   Z-ADD     CPRDDE        G08INV                         *DECORRENZA
041700970603     C                   Z-ADD     *ZEROS        G08DAT
041800970603     C                   MOVEL     '3'           G08ERR
041900970603     C                   CALL      'XSRDA8'
042000970603     C                   PARM                    WLBDA8
042100970603     C                   Z-ADD     G08DAT        V1CDDE
042200970603     C                   Z-ADD     CPRDSC        G08INV                         *SCADENZA
042300970603     C                   Z-ADD     *ZEROS        G08DAT
042400970603     C                   MOVEL     '3'           G08ERR
042500970603     C                   CALL      'XSRDA8'
042600970603     C                   PARM                    WLBDA8
042700970603     C                   Z-ADD     G08DAT        V1CDSC
042800970611     C* DATI SU CUI COPIARE
042900970603     C                   Z-ADD     *ZEROS        V1CVE2                         *VERSIONE
043000970603     C                   Z-ADD     *ZEROS        V1CDD2                         *DECORRENZA
043100970603     C                   Z-ADD     *ZEROS        V1CDS2                         *SCADENZA
043200970611X   1C                   ELSE                                                   *NON TROVATO
043300970611     C                   MOVEL     '1'           $FINE                          *FINE PGM
043400970611     C                   MOVEL     '1'           D00ERR                         *ERRORE
043500970611     C                   MOVEL     MSG(19)       D00MSG                         *MESSSAGGIO
043600970603E   1C                   ENDIF
043700970603     C*
043800970603     C                   ENDSR
043900020919     C*--------------------------------------------------------------------------------------------*
044000020919     C* REPERISCE I DATI UTENTE
044100020919     C*--------------------------------------------------------------------------------------------*
044200020919     C     REPDATIUTE    BEGSR
044300020919     C*
044400020919     C* INIZIALIZZA VARIABILI DI WRK
044500020919     C                   CLEAR                   TIBS34DS
044600020919     C                   CLEAR                   AZUTEDS
044700020919     C                   CLEAR                   DDATIUTE
044800020919     C*
044900020919     C     *DTAARA       DEFINE    §azute        azuteds
045000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
045100020919     C                   IN(E)     *DTAARA
045200020919if  1C                   IF        %Error
045300020919     c                   EVAL      I34Tla = 'L'
045400020919     C                   CALL      'TIBS34R'
045500020919     C                   PARM                    Tibs34Ds
045600020919     C                   IN        *DTAARA
045700020919e   1C                   ENDIF
045800020919     C*
045900020919     C                   ENDSR
046000970603     C*--------------------------------------------------------------------------------------------*
046100970603     C* *INZSR  - ROUTINE INIZIALE
046200970603     C*--------------------------------------------------------------------------------------------*
046300970603     C     *INZSR        BEGSR
046400970603     C*--------------------
046500970603     C* RICEVIMENTO PARAMETRI
046600970603     C*--------------------
046700970603     C     *ENTRY        PLIST
046800970612     C                   PARM                    KPJBA
046900970612     C                   PARM                    DSSI00
047000970603     C*--------------------
047100970603     C* VARIABILI RIFERITE AL DATA BASE
047200970603     C*--------------------
047300970603     C     *LIKE         DEFINE    CPRVER        KCRVER                         *AZCPR01L
047400970603     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC01L
047500970603     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE01L
047600970603     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT01L
047700970603     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL01L
047800970603     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS01L
047900970603     C*--------------------
048000970603     C* VARIABILI NON RIFERITE AL DATA BASE
048100970603     C*--------------------
048200970603     C     *LIKE         DEFINE    V1CDD2        DD2V1C                         *DEPOSITI
048300970603     C     *LIKE         DEFINE    V1CDS2        DS2V1C
048400970603     C*--------------------
048500970603     C* CHIAVI DI LETTURA
048600970603     C*--------------------
048700970603     C* LETTURA AZCPR01L - COMPLETA
048800970603     C     KEYCPR        KLIST
048900970603     C                   KFLD                    KCRVER                         *VERSIONE
049000970603     C* LETTURA AZCPC01L - PARZIALE
049100970603     C     KEYCPC        KLIST
049200970603     C                   KFLD                    KCCVER                         *VERSIONE
049300970603     C* LETTURA AZCPE01L - PARZIALE
049400970603     C     KEYCPE        KLIST
049500970603     C                   KFLD                    KCEVER                         *VERSIONE
049600970603     C* LETTURA AZCPT01L - PARZIALE
049700970603     C     KEYCPT        KLIST
049800970603     C                   KFLD                    KCTVER                         *VERSIONE
049900970603     C* LETTURA AZCPL01L - PARZIALE
050000970603     C     KEYCPL        KLIST
050100970603     C                   KFLD                    KCLVER                         *VERSIONE
050200970603     C* LETTURA AZCPS01L - PARZIALE
050300970603     C     KEYCPS        KLIST
050400970603     C                   KFLD                    KCSVER                         *VERSIONE
050500970603     C***
050600970603     C* IMPOSTAZIONI VIDEO
050700970603     C***
050800970603IF  1C     D00OP0        IFEQ      'R02'
050900970603     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
051000970603X   1C                   ELSE
051100970603     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
051200970603E   1C                   ENDIF
051300970603     C*
051400970603     C* ESEGUE OPERAZIONI INIZIALI STANDARD
051500970603     C                   EXSR      INZSTD
051600970603     C*
051700970603     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
051800970603     C                   EXSR      IMPVID
051900970603     C*
052000970603     C* EMETTO PRIMO FORMATO VIDEO
052100970603     C                   EXSR      CARV1
052200970603     C*
052300970603     C                   ENDSR
052400970603     C*--------------------------------------------------------------------------------------------*
052500970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
052600970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
052700970603     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
052800970603     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
052900970603     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
053000970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053100970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053200970603     C*--------------------------------------------------------------------------------------------*
053300970603     C* INZSTD - ROUTINE INIZIALE STANDARD
053400970603     C*--------------------------------------------------------------------------------------------*
053500970603     C     INZSTD        BEGSR
053600970603     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
053700970603     C                   CALL      'XKEYS'
053800970603     C                   PARM                    DSKEYS
053900970603     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
054000970603     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
054100970603     C     XMSGPA        PLIST
054200970603     C                   PARM                    DSMSMS
054300970603     C                   PARM                    DSMSPG
054400970603     C                   PARM                    DSMSKE
054500970603     C                   PARM                    DSMSER
054600970603     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
054700970603     C                   MOVEL     'TISI48R'     DSMSPG                         *NOME PGM CHIAMANTE
054800970603     C                   MOVEL     'TISI48R'     VMCQUE                         *CODA MESSAGGI DESTI
054900970603     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
055000970603     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
055100970603     C* INIZIALIZZO FLAG CONTROLLO PGM
055200970603     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
055300970603     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
055400970603     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
055500970603     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
055600970603     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
055700970603     C                   MOVEL     '0'           $FINE             1            *FINE PGM
055800970603     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
055900970603     C* LANCIO ROUTINE PER VERIFICARE SE
056000970603     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
056100970603     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
056200020919     C                   EXSR      REPDATIUTE
056300970603     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
056400970603     C                   BITOFF    '01234567'    POSMI1            1
056500970603     C                   BITOFF    '01234567'    POSMI2            1
056600970603     C                   BITOFF    '01234567'    POSMA1            1
056700970603     C                   BITOFF    '01234567'    POSMA2            1
056800970603     C                   BITON     '34'          POSMI1            1            *18
056900970603     C                   BITON     '7'           POSMI2            1            *01
057000970603     C                   BITON     '34'          POSMA1            1            *18
057100970603     C                   BITON     '0'           POSMA2            1            *80
057200970603     C                   MOVE      POSMI2        POSMIN            2
057300970603     C                   MOVEL     POSMI1        POSMIN
057400970603     C                   MOVE      POSMA2        POSMAX            2
057500970603     C                   MOVEL     POSMA1        POSMAX
057600970603     C                   ENDSR
057700970603     C*--------------------------------------------------------------------------------------------*
057800970603     C* FUNSTD - LANCIA SUBROUTINE STANDARD
057900970603     C*--------------------------------------------------------------------------------------------*
058000970603     C     FUNSTD        BEGSR
058100970603     C                   SELECT
058200970603     C     KEY           WHENEQ    F3
058300970603     C                   EXSR      F03STD
058400970603     C     KEY           WHENEQ    F12
058500970603     C                   EXSR      F12STD
058600970603     C     KEY           WHENEQ    F24
058700970603     C                   EXSR      F24STD
058800970603     C                   OTHER
058900970603     C                   MOVEL     MSG(2)        $MSG
059000970603     C                   EXSR      CARMSG
059100970603     C                   ENDSL
059200970603     C                   ENDSR
059300970603     C*--------------------------------------------------------------------------------------------*
059400970603     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
059500970603     C*--------------------------------------------------------------------------------------------*
059600970603     C     F03STD        BEGSR
059700970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
059800970603     C                   MOVEL     '1'           $FINE
059900970603     C* IMPOSTA FLAG DI FINE PROCEDURA
060000970603     C                   MOVEL     '1'           $F03
060100970603     C* PULISCE IL MESSAGGIO
060200970603     C                   MOVEL     *BLANKS       D00MSG
060300970603     C                   ENDSR
060400970603     C*--------------------------------------------------------------------------------------------*
060500970603     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
060600970603     C*--------------------------------------------------------------------------------------------*
060700970603     C     F12STD        BEGSR
060800970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
060900970603     C                   MOVEL     '1'           $FINE
061000970603     C* IMPOSTA FLAG DI FINE PROCEDURA
061100970603     C                   MOVEL     '1'           $F12
061200970603     C* PULISCE IL MESSAGGIO
061300970603     C                   MOVEL     *BLANKS       D00MSG
061400970603     C                   ENDSR
061500970603     C*--------------------------------------------------------------------------------------------*
061600970603     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
061700970603     C*--------------------------------------------------------------------------------------------*
061800970603     C     F24STD        BEGSR
061900970603     C*
062000970603     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
062100970603     C                   EXSR      OUTFUN
062200970603     C*
062300970603     C                   ENDSR
062400970603     C*--------------------------------------------------------------------------------------------*
062500970603     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
062600970603     C*          CODA MESSAGGI DEL PGM
062700970603     C*--------------------------------------------------------------------------------------------*
062800970603     C     CARMSG        BEGSR
062900970603     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
063000970603     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
063100970603     C     MSGULT        IFEQ      0
063200970603     C                   MOVEL     '1'           *IN40
063300970603     C                   WRITE     SI48DVK
063400970603     C                   MOVEL     '0'           *IN40
063500970603     C                   ENDIF
063600970603     C* INVIA MESSAGGIO ALLA CODA DEL PGM
063700970603     C                   MOVEL     $MSG          DSMSMS
063800970603     C                   CALL      'XMSG'        XMSGPA
063900970603     C     DSMSER        IFEQ      0
064000970603     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
064100970603     C                   ENDIF
064200970603     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
064300970603     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
064400970603     C                   WRITE     SI48DVM                                      *SFL MESS
064500970603     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
064600970603     C                   MOVEL     *BLANKS       $MSG
064700970603     C                   ENDSR
064800970603     C*--------------------------------------------------------------------------------------------*
064900970603     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
065000970603     C*--------------------------------------------------------------------------------------------*
065100970603     C     OUTMSG        BEGSR
065200970603     C* VISUALIZZAZIONE MESSAGGIO
065300970603     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
065400970603     C                   WRITE     SI48DVK                                      *CONTR MESS
065500970603     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
065600970603     C                   Z-ADD     *ZEROS        MSGULT
065700970603     C                   ENDSR
065800970603     C*--------------------------------------------------------------------------------------------*
065900970603     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
066000970603     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
066100970603     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
066200970603     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
066300970603     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
066400970603     C*--------------------------------------------------------------------------------------------*
066500970603     C     CARFUN        BEGSR
066600970603     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
066700970603     C     1             DO        31            I                 5 0          --- 1 -->
066800970603     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
066900970603     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
067000970603     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
067100020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
067200970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
067300970603     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
067400970603     C                   ELSE                                                   *INTERROG.
067500970603     C                   MOVEL     FUNASI        FAT(I)
067600970603     C                   ENDIF
067700970603     C                   ELSE                                                   <-- 2 --> *FILIALE
067800970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
067900970603     C                   MOVEL     FUNAFG        FAT(I)
068000970603     C                   ELSE                                                   *INTERROG.
068100970603     C                   MOVEL     FUNAFI        FAT(I)
068200970603     C                   ENDIF
068300970603     C                   ENDIF                                                  <-- 2 ---
068400970603     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
068500970603     C                   MOVEL     USER          WEDP              3            *PROFILO
068600970603     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
068700970603     C                   MOVEL     'S'           FAT(I)                         *ABILITO
068800970603     C                   ELSE
068900970603     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
069000970603     C                   ENDIF
069100970603     C                   ENDIF
069200970603     C                   ENDDO                                                  <-- 1 ---
069300970603     C                   ENDSR
069400970603     C*--------------------------------------------------------------------------------------------*
069500970603     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
069600970603     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
069700970603     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
069800970603     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
069900970603     C*--------------------------------------------------------------------------------------------*
070000970603     C     OUTFUN        BEGSR
070100970603     C     1             DO        8             I                              *PULIZIA FUNZIONI A
070200970603     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
070300970603     C                   ENDDO
070400970603     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
070500970603     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
070600970603     C     1             DO        31            I
070700970603     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
070800970603     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
070900970603     C                   ADD       1             FUNNAT
071000970603     C                   ENDIF
071100970603     C                   ENDDO
071200970603     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
071300970603     C     FUNNAT        IFGT      8
071400970603     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
071500970603     C                   ENDIF
071600970603     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
071700970603     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
071800970603     C                   Z-ADD     8             FUNVIS            5 0
071900970603     C     FAT(23)       IFEQ      'S'
072000970603     C                   SUB       1             FUNVIS
072100970603     C                   ENDIF
072200970603     C     FAT(24)       IFEQ      'S'
072300970603     C                   SUB       1             FUNVIS
072400970603     C                   ENDIF
072500970603     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
072600970603     C                   Z-ADD     *ZEROS        W                 5 0
072700970603     C                   ADD       1             FUNULT
072800970603     C     FUNULT        IFGT      31
072900970603     C                   Z-ADD     1             FUNULT
073000970603     C                   ENDIF
073100970603     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
073200970603     C     FUNULT        DO        31            I                              --- 1 -->
073300970603     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
073400970603     C     W             IFLT      FUNVIS                                       --- 2 -->
073500970603     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
073600970603     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
073700970603     C     FDE(I)        ANDNE     *BLANKS
073800970603     C     I             IFEQ      23                                           *TOLGO F23
073900970603     C     I             OREQ      24                                           *E F24
074000970603     C                   ELSE
074100970603     C                   ADD       1             W
074200970603     C                   MOVEL     FDE(I)        FUV(W)
074300970603     C                   ENDIF
074400970603     C                   ENDIF                                                  <-- 3 ---
074500970603     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
074600970603     C                   Z-ADD     I             FUNULT
074700970603     C                   ENDIF                                                  <-- 2 ---
074800970603     C                   ENDDO                                                  <-- 1 ---
074900970603     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
075000970603     C                   MOVEL     FUV(1)        VFD001
075100970603     C                   MOVEL     FUV(2)        VFD002
075200970603     C                   MOVEL     FUV(3)        VFD003
075300970603     C                   MOVEL     FUV(4)        VFD004
075400970603     C                   MOVEL     FUV(5)        VFD005
075500970603     C                   MOVEL     FUV(6)        VFD006
075600970603     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
075700970603     C     FAT(24)       ANDEQ     'N'
075800970603     C                   MOVEL     FUV(7)        VFD007
075900970603     C                   MOVEL     FUV(8)        VFD008
076000970603     C                   ELSE                                                   <-- 1 -->
076100970603     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
076200970603     C     FAT(24)       ANDEQ     'S'
076300970603     C                   MOVEL     FDE(23)       VFD007
076400970603     C                   MOVEL     FDE(24)       VFD008
076500970603     C                   ELSE                                                   <-- 2 -->
076600970603     C                   MOVEL     FUV(7)        VFD007
076700970603     C     FAT(23)       IFEQ      'S'
076800970603     C                   MOVEL     FDE(23)       VFD008
076900970603     C                   ELSE
077000970603     C                   MOVEL     FDE(24)       VFD008
077100970603     C                   ENDIF
077200970603     C                   ENDIF                                                  <-- 2 ---
077300970603     C                   ENDIF                                                  <-- 1 ---
077400970603     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
077500970603     C                   ENDSR
077600970603     C*--------------------------------------------------------------------------------------------*
077700970603     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
077800970603     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
077900970603     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
078000970603     C*                   - ERRORE SE FUNZIONE NON ATTIVA
078100970603     C*--------------------------------------------------------------------------------------------*
078200970603     C     CONFUN        BEGSR
078300970603     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
078400970603     C     1             DO        31            I                              --- 1 -->
078500970603     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
078600970603     C* CONTROLLO SE IL TASTO E' ATTIVO
078700970603     C     FAT(I)        IFNE      'S'                                          --- 3 -->
078800970603     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
078900970603     C                   EXSR      CARMSG
079000970603     C                   MOVEL     '1'           $ERR
079100970603     C                   ENDIF                                                  <-- 3 ---
079200970603     C                   ENDIF                                                  <-- 2 ---
079300970603     C                   ENDDO                                                  <-- 1 ---
079400970603     C                   ENDSR
079500970603     O*--------------------------------------------------------------------------------------------*
079600970603** -MSG- (MESSAGGI)
079700970603TISI48R- Tasto funzionale non valido.                                          1
079800970603TISI48R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
079900970603TISI48R- Opzione non valida.                                                   3
080000970603TISI48R- Opzione abilitata ma non gestita. Avvertire CED.                      4
080100970603TISI48R- Inizio elenco.                                                        5
080200970603TISI48R- Fine elenco.                                                          6
080300970611TISI48R- Immettere la versione                                                 7
080400970603TISI48R- Data errata                                                           8
080500970603TISI48R- La data scadenza deve essere maggiore della decorrenza                9
080600970603TISI48R- Versione già inserita                                                10
080700970603TISI48R- La versione non può sovrapporsi ad uno già in decorrenza             11
080800970603TISI48R- La versione non può scadere in un giorno già trascorso               12
080900970603TISI48R- 13
081000970603TISI48R- 14
081100970603TISI48R- 15
081200970603TISI48R- 16
081300970603TISI48R- 17
081400970603TISI48R- 18
081500970723TISI48R- Errori in entrata duplica. Non trovato il record da cui copiare      19
081600970603TISI48R- La versione e gli archivi correlati sono stati duplicati             20
081700970603** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
081800970603F1    -                   -    NN     NN
081900970603F2    -                   -    NN     NN
082000970603F3    -F3=Fine            -    NN     NN
082100970603F4    -                   -    NN     NN
082200970603F5    -                   -    NN     NN
082300970603F6    -F6=Conferma        -    SN     SN
082400970603F7    -                   -    NN     NN
082500970603F8    -                   -    NN     NN
082600970603F9    -                   -    NN     NN
082700970603F10   -                   -    NN     NN
082800970603F11   -                   -    NN     NN
082900970603F12   -F12=Ritorno        -    SS     SS
083000970603F13   -                   -    NN     NN
083100970603F14   -                   -    NN     NN
083200970603F15   -                   -    NN     NN
083300970603F16   -                   -    NN     NN
083400970603F17   -                   -    NN     NN
083500970603F18   -                   -    NN     NN
083600970603F19   -                   -    NN     NN
083700970603F20   -                   -    NN     NN
083800970603F21   -                   -    NN     NN
083900970603F22   -                   -    NN     NN
084000970603F23   -                   -    NN     NN
084100970603F24   -F24=Altri tasti    -    NN     NN
084200970603ENTER -                   -    SS     SS
084300970603ROLLUP-                   -    NN     NN
084400970603ROLLDN-                   -    NN     NN
084500970603HELP  -                   -    NN     NN
084600970603PRINT -                   -    NN     NN
084700970603BSPACE-                   -    NN     NN
084800970603CLEAR -                   -    NN     NN

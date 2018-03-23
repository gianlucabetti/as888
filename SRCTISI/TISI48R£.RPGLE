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
001600970603     D*--------------------------------------------------------------------------------------------*
001700970603     D* SCHIERE
001800970603     D*--------------------------------------------------------------------------------------------*
001900970603     D*-------------------
002000970603     D* MESSAGGI E FUNZIONI
002100970603     D*-------------------
002200970603     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002300970603     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002400970603     D*-------------------
002500970603     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600970603     D*-------------------
002700970603     D FFU             S              6    DIM(31)                              *FUNZIONE
002800970603     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002900970603     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003000970603     D*-------------------
003100970603     D* TABELLA DESCRIZIONI A VIDEO
003200970603     D*-------------------
003300970603     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003400970603     D*-------------------
003500970603     D* TABELLA TASTI FUNZIONALI PREMUTI
003600970603     D*-------------------
003700970603     D KEYS            S              1    DIM(40)
003800970603     D*--------------------------------------------------------------------------------------------*
003900970603     D* INPUT
004000970603     D*--------------------------------------------------------------------------------------------*
004100970603     D*-------------------
004200970603     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300970603     D*-------------------
004400970603     D DSFUN         E DS
004500970603     D  FUNRIG                 1     40
004600970603     D*-------------------
004700970603     D* DS PER GESTIONE TASTI FUNZIONALI
004800970603     D*-------------------
004900970603     D DSKEYS        E DS
005000970603     D  KEYRIG                 1     40
005100970603     D*-------------------
005200970603     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005300970603     D*-------------------
005400970603     D                SDS
005500970603     D  USER                 254    263
005600970603     D*-------------------
005700970603     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005800970603     D*-------------------
005900970603     D DEVDS1          DS
006000970603     D  KEY                  369    369
006100970603     D  CURSOR               370    371
006200970603     D*---------------------
006300970603     D* ARCHITETTURA
006400970603     D*---------------------
006500970603     D KPJBA         E DS
006600970603     D*---------------------
006700970603     D* DS ESTERNA DI PROCEDURA
006800970603     D*---------------------
006900970603     D DSSI00        E DS                  EXTNAME(TISI00DS)
007000970603     D*-------------------
007100020919     D* DS REPERIMENTO DATI UTENTE
007200970603     D*-------------------
007300020919     D TIBS34DS      E DS                                                       *Profili utente
007400020919     D DDATIUTE      E DS                                                       *Dati utente
007500020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007600970603     D*-------------------
007700970603     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007800970603     D*-------------------
007900970603     D DSMSG         E DS
008000970603     D*------------------
008100970603     D* DS "XSRDA8" - CONTROLLA DATA (8)
008200970603     D*------------------
008300970603     D WLBDA8          DS
008400970603     D  G08DAT                 1      8  0
008500970603     D  G08INV                 9     16  0
008600970603     D  G08ERR                17     17
008700970603     D  G08TGI                18     22  0
008800970603     C*--------------------------------------------------------------------------------------------*
008900970603     C* MAIN LINE
009000970603     C*--------------------------------------------------------------------------------------------*
009100970603     C*
009200970611     C* SE NON CI SONO ERRORI IN ENTRATA
009300970611IF  1C     $FINE         IFEQ      '0'
009400970603     C                   WRITE     SI48DVT                                      *TESTATA
009500970611DO  2C     $FINE         DOUEQ     '1'
009600970603     C*
009700970603     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009800970611IF  3C     $GES          IFEQ      'V1'
009900970603     C                   EXSR      GESV1
010000970611E   3C                   ENDIF
010100970603     C*
010200970611E   2C                   ENDDO
010300970603     C*
010400970611IF  2C     $F03          IFEQ      '1'
010500970603     C                   MOVEL     '1'           D00F03
010600970611E   2C                   ENDIF
010700970611E   1C                   ENDIF
010800970603     C* FINE PGM
010900970603     C                   SETON                                        LR
011000970603     C*--------------------------------------------------------------------------------------------*
011100970603     C* GESV1 - GESTIONE VIDEO UNO
011200970603     C*--------------------------------------------------------------------------------------------*
011300970603     C     GESV1         BEGSR
011400970603     C*
011500970603IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
011600970603     C                   EXSR      CARV1                                        *CARICA VIDEO
011700970603E   1C                   ENDIF
011800970603     C*
011900970603IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012000970603     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012100970603     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012200970603E   1C                   ENDIF
012300970603     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012400970603     C*
012500970603     C                   EXFMT     SI48DV1                                      *PARAMETRI
012600970603     C*
012700970603     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012800970603     C*
012900970603IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013000970603SL  2C                   SELECT
013100970603     C     KEY           WHENEQ    ENTER
013200970603     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013300970603     C     KEY           WHENEQ    F6
013400970603     C                   EXSR      F06GES
013500970603     C                   OTHER
013600970603     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013700970603E   2C                   ENDSL
013800970603E   1C                   ENDIF
013900970603     C*
014000970603     C                   ENDSR
014100970603     C*--------------------------------------------------------------------------------------------*
014200970603     C* CARV1 - CARICA VIDEO UNO
014300970603     C*--------------------------------------------------------------------------------------------*
014400970603     C     CARV1         BEGSR
014500970603     C*
014600970603     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014700970603     C*
014800970603     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014900970603     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015000970603     C*
015100970603     C*---                 WRITESI48DVT                    *TESTATA
015200970603     C*---                 WRITESI48DVF                    *FUNZIONI
015300970603     C*
015400970603     C                   ENDSR
015500970603     C*--------------------------------------------------------------------------------------------*
015600970603     C* ENTV1 - GESTIONE TASTO ENTER
015700970603     C*--------------------------------------------------------------------------------------------*
015800970603     C     ENTV1         BEGSR
015900970603     C*
016000970603     C* CONTROLLA I CAMPI IMMESSI
016100970603     C                   EXSR      DECODI
016200970603     C*
016300970603     C                   ENDSR
016400970603     C*--------------------------------------------------------------------------------------------*
016500970603     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
016600970603     C*--------------------------------------------------------------------------------------------*
016700970603     C     F06GES        BEGSR
016800970603     C*
016900970603     C* CONTROLLA I CAMPI IMMESSI
017000970603     C                   EXSR      DECODI
017100970603     C*
017200970603     C* RILEGGE LA VERSIONE DA CUI COPIARE
017300970603IF  1C     $ERR          IFEQ      '0'
017400970603     C                   Z-ADD     V1CVER        KCRVER
017500970603     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
017600970603IF  2C     *IN99         IFEQ      *OFF
017700970603     C*--------------------
017800970603     C* CAPPARIO
017900970603     C*--------------------
018000970603     C                   Z-ADD     V1CVER        KCCVER                         *VERSIONE
018100970603     C     KEYCPC        SETLL     AZCPC01L
018200970603     C     KEYCPC        READE(N)  AZCPC01L                               99
018300970603DO  3C     *IN99         DOWEQ     *OFF
018400970603IF  4C     CPCATB        IFEQ      *BLANKS                                      *NO ANNULLATI
018500000427     C                   Z-ADD     V1CVE2        CPCVER                         *VERSIONE
018600000622     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
018700970603     C                   Z-ADD     *ZEROS        CPCDTR                         *DATA TRASMISSIONE
018800970603     C                   WRITE     AZCPC000
018900970603E   4C                   ENDIF
019000970603     C     KEYCPC        READE(N)  AZCPC01L                               99
019100970603E   3C                   ENDDO
019200970603     C*--------------------
019300970603     C* ECCEZIONI LINEE/ZONE
019400970603     C*--------------------
019500970603     C                   Z-ADD     V1CVER        KCEVER                         *VERSIONE
019600970603     C     KEYCPE        SETLL     AZCPE01L
019700970603     C     KEYCPE        READE(N)  AZCPE01L                               99
019800970603DO  3C     *IN99         DOWEQ     *OFF
019900970603IF  4C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
020000000427     C                   Z-ADD     V1CVE2        CPEVER                         *VERSIONE
020100970603     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
020200970603     C                   Z-ADD     *ZEROS        CPEDTR                         *DATA TRASMISSIONE
020300970603     C                   WRITE     AZCPE000
020400970603E   4C                   ENDIF
020500970603     C     KEYCPE        READE(N)  AZCPE01L                               99
020600970603E   3C                   ENDDO
020700970603     C*--------------------
020800970603     C* TEMPI CONSEGNA
020900970603     C*--------------------
021000970603     C                   Z-ADD     V1CVER        KCTVER                         *VERSIONE
021100970603     C     KEYCPT        SETLL     AZCPT01L
021200970603     C     KEYCPT        READE(N)  AZCPT01L                               99
021300970603DO  3C     *IN99         DOWEQ     *OFF
021400970603IF  4C     CPTATB        IFEQ      *BLANKS                                      *NO ANNULLATI
021500000427     C                   Z-ADD     V1CVE2        CPTVER                         *VERSIONE
021600970603     C                   MOVEL     *BLANKS       CPTFTR                         *FLAG TRASMISSIONE
021700970603     C                   Z-ADD     *ZEROS        CPTDTR                         *DATA TRASMISSIONE
021800970603     C                   WRITE     AZCPT000
021900970603E   4C                   ENDIF
022000970603     C     KEYCPT        READE(N)  AZCPT01L                               99
022100970603E   3C                   ENDDO
022200970603     C*--------------------
022300970603     C* LOCALITA'
022400970603     C*--------------------
022500970603     C                   Z-ADD     V1CVER        KCLVER                         *VERSIONE
022600970603     C     KEYCPL        SETLL     AZCPL01L
022700970603     C     KEYCPL        READE(N)  AZCPL01L                               99
022800970603DO  3C     *IN99         DOWEQ     *OFF
022900970603IF  4C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
023000000427     C                   Z-ADD     V1CVE2        CPLVER                         *VERSIONE
023100970603     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
023200970603     C                   Z-ADD     *ZEROS        CPLDTR                         *DATA TRASMISSIONE
023300970603     C                   WRITE     AZCPL000
023400970603E   4C                   ENDIF
023500970603     C     KEYCPL        READE(N)  AZCPL01L                               99
023600970603E   3C                   ENDDO
023700970603     C*--------------------
023800970603     C* SINONIMI LOCALITA'
023900970603     C*--------------------
024000970603     C                   Z-ADD     V1CVER        KCSVER                         *VERSIONE
024100970603     C     KEYCPS        SETLL     AZCPS01L
024200970603     C     KEYCPS        READE(N)  AZCPS01L                               99
024300970603DO  3C     *IN99         DOWEQ     *OFF
024400970603IF  4C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
024500000427     C                   Z-ADD     V1CVE2        CPSVER                         *VERSIONE
024600970603     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
024700970603     C                   Z-ADD     *ZEROS        CPSDTR                         *DATA TRASMISSIONE
024800970603     C                   WRITE     AZCPS000
024900970603E   4C                   ENDIF
025000970603     C     KEYCPS        READE(N)  AZCPS01L                               99
025100970603E   3C                   ENDDO
025200970603     C*--------------------
025300970603     C* VERSIONE
025400970603     C*--------------------
025500970603     C                   Z-ADD     V1CVE2        CPRVER                         *VERSIONE
025600970603     C                   Z-ADD     DD2V1C        CPRDDE                         *DATA DECORRENZA
025700970603     C                   Z-ADD     DS2V1C        CPRDSC                         *DATA SCADENZA
025800970603     C                   MOVEL     *BLANKS       CPRFTR                         *FLAG TRASMISSIONE
025900970603     C                   Z-ADD     *ZEROS        CPRDTR                         *DATA TRASMISSIONE
026000970603     C                   WRITE     AZCPR000
026100970603     C*
026200970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
026300970603     C                   MOVEL     MSG(20)       D00MSG
026400970603     C                   MOVEL     '1'           $FINE
026500970603E   2C                   ENDIF
026600970603E   1C                   ENDIF
026700970603     C*
026800970603     C                   ENDSR
026900970603     C*--------------------------------------------------------------------------------------------*
027000970603     C* DECODI - CONTROLLA CAMPI E DECODIFICA
027100970603     C*--------------------------------------------------------------------------------------------*
027200970603     C     DECODI        BEGSR
027300970603     C*
027400970603     C* REIMPOSTA INDICATORI DI ERRORE
027500970603     C                   SETOFF                                       606162
027600970603     C*--------------------
027700970603     C* VERSIONE
027800970603     C*--------------------
027900970603IF  1C     $ERR          IFEQ      '0'
028000970611IF  2C     V1CVE2        IFEQ      *ZEROS
028100970603     C                   MOVEL     '1'           $ERR
028200970603     C                   MOVEL     MSG(7)        $MSG
028300970603     C                   EXSR      CARMSG
028400970603     C                   SETON                                        60
028500970603X   2C                   ELSE
028600970603E   2C                   ENDIF
028700970603E   1C                   ENDIF
028800970603     C*--------------------
028900970603     C* DECORRENZA
029000970603     C*--------------------
029100970603IF  1C     $ERR          IFEQ      '0'
029200970603     C                   Z-ADD     V1CDD2        G08DAT
029300970603     C                   Z-ADD     *ZEROS        G08INV
029400970603     C                   MOVEL     '0'           G08ERR
029500970603     C                   CALL      'XSRDA8'
029600970603     C                   PARM                    WLBDA8
029700970603     C                   Z-ADD     G08DAT        V1CDD2
029800970603     C                   Z-ADD     G08INV        DD2V1C
029900970603IF  2C     G08ERR        IFEQ      '1'
030000970603     C                   MOVEL     '1'           $ERR
030100970603     C                   MOVEL     MSG(8)        $MSG
030200970603     C                   EXSR      CARMSG
030300970603     C                   SETON                                        61
030400970603E   2C                   ENDIF
030500970603E   1C                   ENDIF
030600970603     C*--------------------
030700970603     C* SCADENZA
030800970603     C*--------------------
030900970603     C* CORRETTA
031000970603IF  1C     $ERR          IFEQ      '0'
031100970603     C                   Z-ADD     V1CDS2        G08DAT
031200970603     C                   Z-ADD     *ZEROS        G08INV
031300970603     C                   MOVEL     '0'           G08ERR
031400970603     C                   CALL      'XSRDA8'
031500970603     C                   PARM                    WLBDA8
031600970603     C                   Z-ADD     G08DAT        V1CDS2
031700970603     C                   Z-ADD     G08INV        DS2V1C
031800970603IF  2C     G08ERR        IFEQ      '1'
031900970603     C                   MOVEL     '1'           $ERR
032000970603     C                   MOVEL     MSG(8)        $MSG
032100970603     C                   EXSR      CARMSG
032200970603     C                   SETON                                        62
032300970603E   2C                   ENDIF
032400970603E   1C                   ENDIF
032500970603     C* > DELLA DECORRENZA
032600970603IF  1C     $ERR          IFEQ      '0'
032700970603IF  2C     DS2V1C        IFLT      DD2V1C
032800970603     C                   MOVEL     '1'           $ERR
032900970603     C                   MOVEL     MSG(9)        $MSG
033000970603     C                   EXSR      CARMSG
033100970603     C                   SETON                                        62
033200970603E   2C                   ENDIF
033300970603E   1C                   ENDIF
033400970603     C* >= DATA CORRENTE
033500970603IF  1C     $ERR          IFEQ      '0'
033600970603IF  2C     DS2V1C        IFLT      DATCOR
033700970603     C                   MOVEL     '1'           $ERR
033800970603     C                   MOVEL     MSG(12)       $MSG
033900970603     C                   EXSR      CARMSG
034000970603     C                   SETON                                        62
034100970603E   2C                   ENDIF
034200970603E   1C                   ENDIF
034300970603     C*--------------------
034400970603     C* LA NUOVA VERSIONE NON PUO' SOVRAPPORSI AD UNA GIA' IN DECORRENZA
034500970603     C*--------------------
034600970603     C* NUOVA VERSIONE INESISTENTE
034700970603IF  1C     $ERR          IFEQ      '0'
034800970603     C                   Z-ADD     V1CVE2        KCRVER
034900970603     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
035000970603IF  2C     *IN99         IFEQ      *OFF
035100970603     C                   MOVEL     '1'           $ERR
035200970603     C                   MOVEL     MSG(10)       $MSG
035300970603     C                   EXSR      CARMSG
035400970603     C                   SETON                                        60
035500970603E   2C                   ENDIF
035600970603E   1C                   ENDIF
035700970603     C* NUOVA VERSIONE NON ACCAVALLATA
035800970603IF  1C     $ERR          IFEQ      '0'
035900970603     C     *LOVAL        SETLL     AZCPR01L
036000970603     C                   READ(N)   AZCPR01L                               99
036100970603DO  2C     *IN99         DOWEQ     *OFF
036200970603IF  3C     CPRATB        IFEQ      *BLANKS
036300970603IF  4C     DD2V1C        IFGE      CPRDDE                                          *DECORRENZA
036400970603     C     DD2V1C        ANDLE     CPRDSC
036500970603     C     DS2V1C        ORGE      CPRDDE                                          *SCADENZA
036600970603     C     DS2V1C        ANDLE     CPRDSC
036700970603     C     CPRDDE        ORGE      DD2V1C
036800970603     C     CPRDSC        ANDLE     DS2V1C
036900970603     C                   MOVEL     '1'           $ERR
037000970603     C                   MOVEL     MSG(11)       $MSG
037100970603     C                   EXSR      CARMSG
037200970603     C                   SETON                                        606162
037300970603     C                   LEAVE
037400970603E   4C                   ENDIF
037500970603E   3C                   ENDIF
037600970603     C                   READ(N)   AZCPR01L                               99
037700970603E   2C                   ENDDO
037800970603E   1C                   ENDIF
037900970603     C*
038000970603     C                   ENDSR
038100970603     C*--------------------------------------------------------------------------------------------*
038200970603     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
038300970603     C*--------------------------------------------------------------------------------------------*
038400970603     C     IMPVID        BEGSR
038500970603     C*
038600970603     C* CALCOLA LA DATA CORRENTE
038700970603     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
038800970603     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
038900970603     C                   Z-ADD     WNUM8         G08DAT
039000970603     C                   Z-ADD     *ZEROS        G08INV
039100970603     C                   MOVEL     '0'           G08ERR
039200970603     C                   CALL      'XSRDA8'
039300970603     C                   PARM                    WLBDA8
039400970603     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
039500970603     C*
039600970603     C* LEGGE LA VERSIONE
039700970603     C                   Z-ADD     D00VER        KCRVER                         *VERSIONE
039800970603     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
039900970611IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
040000970611     C* DATI DA COPIARE
040100970603     C                   Z-ADD     CPRVER        V1CVER                         *VERSIONE
040200970603     C                   Z-ADD     CPRDDE        G08INV                         *DECORRENZA
040300970603     C                   Z-ADD     *ZEROS        G08DAT
040400970603     C                   MOVEL     '3'           G08ERR
040500970603     C                   CALL      'XSRDA8'
040600970603     C                   PARM                    WLBDA8
040700970603     C                   Z-ADD     G08DAT        V1CDDE
040800970603     C                   Z-ADD     CPRDSC        G08INV                         *SCADENZA
040900970603     C                   Z-ADD     *ZEROS        G08DAT
041000970603     C                   MOVEL     '3'           G08ERR
041100970603     C                   CALL      'XSRDA8'
041200970603     C                   PARM                    WLBDA8
041300970603     C                   Z-ADD     G08DAT        V1CDSC
041400970611     C* DATI SU CUI COPIARE
041500970603     C                   Z-ADD     *ZEROS        V1CVE2                         *VERSIONE
041600970603     C                   Z-ADD     *ZEROS        V1CDD2                         *DECORRENZA
041700970603     C                   Z-ADD     *ZEROS        V1CDS2                         *SCADENZA
041800970611X   1C                   ELSE                                                   *NON TROVATO
041900970611     C                   MOVEL     '1'           $FINE                          *FINE PGM
042000970611     C                   MOVEL     '1'           D00ERR                         *ERRORE
042100970611     C                   MOVEL     MSG(19)       D00MSG                         *MESSSAGGIO
042200970603E   1C                   ENDIF
042300970603     C*
042400970603     C                   ENDSR
042500020919     C*--------------------------------------------------------------------------------------------*
042600020919     C* REPERISCE I DATI UTENTE
042700020919     C*--------------------------------------------------------------------------------------------*
042800020919     C     REPDATIUTE    BEGSR
042900020919     C*
043000020919     C* INIZIALIZZA VARIABILI DI WRK
043100020919     C                   CLEAR                   TIBS34DS
043200020919     C                   CLEAR                   AZUTEDS
043300020919     C                   CLEAR                   DDATIUTE
043400020919     C*
043500020919     C     *DTAARA       DEFINE    §azute        azuteds
043600020919     C     *DTAARA       DEFINE    §datiute      ddatiute
043700020919     C                   IN(E)     *DTAARA
043800020919if  1C                   IF        %Error
043900020919     c                   EVAL      I34Tla = 'L'
044000020919     C                   CALL      'TIBS34R'
044100020919     C                   PARM                    Tibs34Ds
044200020919     C                   IN        *DTAARA
044300020919e   1C                   ENDIF
044400020919     C*
044500020919     C                   ENDSR
044600970603     C*--------------------------------------------------------------------------------------------*
044700970603     C* *INZSR  - ROUTINE INIZIALE
044800970603     C*--------------------------------------------------------------------------------------------*
044900970603     C     *INZSR        BEGSR
045000970603     C*--------------------
045100970603     C* RICEVIMENTO PARAMETRI
045200970603     C*--------------------
045300970603     C     *ENTRY        PLIST
045400970612     C                   PARM                    KPJBA
045500970612     C                   PARM                    DSSI00
045600970603     C*--------------------
045700970603     C* VARIABILI RIFERITE AL DATA BASE
045800970603     C*--------------------
045900970603     C     *LIKE         DEFINE    CPRVER        KCRVER                         *AZCPR01L
046000970603     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC01L
046100970603     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE01L
046200970603     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT01L
046300970603     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL01L
046400970603     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS01L
046500970603     C*--------------------
046600970603     C* VARIABILI NON RIFERITE AL DATA BASE
046700970603     C*--------------------
046800970603     C     *LIKE         DEFINE    V1CDD2        DD2V1C                         *DEPOSITI
046900970603     C     *LIKE         DEFINE    V1CDS2        DS2V1C
047000970603     C*--------------------
047100970603     C* CHIAVI DI LETTURA
047200970603     C*--------------------
047300970603     C* LETTURA AZCPR01L - COMPLETA
047400970603     C     KEYCPR        KLIST
047500970603     C                   KFLD                    KCRVER                         *VERSIONE
047600970603     C* LETTURA AZCPC01L - PARZIALE
047700970603     C     KEYCPC        KLIST
047800970603     C                   KFLD                    KCCVER                         *VERSIONE
047900970603     C* LETTURA AZCPE01L - PARZIALE
048000970603     C     KEYCPE        KLIST
048100970603     C                   KFLD                    KCEVER                         *VERSIONE
048200970603     C* LETTURA AZCPT01L - PARZIALE
048300970603     C     KEYCPT        KLIST
048400970603     C                   KFLD                    KCTVER                         *VERSIONE
048500970603     C* LETTURA AZCPL01L - PARZIALE
048600970603     C     KEYCPL        KLIST
048700970603     C                   KFLD                    KCLVER                         *VERSIONE
048800970603     C* LETTURA AZCPS01L - PARZIALE
048900970603     C     KEYCPS        KLIST
049000970603     C                   KFLD                    KCSVER                         *VERSIONE
049100970603     C***
049200970603     C* IMPOSTAZIONI VIDEO
049300970603     C***
049400970603IF  1C     D00OP0        IFEQ      'R02'
049500970603     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
049600970603X   1C                   ELSE
049700970603     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
049800970603E   1C                   ENDIF
049900970603     C*
050000970603     C* ESEGUE OPERAZIONI INIZIALI STANDARD
050100970603     C                   EXSR      INZSTD
050200970603     C*
050300970603     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
050400970603     C                   EXSR      IMPVID
050500970603     C*
050600970603     C* EMETTO PRIMO FORMATO VIDEO
050700970603     C                   EXSR      CARV1
050800970603     C*
050900970603     C                   ENDSR
051000970603     C*--------------------------------------------------------------------------------------------*
051100970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051200970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051300970603     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
051400970603     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
051500970603     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
051600970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051700970603     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051800970603     C*--------------------------------------------------------------------------------------------*
051900970603     C* INZSTD - ROUTINE INIZIALE STANDARD
052000970603     C*--------------------------------------------------------------------------------------------*
052100970603     C     INZSTD        BEGSR
052200970603     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
052300970603     C                   CALL      'XKEYS'
052400970603     C                   PARM                    DSKEYS
052500970603     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
052600970603     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
052700970603     C     XMSGPA        PLIST
052800970603     C                   PARM                    DSMSMS
052900970603     C                   PARM                    DSMSPG
053000970603     C                   PARM                    DSMSKE
053100970603     C                   PARM                    DSMSER
053200970603     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
053300970603     C                   MOVEL     'TISI48R'     DSMSPG                         *NOME PGM CHIAMANTE
053400970603     C                   MOVEL     'TISI48R'     VMCQUE                         *CODA MESSAGGI DESTI
053500970603     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
053600970603     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
053700970603     C* INIZIALIZZO FLAG CONTROLLO PGM
053800970603     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
053900970603     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
054000970603     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
054100970603     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
054200970603     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
054300970603     C                   MOVEL     '0'           $FINE             1            *FINE PGM
054400970603     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
054500970603     C* LANCIO ROUTINE PER VERIFICARE SE
054600970603     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
054700970603     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
054800020919     C                   EXSR      REPDATIUTE
054900970603     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
055000970603     C                   BITOFF    '01234567'    POSMI1            1
055100970603     C                   BITOFF    '01234567'    POSMI2            1
055200970603     C                   BITOFF    '01234567'    POSMA1            1
055300970603     C                   BITOFF    '01234567'    POSMA2            1
055400970603     C                   BITON     '34'          POSMI1            1            *18
055500970603     C                   BITON     '7'           POSMI2            1            *01
055600970603     C                   BITON     '34'          POSMA1            1            *18
055700970603     C                   BITON     '0'           POSMA2            1            *80
055800970603     C                   MOVE      POSMI2        POSMIN            2
055900970603     C                   MOVEL     POSMI1        POSMIN
056000970603     C                   MOVE      POSMA2        POSMAX            2
056100970603     C                   MOVEL     POSMA1        POSMAX
056200970603     C                   ENDSR
056300970603     C*--------------------------------------------------------------------------------------------*
056400970603     C* FUNSTD - LANCIA SUBROUTINE STANDARD
056500970603     C*--------------------------------------------------------------------------------------------*
056600970603     C     FUNSTD        BEGSR
056700970603     C                   SELECT
056800970603     C     KEY           WHENEQ    F3
056900970603     C                   EXSR      F03STD
057000970603     C     KEY           WHENEQ    F12
057100970603     C                   EXSR      F12STD
057200970603     C     KEY           WHENEQ    F24
057300970603     C                   EXSR      F24STD
057400970603     C                   OTHER
057500970603     C                   MOVEL     MSG(2)        $MSG
057600970603     C                   EXSR      CARMSG
057700970603     C                   ENDSL
057800970603     C                   ENDSR
057900970603     C*--------------------------------------------------------------------------------------------*
058000970603     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
058100970603     C*--------------------------------------------------------------------------------------------*
058200970603     C     F03STD        BEGSR
058300970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
058400970603     C                   MOVEL     '1'           $FINE
058500970603     C* IMPOSTA FLAG DI FINE PROCEDURA
058600970603     C                   MOVEL     '1'           $F03
058700970603     C* PULISCE IL MESSAGGIO
058800970603     C                   MOVEL     *BLANKS       D00MSG
058900970603     C                   ENDSR
059000970603     C*--------------------------------------------------------------------------------------------*
059100970603     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
059200970603     C*--------------------------------------------------------------------------------------------*
059300970603     C     F12STD        BEGSR
059400970603     C* IMPOSTA FLAG DI FINE PROGRAMMA
059500970603     C                   MOVEL     '1'           $FINE
059600970603     C* IMPOSTA FLAG DI FINE PROCEDURA
059700970603     C                   MOVEL     '1'           $F12
059800970603     C* PULISCE IL MESSAGGIO
059900970603     C                   MOVEL     *BLANKS       D00MSG
060000970603     C                   ENDSR
060100970603     C*--------------------------------------------------------------------------------------------*
060200970603     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
060300970603     C*--------------------------------------------------------------------------------------------*
060400970603     C     F24STD        BEGSR
060500970603     C*
060600970603     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
060700970603     C                   EXSR      OUTFUN
060800970603     C*
060900970603     C                   ENDSR
061000970603     C*--------------------------------------------------------------------------------------------*
061100970603     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
061200970603     C*          CODA MESSAGGI DEL PGM
061300970603     C*--------------------------------------------------------------------------------------------*
061400970603     C     CARMSG        BEGSR
061500970603     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
061600970603     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
061700970603     C     MSGULT        IFEQ      0
061800970603     C                   MOVEL     '1'           *IN40
061900970603     C                   WRITE     SI48DVK
062000970603     C                   MOVEL     '0'           *IN40
062100970603     C                   ENDIF
062200970603     C* INVIA MESSAGGIO ALLA CODA DEL PGM
062300970603     C                   MOVEL     $MSG          DSMSMS
062400970603     C                   CALL      'XMSG'        XMSGPA
062500970603     C     DSMSER        IFEQ      0
062600970603     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
062700970603     C                   ENDIF
062800970603     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
062900970603     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
063000970603     C                   WRITE     SI48DVM                                      *SFL MESS
063100970603     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
063200970603     C                   MOVEL     *BLANKS       $MSG
063300970603     C                   ENDSR
063400970603     C*--------------------------------------------------------------------------------------------*
063500970603     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
063600970603     C*--------------------------------------------------------------------------------------------*
063700970603     C     OUTMSG        BEGSR
063800970603     C* VISUALIZZAZIONE MESSAGGIO
063900970603     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
064000970603     C                   WRITE     SI48DVK                                      *CONTR MESS
064100970603     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
064200970603     C                   Z-ADD     *ZEROS        MSGULT
064300970603     C                   ENDSR
064400970603     C*--------------------------------------------------------------------------------------------*
064500970603     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
064600970603     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
064700970603     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
064800970603     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
064900970603     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
065000970603     C*--------------------------------------------------------------------------------------------*
065100970603     C     CARFUN        BEGSR
065200970603     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
065300970603     C     1             DO        31            I                 5 0          --- 1 -->
065400970603     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
065500970603     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
065600970603     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
065700020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
065800970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
065900970603     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
066000970603     C                   ELSE                                                   *INTERROG.
066100970603     C                   MOVEL     FUNASI        FAT(I)
066200970603     C                   ENDIF
066300970603     C                   ELSE                                                   <-- 2 --> *FILIALE
066400970603     C     $TGES         IFEQ      'G'                                          *GESTIONE
066500970603     C                   MOVEL     FUNAFG        FAT(I)
066600970603     C                   ELSE                                                   *INTERROG.
066700970603     C                   MOVEL     FUNAFI        FAT(I)
066800970603     C                   ENDIF
066900970603     C                   ENDIF                                                  <-- 2 ---
067000970603     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
067100970603     C                   MOVEL     USER          WEDP              3            *PROFILO
067200970603     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
067300970603     C                   MOVEL     'S'           FAT(I)                         *ABILITO
067400970603     C                   ELSE
067500970603     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
067600970603     C                   ENDIF
067700970603     C                   ENDIF
067800970603     C                   ENDDO                                                  <-- 1 ---
067900970603     C                   ENDSR
068000970603     C*--------------------------------------------------------------------------------------------*
068100970603     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
068200970603     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
068300970603     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
068400970603     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
068500970603     C*--------------------------------------------------------------------------------------------*
068600970603     C     OUTFUN        BEGSR
068700970603     C     1             DO        8             I                              *PULIZIA FUNZIONI A
068800970603     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
068900970603     C                   ENDDO
069000970603     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
069100970603     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
069200970603     C     1             DO        31            I
069300970603     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
069400970603     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
069500970603     C                   ADD       1             FUNNAT
069600970603     C                   ENDIF
069700970603     C                   ENDDO
069800970603     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
069900970603     C     FUNNAT        IFGT      8
070000970603     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
070100970603     C                   ENDIF
070200970603     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
070300970603     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
070400970603     C                   Z-ADD     8             FUNVIS            5 0
070500970603     C     FAT(23)       IFEQ      'S'
070600970603     C                   SUB       1             FUNVIS
070700970603     C                   ENDIF
070800970603     C     FAT(24)       IFEQ      'S'
070900970603     C                   SUB       1             FUNVIS
071000970603     C                   ENDIF
071100970603     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
071200970603     C                   Z-ADD     *ZEROS        W                 5 0
071300970603     C                   ADD       1             FUNULT
071400970603     C     FUNULT        IFGT      31
071500970603     C                   Z-ADD     1             FUNULT
071600970603     C                   ENDIF
071700970603     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
071800970603     C     FUNULT        DO        31            I                              --- 1 -->
071900970603     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
072000970603     C     W             IFLT      FUNVIS                                       --- 2 -->
072100970603     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
072200970603     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
072300970603     C     FDE(I)        ANDNE     *BLANKS
072400970603     C     I             IFEQ      23                                           *TOLGO F23
072500970603     C     I             OREQ      24                                           *E F24
072600970603     C                   ELSE
072700970603     C                   ADD       1             W
072800970603     C                   MOVEL     FDE(I)        FUV(W)
072900970603     C                   ENDIF
073000970603     C                   ENDIF                                                  <-- 3 ---
073100970603     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
073200970603     C                   Z-ADD     I             FUNULT
073300970603     C                   ENDIF                                                  <-- 2 ---
073400970603     C                   ENDDO                                                  <-- 1 ---
073500970603     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
073600970603     C                   MOVEL     FUV(1)        VFD001
073700970603     C                   MOVEL     FUV(2)        VFD002
073800970603     C                   MOVEL     FUV(3)        VFD003
073900970603     C                   MOVEL     FUV(4)        VFD004
074000970603     C                   MOVEL     FUV(5)        VFD005
074100970603     C                   MOVEL     FUV(6)        VFD006
074200970603     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
074300970603     C     FAT(24)       ANDEQ     'N'
074400970603     C                   MOVEL     FUV(7)        VFD007
074500970603     C                   MOVEL     FUV(8)        VFD008
074600970603     C                   ELSE                                                   <-- 1 -->
074700970603     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
074800970603     C     FAT(24)       ANDEQ     'S'
074900970603     C                   MOVEL     FDE(23)       VFD007
075000970603     C                   MOVEL     FDE(24)       VFD008
075100970603     C                   ELSE                                                   <-- 2 -->
075200970603     C                   MOVEL     FUV(7)        VFD007
075300970603     C     FAT(23)       IFEQ      'S'
075400970603     C                   MOVEL     FDE(23)       VFD008
075500970603     C                   ELSE
075600970603     C                   MOVEL     FDE(24)       VFD008
075700970603     C                   ENDIF
075800970603     C                   ENDIF                                                  <-- 2 ---
075900970603     C                   ENDIF                                                  <-- 1 ---
076000970603     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
076100970603     C                   ENDSR
076200970603     C*--------------------------------------------------------------------------------------------*
076300970603     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
076400970603     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
076500970603     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
076600970603     C*                   - ERRORE SE FUNZIONE NON ATTIVA
076700970603     C*--------------------------------------------------------------------------------------------*
076800970603     C     CONFUN        BEGSR
076900970603     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
077000970603     C     1             DO        31            I                              --- 1 -->
077100970603     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
077200970603     C* CONTROLLO SE IL TASTO E' ATTIVO
077300970603     C     FAT(I)        IFNE      'S'                                          --- 3 -->
077400970603     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
077500970603     C                   EXSR      CARMSG
077600970603     C                   MOVEL     '1'           $ERR
077700970603     C                   ENDIF                                                  <-- 3 ---
077800970603     C                   ENDIF                                                  <-- 2 ---
077900970603     C                   ENDDO                                                  <-- 1 ---
078000970603     C                   ENDSR
078100970603     O*--------------------------------------------------------------------------------------------*
078200970603** -MSG- (MESSAGGI)
078300970603TISI48R- Tasto funzionale non valido.                                          1
078400970603TISI48R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
078500970603TISI48R- Opzione non valida.                                                   3
078600970603TISI48R- Opzione abilitata ma non gestita. Avvertire CED.                      4
078700970603TISI48R- Inizio elenco.                                                        5
078800970603TISI48R- Fine elenco.                                                          6
078900970611TISI48R- Immettere la versione                                                 7
079000970603TISI48R- Data errata                                                           8
079100970603TISI48R- La data scadenza deve essere maggiore della decorrenza                9
079200970603TISI48R- Versione già inserita                                                10
079300970603TISI48R- La versione non può sovrapporsi ad uno già in decorrenza             11
079400970603TISI48R- La versione non può scadere in un giorno già trascorso               12
079500970603TISI48R- 13
079600970603TISI48R- 14
079700970603TISI48R- 15
079800970603TISI48R- 16
079900970603TISI48R- 17
080000970603TISI48R- 18
080100970723TISI48R- Errori in entrata duplica. Non trovato il record da cui copiare      19
080200970603TISI48R- La versione e gli archivi correlati sono stati duplicati             20
080300970603** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
080400970603F1    -                   -    NN     NN
080500970603F2    -                   -    NN     NN
080600970603F3    -F3=Fine            -    NN     NN
080700970603F4    -                   -    NN     NN
080800970603F5    -                   -    NN     NN
080900970603F6    -F6=Conferma        -    SN     SN
081000970603F7    -                   -    NN     NN
081100970603F8    -                   -    NN     NN
081200970603F9    -                   -    NN     NN
081300970603F10   -                   -    NN     NN
081400970603F11   -                   -    NN     NN
081500970603F12   -F12=Ritorno        -    SS     SS
081600970603F13   -                   -    NN     NN
081700970603F14   -                   -    NN     NN
081800970603F15   -                   -    NN     NN
081900970603F16   -                   -    NN     NN
082000970603F17   -                   -    NN     NN
082100970603F18   -                   -    NN     NN
082200970603F19   -                   -    NN     NN
082300970603F20   -                   -    NN     NN
082400970603F21   -                   -    NN     NN
082500970603F22   -                   -    NN     NN
082600970603F23   -                   -    NN     NN
082700970603F24   -F24=Altri tasti    -    NN     NN
082800970603ENTER -                   -    SS     SS
082900970603ROLLUP-                   -    NN     NN
083000970603ROLLDN-                   -    NN     NN
083100970603HELP  -                   -    NN     NN
083200970603PRINT -                   -    NN     NN
083300970603BSPACE-                   -    NN     NN
083400970603CLEAR -                   -    NN     NN

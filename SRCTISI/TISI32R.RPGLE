000100970611     H*--------------------------------------------------------------------------------------------*
000200061222     H* GESTIONE LOCALITA' CAPPARIO - DUPLICA LOCALITA' CAPPARIO
000300970611     H*--------------------------------------------------------------------------------------------*
000400970611     H DECEDIT('0,') DATEDIT(*DMY.)
000500970611     F*--------------------------------------------------------------------------------------------*
000600970611     F* DATA BASE
000700970611     F*--------------------------------------------------------------------------------------------*
000800970611     FTISI32D   CF   E             WORKSTN INFDS(DEVDS1)
000900970611     F                                     SFILE(SI32DVM:MSGULT)
001000970611     FAZCPL01L  UF A E           K DISK
001100970917     FAZCPS01L  UF A E           K DISK
001200140318     FAZcos01L  IF A E           K DISK
001300970611     D*--------------------------------------------------------------------------------------------*
001400970611     D* SCHIERE
001500970611     D*--------------------------------------------------------------------------------------------*
001600970611     D*-------------------
001700970611     D* MESSAGGI E FUNZIONI
001800970611     D*-------------------
001900970611     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002000970611     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002100970611     D*-------------------
002200970611     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002300970611     D*-------------------
002400970611     D FFU             S              6    DIM(31)                              *FUNZIONE
002500970611     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002600970611     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002700970611     D*-------------------
002800970611     D* TABELLA DESCRIZIONI A VIDEO
002900970611     D*-------------------
003000970611     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003100970611     D*-------------------
003200970611     D* TABELLA TASTI FUNZIONALI PREMUTI
003300970611     D*-------------------
003400970611     D KEYS            S              1    DIM(40)
003500970611     D*--------------------------------------------------------------------------------------------*
003600970611     D* INPUT
003700970611     D*--------------------------------------------------------------------------------------------*
003800970611     D*-------------------
003900970611     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004000970611     D*-------------------
004100970611     D DSFUN         E DS
004200970611     D  FUNRIG                 1     40
004300970611     D*-------------------
004400970611     D* DS PER GESTIONE TASTI FUNZIONALI
004500970611     D*-------------------
004600970611     D DSKEYS        E DS
004700970611     D  KEYRIG                 1     40
004800970611     D*-------------------
004900970611     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005000970611     D*-------------------
005100970611     D                SDS
005200970611     D  USER                 254    263
005300970611     D*-------------------
005400970611     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005500970611     D*-------------------
005600970611     D DEVDS1          DS
005700970611     D  KEY                  369    369
005800970611     D  CURSOR               370    371
005900970611     D*---------------------
006000970611     D* ARCHITETTURA
006100970611     D*---------------------
006200970611     D KPJBA         E DS
006300970611     D*---------------------
006400970611     D* DS ESTERNA DI PROCEDURA
006500970611     D*---------------------
006600970611     D DSSI00        E DS                  EXTNAME(TISI00DS)
006700970611     D*-------------------
006800020919     D* DS REPERIMENTO DATI UTENTE
006900970611     D*-------------------
007000020919     D TIBS34DS      E DS                                                       *Profili utente
007100020919     D DDATIUTE      E DS                                                       *Dati utente
007200020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007300970917     D*---------------------
007400970918     D* DS "TISI95R" - CONTROLLA CAP
007500970917     D*---------------------
007600970917     D DSSI95        E DS                  EXTNAME(TISI95DS)
007700970918     D*------------
007800970918     D* DS "TISI81R" - AGGIORNA UN CAP DAI SUOI ARCHIVI
007900970918     D*------------
008000970918     D DSSI81        E DS                  EXTNAME(TISI81DS)
008100970611     D*-------------------
008200970611     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
008300970611     D*-------------------
008400970611     D DSMSG         E DS
008500970611     D*------------------
008600970611     D* DS "XSRDA8" - CONTROLLA DATA (8)
008700970611     D*------------------
008800970611     D WLBDA8          DS
008900970611     D  G08DAT                 1      8  0
009000970611     D  G08INV                 9     16  0
009100970611     D  G08ERR                17     17
009200970611     D  G08TGI                18     22  0
009300970611     C*--------------------------------------------------------------------------------------------*
009400970611     C* MAIN LINE
009500970611     C*--------------------------------------------------------------------------------------------*
009600970611     C*
009700970611     C* SE NON CI SONO ERRORI IN ENTRATA
009800970611IF  1C     $FINE         IFEQ      '0'                                          *NO ERRORI
009900970611     C                   WRITE     SI32DVT                                      *TESTATA
010000970611DO  2C     $FINE         DOUEQ     '1'
010100970611     C*
010200970611     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010300970611IF  3C     $GES          IFEQ      'V1'
010400970611     C                   EXSR      GESV1
010500970611E   3C                   ENDIF
010600970611     C*
010700970611E   2C                   ENDDO
010800970611     C*
010900970611IF  2C     $F03          IFEQ      '1'
011000970611     C                   MOVEL     '1'           D00F03
011100970611E   2C                   ENDIF
011200970611E   1C                   ENDIF
011300970917     C*
011400970917     C* OPERAZIONI FINALI
011500970917     C                   EXSR      FINSR
011600970611     C* FINE PGM
011700970611     C                   SETON                                        LR
011800970611     C*--------------------------------------------------------------------------------------------*
011900970611     C* GESV1 - GESTIONE VIDEO UNO
012000970611     C*--------------------------------------------------------------------------------------------*
012100970611     C     GESV1         BEGSR
012200970611     C*
012300970611IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
012400970611     C                   EXSR      CARV1                                        *CARICA VIDEO
012500970611E   1C                   ENDIF
012600970611     C*
012700970611IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012800970611     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012900970611     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
013000970611E   1C                   ENDIF
013100970611     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
013200970611     C*
013300970611     C                   EXFMT     SI32DV1                                      *PARAMETRI
013400970611     C*
013500970611     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013600970611     C*
013700970611IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013800970611SL  2C                   SELECT
013900970611     C     KEY           WHENEQ    ENTER
014000970611     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
014100970611     C     KEY           WHENEQ    F6
014200970611     C                   EXSR      F06GES
014300970611     C                   OTHER
014400970611     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014500970611E   2C                   ENDSL
014600970611E   1C                   ENDIF
014700970611     C*
014800970611     C                   ENDSR
014900970611     C*--------------------------------------------------------------------------------------------*
015000970611     C* CARV1 - CARICA VIDEO UNO
015100970611     C*--------------------------------------------------------------------------------------------*
015200970611     C     CARV1         BEGSR
015300970611     C*
015400970611     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015500970611     C*
015600970611     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015700970611     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015800970611     C*
015900970611     C                   ENDSR
016000970611     C*--------------------------------------------------------------------------------------------*
016100970611     C* ENTV1 - GESTIONE TASTO ENTER
016200970611     C*--------------------------------------------------------------------------------------------*
016300970611     C     ENTV1         BEGSR
016400970611     C*
016500970611     C* CONTROLLA I CAMPI IMMESSI
016600970611     C                   EXSR      DECODI
016700970611     C*
016800970611     C                   ENDSR
016900970611     C*--------------------------------------------------------------------------------------------*
017000970611     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
017100970611     C*--------------------------------------------------------------------------------------------*
017200970611     C     F06GES        BEGSR
017300970611     C*
017400970611     C* CONTROLLA I CAMPI IMMESSI
017500970611     C                   EXSR      DECODI
017600970611     C*
017700970611     C* RILEGGE LA LOCALITA' DA CUI COPIARE
017800970611IF  1C     $ERR          IFEQ      '0'
017900970611     C                   Z-ADD     D00VER        KCLVER
018000970611     C                   MOVEL     V1CNAR        KCLNAR
018100970611     C                   MOVEL     V1CCAP        KCLCAP
018200970611     C                   MOVEL     V1CLOC        KCLLOC
018300970611     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
018400970611IF  2C     *IN99         IFEQ      *OFF
018500970917     C*
018600970917     C* CREA I SINONIMI DELLA NUOVA LOCALITA'
018700970917IF  3C     CPLESS        IFEQ      'S'                                          *ESISTONO SINONIMI
018800970917IF  4C     V1CLOC        IFEQ      V1CLO2                                       *SE UGUALE
018900970917     C                   EXSR      CRECPS                                       *CREA SINONIMI
019000970917X   4C                   ELSE                                                   *NON UGUALE
019100970917     C                   MOVEL     'N'           CPLESS                         *NON ESISTONO SINONI
019200970917E   4C                   ENDIF
019300970917E   3C                   ENDIF
019400140318     c* copio gli orari servizi
019500140318     c                   exsr      crecos
019600970611     C*
019700970611     C* IMPOSTA IL RECORD DA SCRIVERE
019800970611     C                   Z-ADD     D00VER        CPLVER                         *VERSIONE
019900970611     C                   MOVEL     V1CNA2        CPLNAR                         *NAZIONE
020000970611     C                   MOVEL     V1CCA2        CPLCAP                         *CAP
020100970611     C                   MOVEL     V1CLO2        CPLLOC                         *LOCALITA'
020200970919     C                   MOVEL     *BLANKS       CPLFI1                         *FLAG OBSOLETO
020300970611     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
020400970611     C                   Z-ADD     *ZEROS        CPLDTR                         *DATA TRASMISSIONE
020500970611     C* SCRIVE
020600970611     C                   WRITE     AZCPL000
020700970918     C*
020800970918     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
020900970918     C                   Z-ADD     D00VER        D81VER
021000970918     C                   MOVEL     V1CNA2        D81NAR
021100970918     C                   MOVEL     V1CCA2        D81CAP
021200970918     C                   MOVEL     'L'           D81CHK
021300970918     C                   CALL      'TISI81R'
021400970918     C                   PARM                    DSSI81
021500000427     C*
021600000427     C* IMPOSTA FLAG DI FINE PROGRAMMA
021700000427     C                   MOVEL     MSG(20)       D00MSG
021800000427     C                   MOVEL     '1'           $FINE
021900000427E   2C                   ENDIF
022000970611E   1C                   ENDIF
022100970611     C*
022200970611     C                   ENDSR
022300970917     C*--------------------------------------------------------------------------------------------*
022400140318     C* CRECOS - CREA gli orari
022500970917     C*--------------------------------------------------------------------------------------------*
022600140318     C     CRECOS        BEGSR
022700970917     C*
022800970917     C* LEGGE I SINONIMI DELLA LOCALITA' DA COPIARE
022900970917     C                   Z-ADD     D00VER        KCSVER
023000970917     C                   MOVEL     V1CNAR        KCSNAR
023100970917     C                   MOVEL     V1CCAP        KCSCAP
023200970917     C                   MOVEL     V1CLOC        KCSLOC
023300140318     C     KEYCPS        SETLL     AZcos01L
023400140318     C     KEYCPS        READE(N)  AZcos01L                               98
023500970917DO  1C     *IN98         DOWEQ     *OFF
023600970917     C*
023700970917     C* SCRIVE IL SINONIMO DELLA NUOVA LOCALITA'
023800140318     C                   Z-ADD     D00VER        cosVER                         *VERSIONE
023900140318     C                   MOVEL     V1CNA2        cosNAR                         *NAZIONE
024000140318     C                   MOVEL     V1CCA2        cosCAP                         *CAP
024100140318     C                   MOVEL     V1CLO2        cosLOC                         *LOCALITA'
024200140318     C                   MOVEL     knmus         cosute                         *FLAG TRASMISSIONE
024300140318     C                   Z-ADD     datcor        cosduv                         *DATA TRASMISSIONE
024400140318     C                   WRITE     AZcos000
024500140318     c
024600140318     C     KEYCPS        READE(N)  AZcos01L                               98
024700970917E   1C                   ENDDO
024800970917     C*
024900970917     C                   ENDSR
025000140318     c*--------------------------------------------------------------------------------------------*
025100140318     C* CRECPS - CREA I SINONIMI DELLA NUOVA LOCALITA'
025200140318     C*--------------------------------------------------------------------------------------------*
025300140318     C     CRECPS        BEGSR
025400140318     C*
025500140318     C* LEGGE I SINONIMI DELLA LOCALITA' DA COPIARE
025600140318     C                   Z-ADD     D00VER        KCSVER
025700140318     C                   MOVEL     V1CNAR        KCSNAR
025800140318     C                   MOVEL     V1CCAP        KCSCAP
025900140318     C                   MOVEL     V1CLOC        KCSLOC
026000140318     C     KEYCPS        SETLL     AZCPS01L
026100140318     C     KEYCPS        READE(N)  AZCPS01L                               98
026200140318DO  1C     *IN98         DOWEQ     *OFF
026300140318IF  2C     CPSATB        IFEQ      *BLANKS
026400140318     C*
026500140318     C* SCRIVE IL SINONIMO DELLA NUOVA LOCALITA'
026600140318     C                   Z-ADD     D00VER        CPSVER                         *VERSIONE
026700140318     C                   MOVEL     V1CNA2        CPSNAR                         *NAZIONE
026800140318     C                   MOVEL     V1CCA2        CPSCAP                         *CAP
026900140318     C                   MOVEL     V1CLO2        CPSLOC                         *LOCALITA'
027000140318     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
027100140318     C                   Z-ADD     *ZEROS        CPSDTR                         *DATA TRASMISSIONE
027200140318     C                   WRITE     AZCPS000
027300140318E   2C                   ENDIF
027400140318     C     KEYCPS        READE(N)  AZCPS01L                               98
027500140318E   1C                   ENDDO
027600140318     C*
027700140318     C                   ENDSR
027800970611     C*--------------------------------------------------------------------------------------------*
027900970611     C* DECODI - CONTROLLA CAMPI E DECODIFICA
028000970611     C*--------------------------------------------------------------------------------------------*
028100970611     C     DECODI        BEGSR
028200970611     C*
028300970611     C* REIMPOSTA INDICATORI DI ERRORE
028400970611     C                   SETOFF                                       606162
028500970611     C*--------------------
028600970611     C* CAP
028700970611     C*--------------------
028800970611IF  1C     $ERR          IFEQ      '0'
028900970917     C                   MOVEL     V1CNA2        WNAR
029000970917     C                   MOVEL     V1CCA2        WCAP
029100970917     C                   MOVEL     *BLANKS       WFI1
029200970917     C                   EXSR      CHKCAP
029300970917IF  2C     $ERR          IFEQ      '1'
029400970611     C                   MOVEL     '1'           $ERR
029500970611     C                   MOVEL     MSG(8)        $MSG
029600970611     C                   EXSR      CARMSG
029700970611     C                   SETON                                        61
029800970611E   2C                   ENDIF
029900970611E   1C                   ENDIF
030000970917     C*
030100970917     C* IL CAP NON PUO' ESSERE OBSOLETO
030200970917IF  1C     $ERR          IFEQ      '0'
030300970917IF  2C     WFI1          IFEQ      'O '
030400970917     C                   MOVEL     '1'           $ERR
030500970917     C                   MOVEL     MSG(7)        $MSG
030600970917     C                   EXSR      CARMSG
030700970917     C                   SETON                                        6061
030800970917E   2C                   ENDIF
030900970917E   1C                   ENDIF
031000970611     C*--------------------
031100970611     C* LOCALITA'
031200970611     C*--------------------
031300970611IF  1C     $ERR          IFEQ      '0'
031400970611IF  2C     V1CLO2        IFEQ      *BLANKS
031500970611     C                   MOVEL     '1'           $ERR
031600970611     C                   MOVEL     MSG(9)        $MSG
031700970611     C                   EXSR      CARMSG
031800970611     C                   SETON                                        62
031900970611E   2C                   ENDIF
032000970611E   1C                   ENDIF
032100970611     C* NUOVA LOCALITA' INESISTENTE
032200970611IF  1C     $ERR          IFEQ      '0'
032300970611     C                   Z-ADD     D00VER        KCLVER
032400970611     C                   MOVEL     V1CNA2        KCLNAR
032500970611     C                   MOVEL     V1CCA2        KCLCAP
032600970611     C                   MOVEL     V1CLO2        KCLLOC
032700970611     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
032800970611IF  2C     *IN99         IFEQ      *OFF
032900970611     C                   MOVEL     '1'           $ERR
033000970611     C                   MOVEL     MSG(10)       $MSG
033100970611     C                   EXSR      CARMSG
033200970611     C                   SETON                                        606162
033300970611E   2C                   ENDIF
033400970611E   1C                   ENDIF
033500970611     C*
033600970611     C                   ENDSR
033700970917     C*------------------------------------------------------------------------*
033800970917     C* CHKCAP - CONTROLLA IL CAP
033900970917     C*------------------------------------------------------------------------*
034000970917     C     CHKCAP        BEGSR
034100970917     C*
034200970917     C     *LIKE         DEFINE    V1CNA2        WNAR
034300970917     C     *LIKE         DEFINE    V1CCA2        WCAP
034400970917     C     *LIKE         DEFINE    O95CF1        WFI1
034500970917     C*
034600970917     C* CONTROLLA IL CAP
034700970917     C                   CLEAR                   DSSI95
034800970917     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
034900970917     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
035000970917     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
035100970917     C                   MOVEL     WCAP          I95CAP                         *CAP
035200970917     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
035300970917     C                   CALL      'TISI95R'
035400970917     C                   PARM                    DSSI95
035500970917IF  1C     O95ERR        IFNE      *BLANKS
035600970919     C     O95LID        ORLT      '2'
035700970917     C                   MOVEL     '1'           $ERR
035800970917X   1C                   ELSE
035900970917     C                   MOVEL     O95CF1        WFI1
036000970917E   1C                   ENDIF
036100970917     C*
036200970917     C                   ENDSR
036300970611     C*--------------------------------------------------------------------------------------------*
036400970611     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
036500970611     C*--------------------------------------------------------------------------------------------*
036600970611     C     IMPVID        BEGSR
036700970611     C*
036800970611     C* CALCOLA LA DATA CORRENTE
036900970611     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
037000970611     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
037100970611     C                   Z-ADD     WNUM8         G08DAT
037200970611     C                   Z-ADD     *ZEROS        G08INV
037300970611     C                   MOVEL     '0'           G08ERR
037400970611     C                   CALL      'XSRDA8'
037500970611     C                   PARM                    WLBDA8
037600970611     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
037700970611     C*
037800970611     C* LEGGE LA LOCALITA'
037900970611     C                   Z-ADD     D00VER        KCLVER
038000970611     C                   MOVEL     D00NAR        KCLNAR
038100970611     C                   MOVEL     D00CAP        KCLCAP
038200970611     C                   MOVEL     D00LOC        KCLLOC
038300970611     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
038400970611IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
038500970917     C* DATI OLD
038600970611     C                   MOVEL     CPLNAR        V1CNAR                         *NAZIONE
038700970611     C                   MOVEL     CPLCAP        V1CCAP                         *CAP
038800970611     C                   MOVEL     CPLLOC        V1CLOC                         *LOCALITA'
038900970917     C* DATI NEW
039000970611     C                   MOVEL     V1CNAR        V1CNA2                         *NAZIONE
039100970611     C                   MOVEL     V1CCAP        V1CCA2                         *CAP
039200970917     C                   MOVEL     V1CLOC        V1CLO2                         *LOCALITA'
039300970611X   1C                   ELSE                                                   *NON TROVATO
039400970611     C                   MOVEL     '1'           $FINE                          *FINE PGM
039500970611     C                   MOVEL     '1'           D00ERR                         *ERRORE
039600970611     C                   MOVEL     MSG(19)       D00MSG                         *MESSSAGGIO
039700970611E   1C                   ENDIF
039800970611     C*
039900970611     C                   ENDSR
040000970917     C*------------------------------------------------------------------------*
040100970917     C* FINSR - OPERAZIONI FINALI
040200970917     C*------------------------------------------------------------------------*
040300970917     C     FINSR         BEGSR
040400970917     C*
040500970917     C                   CLEAR                   DSSI95
040600970917     C                   MOVEL     'C'           I95TLA
040700970917     C                   CALL      'TISI95R'
040800970917     C                   PARM                    DSSI95
040900970917     C*
041000970917     C                   ENDSR
041100020919     C*--------------------------------------------------------------------------------------------*
041200020919     C* REPERISCE I DATI UTENTE
041300020919     C*--------------------------------------------------------------------------------------------*
041400020919     C     REPDATIUTE    BEGSR
041500020919     C*
041600020919     C* INIZIALIZZA VARIABILI DI WRK
041700020919     C                   CLEAR                   TIBS34DS
041800020919     C                   CLEAR                   AZUTEDS
041900020919     C                   CLEAR                   DDATIUTE
042000020919     C*
042100020919     C     *DTAARA       DEFINE    §azute        azuteds
042200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
042300020919     C                   IN(E)     *DTAARA
042400020919if  1C                   IF        %Error
042500020919     c                   EVAL      I34Tla = 'L'
042600020919     C                   CALL      'TIBS34R'
042700020919     C                   PARM                    Tibs34Ds
042800020919     C                   IN        *DTAARA
042900020919e   1C                   ENDIF
043000020919     C*
043100020919     C                   ENDSR
043200970611     C*--------------------------------------------------------------------------------------------*
043300970611     C* *INZSR  - ROUTINE INIZIALE
043400970611     C*--------------------------------------------------------------------------------------------*
043500970611     C     *INZSR        BEGSR
043600970611     C*--------------------
043700970611     C* RICEVIMENTO PARAMETRI
043800970611     C*--------------------
043900970611     C     *ENTRY        PLIST
044000970612     C                   PARM                    KPJBA
044100970612     C                   PARM                    DSSI00
044200970611     C*--------------------
044300970611     C* VARIABILI RIFERITE AL DATA BASE
044400970611     C*--------------------
044500970611     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL01L
044600970611     C     *LIKE         DEFINE    CPLNAR        KCLNAR
044700970611     C     *LIKE         DEFINE    CPLCAP        KCLCAP
044800970611     C     *LIKE         DEFINE    CPLLOC        KCLLOC
044900970917     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS01L
045000970917     C     *LIKE         DEFINE    CPSNAR        KCSNAR
045100970917     C     *LIKE         DEFINE    CPSCAP        KCSCAP
045200970917     C     *LIKE         DEFINE    CPSLOC        KCSLOC
045300000605     C*
045400970611     C*--------------------
045500970611     C* CHIAVI DI LETTURA
045600970611     C*--------------------
045700970917     C* LETTURA AZCPL01L - COMPLETA
045800970611     C     KEYCPL        KLIST
045900970611     C                   KFLD                    KCLVER                         *VERSIONE
046000970611     C                   KFLD                    KCLNAR                         *NAZIONE
046100970611     C                   KFLD                    KCLCAP                         *CAP
046200970611     C                   KFLD                    KCLLOC                         *LOCALITA'
046300970917     C* LETTURA AZCPS01L - PARZIALE
046400970917     C     KEYCPS        KLIST
046500970917     C                   KFLD                    KCSVER                         *VERSIONE
046600970917     C                   KFLD                    KCSNAR                         *NAZIONE
046700970917     C                   KFLD                    KCSCAP                         *CAP
046800970917     C                   KFLD                    KCSLOC                         *LOCALITA'
046900970611     C***
047000970611     C* IMPOSTAZIONI VIDEO
047100970611     C***
047200970611IF  1C     D00OP0        IFEQ      'L02'
047300970611     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
047400970611X   1C                   ELSE
047500970611     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
047600970611E   1C                   ENDIF
047700970611     C*
047800970611     C* ESEGUE OPERAZIONI INIZIALI STANDARD
047900970611     C                   EXSR      INZSTD
048000970611     C*
048100970611     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
048200970611     C                   EXSR      IMPVID
048300970611     C*
048400970611     C* EMETTO PRIMO FORMATO VIDEO
048500970611     C                   EXSR      CARV1
048600970611     C*
048700970611     C                   ENDSR
048800970611     C*--------------------------------------------------------------------------------------------*
048900970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049000970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049100970611     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
049200970611     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
049300970611     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
049400970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049500970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049600970611     C*--------------------------------------------------------------------------------------------*
049700970611     C* INZSTD - ROUTINE INIZIALE STANDARD
049800970611     C*--------------------------------------------------------------------------------------------*
049900970611     C     INZSTD        BEGSR
050000970611     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
050100970611     C                   CALL      'XKEYS'
050200970611     C                   PARM                    DSKEYS
050300970611     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
050400970611     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
050500970611     C     XMSGPA        PLIST
050600970611     C                   PARM                    DSMSMS
050700970611     C                   PARM                    DSMSPG
050800970611     C                   PARM                    DSMSKE
050900970611     C                   PARM                    DSMSER
051000970611     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
051100970611     C                   MOVEL     'TISI32R'     DSMSPG                         *NOME PGM CHIAMANTE
051200970611     C                   MOVEL     'TISI32R'     VMCQUE                         *CODA MESSAGGI DESTI
051300970611     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
051400970611     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
051500970611     C* INIZIALIZZO FLAG CONTROLLO PGM
051600970611     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
051700970611     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
051800970611     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
051900970611     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
052000970611     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
052100970611     C                   MOVEL     '0'           $FINE             1            *FINE PGM
052200970611     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
052300970611     C* LANCIO ROUTINE PER VERIFICARE SE
052400970611     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
052500970611     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
052600020919     C                   EXSR      REPDATIUTE
052700970611     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
052800970611     C                   BITOFF    '01234567'    POSMI1            1
052900970611     C                   BITOFF    '01234567'    POSMI2            1
053000970611     C                   BITOFF    '01234567'    POSMA1            1
053100970611     C                   BITOFF    '01234567'    POSMA2            1
053200970611     C                   BITON     '34'          POSMI1            1            *18
053300970611     C                   BITON     '7'           POSMI2            1            *01
053400970611     C                   BITON     '34'          POSMA1            1            *18
053500970611     C                   BITON     '0'           POSMA2            1            *80
053600970611     C                   MOVE      POSMI2        POSMIN            2
053700970611     C                   MOVEL     POSMI1        POSMIN
053800970611     C                   MOVE      POSMA2        POSMAX            2
053900970611     C                   MOVEL     POSMA1        POSMAX
054000970611     C                   ENDSR
054100970611     C*--------------------------------------------------------------------------------------------*
054200970611     C* FUNSTD - LANCIA SUBROUTINE STANDARD
054300970611     C*--------------------------------------------------------------------------------------------*
054400970611     C     FUNSTD        BEGSR
054500970611     C                   SELECT
054600970611     C     KEY           WHENEQ    F3
054700970611     C                   EXSR      F03STD
054800970611     C     KEY           WHENEQ    F12
054900970611     C                   EXSR      F12STD
055000970611     C     KEY           WHENEQ    F24
055100970611     C                   EXSR      F24STD
055200970611     C                   OTHER
055300970611     C                   MOVEL     MSG(2)        $MSG
055400970611     C                   EXSR      CARMSG
055500970611     C                   ENDSL
055600970611     C                   ENDSR
055700970611     C*--------------------------------------------------------------------------------------------*
055800970611     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
055900970611     C*--------------------------------------------------------------------------------------------*
056000970611     C     F03STD        BEGSR
056100970611     C* IMPOSTA FLAG DI FINE PROGRAMMA
056200970611     C                   MOVEL     '1'           $FINE
056300970611     C* IMPOSTA FLAG DI FINE PROCEDURA
056400970611     C                   MOVEL     '1'           $F03
056500970611     C* PULISCE IL MESSAGGIO
056600970611     C                   MOVEL     *BLANKS       D00MSG
056700970611     C                   ENDSR
056800970611     C*--------------------------------------------------------------------------------------------*
056900970611     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
057000970611     C*--------------------------------------------------------------------------------------------*
057100970611     C     F12STD        BEGSR
057200970611     C* IMPOSTA FLAG DI FINE PROGRAMMA
057300970611     C                   MOVEL     '1'           $FINE
057400970611     C* IMPOSTA FLAG DI FINE PROCEDURA
057500970611     C                   MOVEL     '1'           $F12
057600970611     C* PULISCE IL MESSAGGIO
057700970611     C                   MOVEL     *BLANKS       D00MSG
057800970611     C                   ENDSR
057900970611     C*--------------------------------------------------------------------------------------------*
058000970611     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
058100970611     C*--------------------------------------------------------------------------------------------*
058200970611     C     F24STD        BEGSR
058300970611     C*
058400970611     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
058500970611     C                   EXSR      OUTFUN
058600970611     C*
058700970611     C                   ENDSR
058800970611     C*--------------------------------------------------------------------------------------------*
058900970611     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
059000970611     C*          CODA MESSAGGI DEL PGM
059100970611     C*--------------------------------------------------------------------------------------------*
059200970611     C     CARMSG        BEGSR
059300970611     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
059400970611     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
059500970611     C     MSGULT        IFEQ      0
059600970611     C                   MOVEL     '1'           *IN40
059700970611     C                   WRITE     SI32DVK
059800970611     C                   MOVEL     '0'           *IN40
059900970611     C                   ENDIF
060000970611     C* INVIA MESSAGGIO ALLA CODA DEL PGM
060100970611     C                   MOVEL     $MSG          DSMSMS
060200970611     C                   CALL      'XMSG'        XMSGPA
060300970611     C     DSMSER        IFEQ      0
060400970611     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
060500970611     C                   ENDIF
060600970611     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
060700970611     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
060800970611     C                   WRITE     SI32DVM                                      *SFL MESS
060900970611     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
061000970611     C                   MOVEL     *BLANKS       $MSG
061100970611     C                   ENDSR
061200970611     C*--------------------------------------------------------------------------------------------*
061300970611     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
061400970611     C*--------------------------------------------------------------------------------------------*
061500970611     C     OUTMSG        BEGSR
061600970611     C* VISUALIZZAZIONE MESSAGGIO
061700970611     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
061800970611     C                   WRITE     SI32DVK                                      *CONTR MESS
061900970611     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
062000970611     C                   Z-ADD     *ZEROS        MSGULT
062100970611     C                   ENDSR
062200970611     C*--------------------------------------------------------------------------------------------*
062300970611     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
062400970611     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
062500970611     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
062600970611     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
062700970611     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
062800970611     C*--------------------------------------------------------------------------------------------*
062900970611     C     CARFUN        BEGSR
063000970611     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
063100970611     C     1             DO        31            I                 5 0          --- 1 -->
063200970611     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
063300970611     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
063400970611     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
063500020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
063600970611     C     $TGES         IFEQ      'G'                                          *GESTIONE
063700970611     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
063800970611     C                   ELSE                                                   *INTERROG.
063900970611     C                   MOVEL     FUNASI        FAT(I)
064000970611     C                   ENDIF
064100970611     C                   ELSE                                                   <-- 2 --> *FILIALE
064200970611     C     $TGES         IFEQ      'G'                                          *GESTIONE
064300970611     C                   MOVEL     FUNAFG        FAT(I)
064400970611     C                   ELSE                                                   *INTERROG.
064500970611     C                   MOVEL     FUNAFI        FAT(I)
064600970611     C                   ENDIF
064700970611     C                   ENDIF                                                  <-- 2 ---
064800970611     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
064900970611     C                   MOVEL     USER          WEDP              3            *PROFILO
065000970611     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
065100970611     C                   MOVEL     'S'           FAT(I)                         *ABILITO
065200970611     C                   ELSE
065300970611     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
065400970611     C                   ENDIF
065500970611     C                   ENDIF
065600970611     C                   ENDDO                                                  <-- 1 ---
065700970611     C                   ENDSR
065800970611     C*--------------------------------------------------------------------------------------------*
065900970611     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
066000970611     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
066100970611     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
066200970611     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
066300970611     C*--------------------------------------------------------------------------------------------*
066400970611     C     OUTFUN        BEGSR
066500970611     C     1             DO        8             I                              *PULIZIA FUNZIONI A
066600970611     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
066700970611     C                   ENDDO
066800970611     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
066900970611     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
067000970611     C     1             DO        31            I
067100970611     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
067200970611     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
067300970611     C                   ADD       1             FUNNAT
067400970611     C                   ENDIF
067500970611     C                   ENDDO
067600970611     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
067700970611     C     FUNNAT        IFGT      8
067800970611     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
067900970611     C                   ENDIF
068000970611     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
068100970611     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
068200970611     C                   Z-ADD     8             FUNVIS            5 0
068300970611     C     FAT(23)       IFEQ      'S'
068400970611     C                   SUB       1             FUNVIS
068500970611     C                   ENDIF
068600970611     C     FAT(24)       IFEQ      'S'
068700970611     C                   SUB       1             FUNVIS
068800970611     C                   ENDIF
068900970611     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
069000970611     C                   Z-ADD     *ZEROS        W                 5 0
069100970611     C                   ADD       1             FUNULT
069200970611     C     FUNULT        IFGT      31
069300970611     C                   Z-ADD     1             FUNULT
069400970611     C                   ENDIF
069500970611     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
069600970611     C     FUNULT        DO        31            I                              --- 1 -->
069700970611     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
069800970611     C     W             IFLT      FUNVIS                                       --- 2 -->
069900970611     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
070000970611     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
070100970611     C     FDE(I)        ANDNE     *BLANKS
070200970611     C     I             IFEQ      23                                           *TOLGO F23
070300970611     C     I             OREQ      24                                           *E F24
070400970611     C                   ELSE
070500970611     C                   ADD       1             W
070600970611     C                   MOVEL     FDE(I)        FUV(W)
070700970611     C                   ENDIF
070800970611     C                   ENDIF                                                  <-- 3 ---
070900970611     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
071000970611     C                   Z-ADD     I             FUNULT
071100970611     C                   ENDIF                                                  <-- 2 ---
071200970611     C                   ENDDO                                                  <-- 1 ---
071300970611     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
071400970611     C                   MOVEL     FUV(1)        VFD001
071500970611     C                   MOVEL     FUV(2)        VFD002
071600970611     C                   MOVEL     FUV(3)        VFD003
071700970611     C                   MOVEL     FUV(4)        VFD004
071800970611     C                   MOVEL     FUV(5)        VFD005
071900970611     C                   MOVEL     FUV(6)        VFD006
072000970611     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
072100970611     C     FAT(24)       ANDEQ     'N'
072200970611     C                   MOVEL     FUV(7)        VFD007
072300970611     C                   MOVEL     FUV(8)        VFD008
072400970611     C                   ELSE                                                   <-- 1 -->
072500970611     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
072600970611     C     FAT(24)       ANDEQ     'S'
072700970611     C                   MOVEL     FDE(23)       VFD007
072800970611     C                   MOVEL     FDE(24)       VFD008
072900970611     C                   ELSE                                                   <-- 2 -->
073000970611     C                   MOVEL     FUV(7)        VFD007
073100970611     C     FAT(23)       IFEQ      'S'
073200970611     C                   MOVEL     FDE(23)       VFD008
073300970611     C                   ELSE
073400970611     C                   MOVEL     FDE(24)       VFD008
073500970611     C                   ENDIF
073600970611     C                   ENDIF                                                  <-- 2 ---
073700970611     C                   ENDIF                                                  <-- 1 ---
073800970611     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
073900970611     C                   ENDSR
074000970611     C*--------------------------------------------------------------------------------------------*
074100970611     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
074200970611     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
074300970611     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
074400970611     C*                   - ERRORE SE FUNZIONE NON ATTIVA
074500970611     C*--------------------------------------------------------------------------------------------*
074600970611     C     CONFUN        BEGSR
074700970611     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
074800970611     C     1             DO        31            I                              --- 1 -->
074900970611     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
075000970611     C* CONTROLLO SE IL TASTO E' ATTIVO
075100970611     C     FAT(I)        IFNE      'S'                                          --- 3 -->
075200970611     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
075300970611     C                   EXSR      CARMSG
075400970611     C                   MOVEL     '1'           $ERR
075500970611     C                   ENDIF                                                  <-- 3 ---
075600970611     C                   ENDIF                                                  <-- 2 ---
075700970611     C                   ENDDO                                                  <-- 1 ---
075800970611     C                   ENDSR
075900970611     O*--------------------------------------------------------------------------------------------*
076000970611** -MSG- (MESSAGGI)
076100970611TISI32R- Tasto funzionale non valido.                                          1
076200970611TISI32R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
076300970611TISI32R- Opzione non valida.                                                   3
076400970611TISI32R- Opzione abilitata ma non gestita. Avvertire CED.                      4
076500970611TISI32R- Inizio elenco.                                                        5
076600970611TISI32R- Fine elenco.                                                          6
076700970917TISI32R- Impossibile inserire località di CAP obsoleti                         7
076800970919TISI32R- Cap inesistente o non valido                                          8
076900970611TISI32R- Immettere la località                                                 9
077000970611TISI32R- Località già inserita                                                10
077100970611TISI32R- 11
077200970611TISI32R- 12
077300970611TISI32R- 13
077400970611TISI32R- 14
077500970611TISI32R- 15
077600970611TISI32R- 16
077700970611TISI32R- 17
077800970611TISI32R- 18
077900970917TISI32R- Errori in entrata duplica. Non trovato il record da cui copiare      19
078000970917TISI32R- La località e gli archivi correlati sono stati duplicati             20
078100970611** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
078200970611F1    -                   -    NN     NN
078300970611F2    -                   -    NN     NN
078400970611F3    -F3=Fine            -    NN     NN
078500970611F4    -                   -    NN     NN
078600970611F5    -                   -    NN     NN
078700970611F6    -F6=Conferma        -    SN     SN
078800970611F7    -                   -    NN     NN
078900970611F8    -                   -    NN     NN
079000970611F9    -                   -    NN     NN
079100970611F10   -                   -    NN     NN
079200970611F11   -                   -    NN     NN
079300970611F12   -F12=Ritorno        -    SS     SS
079400970611F13   -                   -    NN     NN
079500970611F14   -                   -    NN     NN
079600970611F15   -                   -    NN     NN
079700970611F16   -                   -    NN     NN
079800970611F17   -                   -    NN     NN
079900970611F18   -                   -    NN     NN
080000970611F19   -                   -    NN     NN
080100970611F20   -                   -    NN     NN
080200970611F21   -                   -    NN     NN
080300970611F22   -                   -    NN     NN
080400970611F23   -                   -    NN     NN
080500970611F24   -F24=Altri tasti    -    NN     NN
080600970611ENTER -                   -    SS     SS
080700970611ROLLUP-                   -    NN     NN
080800970611ROLLDN-                   -    NN     NN
080900970611HELP  -                   -    NN     NN
081000970611PRINT -                   -    NN     NN
081100970611BSPACE-                   -    NN     NN
081200970611CLEAR -                   -    NN     NN

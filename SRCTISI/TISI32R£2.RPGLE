000100970611     H*--------------------------------------------------------------------------------------------*
000200000427     H* GESTIONE LOCALITA' CAPPARIO - DUPLICA LOCALITA' CAPPARIO + ESTENSIONE X POSTE
000300970611     H*--------------------------------------------------------------------------------------------*
000400970611     H DECEDIT('0,') DATEDIT(*DMY.)
000500970611     F*--------------------------------------------------------------------------------------------*
000600970611     F* DATA BASE
000700970611     F*--------------------------------------------------------------------------------------------*
000800970611     FTISI32D   CF   E             WORKSTN INFDS(DEVDS1)
000900970611     F                                     SFILE(SI32DVM:MSGULT)
001000970611     FAZCPL01L  UF A E           K DISK
001100000427     FAZPPL01L  UF A E           K DISK
001200970917     FAZCPS01L  UF A E           K DISK
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
009300000605     D*-------------------
009400000605     D* DS DI SALVATAGGIO FORMATI RECORDS
009500000605     D*-------------------
009600000605     D DSPPL         E DS                  EXTNAME(AZPPL00F)
009700970611     C*--------------------------------------------------------------------------------------------*
009800970611     C* MAIN LINE
009900970611     C*--------------------------------------------------------------------------------------------*
010000970611     C*
010100970611     C* SE NON CI SONO ERRORI IN ENTRATA
010200970611IF  1C     $FINE         IFEQ      '0'                                          *NO ERRORI
010300970611     C                   WRITE     SI32DVT                                      *TESTATA
010400970611DO  2C     $FINE         DOUEQ     '1'
010500970611     C*
010600970611     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010700970611IF  3C     $GES          IFEQ      'V1'
010800970611     C                   EXSR      GESV1
010900970611E   3C                   ENDIF
011000970611     C*
011100970611E   2C                   ENDDO
011200970611     C*
011300970611IF  2C     $F03          IFEQ      '1'
011400970611     C                   MOVEL     '1'           D00F03
011500970611E   2C                   ENDIF
011600970611E   1C                   ENDIF
011700970917     C*
011800970917     C* OPERAZIONI FINALI
011900970917     C                   EXSR      FINSR
012000970611     C* FINE PGM
012100970611     C                   SETON                                        LR
012200970611     C*--------------------------------------------------------------------------------------------*
012300970611     C* GESV1 - GESTIONE VIDEO UNO
012400970611     C*--------------------------------------------------------------------------------------------*
012500970611     C     GESV1         BEGSR
012600970611     C*
012700970611IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
012800970611     C                   EXSR      CARV1                                        *CARICA VIDEO
012900970611E   1C                   ENDIF
013000970611     C*
013100970611IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
013200970611     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
013300970611     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
013400970611E   1C                   ENDIF
013500970611     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
013600970611     C*
013700970611     C                   EXFMT     SI32DV1                                      *PARAMETRI
013800970611     C*
013900970611     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
014000970611     C*
014100970611IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
014200970611SL  2C                   SELECT
014300970611     C     KEY           WHENEQ    ENTER
014400970611     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
014500970611     C     KEY           WHENEQ    F6
014600970611     C                   EXSR      F06GES
014700970611     C                   OTHER
014800970611     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014900970611E   2C                   ENDSL
015000970611E   1C                   ENDIF
015100970611     C*
015200970611     C                   ENDSR
015300970611     C*--------------------------------------------------------------------------------------------*
015400970611     C* CARV1 - CARICA VIDEO UNO
015500970611     C*--------------------------------------------------------------------------------------------*
015600970611     C     CARV1         BEGSR
015700970611     C*
015800970611     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015900970611     C*
016000970611     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016100970611     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016200970611     C*
016300970611     C                   ENDSR
016400970611     C*--------------------------------------------------------------------------------------------*
016500970611     C* ENTV1 - GESTIONE TASTO ENTER
016600970611     C*--------------------------------------------------------------------------------------------*
016700970611     C     ENTV1         BEGSR
016800970611     C*
016900970611     C* CONTROLLA I CAMPI IMMESSI
017000970611     C                   EXSR      DECODI
017100970611     C*
017200970611     C                   ENDSR
017300970611     C*--------------------------------------------------------------------------------------------*
017400970611     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
017500970611     C*--------------------------------------------------------------------------------------------*
017600970611     C     F06GES        BEGSR
017700970611     C*
017800970611     C* CONTROLLA I CAMPI IMMESSI
017900970611     C                   EXSR      DECODI
018000970611     C*
018100970611     C* RILEGGE LA LOCALITA' DA CUI COPIARE
018200970611IF  1C     $ERR          IFEQ      '0'
018300970611     C                   Z-ADD     D00VER        KCLVER
018400970611     C                   MOVEL     V1CNAR        KCLNAR
018500970611     C                   MOVEL     V1CCAP        KCLCAP
018600970611     C                   MOVEL     V1CLOC        KCLLOC
018700970611     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
018800970611IF  2C     *IN99         IFEQ      *OFF
018900970917     C*
019000970917     C* CREA I SINONIMI DELLA NUOVA LOCALITA'
019100970917IF  3C     CPLESS        IFEQ      'S'                                          *ESISTONO SINONIMI
019200970917IF  4C     V1CLOC        IFEQ      V1CLO2                                       *SE UGUALE
019300970917     C                   EXSR      CRECPS                                       *CREA SINONIMI
019400970917X   4C                   ELSE                                                   *NON UGUALE
019500970917     C                   MOVEL     'N'           CPLESS                         *NON ESISTONO SINONI
019600970917E   4C                   ENDIF
019700970917E   3C                   ENDIF
019800970611     C*
019900970611     C* IMPOSTA IL RECORD DA SCRIVERE
020000970611     C                   Z-ADD     D00VER        CPLVER                         *VERSIONE
020100970611     C                   MOVEL     V1CNA2        CPLNAR                         *NAZIONE
020200970611     C                   MOVEL     V1CCA2        CPLCAP                         *CAP
020300970611     C                   MOVEL     V1CLO2        CPLLOC                         *LOCALITA'
020400970919     C                   MOVEL     *BLANKS       CPLFI1                         *FLAG OBSOLETO
020500970611     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
020600970611     C                   Z-ADD     *ZEROS        CPLDTR                         *DATA TRASMISSIONE
020700970611     C* SCRIVE
020800970611     C                   WRITE     AZCPL000
020900970918     C*
021000970918     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
021100970918     C                   Z-ADD     D00VER        D81VER
021200970918     C                   MOVEL     V1CNA2        D81NAR
021300970918     C                   MOVEL     V1CCA2        D81CAP
021400970918     C                   MOVEL     'L'           D81CHK
021500970918     C                   CALL      'TISI81R'
021600970918     C                   PARM                    DSSI81
021700000427     C*
021800000427     C*--- IMPOSTO IL RECORD DI ESTENSIONE POSTE DA SCRIVERE
021900000605     C                   Z-ADD     D00VER        KCLVER
022000000605     C                   MOVEL     V1CNAR        KCLNAR
022100000605     C                   MOVEL     V1CCAP        KCLCAP
022200000605     C                   MOVEL     V1CLOC        KCLLOC
022300000605     C     KEYCPL        CHAIN(N)  AZPPL01L                           99
022400000605     C                   MOVEL     DSPPL         SAVPPL
022500000605     C* LEGGO IL FUTURO RECORD "DUPLICAO" C DECISIONE WRITE O UPDATE
022600000605     C                   Z-ADD     D00VER        KCLVER                         *VERSIONE
022700000605     C                   MOVEL     V1CNA2        KCLNAR                         *NAZIONE
022800000605     C                   MOVEL     V1CCA2        KCLCAP                         *CAP
022900000605     C                   MOVEL     V1CLO2        KCLLOC                         *LOCALITA'
023000000605     C     KEYCPL        CHAIN     AZPPL01L                           99
023100000605     C                   MOVEL     SAVPPL        DSPPL
023200000427     C*
023300000427     C* IMPOSTA IL RECORD DA SCRIVERE
023400000427     C                   Z-ADD     D00VER        PPLVER                         *VERSIONE
023500000427     C                   MOVEL     V1CNA2        PPLNAR                         *NAZIONE
023600000427     C                   MOVEL     V1CCA2        PPLCAP                         *CAP
023700000427     C                   MOVEL     V1CLO2        PPLLOC                         *LOCALITA'
023800000427     C                   MOVEL     *BLANKS       PPLATB                         *FLAG ANNULLAMENTO
023900000427     C                   MOVEL     *BLANKS       PPLFTR                         *FLAG TRASMISSIONE
024000000427     C                   Z-ADD     *ZEROS        PPLDTR                         *DATA TRASMISSIONE
024100000427     C                   MOVEL     *BLANKS       PPLFL1                         *FLAG VARIE
024200000427     C* SCRIVE O AGGIORNA IN QUANTO L'ESTENSIONE X POSTE VIENE "DI CONSEGUENZA" AL CAPPARIO BART.
024300000427     C   99              WRITE     AZPPL000
024400000427     C  N99              UPDATE    AZPPL000
024500000427     C*--- END POSTE
024600000427     C*
024700000427     C* IMPOSTA FLAG DI FINE PROGRAMMA
024800000427     C                   MOVEL     MSG(20)       D00MSG
024900000427     C                   MOVEL     '1'           $FINE
025000000427E   2C                   ENDIF
025100970611E   1C                   ENDIF
025200970611     C*
025300970611     C                   ENDSR
025400970917     C*--------------------------------------------------------------------------------------------*
025500970917     C* CRECPS - CREA I SINONIMI DELLA NUOVA LOCALITA'
025600970917     C*--------------------------------------------------------------------------------------------*
025700970917     C     CRECPS        BEGSR
025800970917     C*
025900970917     C* LEGGE I SINONIMI DELLA LOCALITA' DA COPIARE
026000970917     C                   Z-ADD     D00VER        KCSVER
026100970917     C                   MOVEL     V1CNAR        KCSNAR
026200970917     C                   MOVEL     V1CCAP        KCSCAP
026300970917     C                   MOVEL     V1CLOC        KCSLOC
026400970917     C     KEYCPS        SETLL     AZCPS01L
026500970917     C     KEYCPS        READE(N)  AZCPS01L                               98
026600970917DO  1C     *IN98         DOWEQ     *OFF
026700970917IF  2C     CPSATB        IFEQ      *BLANKS
026800970917     C*
026900970917     C* SCRIVE IL SINONIMO DELLA NUOVA LOCALITA'
027000970917     C                   Z-ADD     D00VER        CPSVER                         *VERSIONE
027100970917     C                   MOVEL     V1CNA2        CPSNAR                         *NAZIONE
027200970917     C                   MOVEL     V1CCA2        CPSCAP                         *CAP
027300970917     C                   MOVEL     V1CLO2        CPSLOC                         *LOCALITA'
027400970917     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
027500970917     C                   Z-ADD     *ZEROS        CPSDTR                         *DATA TRASMISSIONE
027600970917     C                   WRITE     AZCPS000
027700970917E   2C                   ENDIF
027800970917     C     KEYCPS        READE(N)  AZCPS01L                               98
027900970917E   1C                   ENDDO
028000970917     C*
028100970917     C                   ENDSR
028200970611     C*--------------------------------------------------------------------------------------------*
028300970611     C* DECODI - CONTROLLA CAMPI E DECODIFICA
028400970611     C*--------------------------------------------------------------------------------------------*
028500970611     C     DECODI        BEGSR
028600970611     C*
028700970611     C* REIMPOSTA INDICATORI DI ERRORE
028800970611     C                   SETOFF                                       606162
028900970611     C*--------------------
029000970611     C* CAP
029100970611     C*--------------------
029200970611IF  1C     $ERR          IFEQ      '0'
029300970917     C                   MOVEL     V1CNA2        WNAR
029400970917     C                   MOVEL     V1CCA2        WCAP
029500970917     C                   MOVEL     *BLANKS       WFI1
029600970917     C                   EXSR      CHKCAP
029700970917IF  2C     $ERR          IFEQ      '1'
029800970611     C                   MOVEL     '1'           $ERR
029900970611     C                   MOVEL     MSG(8)        $MSG
030000970611     C                   EXSR      CARMSG
030100970611     C                   SETON                                        61
030200970611E   2C                   ENDIF
030300970611E   1C                   ENDIF
030400970917     C*
030500970917     C* IL CAP NON PUO' ESSERE OBSOLETO
030600970917IF  1C     $ERR          IFEQ      '0'
030700970917IF  2C     WFI1          IFEQ      'O '
030800970917     C                   MOVEL     '1'           $ERR
030900970917     C                   MOVEL     MSG(7)        $MSG
031000970917     C                   EXSR      CARMSG
031100970917     C                   SETON                                        6061
031200970917E   2C                   ENDIF
031300970917E   1C                   ENDIF
031400970611     C*--------------------
031500970611     C* LOCALITA'
031600970611     C*--------------------
031700970611IF  1C     $ERR          IFEQ      '0'
031800970611IF  2C     V1CLO2        IFEQ      *BLANKS
031900970611     C                   MOVEL     '1'           $ERR
032000970611     C                   MOVEL     MSG(9)        $MSG
032100970611     C                   EXSR      CARMSG
032200970611     C                   SETON                                        62
032300970611E   2C                   ENDIF
032400970611E   1C                   ENDIF
032500970611     C* NUOVA LOCALITA' INESISTENTE
032600970611IF  1C     $ERR          IFEQ      '0'
032700970611     C                   Z-ADD     D00VER        KCLVER
032800970611     C                   MOVEL     V1CNA2        KCLNAR
032900970611     C                   MOVEL     V1CCA2        KCLCAP
033000970611     C                   MOVEL     V1CLO2        KCLLOC
033100970611     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
033200970611IF  2C     *IN99         IFEQ      *OFF
033300970611     C                   MOVEL     '1'           $ERR
033400970611     C                   MOVEL     MSG(10)       $MSG
033500970611     C                   EXSR      CARMSG
033600970611     C                   SETON                                        606162
033700970611E   2C                   ENDIF
033800970611E   1C                   ENDIF
033900970611     C*
034000970611     C                   ENDSR
034100970917     C*------------------------------------------------------------------------*
034200970917     C* CHKCAP - CONTROLLA IL CAP
034300970917     C*------------------------------------------------------------------------*
034400970917     C     CHKCAP        BEGSR
034500970917     C*
034600970917     C     *LIKE         DEFINE    V1CNA2        WNAR
034700970917     C     *LIKE         DEFINE    V1CCA2        WCAP
034800970917     C     *LIKE         DEFINE    O95CF1        WFI1
034900970917     C*
035000970917     C* CONTROLLA IL CAP
035100970917     C                   CLEAR                   DSSI95
035200970917     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
035300970917     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
035400970917     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
035500970917     C                   MOVEL     WCAP          I95CAP                         *CAP
035600970917     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
035700970917     C                   CALL      'TISI95R'
035800970917     C                   PARM                    DSSI95
035900970917IF  1C     O95ERR        IFNE      *BLANKS
036000970919     C     O95LID        ORLT      '2'
036100970917     C                   MOVEL     '1'           $ERR
036200970917X   1C                   ELSE
036300970917     C                   MOVEL     O95CF1        WFI1
036400970917E   1C                   ENDIF
036500970917     C*
036600970917     C                   ENDSR
036700970611     C*--------------------------------------------------------------------------------------------*
036800970611     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
036900970611     C*--------------------------------------------------------------------------------------------*
037000970611     C     IMPVID        BEGSR
037100970611     C*
037200970611     C* CALCOLA LA DATA CORRENTE
037300970611     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
037400970611     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
037500970611     C                   Z-ADD     WNUM8         G08DAT
037600970611     C                   Z-ADD     *ZEROS        G08INV
037700970611     C                   MOVEL     '0'           G08ERR
037800970611     C                   CALL      'XSRDA8'
037900970611     C                   PARM                    WLBDA8
038000970611     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
038100970611     C*
038200970611     C* LEGGE LA LOCALITA'
038300970611     C                   Z-ADD     D00VER        KCLVER
038400970611     C                   MOVEL     D00NAR        KCLNAR
038500970611     C                   MOVEL     D00CAP        KCLCAP
038600970611     C                   MOVEL     D00LOC        KCLLOC
038700970611     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
038800970611IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
038900970917     C* DATI OLD
039000970611     C                   MOVEL     CPLNAR        V1CNAR                         *NAZIONE
039100970611     C                   MOVEL     CPLCAP        V1CCAP                         *CAP
039200970611     C                   MOVEL     CPLLOC        V1CLOC                         *LOCALITA'
039300970917     C* DATI NEW
039400970611     C                   MOVEL     V1CNAR        V1CNA2                         *NAZIONE
039500970611     C                   MOVEL     V1CCAP        V1CCA2                         *CAP
039600970917     C                   MOVEL     V1CLOC        V1CLO2                         *LOCALITA'
039700970611X   1C                   ELSE                                                   *NON TROVATO
039800970611     C                   MOVEL     '1'           $FINE                          *FINE PGM
039900970611     C                   MOVEL     '1'           D00ERR                         *ERRORE
040000970611     C                   MOVEL     MSG(19)       D00MSG                         *MESSSAGGIO
040100970611E   1C                   ENDIF
040200970611     C*
040300970611     C                   ENDSR
040400970917     C*------------------------------------------------------------------------*
040500970917     C* FINSR - OPERAZIONI FINALI
040600970917     C*------------------------------------------------------------------------*
040700970917     C     FINSR         BEGSR
040800970917     C*
040900970917     C                   CLEAR                   DSSI95
041000970917     C                   MOVEL     'C'           I95TLA
041100970917     C                   CALL      'TISI95R'
041200970917     C                   PARM                    DSSI95
041300970917     C*
041400970917     C                   ENDSR
041500020919     C*--------------------------------------------------------------------------------------------*
041600020919     C* REPERISCE I DATI UTENTE
041700020919     C*--------------------------------------------------------------------------------------------*
041800020919     C     REPDATIUTE    BEGSR
041900020919     C*
042000020919     C* INIZIALIZZA VARIABILI DI WRK
042100020919     C                   CLEAR                   TIBS34DS
042200020919     C                   CLEAR                   AZUTEDS
042300020919     C                   CLEAR                   DDATIUTE
042400020919     C*
042500020919     C     *DTAARA       DEFINE    §azute        azuteds
042600020919     C     *DTAARA       DEFINE    §datiute      ddatiute
042700020919     C                   IN(E)     *DTAARA
042800020919if  1C                   IF        %Error
042900020919     c                   EVAL      I34Tla = 'L'
043000020919     C                   CALL      'TIBS34R'
043100020919     C                   PARM                    Tibs34Ds
043200020919     C                   IN        *DTAARA
043300020919e   1C                   ENDIF
043400020919     C*
043500020919     C                   ENDSR
043600970611     C*--------------------------------------------------------------------------------------------*
043700970611     C* *INZSR  - ROUTINE INIZIALE
043800970611     C*--------------------------------------------------------------------------------------------*
043900970611     C     *INZSR        BEGSR
044000970611     C*--------------------
044100970611     C* RICEVIMENTO PARAMETRI
044200970611     C*--------------------
044300970611     C     *ENTRY        PLIST
044400970612     C                   PARM                    KPJBA
044500970612     C                   PARM                    DSSI00
044600970611     C*--------------------
044700970611     C* VARIABILI RIFERITE AL DATA BASE
044800970611     C*--------------------
044900970611     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL01L
045000970611     C     *LIKE         DEFINE    CPLNAR        KCLNAR
045100970611     C     *LIKE         DEFINE    CPLCAP        KCLCAP
045200970611     C     *LIKE         DEFINE    CPLLOC        KCLLOC
045300970917     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS01L
045400970917     C     *LIKE         DEFINE    CPSNAR        KCSNAR
045500970917     C     *LIKE         DEFINE    CPSCAP        KCSCAP
045600970917     C     *LIKE         DEFINE    CPSLOC        KCSLOC
045700000605     C*
045800000605     C* IMPOSTO CAMPO DI SALVATAGGIO DS
045900000605     C     *LIKE         DEFINE    DSPPL         SAVPPL
046000970611     C*--------------------
046100970611     C* CHIAVI DI LETTURA
046200970611     C*--------------------
046300970917     C* LETTURA AZCPL01L - COMPLETA
046400970611     C     KEYCPL        KLIST
046500970611     C                   KFLD                    KCLVER                         *VERSIONE
046600970611     C                   KFLD                    KCLNAR                         *NAZIONE
046700970611     C                   KFLD                    KCLCAP                         *CAP
046800970611     C                   KFLD                    KCLLOC                         *LOCALITA'
046900970917     C* LETTURA AZCPS01L - PARZIALE
047000970917     C     KEYCPS        KLIST
047100970917     C                   KFLD                    KCSVER                         *VERSIONE
047200970917     C                   KFLD                    KCSNAR                         *NAZIONE
047300970917     C                   KFLD                    KCSCAP                         *CAP
047400970917     C                   KFLD                    KCSLOC                         *LOCALITA'
047500970611     C***
047600970611     C* IMPOSTAZIONI VIDEO
047700970611     C***
047800970611IF  1C     D00OP0        IFEQ      'L02'
047900970611     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
048000970611X   1C                   ELSE
048100970611     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
048200970611E   1C                   ENDIF
048300970611     C*
048400970611     C* ESEGUE OPERAZIONI INIZIALI STANDARD
048500970611     C                   EXSR      INZSTD
048600970611     C*
048700970611     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
048800970611     C                   EXSR      IMPVID
048900970611     C*
049000970611     C* EMETTO PRIMO FORMATO VIDEO
049100970611     C                   EXSR      CARV1
049200970611     C*
049300970611     C                   ENDSR
049400970611     C*--------------------------------------------------------------------------------------------*
049500970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049600970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049700970611     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
049800970611     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
049900970611     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
050000970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050100970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
050200970611     C*--------------------------------------------------------------------------------------------*
050300970611     C* INZSTD - ROUTINE INIZIALE STANDARD
050400970611     C*--------------------------------------------------------------------------------------------*
050500970611     C     INZSTD        BEGSR
050600970611     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
050700970611     C                   CALL      'XKEYS'
050800970611     C                   PARM                    DSKEYS
050900970611     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
051000970611     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
051100970611     C     XMSGPA        PLIST
051200970611     C                   PARM                    DSMSMS
051300970611     C                   PARM                    DSMSPG
051400970611     C                   PARM                    DSMSKE
051500970611     C                   PARM                    DSMSER
051600970611     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
051700970611     C                   MOVEL     'TISI32R'     DSMSPG                         *NOME PGM CHIAMANTE
051800970611     C                   MOVEL     'TISI32R'     VMCQUE                         *CODA MESSAGGI DESTI
051900970611     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
052000970611     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
052100970611     C* INIZIALIZZO FLAG CONTROLLO PGM
052200970611     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
052300970611     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
052400970611     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
052500970611     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
052600970611     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
052700970611     C                   MOVEL     '0'           $FINE             1            *FINE PGM
052800970611     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
052900970611     C* LANCIO ROUTINE PER VERIFICARE SE
053000970611     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
053100970611     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
053200020919     C                   EXSR      REPDATIUTE
053300970611     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
053400970611     C                   BITOFF    '01234567'    POSMI1            1
053500970611     C                   BITOFF    '01234567'    POSMI2            1
053600970611     C                   BITOFF    '01234567'    POSMA1            1
053700970611     C                   BITOFF    '01234567'    POSMA2            1
053800970611     C                   BITON     '34'          POSMI1            1            *18
053900970611     C                   BITON     '7'           POSMI2            1            *01
054000970611     C                   BITON     '34'          POSMA1            1            *18
054100970611     C                   BITON     '0'           POSMA2            1            *80
054200970611     C                   MOVE      POSMI2        POSMIN            2
054300970611     C                   MOVEL     POSMI1        POSMIN
054400970611     C                   MOVE      POSMA2        POSMAX            2
054500970611     C                   MOVEL     POSMA1        POSMAX
054600970611     C                   ENDSR
054700970611     C*--------------------------------------------------------------------------------------------*
054800970611     C* FUNSTD - LANCIA SUBROUTINE STANDARD
054900970611     C*--------------------------------------------------------------------------------------------*
055000970611     C     FUNSTD        BEGSR
055100970611     C                   SELECT
055200970611     C     KEY           WHENEQ    F3
055300970611     C                   EXSR      F03STD
055400970611     C     KEY           WHENEQ    F12
055500970611     C                   EXSR      F12STD
055600970611     C     KEY           WHENEQ    F24
055700970611     C                   EXSR      F24STD
055800970611     C                   OTHER
055900970611     C                   MOVEL     MSG(2)        $MSG
056000970611     C                   EXSR      CARMSG
056100970611     C                   ENDSL
056200970611     C                   ENDSR
056300970611     C*--------------------------------------------------------------------------------------------*
056400970611     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
056500970611     C*--------------------------------------------------------------------------------------------*
056600970611     C     F03STD        BEGSR
056700970611     C* IMPOSTA FLAG DI FINE PROGRAMMA
056800970611     C                   MOVEL     '1'           $FINE
056900970611     C* IMPOSTA FLAG DI FINE PROCEDURA
057000970611     C                   MOVEL     '1'           $F03
057100970611     C* PULISCE IL MESSAGGIO
057200970611     C                   MOVEL     *BLANKS       D00MSG
057300970611     C                   ENDSR
057400970611     C*--------------------------------------------------------------------------------------------*
057500970611     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
057600970611     C*--------------------------------------------------------------------------------------------*
057700970611     C     F12STD        BEGSR
057800970611     C* IMPOSTA FLAG DI FINE PROGRAMMA
057900970611     C                   MOVEL     '1'           $FINE
058000970611     C* IMPOSTA FLAG DI FINE PROCEDURA
058100970611     C                   MOVEL     '1'           $F12
058200970611     C* PULISCE IL MESSAGGIO
058300970611     C                   MOVEL     *BLANKS       D00MSG
058400970611     C                   ENDSR
058500970611     C*--------------------------------------------------------------------------------------------*
058600970611     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
058700970611     C*--------------------------------------------------------------------------------------------*
058800970611     C     F24STD        BEGSR
058900970611     C*
059000970611     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
059100970611     C                   EXSR      OUTFUN
059200970611     C*
059300970611     C                   ENDSR
059400970611     C*--------------------------------------------------------------------------------------------*
059500970611     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
059600970611     C*          CODA MESSAGGI DEL PGM
059700970611     C*--------------------------------------------------------------------------------------------*
059800970611     C     CARMSG        BEGSR
059900970611     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
060000970611     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
060100970611     C     MSGULT        IFEQ      0
060200970611     C                   MOVEL     '1'           *IN40
060300970611     C                   WRITE     SI32DVK
060400970611     C                   MOVEL     '0'           *IN40
060500970611     C                   ENDIF
060600970611     C* INVIA MESSAGGIO ALLA CODA DEL PGM
060700970611     C                   MOVEL     $MSG          DSMSMS
060800970611     C                   CALL      'XMSG'        XMSGPA
060900970611     C     DSMSER        IFEQ      0
061000970611     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
061100970611     C                   ENDIF
061200970611     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
061300970611     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
061400970611     C                   WRITE     SI32DVM                                      *SFL MESS
061500970611     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
061600970611     C                   MOVEL     *BLANKS       $MSG
061700970611     C                   ENDSR
061800970611     C*--------------------------------------------------------------------------------------------*
061900970611     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
062000970611     C*--------------------------------------------------------------------------------------------*
062100970611     C     OUTMSG        BEGSR
062200970611     C* VISUALIZZAZIONE MESSAGGIO
062300970611     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
062400970611     C                   WRITE     SI32DVK                                      *CONTR MESS
062500970611     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
062600970611     C                   Z-ADD     *ZEROS        MSGULT
062700970611     C                   ENDSR
062800970611     C*--------------------------------------------------------------------------------------------*
062900970611     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
063000970611     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
063100970611     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
063200970611     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
063300970611     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
063400970611     C*--------------------------------------------------------------------------------------------*
063500970611     C     CARFUN        BEGSR
063600970611     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
063700970611     C     1             DO        31            I                 5 0          --- 1 -->
063800970611     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
063900970611     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
064000970611     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
064100020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
064200970611     C     $TGES         IFEQ      'G'                                          *GESTIONE
064300970611     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
064400970611     C                   ELSE                                                   *INTERROG.
064500970611     C                   MOVEL     FUNASI        FAT(I)
064600970611     C                   ENDIF
064700970611     C                   ELSE                                                   <-- 2 --> *FILIALE
064800970611     C     $TGES         IFEQ      'G'                                          *GESTIONE
064900970611     C                   MOVEL     FUNAFG        FAT(I)
065000970611     C                   ELSE                                                   *INTERROG.
065100970611     C                   MOVEL     FUNAFI        FAT(I)
065200970611     C                   ENDIF
065300970611     C                   ENDIF                                                  <-- 2 ---
065400970611     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
065500970611     C                   MOVEL     USER          WEDP              3            *PROFILO
065600970611     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
065700970611     C                   MOVEL     'S'           FAT(I)                         *ABILITO
065800970611     C                   ELSE
065900970611     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
066000970611     C                   ENDIF
066100970611     C                   ENDIF
066200970611     C                   ENDDO                                                  <-- 1 ---
066300970611     C                   ENDSR
066400970611     C*--------------------------------------------------------------------------------------------*
066500970611     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
066600970611     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
066700970611     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
066800970611     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
066900970611     C*--------------------------------------------------------------------------------------------*
067000970611     C     OUTFUN        BEGSR
067100970611     C     1             DO        8             I                              *PULIZIA FUNZIONI A
067200970611     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
067300970611     C                   ENDDO
067400970611     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
067500970611     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
067600970611     C     1             DO        31            I
067700970611     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
067800970611     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
067900970611     C                   ADD       1             FUNNAT
068000970611     C                   ENDIF
068100970611     C                   ENDDO
068200970611     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
068300970611     C     FUNNAT        IFGT      8
068400970611     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
068500970611     C                   ENDIF
068600970611     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
068700970611     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
068800970611     C                   Z-ADD     8             FUNVIS            5 0
068900970611     C     FAT(23)       IFEQ      'S'
069000970611     C                   SUB       1             FUNVIS
069100970611     C                   ENDIF
069200970611     C     FAT(24)       IFEQ      'S'
069300970611     C                   SUB       1             FUNVIS
069400970611     C                   ENDIF
069500970611     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
069600970611     C                   Z-ADD     *ZEROS        W                 5 0
069700970611     C                   ADD       1             FUNULT
069800970611     C     FUNULT        IFGT      31
069900970611     C                   Z-ADD     1             FUNULT
070000970611     C                   ENDIF
070100970611     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
070200970611     C     FUNULT        DO        31            I                              --- 1 -->
070300970611     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
070400970611     C     W             IFLT      FUNVIS                                       --- 2 -->
070500970611     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
070600970611     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
070700970611     C     FDE(I)        ANDNE     *BLANKS
070800970611     C     I             IFEQ      23                                           *TOLGO F23
070900970611     C     I             OREQ      24                                           *E F24
071000970611     C                   ELSE
071100970611     C                   ADD       1             W
071200970611     C                   MOVEL     FDE(I)        FUV(W)
071300970611     C                   ENDIF
071400970611     C                   ENDIF                                                  <-- 3 ---
071500970611     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
071600970611     C                   Z-ADD     I             FUNULT
071700970611     C                   ENDIF                                                  <-- 2 ---
071800970611     C                   ENDDO                                                  <-- 1 ---
071900970611     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
072000970611     C                   MOVEL     FUV(1)        VFD001
072100970611     C                   MOVEL     FUV(2)        VFD002
072200970611     C                   MOVEL     FUV(3)        VFD003
072300970611     C                   MOVEL     FUV(4)        VFD004
072400970611     C                   MOVEL     FUV(5)        VFD005
072500970611     C                   MOVEL     FUV(6)        VFD006
072600970611     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
072700970611     C     FAT(24)       ANDEQ     'N'
072800970611     C                   MOVEL     FUV(7)        VFD007
072900970611     C                   MOVEL     FUV(8)        VFD008
073000970611     C                   ELSE                                                   <-- 1 -->
073100970611     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
073200970611     C     FAT(24)       ANDEQ     'S'
073300970611     C                   MOVEL     FDE(23)       VFD007
073400970611     C                   MOVEL     FDE(24)       VFD008
073500970611     C                   ELSE                                                   <-- 2 -->
073600970611     C                   MOVEL     FUV(7)        VFD007
073700970611     C     FAT(23)       IFEQ      'S'
073800970611     C                   MOVEL     FDE(23)       VFD008
073900970611     C                   ELSE
074000970611     C                   MOVEL     FDE(24)       VFD008
074100970611     C                   ENDIF
074200970611     C                   ENDIF                                                  <-- 2 ---
074300970611     C                   ENDIF                                                  <-- 1 ---
074400970611     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
074500970611     C                   ENDSR
074600970611     C*--------------------------------------------------------------------------------------------*
074700970611     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
074800970611     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
074900970611     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
075000970611     C*                   - ERRORE SE FUNZIONE NON ATTIVA
075100970611     C*--------------------------------------------------------------------------------------------*
075200970611     C     CONFUN        BEGSR
075300970611     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
075400970611     C     1             DO        31            I                              --- 1 -->
075500970611     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
075600970611     C* CONTROLLO SE IL TASTO E' ATTIVO
075700970611     C     FAT(I)        IFNE      'S'                                          --- 3 -->
075800970611     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
075900970611     C                   EXSR      CARMSG
076000970611     C                   MOVEL     '1'           $ERR
076100970611     C                   ENDIF                                                  <-- 3 ---
076200970611     C                   ENDIF                                                  <-- 2 ---
076300970611     C                   ENDDO                                                  <-- 1 ---
076400970611     C                   ENDSR
076500970611     O*--------------------------------------------------------------------------------------------*
076600970611** -MSG- (MESSAGGI)
076700970611TISI32R- Tasto funzionale non valido.                                          1
076800970611TISI32R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
076900970611TISI32R- Opzione non valida.                                                   3
077000970611TISI32R- Opzione abilitata ma non gestita. Avvertire CED.                      4
077100970611TISI32R- Inizio elenco.                                                        5
077200970611TISI32R- Fine elenco.                                                          6
077300970917TISI32R- Impossibile inserire località di CAP obsoleti                         7
077400970919TISI32R- Cap inesistente o non valido                                          8
077500970611TISI32R- Immettere la località                                                 9
077600970611TISI32R- Località già inserita                                                10
077700970611TISI32R- 11
077800970611TISI32R- 12
077900970611TISI32R- 13
078000970611TISI32R- 14
078100970611TISI32R- 15
078200970611TISI32R- 16
078300970611TISI32R- 17
078400970611TISI32R- 18
078500970917TISI32R- Errori in entrata duplica. Non trovato il record da cui copiare      19
078600970917TISI32R- La località e gli archivi correlati sono stati duplicati             20
078700970611** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
078800970611F1    -                   -    NN     NN
078900970611F2    -                   -    NN     NN
079000970611F3    -F3=Fine            -    NN     NN
079100970611F4    -                   -    NN     NN
079200970611F5    -                   -    NN     NN
079300970611F6    -F6=Conferma        -    SN     SN
079400970611F7    -                   -    NN     NN
079500970611F8    -                   -    NN     NN
079600970611F9    -                   -    NN     NN
079700970611F10   -                   -    NN     NN
079800970611F11   -                   -    NN     NN
079900970611F12   -F12=Ritorno        -    SS     SS
080000970611F13   -                   -    NN     NN
080100970611F14   -                   -    NN     NN
080200970611F15   -                   -    NN     NN
080300970611F16   -                   -    NN     NN
080400970611F17   -                   -    NN     NN
080500970611F18   -                   -    NN     NN
080600970611F19   -                   -    NN     NN
080700970611F20   -                   -    NN     NN
080800970611F21   -                   -    NN     NN
080900970611F22   -                   -    NN     NN
081000970611F23   -                   -    NN     NN
081100970611F24   -F24=Altri tasti    -    NN     NN
081200970611ENTER -                   -    SS     SS
081300970611ROLLUP-                   -    NN     NN
081400970611ROLLDN-                   -    NN     NN
081500970611HELP  -                   -    NN     NN
081600970611PRINT -                   -    NN     NN
081700970611BSPACE-                   -    NN     NN
081800970611CLEAR -                   -    NN     NN

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
001200970611     D*--------------------------------------------------------------------------------------------*
001300970611     D* SCHIERE
001400970611     D*--------------------------------------------------------------------------------------------*
001500970611     D*-------------------
001600970611     D* MESSAGGI E FUNZIONI
001700970611     D*-------------------
001800970611     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001900970611     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002000970611     D*-------------------
002100970611     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002200970611     D*-------------------
002300970611     D FFU             S              6    DIM(31)                              *FUNZIONE
002400970611     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002500970611     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002600970611     D*-------------------
002700970611     D* TABELLA DESCRIZIONI A VIDEO
002800970611     D*-------------------
002900970611     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003000970611     D*-------------------
003100970611     D* TABELLA TASTI FUNZIONALI PREMUTI
003200970611     D*-------------------
003300970611     D KEYS            S              1    DIM(40)
003400970611     D*--------------------------------------------------------------------------------------------*
003500970611     D* INPUT
003600970611     D*--------------------------------------------------------------------------------------------*
003700970611     D*-------------------
003800970611     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003900970611     D*-------------------
004000970611     D DSFUN         E DS
004100970611     D  FUNRIG                 1     40
004200970611     D*-------------------
004300970611     D* DS PER GESTIONE TASTI FUNZIONALI
004400970611     D*-------------------
004500970611     D DSKEYS        E DS
004600970611     D  KEYRIG                 1     40
004700970611     D*-------------------
004800970611     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004900970611     D*-------------------
005000970611     D                SDS
005100970611     D  USER                 254    263
005200970611     D*-------------------
005300970611     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005400970611     D*-------------------
005500970611     D DEVDS1          DS
005600970611     D  KEY                  369    369
005700970611     D  CURSOR               370    371
005800970611     D*---------------------
005900970611     D* ARCHITETTURA
006000970611     D*---------------------
006100970611     D KPJBA         E DS
006200970611     D*---------------------
006300970611     D* DS ESTERNA DI PROCEDURA
006400970611     D*---------------------
006500970611     D DSSI00        E DS                  EXTNAME(TISI00DS)
006600970611     D*-------------------
006700020919     D* DS REPERIMENTO DATI UTENTE
006800970611     D*-------------------
006900020919     D TIBS34DS      E DS                                                       *Profili utente
007000020919     D DDATIUTE      E DS                                                       *Dati utente
007100020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007200970917     D*---------------------
007300970918     D* DS "TISI95R" - CONTROLLA CAP
007400970917     D*---------------------
007500970917     D DSSI95        E DS                  EXTNAME(TISI95DS)
007600970918     D*------------
007700970918     D* DS "TISI81R" - AGGIORNA UN CAP DAI SUOI ARCHIVI
007800970918     D*------------
007900970918     D DSSI81        E DS                  EXTNAME(TISI81DS)
008000970611     D*-------------------
008100970611     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
008200970611     D*-------------------
008300970611     D DSMSG         E DS
008400970611     D*------------------
008500970611     D* DS "XSRDA8" - CONTROLLA DATA (8)
008600970611     D*------------------
008700970611     D WLBDA8          DS
008800970611     D  G08DAT                 1      8  0
008900970611     D  G08INV                 9     16  0
009000970611     D  G08ERR                17     17
009100970611     D  G08TGI                18     22  0
009200970611     C*--------------------------------------------------------------------------------------------*
009300970611     C* MAIN LINE
009400970611     C*--------------------------------------------------------------------------------------------*
009500970611     C*
009600970611     C* SE NON CI SONO ERRORI IN ENTRATA
009700970611IF  1C     $FINE         IFEQ      '0'                                          *NO ERRORI
009800970611     C                   WRITE     SI32DVT                                      *TESTATA
009900970611DO  2C     $FINE         DOUEQ     '1'
010000970611     C*
010100970611     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010200970611IF  3C     $GES          IFEQ      'V1'
010300970611     C                   EXSR      GESV1
010400970611E   3C                   ENDIF
010500970611     C*
010600970611E   2C                   ENDDO
010700970611     C*
010800970611IF  2C     $F03          IFEQ      '1'
010900970611     C                   MOVEL     '1'           D00F03
011000970611E   2C                   ENDIF
011100970611E   1C                   ENDIF
011200970917     C*
011300970917     C* OPERAZIONI FINALI
011400970917     C                   EXSR      FINSR
011500970611     C* FINE PGM
011600970611     C                   SETON                                        LR
011700970611     C*--------------------------------------------------------------------------------------------*
011800970611     C* GESV1 - GESTIONE VIDEO UNO
011900970611     C*--------------------------------------------------------------------------------------------*
012000970611     C     GESV1         BEGSR
012100970611     C*
012200970611IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
012300970611     C                   EXSR      CARV1                                        *CARICA VIDEO
012400970611E   1C                   ENDIF
012500970611     C*
012600970611IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012700970611     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012800970611     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012900970611E   1C                   ENDIF
013000970611     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
013100970611     C*
013200970611     C                   EXFMT     SI32DV1                                      *PARAMETRI
013300970611     C*
013400970611     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013500970611     C*
013600970611IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013700970611SL  2C                   SELECT
013800970611     C     KEY           WHENEQ    ENTER
013900970611     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
014000970611     C     KEY           WHENEQ    F6
014100970611     C                   EXSR      F06GES
014200970611     C                   OTHER
014300970611     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014400970611E   2C                   ENDSL
014500970611E   1C                   ENDIF
014600970611     C*
014700970611     C                   ENDSR
014800970611     C*--------------------------------------------------------------------------------------------*
014900970611     C* CARV1 - CARICA VIDEO UNO
015000970611     C*--------------------------------------------------------------------------------------------*
015100970611     C     CARV1         BEGSR
015200970611     C*
015300970611     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015400970611     C*
015500970611     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015600970611     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015700970611     C*
015800970611     C                   ENDSR
015900970611     C*--------------------------------------------------------------------------------------------*
016000970611     C* ENTV1 - GESTIONE TASTO ENTER
016100970611     C*--------------------------------------------------------------------------------------------*
016200970611     C     ENTV1         BEGSR
016300970611     C*
016400970611     C* CONTROLLA I CAMPI IMMESSI
016500970611     C                   EXSR      DECODI
016600970611     C*
016700970611     C                   ENDSR
016800970611     C*--------------------------------------------------------------------------------------------*
016900970611     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
017000970611     C*--------------------------------------------------------------------------------------------*
017100970611     C     F06GES        BEGSR
017200970611     C*
017300970611     C* CONTROLLA I CAMPI IMMESSI
017400970611     C                   EXSR      DECODI
017500970611     C*
017600970611     C* RILEGGE LA LOCALITA' DA CUI COPIARE
017700970611IF  1C     $ERR          IFEQ      '0'
017800970611     C                   Z-ADD     D00VER        KCLVER
017900970611     C                   MOVEL     V1CNAR        KCLNAR
018000970611     C                   MOVEL     V1CCAP        KCLCAP
018100970611     C                   MOVEL     V1CLOC        KCLLOC
018200970611     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
018300970611IF  2C     *IN99         IFEQ      *OFF
018400970917     C*
018500970917     C* CREA I SINONIMI DELLA NUOVA LOCALITA'
018600970917IF  3C     CPLESS        IFEQ      'S'                                          *ESISTONO SINONIMI
018700970917IF  4C     V1CLOC        IFEQ      V1CLO2                                       *SE UGUALE
018800970917     C                   EXSR      CRECPS                                       *CREA SINONIMI
018900970917X   4C                   ELSE                                                   *NON UGUALE
019000970917     C                   MOVEL     'N'           CPLESS                         *NON ESISTONO SINONI
019100970917E   4C                   ENDIF
019200970917E   3C                   ENDIF
019300970611     C*
019400970611     C* IMPOSTA IL RECORD DA SCRIVERE
019500970611     C                   Z-ADD     D00VER        CPLVER                         *VERSIONE
019600970611     C                   MOVEL     V1CNA2        CPLNAR                         *NAZIONE
019700970611     C                   MOVEL     V1CCA2        CPLCAP                         *CAP
019800970611     C                   MOVEL     V1CLO2        CPLLOC                         *LOCALITA'
019900970919     C                   MOVEL     *BLANKS       CPLFI1                         *FLAG OBSOLETO
020000970611     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
020100970611     C                   Z-ADD     *ZEROS        CPLDTR                         *DATA TRASMISSIONE
020200970611     C* SCRIVE
020300970611     C                   WRITE     AZCPL000
020400970918     C*
020500970918     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
020600970918     C                   Z-ADD     D00VER        D81VER
020700970918     C                   MOVEL     V1CNA2        D81NAR
020800970918     C                   MOVEL     V1CCA2        D81CAP
020900970918     C                   MOVEL     'L'           D81CHK
021000970918     C                   CALL      'TISI81R'
021100970918     C                   PARM                    DSSI81
021200000427     C*
021300000427     C* IMPOSTA FLAG DI FINE PROGRAMMA
021400000427     C                   MOVEL     MSG(20)       D00MSG
021500000427     C                   MOVEL     '1'           $FINE
021600000427E   2C                   ENDIF
021700970611E   1C                   ENDIF
021800970611     C*
021900970611     C                   ENDSR
022000970917     C*--------------------------------------------------------------------------------------------*
022100970917     C* CRECPS - CREA I SINONIMI DELLA NUOVA LOCALITA'
022200970917     C*--------------------------------------------------------------------------------------------*
022300970917     C     CRECPS        BEGSR
022400970917     C*
022500970917     C* LEGGE I SINONIMI DELLA LOCALITA' DA COPIARE
022600970917     C                   Z-ADD     D00VER        KCSVER
022700970917     C                   MOVEL     V1CNAR        KCSNAR
022800970917     C                   MOVEL     V1CCAP        KCSCAP
022900970917     C                   MOVEL     V1CLOC        KCSLOC
023000970917     C     KEYCPS        SETLL     AZCPS01L
023100970917     C     KEYCPS        READE(N)  AZCPS01L                               98
023200970917DO  1C     *IN98         DOWEQ     *OFF
023300970917IF  2C     CPSATB        IFEQ      *BLANKS
023400970917     C*
023500970917     C* SCRIVE IL SINONIMO DELLA NUOVA LOCALITA'
023600970917     C                   Z-ADD     D00VER        CPSVER                         *VERSIONE
023700970917     C                   MOVEL     V1CNA2        CPSNAR                         *NAZIONE
023800970917     C                   MOVEL     V1CCA2        CPSCAP                         *CAP
023900970917     C                   MOVEL     V1CLO2        CPSLOC                         *LOCALITA'
024000970917     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
024100970917     C                   Z-ADD     *ZEROS        CPSDTR                         *DATA TRASMISSIONE
024200970917     C                   WRITE     AZCPS000
024300970917E   2C                   ENDIF
024400970917     C     KEYCPS        READE(N)  AZCPS01L                               98
024500970917E   1C                   ENDDO
024600970917     C*
024700970917     C                   ENDSR
024800970611     C*--------------------------------------------------------------------------------------------*
024900970611     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025000970611     C*--------------------------------------------------------------------------------------------*
025100970611     C     DECODI        BEGSR
025200970611     C*
025300970611     C* REIMPOSTA INDICATORI DI ERRORE
025400970611     C                   SETOFF                                       606162
025500970611     C*--------------------
025600970611     C* CAP
025700970611     C*--------------------
025800970611IF  1C     $ERR          IFEQ      '0'
025900970917     C                   MOVEL     V1CNA2        WNAR
026000970917     C                   MOVEL     V1CCA2        WCAP
026100970917     C                   MOVEL     *BLANKS       WFI1
026200970917     C                   EXSR      CHKCAP
026300970917IF  2C     $ERR          IFEQ      '1'
026400970611     C                   MOVEL     '1'           $ERR
026500970611     C                   MOVEL     MSG(8)        $MSG
026600970611     C                   EXSR      CARMSG
026700970611     C                   SETON                                        61
026800970611E   2C                   ENDIF
026900970611E   1C                   ENDIF
027000970917     C*
027100970917     C* IL CAP NON PUO' ESSERE OBSOLETO
027200970917IF  1C     $ERR          IFEQ      '0'
027300970917IF  2C     WFI1          IFEQ      'O '
027400970917     C                   MOVEL     '1'           $ERR
027500970917     C                   MOVEL     MSG(7)        $MSG
027600970917     C                   EXSR      CARMSG
027700970917     C                   SETON                                        6061
027800970917E   2C                   ENDIF
027900970917E   1C                   ENDIF
028000970611     C*--------------------
028100970611     C* LOCALITA'
028200970611     C*--------------------
028300970611IF  1C     $ERR          IFEQ      '0'
028400970611IF  2C     V1CLO2        IFEQ      *BLANKS
028500970611     C                   MOVEL     '1'           $ERR
028600970611     C                   MOVEL     MSG(9)        $MSG
028700970611     C                   EXSR      CARMSG
028800970611     C                   SETON                                        62
028900970611E   2C                   ENDIF
029000970611E   1C                   ENDIF
029100970611     C* NUOVA LOCALITA' INESISTENTE
029200970611IF  1C     $ERR          IFEQ      '0'
029300970611     C                   Z-ADD     D00VER        KCLVER
029400970611     C                   MOVEL     V1CNA2        KCLNAR
029500970611     C                   MOVEL     V1CCA2        KCLCAP
029600970611     C                   MOVEL     V1CLO2        KCLLOC
029700970611     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
029800970611IF  2C     *IN99         IFEQ      *OFF
029900970611     C                   MOVEL     '1'           $ERR
030000970611     C                   MOVEL     MSG(10)       $MSG
030100970611     C                   EXSR      CARMSG
030200970611     C                   SETON                                        606162
030300970611E   2C                   ENDIF
030400970611E   1C                   ENDIF
030500970611     C*
030600970611     C                   ENDSR
030700970917     C*------------------------------------------------------------------------*
030800970917     C* CHKCAP - CONTROLLA IL CAP
030900970917     C*------------------------------------------------------------------------*
031000970917     C     CHKCAP        BEGSR
031100970917     C*
031200970917     C     *LIKE         DEFINE    V1CNA2        WNAR
031300970917     C     *LIKE         DEFINE    V1CCA2        WCAP
031400970917     C     *LIKE         DEFINE    O95CF1        WFI1
031500970917     C*
031600970917     C* CONTROLLA IL CAP
031700970917     C                   CLEAR                   DSSI95
031800970917     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
031900970917     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
032000970917     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
032100970917     C                   MOVEL     WCAP          I95CAP                         *CAP
032200970917     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
032300970917     C                   CALL      'TISI95R'
032400970917     C                   PARM                    DSSI95
032500970917IF  1C     O95ERR        IFNE      *BLANKS
032600970919     C     O95LID        ORLT      '2'
032700970917     C                   MOVEL     '1'           $ERR
032800970917X   1C                   ELSE
032900970917     C                   MOVEL     O95CF1        WFI1
033000970917E   1C                   ENDIF
033100970917     C*
033200970917     C                   ENDSR
033300970611     C*--------------------------------------------------------------------------------------------*
033400970611     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
033500970611     C*--------------------------------------------------------------------------------------------*
033600970611     C     IMPVID        BEGSR
033700970611     C*
033800970611     C* CALCOLA LA DATA CORRENTE
033900970611     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
034000970611     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
034100970611     C                   Z-ADD     WNUM8         G08DAT
034200970611     C                   Z-ADD     *ZEROS        G08INV
034300970611     C                   MOVEL     '0'           G08ERR
034400970611     C                   CALL      'XSRDA8'
034500970611     C                   PARM                    WLBDA8
034600970611     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
034700970611     C*
034800970611     C* LEGGE LA LOCALITA'
034900970611     C                   Z-ADD     D00VER        KCLVER
035000970611     C                   MOVEL     D00NAR        KCLNAR
035100970611     C                   MOVEL     D00CAP        KCLCAP
035200970611     C                   MOVEL     D00LOC        KCLLOC
035300970611     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
035400970611IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
035500970917     C* DATI OLD
035600970611     C                   MOVEL     CPLNAR        V1CNAR                         *NAZIONE
035700970611     C                   MOVEL     CPLCAP        V1CCAP                         *CAP
035800970611     C                   MOVEL     CPLLOC        V1CLOC                         *LOCALITA'
035900970917     C* DATI NEW
036000970611     C                   MOVEL     V1CNAR        V1CNA2                         *NAZIONE
036100970611     C                   MOVEL     V1CCAP        V1CCA2                         *CAP
036200970917     C                   MOVEL     V1CLOC        V1CLO2                         *LOCALITA'
036300970611X   1C                   ELSE                                                   *NON TROVATO
036400970611     C                   MOVEL     '1'           $FINE                          *FINE PGM
036500970611     C                   MOVEL     '1'           D00ERR                         *ERRORE
036600970611     C                   MOVEL     MSG(19)       D00MSG                         *MESSSAGGIO
036700970611E   1C                   ENDIF
036800970611     C*
036900970611     C                   ENDSR
037000970917     C*------------------------------------------------------------------------*
037100970917     C* FINSR - OPERAZIONI FINALI
037200970917     C*------------------------------------------------------------------------*
037300970917     C     FINSR         BEGSR
037400970917     C*
037500970917     C                   CLEAR                   DSSI95
037600970917     C                   MOVEL     'C'           I95TLA
037700970917     C                   CALL      'TISI95R'
037800970917     C                   PARM                    DSSI95
037900970917     C*
038000970917     C                   ENDSR
038100020919     C*--------------------------------------------------------------------------------------------*
038200020919     C* REPERISCE I DATI UTENTE
038300020919     C*--------------------------------------------------------------------------------------------*
038400020919     C     REPDATIUTE    BEGSR
038500020919     C*
038600020919     C* INIZIALIZZA VARIABILI DI WRK
038700020919     C                   CLEAR                   TIBS34DS
038800020919     C                   CLEAR                   AZUTEDS
038900020919     C                   CLEAR                   DDATIUTE
039000020919     C*
039100020919     C     *DTAARA       DEFINE    §azute        azuteds
039200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
039300020919     C                   IN(E)     *DTAARA
039400020919if  1C                   IF        %Error
039500020919     c                   EVAL      I34Tla = 'L'
039600020919     C                   CALL      'TIBS34R'
039700020919     C                   PARM                    Tibs34Ds
039800020919     C                   IN        *DTAARA
039900020919e   1C                   ENDIF
040000020919     C*
040100020919     C                   ENDSR
040200970611     C*--------------------------------------------------------------------------------------------*
040300970611     C* *INZSR  - ROUTINE INIZIALE
040400970611     C*--------------------------------------------------------------------------------------------*
040500970611     C     *INZSR        BEGSR
040600970611     C*--------------------
040700970611     C* RICEVIMENTO PARAMETRI
040800970611     C*--------------------
040900970611     C     *ENTRY        PLIST
041000970612     C                   PARM                    KPJBA
041100970612     C                   PARM                    DSSI00
041200970611     C*--------------------
041300970611     C* VARIABILI RIFERITE AL DATA BASE
041400970611     C*--------------------
041500970611     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL01L
041600970611     C     *LIKE         DEFINE    CPLNAR        KCLNAR
041700970611     C     *LIKE         DEFINE    CPLCAP        KCLCAP
041800970611     C     *LIKE         DEFINE    CPLLOC        KCLLOC
041900970917     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS01L
042000970917     C     *LIKE         DEFINE    CPSNAR        KCSNAR
042100970917     C     *LIKE         DEFINE    CPSCAP        KCSCAP
042200970917     C     *LIKE         DEFINE    CPSLOC        KCSLOC
042300000605     C*
042400970611     C*--------------------
042500970611     C* CHIAVI DI LETTURA
042600970611     C*--------------------
042700970917     C* LETTURA AZCPL01L - COMPLETA
042800970611     C     KEYCPL        KLIST
042900970611     C                   KFLD                    KCLVER                         *VERSIONE
043000970611     C                   KFLD                    KCLNAR                         *NAZIONE
043100970611     C                   KFLD                    KCLCAP                         *CAP
043200970611     C                   KFLD                    KCLLOC                         *LOCALITA'
043300970917     C* LETTURA AZCPS01L - PARZIALE
043400970917     C     KEYCPS        KLIST
043500970917     C                   KFLD                    KCSVER                         *VERSIONE
043600970917     C                   KFLD                    KCSNAR                         *NAZIONE
043700970917     C                   KFLD                    KCSCAP                         *CAP
043800970917     C                   KFLD                    KCSLOC                         *LOCALITA'
043900970611     C***
044000970611     C* IMPOSTAZIONI VIDEO
044100970611     C***
044200970611IF  1C     D00OP0        IFEQ      'L02'
044300970611     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
044400970611X   1C                   ELSE
044500970611     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
044600970611E   1C                   ENDIF
044700970611     C*
044800970611     C* ESEGUE OPERAZIONI INIZIALI STANDARD
044900970611     C                   EXSR      INZSTD
045000970611     C*
045100970611     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
045200970611     C                   EXSR      IMPVID
045300970611     C*
045400970611     C* EMETTO PRIMO FORMATO VIDEO
045500970611     C                   EXSR      CARV1
045600970611     C*
045700970611     C                   ENDSR
045800970611     C*--------------------------------------------------------------------------------------------*
045900970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
046000970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
046100970611     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
046200970611     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
046300970611     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
046400970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
046500970611     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
046600970611     C*--------------------------------------------------------------------------------------------*
046700970611     C* INZSTD - ROUTINE INIZIALE STANDARD
046800970611     C*--------------------------------------------------------------------------------------------*
046900970611     C     INZSTD        BEGSR
047000970611     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
047100970611     C                   CALL      'XKEYS'
047200970611     C                   PARM                    DSKEYS
047300970611     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
047400970611     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
047500970611     C     XMSGPA        PLIST
047600970611     C                   PARM                    DSMSMS
047700970611     C                   PARM                    DSMSPG
047800970611     C                   PARM                    DSMSKE
047900970611     C                   PARM                    DSMSER
048000970611     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
048100970611     C                   MOVEL     'TISI32R'     DSMSPG                         *NOME PGM CHIAMANTE
048200970611     C                   MOVEL     'TISI32R'     VMCQUE                         *CODA MESSAGGI DESTI
048300970611     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
048400970611     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
048500970611     C* INIZIALIZZO FLAG CONTROLLO PGM
048600970611     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
048700970611     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
048800970611     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
048900970611     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
049000970611     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
049100970611     C                   MOVEL     '0'           $FINE             1            *FINE PGM
049200970611     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
049300970611     C* LANCIO ROUTINE PER VERIFICARE SE
049400970611     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
049500970611     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
049600020919     C                   EXSR      REPDATIUTE
049700970611     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
049800970611     C                   BITOFF    '01234567'    POSMI1            1
049900970611     C                   BITOFF    '01234567'    POSMI2            1
050000970611     C                   BITOFF    '01234567'    POSMA1            1
050100970611     C                   BITOFF    '01234567'    POSMA2            1
050200970611     C                   BITON     '34'          POSMI1            1            *18
050300970611     C                   BITON     '7'           POSMI2            1            *01
050400970611     C                   BITON     '34'          POSMA1            1            *18
050500970611     C                   BITON     '0'           POSMA2            1            *80
050600970611     C                   MOVE      POSMI2        POSMIN            2
050700970611     C                   MOVEL     POSMI1        POSMIN
050800970611     C                   MOVE      POSMA2        POSMAX            2
050900970611     C                   MOVEL     POSMA1        POSMAX
051000970611     C                   ENDSR
051100970611     C*--------------------------------------------------------------------------------------------*
051200970611     C* FUNSTD - LANCIA SUBROUTINE STANDARD
051300970611     C*--------------------------------------------------------------------------------------------*
051400970611     C     FUNSTD        BEGSR
051500970611     C                   SELECT
051600970611     C     KEY           WHENEQ    F3
051700970611     C                   EXSR      F03STD
051800970611     C     KEY           WHENEQ    F12
051900970611     C                   EXSR      F12STD
052000970611     C     KEY           WHENEQ    F24
052100970611     C                   EXSR      F24STD
052200970611     C                   OTHER
052300970611     C                   MOVEL     MSG(2)        $MSG
052400970611     C                   EXSR      CARMSG
052500970611     C                   ENDSL
052600970611     C                   ENDSR
052700970611     C*--------------------------------------------------------------------------------------------*
052800970611     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
052900970611     C*--------------------------------------------------------------------------------------------*
053000970611     C     F03STD        BEGSR
053100970611     C* IMPOSTA FLAG DI FINE PROGRAMMA
053200970611     C                   MOVEL     '1'           $FINE
053300970611     C* IMPOSTA FLAG DI FINE PROCEDURA
053400970611     C                   MOVEL     '1'           $F03
053500970611     C* PULISCE IL MESSAGGIO
053600970611     C                   MOVEL     *BLANKS       D00MSG
053700970611     C                   ENDSR
053800970611     C*--------------------------------------------------------------------------------------------*
053900970611     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
054000970611     C*--------------------------------------------------------------------------------------------*
054100970611     C     F12STD        BEGSR
054200970611     C* IMPOSTA FLAG DI FINE PROGRAMMA
054300970611     C                   MOVEL     '1'           $FINE
054400970611     C* IMPOSTA FLAG DI FINE PROCEDURA
054500970611     C                   MOVEL     '1'           $F12
054600970611     C* PULISCE IL MESSAGGIO
054700970611     C                   MOVEL     *BLANKS       D00MSG
054800970611     C                   ENDSR
054900970611     C*--------------------------------------------------------------------------------------------*
055000970611     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
055100970611     C*--------------------------------------------------------------------------------------------*
055200970611     C     F24STD        BEGSR
055300970611     C*
055400970611     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
055500970611     C                   EXSR      OUTFUN
055600970611     C*
055700970611     C                   ENDSR
055800970611     C*--------------------------------------------------------------------------------------------*
055900970611     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
056000970611     C*          CODA MESSAGGI DEL PGM
056100970611     C*--------------------------------------------------------------------------------------------*
056200970611     C     CARMSG        BEGSR
056300970611     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
056400970611     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
056500970611     C     MSGULT        IFEQ      0
056600970611     C                   MOVEL     '1'           *IN40
056700970611     C                   WRITE     SI32DVK
056800970611     C                   MOVEL     '0'           *IN40
056900970611     C                   ENDIF
057000970611     C* INVIA MESSAGGIO ALLA CODA DEL PGM
057100970611     C                   MOVEL     $MSG          DSMSMS
057200970611     C                   CALL      'XMSG'        XMSGPA
057300970611     C     DSMSER        IFEQ      0
057400970611     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
057500970611     C                   ENDIF
057600970611     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
057700970611     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
057800970611     C                   WRITE     SI32DVM                                      *SFL MESS
057900970611     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
058000970611     C                   MOVEL     *BLANKS       $MSG
058100970611     C                   ENDSR
058200970611     C*--------------------------------------------------------------------------------------------*
058300970611     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
058400970611     C*--------------------------------------------------------------------------------------------*
058500970611     C     OUTMSG        BEGSR
058600970611     C* VISUALIZZAZIONE MESSAGGIO
058700970611     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
058800970611     C                   WRITE     SI32DVK                                      *CONTR MESS
058900970611     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
059000970611     C                   Z-ADD     *ZEROS        MSGULT
059100970611     C                   ENDSR
059200970611     C*--------------------------------------------------------------------------------------------*
059300970611     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
059400970611     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
059500970611     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
059600970611     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
059700970611     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
059800970611     C*--------------------------------------------------------------------------------------------*
059900970611     C     CARFUN        BEGSR
060000970611     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
060100970611     C     1             DO        31            I                 5 0          --- 1 -->
060200970611     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
060300970611     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
060400970611     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
060500020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
060600970611     C     $TGES         IFEQ      'G'                                          *GESTIONE
060700970611     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
060800970611     C                   ELSE                                                   *INTERROG.
060900970611     C                   MOVEL     FUNASI        FAT(I)
061000970611     C                   ENDIF
061100970611     C                   ELSE                                                   <-- 2 --> *FILIALE
061200970611     C     $TGES         IFEQ      'G'                                          *GESTIONE
061300970611     C                   MOVEL     FUNAFG        FAT(I)
061400970611     C                   ELSE                                                   *INTERROG.
061500970611     C                   MOVEL     FUNAFI        FAT(I)
061600970611     C                   ENDIF
061700970611     C                   ENDIF                                                  <-- 2 ---
061800970611     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
061900970611     C                   MOVEL     USER          WEDP              3            *PROFILO
062000970611     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
062100970611     C                   MOVEL     'S'           FAT(I)                         *ABILITO
062200970611     C                   ELSE
062300970611     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
062400970611     C                   ENDIF
062500970611     C                   ENDIF
062600970611     C                   ENDDO                                                  <-- 1 ---
062700970611     C                   ENDSR
062800970611     C*--------------------------------------------------------------------------------------------*
062900970611     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
063000970611     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
063100970611     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
063200970611     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
063300970611     C*--------------------------------------------------------------------------------------------*
063400970611     C     OUTFUN        BEGSR
063500970611     C     1             DO        8             I                              *PULIZIA FUNZIONI A
063600970611     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
063700970611     C                   ENDDO
063800970611     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
063900970611     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
064000970611     C     1             DO        31            I
064100970611     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
064200970611     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
064300970611     C                   ADD       1             FUNNAT
064400970611     C                   ENDIF
064500970611     C                   ENDDO
064600970611     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
064700970611     C     FUNNAT        IFGT      8
064800970611     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
064900970611     C                   ENDIF
065000970611     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
065100970611     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
065200970611     C                   Z-ADD     8             FUNVIS            5 0
065300970611     C     FAT(23)       IFEQ      'S'
065400970611     C                   SUB       1             FUNVIS
065500970611     C                   ENDIF
065600970611     C     FAT(24)       IFEQ      'S'
065700970611     C                   SUB       1             FUNVIS
065800970611     C                   ENDIF
065900970611     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
066000970611     C                   Z-ADD     *ZEROS        W                 5 0
066100970611     C                   ADD       1             FUNULT
066200970611     C     FUNULT        IFGT      31
066300970611     C                   Z-ADD     1             FUNULT
066400970611     C                   ENDIF
066500970611     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
066600970611     C     FUNULT        DO        31            I                              --- 1 -->
066700970611     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
066800970611     C     W             IFLT      FUNVIS                                       --- 2 -->
066900970611     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
067000970611     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
067100970611     C     FDE(I)        ANDNE     *BLANKS
067200970611     C     I             IFEQ      23                                           *TOLGO F23
067300970611     C     I             OREQ      24                                           *E F24
067400970611     C                   ELSE
067500970611     C                   ADD       1             W
067600970611     C                   MOVEL     FDE(I)        FUV(W)
067700970611     C                   ENDIF
067800970611     C                   ENDIF                                                  <-- 3 ---
067900970611     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
068000970611     C                   Z-ADD     I             FUNULT
068100970611     C                   ENDIF                                                  <-- 2 ---
068200970611     C                   ENDDO                                                  <-- 1 ---
068300970611     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
068400970611     C                   MOVEL     FUV(1)        VFD001
068500970611     C                   MOVEL     FUV(2)        VFD002
068600970611     C                   MOVEL     FUV(3)        VFD003
068700970611     C                   MOVEL     FUV(4)        VFD004
068800970611     C                   MOVEL     FUV(5)        VFD005
068900970611     C                   MOVEL     FUV(6)        VFD006
069000970611     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
069100970611     C     FAT(24)       ANDEQ     'N'
069200970611     C                   MOVEL     FUV(7)        VFD007
069300970611     C                   MOVEL     FUV(8)        VFD008
069400970611     C                   ELSE                                                   <-- 1 -->
069500970611     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
069600970611     C     FAT(24)       ANDEQ     'S'
069700970611     C                   MOVEL     FDE(23)       VFD007
069800970611     C                   MOVEL     FDE(24)       VFD008
069900970611     C                   ELSE                                                   <-- 2 -->
070000970611     C                   MOVEL     FUV(7)        VFD007
070100970611     C     FAT(23)       IFEQ      'S'
070200970611     C                   MOVEL     FDE(23)       VFD008
070300970611     C                   ELSE
070400970611     C                   MOVEL     FDE(24)       VFD008
070500970611     C                   ENDIF
070600970611     C                   ENDIF                                                  <-- 2 ---
070700970611     C                   ENDIF                                                  <-- 1 ---
070800970611     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
070900970611     C                   ENDSR
071000970611     C*--------------------------------------------------------------------------------------------*
071100970611     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
071200970611     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
071300970611     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
071400970611     C*                   - ERRORE SE FUNZIONE NON ATTIVA
071500970611     C*--------------------------------------------------------------------------------------------*
071600970611     C     CONFUN        BEGSR
071700970611     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
071800970611     C     1             DO        31            I                              --- 1 -->
071900970611     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
072000970611     C* CONTROLLO SE IL TASTO E' ATTIVO
072100970611     C     FAT(I)        IFNE      'S'                                          --- 3 -->
072200970611     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
072300970611     C                   EXSR      CARMSG
072400970611     C                   MOVEL     '1'           $ERR
072500970611     C                   ENDIF                                                  <-- 3 ---
072600970611     C                   ENDIF                                                  <-- 2 ---
072700970611     C                   ENDDO                                                  <-- 1 ---
072800970611     C                   ENDSR
072900970611     O*--------------------------------------------------------------------------------------------*
073000970611** -MSG- (MESSAGGI)
073100970611TISI32R- Tasto funzionale non valido.                                          1
073200970611TISI32R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
073300970611TISI32R- Opzione non valida.                                                   3
073400970611TISI32R- Opzione abilitata ma non gestita. Avvertire CED.                      4
073500970611TISI32R- Inizio elenco.                                                        5
073600970611TISI32R- Fine elenco.                                                          6
073700970917TISI32R- Impossibile inserire località di CAP obsoleti                         7
073800970919TISI32R- Cap inesistente o non valido                                          8
073900970611TISI32R- Immettere la località                                                 9
074000970611TISI32R- Località già inserita                                                10
074100970611TISI32R- 11
074200970611TISI32R- 12
074300970611TISI32R- 13
074400970611TISI32R- 14
074500970611TISI32R- 15
074600970611TISI32R- 16
074700970611TISI32R- 17
074800970611TISI32R- 18
074900970917TISI32R- Errori in entrata duplica. Non trovato il record da cui copiare      19
075000970917TISI32R- La località e gli archivi correlati sono stati duplicati             20
075100970611** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
075200970611F1    -                   -    NN     NN
075300970611F2    -                   -    NN     NN
075400970611F3    -F3=Fine            -    NN     NN
075500970611F4    -                   -    NN     NN
075600970611F5    -                   -    NN     NN
075700970611F6    -F6=Conferma        -    SN     SN
075800970611F7    -                   -    NN     NN
075900970611F8    -                   -    NN     NN
076000970611F9    -                   -    NN     NN
076100970611F10   -                   -    NN     NN
076200970611F11   -                   -    NN     NN
076300970611F12   -F12=Ritorno        -    SS     SS
076400970611F13   -                   -    NN     NN
076500970611F14   -                   -    NN     NN
076600970611F15   -                   -    NN     NN
076700970611F16   -                   -    NN     NN
076800970611F17   -                   -    NN     NN
076900970611F18   -                   -    NN     NN
077000970611F19   -                   -    NN     NN
077100970611F20   -                   -    NN     NN
077200970611F21   -                   -    NN     NN
077300970611F22   -                   -    NN     NN
077400970611F23   -                   -    NN     NN
077500970611F24   -F24=Altri tasti    -    NN     NN
077600970611ENTER -                   -    SS     SS
077700970611ROLLUP-                   -    NN     NN
077800970611ROLLDN-                   -    NN     NN
077900970611HELP  -                   -    NN     NN
078000970611PRINT -                   -    NN     NN
078100970611BSPACE-                   -    NN     NN
078200970611CLEAR -                   -    NN     NN

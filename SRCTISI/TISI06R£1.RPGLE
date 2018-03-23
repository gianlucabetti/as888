000100970917     H*--------------------------------------------------------------------------------------------*
000200000420     H* GESTIONE CAP - DUPLICA CAP + ESTENSIONE X POSTE
000300970917     H*--------------------------------------------------------------------------------------------*
000400970917     H DECEDIT('0,') DATEDIT(*DMY.)
000500970917     F*--------------------------------------------------------------------------------------------*
000600970917     F* DATA BASE
000700970917     F*--------------------------------------------------------------------------------------------*
000800970917     FTISI06D   CF   E             WORKSTN INFDS(DEVDS1)
000900970917     F                                     SFILE(SI06DVM:MSGULT)
001000970917     FAZCPC01L  UF A E           K DISK
001100000420     FAZPPC01L  UF A E           K DISK
001200970917     FTABEL00F  IF   E           K DISK
001300970917     D*--------------------------------------------------------------------------------------------*
001400970917     D* SCHIERE
001500970917     D*--------------------------------------------------------------------------------------------*
001600970917     D*-------------------
001700970917     D* MESSAGGI E FUNZIONI
001800970917     D*-------------------
001900970917     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002000970917     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002100970917     D*-------------------
002200970917     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002300970917     D*-------------------
002400970917     D FFU             S              6    DIM(31)                              *FUNZIONE
002500970917     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002600970917     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002700970917     D*-------------------
002800970917     D* TABELLA DESCRIZIONI A VIDEO
002900970917     D*-------------------
003000970917     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003100970917     D*-------------------
003200970917     D* TABELLA TASTI FUNZIONALI PREMUTI
003300970917     D*-------------------
003400970917     D KEYS            S              1    DIM(40)
003500970917     D*--------------------------------------------------------------------------------------------*
003600970917     D* INPUT
003700970917     D*--------------------------------------------------------------------------------------------*
003800970917     D*-------------------
003900970917     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004000970917     D*-------------------
004100970917     D DSFUN         E DS
004200970917     D  FUNRIG                 1     40
004300970917     D*-------------------
004400970917     D* DS PER GESTIONE TASTI FUNZIONALI
004500970917     D*-------------------
004600970917     D DSKEYS        E DS
004700970917     D  KEYRIG                 1     40
004800970917     D*-------------------
004900970917     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005000970917     D*-------------------
005100970917     D                SDS
005200970917     D  USER                 254    263
005300970917     D*-------------------
005400970917     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005500970917     D*-------------------
005600970917     D DEVDS1          DS
005700970917     D  KEY                  369    369
005800970917     D  CURSOR               370    371
005900970917     D*---------------------
006000970917     D* ARCHITETTURA
006100970917     D*---------------------
006200970917     D KPJBA         E DS
006300970917     D*---------------------
006400970917     D* DS ESTERNA DI PROCEDURA
006500970917     D*---------------------
006600970917     D DSSI00        E DS                  EXTNAME(TISI00DS)
006700970917     D*-------------------
006800020916     D* DS REPERIMENTO DATI UTENTE
006900020916     D*-------------------
007000020916     D TIBS34DS      E DS                                                       *Profili utente
007100020916     D DDATIUTE      E DS                                                       *Dati utente
007200020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007300970917     D*---------------------
007400970917     D* DS ESTERNA DI PROCEDURA
007500970917     D*---------------------
007600970917     D DSSI95        E DS                  EXTNAME(TISI95DS)
007700970917     D*-------------------
007800970917     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007900970917     D*-------------------
008000970917     D DSMSG         E DS
008100970917     D*------------------
008200970917     D* DS "XSRDA8" - CONTROLLA DATA (8)
008300970917     D*------------------
008400970917     D WLBDA8          DS
008500970917     D  G08DAT                 1      8  0
008600970917     D  G08INV                 9     16  0
008700970917     D  G08ERR                17     17
008800970917     D  G08TGI                18     22  0
008900000605     D*-------------------
009000000605     D* DS DI SALVATAGGIO FORMATI RECORDS
009100000605     D*-------------------
009200000605     D DSPPC         E DS                  EXTNAME(AZPPC00F)
009300970917     C*--------------------------------------------------------------------------------------------*
009400970917     C* MAIN LINE
009500970917     C*--------------------------------------------------------------------------------------------*
009600970917     C*
009700970917     C* SE NON CI SONO ERRORI IN ENTRATA
009800970917IF  1C     $FINE         IFEQ      '0'                                          *NO ERRORI
009900970917     C                   WRITE     SI06DVT                                      *TESTATA
010000970917DO  2C     $FINE         DOUEQ     '1'
010100970917     C*
010200970917     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010300970917IF  3C     $GES          IFEQ      'V1'
010400970917     C                   EXSR      GESV1
010500970917E   3C                   ENDIF
010600970917     C*
010700970917E   2C                   ENDDO
010800970917     C*
010900970917IF  2C     $F03          IFEQ      '1'
011000970917     C                   MOVEL     '1'           D00F03
011100970917E   2C                   ENDIF
011200970917E   1C                   ENDIF
011300970917     C*
011400970917     C* OPERAZIONI FINALI
011500970917     C                   EXSR      FINSR
011600970917     C* FINE PGM
011700970917     C                   SETON                                        LR
011800970917     C*--------------------------------------------------------------------------------------------*
011900970917     C* GESV1 - GESTIONE VIDEO UNO
012000970917     C*--------------------------------------------------------------------------------------------*
012100970917     C     GESV1         BEGSR
012200970917     C*
012300970917IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
012400970917     C                   EXSR      CARV1                                        *CARICA VIDEO
012500970917E   1C                   ENDIF
012600970917     C*
012700970917IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012800970917     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012900970917     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
013000970917E   1C                   ENDIF
013100970917     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
013200970917     C*
013300970917     C                   EXFMT     SI06DV1                                      *PARAMETRI
013400970917     C*
013500970917     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013600970917     C*
013700970917IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013800970917SL  2C                   SELECT
013900970917     C     KEY           WHENEQ    ENTER
014000970917     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
014100970917     C     KEY           WHENEQ    F6
014200970917     C                   EXSR      F06GES
014300970917     C                   OTHER
014400970917     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014500970917E   2C                   ENDSL
014600970917E   1C                   ENDIF
014700970917     C*
014800970917     C                   ENDSR
014900970917     C*--------------------------------------------------------------------------------------------*
015000970917     C* CARV1 - CARICA VIDEO UNO
015100970917     C*--------------------------------------------------------------------------------------------*
015200970917     C     CARV1         BEGSR
015300970917     C*
015400970917     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015500970917     C*
015600970917     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015700970917     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015800970917     C*
015900970917     C                   ENDSR
016000970917     C*--------------------------------------------------------------------------------------------*
016100970917     C* ENTV1 - GESTIONE TASTO ENTER
016200970917     C*--------------------------------------------------------------------------------------------*
016300970917     C     ENTV1         BEGSR
016400970917     C*
016500970917     C* CONTROLLA I CAMPI IMMESSI
016600970917     C                   EXSR      DECODI
016700970917     C*
016800970917     C                   ENDSR
016900970917     C*--------------------------------------------------------------------------------------------*
017000970917     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
017100970917     C*--------------------------------------------------------------------------------------------*
017200970917     C     F06GES        BEGSR
017300970917     C*
017400970917     C* CONTROLLA I CAMPI IMMESSI
017500970917     C                   EXSR      DECODI
017600970917     C*
017700970917     C* RILEGGE IL CAP DA CUI COPIARE
017800970917IF  1C     $ERR          IFEQ      '0'
017900970917     C                   Z-ADD     D00VER        KCCVER
018000970917     C                   MOVEL     V1CNAR        KCCNAR
018100970917     C                   MOVEL     V1CCAP        KCCCAP
018200970917     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
018300970917IF  2C     *IN99         IFEQ      *OFF
018400970917     C*
018500970917     C* IMPOSTA IL RECORD DA SCRIVERE
018600970917     C                   Z-ADD     D00VER        CPCVER                         *VERSIONE
018700970917     C                   MOVEL     V1CNA2        CPCNAR                         *NAZIONE
018800970917     C                   MOVEL     V1CCA2        CPCCAP                         *CAP
018900970917     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
019000970917     C                   Z-ADD     *ZEROS        CPCDTR                         *DATA TRASMISSIONE
019100970917     C* SCRIVE
019200970917     C                   WRITE     AZCPC000
019300000420     C*
019400021106     C*--- SOLO SE TRATTASI DI CAP ITALIA
019500000420     C*--- IMPOSTO IL RECORD DI ESTENSIONE POSTE DA SCRIVERE
019600021106     C                   IF        CPCNAR = *BLANKS
019700000420     C                   Z-ADD     D00VER        KCCVER
019800000605     C                   MOVEL     V1CNAR        KCCNAR
019900000605     C                   MOVEL     V1CCAP        KCCCAP
020000000420     C     KEYCPC        CHAIN(N)  AZPPC01L                           99
020100000605     C                   MOVEL     DSPPC         SAVPPC
020200000605     C* LEGGO IL FUTURO RECORD "DUPLICATO" C DECISIONE WRITE O UPDATE
020300000605     C                   Z-ADD     D00VER        KCCVER                         *VERSIONE
020400000605     C                   MOVEL     V1CNA2        KCCNAR                         *NAZIONE
020500000605     C                   MOVEL     V1CCA2        KCCCAP                         *CAP
020600000605     C     KEYCPC        CHAIN     AZPPC01L                           99
020700000605     C                   MOVEL     SAVPPC        DSPPC
020800000420     C*
020900000420     C* IMPOSTA IL RECORD DA SCRIVERE
021000000420     C                   Z-ADD     D00VER        PPCVER                         *VERSIONE
021100000420     C                   MOVEL     V1CNA2        PPCNAR                         *NAZIONE
021200000420     C                   MOVEL     V1CCA2        PPCCAP                         *CAP
021300000420     C                   MOVEL     *BLANKS       PPCATB                         *FLAG ANNULLAMENTO
021400000420     C                   MOVEL     *BLANKS       PPCFTR                         *FLAG TRASMISSIONE
021500000420     C                   Z-ADD     *ZEROS        PPCDTR                         *DATA TRASMISSIONE
021600000426     C* SCRIVE O AGGIORNA IN QUANTO L'ESTENSIONE X POSTE VIENE "DI CONSEGUENZA" AL CAPPARIO BART.
021700000426     C   99              WRITE     AZPPC000
021800000426     C  N99              UPDATE    AZPPC000
021900021106     C                   ENDIF
022000000420     C*--- END POSTE
022100970917     C*
022200970917     C* IMPOSTA FLAG DI FINE PROGRAMMA
022300970917     C                   MOVEL     MSG(20)       D00MSG
022400970917     C                   MOVEL     '1'           $FINE
022500000427E   2C                   ENDIF
022600970917E   1C                   ENDIF
022700970917     C*
022800970917     C                   ENDSR
022900970917     C*--------------------------------------------------------------------------------------------*
023000970917     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023100970917     C*--------------------------------------------------------------------------------------------*
023200970917     C     DECODI        BEGSR
023300970917     C*
023400970917     C* REIMPOSTA INDICATORI DI ERRORE
023500970917     C                   SETOFF                                       6061
023600970917     C*--------------------
023700970917     C* NAZIONE
023800970917     C*--------------------
023900970917IF  1C     $ERR          IFEQ      '0'
024000970917     C                   MOVEL     V1CNA2        WNAR
024100970917     C                   EXSR      CHKNAR
024200970917IF  2C     $ERR          IFEQ      '1'
024300970917     C                   MOVEL     MSG(10)       $MSG
024400970917     C                   EXSR      CARMSG
024500970917     C                   SETON                                        60
024600970917E   2C                   ENDIF
024700970917E   1C                   ENDIF
024800970917     C*--------------------
024900970917     C* CAP
025000970917     C*--------------------
025100970917IF  1C     $ERR          IFEQ      '0'
025200970917     C                   MOVEL     V1CNA2        WNAR
025300970917     C                   MOVEL     V1CCA2        WCAP
025400970917     C                   EXSR      CHKCAP
025500970917IF  2C     $ERR          IFEQ      '1'
025600970917     C                   SETON                                        61
025700970917     C                   EXSR      CARMSG
025800970917E   2C                   ENDIF
025900970917E   1C                   ENDIF
026000970917     C*
026100970917     C* NUOVO CAP INESISTENTE
026200970917IF  1C     $ERR          IFEQ      '0'
026300970917     C                   Z-ADD     D00VER        KCCVER
026400970917     C                   MOVEL     V1CNA2        KCCNAR
026500970917     C                   MOVEL     V1CCA2        KCCCAP
026600970917     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
026700970917IF  2C     *IN99         IFEQ      *OFF
026800970917     C                   MOVEL     '1'           $ERR
026900970917     C                   MOVEL     MSG(11)       $MSG
027000970917     C                   EXSR      CARMSG
027100970917     C                   SETON                                        6061
027200970917E   2C                   ENDIF
027300970917E   1C                   ENDIF
027400970917     C*
027500970917     C                   ENDSR
027600970917     C*------------------------------------------------------------------------*
027700970917     C* CHKNAR - CONTROLLA LA NAZIONE
027800970917     C*------------------------------------------------------------------------*
027900970917     C     CHKNAR        BEGSR
028000970917     C*
028100970917     C     *LIKE         DEFINE    V1CNAR        WNAR
028200970917     C*
028300970917     C* CONTROLLA ESISTENZA CODICE
028400020916     C                   MOVEL     1             KTBKUT
028500970917     C                   MOVEL     '15'          KTBCOD
028600970917     C                   MOVEL(P)  WNAR          KTBKEY
028700970917     C     KEYTAB        CHAIN     TABEL00F                           99
028800970917IF  1C     *IN99         IFEQ      *ON                                          *INESISTENTE
028900970917     C     TBLFLG        ORNE      *BLANKS                                      *ANNULLATO
029000970917     C                   MOVEL     '1'           $ERR
029100970917E   1C                   ENDIF
029200970917     C*
029300970917     C                   ENDSR
029400970917     C*------------------------------------------------------------------------*
029500970917     C* CHKCAP - CONTROLLA IL CAP
029600970917     C*------------------------------------------------------------------------*
029700970917     C     CHKCAP        BEGSR
029800970917     C*
029900970917     C     *LIKE         DEFINE    V1CCAP        WCAP
030000970917     C*
030100970917     C* CONTROLLA IL CAP
030200970917     C                   CLEAR                   DSSI95
030300970917     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
030400970917     C                   MOVEL     '1'           I95TCN                         *CONTROLLO FORMALE
030500970917     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
030600970917     C                   MOVEL     WCAP          I95CAP                         *CAP
030700970917     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
030800970917     C                   CALL      'TISI95R'
030900970917     C                   PARM                    DSSI95
031000970917IF  1C     O95ERR        IFNE      *BLANKS
031100970917     C                   MOVEL     '1'           $ERR
031200970917     C                   MOVEL     O95MSG        $MSG
031300970917E   1C                   ENDIF
031400970917     C*
031500970917     C                   ENDSR
031600970917     C*--------------------------------------------------------------------------------------------*
031700970917     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
031800970917     C*--------------------------------------------------------------------------------------------*
031900970917     C     IMPVID        BEGSR
032000970917     C*
032100970917     C* CALCOLA LA DATA CORRENTE
032200970917     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
032300970917     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
032400970917     C                   Z-ADD     WNUM8         G08DAT
032500970917     C                   Z-ADD     *ZEROS        G08INV
032600970917     C                   MOVEL     '0'           G08ERR
032700970917     C                   CALL      'XSRDA8'
032800970917     C                   PARM                    WLBDA8
032900970917     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
033000970917     C*
033100970917     C* LEGGE IL CAP
033200970917     C                   Z-ADD     D00VER        KCCVER
033300970917     C                   MOVEL     D00NAR        KCCNAR
033400970917     C                   MOVEL     D00CAP        KCCCAP
033500970917     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
033600970917IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
033700970917     C* DATI OLD
033800970917     C                   MOVEL     CPCNAR        V1CNAR                         *NAZIONE
033900970917     C                   MOVEL     CPCCAP        V1CCAP                         *CAP
034000970917     C* DATI NEW
034100970917     C                   MOVEL     V1CNAR        V1CNA2                         *NAZIONE
034200970917     C                   MOVEL     V1CCAP        V1CCA2                         *CAP
034300970917X   1C                   ELSE                                                   *NON TROVATO
034400970917     C                   MOVEL     '1'           $FINE                          *FINE PGM
034500970917     C                   MOVEL     '1'           D00ERR                         *ERRORE
034600970917     C                   MOVEL     MSG(19)       D00MSG                         *MESSSAGGIO
034700970917E   1C                   ENDIF
034800970917     C*
034900970917     C                   ENDSR
035000970917     C*------------------------------------------------------------------------*
035100970917     C* FINSR - OPERAZIONI FINALI
035200970917     C*------------------------------------------------------------------------*
035300970917     C     FINSR         BEGSR
035400970917     C*
035500970917     C                   CLEAR                   DSSI95
035600970917     C                   MOVEL     'C'           I95TLA
035700970917     C                   CALL      'TISI95R'
035800970917     C                   PARM                    DSSI95
035900970917     C*
036000970917     C                   ENDSR
036100970917     C*--------------------------------------------------------------------------------------------*
036200020916     C* REPERISCE I DATI UTENTE
036300020916     C*--------------------------------------------------------------------------------------------*
036400020916     C     REPDATIUTE    BEGSR
036500020916     C*
036600020916     C* INIZIALIZZA VARIABILI DI WRK
036700020916     C                   CLEAR                   TIBS34DS
036800020916     C                   CLEAR                   AZUTEDS
036900020916     C                   CLEAR                   DDATIUTE
037000020916     C*
037100020916     C     *DTAARA       DEFINE    §azute        azuteds
037200020916     C     *DTAARA       DEFINE    §datiute      ddatiute
037300020916     C                   IN(E)     *DTAARA
037400020916if  1C                   IF        %Error
037500020916     c                   EVAL      I34Tla = 'L'
037600020916     C                   CALL      'TIBS34R'
037700020916     C                   PARM                    Tibs34Ds
037800020916     C                   IN        *DTAARA
037900020916e   1C                   ENDIF
038000020916     C*
038100020916     C                   ENDSR
038200020916     C*--------------------------------------------------------------------------------------------*
038300970917     C* *INZSR  - ROUTINE INIZIALE
038400970917     C*--------------------------------------------------------------------------------------------*
038500970917     C     *INZSR        BEGSR
038600970917     C*--------------------
038700970917     C* RICEVIMENTO PARAMETRI
038800970917     C*--------------------
038900970917     C     *ENTRY        PLIST
039000970917     C                   PARM                    KPJBA
039100970917     C                   PARM                    DSSI00
039200970917     C*--------------------
039300970917     C* VARIABILI RIFERITE AL DATA BASE
039400970917     C*--------------------
039500970917     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
039600970917     C     *LIKE         DEFINE    TBLCOD        KTBCOD
039700970917     C     *LIKE         DEFINE    TBLKEY        KTBKEY
039800970917     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
039900970917     C     *LIKE         DEFINE    CPCNAR        KCCNAR
040000970917     C     *LIKE         DEFINE    CPCCAP        KCCCAP
040100000605     C*
040200000605     C* IMPOSTO CAMPO DI SALVATAGGIO DS
040300000605     C     *LIKE         DEFINE    DSPPC         SAVPPC
040400970917     C*--------------------
040500970917     C* CHIAVI DI LETTURA
040600970917     C*--------------------
040700970917     C* LETTURA TABEL00F - COMPLETA
040800970917     C     KEYTAB        KLIST
040900970917     C                   KFLD                    KTBKUT                         *CODICE UTENTE
041000970917     C                   KFLD                    KTBCOD                         *TABELLA
041100970917     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
041200970917     C* LETTURA AZCPC01L - COMPLETA
041300970917     C     KEYCPC        KLIST
041400970917     C                   KFLD                    KCCVER                         *VERSIONE
041500970917     C                   KFLD                    KCCNAR                         *NAZIONE
041600970917     C                   KFLD                    KCCCAP                         *CAP
041700970917     C***
041800970917     C* IMPOSTAZIONI VIDEO
041900970917     C***
042000970917IF  1C     D00OP0        IFEQ      'C02'
042100970917     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
042200970917X   1C                   ELSE
042300970917     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
042400970917E   1C                   ENDIF
042500970917     C*
042600970917     C* ESEGUE OPERAZIONI INIZIALI STANDARD
042700970917     C                   EXSR      INZSTD
042800970917     C*
042900970917     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
043000970917     C                   EXSR      IMPVID
043100970917     C*
043200970917     C* EMETTO PRIMO FORMATO VIDEO
043300970917     C                   EXSR      CARV1
043400970917     C*
043500970917     C                   ENDSR
043600970917     C*--------------------------------------------------------------------------------------------*
043700970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043800970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043900970917     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
044000970917     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
044100970917     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
044200970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
044300970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
044400970917     C*--------------------------------------------------------------------------------------------*
044500970917     C* INZSTD - ROUTINE INIZIALE STANDARD
044600970917     C*--------------------------------------------------------------------------------------------*
044700970917     C     INZSTD        BEGSR
044800970917     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
044900970917     C                   CALL      'XKEYS'
045000970917     C                   PARM                    DSKEYS
045100970917     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
045200970917     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
045300970917     C     XMSGPA        PLIST
045400970917     C                   PARM                    DSMSMS
045500970917     C                   PARM                    DSMSPG
045600970917     C                   PARM                    DSMSKE
045700970917     C                   PARM                    DSMSER
045800970917     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
045900970917     C                   MOVEL     'TISI06R'     DSMSPG                         *NOME PGM CHIAMANTE
046000970917     C                   MOVEL     'TISI06R'     VMCQUE                         *CODA MESSAGGI DESTI
046100970917     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
046200970917     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
046300970917     C* INIZIALIZZO FLAG CONTROLLO PGM
046400970917     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
046500970917     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
046600970917     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
046700970917     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
046800970917     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
046900970917     C                   MOVEL     '0'           $FINE             1            *FINE PGM
047000970917     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
047100970917     C* LANCIO ROUTINE PER VERIFICARE SE
047200970917     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
047300970917     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
047400020916     C                   EXSR      REPDATIUTE
047500970917     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
047600970917     C                   BITOFF    '01234567'    POSMI1            1
047700970917     C                   BITOFF    '01234567'    POSMI2            1
047800970917     C                   BITOFF    '01234567'    POSMA1            1
047900970917     C                   BITOFF    '01234567'    POSMA2            1
048000970917     C                   BITON     '34'          POSMI1            1            *18
048100970917     C                   BITON     '7'           POSMI2            1            *01
048200970917     C                   BITON     '34'          POSMA1            1            *18
048300970917     C                   BITON     '0'           POSMA2            1            *80
048400970917     C                   MOVE      POSMI2        POSMIN            2
048500970917     C                   MOVEL     POSMI1        POSMIN
048600970917     C                   MOVE      POSMA2        POSMAX            2
048700970917     C                   MOVEL     POSMA1        POSMAX
048800970917     C                   ENDSR
048900970917     C*--------------------------------------------------------------------------------------------*
049000970917     C* FUNSTD - LANCIA SUBROUTINE STANDARD
049100970917     C*--------------------------------------------------------------------------------------------*
049200970917     C     FUNSTD        BEGSR
049300970917     C                   SELECT
049400970917     C     KEY           WHENEQ    F3
049500970917     C                   EXSR      F03STD
049600970917     C     KEY           WHENEQ    F12
049700970917     C                   EXSR      F12STD
049800970917     C     KEY           WHENEQ    F24
049900970917     C                   EXSR      F24STD
050000970917     C                   OTHER
050100970917     C                   MOVEL     MSG(2)        $MSG
050200970917     C                   EXSR      CARMSG
050300970917     C                   ENDSL
050400970917     C                   ENDSR
050500970917     C*--------------------------------------------------------------------------------------------*
050600970917     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
050700970917     C*--------------------------------------------------------------------------------------------*
050800970917     C     F03STD        BEGSR
050900970917     C* IMPOSTA FLAG DI FINE PROGRAMMA
051000970917     C                   MOVEL     '1'           $FINE
051100970917     C* IMPOSTA FLAG DI FINE PROCEDURA
051200970917     C                   MOVEL     '1'           $F03
051300970917     C* PULISCE IL MESSAGGIO
051400970917     C                   MOVEL     *BLANKS       D00MSG
051500970917     C                   ENDSR
051600970917     C*--------------------------------------------------------------------------------------------*
051700970917     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
051800970917     C*--------------------------------------------------------------------------------------------*
051900970917     C     F12STD        BEGSR
052000970917     C* IMPOSTA FLAG DI FINE PROGRAMMA
052100970917     C                   MOVEL     '1'           $FINE
052200970917     C* IMPOSTA FLAG DI FINE PROCEDURA
052300970917     C                   MOVEL     '1'           $F12
052400970917     C* PULISCE IL MESSAGGIO
052500970917     C                   MOVEL     *BLANKS       D00MSG
052600970917     C                   ENDSR
052700970917     C*--------------------------------------------------------------------------------------------*
052800970917     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
052900970917     C*--------------------------------------------------------------------------------------------*
053000970917     C     F24STD        BEGSR
053100970917     C*
053200970917     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
053300970917     C                   EXSR      OUTFUN
053400970917     C*
053500970917     C                   ENDSR
053600970917     C*--------------------------------------------------------------------------------------------*
053700970917     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
053800970917     C*          CODA MESSAGGI DEL PGM
053900970917     C*--------------------------------------------------------------------------------------------*
054000970917     C     CARMSG        BEGSR
054100970917     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
054200970917     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
054300970917     C     MSGULT        IFEQ      0
054400970917     C                   MOVEL     '1'           *IN40
054500970917     C                   WRITE     SI06DVK
054600970917     C                   MOVEL     '0'           *IN40
054700970917     C                   ENDIF
054800970917     C* INVIA MESSAGGIO ALLA CODA DEL PGM
054900970917     C                   MOVEL     $MSG          DSMSMS
055000970917     C                   CALL      'XMSG'        XMSGPA
055100970917     C     DSMSER        IFEQ      0
055200970917     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
055300970917     C                   ENDIF
055400970917     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
055500970917     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
055600970917     C                   WRITE     SI06DVM                                      *SFL MESS
055700970917     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
055800970917     C                   MOVEL     *BLANKS       $MSG
055900970917     C                   ENDSR
056000970917     C*--------------------------------------------------------------------------------------------*
056100970917     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
056200970917     C*--------------------------------------------------------------------------------------------*
056300970917     C     OUTMSG        BEGSR
056400970917     C* VISUALIZZAZIONE MESSAGGIO
056500970917     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
056600970917     C                   WRITE     SI06DVK                                      *CONTR MESS
056700970917     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
056800970917     C                   Z-ADD     *ZEROS        MSGULT
056900970917     C                   ENDSR
057000970917     C*--------------------------------------------------------------------------------------------*
057100970917     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
057200970917     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
057300970917     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
057400970917     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
057500970917     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
057600970917     C*--------------------------------------------------------------------------------------------*
057700970917     C     CARFUN        BEGSR
057800970917     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
057900970917     C     1             DO        31            I                 5 0          --- 1 -->
058000970917     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
058100970917     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
058200970917     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
058300020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
058400970917     C     $TGES         IFEQ      'G'                                          *GESTIONE
058500970917     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
058600970917     C                   ELSE                                                   *INTERROG.
058700970917     C                   MOVEL     FUNASI        FAT(I)
058800970917     C                   ENDIF
058900970917     C                   ELSE                                                   <-- 2 --> *FILIALE
059000970917     C     $TGES         IFEQ      'G'                                          *GESTIONE
059100970917     C                   MOVEL     FUNAFG        FAT(I)
059200970917     C                   ELSE                                                   *INTERROG.
059300970917     C                   MOVEL     FUNAFI        FAT(I)
059400970917     C                   ENDIF
059500970917     C                   ENDIF                                                  <-- 2 ---
059600970917     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
059700970917     C                   MOVEL     USER          WEDP              3            *PROFILO
059800970917     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
059900970917     C                   MOVEL     'S'           FAT(I)                         *ABILITO
060000970917     C                   ELSE
060100970917     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
060200970917     C                   ENDIF
060300970917     C                   ENDIF
060400970917     C                   ENDDO                                                  <-- 1 ---
060500970917     C                   ENDSR
060600970917     C*--------------------------------------------------------------------------------------------*
060700970917     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
060800970917     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
060900970917     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
061000970917     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
061100970917     C*--------------------------------------------------------------------------------------------*
061200970917     C     OUTFUN        BEGSR
061300970917     C     1             DO        8             I                              *PULIZIA FUNZIONI A
061400970917     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
061500970917     C                   ENDDO
061600970917     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
061700970917     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
061800970917     C     1             DO        31            I
061900970917     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
062000970917     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
062100970917     C                   ADD       1             FUNNAT
062200970917     C                   ENDIF
062300970917     C                   ENDDO
062400970917     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
062500970917     C     FUNNAT        IFGT      8
062600970917     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
062700970917     C                   ENDIF
062800970917     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
062900970917     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
063000970917     C                   Z-ADD     8             FUNVIS            5 0
063100970917     C     FAT(23)       IFEQ      'S'
063200970917     C                   SUB       1             FUNVIS
063300970917     C                   ENDIF
063400970917     C     FAT(24)       IFEQ      'S'
063500970917     C                   SUB       1             FUNVIS
063600970917     C                   ENDIF
063700970917     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
063800970917     C                   Z-ADD     *ZEROS        W                 5 0
063900970917     C                   ADD       1             FUNULT
064000970917     C     FUNULT        IFGT      31
064100970917     C                   Z-ADD     1             FUNULT
064200970917     C                   ENDIF
064300970917     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
064400970917     C     FUNULT        DO        31            I                              --- 1 -->
064500970917     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
064600970917     C     W             IFLT      FUNVIS                                       --- 2 -->
064700970917     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
064800970917     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
064900970917     C     FDE(I)        ANDNE     *BLANKS
065000970917     C     I             IFEQ      23                                           *TOLGO F23
065100970917     C     I             OREQ      24                                           *E F24
065200970917     C                   ELSE
065300970917     C                   ADD       1             W
065400970917     C                   MOVEL     FDE(I)        FUV(W)
065500970917     C                   ENDIF
065600970917     C                   ENDIF                                                  <-- 3 ---
065700970917     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
065800970917     C                   Z-ADD     I             FUNULT
065900970917     C                   ENDIF                                                  <-- 2 ---
066000970917     C                   ENDDO                                                  <-- 1 ---
066100970917     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
066200970917     C                   MOVEL     FUV(1)        VFD001
066300970917     C                   MOVEL     FUV(2)        VFD002
066400970917     C                   MOVEL     FUV(3)        VFD003
066500970917     C                   MOVEL     FUV(4)        VFD004
066600970917     C                   MOVEL     FUV(5)        VFD005
066700970917     C                   MOVEL     FUV(6)        VFD006
066800970917     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
066900970917     C     FAT(24)       ANDEQ     'N'
067000970917     C                   MOVEL     FUV(7)        VFD007
067100970917     C                   MOVEL     FUV(8)        VFD008
067200970917     C                   ELSE                                                   <-- 1 -->
067300970917     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
067400970917     C     FAT(24)       ANDEQ     'S'
067500970917     C                   MOVEL     FDE(23)       VFD007
067600970917     C                   MOVEL     FDE(24)       VFD008
067700970917     C                   ELSE                                                   <-- 2 -->
067800970917     C                   MOVEL     FUV(7)        VFD007
067900970917     C     FAT(23)       IFEQ      'S'
068000970917     C                   MOVEL     FDE(23)       VFD008
068100970917     C                   ELSE
068200970917     C                   MOVEL     FDE(24)       VFD008
068300970917     C                   ENDIF
068400970917     C                   ENDIF                                                  <-- 2 ---
068500970917     C                   ENDIF                                                  <-- 1 ---
068600970917     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
068700970917     C                   ENDSR
068800970917     C*--------------------------------------------------------------------------------------------*
068900970917     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
069000970917     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
069100970917     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
069200970917     C*                   - ERRORE SE FUNZIONE NON ATTIVA
069300970917     C*--------------------------------------------------------------------------------------------*
069400970917     C     CONFUN        BEGSR
069500970917     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
069600970917     C     1             DO        31            I                              --- 1 -->
069700970917     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
069800970917     C* CONTROLLO SE IL TASTO E' ATTIVO
069900970917     C     FAT(I)        IFNE      'S'                                          --- 3 -->
070000970917     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
070100970917     C                   EXSR      CARMSG
070200970917     C                   MOVEL     '1'           $ERR
070300970917     C                   ENDIF                                                  <-- 3 ---
070400970917     C                   ENDIF                                                  <-- 2 ---
070500970917     C                   ENDDO                                                  <-- 1 ---
070600970917     C                   ENDSR
070700970917     O*--------------------------------------------------------------------------------------------*
070800970917** -MSG- (MESSAGGI)
070900970917TISI06R- Tasto funzionale non valido.                                          1
071000970917TISI06R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
071100970917TISI06R- Opzione non valida.                                                   3
071200970917TISI06R- Opzione abilitata ma non gestita. Avvertire CED.                      4
071300970917TISI06R- Inizio elenco.                                                        5
071400970917TISI06R- Fine elenco.                                                          6
071500970917TISI06R- 7
071600970917TISI06R- 8
071700970917TISI06R- 9
071800970917TISI06R- Nazione errata                                                       10
071900970917TISI06R- CAP già inserito                                                     11
072000970917TISI06R- 12
072100970917TISI06R- 13
072200970917TISI06R- 14
072300970917TISI06R- 15
072400970917TISI06R- 16
072500970917TISI06R- 17
072600970917TISI06R- 18
072700970917TISI06R- Errori in entrata duplica. Non trovato il record da cui copiare      19
072800970917TISI06R- Il cap è stato duplicato                                             20
072900970917** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
073000970917F1    -                   -    NN     NN
073100970917F2    -                   -    NN     NN
073200970917F3    -F3=Fine            -    NN     NN
073300970917F4    -                   -    NN     NN
073400970917F5    -                   -    NN     NN
073500970917F6    -F6=Conferma        -    SN     SN
073600970917F7    -                   -    NN     NN
073700970917F8    -                   -    NN     NN
073800970917F9    -                   -    NN     NN
073900970917F10   -                   -    NN     NN
074000970917F11   -                   -    NN     NN
074100970917F12   -F12=Ritorno        -    SS     SS
074200970917F13   -                   -    NN     NN
074300970917F14   -                   -    NN     NN
074400970917F15   -                   -    NN     NN
074500970917F16   -                   -    NN     NN
074600970917F17   -                   -    NN     NN
074700970917F18   -                   -    NN     NN
074800970917F19   -                   -    NN     NN
074900970917F20   -                   -    NN     NN
075000970917F21   -                   -    NN     NN
075100970917F22   -                   -    NN     NN
075200970917F23   -                   -    NN     NN
075300970917F24   -F24=Altri tasti    -    NN     NN
075400970917ENTER -                   -    SS     SS
075500970917ROLLUP-                   -    NN     NN
075600970917ROLLDN-                   -    NN     NN
075700970917HELP  -                   -    NN     NN
075800970917PRINT -                   -    NN     NN
075900970917BSPACE-                   -    NN     NN
076000970917CLEAR -                   -    NN     NN

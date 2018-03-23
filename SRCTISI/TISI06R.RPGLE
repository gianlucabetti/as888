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
001100140318     Fazcos01l  IF A E           K DISK
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
008900970917     C*--------------------------------------------------------------------------------------------*
009000970917     C* MAIN LINE
009100970917     C*--------------------------------------------------------------------------------------------*
009200970917     C*
009300970917     C* SE NON CI SONO ERRORI IN ENTRATA
009400970917IF  1C     $FINE         IFEQ      '0'                                          *NO ERRORI
009500970917     C                   WRITE     SI06DVT                                      *TESTATA
009600970917DO  2C     $FINE         DOUEQ     '1'
009700970917     C*
009800970917     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009900970917IF  3C     $GES          IFEQ      'V1'
010000970917     C                   EXSR      GESV1
010100970917E   3C                   ENDIF
010200970917     C*
010300970917E   2C                   ENDDO
010400970917     C*
010500970917IF  2C     $F03          IFEQ      '1'
010600970917     C                   MOVEL     '1'           D00F03
010700970917E   2C                   ENDIF
010800970917E   1C                   ENDIF
010900970917     C*
011000970917     C* OPERAZIONI FINALI
011100970917     C                   EXSR      FINSR
011200970917     C* FINE PGM
011300970917     C                   SETON                                        LR
011400970917     C*--------------------------------------------------------------------------------------------*
011500970917     C* GESV1 - GESTIONE VIDEO UNO
011600970917     C*--------------------------------------------------------------------------------------------*
011700970917     C     GESV1         BEGSR
011800970917     C*
011900970917IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
012000970917     C                   EXSR      CARV1                                        *CARICA VIDEO
012100970917E   1C                   ENDIF
012200970917     C*
012300970917IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012400970917     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012500970917     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012600970917E   1C                   ENDIF
012700970917     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012800970917     C*
012900970917     C                   EXFMT     SI06DV1                                      *PARAMETRI
013000970917     C*
013100970917     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013200970917     C*
013300970917IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013400970917SL  2C                   SELECT
013500970917     C     KEY           WHENEQ    ENTER
013600970917     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013700970917     C     KEY           WHENEQ    F6
013800970917     C                   EXSR      F06GES
013900970917     C                   OTHER
014000970917     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014100970917E   2C                   ENDSL
014200970917E   1C                   ENDIF
014300970917     C*
014400970917     C                   ENDSR
014500970917     C*--------------------------------------------------------------------------------------------*
014600970917     C* CARV1 - CARICA VIDEO UNO
014700970917     C*--------------------------------------------------------------------------------------------*
014800970917     C     CARV1         BEGSR
014900970917     C*
015000970917     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015100970917     C*
015200970917     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015300970917     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015400970917     C*
015500970917     C                   ENDSR
015600970917     C*--------------------------------------------------------------------------------------------*
015700970917     C* ENTV1 - GESTIONE TASTO ENTER
015800970917     C*--------------------------------------------------------------------------------------------*
015900970917     C     ENTV1         BEGSR
016000970917     C*
016100970917     C* CONTROLLA I CAMPI IMMESSI
016200970917     C                   EXSR      DECODI
016300970917     C*
016400970917     C                   ENDSR
016500970917     C*--------------------------------------------------------------------------------------------*
016600970917     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
016700970917     C*--------------------------------------------------------------------------------------------*
016800970917     C     F06GES        BEGSR
016900970917     C*
017000970917     C* CONTROLLA I CAMPI IMMESSI
017100970917     C                   EXSR      DECODI
017200970917     C*
017300970917     C* RILEGGE IL CAP DA CUI COPIARE
017400970917IF  1C     $ERR          IFEQ      '0'
017500970917     C                   Z-ADD     D00VER        KCCVER
017600970917     C                   MOVEL     V1CNAR        KCCNAR
017700970917     C                   MOVEL     V1CCAP        KCCCAP
017800970917     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
017900970917IF  2C     *IN99         IFEQ      *OFF
018000140318     c* copio gli orari servizi
018100140318     c                   exsr      crecos
018200970917     C*
018300970917     C* IMPOSTA IL RECORD DA SCRIVERE
018400970917     C                   Z-ADD     D00VER        CPCVER                         *VERSIONE
018500970917     C                   MOVEL     V1CNA2        CPCNAR                         *NAZIONE
018600970917     C                   MOVEL     V1CCA2        CPCCAP                         *CAP
018700970917     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
018800970917     C                   Z-ADD     *ZEROS        CPCDTR                         *DATA TRASMISSIONE
018900970917     C* SCRIVE
019000970917     C                   WRITE     AZCPC000
019100970917     C*
019200970917     C* IMPOSTA FLAG DI FINE PROGRAMMA
019300970917     C                   MOVEL     MSG(20)       D00MSG
019400970917     C                   MOVEL     '1'           $FINE
019500000427E   2C                   ENDIF
019600970917E   1C                   ENDIF
019700970917     C*
019800970917     C                   ENDSR
019900970917     C*--------------------------------------------------------------------------------------------*
020000970917     C* DECODI - CONTROLLA CAMPI E DECODIFICA
020100970917     C*--------------------------------------------------------------------------------------------*
020200970917     C     DECODI        BEGSR
020300970917     C*
020400970917     C* REIMPOSTA INDICATORI DI ERRORE
020500970917     C                   SETOFF                                       6061
020600970917     C*--------------------
020700970917     C* NAZIONE
020800970917     C*--------------------
020900970917IF  1C     $ERR          IFEQ      '0'
021000970917     C                   MOVEL     V1CNA2        WNAR
021100970917     C                   EXSR      CHKNAR
021200970917IF  2C     $ERR          IFEQ      '1'
021300970917     C                   MOVEL     MSG(10)       $MSG
021400970917     C                   EXSR      CARMSG
021500970917     C                   SETON                                        60
021600970917E   2C                   ENDIF
021700970917E   1C                   ENDIF
021800970917     C*--------------------
021900970917     C* CAP
022000970917     C*--------------------
022100970917IF  1C     $ERR          IFEQ      '0'
022200970917     C                   MOVEL     V1CNA2        WNAR
022300970917     C                   MOVEL     V1CCA2        WCAP
022400970917     C                   EXSR      CHKCAP
022500970917IF  2C     $ERR          IFEQ      '1'
022600970917     C                   SETON                                        61
022700970917     C                   EXSR      CARMSG
022800970917E   2C                   ENDIF
022900970917E   1C                   ENDIF
023000970917     C*
023100970917     C* NUOVO CAP INESISTENTE
023200970917IF  1C     $ERR          IFEQ      '0'
023300970917     C                   Z-ADD     D00VER        KCCVER
023400970917     C                   MOVEL     V1CNA2        KCCNAR
023500970917     C                   MOVEL     V1CCA2        KCCCAP
023600970917     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
023700970917IF  2C     *IN99         IFEQ      *OFF
023800970917     C                   MOVEL     '1'           $ERR
023900970917     C                   MOVEL     MSG(11)       $MSG
024000970917     C                   EXSR      CARMSG
024100970917     C                   SETON                                        6061
024200970917E   2C                   ENDIF
024300970917E   1C                   ENDIF
024400970917     C*
024500970917     C                   ENDSR
024600970917     C*------------------------------------------------------------------------*
024700970917     C* CHKNAR - CONTROLLA LA NAZIONE
024800970917     C*------------------------------------------------------------------------*
024900970917     C     CHKNAR        BEGSR
025000970917     C*
025100970917     C     *LIKE         DEFINE    V1CNAR        WNAR
025200970917     C*
025300970917     C* CONTROLLA ESISTENZA CODICE
025400020916     C                   MOVEL     1             KTBKUT
025500970917     C                   MOVEL     '15'          KTBCOD
025600970917     C                   MOVEL(P)  WNAR          KTBKEY
025700970917     C     KEYTAB        CHAIN     TABEL00F                           99
025800970917IF  1C     *IN99         IFEQ      *ON                                          *INESISTENTE
025900970917     C     TBLFLG        ORNE      *BLANKS                                      *ANNULLATO
026000970917     C                   MOVEL     '1'           $ERR
026100970917E   1C                   ENDIF
026200970917     C*
026300970917     C                   ENDSR
026400970917     C*------------------------------------------------------------------------*
026500970917     C* CHKCAP - CONTROLLA IL CAP
026600970917     C*------------------------------------------------------------------------*
026700970917     C     CHKCAP        BEGSR
026800970917     C*
026900970917     C     *LIKE         DEFINE    V1CCAP        WCAP
027000970917     C*
027100970917     C* CONTROLLA IL CAP
027200970917     C                   CLEAR                   DSSI95
027300970917     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
027400970917     C                   MOVEL     '1'           I95TCN                         *CONTROLLO FORMALE
027500970917     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
027600970917     C                   MOVEL     WCAP          I95CAP                         *CAP
027700970917     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
027800970917     C                   CALL      'TISI95R'
027900970917     C                   PARM                    DSSI95
028000970917IF  1C     O95ERR        IFNE      *BLANKS
028100970917     C                   MOVEL     '1'           $ERR
028200970917     C                   MOVEL     O95MSG        $MSG
028300970917E   1C                   ENDIF
028400970917     C*
028500970917     C                   ENDSR
028600970917     C*--------------------------------------------------------------------------------------------*
028700970917     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
028800970917     C*--------------------------------------------------------------------------------------------*
028900970917     C     IMPVID        BEGSR
029000970917     C*
029100970917     C* CALCOLA LA DATA CORRENTE
029200970917     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
029300970917     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
029400970917     C                   Z-ADD     WNUM8         G08DAT
029500970917     C                   Z-ADD     *ZEROS        G08INV
029600970917     C                   MOVEL     '0'           G08ERR
029700970917     C                   CALL      'XSRDA8'
029800970917     C                   PARM                    WLBDA8
029900970917     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
030000970917     C*
030100970917     C* LEGGE IL CAP
030200970917     C                   Z-ADD     D00VER        KCCVER
030300970917     C                   MOVEL     D00NAR        KCCNAR
030400970917     C                   MOVEL     D00CAP        KCCCAP
030500970917     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
030600970917IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
030700970917     C* DATI OLD
030800970917     C                   MOVEL     CPCNAR        V1CNAR                         *NAZIONE
030900970917     C                   MOVEL     CPCCAP        V1CCAP                         *CAP
031000970917     C* DATI NEW
031100970917     C                   MOVEL     V1CNAR        V1CNA2                         *NAZIONE
031200970917     C                   MOVEL     V1CCAP        V1CCA2                         *CAP
031300970917X   1C                   ELSE                                                   *NON TROVATO
031400970917     C                   MOVEL     '1'           $FINE                          *FINE PGM
031500970917     C                   MOVEL     '1'           D00ERR                         *ERRORE
031600970917     C                   MOVEL     MSG(19)       D00MSG                         *MESSSAGGIO
031700970917E   1C                   ENDIF
031800970917     C*
031900970917     C                   ENDSR
032000970917     C*------------------------------------------------------------------------*
032100970917     C* FINSR - OPERAZIONI FINALI
032200970917     C*------------------------------------------------------------------------*
032300970917     C     FINSR         BEGSR
032400970917     C*
032500970917     C                   CLEAR                   DSSI95
032600970917     C                   MOVEL     'C'           I95TLA
032700970917     C                   CALL      'TISI95R'
032800970917     C                   PARM                    DSSI95
032900970917     C*
033000970917     C                   ENDSR
033100970917     C*--------------------------------------------------------------------------------------------*
033200020916     C* REPERISCE I DATI UTENTE
033300020916     C*--------------------------------------------------------------------------------------------*
033400020916     C     REPDATIUTE    BEGSR
033500020916     C*
033600020916     C* INIZIALIZZA VARIABILI DI WRK
033700020916     C                   CLEAR                   TIBS34DS
033800020916     C                   CLEAR                   AZUTEDS
033900020916     C                   CLEAR                   DDATIUTE
034000020916     C*
034100020916     C     *DTAARA       DEFINE    §azute        azuteds
034200020916     C     *DTAARA       DEFINE    §datiute      ddatiute
034300020916     C                   IN(E)     *DTAARA
034400020916if  1C                   IF        %Error
034500020916     c                   EVAL      I34Tla = 'L'
034600020916     C                   CALL      'TIBS34R'
034700020916     C                   PARM                    Tibs34Ds
034800020916     C                   IN        *DTAARA
034900020916e   1C                   ENDIF
035000020916     C*
035100020916     C                   ENDSR
035200140318     C*--------------------------------------------------------------------------------------------*
035300140318     C* CRECOS - CREA gli orari
035400140318     C*--------------------------------------------------------------------------------------------*
035500140318     C     CRECOS        BEGSR
035600140318     C     *LIKE         DEFINE    cosVER        KCSVER                         *AZCPS01L
035700140318     C     *LIKE         DEFINE    cosNAR        KCSNAR
035800140318     C     *LIKE         DEFINE    cosCAP        KCSCAP
035900140318     C     *LIKE         DEFINE    cosLOC        KCSLOC
036000140318     C*
036100140318     C* LEGGE I SINONIMI DELLA LOCALITA' DA COPIARE
036200140318     C                   Z-ADD     D00VER        KCSVER
036300140318     C                   MOVEL     V1CNAR        KCSNAR
036400140318     C                   MOVEL     V1CCAP        KCSCAP
036500140318     C                   clear                   KCSLOC
036600140318     C     KEYcos        SETLL     AZcos01L
036700140318     C     KEYcos        READE(N)  AZcos01L                               98
036800140318DO  1C     *IN98         DOWEQ     *OFF
036900140318     C*
037000140318     C* SCRIVE IL SINONIMO DELLA NUOVA LOCALITA'
037100140318     C                   Z-ADD     D00VER        cosVER                         *VERSIONE
037200140318     C                   MOVEL     V1CNA2        cosNAR                         *NAZIONE
037300140318     C                   MOVEL     V1CCA2        cosCAP                         *CAP
037400140318     C                   MOVEL     knmus         cosute                         *FLAG TRASMISSIONE
037500140318     C                   Z-ADD     datcor        cosduv                         *DATA TRASMISSIONE
037600140318     C                   WRITE     AZcos000
037700140318     c
037800140318     C     KEYcos        READE(N)  AZcos01L                               98
037900140318E   1C                   ENDDO
038000140318     C*
038100140318     C                   ENDSR
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
040200970917     C*--------------------
040300970917     C* CHIAVI DI LETTURA
040400970917     C*--------------------
040500970917     C* LETTURA TABEL00F - COMPLETA
040600970917     C     KEYTAB        KLIST
040700970917     C                   KFLD                    KTBKUT                         *CODICE UTENTE
040800970917     C                   KFLD                    KTBCOD                         *TABELLA
040900970917     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
041000970917     C* LETTURA AZCPC01L - COMPLETA
041100970917     C     KEYCPC        KLIST
041200970917     C                   KFLD                    KCCVER                         *VERSIONE
041300970917     C                   KFLD                    KCCNAR                         *NAZIONE
041400970917     C                   KFLD                    KCCCAP                         *CAP
041500140318     C     KEYcos        KLIST
041600140318     C                   KFLD                    KCSVER                         *VERSIONE
041700140318     C                   KFLD                    KCSNAR                         *NAZIONE
041800140318     C                   KFLD                    KCSCAP                         *CAP
041900140318     C                   KFLD                    KCSLOC                         *LOCALITA'
042000970917     C***
042100970917     C* IMPOSTAZIONI VIDEO
042200970917     C***
042300970917IF  1C     D00OP0        IFEQ      'C02'
042400970917     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
042500970917X   1C                   ELSE
042600970917     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
042700970917E   1C                   ENDIF
042800970917     C*
042900970917     C* ESEGUE OPERAZIONI INIZIALI STANDARD
043000970917     C                   EXSR      INZSTD
043100970917     C*
043200970917     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
043300970917     C                   EXSR      IMPVID
043400970917     C*
043500970917     C* EMETTO PRIMO FORMATO VIDEO
043600970917     C                   EXSR      CARV1
043700970917     C*
043800970917     C                   ENDSR
043900970917     C*--------------------------------------------------------------------------------------------*
044000970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
044100970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
044200970917     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
044300970917     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
044400970917     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
044500970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
044600970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
044700970917     C*--------------------------------------------------------------------------------------------*
044800970917     C* INZSTD - ROUTINE INIZIALE STANDARD
044900970917     C*--------------------------------------------------------------------------------------------*
045000970917     C     INZSTD        BEGSR
045100970917     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
045200970917     C                   CALL      'XKEYS'
045300970917     C                   PARM                    DSKEYS
045400970917     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
045500970917     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
045600970917     C     XMSGPA        PLIST
045700970917     C                   PARM                    DSMSMS
045800970917     C                   PARM                    DSMSPG
045900970917     C                   PARM                    DSMSKE
046000970917     C                   PARM                    DSMSER
046100970917     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
046200970917     C                   MOVEL     'TISI06R'     DSMSPG                         *NOME PGM CHIAMANTE
046300970917     C                   MOVEL     'TISI06R'     VMCQUE                         *CODA MESSAGGI DESTI
046400970917     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
046500970917     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
046600970917     C* INIZIALIZZO FLAG CONTROLLO PGM
046700970917     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
046800970917     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
046900970917     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
047000970917     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
047100970917     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
047200970917     C                   MOVEL     '0'           $FINE             1            *FINE PGM
047300970917     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
047400970917     C* LANCIO ROUTINE PER VERIFICARE SE
047500970917     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
047600970917     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
047700020916     C                   EXSR      REPDATIUTE
047800970917     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
047900970917     C                   BITOFF    '01234567'    POSMI1            1
048000970917     C                   BITOFF    '01234567'    POSMI2            1
048100970917     C                   BITOFF    '01234567'    POSMA1            1
048200970917     C                   BITOFF    '01234567'    POSMA2            1
048300970917     C                   BITON     '34'          POSMI1            1            *18
048400970917     C                   BITON     '7'           POSMI2            1            *01
048500970917     C                   BITON     '34'          POSMA1            1            *18
048600970917     C                   BITON     '0'           POSMA2            1            *80
048700970917     C                   MOVE      POSMI2        POSMIN            2
048800970917     C                   MOVEL     POSMI1        POSMIN
048900970917     C                   MOVE      POSMA2        POSMAX            2
049000970917     C                   MOVEL     POSMA1        POSMAX
049100970917     C                   ENDSR
049200970917     C*--------------------------------------------------------------------------------------------*
049300970917     C* FUNSTD - LANCIA SUBROUTINE STANDARD
049400970917     C*--------------------------------------------------------------------------------------------*
049500970917     C     FUNSTD        BEGSR
049600970917     C                   SELECT
049700970917     C     KEY           WHENEQ    F3
049800970917     C                   EXSR      F03STD
049900970917     C     KEY           WHENEQ    F12
050000970917     C                   EXSR      F12STD
050100970917     C     KEY           WHENEQ    F24
050200970917     C                   EXSR      F24STD
050300970917     C                   OTHER
050400970917     C                   MOVEL     MSG(2)        $MSG
050500970917     C                   EXSR      CARMSG
050600970917     C                   ENDSL
050700970917     C                   ENDSR
050800970917     C*--------------------------------------------------------------------------------------------*
050900970917     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
051000970917     C*--------------------------------------------------------------------------------------------*
051100970917     C     F03STD        BEGSR
051200970917     C* IMPOSTA FLAG DI FINE PROGRAMMA
051300970917     C                   MOVEL     '1'           $FINE
051400970917     C* IMPOSTA FLAG DI FINE PROCEDURA
051500970917     C                   MOVEL     '1'           $F03
051600970917     C* PULISCE IL MESSAGGIO
051700970917     C                   MOVEL     *BLANKS       D00MSG
051800970917     C                   ENDSR
051900970917     C*--------------------------------------------------------------------------------------------*
052000970917     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
052100970917     C*--------------------------------------------------------------------------------------------*
052200970917     C     F12STD        BEGSR
052300970917     C* IMPOSTA FLAG DI FINE PROGRAMMA
052400970917     C                   MOVEL     '1'           $FINE
052500970917     C* IMPOSTA FLAG DI FINE PROCEDURA
052600970917     C                   MOVEL     '1'           $F12
052700970917     C* PULISCE IL MESSAGGIO
052800970917     C                   MOVEL     *BLANKS       D00MSG
052900970917     C                   ENDSR
053000970917     C*--------------------------------------------------------------------------------------------*
053100970917     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
053200970917     C*--------------------------------------------------------------------------------------------*
053300970917     C     F24STD        BEGSR
053400970917     C*
053500970917     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
053600970917     C                   EXSR      OUTFUN
053700970917     C*
053800970917     C                   ENDSR
053900970917     C*--------------------------------------------------------------------------------------------*
054000970917     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
054100970917     C*          CODA MESSAGGI DEL PGM
054200970917     C*--------------------------------------------------------------------------------------------*
054300970917     C     CARMSG        BEGSR
054400970917     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
054500970917     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
054600970917     C     MSGULT        IFEQ      0
054700970917     C                   MOVEL     '1'           *IN40
054800970917     C                   WRITE     SI06DVK
054900970917     C                   MOVEL     '0'           *IN40
055000970917     C                   ENDIF
055100970917     C* INVIA MESSAGGIO ALLA CODA DEL PGM
055200970917     C                   MOVEL     $MSG          DSMSMS
055300970917     C                   CALL      'XMSG'        XMSGPA
055400970917     C     DSMSER        IFEQ      0
055500970917     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
055600970917     C                   ENDIF
055700970917     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
055800970917     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
055900970917     C                   WRITE     SI06DVM                                      *SFL MESS
056000970917     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
056100970917     C                   MOVEL     *BLANKS       $MSG
056200970917     C                   ENDSR
056300970917     C*--------------------------------------------------------------------------------------------*
056400970917     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
056500970917     C*--------------------------------------------------------------------------------------------*
056600970917     C     OUTMSG        BEGSR
056700970917     C* VISUALIZZAZIONE MESSAGGIO
056800970917     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
056900970917     C                   WRITE     SI06DVK                                      *CONTR MESS
057000970917     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
057100970917     C                   Z-ADD     *ZEROS        MSGULT
057200970917     C                   ENDSR
057300970917     C*--------------------------------------------------------------------------------------------*
057400970917     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
057500970917     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
057600970917     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
057700970917     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
057800970917     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
057900970917     C*--------------------------------------------------------------------------------------------*
058000970917     C     CARFUN        BEGSR
058100970917     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
058200970917     C     1             DO        31            I                 5 0          --- 1 -->
058300970917     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
058400970917     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
058500970917     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
058600020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
058700970917     C     $TGES         IFEQ      'G'                                          *GESTIONE
058800970917     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
058900970917     C                   ELSE                                                   *INTERROG.
059000970917     C                   MOVEL     FUNASI        FAT(I)
059100970917     C                   ENDIF
059200970917     C                   ELSE                                                   <-- 2 --> *FILIALE
059300970917     C     $TGES         IFEQ      'G'                                          *GESTIONE
059400970917     C                   MOVEL     FUNAFG        FAT(I)
059500970917     C                   ELSE                                                   *INTERROG.
059600970917     C                   MOVEL     FUNAFI        FAT(I)
059700970917     C                   ENDIF
059800970917     C                   ENDIF                                                  <-- 2 ---
059900970917     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
060000970917     C                   MOVEL     USER          WEDP              3            *PROFILO
060100970917     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
060200970917     C                   MOVEL     'S'           FAT(I)                         *ABILITO
060300970917     C                   ELSE
060400970917     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
060500970917     C                   ENDIF
060600970917     C                   ENDIF
060700970917     C                   ENDDO                                                  <-- 1 ---
060800970917     C                   ENDSR
060900970917     C*--------------------------------------------------------------------------------------------*
061000970917     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
061100970917     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
061200970917     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
061300970917     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
061400970917     C*--------------------------------------------------------------------------------------------*
061500970917     C     OUTFUN        BEGSR
061600970917     C     1             DO        8             I                              *PULIZIA FUNZIONI A
061700970917     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
061800970917     C                   ENDDO
061900970917     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
062000970917     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
062100970917     C     1             DO        31            I
062200970917     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
062300970917     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
062400970917     C                   ADD       1             FUNNAT
062500970917     C                   ENDIF
062600970917     C                   ENDDO
062700970917     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
062800970917     C     FUNNAT        IFGT      8
062900970917     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
063000970917     C                   ENDIF
063100970917     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
063200970917     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
063300970917     C                   Z-ADD     8             FUNVIS            5 0
063400970917     C     FAT(23)       IFEQ      'S'
063500970917     C                   SUB       1             FUNVIS
063600970917     C                   ENDIF
063700970917     C     FAT(24)       IFEQ      'S'
063800970917     C                   SUB       1             FUNVIS
063900970917     C                   ENDIF
064000970917     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
064100970917     C                   Z-ADD     *ZEROS        W                 5 0
064200970917     C                   ADD       1             FUNULT
064300970917     C     FUNULT        IFGT      31
064400970917     C                   Z-ADD     1             FUNULT
064500970917     C                   ENDIF
064600970917     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
064700970917     C     FUNULT        DO        31            I                              --- 1 -->
064800970917     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
064900970917     C     W             IFLT      FUNVIS                                       --- 2 -->
065000970917     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
065100970917     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
065200970917     C     FDE(I)        ANDNE     *BLANKS
065300970917     C     I             IFEQ      23                                           *TOLGO F23
065400970917     C     I             OREQ      24                                           *E F24
065500970917     C                   ELSE
065600970917     C                   ADD       1             W
065700970917     C                   MOVEL     FDE(I)        FUV(W)
065800970917     C                   ENDIF
065900970917     C                   ENDIF                                                  <-- 3 ---
066000970917     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
066100970917     C                   Z-ADD     I             FUNULT
066200970917     C                   ENDIF                                                  <-- 2 ---
066300970917     C                   ENDDO                                                  <-- 1 ---
066400970917     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
066500970917     C                   MOVEL     FUV(1)        VFD001
066600970917     C                   MOVEL     FUV(2)        VFD002
066700970917     C                   MOVEL     FUV(3)        VFD003
066800970917     C                   MOVEL     FUV(4)        VFD004
066900970917     C                   MOVEL     FUV(5)        VFD005
067000970917     C                   MOVEL     FUV(6)        VFD006
067100970917     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
067200970917     C     FAT(24)       ANDEQ     'N'
067300970917     C                   MOVEL     FUV(7)        VFD007
067400970917     C                   MOVEL     FUV(8)        VFD008
067500970917     C                   ELSE                                                   <-- 1 -->
067600970917     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
067700970917     C     FAT(24)       ANDEQ     'S'
067800970917     C                   MOVEL     FDE(23)       VFD007
067900970917     C                   MOVEL     FDE(24)       VFD008
068000970917     C                   ELSE                                                   <-- 2 -->
068100970917     C                   MOVEL     FUV(7)        VFD007
068200970917     C     FAT(23)       IFEQ      'S'
068300970917     C                   MOVEL     FDE(23)       VFD008
068400970917     C                   ELSE
068500970917     C                   MOVEL     FDE(24)       VFD008
068600970917     C                   ENDIF
068700970917     C                   ENDIF                                                  <-- 2 ---
068800970917     C                   ENDIF                                                  <-- 1 ---
068900970917     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
069000970917     C                   ENDSR
069100970917     C*--------------------------------------------------------------------------------------------*
069200970917     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
069300970917     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
069400970917     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
069500970917     C*                   - ERRORE SE FUNZIONE NON ATTIVA
069600970917     C*--------------------------------------------------------------------------------------------*
069700970917     C     CONFUN        BEGSR
069800970917     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
069900970917     C     1             DO        31            I                              --- 1 -->
070000970917     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
070100970917     C* CONTROLLO SE IL TASTO E' ATTIVO
070200970917     C     FAT(I)        IFNE      'S'                                          --- 3 -->
070300970917     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
070400970917     C                   EXSR      CARMSG
070500970917     C                   MOVEL     '1'           $ERR
070600970917     C                   ENDIF                                                  <-- 3 ---
070700970917     C                   ENDIF                                                  <-- 2 ---
070800970917     C                   ENDDO                                                  <-- 1 ---
070900970917     C                   ENDSR
071000970917     O*--------------------------------------------------------------------------------------------*
071100970917** -MSG- (MESSAGGI)
071200970917TISI06R- Tasto funzionale non valido.                                          1
071300970917TISI06R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
071400970917TISI06R- Opzione non valida.                                                   3
071500970917TISI06R- Opzione abilitata ma non gestita. Avvertire CED.                      4
071600970917TISI06R- Inizio elenco.                                                        5
071700970917TISI06R- Fine elenco.                                                          6
071800970917TISI06R- 7
071900970917TISI06R- 8
072000970917TISI06R- 9
072100970917TISI06R- Nazione errata                                                       10
072200970917TISI06R- CAP già inserito                                                     11
072300970917TISI06R- 12
072400970917TISI06R- 13
072500970917TISI06R- 14
072600970917TISI06R- 15
072700970917TISI06R- 16
072800970917TISI06R- 17
072900970917TISI06R- 18
073000970917TISI06R- Errori in entrata duplica. Non trovato il record da cui copiare      19
073100970917TISI06R- Il cap è stato duplicato                                             20
073200970917** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
073300970917F1    -                   -    NN     NN
073400970917F2    -                   -    NN     NN
073500970917F3    -F3=Fine            -    NN     NN
073600970917F4    -                   -    NN     NN
073700970917F5    -                   -    NN     NN
073800970917F6    -F6=Conferma        -    SN     SN
073900970917F7    -                   -    NN     NN
074000970917F8    -                   -    NN     NN
074100970917F9    -                   -    NN     NN
074200970917F10   -                   -    NN     NN
074300970917F11   -                   -    NN     NN
074400970917F12   -F12=Ritorno        -    SS     SS
074500970917F13   -                   -    NN     NN
074600970917F14   -                   -    NN     NN
074700970917F15   -                   -    NN     NN
074800970917F16   -                   -    NN     NN
074900970917F17   -                   -    NN     NN
075000970917F18   -                   -    NN     NN
075100970917F19   -                   -    NN     NN
075200970917F20   -                   -    NN     NN
075300970917F21   -                   -    NN     NN
075400970917F22   -                   -    NN     NN
075500970917F23   -                   -    NN     NN
075600970917F24   -F24=Altri tasti    -    NN     NN
075700970917ENTER -                   -    SS     SS
075800970917ROLLUP-                   -    NN     NN
075900970917ROLLDN-                   -    NN     NN
076000970917HELP  -                   -    NN     NN
076100970917PRINT -                   -    NN     NN
076200970917BSPACE-                   -    NN     NN
076300970917CLEAR -                   -    NN     NN

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
001100970917     FTABEL00F  IF   E           K DISK
001200970917     D*--------------------------------------------------------------------------------------------*
001300970917     D* SCHIERE
001400970917     D*--------------------------------------------------------------------------------------------*
001500970917     D*-------------------
001600970917     D* MESSAGGI E FUNZIONI
001700970917     D*-------------------
001800970917     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001900970917     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002000970917     D*-------------------
002100970917     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002200970917     D*-------------------
002300970917     D FFU             S              6    DIM(31)                              *FUNZIONE
002400970917     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002500970917     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002600970917     D*-------------------
002700970917     D* TABELLA DESCRIZIONI A VIDEO
002800970917     D*-------------------
002900970917     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003000970917     D*-------------------
003100970917     D* TABELLA TASTI FUNZIONALI PREMUTI
003200970917     D*-------------------
003300970917     D KEYS            S              1    DIM(40)
003400970917     D*--------------------------------------------------------------------------------------------*
003500970917     D* INPUT
003600970917     D*--------------------------------------------------------------------------------------------*
003700970917     D*-------------------
003800970917     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003900970917     D*-------------------
004000970917     D DSFUN         E DS
004100970917     D  FUNRIG                 1     40
004200970917     D*-------------------
004300970917     D* DS PER GESTIONE TASTI FUNZIONALI
004400970917     D*-------------------
004500970917     D DSKEYS        E DS
004600970917     D  KEYRIG                 1     40
004700970917     D*-------------------
004800970917     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004900970917     D*-------------------
005000970917     D                SDS
005100970917     D  USER                 254    263
005200970917     D*-------------------
005300970917     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005400970917     D*-------------------
005500970917     D DEVDS1          DS
005600970917     D  KEY                  369    369
005700970917     D  CURSOR               370    371
005800970917     D*---------------------
005900970917     D* ARCHITETTURA
006000970917     D*---------------------
006100970917     D KPJBA         E DS
006200970917     D*---------------------
006300970917     D* DS ESTERNA DI PROCEDURA
006400970917     D*---------------------
006500970917     D DSSI00        E DS                  EXTNAME(TISI00DS)
006600970917     D*-------------------
006700020916     D* DS REPERIMENTO DATI UTENTE
006800020916     D*-------------------
006900020916     D TIBS34DS      E DS                                                       *Profili utente
007000020916     D DDATIUTE      E DS                                                       *Dati utente
007100020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007200970917     D*---------------------
007300970917     D* DS ESTERNA DI PROCEDURA
007400970917     D*---------------------
007500970917     D DSSI95        E DS                  EXTNAME(TISI95DS)
007600970917     D*-------------------
007700970917     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007800970917     D*-------------------
007900970917     D DSMSG         E DS
008000970917     D*------------------
008100970917     D* DS "XSRDA8" - CONTROLLA DATA (8)
008200970917     D*------------------
008300970917     D WLBDA8          DS
008400970917     D  G08DAT                 1      8  0
008500970917     D  G08INV                 9     16  0
008600970917     D  G08ERR                17     17
008700970917     D  G08TGI                18     22  0
008800970917     C*--------------------------------------------------------------------------------------------*
008900970917     C* MAIN LINE
009000970917     C*--------------------------------------------------------------------------------------------*
009100970917     C*
009200970917     C* SE NON CI SONO ERRORI IN ENTRATA
009300970917IF  1C     $FINE         IFEQ      '0'                                          *NO ERRORI
009400970917     C                   WRITE     SI06DVT                                      *TESTATA
009500970917DO  2C     $FINE         DOUEQ     '1'
009600970917     C*
009700970917     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009800970917IF  3C     $GES          IFEQ      'V1'
009900970917     C                   EXSR      GESV1
010000970917E   3C                   ENDIF
010100970917     C*
010200970917E   2C                   ENDDO
010300970917     C*
010400970917IF  2C     $F03          IFEQ      '1'
010500970917     C                   MOVEL     '1'           D00F03
010600970917E   2C                   ENDIF
010700970917E   1C                   ENDIF
010800970917     C*
010900970917     C* OPERAZIONI FINALI
011000970917     C                   EXSR      FINSR
011100970917     C* FINE PGM
011200970917     C                   SETON                                        LR
011300970917     C*--------------------------------------------------------------------------------------------*
011400970917     C* GESV1 - GESTIONE VIDEO UNO
011500970917     C*--------------------------------------------------------------------------------------------*
011600970917     C     GESV1         BEGSR
011700970917     C*
011800970917IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
011900970917     C                   EXSR      CARV1                                        *CARICA VIDEO
012000970917E   1C                   ENDIF
012100970917     C*
012200970917IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012300970917     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012400970917     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012500970917E   1C                   ENDIF
012600970917     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012700970917     C*
012800970917     C                   EXFMT     SI06DV1                                      *PARAMETRI
012900970917     C*
013000970917     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
013100970917     C*
013200970917IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013300970917SL  2C                   SELECT
013400970917     C     KEY           WHENEQ    ENTER
013500970917     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013600970917     C     KEY           WHENEQ    F6
013700970917     C                   EXSR      F06GES
013800970917     C                   OTHER
013900970917     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
014000970917E   2C                   ENDSL
014100970917E   1C                   ENDIF
014200970917     C*
014300970917     C                   ENDSR
014400970917     C*--------------------------------------------------------------------------------------------*
014500970917     C* CARV1 - CARICA VIDEO UNO
014600970917     C*--------------------------------------------------------------------------------------------*
014700970917     C     CARV1         BEGSR
014800970917     C*
014900970917     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015000970917     C*
015100970917     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015200970917     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015300970917     C*
015400970917     C                   ENDSR
015500970917     C*--------------------------------------------------------------------------------------------*
015600970917     C* ENTV1 - GESTIONE TASTO ENTER
015700970917     C*--------------------------------------------------------------------------------------------*
015800970917     C     ENTV1         BEGSR
015900970917     C*
016000970917     C* CONTROLLA I CAMPI IMMESSI
016100970917     C                   EXSR      DECODI
016200970917     C*
016300970917     C                   ENDSR
016400970917     C*--------------------------------------------------------------------------------------------*
016500970917     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
016600970917     C*--------------------------------------------------------------------------------------------*
016700970917     C     F06GES        BEGSR
016800970917     C*
016900970917     C* CONTROLLA I CAMPI IMMESSI
017000970917     C                   EXSR      DECODI
017100970917     C*
017200970917     C* RILEGGE IL CAP DA CUI COPIARE
017300970917IF  1C     $ERR          IFEQ      '0'
017400970917     C                   Z-ADD     D00VER        KCCVER
017500970917     C                   MOVEL     V1CNAR        KCCNAR
017600970917     C                   MOVEL     V1CCAP        KCCCAP
017700970917     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
017800970917IF  2C     *IN99         IFEQ      *OFF
017900970917     C*
018000970917     C* IMPOSTA IL RECORD DA SCRIVERE
018100970917     C                   Z-ADD     D00VER        CPCVER                         *VERSIONE
018200970917     C                   MOVEL     V1CNA2        CPCNAR                         *NAZIONE
018300970917     C                   MOVEL     V1CCA2        CPCCAP                         *CAP
018400970917     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
018500970917     C                   Z-ADD     *ZEROS        CPCDTR                         *DATA TRASMISSIONE
018600970917     C* SCRIVE
018700970917     C                   WRITE     AZCPC000
018800970917     C*
018900970917     C* IMPOSTA FLAG DI FINE PROGRAMMA
019000970917     C                   MOVEL     MSG(20)       D00MSG
019100970917     C                   MOVEL     '1'           $FINE
019200000427E   2C                   ENDIF
019300970917E   1C                   ENDIF
019400970917     C*
019500970917     C                   ENDSR
019600970917     C*--------------------------------------------------------------------------------------------*
019700970917     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019800970917     C*--------------------------------------------------------------------------------------------*
019900970917     C     DECODI        BEGSR
020000970917     C*
020100970917     C* REIMPOSTA INDICATORI DI ERRORE
020200970917     C                   SETOFF                                       6061
020300970917     C*--------------------
020400970917     C* NAZIONE
020500970917     C*--------------------
020600970917IF  1C     $ERR          IFEQ      '0'
020700970917     C                   MOVEL     V1CNA2        WNAR
020800970917     C                   EXSR      CHKNAR
020900970917IF  2C     $ERR          IFEQ      '1'
021000970917     C                   MOVEL     MSG(10)       $MSG
021100970917     C                   EXSR      CARMSG
021200970917     C                   SETON                                        60
021300970917E   2C                   ENDIF
021400970917E   1C                   ENDIF
021500970917     C*--------------------
021600970917     C* CAP
021700970917     C*--------------------
021800970917IF  1C     $ERR          IFEQ      '0'
021900970917     C                   MOVEL     V1CNA2        WNAR
022000970917     C                   MOVEL     V1CCA2        WCAP
022100970917     C                   EXSR      CHKCAP
022200970917IF  2C     $ERR          IFEQ      '1'
022300970917     C                   SETON                                        61
022400970917     C                   EXSR      CARMSG
022500970917E   2C                   ENDIF
022600970917E   1C                   ENDIF
022700970917     C*
022800970917     C* NUOVO CAP INESISTENTE
022900970917IF  1C     $ERR          IFEQ      '0'
023000970917     C                   Z-ADD     D00VER        KCCVER
023100970917     C                   MOVEL     V1CNA2        KCCNAR
023200970917     C                   MOVEL     V1CCA2        KCCCAP
023300970917     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
023400970917IF  2C     *IN99         IFEQ      *OFF
023500970917     C                   MOVEL     '1'           $ERR
023600970917     C                   MOVEL     MSG(11)       $MSG
023700970917     C                   EXSR      CARMSG
023800970917     C                   SETON                                        6061
023900970917E   2C                   ENDIF
024000970917E   1C                   ENDIF
024100970917     C*
024200970917     C                   ENDSR
024300970917     C*------------------------------------------------------------------------*
024400970917     C* CHKNAR - CONTROLLA LA NAZIONE
024500970917     C*------------------------------------------------------------------------*
024600970917     C     CHKNAR        BEGSR
024700970917     C*
024800970917     C     *LIKE         DEFINE    V1CNAR        WNAR
024900970917     C*
025000970917     C* CONTROLLA ESISTENZA CODICE
025100020916     C                   MOVEL     1             KTBKUT
025200970917     C                   MOVEL     '15'          KTBCOD
025300970917     C                   MOVEL(P)  WNAR          KTBKEY
025400970917     C     KEYTAB        CHAIN     TABEL00F                           99
025500970917IF  1C     *IN99         IFEQ      *ON                                          *INESISTENTE
025600970917     C     TBLFLG        ORNE      *BLANKS                                      *ANNULLATO
025700970917     C                   MOVEL     '1'           $ERR
025800970917E   1C                   ENDIF
025900970917     C*
026000970917     C                   ENDSR
026100970917     C*------------------------------------------------------------------------*
026200970917     C* CHKCAP - CONTROLLA IL CAP
026300970917     C*------------------------------------------------------------------------*
026400970917     C     CHKCAP        BEGSR
026500970917     C*
026600970917     C     *LIKE         DEFINE    V1CCAP        WCAP
026700970917     C*
026800970917     C* CONTROLLA IL CAP
026900970917     C                   CLEAR                   DSSI95
027000970917     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
027100970917     C                   MOVEL     '1'           I95TCN                         *CONTROLLO FORMALE
027200970917     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
027300970917     C                   MOVEL     WCAP          I95CAP                         *CAP
027400970917     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
027500970917     C                   CALL      'TISI95R'
027600970917     C                   PARM                    DSSI95
027700970917IF  1C     O95ERR        IFNE      *BLANKS
027800970917     C                   MOVEL     '1'           $ERR
027900970917     C                   MOVEL     O95MSG        $MSG
028000970917E   1C                   ENDIF
028100970917     C*
028200970917     C                   ENDSR
028300970917     C*--------------------------------------------------------------------------------------------*
028400970917     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
028500970917     C*--------------------------------------------------------------------------------------------*
028600970917     C     IMPVID        BEGSR
028700970917     C*
028800970917     C* CALCOLA LA DATA CORRENTE
028900970917     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
029000970917     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
029100970917     C                   Z-ADD     WNUM8         G08DAT
029200970917     C                   Z-ADD     *ZEROS        G08INV
029300970917     C                   MOVEL     '0'           G08ERR
029400970917     C                   CALL      'XSRDA8'
029500970917     C                   PARM                    WLBDA8
029600970917     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
029700970917     C*
029800970917     C* LEGGE IL CAP
029900970917     C                   Z-ADD     D00VER        KCCVER
030000970917     C                   MOVEL     D00NAR        KCCNAR
030100970917     C                   MOVEL     D00CAP        KCCCAP
030200970917     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
030300970917IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
030400970917     C* DATI OLD
030500970917     C                   MOVEL     CPCNAR        V1CNAR                         *NAZIONE
030600970917     C                   MOVEL     CPCCAP        V1CCAP                         *CAP
030700970917     C* DATI NEW
030800970917     C                   MOVEL     V1CNAR        V1CNA2                         *NAZIONE
030900970917     C                   MOVEL     V1CCAP        V1CCA2                         *CAP
031000970917X   1C                   ELSE                                                   *NON TROVATO
031100970917     C                   MOVEL     '1'           $FINE                          *FINE PGM
031200970917     C                   MOVEL     '1'           D00ERR                         *ERRORE
031300970917     C                   MOVEL     MSG(19)       D00MSG                         *MESSSAGGIO
031400970917E   1C                   ENDIF
031500970917     C*
031600970917     C                   ENDSR
031700970917     C*------------------------------------------------------------------------*
031800970917     C* FINSR - OPERAZIONI FINALI
031900970917     C*------------------------------------------------------------------------*
032000970917     C     FINSR         BEGSR
032100970917     C*
032200970917     C                   CLEAR                   DSSI95
032300970917     C                   MOVEL     'C'           I95TLA
032400970917     C                   CALL      'TISI95R'
032500970917     C                   PARM                    DSSI95
032600970917     C*
032700970917     C                   ENDSR
032800970917     C*--------------------------------------------------------------------------------------------*
032900020916     C* REPERISCE I DATI UTENTE
033000020916     C*--------------------------------------------------------------------------------------------*
033100020916     C     REPDATIUTE    BEGSR
033200020916     C*
033300020916     C* INIZIALIZZA VARIABILI DI WRK
033400020916     C                   CLEAR                   TIBS34DS
033500020916     C                   CLEAR                   AZUTEDS
033600020916     C                   CLEAR                   DDATIUTE
033700020916     C*
033800020916     C     *DTAARA       DEFINE    §azute        azuteds
033900020916     C     *DTAARA       DEFINE    §datiute      ddatiute
034000020916     C                   IN(E)     *DTAARA
034100020916if  1C                   IF        %Error
034200020916     c                   EVAL      I34Tla = 'L'
034300020916     C                   CALL      'TIBS34R'
034400020916     C                   PARM                    Tibs34Ds
034500020916     C                   IN        *DTAARA
034600020916e   1C                   ENDIF
034700020916     C*
034800020916     C                   ENDSR
034900020916     C*--------------------------------------------------------------------------------------------*
035000970917     C* *INZSR  - ROUTINE INIZIALE
035100970917     C*--------------------------------------------------------------------------------------------*
035200970917     C     *INZSR        BEGSR
035300970917     C*--------------------
035400970917     C* RICEVIMENTO PARAMETRI
035500970917     C*--------------------
035600970917     C     *ENTRY        PLIST
035700970917     C                   PARM                    KPJBA
035800970917     C                   PARM                    DSSI00
035900970917     C*--------------------
036000970917     C* VARIABILI RIFERITE AL DATA BASE
036100970917     C*--------------------
036200970917     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
036300970917     C     *LIKE         DEFINE    TBLCOD        KTBCOD
036400970917     C     *LIKE         DEFINE    TBLKEY        KTBKEY
036500970917     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
036600970917     C     *LIKE         DEFINE    CPCNAR        KCCNAR
036700970917     C     *LIKE         DEFINE    CPCCAP        KCCCAP
036800000605     C*
036900970917     C*--------------------
037000970917     C* CHIAVI DI LETTURA
037100970917     C*--------------------
037200970917     C* LETTURA TABEL00F - COMPLETA
037300970917     C     KEYTAB        KLIST
037400970917     C                   KFLD                    KTBKUT                         *CODICE UTENTE
037500970917     C                   KFLD                    KTBCOD                         *TABELLA
037600970917     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
037700970917     C* LETTURA AZCPC01L - COMPLETA
037800970917     C     KEYCPC        KLIST
037900970917     C                   KFLD                    KCCVER                         *VERSIONE
038000970917     C                   KFLD                    KCCNAR                         *NAZIONE
038100970917     C                   KFLD                    KCCCAP                         *CAP
038200970917     C***
038300970917     C* IMPOSTAZIONI VIDEO
038400970917     C***
038500970917IF  1C     D00OP0        IFEQ      'C02'
038600970917     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
038700970917X   1C                   ELSE
038800970917     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
038900970917E   1C                   ENDIF
039000970917     C*
039100970917     C* ESEGUE OPERAZIONI INIZIALI STANDARD
039200970917     C                   EXSR      INZSTD
039300970917     C*
039400970917     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
039500970917     C                   EXSR      IMPVID
039600970917     C*
039700970917     C* EMETTO PRIMO FORMATO VIDEO
039800970917     C                   EXSR      CARV1
039900970917     C*
040000970917     C                   ENDSR
040100970917     C*--------------------------------------------------------------------------------------------*
040200970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040300970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040400970917     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
040500970917     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
040600970917     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
040700970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040800970917     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
040900970917     C*--------------------------------------------------------------------------------------------*
041000970917     C* INZSTD - ROUTINE INIZIALE STANDARD
041100970917     C*--------------------------------------------------------------------------------------------*
041200970917     C     INZSTD        BEGSR
041300970917     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
041400970917     C                   CALL      'XKEYS'
041500970917     C                   PARM                    DSKEYS
041600970917     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
041700970917     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
041800970917     C     XMSGPA        PLIST
041900970917     C                   PARM                    DSMSMS
042000970917     C                   PARM                    DSMSPG
042100970917     C                   PARM                    DSMSKE
042200970917     C                   PARM                    DSMSER
042300970917     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
042400970917     C                   MOVEL     'TISI06R'     DSMSPG                         *NOME PGM CHIAMANTE
042500970917     C                   MOVEL     'TISI06R'     VMCQUE                         *CODA MESSAGGI DESTI
042600970917     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
042700970917     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
042800970917     C* INIZIALIZZO FLAG CONTROLLO PGM
042900970917     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
043000970917     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
043100970917     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
043200970917     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
043300970917     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
043400970917     C                   MOVEL     '0'           $FINE             1            *FINE PGM
043500970917     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
043600970917     C* LANCIO ROUTINE PER VERIFICARE SE
043700970917     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
043800970917     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
043900020916     C                   EXSR      REPDATIUTE
044000970917     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
044100970917     C                   BITOFF    '01234567'    POSMI1            1
044200970917     C                   BITOFF    '01234567'    POSMI2            1
044300970917     C                   BITOFF    '01234567'    POSMA1            1
044400970917     C                   BITOFF    '01234567'    POSMA2            1
044500970917     C                   BITON     '34'          POSMI1            1            *18
044600970917     C                   BITON     '7'           POSMI2            1            *01
044700970917     C                   BITON     '34'          POSMA1            1            *18
044800970917     C                   BITON     '0'           POSMA2            1            *80
044900970917     C                   MOVE      POSMI2        POSMIN            2
045000970917     C                   MOVEL     POSMI1        POSMIN
045100970917     C                   MOVE      POSMA2        POSMAX            2
045200970917     C                   MOVEL     POSMA1        POSMAX
045300970917     C                   ENDSR
045400970917     C*--------------------------------------------------------------------------------------------*
045500970917     C* FUNSTD - LANCIA SUBROUTINE STANDARD
045600970917     C*--------------------------------------------------------------------------------------------*
045700970917     C     FUNSTD        BEGSR
045800970917     C                   SELECT
045900970917     C     KEY           WHENEQ    F3
046000970917     C                   EXSR      F03STD
046100970917     C     KEY           WHENEQ    F12
046200970917     C                   EXSR      F12STD
046300970917     C     KEY           WHENEQ    F24
046400970917     C                   EXSR      F24STD
046500970917     C                   OTHER
046600970917     C                   MOVEL     MSG(2)        $MSG
046700970917     C                   EXSR      CARMSG
046800970917     C                   ENDSL
046900970917     C                   ENDSR
047000970917     C*--------------------------------------------------------------------------------------------*
047100970917     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
047200970917     C*--------------------------------------------------------------------------------------------*
047300970917     C     F03STD        BEGSR
047400970917     C* IMPOSTA FLAG DI FINE PROGRAMMA
047500970917     C                   MOVEL     '1'           $FINE
047600970917     C* IMPOSTA FLAG DI FINE PROCEDURA
047700970917     C                   MOVEL     '1'           $F03
047800970917     C* PULISCE IL MESSAGGIO
047900970917     C                   MOVEL     *BLANKS       D00MSG
048000970917     C                   ENDSR
048100970917     C*--------------------------------------------------------------------------------------------*
048200970917     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
048300970917     C*--------------------------------------------------------------------------------------------*
048400970917     C     F12STD        BEGSR
048500970917     C* IMPOSTA FLAG DI FINE PROGRAMMA
048600970917     C                   MOVEL     '1'           $FINE
048700970917     C* IMPOSTA FLAG DI FINE PROCEDURA
048800970917     C                   MOVEL     '1'           $F12
048900970917     C* PULISCE IL MESSAGGIO
049000970917     C                   MOVEL     *BLANKS       D00MSG
049100970917     C                   ENDSR
049200970917     C*--------------------------------------------------------------------------------------------*
049300970917     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
049400970917     C*--------------------------------------------------------------------------------------------*
049500970917     C     F24STD        BEGSR
049600970917     C*
049700970917     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
049800970917     C                   EXSR      OUTFUN
049900970917     C*
050000970917     C                   ENDSR
050100970917     C*--------------------------------------------------------------------------------------------*
050200970917     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
050300970917     C*          CODA MESSAGGI DEL PGM
050400970917     C*--------------------------------------------------------------------------------------------*
050500970917     C     CARMSG        BEGSR
050600970917     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
050700970917     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
050800970917     C     MSGULT        IFEQ      0
050900970917     C                   MOVEL     '1'           *IN40
051000970917     C                   WRITE     SI06DVK
051100970917     C                   MOVEL     '0'           *IN40
051200970917     C                   ENDIF
051300970917     C* INVIA MESSAGGIO ALLA CODA DEL PGM
051400970917     C                   MOVEL     $MSG          DSMSMS
051500970917     C                   CALL      'XMSG'        XMSGPA
051600970917     C     DSMSER        IFEQ      0
051700970917     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
051800970917     C                   ENDIF
051900970917     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
052000970917     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
052100970917     C                   WRITE     SI06DVM                                      *SFL MESS
052200970917     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
052300970917     C                   MOVEL     *BLANKS       $MSG
052400970917     C                   ENDSR
052500970917     C*--------------------------------------------------------------------------------------------*
052600970917     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
052700970917     C*--------------------------------------------------------------------------------------------*
052800970917     C     OUTMSG        BEGSR
052900970917     C* VISUALIZZAZIONE MESSAGGIO
053000970917     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
053100970917     C                   WRITE     SI06DVK                                      *CONTR MESS
053200970917     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
053300970917     C                   Z-ADD     *ZEROS        MSGULT
053400970917     C                   ENDSR
053500970917     C*--------------------------------------------------------------------------------------------*
053600970917     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
053700970917     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
053800970917     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
053900970917     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
054000970917     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
054100970917     C*--------------------------------------------------------------------------------------------*
054200970917     C     CARFUN        BEGSR
054300970917     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
054400970917     C     1             DO        31            I                 5 0          --- 1 -->
054500970917     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
054600970917     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
054700970917     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
054800020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
054900970917     C     $TGES         IFEQ      'G'                                          *GESTIONE
055000970917     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
055100970917     C                   ELSE                                                   *INTERROG.
055200970917     C                   MOVEL     FUNASI        FAT(I)
055300970917     C                   ENDIF
055400970917     C                   ELSE                                                   <-- 2 --> *FILIALE
055500970917     C     $TGES         IFEQ      'G'                                          *GESTIONE
055600970917     C                   MOVEL     FUNAFG        FAT(I)
055700970917     C                   ELSE                                                   *INTERROG.
055800970917     C                   MOVEL     FUNAFI        FAT(I)
055900970917     C                   ENDIF
056000970917     C                   ENDIF                                                  <-- 2 ---
056100970917     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
056200970917     C                   MOVEL     USER          WEDP              3            *PROFILO
056300970917     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
056400970917     C                   MOVEL     'S'           FAT(I)                         *ABILITO
056500970917     C                   ELSE
056600970917     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
056700970917     C                   ENDIF
056800970917     C                   ENDIF
056900970917     C                   ENDDO                                                  <-- 1 ---
057000970917     C                   ENDSR
057100970917     C*--------------------------------------------------------------------------------------------*
057200970917     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
057300970917     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
057400970917     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
057500970917     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
057600970917     C*--------------------------------------------------------------------------------------------*
057700970917     C     OUTFUN        BEGSR
057800970917     C     1             DO        8             I                              *PULIZIA FUNZIONI A
057900970917     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
058000970917     C                   ENDDO
058100970917     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
058200970917     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
058300970917     C     1             DO        31            I
058400970917     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
058500970917     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
058600970917     C                   ADD       1             FUNNAT
058700970917     C                   ENDIF
058800970917     C                   ENDDO
058900970917     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
059000970917     C     FUNNAT        IFGT      8
059100970917     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
059200970917     C                   ENDIF
059300970917     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
059400970917     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
059500970917     C                   Z-ADD     8             FUNVIS            5 0
059600970917     C     FAT(23)       IFEQ      'S'
059700970917     C                   SUB       1             FUNVIS
059800970917     C                   ENDIF
059900970917     C     FAT(24)       IFEQ      'S'
060000970917     C                   SUB       1             FUNVIS
060100970917     C                   ENDIF
060200970917     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
060300970917     C                   Z-ADD     *ZEROS        W                 5 0
060400970917     C                   ADD       1             FUNULT
060500970917     C     FUNULT        IFGT      31
060600970917     C                   Z-ADD     1             FUNULT
060700970917     C                   ENDIF
060800970917     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
060900970917     C     FUNULT        DO        31            I                              --- 1 -->
061000970917     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
061100970917     C     W             IFLT      FUNVIS                                       --- 2 -->
061200970917     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
061300970917     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
061400970917     C     FDE(I)        ANDNE     *BLANKS
061500970917     C     I             IFEQ      23                                           *TOLGO F23
061600970917     C     I             OREQ      24                                           *E F24
061700970917     C                   ELSE
061800970917     C                   ADD       1             W
061900970917     C                   MOVEL     FDE(I)        FUV(W)
062000970917     C                   ENDIF
062100970917     C                   ENDIF                                                  <-- 3 ---
062200970917     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
062300970917     C                   Z-ADD     I             FUNULT
062400970917     C                   ENDIF                                                  <-- 2 ---
062500970917     C                   ENDDO                                                  <-- 1 ---
062600970917     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
062700970917     C                   MOVEL     FUV(1)        VFD001
062800970917     C                   MOVEL     FUV(2)        VFD002
062900970917     C                   MOVEL     FUV(3)        VFD003
063000970917     C                   MOVEL     FUV(4)        VFD004
063100970917     C                   MOVEL     FUV(5)        VFD005
063200970917     C                   MOVEL     FUV(6)        VFD006
063300970917     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
063400970917     C     FAT(24)       ANDEQ     'N'
063500970917     C                   MOVEL     FUV(7)        VFD007
063600970917     C                   MOVEL     FUV(8)        VFD008
063700970917     C                   ELSE                                                   <-- 1 -->
063800970917     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
063900970917     C     FAT(24)       ANDEQ     'S'
064000970917     C                   MOVEL     FDE(23)       VFD007
064100970917     C                   MOVEL     FDE(24)       VFD008
064200970917     C                   ELSE                                                   <-- 2 -->
064300970917     C                   MOVEL     FUV(7)        VFD007
064400970917     C     FAT(23)       IFEQ      'S'
064500970917     C                   MOVEL     FDE(23)       VFD008
064600970917     C                   ELSE
064700970917     C                   MOVEL     FDE(24)       VFD008
064800970917     C                   ENDIF
064900970917     C                   ENDIF                                                  <-- 2 ---
065000970917     C                   ENDIF                                                  <-- 1 ---
065100970917     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
065200970917     C                   ENDSR
065300970917     C*--------------------------------------------------------------------------------------------*
065400970917     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
065500970917     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
065600970917     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
065700970917     C*                   - ERRORE SE FUNZIONE NON ATTIVA
065800970917     C*--------------------------------------------------------------------------------------------*
065900970917     C     CONFUN        BEGSR
066000970917     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
066100970917     C     1             DO        31            I                              --- 1 -->
066200970917     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
066300970917     C* CONTROLLO SE IL TASTO E' ATTIVO
066400970917     C     FAT(I)        IFNE      'S'                                          --- 3 -->
066500970917     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
066600970917     C                   EXSR      CARMSG
066700970917     C                   MOVEL     '1'           $ERR
066800970917     C                   ENDIF                                                  <-- 3 ---
066900970917     C                   ENDIF                                                  <-- 2 ---
067000970917     C                   ENDDO                                                  <-- 1 ---
067100970917     C                   ENDSR
067200970917     O*--------------------------------------------------------------------------------------------*
067300970917** -MSG- (MESSAGGI)
067400970917TISI06R- Tasto funzionale non valido.                                          1
067500970917TISI06R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
067600970917TISI06R- Opzione non valida.                                                   3
067700970917TISI06R- Opzione abilitata ma non gestita. Avvertire CED.                      4
067800970917TISI06R- Inizio elenco.                                                        5
067900970917TISI06R- Fine elenco.                                                          6
068000970917TISI06R- 7
068100970917TISI06R- 8
068200970917TISI06R- 9
068300970917TISI06R- Nazione errata                                                       10
068400970917TISI06R- CAP già inserito                                                     11
068500970917TISI06R- 12
068600970917TISI06R- 13
068700970917TISI06R- 14
068800970917TISI06R- 15
068900970917TISI06R- 16
069000970917TISI06R- 17
069100970917TISI06R- 18
069200970917TISI06R- Errori in entrata duplica. Non trovato il record da cui copiare      19
069300970917TISI06R- Il cap è stato duplicato                                             20
069400970917** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
069500970917F1    -                   -    NN     NN
069600970917F2    -                   -    NN     NN
069700970917F3    -F3=Fine            -    NN     NN
069800970917F4    -                   -    NN     NN
069900970917F5    -                   -    NN     NN
070000970917F6    -F6=Conferma        -    SN     SN
070100970917F7    -                   -    NN     NN
070200970917F8    -                   -    NN     NN
070300970917F9    -                   -    NN     NN
070400970917F10   -                   -    NN     NN
070500970917F11   -                   -    NN     NN
070600970917F12   -F12=Ritorno        -    SS     SS
070700970917F13   -                   -    NN     NN
070800970917F14   -                   -    NN     NN
070900970917F15   -                   -    NN     NN
071000970917F16   -                   -    NN     NN
071100970917F17   -                   -    NN     NN
071200970917F18   -                   -    NN     NN
071300970917F19   -                   -    NN     NN
071400970917F20   -                   -    NN     NN
071500970917F21   -                   -    NN     NN
071600970917F22   -                   -    NN     NN
071700970917F23   -                   -    NN     NN
071800970917F24   -F24=Altri tasti    -    NN     NN
071900970917ENTER -                   -    SS     SS
072000970917ROLLUP-                   -    NN     NN
072100970917ROLLDN-                   -    NN     NN
072200970917HELP  -                   -    NN     NN
072300970917PRINT -                   -    NN     NN
072400970917BSPACE-                   -    NN     NN
072500970917CLEAR -                   -    NN     NN

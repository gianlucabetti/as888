000100971007     H*-------------------------------------------------------------------------
000200971007     H* GESTIONE VOCI DI CONTO ECONOMICO - DUPLICA
000300971007     H*-------------------------------------------------------------------------
000400971007     H DECEDIT('0,') DATEDIT(*DMY.)
000500971007     F*-------------------------------------------------------------------------
000600971007     F* DATA BASE
000700971007     F*-------------------------------------------------------------------------
000800971007     FTNTL76D   CF   E             WORKSTN INFDS(DEVDS1)
000900971007     F                                     SFILE(TL76DVM:MSGULT)
001000011019     FTITCG01L  UF A E           K DISK
001100971007     D*-------------------------------------------------------------------------
001200971007     D* SCHIERE
001300971007     D*-------------------------------------------------------------------------
001400971007     D*-------------------
001500971007     D* MESSAGGI E FUNZIONI
001600971007     D*-------------------
001700971007     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001800971007     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001900971007     D*-------------------
002000971007     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002100971007     D*-------------------
002200971007     D FFU             S              6    DIM(31)                              *FUNZIONE
002300971007     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002400971007     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002500971007     D*-------------------
002600971007     D* TABELLA DESCRIZIONI A VIDEO
002700971007     D*-------------------
002800971007     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002900971007     D*-------------------
003000971007     D* TABELLA TASTI FUNZIONALI PREMUTI
003100971007     D*-------------------
003200971007     D KEYS            S              1    DIM(40)
003300971007     D*-------------------------------------------------------------------------
003400971007     D* INPUT
003500971007     D*-------------------------------------------------------------------------
003600971007     D*-------------------
003700971007     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003800971007     D*-------------------
003900971007     D DSFUN         E DS
004000971007     D  FUNRIG                 1     40
004100971007     D*-------------------
004200971007     D* DS PER GESTIONE TASTI FUNZIONALI
004300971007     D*-------------------
004400971007     D DSKEYS        E DS
004500971007     D  KEYRIG                 1     40
004600971007     D*-------------------
004700971007     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004800971007     D*-------------------
004900971007     D                SDS
005000971007     D  USER                 254    263
005100971007     D*-------------------
005200971007     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005300971007     D*-------------------
005400971007     D DEVDS1          DS
005500971007     D  KEY                  369    369
005600971007     D  CURSOR               370    371
005700971007     D*-------------------
005800971007     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
005900971007     D*-------------------
006000971007     D DSMSG         E DS
006100971007     D*-------------------
006200020923     D* DS REPERIMENTO DATI UTENTE
006300971007     D*-------------------
006400020923     D TIBS34DS      E DS                                                       *Profili utente
006500020923     D DDATIUTE      E DS                                                       *Dati utente
006600020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006700971007     D*------------------
006800971007     D* DS "XSRDA8" - CONTROLLA DATA (8)
006900971007     D*------------------
007000971007     D WLBDA8          DS
007100971007     D  G08DAT                 1      8  0
007200971007     D  G08INV                 9     16  0
007300971007     D  G08ERR                17     17
007400971007     D  G08TGI                18     22  0
007500971007     D*---------------------
007600971007     D* DS ESTERNA DI PROCEDURA
007700971007     D*---------------------
007800971007     D DSTL73        E DS                  EXTNAME(TNTL73DS)
007900971007     D*---------------------
008000971007     D* ARCHITETTURA
008100971007     D*---------------------
008200971007     D KPJBA         E DS
008300971007     C*-------------------------------------------------------------------------
008400971007     C* MAIN LINE
008500971007     C*-------------------------------------------------------------------------
008600971007     C*
008700971007     C* SE NON CI SONO ERRORI IN ENTRATA
008800971007IF  1C     $FINE         IFEQ      '0'                                          *NO ERRORI
008900971007     C                   WRITE     TL76DVT                                      *TESTATA
009000971007     C*
009100971007     C* GESTIONE FILE VIDEO
009200971007DO  2C     $FINE         DOUEQ     '1'
009300971007     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009400971007IF  3C     $GES          IFEQ      'V1'
009500971007     C                   EXSR      GESV1
009600971007E   3C                   ENDIF
009700971007E   2C                   ENDDO
009800971007     C*
009900971007IF  2C     $F03          IFEQ      '1'
010000971007     C                   MOVEL     '1'           D73F03
010100971007E   2C                   ENDIF
010200971007E   1C                   ENDIF
010300971007     C* FINE PGM
010400971007     C                   SETON                                        LR
010500971007     C*-------------------------------------------------------------------------
010600971007     C* GESV1 - GESTIONE VIDEO UNO
010700971007     C*-------------------------------------------------------------------------
010800971007     C     GESV1         BEGSR
010900971007     C*
011000971007IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
011100971007     C                   EXSR      CARV1                                        *CARICA VIDEO
011200971007E   1C                   ENDIF
011300971007     C*
011400971007IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011500971007     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011600971007     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011700971007E   1C                   ENDIF
011800971007     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011900971007     C*
012000971007     C                   EXFMT     TL76DV1                                      *PARAMETRI
012100971007     C*
012200971007     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012300971007     C*
012400971007IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012500971007SL  2C                   SELECT
012600971007     C     KEY           WHENEQ    ENTER
012700971007     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012800971007     C     KEY           WHENEQ    F6
012900971007     C                   EXSR      F06GES
013000971007     C                   OTHER
013100971007     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013200971007E   2C                   ENDSL
013300971007E   1C                   ENDIF
013400971007     C*
013500971007     C                   ENDSR
013600971007     C*-------------------------------------------------------------------------
013700971007     C* CARV1 - CARICA VIDEO UNO
013800971007     C*-------------------------------------------------------------------------
013900971007     C     CARV1         BEGSR
014000971007     C*
014100971007     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014200971007     C*
014300971007     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014400971007     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014500971007     C*
014600971007     C                   ENDSR
014700971007     C*-------------------------------------------------------------------------
014800971007     C* ENTV1 - GESTIONE TASTO ENTER
014900971007     C*-------------------------------------------------------------------------
015000971007     C     ENTV1         BEGSR
015100971007     C*
015200971007     C* CONTROLLA I CAMPI IMMESSI
015300971007     C                   EXSR      DECODI
015400971007     C*
015500971007     C                   ENDSR
015600971007     C*-------------------------------------------------------------------------
015700971007     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
015800971007     C*-------------------------------------------------------------------------
015900971007     C     F06GES        BEGSR
016000971007     C*
016100971007     C* CONTROLLA I CAMPI IMMESSI
016200971007     C                   EXSR      DECODI
016300971007     C*
016400971007     C* RILEGGE IL RECORD DA CUI COPIARE
016500971007IF  1C     $ERR          IFEQ      '0'
016600971007     C                   Z-ADD     D73DAT        KTCDAT
016700011019     C     KEYTCG        CHAIN(N)  TITCG01L                           99
016800971007     C*
016900971007     C* IMPOSTA IL RECORD DA SCRIVERE
017000971007IF  2C     *IN99         IFEQ      *OFF
017100971007     C                   Z-ADD     DA2V1C        TCGDAT                         *DATA
017200971007     C                   MOVEL     *BLANKS       TCGFTR                         *FLAG TRASMISSIONE
017300971007     C                   Z-ADD     *ZEROS        TCGDTR                         *DATA TRASMISSIONE
017400971007     C* SCRIVE
017500011019     C                   WRITE     TITCG000
017600971007     C*
017700971007     C* IMPOSTA FLAG DI FINE PROGRAMMA
017800971007     C                   MOVEL     MSG(20)       D73MSG
017900971007     C                   MOVEL     '1'           $FINE
018000971007E   2C                   ENDIF
018100971007E   1C                   ENDIF
018200971007     C*
018300971007     C                   ENDSR
018400971007     C*-------------------------------------------------------------------------
018500971007     C* DECODI - CONTROLLA CAMPI E DECODIFICA
018600971007     C*-------------------------------------------------------------------------
018700971007     C     DECODI        BEGSR
018800971007     C*
018900971007     C* REIMPOSTA INDICATORI DI ERRORE
019000971007     C                   SETOFF                                       6061
019100971007     C*--------------------
019200971007     C* DATA
019300971007     C*--------------------
019400971007     C     *LIKE         DEFINE    V1CDA2        DA2V1C
019500971007IF  1C     $ERR          IFEQ      '0'
019600971007     C                   Z-ADD     *ZEROS        DA2V1C
019700971007     C                   Z-ADD     V1CDA2        WDA8
019800971007     C                   Z-ADD     *ZEROS        DA8W
019900971007     C                   EXSR      CHKDA8
020000971007     C                   Z-ADD     WDA8          V1CDA2
020100971007     C                   Z-ADD     DA8W          DA2V1C
020200971007IF  2C     $ERR          IFEQ      '1'
020300971007     C                   MOVEL     '1'           $ERR
020400971007     C                   MOVEL     MSG(10)       $MSG
020500971007     C                   EXSR      CARMSG
020600971007     C                   SETON                                        60
020700971007E   2C                   ENDIF
020800971007E   1C                   ENDIF
020900971007     C*
021000971007     C* NUOVO RECORD INESISTENTE
021100971007IF  1C     $ERR          IFEQ      '0'
021200971007     C                   Z-ADD     DA2V1C        KTCDAT
021300011019     C     KEYTCG        CHAIN(N)  TITCG01L                           99
021400971007IF  2C     *IN99         IFEQ      *OFF
021500971007     C                   MOVEL     '1'           $ERR
021600971007     C                   MOVEL     MSG(18)       $MSG
021700971007     C                   EXSR      CARMSG
021800971007     C                   SETON                                        60
021900971007E   2C                   ENDIF
022000971007E   1C                   ENDIF
022100011019     C*--------------------
022200011019     C* DIVISA
022300011019     C*--------------------
022400011019IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
022500011019IF  2C     TCGDIV        IFEQ      'ITL'
022600011019     C     DA2V1C        ANDGE     20020101
022700011019     C                   MOVEL     '1'           $ERR
022800011019     C                   MOVEL     MSG(11)       $MSG
022900011019     C                   EXSR      CARMSG
023000011019     C                   SETON                                        60
023100011019E   2C                   ENDIF
023200011019E   1C                   ENDIF
023300971007     C*
023400971007     C                   ENDSR
023500971007     C*------------------------------------------------------------------------*
023600971007     C* CHKDA8 - CONTROLLA DATA (8)
023700971007     C*------------------------------------------------------------------------*
023800971007     C     CHKDA8        BEGSR
023900971007     C*
024000971007     C     *LIKE         DEFINE    V1CDAT        WDA8
024100971007     C     *LIKE         DEFINE    V1CDAT        DA8W
024200971007     C*
024300971007     C                   Z-ADD     WDA8          G08DAT
024400971007     C                   Z-ADD     *ZEROS        G08INV
024500971007     C                   MOVEL     '0'           G08ERR
024600971007     C                   CALL      'XSRDA8'
024700971007     C                   PARM                    WLBDA8
024800971007     C                   Z-ADD     G08DAT        WDA8
024900971007     C                   Z-ADD     G08INV        DA8W
025000971007IF  1C     G08ERR        IFEQ      '1'
025100971007     C                   MOVEL     '1'           $ERR
025200971007E   1C                   ENDIF
025300971007     C*
025400971007     C                   ENDSR
025500971007     C*-------------------------------------------------------------------------
025600971007     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
025700971007     C*-------------------------------------------------------------------------
025800971007     C     IMPVID        BEGSR
025900971007     C*
026000971007     C* LEGGE IL RECORD
026100971007     C                   Z-ADD     D73DAT        KTCDAT
026200011019     C     KEYTCG        CHAIN(N)  TITCG01L                           99
026300971007IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
026400971007     C                   Z-ADD     TCGDAT        G08INV
026500971007     C                   Z-ADD     *ZEROS        G08DAT
026600971007     C                   MOVEL     '3'           G08ERR
026700971007     C                   CALL      'XSRDA8'
026800971007     C                   PARM                    WLBDA8
026900971007     C                   Z-ADD     G08DAT        V1CDAT
027000971007     C                   ELSE                                                   *NON TROVATO
027100971007     C                   MOVEL     '1'           $FINE                          *FINE PGM
027200971007     C                   MOVEL     '1'           D73ERR                         *ERRORE
027300971007     C                   MOVEL     MSG(19)       D73MSG                         *MESSSAGGIO
027400971007E   1C                   ENDIF
027500971007     C*
027600971007     C                   ENDSR
027700020923     C*--------------------------------------------------------------------------------------------*
027800020923     C* REPERISCE I DATI UTENTE
027900020923     C*--------------------------------------------------------------------------------------------*
028000020923     C     REPDATIUTE    BEGSR
028100020923     C*
028200020923     C* INIZIALIZZA VARIABILI DI WRK
028300020923     C                   CLEAR                   TIBS34DS
028400020923     C                   CLEAR                   AZUTEDS
028500020923     C                   CLEAR                   DDATIUTE
028600020923     C*
028700020923     C     *DTAARA       DEFINE    §azute        azuteds
028800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
028900020923     C                   IN(E)     *DTAARA
029000020923if  1C                   IF        %Error
029100020923     c                   EVAL      I34Tla = 'L'
029200020923     C                   CALL      'TIBS34R'
029300020923     C                   PARM                    Tibs34Ds
029400020923     C                   IN        *DTAARA
029500020923e   1C                   ENDIF
029600020923     C*
029700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
029800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
029900020923     C*
030000020923     C                   ENDSR
030100971007     C*-------------------------------------------------------------------------
030200971007     C* *INZSR  - ROUTINE INIZIALE
030300971007     C*-------------------------------------------------------------------------
030400971007     C     *INZSR        BEGSR
030500971007     C*--------------------
030600971007     C* RICEVIMENTO PARAMETRI
030700971007     C*--------------------
030800971007     C     *ENTRY        PLIST
030900971007     C                   PARM                    KPJBA
031000971007     C                   PARM                    DSTL73
031100971007     C*--------------------
031200971007     C* VARIABILI RIFERITE AL DATA BASE
031300971007     C*--------------------
031400011019     C     *LIKE         DEFINE    TCGDAT        KTCDAT                         *TITCG00F
031500971007     C*--------------------
031600971007     C* CHIAVI DI LETTURA
031700971007     C*--------------------
031800971007     C* LETTURA TABEL00F - COMPLETA
031900971007     C     KEYTCG        KLIST
032000971007     C                   KFLD                    KTCDAT                         *DATA
032100971007     C***
032200971007     C* IMPOSTAZIONI VIDEO
032300971007     C***
032400971007IF  1C     D73OP0        IFEQ      'V02'
032500971007     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
032600971007X   1C                   ELSE
032700971007     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
032800971007E   1C                   ENDIF
032900971007     C*
033000971007     C* ESEGUE OPERAZIONI INIZIALI STANDARD
033100971007     C                   EXSR      INZSTD
033200971007     C*
033300971007     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
033400971007     C                   EXSR      IMPVID
033500971007     C*
033600971007     C* EMETTO PRIMO FORMATO VIDEO
033700971007     C                   EXSR      CARV1
033800971007     C*
033900971007     C                   ENDSR
034000971007     C*-------------------------------------------------------------------------
034100971007     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034200971007     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034300971007     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
034400971007     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
034500971007     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
034600971007     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034700971007     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
034800971007     C*-------------------------------------------------------------------------
034900971007     C* INZSTD - ROUTINE INIZIALE STANDARD
035000971007     C*-------------------------------------------------------------------------
035100971007     C     INZSTD        BEGSR
035200971007     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
035300971007     C                   CALL      'XKEYS'
035400971007     C                   PARM                    DSKEYS
035500971007     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
035600971007     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
035700971007     C     XMSGPA        PLIST
035800971007     C                   PARM                    DSMSMS
035900971007     C                   PARM                    DSMSPG
036000971007     C                   PARM                    DSMSKE
036100971007     C                   PARM                    DSMSER
036200971007     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
036300971007     C                   MOVEL     'TNTL76R'     DSMSPG                         *NOME PGM CHIAMANTE
036400971007     C                   MOVEL     'TNTL76R'     VMCQUE                         *CODA MESSAGGI DESTI
036500971007     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
036600971007     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
036700971007     C* INIZIALIZZO FLAG CONTROLLO PGM
036800971007     C                   MOVEL     '0'           $ERR              1            *ERRORE VIDEO UNO
036900971007     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
037000971007     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
037100971007     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
037200971007     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
037300971007     C                   MOVEL     '0'           $FINE             1            *FINE PGM
037400971007     C                   MOVEL     '1'           $TESVD            1            1=RIEMETTERE TESTATA
037500971007     C* LANCIO ROUTINE PER VERIFICARE SE
037600971007     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
037700971007     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
037800020923     C                   EXSR      REPDATIUTE
037900971007     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
038000971007     C                   BITOFF    '01234567'    POSMI1            1
038100971007     C                   BITOFF    '01234567'    POSMI2            1
038200971007     C                   BITOFF    '01234567'    POSMA1            1
038300971007     C                   BITOFF    '01234567'    POSMA2            1
038400971007     C                   BITON     '34'          POSMI1            1            *18
038500971007     C                   BITON     '7'           POSMI2            1            *01
038600971007     C                   BITON     '34'          POSMA1            1            *18
038700971007     C                   BITON     '0'           POSMA2            1            *80
038800971007     C                   MOVE      POSMI2        POSMIN            2
038900971007     C                   MOVEL     POSMI1        POSMIN
039000971007     C                   MOVE      POSMA2        POSMAX            2
039100971007     C                   MOVEL     POSMA1        POSMAX
039200971007     C                   ENDSR
039300971007     C*-------------------------------------------------------------------------
039400971007     C* FUNSTD - LANCIA SUBROUTINE STANDARD
039500971007     C*-------------------------------------------------------------------------
039600971007     C     FUNSTD        BEGSR
039700971007     C                   SELECT
039800971007     C     KEY           WHENEQ    F3
039900971007     C                   EXSR      F03STD
040000971007     C     KEY           WHENEQ    F12
040100971007     C                   EXSR      F12STD
040200971007     C     KEY           WHENEQ    F24
040300971007     C                   EXSR      F24STD
040400971007     C                   OTHER
040500971007     C                   MOVEL     MSG(2)        $MSG
040600971007     C                   EXSR      CARMSG
040700971007     C                   ENDSL
040800971007     C                   ENDSR
040900971007     C*-------------------------------------------------------------------------
041000971007     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
041100971007     C*-------------------------------------------------------------------------
041200971007     C     F03STD        BEGSR
041300971007     C* IMPOSTA FLAG DI FINE PROGRAMMA
041400971007     C                   MOVEL     '1'           $FINE
041500971007     C* IMPOSTA FLAG DI FINE PROCEDURA
041600971007     C                   MOVEL     '1'           $F03
041700971007     C* PULISCE IL MESSAGGIO
041800971007     C                   MOVEL     *BLANKS       D73MSG
041900971007     C                   ENDSR
042000971007     C*-------------------------------------------------------------------------
042100971007     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
042200971007     C*-------------------------------------------------------------------------
042300971007     C     F12STD        BEGSR
042400971007     C* IMPOSTA FLAG DI FINE PROGRAMMA
042500971007     C                   MOVEL     '1'           $FINE
042600971007     C* IMPOSTA FLAG DI FINE PROCEDURA
042700971007     C                   MOVEL     '1'           $F12
042800971007     C* PULISCE IL MESSAGGIO
042900971007     C                   MOVEL     *BLANKS       D73MSG
043000971007     C                   ENDSR
043100971007     C*-------------------------------------------------------------------------
043200971007     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
043300971007     C*-------------------------------------------------------------------------
043400971007     C     F24STD        BEGSR
043500971007     C*
043600971007     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
043700971007     C                   EXSR      OUTFUN
043800971007     C*
043900971007     C                   ENDSR
044000971007     C*-------------------------------------------------------------------------
044100971007     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
044200971007     C*          CODA MESSAGGI DEL PGM
044300971007     C*-------------------------------------------------------------------------
044400971007     C     CARMSG        BEGSR
044500971007     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
044600971007     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
044700971007     C     MSGULT        IFEQ      0
044800971007     C                   MOVEL     '1'           *IN40
044900971007     C                   WRITE     TL76DVK
045000971007     C                   MOVEL     '0'           *IN40
045100971007     C                   ENDIF
045200971007     C* INVIA MESSAGGIO ALLA CODA DEL PGM
045300971007     C                   MOVEL     $MSG          DSMSMS
045400971007     C                   CALL      'XMSG'        XMSGPA
045500971007     C     DSMSER        IFEQ      0
045600971007     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
045700971007     C                   ENDIF
045800971007     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
045900971007     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
046000971007     C                   WRITE     TL76DVM                                      *SFL MESS
046100971007     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
046200971007     C                   MOVEL     *BLANKS       $MSG
046300971007     C                   ENDSR
046400971007     C*-------------------------------------------------------------------------
046500971007     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
046600971007     C*-------------------------------------------------------------------------
046700971007     C     OUTMSG        BEGSR
046800971007     C* VISUALIZZAZIONE MESSAGGIO
046900971007     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
047000971007     C                   WRITE     TL76DVK                                      *CONTR MESS
047100971007     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
047200971007     C                   Z-ADD     *ZEROS        MSGULT
047300971007     C                   ENDSR
047400971007     C*-------------------------------------------------------------------------
047500971007     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
047600971007     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
047700971007     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
047800971007     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
047900971007     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
048000971007     C*-------------------------------------------------------------------------
048100971007     C     CARFUN        BEGSR
048200971007     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
048300971007     C     1             DO        31            I                 5 0          --- 1 -->
048400971007     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
048500971007     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
048600971007     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
048700020923     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
048800971007     C     $TGES         IFEQ      'G'                                          *GESTIONE
048900971007     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
049000971007     C                   ELSE                                                   *INTERROG.
049100971007     C                   MOVEL     FUNASI        FAT(I)
049200971007     C                   ENDIF
049300971007     C                   ELSE                                                   <-- 2 --> *FILIALE
049400971007     C     $TGES         IFEQ      'G'                                          *GESTIONE
049500971007     C                   MOVEL     FUNAFG        FAT(I)
049600971007     C                   ELSE                                                   *INTERROG.
049700971007     C                   MOVEL     FUNAFI        FAT(I)
049800971007     C                   ENDIF
049900971007     C                   ENDIF                                                  <-- 2 ---
050000971007     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
050100971007     C                   MOVEL     USER          WEDP              3            *PROFILO
050200971007     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
050300971007     C                   MOVEL     'S'           FAT(I)                         *ABILITO
050400971007     C                   ELSE
050500971007     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
050600971007     C                   ENDIF
050700971007     C                   ENDIF
050800971007     C                   ENDDO                                                  <-- 1 ---
050900971007     C                   ENDSR
051000971007     C*-------------------------------------------------------------------------
051100971007     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
051200971007     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
051300971007     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
051400971007     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
051500971007     C*-------------------------------------------------------------------------
051600971007     C     OUTFUN        BEGSR
051700971007     C     1             DO        8             I                              *PULIZIA FUNZIONI A
051800971007     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
051900971007     C                   ENDDO
052000971007     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
052100971007     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
052200971007     C     1             DO        31            I
052300971007     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
052400971007     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
052500971007     C                   ADD       1             FUNNAT
052600971007     C                   ENDIF
052700971007     C                   ENDDO
052800971007     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
052900971007     C     FUNNAT        IFGT      8
053000971007     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
053100971007     C                   ENDIF
053200971007     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
053300971007     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
053400971007     C                   Z-ADD     8             FUNVIS            5 0
053500971007     C     FAT(23)       IFEQ      'S'
053600971007     C                   SUB       1             FUNVIS
053700971007     C                   ENDIF
053800971007     C     FAT(24)       IFEQ      'S'
053900971007     C                   SUB       1             FUNVIS
054000971007     C                   ENDIF
054100971007     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
054200971007     C                   Z-ADD     *ZEROS        W                 5 0
054300971007     C                   ADD       1             FUNULT
054400971007     C     FUNULT        IFGT      31
054500971007     C                   Z-ADD     1             FUNULT
054600971007     C                   ENDIF
054700971007     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
054800971007     C     FUNULT        DO        31            I                              --- 1 -->
054900971007     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
055000971007     C     W             IFLT      FUNVIS                                       --- 2 -->
055100971007     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
055200971007     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
055300971007     C     FDE(I)        ANDNE     *BLANKS
055400971007     C     I             IFEQ      23                                           *TOLGO F23
055500971007     C     I             OREQ      24                                           *E F24
055600971007     C                   ELSE
055700971007     C                   ADD       1             W
055800971007     C                   MOVEL     FDE(I)        FUV(W)
055900971007     C                   ENDIF
056000971007     C                   ENDIF                                                  <-- 3 ---
056100971007     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
056200971007     C                   Z-ADD     I             FUNULT
056300971007     C                   ENDIF                                                  <-- 2 ---
056400971007     C                   ENDDO                                                  <-- 1 ---
056500971007     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
056600971007     C                   MOVEL     FUV(1)        VFD001
056700971007     C                   MOVEL     FUV(2)        VFD002
056800971007     C                   MOVEL     FUV(3)        VFD003
056900971007     C                   MOVEL     FUV(4)        VFD004
057000971007     C                   MOVEL     FUV(5)        VFD005
057100971007     C                   MOVEL     FUV(6)        VFD006
057200971007     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
057300971007     C     FAT(24)       ANDEQ     'N'
057400971007     C                   MOVEL     FUV(7)        VFD007
057500971007     C                   MOVEL     FUV(8)        VFD008
057600971007     C                   ELSE                                                   <-- 1 -->
057700971007     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
057800971007     C     FAT(24)       ANDEQ     'S'
057900971007     C                   MOVEL     FDE(23)       VFD007
058000971007     C                   MOVEL     FDE(24)       VFD008
058100971007     C                   ELSE                                                   <-- 2 -->
058200971007     C                   MOVEL     FUV(7)        VFD007
058300971007     C     FAT(23)       IFEQ      'S'
058400971007     C                   MOVEL     FDE(23)       VFD008
058500971007     C                   ELSE
058600971007     C                   MOVEL     FDE(24)       VFD008
058700971007     C                   ENDIF
058800971007     C                   ENDIF                                                  <-- 2 ---
058900971007     C                   ENDIF                                                  <-- 1 ---
059000971007     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
059100971007     C                   ENDSR
059200971007     C*-------------------------------------------------------------------------
059300971007     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
059400971007     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
059500971007     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
059600971007     C*                   - ERRORE SE FUNZIONE NON ATTIVA
059700971007     C*-------------------------------------------------------------------------
059800971007     C     CONFUN        BEGSR
059900971007     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
060000971007     C     1             DO        31            I                              --- 1 -->
060100971007     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
060200971007     C* CONTROLLO SE IL TASTO E' ATTIVO
060300971007     C     FAT(I)        IFNE      'S'                                          --- 3 -->
060400971007     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
060500971007     C                   EXSR      CARMSG
060600971007     C                   MOVEL     '1'           $ERR
060700971007     C                   ENDIF                                                  <-- 3 ---
060800971007     C                   ENDIF                                                  <-- 2 ---
060900971007     C                   ENDDO                                                  <-- 1 ---
061000971007     C                   ENDSR
061100971007     O*-------------------------------------------------------------------------
061200971007** -MSG- (MESSAGGI)
061300971007TNTL76R- Tasto funzionale non valido.                                          1
061400971007TNTL76R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
061500971007TNTL76R- Opzione non valida.                                                   3
061600971007TNTL76R- Opzione abilitata ma non gestita. Avvertire CED.                      4
061700971007TNTL76R- Inizio elenco.                                                        5
061800971007TNTL76R- Fine elenco.                                                          6
061900971007TNTL76R- 7
062000971007TNTL76R- 8
062100971007TNTL76R- 9
062200971007TNTL76R- Data errata                                                          10
062300011019TNTL76R- Il record non può essere duplicato: divise incongrunti con la data   11
062400971007TNTL76R- 12
062500971007TNTL76R- 13
062600971007TNTL76R- 14
062700971007TNTL76R- 15
062800971007TNTL76R- 16
062900971007TNTL76R- 17
063000971007TNTL76R- Record già inserito                                                  18
063100971007TNTL76R- Errori in entrata duplica. Non trovato il record da cui copiare      19
063200971007TNTL76R- Il record è stato duplicato                                          20
063300971007** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
063400971007F1    -                   -    NN     NN
063500971007F2    -                   -    NN     NN
063600971007F3    -F3=Fine            -    NN     NN
063700971007F4    -                   -    NN     NN
063800971007F5    -                   -    NN     NN
063900971007F6    -F6=Conferma        -    SN     SN
064000971007F7    -                   -    NN     NN
064100971007F8    -                   -    NN     NN
064200971007F9    -                   -    NN     NN
064300971007F10   -                   -    NN     NN
064400971007F11   -                   -    NN     NN
064500971007F12   -F12=Ritorno        -    SS     SS
064600971007F13   -                   -    NN     NN
064700971007F14   -                   -    NN     NN
064800971007F15   -                   -    NN     NN
064900971007F16   -                   -    NN     NN
065000971007F17   -                   -    NN     NN
065100971007F18   -                   -    NN     NN
065200971007F19   -                   -    NN     NN
065300971007F20   -                   -    NN     NN
065400971007F21   -                   -    NN     NN
065500971007F22   -                   -    NN     NN
065600971007F23   -                   -    NN     NN
065700971007F24   -F24=Altri tasti    -    NN     NN
065800971007ENTER -                   -    SS     SS
065900971007ROLLUP-                   -    NN     NN
066000971007ROLLDN-                   -    NN     NN
066100971007HELP  -                   -    NN     NN
066200971007PRINT -                   -    NN     NN
066300971007BSPACE-                   -    NN     NN
066400971007CLEAR -                   -    NN     NN

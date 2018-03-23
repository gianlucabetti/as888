000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC73R
000300000000      *  Descrizione   :  Gestione competenze di conto economico
000400000000      *                   Programma di immissione, modifica, visualizzazione,...
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  26 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC73D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC73DVM:MSGULT)
001400000000     FECECT01L  UF A E           K DISK
001500000000     FECECD01L  UF A E           K DISK
001501130924     FECnot01L  UF A E           K DISK
001600940721     FTABEL00F  UF   E           K DISK
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D MSG             S             78    DIM(25) CTDATA PERRCD(1)
002100000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002200000000     D*-------------------
002300000000     D* DECODIFICA OPZIONE RICHIAMATA
002400000000     D*-------------------
002500981015     D DOPZ            S             16    DIM(10) CTDATA PERRCD(1)
002600000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002700000000     D FFU             S              6    DIM(31)
002800000000     D FDE             S             19    DIM(31)
002900000000     D FAT             S              1    DIM(31)
003000000000     D* TABELLA DESCRIZIONI A VIDEO
003100000000     D FUV             S             19    DIM(8)
003200000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003300000000     D KEYS            S              1    DIM(40)
003400000000     D* DECODIFICHE DELLE MENSILITA'
003500000000     D MES             S              9    DIM(12) CTDATA PERRCD(1)
003600000000     D*------------------------------------------------------------------------*
003700000000     D* INPUT
003800000000     D*------------------------------------------------------------------------*
003900000000     D*-------------------
004000000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004100000000     D*-------------------
004200000000     D DSFUN         E DS
004300000000     D  FUNRIG                 1     40
004301031212     D*------------------
004302031212     D* DS GESTIONE RICERCA CONTROLLO TABELLE
004303031212     D*------------------
004304031212     D TIBS02DS      E DS
004305031212     D*------------------
004306031212     D* DS LETTURA TABELLA CO1 tipo data x calcolo
004307031212     D*------------------
004308031212     D DCO1          E DS
004400000000     D*-------------------
004500000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004600000000     D*-------------------
004700000000     D DSMSG         E DS
004800000000     D*-------------------
004900020923     D* DS REPERIMENTO DATI UTENTE
005000020923     D*-------------------
005100020923     D TIBS34DS      E DS
005200020923     D DDATIUTE      E DS
005300020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005400000000     D*-------------------
005500000000     D* DS PER GESTIONE TASTI FUNZIONALI
005600000000     D*-------------------
005700000000     D DSKEYS        E DS
005800000000     D  KEYRIG                 1     40
005900000000     D*-------------------
006000000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006100000000     D*-------------------
006200000000     D                SDS
006300000000     D  USER                 254    263
006400000000     D*-------------------
006500000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006600000000     D*-------------------
006700000000     D DEVDS1          DS
006800000000     D* CHIAVE FUNZIONALE BATTUTA
006900000000     D  KEY                  369    369
007000000000     D* POSIZIONE CURSORE (IN HEX)
007100000000     D  CURSOR               370    371
007200000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
007300000000     D  FRTSFL               378    379B 0
007400000000     D*---------------------
007500000000     D* DS ESTERNA DI PROCEDURA
007600000000     D*---------------------
007700000000     D DS60          E DS                  EXTNAME(TREC60DS)
007800000000     D*-------------------
007900000000     D* ARCHITETTURA
008000000000     D*-------------------
008100000000     D KPJBA         E DS
008200940721     D*-------------------
008300980130     D* DS DATI VARI PER SALDI E C/E -NON DI GRUPPO-
008400940721     D*-------------------
008500980130     D DS2K          E DS
008600000000     D*------------------
008700000000     D* CALL XSRDA8 - CONTROLLO DATE
008800000000     D*------------------
008900000000     D WLBDAT          DS                  INZ
009000000000     D  G02DAT                 1      8  0
009100000000     D  G02INV                 9     16  0
009200000000     D  G02ERR                17     17
009300000000     D  G02TGI                18     22  0
009400000000     C*------------------------------------------------------------------------*
009500000000     C* MAIN LINE
009600000000     C*------------------------------------------------------------------------*
009700000000     C*
009800000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
009900000000IF  1C     $FINE         IFEQ      '1'
010000000000     C*
010100000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
010200000000     C                   MOVEL     '1'           D60ERR
010300000000     C*
010400000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
010500000000X   1C                   ELSE
010600000000     C*
010700000000     C* CICLO FINO A CHE:
010800000000DO  2C     $FINE         DOWEQ     '0'
010900000000     C                   MOVEL     '0'           $ERR
011000000000     C*
011100000000     C* CONTROLLA GESTIONE VIDEO
011200000000IF  3C     $GES          IFEQ      'V1'
011300000000     C                   EXSR      GESV1
011400000000E   3C                   ENDIF
011500000000     C*
011600000000E   2C                   ENDDO
011700000000E   1C                   ENDIF
011800000000     C*
011900000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
012000000000     C     $F03          IFEQ      '1'
012100000000     C                   MOVEL     '1'           D60F03
012200000000E   1C                   ENDIF
012300000000     C* FINE PGM
012400000000     C                   MOVEL     DS60          KPJBU
012500000000     C                   SETON                                        LR
012600000000     C*------------------------------------------------------------------------*
012700000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
012800000000     C*------------------------------------------------------------------------*
012900000000     C     CARV1         BEGSR
013000000000     C*
013100000000     C                   MOVEL     'V1'          $GES
013200000000     C*
013300000000     C                   EXSR      CARFUN
013400000000     C                   EXSR      OUTFUN
013500000000     C*
013600000000     C                   WRITE     EC73DVT
013700000000     C                   WRITE     EC73DVF
013800000000     C*
013900000000     C                   ENDSR
014000000000     C*------------------------------------------------------------------------*
014100000000     C* GESV1  - GESTIONE PRIMO VIDEO
014200000000     C*------------------------------------------------------------------------*
014300000000     C     GESV1         BEGSR
014400000000     C*
014500000000IF  1C     $TESVD        IFEQ      '1'
014600000000     C                   EXSR      CARV1
014700000000E   1C                   ENDIF
014800000000     C*
014900000000IF  1C     MSGULT        IFNE      *ZEROS
015000000000     C                   EXSR      OUTMSG
015100000000E   1C                   ENDIF
015200000000     C*
015300000000     C                   EXFMT     EC73DV1
015400000000     C*
015500000000     C                   EXSR      CONFUN
015600000000     C*
015700000000IF  1C     $ERR          IFEQ      '0'
015800000000SL  2C                   SELECT
015900000000     C     KEY           WHENEQ    ENTER
016000000000     C                   EXSR      ENTV1
016100000000     C     KEY           WHENEQ    F6
016200000000     C                   EXSR      F06GES
016300000000     C     KEY           WHENEQ    F11
016400000000     C                   EXSR      F11GES
016500000000     C                   OTHER
016600000000     C                   EXSR      FUNSTD
016700000000E   2C                   ENDSL
016800000000E   1C                   ENDIF
016900000000     C*
017000000000     C                   ENDSR
017100000000     C*------------------------------------------------------------------------*
017200000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
017300000000     C*------------------------------------------------------------------------*
017400000000     C     ENTV1         BEGSR
017500000000     C*
017600000000     C* CONTROLLA I CAMPI IMMESSI
017700000000     C                   EXSR      DECODI
017800000000     C*
017900000000     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
018000000000IF  1C     D60OP1        IFEQ      'O05'
018100000000     C                   MOVEL     '1'           $FINE
018200000000E   1C                   ENDIF
018300000000     C*
018400000000     C                   ENDSR
018500000000     C*------------------------------------------------------------------------*
018600000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
018700000000     C*------------------------------------------------------------------------*
018800000000     C     F06GES        BEGSR
018900000000     C*
019000000000     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
019100000000IF  1C     D60OP1        IFEQ      'F06'
019200000000     C                   EXSR      INSERI
019300000000E   1C                   ENDIF
019400000000IF  1C     D60OP1        IFEQ      'O02'
019500000000     C                   EXSR      MODIFI
019600000000E   1C                   ENDIF
019700000000IF  1C     D60OP1        IFEQ      'O04'
019800000000     C                   EXSR      ANNULL
019900000000E   1C                   ENDIF
020000000000IF  1C     D60OP1        IFEQ      'O07'
020100000000     C                   EXSR      RIPRIS
020200000000E   1C                   ENDIF
020300000000     C*
020400000000     C                   ENDSR
020500000000     C*------------------------------------------------------------------------*
020600000000     C* F11GES -GEST.TASTO FUNZ. F11 ===> CONTI DI CONTABILITA'
020700000000     C*------------------------------------------------------------------------*
020800000000     C     F11GES        BEGSR
020900000000     C*
021000000000     C* LANCIO LA GESTIONE DEI CONTI DI CONTABILITA'
021100000000     C                   Z-ADD     V1CNBC        WNBC
021200000000     C                   Z-ADD     V1CCMP        WCMP
021300000000     C                   Z-ADD     V1CDDC        WDDC
021400000000     C                   MOVEL     01            WDDC
021500000000     C*
021600000000     C                   MOVEL     D60OP0        WOP0              3
021700000000     C                   MOVEL     D60OP1        WOP1              3
021800000000     C                   MOVEL     'TREC75R'     WPGM              8
021900000000     C                   EXSR      LANPGM
022000000000     C*
022100000000     C* POTREBBE ESSERE CAMBIATO IL NR.BLOCCO
022200000000     C                   Z-ADD     D60NBC        V1CNBC
022300000000     C*
022400000000     C                   ENDSR
022500000000     C*------------------------------------------------------------------------*
022600000000     C* INSERI - FUNZIONE DI INSERIMENTO
022700000000     C*------------------------------------------------------------------------*
022800000000     C     INSERI        BEGSR
022900000000     C*
023000000000     C* CONTROLLA I CAMPI IMMESSI
023100000000     C                   EXSR      DECODI
023200000000IF  1C     $ERR          IFEQ      '0'
023300940721     C*
023400940721     C* PRIMA DI INSERIRE DEVO:
023500940721     C*   1.  DUPLICARE TUTTE LE COMPETENZE DEL BLOCCO CON NUOVO BLOCCO
023600940721     C*   2.  INCREMENTARE NUMERO BLOCCO
023700940721     C* TRANNE CHE IL BLOCCO NON SIA STATO MAI USATO PER SCRIVERE IL C/ECONOMICO
023800940721IF  2C     $FLAU         IFEQ      'S'
023900940721     C                   EXSR      DUPNBC
024000940721E   2C                   ENDIF
024100000000     C*
024200000000     C                   Z-ADD     V1CNBC        ECTNBC
024300000000     C                   Z-ADD     V1CCMP        ECTCMP
024400000000     C                   Z-ADD     V1CDDC        G02DAT
024500000000     C                   MOVEL     01            G02DAT
024600000000     C                   Z-ADD     *ZEROS        G02INV
024700000000     C                   MOVEL     '0'           G02ERR
024800000000     C                   CALL      'XSRDA8'
024900000000     C                   PARM                    WLBDAT
025000000000     C                   Z-ADD     G02INV        ECTDDC
025100000000     C                   MOVE      00            ECTDDC
025200000000     C                   Z-ADD     V1CDSC        G02DAT
025300000000     C                   MOVEL     01            G02DAT
025400000000     C                   Z-ADD     *ZEROS        G02INV
025500000000     C                   MOVEL     '0'           G02ERR
025600000000     C                   CALL      'XSRDA8'
025700000000     C                   PARM                    WLBDAT
025800000000     C                   Z-ADD     G02INV        ECTDSC
025900000000     C                   MOVE      00            ECTDSC
026000000000     C                   MOVEL     V1CCMD        ECTCMD
026100000000     C                   MOVEL     V1CFCR        ECTFCR
026200020130     C                   MOVEL     V1CFDR        ECTFDR
026300980312     C                   MOVEL     V1CRIC        ECTRIC
026400000000     C                   MOVEL     V1CMCE        ECTMCE
026500000000     C*
026600000000     C                   MOVEL     *BLANKS       ECTATB
026700000000     C                   MOVEL     *BLANKS       ECTFTR
026800000000     C                   Z-ADD     *ZEROS        ECTDTR
026900000000     C*
027000000000     C                   WRITE     ECECT
027100000000     C*
027200000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
027300000000     C                   MOVEL     '1'           $FINE
027400000000     C*
027500000000     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
027600000000     C                   MOVEL     MSG(17)       D60MSG
027700000000     C*
027800000000E   1C                   ENDIF
027900000000     C*
028000000000     C                   ENDSR
028100000000     C*------------------------------------------------------------------------*
028200000000     C* MODIFI - FUNZIONE DI MODIFICA
028300000000     C*------------------------------------------------------------------------*
028400000000     C     MODIFI        BEGSR
028500000000     C*
028600000000     C* CONTROLLA I CAMPI IMMESSI
028700000000     C                   EXSR      DECODI
028800000000IF  1C     $ERR          IFEQ      '0'
028900000000     C*
029000000000     C* PRIMA DI AGGIORNARE DEVO:
029100000000     C*   1.  DUPLICARE TUTTE LE COMPETENZE DEL BLOCCO CON NUOVO BLOCCO
029200000000     C*   2.  INCREMENTARE NUMERO BLOCCO
029300940721     C* TRANNE CHE IL BLOCCO NON SIA STATO MAI USATO PER SCRIVERE IL C/ECONOMICO
029400940721IF  2C     $FLAU         IFEQ      'S'
029500940721     C                   EXSR      DUPNBC
029600940721E   2C                   ENDIF
029700000000     C*
029800000000     C* RILEGGO LA COMPETENZA PER SPOSIZIONAMENTO NEI CONTROLLI
029900940721     C                   Z-ADD     V1CNBC        KCTNBC
030000000000     C                   Z-ADD     D60CMP        KCTCMP
030100000000     C                   Z-ADD     DDCKCT        KCTDDC
030200000000     C     KEYECT        CHAIN     ECECT01L                           99
030300000000IF  2C     *IN99         IFEQ      *OFF
030400000000     C*
030500000000     C*******              Z-ADDV1CNBC    ECTNBC           *NR.BLOCCO
030600000000     C*******              Z-ADDV1CCMP    ECTCMP           *CODICE COMPETENZA
030700000000     C*******              Z-ADDV1CDDC    G02DAT           *DECORRENZA (G/M/A)
030800000000     C*******              MOVEL01        G02DAT           *AGGIUNGO I GIORNI
030900000000     C*******              Z-ADD*ZEROS    G02INV
031000000000     C*******              MOVEL'0'       G02ERR
031100000000     C*******              CALL 'XSRDA8'
031200000000     C*******              PARM           WLBDAT
031300000000     C*******              Z-ADDG02INV    ECTDDC
031400000000     C*******              MOVE 00        ECTDDC           *TOLGO I GIORNI
031500000000     C                   Z-ADD     V1CDSC        G02DAT
031600000000     C                   MOVEL     01            G02DAT
031700000000     C                   Z-ADD     *ZEROS        G02INV
031800000000     C                   MOVEL     '0'           G02ERR
031900000000     C                   CALL      'XSRDA8'
032000000000     C                   PARM                    WLBDAT
032100000000     C                   Z-ADD     G02INV        ECTDSC
032200000000     C                   MOVE      00            ECTDSC
032300000000     C                   MOVEL     V1CCMD        ECTCMD
032400000000     C                   MOVEL     V1CFCR        ECTFCR
032500020130     C                   MOVEL     V1CFDR        ECTFDR
032600980312     C                   MOVEL     V1CRIC        ECTRIC
032700000000     C                   MOVEL     V1CMCE        ECTMCE
032800000000     C*
032900000000     C                   MOVEL     *BLANKS       ECTATB
033000000000     C                   MOVEL     *BLANKS       ECTFTR
033100970903     C*---                 Z-ADD*ZEROS    ECTDTR           *DATA TRASMISSIONE
033200000000     C*
033300000000     C                   UPDATE    ECECT
033400000000     C*
033500000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
033600000000     C                   MOVEL     '1'           $FINE
033700000000     C*
033800000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
033900000000     C                   MOVEL     MSG(18)       D60MSG
034000000000     C*
034100000000E   2C                   ENDIF
034200000000E   1C                   ENDIF
034300000000     C*
034400000000     C                   ENDSR
034500000000     C*------------------------------------------------------------------------*
034600000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
034700000000     C*------------------------------------------------------------------------*
034800000000     C     ANNULL        BEGSR
034900000000     C*
035000000000     C* CONTROLLA I CAMPI IMMESSI
035100000000     C                   EXSR      DECODI
035200000000IF  1C     $ERR          IFEQ      '0'
035300000000     C*
035400000000     C* PRIMA DI AGGIORNARE DEVO:
035500000000     C*   1.  DUPLICARE TUTTE LE COMPETENZE DEL BLOCCO CON NUOVO BLOCCO
035600000000     C*   2.  INCREMENTARE NUMERO BLOCCO
035700940721     C* TRANNE CHE IL BLOCCO NON SIA STATO MAI USATO PER SCRIVERE IL C/ECONOMICO
035800940721IF  2C     $FLAU         IFEQ      'S'
035900940721     C                   EXSR      DUPNBC
036000940721E   2C                   ENDIF
036100000000     C*
036200000000     C* RILEGGO LA COMPETENZA PER SPOSIZIONAMENTO NEI CONTROLLI
036300000000     C                   Z-ADD     V1CNBC        KCTNBC
036400000000     C                   Z-ADD     V1CCMP        KCTCMP
036500000000     C                   Z-ADD     DDCKCT        KCTDDC
036600000000     C     KEYECT        CHAIN     ECECT01L                           99
036700000000IF  2C     *IN99         IFEQ      *OFF
036800000000     C*
036900000000     C*--------------------
037000000000     C* ANNULLO VOCE
037100000000     C*--------------------
037200970903     C* SE NON E' STATO MAI TRASMESSO, CANCELLA FISICAMENTE IL RECORD
037300970903IF  3C     ECTDTR        IFGT      *ZEROS
037400000000     C                   MOVEL     'A'           ECTATB
037500000000     C                   MOVEL     *BLANKS       ECTFTR
037600970903     C*---                 Z-ADD*ZEROS    ECTDTR           *DATA TRASMISSIONE
037700000000     C                   UPDATE    ECECT
037800970903X   3C                   ELSE
037900970903     C                   DELETE    ECECT
038000970903E   3C                   ENDIF
038100000000     C*--------------------
038200000000     C* ANNULLO TARIFFE VOCE
038300000000     C*--------------------
038400000000     C                   Z-ADD     V1CNBC        KCDNBC
038500000000     C                   Z-ADD     V1CCMP        KCDCMP
038600000000     C     KEYECD        SETLL     ECECD01L
038700000000     C     KEYECD        READE     ECECD01L                               99
038800000000DO  3C     *IN99         DOWEQ     *OFF
038900000000     C*
039000000000IF  4C     ECDDDO        IFGE      ECTDDC
039100000000     C     ECDDSO        ANDLE     ECTDSC
039200000000     C     ECDATB        ANDNE     'A'
039300970903IF  5C     ECDDTR        IFGT      *ZEROS
039400000000     C                   MOVEL     'A'           ECDATB
039500000000     C                   MOVEL     *BLANKS       ECDFTR
039600970903     C*---                 Z-ADD*ZEROS    ECDDTR           *DATA TRASMISSIONE
039700000000     C                   UPDATE    ECECD
039800970903X   5C                   ELSE
039900970903     C                   DELETE    ECECD
040000970903E   5C                   ENDIF
040100970903E   4C                   ENDIF
040200000000     C     KEYECD        READE     ECECD01L                               99
040300000000E   3C                   ENDDO
040400000000     C*
040500000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
040600000000     C                   MOVEL     '1'           $FINE
040700000000     C*
040800000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
040900000000     C                   MOVEL     MSG(19)       D60MSG
041000000000     C*
041100000000E   2C                   ENDIF
041200000000E   1C                   ENDIF
041300000000     C*
041400000000     C                   ENDSR
041500000000     C*------------------------------------------------------------------------*
041600000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
041700000000     C*------------------------------------------------------------------------*
041800000000     C     RIPRIS        BEGSR
041900000000     C*
042000000000     C* CONTROLLA I CAMPI IMMESSI
042100000000     C                   EXSR      DECODI
042200000000IF  1C     $ERR          IFEQ      '0'
042300000000     C*
042400000000     C* PRIMA DI AGGIORNARE DEVO:
042500000000     C*   1.  DUPLICARE TUTTE LE COMPETENZE DEL BLOCCO CON NUOVO BLOCCO
042600000000     C*   2.  INCREMENTARE NUMERO BLOCCO
042700940721IF  2C     $FLAU         IFEQ      'S'
042800940721     C                   EXSR      DUPNBC
042900940721E   2C                   ENDIF
043000000000     C*
043100000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
043200000000     C                   Z-ADD     V1CNBC        KCTNBC
043300000000     C                   Z-ADD     V1CCMP        KCTCMP
043400000000     C                   Z-ADD     DDCKCT        KCTDDC
043500000000     C     KEYECT        CHAIN     ECECT01L                           99
043600000000IF  2C     *IN99         IFEQ      *OFF
043700000000     C*
043800000000     C*--------------------
043900000000     C* RIPRISTINO VOCE
044000000000     C*--------------------
044100000000     C                   MOVEL     *BLANKS       ECTATB
044200000000     C                   MOVEL     *BLANKS       ECTFTR
044300970903     C*---                 Z-ADD*ZEROS    ECTDTR           *DATA TRASMISSIONE
044400000000     C*
044500000000     C                   UPDATE    ECECT
044600000000     C*
044700000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
044800000000     C                   MOVEL     '1'           $FINE
044900000000     C*
045000000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
045100000000     C                   MOVEL     MSG(20)       D60MSG
045200000000     C*
045300000000E   2C                   ENDIF
045400000000E   1C                   ENDIF
045500000000     C*
045600000000     C                   ENDSR
045700000000     C*------------------------------------------------------------------------*
045800000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
045900000000     C*------------------------------------------------------------------------*
046000000000     C     DECODI        BEGSR
046100000000     C*
046200000000     C* REIMPOSTO INDICATORI DI ERRORE
046300000000     C                   SETOFF                                       606162
046400000000     C                   SETOFF                                       636465
046500020130     C                   SETOFF                                       6667
046600000000     C*--------------------
046700000000     C* CODICE VOCE
046800000000     C*--------------------
046900000000IF  1C     V1CCMP        IFEQ      *ZEROS
047000951207     C                   MOVEL     '1'           $ERR
047100000000     C                   MOVEL     MSG(9)        $MSG
047200000000     C                   EXSR      CARMSG
047300000000     C                   SETON                                        60
047400000000E   1C                   ENDIF
047500000000     C*--------------------
047600000000     C* DATA DECORRENZA
047700000000     C*--------------------
047800000000IF  1C     $ERR          IFEQ      '0'
047900000000     C                   MOVEL     *BLANKS       V1DDDC
048000000000     C                   Z-ADD     V1CDDC        $DAT              8 0
048100000000     C                   MOVEL     01            $DAT
048200000000     C                   EXSR      CHKDAT
048300000000     C                   Z-ADD     $DAT          V1CDDC
048400000000     C                   Z-ADD     G02INV        DDCV1C            8 0
048500000000IF  2C     $ERR          IFEQ      '1'
048600000000     C                   EXSR      CARMSG
048700000000     C                   SETON                                        61
048800000000     C                   ELSE
048900000000     C                   MOVEL     V1CDDC        $A                2 0
049000000000     C                   MOVEL     MES($A)       V1DDDC
049100000000E   2C                   ENDIF
049200000000E   1C                   ENDIF
049300951207     C*--------------------
049400951207     C* INSERIMENTO -> COMPETENZA INSESISTENTE
049500951207     C*--------------------
049600951207IF  1C     $ERR          IFEQ      '0'
049700951207IF  2C     D60OP1        IFEQ      'F06'
049800951207     C                   Z-ADD     V1CNBC        KCTNBC
049900951207     C                   Z-ADD     V1CCMP        KCTCMP
050000951207     C                   Z-ADD     DDCKCT        KCTDDC
050100951207     C     KEYECT        SETLL     ECECT01L                               99
050200951207IF  3C     *IN99         IFEQ      *ON
050300951207     C                   MOVEL     '1'           $ERR
050400951207     C                   MOVEL     MSG(13)       $MSG
050500951207     C                   EXSR      CARMSG
050600951207     C                   SETON                                        6061
050700951207E   3C                   ENDIF
050800951207E   2C                   ENDIF
050900951207E   1C                   ENDIF
051000000000     C*--------------------
051100000000     C* DATA SCADENZA
051200000000     C*--------------------
051300000000IF  1C     $ERR          IFEQ      '0'
051400000000     C                   MOVEL     *BLANKS       V1DDSC
051500000000     C                   Z-ADD     V1CDSC        $DAT
051600000000     C                   MOVEL     01            $DAT
051700000000     C                   EXSR      CHKDAT
051800000000     C                   Z-ADD     $DAT          V1CDSC
051900000000     C                   Z-ADD     G02INV        DSCV1C            8 0
052000000000IF  2C     $ERR          IFEQ      '1'
052100000000     C                   EXSR      CARMSG
052200000000     C                   SETON                                        62
052300000000     C                   ELSE
052400000000     C                   MOVEL     V1CDSC        $A
052500951207     C                   MOVEL     MES($A)       V1DDSC
052600000000E   2C                   ENDIF
052700000000     C*
052800000000     C* MAGGIORE DELLA DECORRENZA
052900000000IF  2C     $ERR          IFEQ      '0'
053000000000IF  3C     DSCV1C        IFLT      DDCV1C
053100000000     C                   MOVEL     '1'           $ERR
053200000000     C                   MOVEL     MSG(23)       $MSG
053300000000     C                   EXSR      CARMSG
053400000000     C                   SETON                                        6162
053500000000E   3C                   ENDIF
053600000000E   2C                   ENDIF
053700000000E   1C                   ENDIF
053800951207     C*--------------------
053900000000     C* VALIDITA' VOCE
054000000000     C*--------------------
054100000000IF  1C     $ERR          IFEQ      '0'
054200000000     C*
054300000000     C                   Z-ADD     V1CNBC        KCTNBC
054400000000     C                   Z-ADD     V1CCMP        KCTCMP
054500000000     C     KE2ECT        SETLL     ECECT01L
054600000000     C     KE2ECT        READE(N)  ECECT01L                               99
054700000000DO  2C     *IN99         DOWEQ     *OFF
054800951207IF  3C     ECTATB        IFEQ      *BLANKS
054900000000     C*
055000000000     C* SE SONO IN MODIFI/ANNULL/RIPRIS DAL CONTROLLO ESCLUDO LA VOCE IN LINEA
055100000000IF  4C     D60OP1        IFNE      'F06'
055200000000     C     DDCKCT        ANDEQ     ECTDDC
055300000000X   4C                   ELSE
055400000000     C*
055500000000IF  5C     DDCV1C        IFLE      ECTDSC
055600000000IF  6C     DSCV1C        IFLT      ECTDDC
055700000000X   6C                   ELSE
055800000000     C                   MOVEL     '1'           $ERR
055900000000     C                   MOVEL     MSG(24)       $MSG
056000000000     C                   EXSR      CARMSG
056100000000     C                   SETON                                        616299
056200000000E   6C                   ENDIF
056300000000E   5C                   ENDIF
056400000000E   4C                   ENDIF
056500951207E   3C                   ENDIF
056600000000     C  N99KE2ECT        READE(N)  ECECT01L                               99
056700000000E   2C                   ENDDO
056800000000     C*
056900000000E   1C                   ENDIF
057000000000     C*--------------------
057100000000     C* DESCRIZIONE VOCE
057200000000     C*--------------------
057300000000IF  1C     $ERR          IFEQ      '0'
057400000000IF  2C     V1CCMD        IFEQ      *BLANKS
057500000000     C                   MOVEL     '1'           $ERR
057600000000     C                   MOVEL     MSG(10)       $MSG
057700000000     C                   EXSR      CARMSG
057800000000     C                   SETON                                        63
057900000000E   2C                   ENDIF
058000000000E   1C                   ENDIF
058100000000     C*--------------------
058200000000     C* TIPO COMPETENZA
058300000000     C*--------------------
058400000000IF  1C     $ERR          IFEQ      '0'
058500000000IF  2C     V1CFCR        IFEQ      'C'
058600000000     C     V1CFCR        OREQ      'R'
058700000000X   2C                   ELSE
058800000000     C                   MOVEL     '1'           $ERR
058900980312     C                   MOVEL     MSG(11)       $MSG
059000000000     C                   EXSR      CARMSG
059100000000     C                   SETON                                        64
059200000000E   2C                   ENDIF
059300000000E   1C                   ENDIF
059400000000     C*--------------------
059500000000     C* DA MOVIMENTARE IN CONTO ECONOMICO
059600000000     C*--------------------
059700000000IF  1C     $ERR          IFEQ      '0'
059800000000IF  2C     V1CMCE        IFEQ      'S'
059900000000     C     V1CMCE        OREQ      'N'
060000000000     C     V1CMCE        OREQ      *BLANKS
060100000000X   2C                   ELSE
060200000000     C                   MOVEL     '1'           $ERR
060300000000     C                   MOVEL     MSG(12)       $MSG
060400000000     C                   EXSR      CARMSG
060500000000     C                   SETON                                        65
060600000000E   2C                   ENDIF
060700000000IF  2C     V1CMCE        IFEQ      *BLANKS
060800000000     C                   MOVE      'N'           V1CMCE
060900000000E   2C                   ENDIF
061000000000E   1C                   ENDIF
061100980312     C*--------------------
061200980312     C* RIFERIMENTO CENTRO DI COSTO
061300980312     C*--------------------
061400980312IF  1C     $ERR          IFEQ      '0'
061500980312IF  2C     V1CRIC        IFNE      *BLANKS
061600980312     C     V1CRIC        ANDNE     *ZEROS
061700980312     C                   MOVEL     '2T'          §COD
061800980312     C                   MOVEL     *BLANKS       §KEY
061900980312     C                   MOVEL     V1CRIC        §KEY
062000980312     C                   EXSR      CHKTAB
062100980312     C                   MOVEL     §KEY          V1CRIC
062200980312     C                   MOVEL     §DES          V1CRID
062300980312IF  3C     $ERR          IFEQ      '1'
062400980312     C                   MOVEL     MSG(14)       $MSG
062500980312     C                   EXSR      CARMSG
062600980312     C                   SETON                                        66
062700980312E   3C                   ENDIF
062800980312E   2C                   ENDIF
062900980312E   1C                   ENDIF
063000020130     C*--------------------
063100020130     C* FLAG DATA DI RIFERIMENTO
063200020130     C*--------------------
063300020130IF  1C     $ERR          IFEQ      '0'
063301031212     c* lascio il controllo di '0' o '1' x le anagrafiche obsolete
063302031212     c                   if        dscv1c < 20040101
063400020130IF  2C     V1CFDR        IFEQ      '0'
063500020130     C     V1CFDR        OREQ      '1'
063501031212     c                   if        v1cfdr = '0'
063502031212     c                   eval      v1dfdr = 'MM spedizione'
063503031212     c                   else
063504031212     c                   eval      v1dfdr = 'MM successivo'
063505031212     c                   end
063600020130X   2C                   ELSE
063700020130     C                   MOVEL     '1'           $ERR
063800020130     C                   MOVEL     MSG(15)       $MSG
063900020130     C                   EXSR      CARMSG
064000020130     C                   SETON                                        67
064100020130E   2C                   ENDIF
064101031212     c                   else
064102031212     c* dal 2004 il flag della data sarà controllato con la tabella CO1
064103031212     C                   CLEAR                   TIBS02DS
064104031212     C                   CLEAR                   DCO1
064105031212     C*
064106031212     C     '?'           SCAN      V1Cfdr                                 30
064107031212     C*  Ricerca
064108031212     C                   EVAL      T02TLA = 'L'
064109031212     C                   IF        *IN30 = *ON
064110031212     C                   MOVEL     'R'           T02MOD
064111031212     C                   ELSE
064112031212     C                   MOVEL     'C'           T02MOD
064113031212     C                   MOVEL(P)  V1Cfdr        T02KE1
064114031212     C                   ENDIF
064115031212     C                   MOVEL     'CO1'         T02COD
064116031212     C                   MOVEL     KNSIF         T02SIF
064117031212     C                   CALL      'TIBS02R'
064118031212     C                   PARM                    KPJBA
064119031212     C                   PARM                    TIBS02DS
064120031212     C
064121031212     C*
064122031212     C                   IF        T02ERR = *BLANKS
064123031212     C                   MOVEL     T02KE1        V1Cfdr
064124031212     C                   MOVEL     T02UNI        DCO1
064125031212     C                   MOVEL(p)  §CO1DES       V1Dfdr
064126031212     C                   ELSE
064127031212     C                   MOVEL     '1'           $ERR
064128031212     C                   SETON                                        67
064129031212     C                   MOVEL     MSG(15)       $MSG
064130031212     C                   EXSR      CARMSG
064131031212     C                   ENDIF
064132031212     C                   ENDIF
064133031212     c
064200020130E   1C                   ENDIF
064300000000     C*
064400000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
064500000000     C* CURSORE SUL CAMPO DEL CONTROL
064600000000IF  1C     $ERR          IFEQ      '1'
064700000000     C                   MOVEL     '0'           *IN43
064800000000X   1C                   ELSE
064900000000     C                   MOVEL     '1'           *IN43
065000000000E   1C                   ENDIF
065100000000     C*
065200000000     C                   ENDSR
065300980312     C*------------------------------------------------------------------------*
065400980312     C* CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
065500980312     C*          INPUT --> §COD: CODICE TABELLA
065600980312     C*                    $KEY: CODICE ELEMENTO
065700980312     C*------------------------------------------------------------------------*
065800980312     C     CHKTAB        BEGSR
065900980312     C*
066000980312     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
066100980312     C     '?'           SCAN      §KEY                                   99
066200980312     C     *IN99         IFEQ      *ON
066300020923     C                   MOVEL     1             §KUT
066400980312     C                   MOVE      *BLANKS       §DES
066500980312     C                   CALL      'X§TABER'
066600980312     C                   PARM                    §KUT              1 0
066700980312     C                   PARM                    §COD              2
066800980312     C                   PARM                    §KEY              8
066900980312     C                   PARM                    §DES             30
067000980312     C                   MOVEL     '1'           $PUNTO            1
067100980312     C*
067200980312     C                   ENDIF
067300980312     C*
067400980312     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
067500020923     C                   MOVEL     1             KTBKUT
067600980312     C                   MOVEL     §COD          KTBCOD
067700980312     C                   MOVE      *BLANKS       KTBKEY
067800980312     C                   MOVEL     §KEY          KTBKEY
067900980312     C     KEYTAB        CHAIN     TABEL00F                           99
068000980312     C     *IN99         IFEQ      '1'
068100980312     C     TBLFLG        ORNE      *BLANKS
068200980312     C                   MOVEL     '1'           $ERR
068300980312     C                   MOVEL     *ALL'*'       §DES
068400980312     C                   ELSE
068500980312     C                   MOVEL     TBLUNI        §DES
068600980312     C                   ENDIF
068700980312     C*
068800980312     C                   ENDSR
068900000000     C*------------------------------------------------------------------------*
069000000000     C* CHKDAT - CONTROLLA LA DATA
069100000000     C*          INPUT --> $DAT:  DATA
069200000000     C*------------------------------------------------------------------------*
069300000000     C     CHKDAT        BEGSR
069400000000     C*
069500000000     C* OBBLIGATORIA
069600000000IF  1C     $DAT          IFEQ      *ZEROS
069700000000     C                   MOVEL     '1'           $ERR
069800000000     C                   MOVEL     MSG(8)        $MSG
069900000000X   1C                   ELSE
070000000000     C* CORRETTA
070100000000     C                   Z-ADD     $DAT          G02DAT
070200000000     C                   Z-ADD     *ZEROS        G02INV
070300000000     C                   MOVEL     '0'           G02ERR
070400000000     C                   CALL      'XSRDA8'
070500000000     C                   PARM                    WLBDAT
070600000000     C*
070700000000IF  2C     G02ERR        IFEQ      '1'
070800000000     C                   MOVEL     '1'           $ERR
070900000000     C                   MOVEL     MSG(8)        $MSG
071000000000X   2C                   ELSE
071100000000     C                   Z-ADD     G02DAT        $DAT
071200000000E   2C                   ENDIF
071300000000E   1C                   ENDIF
071400000000     C*
071500000000     C                   ENDSR
071600000000     C*------------------------------------------------------------------------*
071700000000     C*  DUPNBC - DUPLICAZIONE DI TUTTI I RECORD DI ECECT CHE HANNO LO STESSO
071800000000     C*           NUMERO BLOCCO.
071900000000     C*           DUPLICAZIONE DI TUTTI I RECORD DI ECECD CHE HANNO LO STESSO
072000000000     C*           NUMERO BLOCCO.
072100000000     C*           INCREMENTO DEL NUMERO BLOCCO.
072200000000     C*------------------------------------------------------------------------*
072300000000     C     DUPNBC        BEGSR
072400940721     C*
072500940721     C* INCREMENTO IL NUMERO BLOCCO SE L'ULTIMO E'GIA STATO USATO PER AGGIORNARE
072600940721     C* IL CONTO ECONOMICO, ALTRIMENTI USO LO STESSO NUMERO BLOCCO
072700940721     C     *LIKE         DEFINE    ECTNBC        $NBC
072800940721     C     V1CNBC        ADD       1             $NBC
072900940721     C*
073000940721     C* AGGIORNA LA TABELLA
073100020923     C                   MOVEL     1             KTBKUT
073200980130     C                   MOVEL     '2K'          KTBCOD
073300940721     C                   MOVE      *BLANKS       KTBKEY
073400940721     C     KEYTAB        SETLL     TABEL00F
073500940721     C     KEYTAB        READE     TABEL00F                               99
073600980130     C                   Z-ADD     $NBC          §2KNBC
073700980130     C                   MOVEL     'N'           §2KUBC
073800980130     C                   MOVEL     DS2K          TBLUNI
073900940721     C                   MOVEL     *BLANKS       TBLFTR
074000940721     C                   Z-ADD     *ZEROS        TBLDTR
074100940721     C                   UPDATE    TABEL
074200000000     C*
074300000000     C* DUPLICO IL FILE DELLE TESTATE (  E C E C T 0 0 F  )
074301130924     c                   z-add     v1cnbc        blcprg
074400000000     C     V1CNBC        SETLL     ECECT01L
074500000000     C     V1CNBC        READE     ECECT01L                               99
074600000000     C     *IN99         DOWEQ     *OFF
074700000000     C                   Z-ADD     $NBC          ECTNBC
074800000000     C                   MOVE      *BLANKS       ECTFTR
074900000000     C                   Z-ADD     *ZERO         ECTDTR
075000000000     C                   WRITE     ECECT
075001130924      *duplica note se presenti
075003130924     c                   exsr      dupnot
075100000000     C     V1CNBC        READE     ECECT01L                               99
075200000000     C                   ENDDO
075300000000     C*
075400000000     C* DUPLICO IL FILE DEL DETTAGLIO (  E C E C D 0 0 F  )
075500000000     C     V1CNBC        SETLL     ECECD01L
075600000000     C     V1CNBC        READE     ECECD01L                               99
075700000000     C     *IN99         DOWEQ     *OFF
075800000000     C                   Z-ADD     $NBC          ECDNBC
075900000000     C                   MOVE      *BLANKS       ECDFTR
076000000000     C                   Z-ADD     *ZERO         ECDDTR
076100000000     C                   WRITE     ECECD
076200000000     C     V1CNBC        READE     ECECD01L                               99
076300000000     C                   ENDDO
076400000000     C*
076500000000     C                   Z-ADD     $NBC          V1CNBC
076600940721     C                   Z-ADD     $NBC          D60NBC
076700000000     C*
076800000000     C                   ENDSR
076900000000     C*------------------------------------------------------------------------*
077000000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
077100000000     C*------------------------------------------------------------------------*
077200000000     C     RECVID        BEGSR
077300000000     C*
077400000000     C                   Z-ADD     D60NBC        V1CNBC
077500000000     C                   Z-ADD     D60CMP        V1CCMP
077600000000     C                   Z-ADD     D60DDC        V1CDDC
077700000000     C                   Z-ADD     ECTDSC        G02INV
077800000000     C                   Z-ADD     *ZEROS        G02DAT
077900000000     C                   MOVEL     '3'           G02ERR
078000000000     C                   CALL      'XSRDA8'
078100000000     C                   PARM                    WLBDAT
078200000000     C                   Z-ADD     G02DAT        V1CDSC
078300000000     C                   MOVEL     ECTCMD        V1CCMD
078400000000     C                   MOVEL     ECTFCR        V1CFCR
078500020130     C                   MOVEL     ECTFDR        V1CFDR
078600980312     C                   MOVEL     ECTRIC        V1CRIC
078700000000     C                   MOVEL     ECTMCE        V1CMCE
078800981015IF  1C     ECTATB        IFNE      *BLANKS
078900981015     C                   MOVEL     DOPZ(6)       V1CATB
079000981015E   1C                   ENDIF
079100000000     C*
079200000000     C                   ENDSR
079300000000     C*------------------------------------------------------------------------*
079400000000     C*  AZZVID - PULISCE TUTTI ICAMPI DEL VIDEO
079500000000     C*------------------------------------------------------------------------*
079600000000     C     AZZVID        BEGSR
079700000000     C*
079800000000     C                   Z-ADD     D60NBC        V1CNBC
079900000000     C                   Z-ADD     *ZEROS        V1CCMP
080000000000     C                   Z-ADD     *ZEROS        V1CDDC
080100000000     C                   Z-ADD     *ZEROS        V1CDSC
080200000000     C                   MOVEL     *BLANKS       V1CCMD
080300000000     C                   MOVEL     *BLANKS       V1CFCR
080400020130     C                   MOVEL     *BLANKS       V1CFDR
080401031212     C                   MOVEL     *BLANKS       V1dFDR
080500980312     C                   MOVEL     *BLANKS       V1CRIC
080600000000     C                   MOVEL     *BLANKS       ECTMCE
080700981015     C                   MOVEL     *BLANKS       V1CATB
080800000000     C*
080900000000     C                   ENDSR
081000000000     C*------------------------------------------------------------------------*
081100000000     C* LANPGM - LANCIA UN PROGRAMMA
081200000000     C*------------------------------------------------------------------------*
081300000000     C     LANPGM        BEGSR
081400000000     C*--------------------
081500000000     C* PARAMETRI INFORMATIVI
081600000000     C*--------------------
081700000000     C                   MOVEL     WOP0          D60OP0
081800000000     C                   MOVEL     WOP1          D60OP1
081900000000     C                   MOVEL     '0'           D60F03
082000000000     C                   MOVEL     '0'           D60ERR
082100000000     C                   MOVEL     *BLANKS       D60MSG
082200000000     C*--------------------
082300000000     C* PARAMETRI
082400000000     C*--------------------
082500000000     C                   Z-ADD     WNBC          D60NBC
082600000000     C                   Z-ADD     WCMP          D60CMP
082700000000     C                   Z-ADD     WDDC          D60DDC
082800000000     C* LANCIO
082900000000     C                   MOVEL     DS60          KPJBU
083000000000     C                   CALL      WPGM                                 99
083100000000     C                   PARM                    KPJBA
083200000000     C                   MOVEL     KPJBU         DS60
083300000000     C*
083400000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
083500000000IF  1C     D60F03        IFEQ      '1'
083600000000     C                   MOVEL     '1'           $F03
083700000000     C                   MOVEL     '1'           $ERR
083800000000     C                   MOVEL     '1'           $FINE
083900000000     C*
084000000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
084100000000X   1C                   ELSE
084200000000     C                   MOVEL     D60MSG        $MSG
084300000000     C                   EXSR      CARMSG
084400000000E   1C                   ENDIF
084500000000     C*
084600000000     C                   ENDSR
084700020923     C*-------------------------------------------------------------------------
084800020923     C* REPERISCE I DATI UTENTE
084900020923     C*-------------------------------------------------------------------------
085000020923     C     REPDATIUTE    BEGSR
085100020923     C*
085200020923     C* INIZIALIZZA VARIABILI DI WRK
085300020923     C                   CLEAR                   TIBS34DS
085400020923     C                   CLEAR                   AZUTEDS
085500020923     C                   CLEAR                   DDATIUTE
085600020923     C*
085700020923     C     *DTAARA       DEFINE    §azute        azuteds
085800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
085900020923     C                   IN(E)     *DTAARA
086000020923if  1C                   IF        %Error
086100020923     c                   EVAL      I34Tla = 'L'
086200020923     C                   CALL      'TIBS34R'
086300020923     C                   PARM                    Tibs34Ds
086400020923     C                   IN        *DTAARA
086500020923e   1C                   ENDIF
086600020923     C*
086700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
086800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
086900020923     C*
087000020923     C                   ENDSR
087001130924     C*------------------------------------------------------------------------*
087002130924     C* dupnot - duplica note da precedente progressivo se esistono
087003130924     C*------------------------------------------------------------------------*
087004130924     C     dupnot        begsr
087011130924     c     knot          setll     ecnot01l
087012130924     c                   do        *hival
087013130924     c     knot          reade     ecnot01l
087014130924     c                   if        %eof(ecnot01l)
087015130924     c                   leave
087016130924     c                   endif
087017130924     c                   z-add     ectnbc        notprg
087018130924     c     knotb         chain     ecnot01l
087019130924     c                   if        not %found(ecnot01l)
087021130924     c                   write     ecnot000
087022130924     c                   endif
087023130924     c                   enddo
087024130924     c                   endsr
087100000000     C*------------------------------------------------------------------------*
087200000000     C*  *INZSR - ROUTINE INIZIALE
087300000000     C*------------------------------------------------------------------------*
087400000000     C     *INZSR        BEGSR
087500000000     C*
087600000000     C* RICEVIMENTO PARAMETRI
087700000000     C     *ENTRY        PLIST
087800000000     C                   PARM                    KPJBA
087900000000     C                   MOVEL     KPJBU         DS60
088000000000     C*
088100000000     C* DEFINIZIONE VARIABILI
088200000000     C     *LIKE         DEFINE    ECTNBC        KCTNBC
088300000000     C     *LIKE         DEFINE    ECTCMP        KCTCMP
088400000000     C     *LIKE         DEFINE    ECTDDC        KCTDDC
088500000000     C     *LIKE         DEFINE    ECDNBC        KCDNBC
088600000000     C     *LIKE         DEFINE    ECDCMP        KCDCMP
088700940721     C     *LIKE         DEFINE    TBLKUT        KTBKUT
088800940721     C     *LIKE         DEFINE    TBLCOD        KTBCOD
088900940721     C     *LIKE         DEFINE    TBLKEY        KTBKEY
089000000000     C     *LIKE         DEFINE    ECTNBC        WNBC
089100000000     C     *LIKE         DEFINE    ECTCMP        WCMP
089200000000     C     *LIKE         DEFINE    ECTDDC        WDDC
089300000000     C*
089400000000     C* CHIAVE LETTURA ECECT01L - COMPLETA
089500000000     C     KEYECT        KLIST
089600000000     C                   KFLD                    KCTNBC
089700000000     C                   KFLD                    KCTCMP
089800000000     C                   KFLD                    KCTDDC
089801130924     C     Knot          KLIST
089802130924     C                   KFLD                    tpana             1
089803130924     C                   KFLD                    tnota             1
089804130924     C                   KFLD                    ectcmp
089805130924     C                   KFLD                    blcPRG            3 0
089806130924     C                   move      'C'           tpana
089807130924     C     Knotb         KLIST
089808130924     C                   KFLD                    tpana
089809130924     C                   KFLD                    tnota
089810130924     C                   KFLD                    ectcmp
089811130924     C                   KFLD                    notprg
089812130924     C                   KFLD                    notpno
089900000000     C*
090000000000     C* CHIAVE LETTURA ECECT01L - PARZIALE
090100000000     C     KE2ECT        KLIST
090200000000     C                   KFLD                    KCTNBC
090300000000     C                   KFLD                    KCTCMP
090400000000     C*
090500000000     C* CHIAVE LETTURA ECEVD01L - PARZIALE
090600000000     C     KEYECD        KLIST
090700000000     C                   KFLD                    KCDNBC
090800000000     C                   KFLD                    KCDCMP
090900940721     C*
091000940721     C* CHIAVE LETTURA TABEL00F - COMPLETA
091100940721     C     KEYTAB        KLIST
091200940721     C                   KFLD                    KTBKUT
091300940721     C                   KFLD                    KTBCOD
091400940721     C                   KFLD                    KTBKEY
091500000000     C*
091600000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
091700000000     C                   MOVE      D60OP0        INTGES            2
091800000000IF  1C     INTGES        IFEQ      '02'
091900000000     C                   MOVEL     'G'           $TGES             1
092000000000X   1C                   ELSE
092100000000     C                   MOVEL     'I'           $TGES             1
092200000000E   1C                   ENDIF
092300940721     C*
092400940721     C* CONTROLLA SE IL N°BLOCCO PASSATO GIA' STATO USATO PER AGGIORNARE IL C/E
092500980130     C     *LIKE         DEFINE    §2KUBC        $FLAU
092600940721     C                   Z-ADD     1             KTBKUT
092700980130     C                   MOVEL     '2K'          KTBCOD
092800940721     C                   MOVE      *BLANKS       KTBKEY
092900940721     C     KEYTAB        CHAIN(N)  TABEL00F                           99
093000940721IF  1C     *IN99         IFEQ      *ON
093100940721     C                   MOVEL     'S'           $FLAU
093200940721X   1C                   ELSE
093300980130     C                   MOVEL     TBLUNI        DS2K
093400980130     C                   MOVEL     §2KUBC        $FLAU
093500940721E   1C                   ENDIF
093600000000     C*
093700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
093800000000     C                   EXSR      INZSTD
093900000000     C*
094000000000     C* RAGIONE SOCIALE UTENTE
094100000000     C                   MOVEL     DSFIRS        VTDRSU
094200000000     C*
094300000000     C* DECODIFICA OPZIONE RICHIAMATA
094400000000IF  1C     D60OP1        IFEQ      'F06'
094500000000     C                   MOVEL     DOPZ(1)       VTDFUN
094600000000E   1C                   ENDIF
094700000000IF  2C     D60OP1        IFEQ      'O02'
094800000000     C                   MOVEL     DOPZ(2)       VTDFUN
094900000000E   1C                   ENDIF
095000000000IF  1C     D60OP1        IFEQ      'O04'
095100000000     C                   MOVEL     DOPZ(3)       VTDFUN
095200000000E   1C                   ENDIF
095300000000IF  1C     D60OP1        IFEQ      'O07'
095400000000     C                   MOVEL     DOPZ(4)       VTDFUN
095500000000E   1C                   ENDIF
095600000000IF  1C     D60OP1        IFEQ      'O05'
095700000000     C                   MOVEL     DOPZ(5)       VTDFUN
095800000000E   1C                   ENDIF
095900000000     C*
096000000000     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
096100000000     C* 0=NON PROTETTO, 1=PROTETTO
096200000000     C*   90 - CAMPI CHIAVE
096300000000     C*   91 - ALTRI CAMPI
096400000000IF  1C     D60OP1        IFEQ      'F06'
096500000000     C                   SETOFF                                       9091
096600000000E   1C                   ENDIF
096700000000IF  1C     D60OP1        IFEQ      'O02'
096800000000     C                   SETON                                        90
096900000000E   1C                   ENDIF
097000000000IF  1C     D60OP1        IFEQ      'O04'
097100000000     C     D60OP1        OREQ      'O07'
097200000000     C     D60OP1        OREQ      'O05'
097300000000     C                   SETON                                        9091
097400000000E   1C                   ENDIF
097500000000     C*
097600000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
097700000000IF  1C     D60OP1        IFEQ      'O05'
097800000000     C                   MOVEL     FUN(6)        FUNRIG
097900000000     C                   MOVEL     'N'           FUNASG
098000000000     C                   MOVEL     'N'           FUNASI
098100000000     C                   MOVEL     'N'           FUNAFG
098200000000     C                   MOVEL     'N'           FUNAFG
098300000000     C                   MOVEL     FUNRIG        FUN(6)
098400000000E   1C                   ENDIF
098500000000     C*
098600000000     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
098700000000IF  1C     D60OP1        IFEQ      'O02'
098800000000     C     D60OP1        OREQ      'O04'
098900000000     C     D60OP1        OREQ      'O05'
099000000000     C     D60OP1        OREQ      'O07'
099100000000     C                   Z-ADD     D60NBC        KCTNBC
099200000000     C                   Z-ADD     D60CMP        KCTCMP
099300000000     C                   Z-ADD     D60DDC        G02DAT
099400000000     C                   ADD       1000000       G02DAT
099500000000     C                   Z-ADD     *ZEROS        G02INV
099600000000     C                   MOVEL     '0'           G02ERR
099700000000     C                   CALL      'XSRDA8'
099800000000     C                   PARM                    WLBDAT
099900000000     C                   MOVE      00            G02INV
100000000000     C                   Z-ADD     G02INV        KCTDDC
100100000000     C                   Z-ADD     G02INV        DDCKCT            8 0
100200000000     C*
100300000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
100400000000     C     KEYECT        CHAIN     ECECT01L                           9998
100500000000IF  2C     *IN99         IFEQ      '1'
100600000000     C                   MOVEL     '1'           $ERR
100700000000     C                   MOVEL     '1'           $FINE
100800000000     C                   MOVEL     MSG(21)       D60MSG
100900000000X   2C                   ELSE
101000000000IF  3C     *IN98         IFEQ      '1'
101100000000     C                   MOVEL     '1'           $ERR
101200000000     C                   MOVEL     '1'           $FINE
101300000000     C                   MOVEL     MSG(22)       D60MSG
101400000000X   3C                   ELSE
101500000000     C                   Z-ADD     ECTDSC        G02INV
101600000000     C                   Z-ADD     *ZEROS        G02DAT
101700000000     C                   MOVEL     '3'           G02ERR
101800000000     C                   CALL      'XSRDA8'
101900000000     C                   PARM                    WLBDAT
102000000000     C                   Z-ADD     G02DAT        DSCKCT            8 0
102100000000     C*
102200000000     C                   EXSR      RECVID
102300000000     C                   EXSR      CARV1
102400000000E   3C                   ENDIF
102500000000E   2C                   ENDIF
102600000000E   1C                   ENDIF
102700000000     C*
102800000000     C* SE INSERIMENTO
102900000000IF  1C     D60OP1        IFEQ      'F06'
103000000000     C                   EXSR      AZZVID
103100000000     C                   EXSR      CARV1
103200000000     C                   ELSE
103300000000     C                   EXSR      DECODI
103400000000E   1C                   ENDIF
103500000000     C*
103600000000     C                   ENDSR
103700000000     C*------------------------------------------------------------------------*
103800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
103900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
104000000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
104100000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
104200000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
104300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
104400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
104500000000     C*------------------------------------------------------------------------*
104600000000     C* INZSTD - ROUTINE INIZIALE STANDARD
104700000000     C*------------------------------------------------------------------------*
104800000000     C     INZSTD        BEGSR
104900000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
105000000000     C                   CALL      'XKEYS'
105100000000     C                   PARM                    DSKEYS
105200000000     C                   MOVEA     KEYRIG        KEYS
105300000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
105400000000     C     XMSGPA        PLIST
105500000000     C                   PARM                    DSMSMS
105600000000     C                   PARM                    DSMSPG
105700000000     C                   PARM                    DSMSKE
105800000000     C                   PARM                    DSMSER
105900000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
106000000000     C                   MOVEL     'TREC73R'     DSMSPG
106100000000     C                   MOVEL     'TREC73R'     VMCQUE
106200000000     C                   MOVEL     *BLANKS       VMCKEY
106300000000     C                   MOVEL     *BLANKS       DSMSMS
106400000000     C* INIZIALIZZO FLAG CONTROLLO PGM
106500000000     C                   MOVEL     '0'           $ERR              1
106600000000     C                   MOVEL     '0'           $F03              1
106700000000     C                   MOVEL     *BLANKS       $GES              2
106800000000     C                   MOVEL     *BLANKS       $MSG             80
106900000000     C                   MOVEL     '0'           $FINE             1
107000000000     C                   MOVEL     '0'           $TESVD            1
107100000000     C* LANCIO ROUTINE PER VERIFICARE SE
107200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
107300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
107400020923     C                   EXSR      REPDATIUTE
107500000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
107600000000     C                   BITOFF    '01234567'    POSMI1            1
107700000000     C                   BITOFF    '01234567'    POSMI2            1
107800000000     C                   BITOFF    '01234567'    POSMA1            1
107900000000     C                   BITOFF    '01234567'    POSMA2            1
108000000000     C                   BITON     '34'          POSMI1            1
108100000000     C                   BITON     '7'           POSMI2            1
108200000000     C                   BITON     '34'          POSMA1            1
108300000000     C                   BITON     '0'           POSMA2            1
108400000000     C                   MOVE      POSMI2        POSMIN            2
108500000000     C                   MOVEL     POSMI1        POSMIN
108600000000     C                   MOVE      POSMA2        POSMAX            2
108700000000     C                   MOVEL     POSMA1        POSMAX
108800000000     C                   ENDSR
108900000000     C*------------------------------------------------------------------------*
109000000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
109100000000     C*------------------------------------------------------------------------*
109200000000     C     FUNSTD        BEGSR
109300000000     C                   SELECT
109400000000     C     KEY           WHENEQ    F3
109500000000     C                   EXSR      F03STD
109600000000     C     KEY           WHENEQ    F12
109700000000     C                   EXSR      F12STD
109800000000     C     KEY           WHENEQ    F24
109900000000     C                   EXSR      F24STD
110000000000     C                   OTHER
110100000000     C                   MOVEL     MSG(2)        $MSG
110200000000     C                   EXSR      CARMSG
110300000000     C                   ENDSL
110400000000     C                   ENDSR
110500000000     C*------------------------------------------------------------------------*
110600000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
110700000000     C*------------------------------------------------------------------------*
110800000000     C     F03STD        BEGSR
110900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
111000000000     C                   MOVEL     '1'           $F03
111100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
111200000000     C                   MOVEL     '1'           $FINE
111300000000     C                   ENDSR
111400000000     C*------------------------------------------------------------------------*
111500000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
111600000000     C*------------------------------------------------------------------------*
111700000000     C     F12STD        BEGSR
111800000000     C* IMPOSTO FLAG DI FIN PGM
111900000000     C                   MOVEL     '1'           $FINE
112000000000     C                   ENDSR
112100000000     C*------------------------------------------------------------------------*
112200000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
112300000000     C*------------------------------------------------------------------------*
112400000000     C     F24STD        BEGSR
112500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
112600000000     C                   EXSR      OUTFUN
112700000000     C                   ENDSR
112800000000     C*------------------------------------------------------------------------*
112900000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
113000000000     C*          CODA MESSAGGI DEL PGM
113100000000     C*------------------------------------------------------------------------*
113200000000     C     CARMSG        BEGSR
113300000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
113400000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
113500000000     C     MSGULT        IFEQ      0
113600000000     C                   MOVEL     '1'           *IN40
113700000000     C                   WRITE     EC73DVK
113800000000     C                   MOVEL     '0'           *IN40
113900000000     C                   ENDIF
114000000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
114100000000     C                   MOVEL     $MSG          DSMSMS
114200000000     C                   CALL      'XMSG'        XMSGPA
114300000000     C     DSMSER        IFEQ      0
114400000000     C                   MOVEL     DSMSKE        VMCKEY
114500000000     C                   ENDIF
114600000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
114700000000     C                   ADD       1             MSGULT            4 0
114800000000     C                   WRITE     EC73DVM
114900000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
115000000000     C                   MOVEL     *BLANKS       $MSG
115100000000     C                   ENDSR
115200000000     C*------------------------------------------------------------------------*
115300000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
115400000000     C*------------------------------------------------------------------------*
115500000000     C     OUTMSG        BEGSR
115600000000     C* VISUALIZZAZIONE MESSAGGIO
115700000000     C                   MOVEL     '1'           *IN44
115800000000     C                   WRITE     EC73DVK
115900000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
116000000000     C                   Z-ADD     *ZEROS        MSGULT
116100000000     C                   ENDSR
116200000000     C*------------------------------------------------------------------------*
116300000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
116400000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
116500000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
116600000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
116700000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
116800000000     C*------------------------------------------------------------------------*
116900000000     C     CARFUN        BEGSR
117000000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
117100000000     C     1             DO        31            I                 5 0
117200000000     C                   MOVEL     FUN(I)        FUNRIG
117300000000     C                   MOVEL     FUNFUN        FFU(I)
117400000000     C                   MOVEL     FUNDES        FDE(I)
117500020923     C     DUTLPO        IFEQ      'S'
117600000000     C     $TGES         IFEQ      'G'
117700000000     C                   MOVEL     FUNASG        FAT(I)
117800000000     C                   ELSE
117900000000     C                   MOVEL     FUNASI        FAT(I)
118000000000     C                   ENDIF
118100000000     C                   ELSE
118200000000     C     $TGES         IFEQ      'G'
118300000000     C                   MOVEL     FUNAFG        FAT(I)
118400000000     C                   ELSE
118500000000     C                   MOVEL     FUNAFI        FAT(I)
118600000000     C                   ENDIF
118700000000     C                   ENDIF
118800000000     C     FAT(I)        IFEQ      'E'
118900000000     C                   MOVEL     USER          WEDP              3
119000000000     C     WEDP          IFEQ      'EDP'
119100000000     C                   MOVEL     'S'           FAT(I)
119200000000     C                   ELSE
119300000000     C                   MOVEL     'N'           FAT(I)
119400000000     C                   ENDIF
119500000000     C                   ENDIF
119600000000     C                   ENDDO
119700000000     C                   ENDSR
119800000000     C*------------------------------------------------------------------------*
119900000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
120000000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
120100000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
120200000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
120300000000     C*------------------------------------------------------------------------*
120400000000     C     OUTFUN        BEGSR
120500000000     C     1             DO        8             I
120600000000     C                   MOVEL     *BLANKS       FUV(I)
120700000000     C                   ENDDO
120800000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
120900000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
121000000000     C     1             DO        31            I
121100000000     C     FAT(I)        IFEQ      'S'
121200000000     C     FDE(I)        ANDNE     *BLANKS
121300000000     C                   ADD       1             FUNNAT
121400000000     C                   ENDIF
121500000000     C                   ENDDO
121600000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
121700000000     C     FUNNAT        IFGT      8
121800000000     C                   MOVEL     'S'           FAT(24)
121900000000     C                   ENDIF
122000000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
122100000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
122200000000     C                   Z-ADD     8             FUNVIS            5 0
122300000000     C     FAT(23)       IFEQ      'S'
122400000000     C                   SUB       1             FUNVIS
122500000000     C                   ENDIF
122600000000     C     FAT(24)       IFEQ      'S'
122700000000     C                   SUB       1             FUNVIS
122800000000     C                   ENDIF
122900000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
123000000000     C                   Z-ADD     *ZEROS        W                 5 0
123100000000     C                   ADD       1             FUNULT
123200000000     C     FUNULT        IFGT      31
123300000000     C                   Z-ADD     1             FUNULT
123400000000     C                   ENDIF
123500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
123600000000     C     FUNULT        DO        31            I
123700000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
123800000000     C     W             IFLT      FUNVIS
123900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
124000000000     C     FAT(I)        IFEQ      'S'
124100000000     C     FDE(I)        ANDNE     *BLANKS
124200000000     C     I             IFEQ      23
124300000000     C     I             OREQ      24
124400000000     C                   ELSE
124500000000     C                   ADD       1             W
124600000000     C                   MOVEL     FDE(I)        FUV(W)
124700000000     C                   ENDIF
124800000000     C                   ENDIF
124900000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
125000000000     C                   Z-ADD     I             FUNULT
125100000000     C                   ENDIF
125200000000     C                   ENDDO
125300000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
125400000000     C                   MOVEL     FUV(1)        VFD001
125500000000     C                   MOVEL     FUV(2)        VFD002
125600000000     C                   MOVEL     FUV(3)        VFD003
125700000000     C                   MOVEL     FUV(4)        VFD004
125800000000     C                   MOVEL     FUV(5)        VFD005
125900000000     C                   MOVEL     FUV(6)        VFD006
126000000000     C     FAT(23)       IFEQ      'N'
126100000000     C     FAT(24)       ANDEQ     'N'
126200000000     C                   MOVEL     FUV(7)        VFD007
126300000000     C                   MOVEL     FUV(8)        VFD008
126400000000     C                   ELSE
126500000000     C     FAT(23)       IFEQ      'S'
126600000000     C     FAT(24)       ANDEQ     'S'
126700000000     C                   MOVEL     FDE(23)       VFD007
126800000000     C                   MOVEL     FDE(24)       VFD008
126900000000     C                   ELSE
127000000000     C                   MOVEL     FUV(7)        VFD007
127100000000     C     FAT(23)       IFEQ      'S'
127200000000     C                   MOVEL     FDE(23)       VFD008
127300000000     C                   ELSE
127400000000     C                   MOVEL     FDE(24)       VFD008
127500000000     C                   ENDIF
127600000000     C                   ENDIF
127700000000     C                   ENDIF
127800000000     C                   MOVEL     '1'           $TESVD
127900000000     C                   ENDSR
128000000000     C*------------------------------------------------------------------------*
128100000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
128200000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
128300000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
128400000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
128500000000     C*------------------------------------------------------------------------*
128600000000     C     CONFUN        BEGSR
128700000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
128800000000     C     1             DO        31            I
128900000000     C     KEY           IFEQ      KEYS(I)
129000000000     C* CONTROLLO SE IL TASTO E' ATTIVO
129100000000     C     FAT(I)        IFNE      'S'
129200000000     C                   MOVEL     MSG(1)        $MSG
129300000000     C                   EXSR      CARMSG
129400000000     C                   MOVEL     '1'           $ERR
129500000000     C                   ENDIF
129600000000     C                   ENDIF
129700000000     C                   ENDDO
129800000000     C                   ENDSR
129900000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
130000000000TREC73R- Tasto funzionale non valido.                                          1
130100000000TREC73R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
130200000000TREC73R- Opzione non valida.                                                   3
130300000000TREC73R- Opzione abilitata ma non gestita. Avvertire CED.                      4
130400000000TREC73R- Inizio elenco.                                                        5
130500000000TREC73R- Fine elenco.                                                          6
130600000000TREC73R- 7
130700000000TREC73R- Immettere data formalmente valida                                     8
130800000000TREC73R- Immettere il codice competenza                                        9
130900000000TREC73R- Immettere la descrizione competenza                                  10
131000000000TREC73R- Immettere, come tipo competenza, 'C'=Competenza o 'R'=Ricavo         11
131100000000TREC73R- Immettere S=Si oppure N=No                                           12
131200951207TREC73R- Competenza già esistente                                             13
131300980312TREC73R- Immettere riferimento centro di costo esistente e non annullato      14
131400020130TREC73R- Immettere uno dei valori possibili                                   15
131500000000TREC73R- 16
131600000000TREC73R- Inserimento effettuato.                                              17
131700000000TREC73R- Modifica effettuata.                                                 18
131800000000TREC73R- Cancellazione competenza e conti di contabilita' effettuata.         19
131900000000TREC73R- Ripristino effettuato.                                               20
132000000000TREC73R- Richiesta funzione per voce già esistente                            21
132100000000TREC73R- Codice voce momentaneamente allocato: riprovare più tardi            22
132200000000TREC73R- La data scadenza deve essere maggiore o uguale della decorrenza      23
132300000000TREC73R- La validità della competenza si sovrappone ad una già in decorrenza  24
132400000000TREC73R- 25
132500000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
132600000000F1    -                   -    NN     NN
132700000000F2    -                   -    NN     NN
132800000000F3    -F3=Fine            -    SS     SS
132900000000F4    -                   -    NN     NN
133000000000F5    -                   -    NN     NN
133100000000F6    -F6=Aggiornamento   -    SS     SS
133200000000F7    -                   -    NN     NN
133300000000F8    -                   -    NN     NN
133400000000F9    -                   -    NN     NN
133500000000F10   -                   -    NN     NN
133600000000F11   -F11=Conti contab.tà-    SS     SS
133700000000F12   -F12=Ritorno        -    SS     SS
133800000000F13   -                   -    NN     NN
133900000000F14   -                   -    NN     NN
134000000000F15   -                   -    NN     NN
134100000000F16   -                   -    NN     NN
134200000000F17   -                   -    NN     NN
134300000000F18   -                   -    NN     NN
134400000000F19   -                   -    NN     NN
134500000000F20   -                   -    NN     NN
134600000000F21   -                   -    NN     NN
134700000000F22   -                   -    NN     NN
134800000000F23   -                   -    NN     NN
134900000000F24   -F24=Altri tasti    -    NN     NN
135000000000ENTER -                   -    SS     SS
135100000000ROLLUP-                   -    NN     NN
135200000000ROLLDN-                   -    NN     NN
135300000000HELP  -                   -    NN     NN
135400000000PRINT -                   -    NN     NN
135500000000BSPACE-                   -    NN     NN
135600000000CLEAR -                   -    NN     NN
135700000000** DOPZ  (OPZIONE RICHIAMATA)
135800000000* INSERIMENTO *                                                 1
135900000000*  VARIAZIONE  *                                                2
136000000000*CANCELLAZIONE*                                                 3
136100000000*  RIPRISTINO  *                                                4
136200000000VISUALIZZAZIONE                                                 5
136300981015ANNULLATO                                                       6
1364009810157
1365009810158
1366009810159
13670098101510
136800000000** DECODIFICHE DELLE EMSILITA'
136900000000GENNAIO
137000000000FEBBRAIO
137100000000MARZO
137200000000APRILE
137300000000MAGGIO
137400000000GIUGNO
137500000000LUGLIO
137600000000AGOSTO
137700000000SETTEMBRE
137800000000OTTOBRE
137900000000NOVEMBRE
138000000000DICEMBRE

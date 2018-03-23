000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC76R
000300000000      *  Descrizione   :  Gestione competenze di conto economico (dettaglio)
000400000000      *                   Programma di immissione, modifica, visualizzazione,...
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  30 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC76D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC76DVM:MSGULT)
001400000000     FECECD01L  UF A E           K DISK
001500000000     FECECT01L  UF A E           K DISK
001600020520     FANENT01L  IF   E           K DISK
001700940721     FTABEL00F  UF   E           K DISK
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE
002000000000     D*------------------------------------------------------------------------*
002100940801     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
002200000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002300000000     D*-------------------
002400000000     D* DECODIFICA OPZIONE RICHIAMATA
002500000000     D*-------------------
002600981015     D DOPZ            S             16    DIM(10) CTDATA PERRCD(1)
002700000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002800000000     D FFU             S              6    DIM(31)
002900000000     D FDE             S             19    DIM(31)
003000000000     D FAT             S              1    DIM(31)
003100000000     D* TABELLA DESCRIZIONI A VIDEO
003200000000     D FUV             S             19    DIM(8)
003300000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003400000000     D KEYS            S              1    DIM(40)
003500940805     D*-------------------
003600940805     D* RICERCA PARAMETRI UTENTE
003700940805     D*-------------------
003800000000     D*------------------------------------------------------------------------*
003900000000     D* INPUT
004000000000     D*------------------------------------------------------------------------*
004100000000     D*-------------------
004200000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300000000     D*-------------------
004400000000     D DSFUN         E DS
004500000000     D  FUNRIG                 1     40
004600000000     D*-------------------
004700000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004800000000     D*-------------------
004900000000     D DSMSG         E DS
005000000000     D*-------------------
005100020923     D* DS REPERIMENTO DATI UTENTE
005200020923     D*-------------------
005300020923     D TIBS34DS      E DS
005400020923     D DDATIUTE      E DS
005500020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005600000000     D*-------------------
005700000000     D* DS PER GESTIONE TASTI FUNZIONALI
005800000000     D*-------------------
005900000000     D DSKEYS        E DS
006000000000     D  KEYRIG                 1     40
006100000000     D*-------------------
006200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006300000000     D*-------------------
006400000000     D                SDS
006500000000     D  USER                 254    263
006600000000     D*-------------------
006700000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006800000000     D*-------------------
006900000000     D DEVDS1          DS
007000000000     D  KEY                  369    369
007100000000     D  CURSOR               370    371
007200000000     D  FRTSFL               378    379B 0
007300000000     D*---------------------
007400000000     D* DS ESTERNA DI PROCEDURA
007500000000     D*---------------------
007600000000     D DS60          E DS                  EXTNAME(TREC60DS)
007700000000     D*-------------------
007800000000     D* ARCHITETTURA
007900000000     D*-------------------
008000000000     D KPJBA         E DS
008100940721     D*-------------------
008200980130     D* DS DATI VARI PER SALDI E C/E -NON DI GRUPPO-
008300940721     D*-------------------
008400980130     D DS2K          E DS
008500000000     D*------------------
008600000000     D* CALL XSRDA8 - CONTROLLO DATE
008700000000     D*------------------
008800000000     D WLBDAT          DS                  INZ
008900000000     D  G02DAT                 1      8  0
009000000000     D  G02INV                 9     16  0
009100000000     D  G02ERR                17     17
009200000000     D  G02TGI                18     22  0
009300981009     D*------------------
009400981009     D* DS "XSRDA8" - CONTROLLA DATA (8)
009500981009     D*------------------
009600981009     D WLBDA8          DS                  INZ
009700981009     D  G08DAT                 1      8  0
009800981009     D  G08INV                 9     16  0
009900981009     D  G08ERR                17     17
010000981009     D  G08TGI                18     22  0
010100000000     C*------------------------------------------------------------------------*
010200000000     C* MAIN LINE
010300000000     C*------------------------------------------------------------------------*
010400000000     C*
010500000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
010600000000IF  1C     $FINE         IFEQ      '1'
010700000000     C*
010800000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
010900000000     C                   MOVEL     '1'           D60ERR
011000000000     C*
011100000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
011200000000X   1C                   ELSE
011300000000     C*
011400000000     C* CICLO FINO A CHE:
011500000000DO  2C     $FINE         DOWEQ     '0'
011600000000     C                   MOVEL     '0'           $ERR
011700000000     C*
011800000000     C* CONTROLLA GESTIONE VIDEO
011900000000IF  3C     $GES          IFEQ      'V1'
012000000000     C                   EXSR      GESV1
012100000000E   3C                   ENDIF
012200000000     C*
012300000000E   2C                   ENDDO
012400000000E   1C                   ENDIF
012500000000     C*
012600000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
012700000000     C     $F03          IFEQ      '1'
012800000000     C                   MOVEL     '1'           D60F03
012900000000E   1C                   ENDIF
013000980710     C*
013100000000     C                   MOVEL     DS60          KPJBU
013200000000     C                   SETON                                        LR
013300000000     C*------------------------------------------------------------------------*
013400000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
013500000000     C*------------------------------------------------------------------------*
013600000000     C     CARV1         BEGSR
013700000000     C*
013800000000     C                   MOVEL     'V1'          $GES
013900000000     C*
014000000000     C                   EXSR      CARFUN
014100000000     C                   EXSR      OUTFUN
014200000000     C*
014300000000     C                   WRITE     EC76DVT
014400000000     C                   WRITE     EC76DVF
014500000000     C*
014600000000     C                   ENDSR
014700000000     C*------------------------------------------------------------------------*
014800000000     C* GESV1  - GESTIONE PRIMO VIDEO
014900000000     C*------------------------------------------------------------------------*
015000000000     C     GESV1         BEGSR
015100000000     C*
015200000000IF  1C     $TESVD        IFEQ      '1'
015300000000     C                   EXSR      CARV1
015400000000E   1C                   ENDIF
015500000000     C*
015600000000IF  1C     MSGULT        IFNE      *ZEROS
015700000000     C                   EXSR      OUTMSG
015800000000E   1C                   ENDIF
015900000000     C*
016000000000     C                   EXFMT     EC76DV1
016100000000     C*
016200000000     C                   EXSR      CONFUN
016300000000     C*
016400000000IF  1C     $ERR          IFEQ      '0'
016500000000SL  2C                   SELECT
016600000000     C     KEY           WHENEQ    ENTER
016700000000     C                   EXSR      ENTV1
016800000000     C     KEY           WHENEQ    F6
016900000000     C                   EXSR      F06GES
017000000000     C                   OTHER
017100000000     C                   EXSR      FUNSTD
017200000000E   2C                   ENDSL
017300000000E   1C                   ENDIF
017400000000     C*
017500000000     C                   ENDSR
017600000000     C*------------------------------------------------------------------------*
017700000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
017800000000     C*------------------------------------------------------------------------*
017900000000     C     ENTV1         BEGSR
018000000000     C*
018100000000     C* CONTROLLA I CAMPI IMMESSI
018200000000     C                   EXSR      DECODI
018300000000     C*
018400000000     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
018500000000IF  1C     D60OP1        IFEQ      'O05'
018600000000     C                   MOVEL     '1'           $FINE
018700000000E   1C                   ENDIF
018800000000     C*
018900000000     C                   ENDSR
019000000000     C*------------------------------------------------------------------------*
019100000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
019200000000     C*------------------------------------------------------------------------*
019300000000     C     F06GES        BEGSR
019400000000     C*
019500000000     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
019600000000IF  1C     D60OP1        IFEQ      'F06'
019700000000     C                   EXSR      INSERI
019800000000E   1C                   ENDIF
019900000000IF  1C     D60OP1        IFEQ      'O02'
020000000000     C                   EXSR      MODIFI
020100000000E   1C                   ENDIF
020200000000IF  1C     D60OP1        IFEQ      'O04'
020300000000     C                   EXSR      ANNULL
020400000000E   1C                   ENDIF
020500000000IF  1C     D60OP1        IFEQ      'O07'
020600000000     C                   EXSR      RIPRIS
020700000000E   1C                   ENDIF
020800000000     C*
020900000000     C                   ENDSR
021000000000     C*------------------------------------------------------------------------*
021100000000     C* INSERI - FUNZIONE DI INSERIMENTO
021200000000     C*------------------------------------------------------------------------*
021300000000     C     INSERI        BEGSR
021400000000     C*
021500000000     C* CONTROLLA I CAMPI IMMESSI
021600000000     C                   EXSR      DECODI
021700000000     C*
021800000000IF  1C     $ERR          IFEQ      '0'
021900940721     C*
022000940721     C* PRIMA DI INSERIRE DEVO:
022100940721     C*   1.  DUPLICARE TUTTE LE COMPETENZE DEL BLOCCO CON NUOVO BLOCCO
022200940721     C*   2.  INCREMENTARE NUMERO BLOCCO
022300940721     C* TRANNE CHE IL BLOCCO NON SIA STATO MAI USATO PER SCRIVERE IL C/ECONOMICO
022400940721IF  2C     $FLAU         IFEQ      'S'
022500940721     C                   EXSR      DUPNBC
022600940721E   2C                   ENDIF
022700000000     C*
022800000000     C                   Z-ADD     V1CNBC        ECDNBC
022900000000     C                   Z-ADD     V1CCMP        ECDCMP
023000940719     C                   MOVEL     V1CFRA        ECDFRA
023100940726     C                   MOVEL     V1CIOE        ECDIOE
023200940722     C                   Z-ADD     V1CDDO        G02DAT
023300940722     C                   MOVEL     01            G02DAT
023400000000     C                   Z-ADD     *ZEROS        G02INV
023500000000     C                   MOVEL     '0'           G02ERR
023600000000     C                   CALL      'XSRDA8'
023700000000     C                   PARM                    WLBDAT
023800940722     C                   MOVE      00            G02INV
023900940722     C                   Z-ADD     G02INV        ECDDDO
024000940722     C*
024100940722     C                   Z-ADD     V1CDSO        G02DAT
024200940722     C                   MOVEL     01            G02DAT
024300000000     C                   Z-ADD     *ZEROS        G02INV
024400000000     C                   MOVEL     '0'           G02ERR
024500000000     C                   CALL      'XSRDA8'
024600000000     C                   PARM                    WLBDAT
024700940722     C                   MOVE      00            G02INV
024800940722     C                   Z-ADD     G02INV        ECDDSO
024900000000     C                   Z-ADD     V1CKCC        ECDKCC
025000000000     C                   MOVE      V1CKSC        ECDKSC
025100000000     C                   MOVE      V1CRIC        ECDRIC
025200940719     C                   Z-ADD     V1CCCF        ECDCCF
025300000000     C                   MOVE      V1CSEG        ECDSEG
025400000000     C                   Z-ADD     V1CPER        ECDPER
025500000000     C*
025600000000     C                   MOVEL     *BLANKS       ECDATB
025700000000     C                   MOVEL     *BLANKS       ECDFTR
025800000000     C                   Z-ADD     *ZEROS        ECDDTR
025900000000     C*
026000000000     C                   WRITE     ECECD
026100000000     C*
026200000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
026300000000     C                   MOVEL     '1'           $FINE
026400000000     C*
026500000000     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
026600000000     C                   MOVEL     MSG(17)       D60MSG
026700000000     C*
026800000000E   1C                   ENDIF
026900000000     C*
027000000000     C                   ENDSR
027100000000     C*------------------------------------------------------------------------*
027200000000     C* MODIFI - FUNZIONE DI MODIFICA
027300000000     C*------------------------------------------------------------------------*
027400000000     C     MODIFI        BEGSR
027500000000     C*
027600000000     C* CONTROLLA I CAMPI IMMESSI
027700000000     C                   EXSR      DECODI
027800000000IF  1C     $ERR          IFEQ      '0'
027900000000     C*
028000000000     C* PRIMA DI AGGIORNARE DEVO:
028100000000     C*   1.  DUPLICARE TUTTE LE COMPETENZE DEL BLOCCO CON NUOVO BLOCCO
028200000000     C*   2.  INCREMENTARE NUMERO BLOCCO
028300940721     C* TRANNE CHE IL BLOCCO NON SIA STATO MAI USATO PER SCRIVERE IL C/ECONOMICO
028400940721IF  2C     $FLAU         IFEQ      'S'
028500000000     C                   EXSR      DUPNBC
028600940721E   2C                   ENDIF
028700000000     C*
028800000000     C* RILEGGO LA COMPETENZA PER SPOSIZIONAMENTO NEI CONTROLLI
028900000000     C                   Z-ADD     V1CNBC        KCDNBC
029000000000     C                   Z-ADD     D60CMP        KCDCMP
029100940719     C                   MOVEL     D60FRA        KCDFRA
029200940726     C                   MOVEL     D60IOE        KCDIOE
029300000000     C                   Z-ADD     D60KCC        KCDKCC
029400000000     C                   Z-ADD     D60KSC        KCDKSC
029500000000     C                   MOVE      D60RIC        KCDRIC
029600940719     C                   Z-ADD     D60CCF        KCDCCF
029700000000     C                   Z-ADD     DDOKCD        KCDDDO
029800000000     C     KEYECD        CHAIN     ECECD01L                           99
029900000000IF  2C     *IN99         IFEQ      *OFF
030000000000     C*
030100000000     C* I CAMPI ASTERISCATI SONO CAMPI CHIAVE, NON MODIFICABILI
030200000000     C                   Z-ADD     V1CDSO        G02DAT
030300950612     C                   MOVEL     01            G02DAT
030400000000     C                   Z-ADD     *ZEROS        G02INV
030500000000     C                   MOVEL     '0'           G02ERR
030600000000     C                   CALL      'XSRDA8'
030700000000     C                   PARM                    WLBDAT
030800000000     C                   Z-ADD     G02INV        ECDDSO
030900940722     C                   MOVE      00            ECDDSO
031000000000     C                   MOVE      V1CSEG        ECDSEG
031100000000     C                   Z-ADD     V1CPER        ECDPER
031200000000     C*
031300000000     C                   MOVEL     *BLANKS       ECDATB
031400000000     C                   MOVEL     *BLANKS       ECDFTR
031500970903     C*---                 Z-ADD*ZEROS    ECDDTR           *DATA TRASMISSIONE
031600000000     C*
031700000000     C                   UPDATE    ECECD
031800000000     C*
031900000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
032000000000     C                   MOVEL     '1'           $FINE
032100000000     C*
032200000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
032300000000     C                   MOVEL     MSG(18)       D60MSG
032400000000     C*
032500000000E   2C                   ENDIF
032600000000E   1C                   ENDIF
032700000000     C*
032800000000     C                   ENDSR
032900000000     C*------------------------------------------------------------------------*
033000000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
033100000000     C*------------------------------------------------------------------------*
033200000000     C     ANNULL        BEGSR
033300000000     C*
033400000000     C* CONTROLLA I CAMPI IMMESSI
033500000000     C                   EXSR      DECODI
033600000000     C*
033700000000IF  1C     $ERR          IFEQ      '0'
033800000000     C*
033900000000     C* PRIMA DI AGGIORNARE DEVO:
034000000000     C*   1.  DUPLICARE TUTTE LE COMPETENZE DEL BLOCCO CON NUOVO BLOCCO
034100000000     C*   2.  INCREMENTARE NUMERO BLOCCO
034200940721     C* TRANNE CHE IL BLOCCO NON SIA STATO MAI USATO PER SCRIVERE IL C/ECONOMICO
034300940721IF  2C     $FLAU         IFEQ      'S'
034400000000     C                   EXSR      DUPNBC
034500940721E   2C                   ENDIF
034600000000     C*
034700000000     C* RILEGGO LA COMPETENZA PER SPOSIZIONAMENTO NEI CONTROLLI
034800000000     C                   Z-ADD     V1CNBC        KCDNBC
034900000000     C                   Z-ADD     D60CMP        KCDCMP
035000940719     C                   MOVEL     D60FRA        KCDFRA
035100940726     C                   MOVEL     D60IOE        KCDIOE
035200000000     C                   Z-ADD     D60KCC        KCDKCC
035300000000     C                   Z-ADD     D60KSC        KCDKSC
035400000000     C                   MOVE      D60RIC        KCDRIC
035500940719     C                   Z-ADD     D60CCF        KCDCCF
035600000000     C                   Z-ADD     DDOKCD        KCDDDO
035700000000     C     KEYECD        CHAIN     ECECD01L                           99
035800000000IF  2C     *IN99         IFEQ      *OFF
035900000000     C*--------------------
036000000000     C* ANNULLO VOCE
036100000000     C*--------------------
036200970903     C* SE NON E' STATO MAI TRASMESSO, CANCELLA FISICAMENTE IL RECORD
036300970903IF  3C     ECDDTR        IFGT      *ZEROS
036400000000     C                   MOVEL     'A'           ECDATB
036500000000     C                   MOVEL     *BLANKS       ECDFTR
036600970903     C*---                 Z-ADD*ZEROS    ECDDTR           *DATA TRASMISSIONE
036700000000     C                   UPDATE    ECECD
036800970903X   3C                   ELSE
036900970903     C                   DELETE    ECECD
037000970903E   3C                   ENDIF
037100000000     C*
037200000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
037300000000     C                   MOVEL     '1'           $FINE
037400000000     C*
037500000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
037600000000     C                   MOVEL     MSG(19)       D60MSG
037700000000     C*
037800000000E   2C                   ENDIF
037900000000E   1C                   ENDIF
038000000000     C*
038100000000     C                   ENDSR
038200000000     C*------------------------------------------------------------------------*
038300000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
038400000000     C*------------------------------------------------------------------------*
038500000000     C     RIPRIS        BEGSR
038600000000     C*
038700000000     C* CONTROLLA I CAMPI IMMESSI
038800000000     C                   EXSR      DECODI
038900000000     C*
039000000000IF  1C     $ERR          IFEQ      '0'
039100000000     C*
039200000000     C* PRIMA DI AGGIORNARE DEVO:
039300000000     C*   1.  DUPLICARE TUTTE LE COMPETENZE DEL BLOCCO CON NUOVO BLOCCO
039400000000     C*   2.  INCREMENTARE NUMERO BLOCCO
039500940721     C* TRANNE CHE IL BLOCCO NON SIA STATO MAI USATO PER SCRIVERE IL C/ECONOMICO
039600940721IF  2C     $FLAU         IFEQ      'S'
039700000000     C                   EXSR      DUPNBC
039800940721E   2C                   ENDIF
039900000000     C*
040000000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
040100000000     C                   Z-ADD     V1CNBC        KCDNBC
040200000000     C                   Z-ADD     D60CMP        KCDCMP
040300940719     C                   MOVEL     D60FRA        KCDFRA
040400940726     C                   MOVEL     D60IOE        KCDIOE
040500000000     C                   Z-ADD     DDOKCD        KCDDDO
040600000000     C                   Z-ADD     D60KCC        KCDKCC
040700000000     C                   Z-ADD     D60KSC        KCDKSC
040800000000     C                   MOVE      D60RIC        KCDRIC
040900940719     C                   Z-ADD     D60CCF        KCDCCF
041000000000     C     KEYECD        CHAIN     ECECD01L                           99
041100000000IF  2C     *IN99         IFEQ      *OFF
041200000000     C*
041300000000     C*--------------------
041400000000     C* RIPRISTINO VOCE
041500000000     C*--------------------
041600000000     C                   MOVEL     *BLANKS       ECDATB
041700000000     C                   MOVEL     *BLANKS       ECDFTR
041800970903     C*---                 Z-ADD*ZEROS    ECDDTR           *DATA TRASMISSIONE
041900000000     C*
042000000000     C                   UPDATE    ECECD
042100000000     C*
042200000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
042300000000     C                   MOVEL     '1'           $FINE
042400000000     C*
042500000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
042600000000     C                   MOVEL     MSG(20)       D60MSG
042700000000     C*
042800000000E   2C                   ENDIF
042900000000E   1C                   ENDIF
043000000000     C*
043100000000     C                   ENDSR
043200000000     C*------------------------------------------------------------------------*
043300000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
043400000000     C*------------------------------------------------------------------------*
043500000000     C     DECODI        BEGSR
043600000000     C*
043700000000     C* REIMPOSTO INDICATORI DI ERRORE
043800000000     C                   SETOFF                                       606162
043900000000     C                   SETOFF                                       636465
044000940719     C                   SETOFF                                       666768
044100940803     C                   SETOFF                                       6970
044200000000     C                   MOVE      *BLANKS       $PUNTO
044300940721     C*--------------------
044400940721     C* CODICE COMPETENZA
044500940721     C*--------------------
044600940721     C                   Z-ADD     V1CNBC        KCTNBC
044700940721     C                   Z-ADD     V1CCMP        KCTCMP
044800940721     C     KEYECT        CHAIN     ECECT01L                           60
044900940721     C                   MOVE      *BLANKS       V1CCMD
045000940721     C                   MOVEL     *BLANKS       V1CFCR
045100940721IF  1C     *IN60         IFEQ      *ON
045200940721     C                   MOVEL     '1'           $ERR
045300940721     C                   MOVEL     MSG(9)        $MSG
045400940721     C                   EXSR      CARMSG
045500940721E   1C                   ENDIF
045600940719     C*--------------------
045700940719     C* COMPETENZA FRANCO O ASSEGNATO
045800940719     C*--------------------
045900940719IF  1C     $ERR          IFEQ      '0'
046000940719IF  2C     V1CFRA        IFEQ      'F'
046100940719     C     V1CFRA        OREQ      'A'
046200940804     C     V1CFRA        OREQ      ' '
046300940719     C                   ELSE
046400940719     C                   MOVEL     '1'           $ERR
046500940719     C                   MOVEL     MSG(25)       $MSG
046600940719     C                   EXSR      CARMSG
046700940719     C                   SETON                                        68
046800940719E   2C                   ENDIF
046900940719E   1C                   ENDIF
047000940725     C*--------------------
047100940725     C* COMPETENZA ITALIA O ESTERO O TUTTE E DUE
047200940725     C*--------------------
047300940725IF  1C     $ERR          IFEQ      '0'
047400940725IF  2C     V1CIOE        IFEQ      'I'
047500940725     C     V1CIOE        OREQ      'E'
047600940804     C     V1CIOE        OREQ      ' '
047700940725     C                   ELSE
047800940725     C                   MOVEL     '1'           $ERR
047900940725     C                   MOVEL     MSG(27)       $MSG
048000940725     C                   EXSR      CARMSG
048100940725     C                   SETON                                        70
048200940725E   2C                   ENDIF
048300940725E   1C                   ENDIF
048400000000     C*--------------------
048500000000     C* CODICE CONTO
048600000000     C*--------------------
048700981015     C                   MOVEL     *BLANKS       V1CKCD
048800981009IF  1C     $ERR          IFEQ      '0'
048900981019IF  2C     V1CKCC        IFGT      *ZEROS
049000981009     C                   MOVEL     '201'         YSOC              3
049100981013     C                   Z-ADD     V1CKCC        YVON              4 0
049200981013     C                   MOVEL     *BLANKS       YVOA              8
049300981009     C                   Z-ADD     DATCOR        YDAT              8 0
049400981009     C                   MOVEL     *BLANKS       YDES             20
049500981009     C                   MOVEL     *BLANKS       YERR              1
049600981009     C                   MOVEL     *BLANKS       YMSG             80
049700981009     C                   CALL      'YCO900R'
049800981009     C                   PARM                    YSOC
049900981013     C                   PARM                    YVON
050000981013     C                   PARM                    YVOA
050100981009     C                   PARM                    YDAT
050200981009     C                   PARM                    YDES
050300981009     C                   PARM                    YERR
050400981009     C                   PARM                    YMSG
050500981015IF  3C     YERR          IFEQ      'S'
050600981015     C                   SETON                                        61
050700981009     C                   MOVEL     '1'           $ERR
050800981009     C                   MOVEL     YMSG          $MSG
050900981009     C                   EXSR      CARMSG
051000981015X   3C                   ELSE
051100981009     C                   MOVEL     YDES          V1CKCD
051200981015E   3C                   ENDIF
051300981015E   2C                   ENDIF
051400981009E   1C                   ENDIF
051500000000     C*--------------------
051600940719     C* CODICE RIFERIMENTO CENTRO DI COSTO OPPURE CENTRO DI COSTO FORZATO
051700000000     C*--------------------
051800940719     C* IMMETTERE IL RIFERIMENTO O IL C.D.C. FORZATO (SOLO UNO DEI DUE)
051900940719IF  1C     $ERR          IFEQ      '0'
052000940719IF  2C     V1CRIC        IFNE      *BLANKS
052100940721     C     V1CRIC        ANDNE     *ZEROS
052200940719IF  3C     V1CCCF        IFGT      *ZEROS
052300940719     C                   MOVEL     '1'           $ERR
052400940719     C                   MOVEL     MSG(16)       $MSG
052500940719     C                   EXSR      CARMSG
052600940719     C                   SETON                                        6369
052700940719E   3C                   ENDIF
052800940719E   2C                   ENDIF
052900940719E   1C                   ENDIF
053000940719     C*
053100000000IF  1C     $ERR          IFEQ      '0'
053200940719IF  2C     V1CRIC        IFNE      *BLANKS
053300940721     C     V1CRIC        ANDNE     *ZEROS
053400000000     C                   MOVEL     '2T'          §COD
053500000000     C                   MOVEL     *BLANKS       §KEY
053600000000     C                   MOVEL     V1CRIC        §KEY
053700000000     C                   EXSR      CHKTAB
053800000000     C                   MOVEL     §KEY          V1CRIC
053900000000     C                   MOVEL     §DES          V1CRID
054000940719IF  3C     $ERR          IFEQ      '1'
054100000000     C                   MOVEL     MSG(11)       $MSG
054200000000     C                   EXSR      CARMSG
054300000000     C                   SETON                                        63
054400940719E   3C                   ENDIF
054500940719E   2C                   ENDIF
054600940719E   1C                   ENDIF
054700940719     C*
054800940719     C* IMMETTERE IL RIFERIMENTO O IL C.D.C. FORZATO (ALMENO UNO DEI DUE)
054900940727     C                   MOVEL     *BLANKS       V1DCCF
055000940719IF  1C     $ERR          IFEQ      '0'
055100940719IF  2C     V1CRIC        IFEQ      *BLANKS
055200940721     C     V1CRIC        OREQ      *ZEROS
055300940719IF  3C     V1CCCF        IFEQ      *ZEROS
055400940719     C                   MOVEL     '1'           $ERR
055500940719     C                   MOVEL     MSG(16)       $MSG
055600940719     C                   EXSR      CARMSG
055700940719     C                   SETON                                        6369
055800940727X   3C                   ELSE
055900940727     C                   Z-ADD     V1CCCF        $CCF
056000940727     C                   MOVEL     *BLANKS       $DCCF
056100940727     C                   EXSR      CHKCCF
056200940727     C                   MOVEL     $DCCF         V1DCCF
056300940727IF  4C     $ERR          IFEQ      '1'
056400940727     C                   MOVEL     MSG(28)       $MSG
056500940727     C                   EXSR      CARMSG
056600940727     C                   SETON                                        69
056700940727E   4C                   ENDIF
056800940727E   3C                   ENDIF
056900940719E   2C                   ENDIF
057000940719E   1C                   ENDIF
057100000000     C*--------------------
057200000000     C* DATA DECORRENZA
057300000000     C*--------------------
057400000000IF  1C     $ERR          IFEQ      '0'
057500940722     C                   Z-ADD     V1CDDO        $DAT              8 0
057600940722     C                   MOVEL     01            $DAT
057700940722     C                   EXSR      CHKDAT
057800000000     C                   Z-ADD     $DAT          V1CDDO
057900000000IF  2C     $ERR          IFEQ      '1'
058000000000     C                   EXSR      CARMSG
058100000000     C                   SETON                                        64
058200000000E   2C                   ENDIF
058300000000E   1C                   ENDIF
058400951207     C*--------------------
058500951207     C* INSERIMENTO -> CONTO DI COMPETENZA INESISTENTE
058600951207     C*--------------------
058700951207IF  1C     $ERR          IFEQ      '0'
058800951207IF  2C     D60OP1        IFEQ      'F06'
058900951207     C                   Z-ADD     V1CNBC        KCDNBC
059000951207     C                   Z-ADD     D60CMP        KCDCMP
059100951207     C                   MOVEL     D60FRA        KCDFRA
059200951207     C                   MOVEL     D60IOE        KCDIOE
059300951207     C                   Z-ADD     D60KCC        KCDKCC
059400951207     C                   Z-ADD     D60KSC        KCDKSC
059500951207     C                   MOVE      D60RIC        KCDRIC
059600951207     C                   Z-ADD     D60CCF        KCDCCF
059700951207     C                   Z-ADD     DDOKCD        KCDDDO
059800951207     C     KEYECD        SETLL     ECECD01L                               99
059900951207IF  3C     *IN99         IFEQ      *ON
060000951207     C                   MOVEL     '1'           $ERR
060100951207     C                   MOVEL     MSG(29)       $MSG
060200951207     C                   EXSR      CARMSG
060300951207     C                   SETON                                        606870
060400951207     C                   SETON                                        616263
060500951207     C                   SETON                                        6964
060600951207E   3C                   ENDIF
060700951207E   2C                   ENDIF
060800951207E   1C                   ENDIF
060900000000     C*--------------------
061000000000     C* DATA SCADENZA
061100000000     C*--------------------
061200000000IF  1C     $ERR          IFEQ      '0'
061300940722     C                   Z-ADD     V1CDSO        $DAT
061400940722     C                   MOVEL     01            $DAT
061500000000     C                   EXSR      CHKDAT
061600000000     C                   Z-ADD     $DAT          V1CDSO
061700000000IF  2C     $ERR          IFEQ      '1'
061800000000     C                   EXSR      CARMSG
061900000000     C                   SETON                                        65
062000000000E   2C                   ENDIF
062100000000     C*
062200000000     C* MAGGIORE DELLA DECORRENZA
062300000000IF  2C     $ERR          IFEQ      '0'
062400000000     C                   Z-ADD     V1CDDO        G02DAT
062500940722     C                   MOVEL     01            G02DAT
062600000000     C                   Z-ADD     *ZEROS        G02INV
062700000000     C                   MOVEL     '0'           G02ERR
062800000000     C                   CALL      'XSRDA8'
062900000000     C                   PARM                    WLBDAT
063000940722     C                   MOVE      00            G02INV
063100940722     C                   Z-ADD     G02INV        DDOV1C            8 0
063200000000     C*
063300000000     C                   Z-ADD     V1CDSO        G02DAT
063400940722     C                   MOVEL     01            G02DAT
063500000000     C                   Z-ADD     *ZEROS        G02INV
063600000000     C                   MOVEL     '0'           G02ERR
063700000000     C                   CALL      'XSRDA8'
063800000000     C                   PARM                    WLBDAT
063900940722     C                   MOVE      00            G02INV
064000000000     C                   Z-ADD     G02INV        DSOV1C            8 0
064100000000     C*
064200000000IF  3C     DSOV1C        IFLT      DDOV1C
064300000000     C                   MOVEL     '1'           $ERR
064400000000     C                   MOVEL     MSG(10)       $MSG
064500000000     C                   EXSR      CARMSG
064600000000     C                   SETON                                        6465
064700000000E   3C                   ENDIF
064800000000E   2C                   ENDIF
064900000000E   1C                   ENDIF
065000000000     C*--------------------
065100000000     C* VALIDITA' DETTAGLIO COMPETENZA
065200000000     C*--------------------
065300000000IF  1C     $ERR          IFEQ      '0'
065400000000     C*
065500000000     C                   Z-ADD     V1CNBC        KCDNBC
065600000000     C                   Z-ADD     V1CCMP        KCDCMP
065700940719     C                   MOVEL     V1CFRA        KCDFRA
065800940725     C                   MOVEL     V1CIOE        KCDIOE
065900000000     C                   MOVE      V1CKCC        KCDKCC
066000000000     C                   MOVE      V1CKSC        KCDKSC
066100000000     C                   MOVE      V1CRIC        KCDRIC
066200940719     C                   Z-ADD     V1CCCF        KCDCCF
066300000000     C     KE2ECD        SETLL     ECECD01L
066400000000     C     KE2ECD        READE(N)  ECECD01L                               99
066500000000DO  2C     *IN99         DOWEQ     *OFF
066600951207IF  3C     ECDATB        IFEQ      *BLANKS
066700000000     C*
066800000000     C* SE SONO IN MODIFI/ANNULL/RIPRIS DAL CONTROLLO ESCLUDO LA VOCE IN LINEA
066900000000IF  4C     D60OP1        IFNE      'F06'
067000000000     C     DDOKCD        ANDEQ     ECDDDO
067100000000X   4C                   ELSE
067200000000     C*
067300000000IF  5C     DDOV1C        IFLE      ECDDSO
067400000000IF  6C     DSOV1C        IFLT      ECDDDO
067500000000X   6C                   ELSE
067600000000     C                   MOVEL     '1'           $ERR
067700000000     C                   MOVEL     MSG(24)       $MSG
067800000000     C                   EXSR      CARMSG
067900000000     C                   SETON                                        646599
068000000000E   6C                   ENDIF
068100000000E   5C                   ENDIF
068200000000E   4C                   ENDIF
068300951207E   3C                   ENDIF
068400000000     C  N99KE2ECD        READE(N)  ECECD01L                               99
068500000000E   2C                   ENDDO
068600000000     C*
068700000000E   1C                   ENDIF
068800000000     C*--------------------
068900940721     C* IL PERDIODO IMMESSO DEVE ESSERE COMPRESO IN UNA DELLE COMPETENZE
069000940721     C* PRESENTI IN TESTATA (ECECT00F)
069100000000     C*--------------------
069200000000IF  1C     $ERR          IFEQ      '0'
069300000000     C*
069400000000     C* DEVO TROVARE ALMENO UNA COMPETENZA CHE CONTENGA INTERAMENTE LE DUE DATE
069500000000     C                   MOVE      'N'           $OK               1
069600000000     C                   Z-ADD     V1CNBC        KCTNBC
069700000000     C                   Z-ADD     V1CCMP        KCTCMP
069800000000     C     KEYECT        SETLL     ECECT01L
069900000000DO  2C                   DO        *HIVAL
070000000000     C     KEYECT        READE(N)  ECECT01L                               99
070100000000     C*
070200000000     C* SE LA DATA SCADENZA LETTA (DA ECEVT) E' INFERIORE ALLA SCADENZA A VIDEO
070300000000IF  3C  N99ECTATB        IFEQ      *BLANKS
070400940722     C                   MOVE      00            ECTDDC
070500940722     C                   MOVE      00            ECTDSC
070600000000IF  4C     DDOV1C        IFGE      ECTDDC
070700000000     C     DSOV1C        ANDLE     ECTDSC
070800940721     C                   MOVE      'S'           $OK
070900000000     C                   SETON                                        99
071000940721     C*
071100940721     C* DEPOSITO LA DECORRENZA DELLA COMPETENZA TROVATA PER CONTROLLI SUCESSIVI
071200940721     C                   Z-ADD     ECTDDC        DEPDDC
071300940721     C*
071400000000E   4C                   ENDIF
071500000000E   3C                   ENDIF
071600000000E   2C  N99              ENDDO
071700000000     C*
071800000000IF  2C     $OK           IFNE      'S'
071900000000     C                   MOVEL     '1'           $ERR
072000000000     C                   MOVEL     MSG(15)       $MSG
072100000000     C                   EXSR      CARMSG
072200000000     C                   SETON                                        6465
072300000000E   2C                   ENDIF
072400000000     C*
072500000000E   1C                   ENDIF
072600940801     C*--------------------
072700940801     C* NON PIU' DI 10 CONTI PER UNA STESSA COMPETENZA
072800940801     C*--------------------
072900940803IF  1C     D60OP1        IFEQ      'F06'
073000940803     C     $ERR          ANDEQ     '0'
073100940803     C                   Z-ADD     *ZERO         $CONTA            3 0
073200940803     C                   Z-ADD     V1CNBC        KCDNBC
073300940803     C                   Z-ADD     V1CCMP        KCDCMP
073400940803     C     KE3ECD        SETLL     ECECD01L
073500981109     C     KE3ECD        READE(N)  ECECD01L                               99
073600000000DO  2C     *IN99         DOWEQ     *OFF
073700000000IF  3C     ECDATB        IFEQ      *BLANKS
073800000000     C                   ADD       1             $CONTA
073900000000E   3C                   ENDIF
074000981109     C     KE3ECD        READE(N)  ECECD01L                               99
074100000000E   2C                   ENDDO
074200940801IF  2C     $CONTA        IFGE      10
074300000000     C                   MOVEL     '1'           $ERR
074400000000     C                   MOVEL     MSG(23)       $MSG
074500000000     C                   EXSR      CARMSG
074600000000     C                   SETON                                        60
074700940801E   2C                   ENDIF
074800940801E   1C                   ENDIF
074900000000     C*--------------------
075000940719     C* SEGNO (DARE O AVERE)
075100000000     C*--------------------
075200000000IF  1C     $ERR          IFEQ      '0'
075300940719IF  2C     V1CSEG        IFNE      'D'
075400940719     C     V1CSEG        ANDNE     'A'
075500000000     C                   MOVEL     '1'           $ERR
075600000000     C                   MOVEL     MSG(12)       $MSG
075700000000     C                   EXSR      CARMSG
075800000000     C                   SETON                                        66
075900000000E   2C                   ENDIF
076000000000E   1C                   ENDIF
076100000000     C*--------------------
076200000000     C* PERCENTUALE
076300000000     C*--------------------
076400000000IF  1C     $ERR          IFEQ      '0'
076500000000IF  2C     V1CPER        IFLE      *ZERO
076600000000     C     V1CPER        ORGT      100
076700000000     C                   MOVEL     '1'           $ERR
076800000000     C                   MOVEL     MSG(13)       $MSG
076900000000     C                   EXSR      CARMSG
077000000000     C                   SETON                                        67
077100000000E   2C                   ENDIF
077200000000E   1C                   ENDIF
077300940721     C*--------------------
077400940721     C* DECODIFICO CODICE COMPETENZA SOLO DOPO CHE MI HAANO IMMESSO LA VALIDITA'
077500940721     C* DEL CONTO PER SAPERE SE E' UNA COMPETENZA CHE VUOLE IL DETTAGLIO O MENO
077600940721     C*--------------------
077700940721IF  1C     $ERR          IFEQ      '0'
077800940721     C                   Z-ADD     V1CNBC        KCTNBC
077900940721     C                   Z-ADD     V1CCMP        KCTCMP
078000940721     C                   Z-ADD     DEPDDC        KCTDDC
078100940721     C     KEYEC2        CHAIN     ECECT01L                           60
078200940721     C                   MOVEL     ECTCMD        V1CCMD
078300940721     C                   MOVEL     ECTFCR        V1CFCR
078400940721     C*
078500940721     C* COMPETENZA CHE NON AGGIORNA IL C/E: NON INSERIRE IL DETTAGLIO
078600940721IF  2C     ECTMCE        IFEQ      'N'
078700940721     C                   MOVEL     '1'           $ERR
078800940721     C                   MOVEL     MSG(26)       $MSG
078900940721     C                   EXSR      CARMSG
079000940721     C                   SETON                                        60
079100940721E   2C                   ENDIF
079200940721E   1C                   ENDIF
079300940801     C*
079400000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
079500000000     C* CURSORE SUL CAMPO DEL CONTROL
079600000000IF  1C     $ERR          IFEQ      '1'
079700000000     C                   MOVEL     '0'           *IN43
079800000000X   1C                   ELSE
079900000000     C                   MOVEL     '1'           *IN43
080000000000E   1C                   ENDIF
080100940801     C*
080200000000     C                   ENDSR
080300000000     C*------------------------------------------------------------------------*
080400000000     C* CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
080500000000     C*          INPUT --> §COD: CODICE TABELLA
080600000000     C*                    $KEY: CODICE ELEMENTO
080700000000     C*------------------------------------------------------------------------*
080800000000     C     CHKTAB        BEGSR
080900000000     C*
081000000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
081100000000     C     '?'           SCAN      §KEY                                   99
081200000000     C     *IN99         IFEQ      *ON
081300020923     C                   MOVEL     1             §KUT
081400000000     C                   MOVE      *BLANKS       §DES
081500000000     C                   CALL      'X§TABER'
081600000000     C                   PARM                    §KUT              1 0
081700000000     C                   PARM                    §COD              2
081800000000     C                   PARM                    §KEY              8
081900000000     C                   PARM                    §DES             30
082000000000     C                   MOVEL     '1'           $PUNTO            1
082100000000     C*
082200000000     C                   ENDIF
082300000000     C*
082400000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
082500020923     C                   MOVEL     1             KTBKUT
082600000000     C                   MOVEL     §COD          KTBCOD
082700000000     C                   MOVE      *BLANKS       KTBKEY
082800000000     C                   MOVEL     §KEY          KTBKEY
082900000000     C     KEYTAB        CHAIN     TABEL00F                           99
083000000000     C     *IN99         IFEQ      '1'
083100000000     C     TBLFLG        ORNE      *BLANKS
083200000000     C                   MOVEL     '1'           $ERR
083300000000     C                   MOVEL     *ALL'*'       §DES
083400000000     C                   ELSE
083500000000     C                   MOVEL     TBLUNI        §DES
083600000000     C                   ENDIF
083700000000     C*
083800000000     C                   ENDSR
083900000000     C*------------------------------------------------------------------------*
084000000000     C* CHKDAT - CONTROLLA LA DATA
084100000000     C*          INPUT --> $DAT:  DATA
084200000000     C*------------------------------------------------------------------------*
084300000000     C     CHKDAT        BEGSR
084400000000     C*
084500000000     C* OBBLIGATORIA
084600000000IF  1C     $DAT          IFEQ      *ZEROS
084700000000     C                   MOVEL     '1'           $ERR
084800000000     C                   MOVEL     MSG(8)        $MSG
084900000000X   1C                   ELSE
085000000000     C* CORRETTA
085100000000     C                   Z-ADD     $DAT          G02DAT
085200000000     C                   Z-ADD     *ZEROS        G02INV
085300000000     C                   MOVEL     '0'           G02ERR
085400000000     C                   CALL      'XSRDA8'
085500000000     C                   PARM                    WLBDAT
085600000000     C*
085700000000IF  2C     G02ERR        IFEQ      '1'
085800000000     C                   MOVEL     '1'           $ERR
085900000000     C                   MOVEL     MSG(8)        $MSG
086000000000X   2C                   ELSE
086100000000     C                   Z-ADD     G02DAT        $DAT
086200000000E   2C                   ENDIF
086300000000E   1C                   ENDIF
086400000000     C*
086500000000     C                   ENDSR
086600940727     C*------------------------------------------------------------------------*
086700940727     C* CHKCCF - CONTROLLO CENTRO DI COSTO
086800940727     C*------------------------------------------------------------------------*
086900940727     C     CHKCCF        BEGSR
087000940727     C*
087100020520     C     *LIKE         DEFINE    V1CCCF        $CCF
087200940727     C     *LIKE         DEFINE    V1DCCF        $DCCF
087300981009     C*
087400020520     C* COSTRUISCO LA CHIAVE X ANENT01L
087500020520     C                   MOVEL     $CCF          ENTENTITA
087600020520     C                   EVAL      ENTSOCIETA = YSOC
087700020520     C                   EVAL      ENTTPRIFAN = 'CDC'
087800020520     C                   EVAL      ENTENTITA  = %subst(ENTENTITA:3:3)
087900020520     C*
088000020520     C* REPERISCO LA DESCRIZIONE DEL CAPOCONTO
088100020520     C     KEYENT        CHAIN     ANENT01L
088200020520     C                   IF        %found(ANENT01L)
088300020520     C                   EVAL      $DCCF = ENTDESCR
088400020520     C                   ELSE
088500020520     C                   MOVEL     '1'           $ERR
088600020520     C                   MOVEL     *ALL'*'       $DCCF
088700020520     C                   ENDIF
088800940727     C*
088900940727     C                   ENDSR
089000000000     C*------------------------------------------------------------------------*
089100000000     C*  DUPNBC - DUPLICAZIONE DI TUTTI I RECORD DI ECECT CHE HANNO LO STESSO
089200000000     C*           NUMERO BLOCCO.
089300000000     C*           DUPLICAZIONE DI TUTTI I RECORD DI ECECD CHE HANNO LO STESSO
089400000000     C*           NUMERO BLOCCO.
089500980130     C*           INCREMENTO DEL NUMERO BLOCCO E AGGIORNAMENTO TABELLA '2K'
089600000000     C*------------------------------------------------------------------------*
089700000000     C     DUPNBC        BEGSR
089800000000     C*
089900940721     C* INCREMENTO IL NUMERO BLOCCO SE L'ULTIMO E'GIA STATO USATO PER AGGIORNARE
090000940721     C* IL CONTO ECONOMICO, ALTRIMENTI USO LO STESSO NUMERO BLOCCO
090100000000     C     *LIKE         DEFINE    ECTNBC        $NBC
090200000000     C     V1CNBC        ADD       1             $NBC
090300940721     C*
090400940721     C* AGGIORNA LA TABELLA
090500020923     C                   MOVEL     1             KTBKUT
090600980130     C                   MOVEL     '2K'          KTBCOD
090700940721     C                   MOVE      *BLANKS       KTBKEY
090800940721     C     KEYTAB        SETLL     TABEL00F
090900940721     C     KEYTAB        READE     TABEL00F                               99
091000980130     C                   Z-ADD     $NBC          §2KNBC
091100980130     C                   MOVEL     'N'           §2KUBC
091200980130     C                   MOVEL     DS2K          TBLUNI
091300940721     C                   MOVEL     *BLANKS       TBLFTR
091400940721     C                   Z-ADD     *ZEROS        TBLDTR
091500940721     C                   UPDATE    TABEL
091600000000     C*
091700000000     C* DUPLICO IL FILE DELLE TESTATE (  E C E C T 0 0 F  )
091800000000     C     V1CNBC        SETLL     ECECT01L
091900000000     C     V1CNBC        READE     ECECT01L                               99
092000000000     C     *IN99         DOWEQ     *OFF
092100000000     C                   Z-ADD     $NBC          ECTNBC
092200000000     C                   MOVE      *BLANKS       ECTFTR
092300000000     C                   Z-ADD     *ZERO         ECTDTR
092400000000     C                   WRITE     ECECT
092500000000     C     V1CNBC        READE     ECECT01L                               99
092600000000     C                   ENDDO
092700000000     C*
092800000000     C* DUPLICO IL FILE DEL DETTAGLIO (  E C E C D 0 0 F  )
092900000000     C     V1CNBC        SETLL     ECECD01L
093000000000     C     V1CNBC        READE     ECECD01L                               99
093100000000     C     *IN99         DOWEQ     *OFF
093200000000     C                   Z-ADD     $NBC          ECDNBC
093300000000     C                   MOVE      *BLANKS       ECDFTR
093400000000     C                   Z-ADD     *ZERO         ECDDTR
093500000000     C                   WRITE     ECECD
093600000000     C     V1CNBC        READE     ECECD01L                               99
093700000000     C                   ENDDO
093800000000     C*
093900000000     C                   Z-ADD     $NBC          V1CNBC
094000000000     C                   Z-ADD     $NBC          D60NBC
094100000000     C*
094200000000     C                   ENDSR
094300000000     C*------------------------------------------------------------------------*
094400000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
094500000000     C*------------------------------------------------------------------------*
094600000000     C     RECVID        BEGSR
094700000000     C*
094800000000     C                   Z-ADD     D60NBC        V1CNBC
094900000000     C                   Z-ADD     D60CMP        V1CCMP
095000940719     C                   MOVEL     D60FRA        V1CFRA
095100940726     C                   MOVEL     D60IOE        V1CIOE
095200000000     C                   Z-ADD     D60DDO        V1CDDO
095300000000     C                   Z-ADD     ECDDSO        G02INV
095400000000     C                   Z-ADD     *ZEROS        G02DAT
095500000000     C                   MOVEL     '3'           G02ERR
095600000000     C                   CALL      'XSRDA8'
095700000000     C                   PARM                    WLBDAT
095800000000     C                   Z-ADD     G02DAT        V1CDSO
095900000000     C                   MOVE      ECDKCC        V1CKCC
096000000000     C                   MOVE      ECDKSC        V1CKSC
096100000000     C                   MOVE      ECDRIC        V1CRIC
096200940719     C                   Z-ADD     ECDCCF        V1CCCF
096300000000     C                   MOVE      ECDSEG        V1CSEG
096400000000     C                   Z-ADD     ECDPER        V1CPER
096500981015IF  1C     ECDATB        IFNE      *BLANKS
096600981015     C                   MOVEL     DOPZ(6)       V1CATB
096700981015E   1C                   ENDIF
096800000000     C*
096900000000     C                   ENDSR
097000000000     C*------------------------------------------------------------------------*
097100000000     C*  AZZVID - PULISCE TUTTI ICAMPI DEL VIDEO
097200000000     C*------------------------------------------------------------------------*
097300000000     C     AZZVID        BEGSR
097400000000     C*
097500000000     C                   Z-ADD     *ZEROS        V1CCMP
097600940804     C                   MOVEL     ' '           V1CFRA
097700940804     C                   MOVEL     ' '           V1CIOE
097800000000     C                   MOVE      *BLANKS       V1CKCC
097900000000     C                   MOVE      *BLANKS       V1CKSC
098000000000     C                   MOVE      *BLANKS       V1CRIC
098100940719     C                   Z-ADD     *ZERO         V1CCCF
098200000000     C                   Z-ADD     *ZEROS        V1CDDO
098300000000     C                   Z-ADD     *ZEROS        V1CDSO
098400000000     C                   MOVE      *BLANKS       V1CSEG
098500940727     C                   Z-ADD     100           V1CPER
098600981015     C                   MOVEL     *BLANKS       V1CATB
098700000000     C*
098800000000     C                   ENDSR
098900020923     C*-------------------------------------------------------------------------
099000020923     C* REPERISCE I DATI UTENTE
099100020923     C*-------------------------------------------------------------------------
099200020923     C     REPDATIUTE    BEGSR
099300020923     C*
099400020923     C* INIZIALIZZA VARIABILI DI WRK
099500020923     C                   CLEAR                   TIBS34DS
099600020923     C                   CLEAR                   AZUTEDS
099700020923     C                   CLEAR                   DDATIUTE
099800020923     C*
099900020923     C     *DTAARA       DEFINE    §azute        azuteds
100000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
100100020923     C                   IN(E)     *DTAARA
100200020923if  1C                   IF        %Error
100300020923     c                   EVAL      I34Tla = 'L'
100400020923     C                   CALL      'TIBS34R'
100500020923     C                   PARM                    Tibs34Ds
100600020923     C                   IN        *DTAARA
100700020923e   1C                   ENDIF
100800020923     C*
100900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
101000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
101100020923     C*
101200020923     C                   ENDSR
101300000000     C*------------------------------------------------------------------------*
101400940727     C* *INZSR - ROUTINE INIZIALE
101500000000     C*------------------------------------------------------------------------*
101600000000     C     *INZSR        BEGSR
101700000000     C*
101800000000     C* RICEVIMENTO PARAMETRI
101900000000     C     *ENTRY        PLIST
102000000000     C                   PARM                    KPJBA
102100000000     C                   MOVEL     KPJBU         DS60
102200000000     C                   Z-ADD     D60NBC        V1CNBC
102300000000     C*
102400000000     C* DEFINIZIONE VARIABILI
102500000000     C     *LIKE         DEFINE    ECDNBC        KCDNBC
102600000000     C     *LIKE         DEFINE    ECDCMP        KCDCMP
102700940719     C     *LIKE         DEFINE    ECDFRA        KCDFRA
102800940725     C     *LIKE         DEFINE    ECDIOE        KCDIOE
102900000000     C     *LIKE         DEFINE    ECDKCC        KCDKCC
103000000000     C     *LIKE         DEFINE    ECDKSC        KCDKSC
103100000000     C     *LIKE         DEFINE    ECDRIC        KCDRIC
103200940719     C     *LIKE         DEFINE    ECDCCF        KCDCCF
103300000000     C     *LIKE         DEFINE    ECDDDO        KCDDDO
103400000000     C     *LIKE         DEFINE    ECTNBC        KCTNBC
103500000000     C     *LIKE         DEFINE    ECTCMP        KCTCMP
103600000000     C     *LIKE         DEFINE    ECTDDC        KCTDDC
103700000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
103800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
103900000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
104000940721     C*
104100940721     C* DEFINIZIONE DEPOSITI
104200940721     C     *LIKE         DEFINE    ECTDDC        DEPDDC
104300000000     C*
104400000000     C* CHIAVE LETTURA ECECD01L - COMPLETA
104500000000     C     KEYECD        KLIST
104600000000     C                   KFLD                    KCDNBC
104700000000     C                   KFLD                    KCDCMP
104800940719     C                   KFLD                    KCDFRA
104900940726     C                   KFLD                    KCDIOE
105000000000     C                   KFLD                    KCDKCC
105100000000     C                   KFLD                    KCDKSC
105200000000     C                   KFLD                    KCDRIC
105300940719     C                   KFLD                    KCDCCF
105400000000     C                   KFLD                    KCDDDO
105500000000     C*
105600000000     C* CHIAVE LETTURA ECECD01L - PARZIALE
105700000000     C     KE2ECD        KLIST
105800000000     C                   KFLD                    KCDNBC
105900000000     C                   KFLD                    KCDCMP
106000940719     C                   KFLD                    KCDFRA
106100940726     C                   KFLD                    KCDIOE
106200000000     C                   KFLD                    KCDKCC
106300000000     C                   KFLD                    KCDKSC
106400000000     C                   KFLD                    KCDRIC
106500940719     C                   KFLD                    KCDCCF
106600000000     C*
106700000000     C* CHIAVE LETTURA ECECD01L - PARZIALE
106800000000     C     KE3ECD        KLIST
106900000000     C                   KFLD                    KCDNBC
107000000000     C                   KFLD                    KCDCMP
107100000000     C*
107200000000     C* CHIAVE LETTURA ECECT01L - PARZIALE
107300000000     C     KEYECT        KLIST
107400000000     C                   KFLD                    KCTNBC
107500000000     C                   KFLD                    KCTCMP
107600940721     C*
107700940721     C* CHIAVE LETTURA ECECT01L - COMPLETA
107800940721     C     KEYEC2        KLIST
107900940721     C                   KFLD                    KCTNBC
108000940721     C                   KFLD                    KCTCMP
108100940721     C                   KFLD                    KCTDDC
108200000000     C*
108300000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
108400000000     C     KEYTAB        KLIST
108500000000     C                   KFLD                    KTBKUT
108600000000     C                   KFLD                    KTBCOD
108700000000     C                   KFLD                    KTBKEY
108800020520     C*
108900020520     C* CHIAVE LETTURA ANENT01L - COMPLETA
109000020520     C     KEYENT        KLIST
109100020520     C                   KFLD                    ENTSOCIETA
109200020520     C                   KFLD                    ENTTPRIFAN
109300020520     C                   KFLD                    ENTENTITA
109400000000     C*
109500000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
109600000000     C                   MOVE      D60OP0        INTGES            2
109700000000IF  1C     INTGES        IFEQ      '02'
109800000000     C                   MOVEL     'G'           $TGES             1
109900000000X   1C                   ELSE
110000000000     C                   MOVEL     'I'           $TGES             1
110100000000E   1C                   ENDIF
110200940721     C*
110300940721     C* CONTROLLA SE IL N°BLOCCO PASSATO GIA' STATO USATO PER AGGIORNARE IL C/E
110400980130     C     *LIKE         DEFINE    §2KUBC        $FLAU
110500940721     C                   Z-ADD     1             KTBKUT
110600980130     C                   MOVEL     '2K'          KTBCOD
110700940721     C                   MOVE      *BLANKS       KTBKEY
110800940721     C     KEYTAB        CHAIN(N)  TABEL00F                           99
110900940721IF  1C     *IN99         IFEQ      *ON
111000940721     C                   MOVEL     'S'           $FLAU
111100940721X   1C                   ELSE
111200980130     C                   MOVEL     TBLUNI        DS2K
111300980130     C                   MOVEL     §2KUBC        $FLAU
111400940721E   1C                   ENDIF
111500000000     C*
111600000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
111700000000     C                   EXSR      INZSTD
111800981009     C*
111900981009     C* CALCOLA LA DATA CORRENTE
112000981009     C                   TIME                    N14              14 0
112100981009     C                   MOVE      N14           N8                8 0
112200981009     C                   Z-ADD     N8            G08DAT
112300981009     C                   Z-ADD     *ZEROS        G08INV
112400981009     C                   MOVEL     '0'           G08ERR
112500981009     C                   CALL      'XSRDA8'
112600981009     C                   PARM                    WLBDA8
112700981009     C                   Z-ADD     G08INV        DATCOR            8 0
112800000000     C*
112900000000     C* RAGIONE SOCIALE UTENTE
113000000000     C                   MOVEL     DSFIRS        VTDRSU
113100000000     C*
113200000000     C* DECODIFICA OPZIONE RICHIAMATA
113300000000IF  1C     D60OP1        IFEQ      'F06'
113400000000     C                   MOVEL     DOPZ(1)       VTDFUN
113500000000E   1C                   ENDIF
113600000000IF  2C     D60OP1        IFEQ      'O02'
113700000000     C                   MOVEL     DOPZ(2)       VTDFUN
113800000000E   1C                   ENDIF
113900000000IF  1C     D60OP1        IFEQ      'O04'
114000000000     C                   MOVEL     DOPZ(3)       VTDFUN
114100000000E   1C                   ENDIF
114200000000IF  1C     D60OP1        IFEQ      'O07'
114300000000     C                   MOVEL     DOPZ(4)       VTDFUN
114400000000E   1C                   ENDIF
114500000000IF  1C     D60OP1        IFEQ      'O05'
114600000000     C                   MOVEL     DOPZ(5)       VTDFUN
114700000000E   1C                   ENDIF
114800000000     C*
114900000000     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
115000000000     C* 0=NON PROTETTO, 1=PROTETTO
115100000000     C*   90 - CAMPI CHIAVE
115200000000     C*   91 - ALTRI CAMPI
115300000000IF  1C     D60OP1        IFEQ      'F06'
115400000000     C                   SETOFF                                       9091
115500000000E   1C                   ENDIF
115600000000IF  1C     D60OP1        IFEQ      'O02'
115700000000     C                   SETON                                        90
115800000000E   1C                   ENDIF
115900000000IF  1C     D60OP1        IFEQ      'O04'
116000000000     C     D60OP1        OREQ      'O07'
116100000000     C     D60OP1        OREQ      'O05'
116200000000     C                   SETON                                        9091
116300000000E   1C                   ENDIF
116400000000     C*
116500000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
116600000000IF  1C     D60OP1        IFEQ      'O05'
116700000000     C                   MOVEL     FUN(6)        FUNRIG
116800000000     C                   MOVEL     'N'           FUNASG
116900000000     C                   MOVEL     'N'           FUNASI
117000000000     C                   MOVEL     'N'           FUNAFG
117100000000     C                   MOVEL     'N'           FUNAFG
117200000000     C                   MOVEL     FUNRIG        FUN(6)
117300000000E   1C                   ENDIF
117400000000     C*
117500000000     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
117600000000IF  1C     D60OP1        IFEQ      'O02'
117700000000     C     D60OP1        OREQ      'O04'
117800000000     C     D60OP1        OREQ      'O05'
117900000000     C     D60OP1        OREQ      'O07'
118000000000     C                   Z-ADD     D60NBC        KCDNBC
118100000000     C                   Z-ADD     D60CMP        KCDCMP
118200940719     C                   MOVEL     D60FRA        KCDFRA
118300940726     C                   MOVEL     D60IOE        KCDIOE
118400000000     C                   Z-ADD     D60DDO        G02DAT
118500940722     C                   ADD       1000000       G02DAT
118600000000     C                   Z-ADD     *ZEROS        G02INV
118700000000     C                   MOVEL     '0'           G02ERR
118800000000     C                   CALL      'XSRDA8'
118900000000     C                   PARM                    WLBDAT
119000940722     C                   MOVE      00            G02INV
119100000000     C                   Z-ADD     G02INV        KCDDDO
119200000000     C                   Z-ADD     G02INV        DDOKCD            8 0
119300000000     C                   Z-ADD     D60KCC        KCDKCC
119400000000     C                   Z-ADD     D60KSC        KCDKSC
119500000000     C                   MOVE      D60RIC        KCDRIC
119600940719     C                   Z-ADD     D60CCF        KCDCCF
119700000000     C*
119800000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
119900000000     C     KEYECD        CHAIN     ECECD01L                           9998
120000000000IF  2C     *IN99         IFEQ      '1'
120100000000     C                   MOVEL     '1'           $ERR
120200000000     C                   MOVEL     '1'           $FINE
120300000000     C                   MOVEL     MSG(21)       D60MSG
120400000000X   2C                   ELSE
120500000000IF  3C     *IN98         IFEQ      '1'
120600000000     C                   MOVEL     '1'           $ERR
120700000000     C                   MOVEL     '1'           $FINE
120800000000     C                   MOVEL     MSG(22)       D60MSG
120900000000X   3C                   ELSE
121000000000     C                   Z-ADD     ECDDSO        G02INV
121100000000     C                   Z-ADD     *ZEROS        G02DAT
121200000000     C                   MOVEL     '3'           G02ERR
121300000000     C                   CALL      'XSRDA8'
121400000000     C                   PARM                    WLBDAT
121500000000     C                   Z-ADD     G02DAT        DSOKCD            8 0
121600000000     C*
121700000000     C                   EXSR      RECVID
121800000000     C                   EXSR      CARV1
121900000000E   3C                   ENDIF
122000000000E   2C                   ENDIF
122100000000E   1C                   ENDIF
122200000000     C*
122300000000     C* SE INSERIMENTO
122400000000IF  1C     D60OP1        IFEQ      'F06'
122500000000     C                   EXSR      AZZVID
122600000000     C                   EXSR      CARV1
122700000000     C                   ELSE
122800000000     C                   EXSR      DECODI
122900000000E   1C                   ENDIF
123000940727     C*
123100000000     C                   ENDSR
123200000000     C*------------------------------------------------------------------------*
123300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
123400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
123500000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
123600000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
123700000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
123800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
123900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
124000000000     C*------------------------------------------------------------------------*
124100000000     C* INZSTD - ROUTINE INIZIALE STANDARD
124200000000     C*------------------------------------------------------------------------*
124300000000     C     INZSTD        BEGSR
124400000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
124500000000     C                   CALL      'XKEYS'
124600000000     C                   PARM                    DSKEYS
124700000000     C                   MOVEA     KEYRIG        KEYS
124800000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
124900000000     C     XMSGPA        PLIST
125000000000     C                   PARM                    DSMSMS
125100000000     C                   PARM                    DSMSPG
125200000000     C                   PARM                    DSMSKE
125300000000     C                   PARM                    DSMSER
125400000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
125500000000     C                   MOVEL     'TREC76R'     DSMSPG
125600000000     C                   MOVEL     'TREC76R'     VMCQUE
125700000000     C                   MOVEL     *BLANKS       VMCKEY
125800000000     C                   MOVEL     *BLANKS       DSMSMS
125900000000     C* INIZIALIZZO FLAG CONTROLLO PGM
126000000000     C                   MOVEL     '0'           $ERR              1
126100000000     C                   MOVEL     '0'           $F03              1
126200000000     C                   MOVEL     *BLANKS       $GES              2
126300000000     C                   MOVEL     *BLANKS       $MSG             80
126400000000     C                   MOVEL     '0'           $FINE             1
126500000000     C                   MOVEL     '0'           $TESVD            1
126600000000     C* LANCIO ROUTINE PER VERIFICARE SE
126700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
126800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
126900020923     C                   EXSR      REPDATIUTE
127000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
127100000000     C                   BITOFF    '01234567'    POSMI1            1
127200000000     C                   BITOFF    '01234567'    POSMI2            1
127300000000     C                   BITOFF    '01234567'    POSMA1            1
127400000000     C                   BITOFF    '01234567'    POSMA2            1
127500000000     C                   BITON     '34'          POSMI1            1
127600000000     C                   BITON     '7'           POSMI2            1
127700000000     C                   BITON     '34'          POSMA1            1
127800000000     C                   BITON     '0'           POSMA2            1
127900000000     C                   MOVE      POSMI2        POSMIN            2
128000000000     C                   MOVEL     POSMI1        POSMIN
128100000000     C                   MOVE      POSMA2        POSMAX            2
128200000000     C                   MOVEL     POSMA1        POSMAX
128300000000     C                   ENDSR
128400000000     C*------------------------------------------------------------------------*
128500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
128600000000     C*------------------------------------------------------------------------*
128700000000     C     FUNSTD        BEGSR
128800000000     C                   SELECT
128900000000     C     KEY           WHENEQ    F3
129000000000     C                   EXSR      F03STD
129100000000     C     KEY           WHENEQ    F12
129200000000     C                   EXSR      F12STD
129300000000     C     KEY           WHENEQ    F24
129400000000     C                   EXSR      F24STD
129500000000     C                   OTHER
129600000000     C                   MOVEL     MSG(2)        $MSG
129700000000     C                   EXSR      CARMSG
129800000000     C                   ENDSL
129900000000     C                   ENDSR
130000000000     C*------------------------------------------------------------------------*
130100000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
130200000000     C*------------------------------------------------------------------------*
130300000000     C     F03STD        BEGSR
130400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
130500000000     C                   MOVEL     '1'           $F03
130600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
130700000000     C                   MOVEL     '1'           $FINE
130800000000     C                   ENDSR
130900000000     C*------------------------------------------------------------------------*
131000000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
131100000000     C*------------------------------------------------------------------------*
131200000000     C     F12STD        BEGSR
131300000000     C* IMPOSTO FLAG DI FIN PGM
131400000000     C                   MOVEL     '1'           $FINE
131500000000     C                   ENDSR
131600000000     C*------------------------------------------------------------------------*
131700000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
131800000000     C*------------------------------------------------------------------------*
131900000000     C     F24STD        BEGSR
132000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
132100000000     C                   EXSR      OUTFUN
132200000000     C                   ENDSR
132300000000     C*------------------------------------------------------------------------*
132400000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
132500000000     C*          CODA MESSAGGI DEL PGM
132600000000     C*------------------------------------------------------------------------*
132700000000     C     CARMSG        BEGSR
132800000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
132900000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
133000000000     C     MSGULT        IFEQ      0
133100000000     C                   MOVEL     '1'           *IN40
133200000000     C                   WRITE     EC76DVK
133300000000     C                   MOVEL     '0'           *IN40
133400000000     C                   ENDIF
133500000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
133600000000     C                   MOVEL     $MSG          DSMSMS
133700000000     C                   CALL      'XMSG'        XMSGPA
133800000000     C     DSMSER        IFEQ      0
133900000000     C                   MOVEL     DSMSKE        VMCKEY
134000000000     C                   ENDIF
134100000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
134200000000     C                   ADD       1             MSGULT            4 0
134300000000     C                   WRITE     EC76DVM
134400000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
134500000000     C                   MOVEL     *BLANKS       $MSG
134600000000     C                   ENDSR
134700000000     C*------------------------------------------------------------------------*
134800000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
134900000000     C*------------------------------------------------------------------------*
135000000000     C     OUTMSG        BEGSR
135100000000     C* VISUALIZZAZIONE MESSAGGIO
135200000000     C                   MOVEL     '1'           *IN44
135300000000     C                   WRITE     EC76DVK
135400000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
135500000000     C                   Z-ADD     *ZEROS        MSGULT
135600000000     C                   ENDSR
135700000000     C*------------------------------------------------------------------------*
135800000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
135900000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
136000000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
136100000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
136200000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
136300000000     C*------------------------------------------------------------------------*
136400000000     C     CARFUN        BEGSR
136500000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
136600000000     C     1             DO        31            I                 5 0
136700000000     C                   MOVEL     FUN(I)        FUNRIG
136800000000     C                   MOVEL     FUNFUN        FFU(I)
136900000000     C                   MOVEL     FUNDES        FDE(I)
137000020923     C     DUTLPO        IFEQ      'S'
137100000000     C     $TGES         IFEQ      'G'
137200000000     C                   MOVEL     FUNASG        FAT(I)
137300000000     C                   ELSE
137400000000     C                   MOVEL     FUNASI        FAT(I)
137500000000     C                   ENDIF
137600000000     C                   ELSE
137700000000     C     $TGES         IFEQ      'G'
137800000000     C                   MOVEL     FUNAFG        FAT(I)
137900000000     C                   ELSE
138000000000     C                   MOVEL     FUNAFI        FAT(I)
138100000000     C                   ENDIF
138200000000     C                   ENDIF
138300000000     C     FAT(I)        IFEQ      'E'
138400000000     C                   MOVEL     USER          WEDP              3
138500000000     C     WEDP          IFEQ      'EDP'
138600000000     C                   MOVEL     'S'           FAT(I)
138700000000     C                   ELSE
138800000000     C                   MOVEL     'N'           FAT(I)
138900000000     C                   ENDIF
139000000000     C                   ENDIF
139100000000     C                   ENDDO
139200000000     C                   ENDSR
139300000000     C*------------------------------------------------------------------------*
139400000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
139500000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
139600000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
139700000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
139800000000     C*------------------------------------------------------------------------*
139900000000     C     OUTFUN        BEGSR
140000000000     C     1             DO        8             I
140100000000     C                   MOVEL     *BLANKS       FUV(I)
140200000000     C                   ENDDO
140300000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
140400000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
140500000000     C     1             DO        31            I
140600000000     C     FAT(I)        IFEQ      'S'
140700000000     C     FDE(I)        ANDNE     *BLANKS
140800000000     C                   ADD       1             FUNNAT
140900000000     C                   ENDIF
141000000000     C                   ENDDO
141100000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
141200000000     C     FUNNAT        IFGT      8
141300000000     C                   MOVEL     'S'           FAT(24)
141400000000     C                   ENDIF
141500000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
141600000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
141700000000     C                   Z-ADD     8             FUNVIS            5 0
141800000000     C     FAT(23)       IFEQ      'S'
141900000000     C                   SUB       1             FUNVIS
142000000000     C                   ENDIF
142100000000     C     FAT(24)       IFEQ      'S'
142200000000     C                   SUB       1             FUNVIS
142300000000     C                   ENDIF
142400000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
142500000000     C                   Z-ADD     *ZEROS        W                 5 0
142600000000     C                   ADD       1             FUNULT
142700000000     C     FUNULT        IFGT      31
142800000000     C                   Z-ADD     1             FUNULT
142900000000     C                   ENDIF
143000000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
143100000000     C     FUNULT        DO        31            I
143200000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
143300000000     C     W             IFLT      FUNVIS
143400000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
143500000000     C     FAT(I)        IFEQ      'S'
143600000000     C     FDE(I)        ANDNE     *BLANKS
143700000000     C     I             IFEQ      23
143800000000     C     I             OREQ      24
143900000000     C                   ELSE
144000000000     C                   ADD       1             W
144100000000     C                   MOVEL     FDE(I)        FUV(W)
144200000000     C                   ENDIF
144300000000     C                   ENDIF
144400000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
144500000000     C                   Z-ADD     I             FUNULT
144600000000     C                   ENDIF
144700000000     C                   ENDDO
144800000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
144900000000     C                   MOVEL     FUV(1)        VFD001
145000000000     C                   MOVEL     FUV(2)        VFD002
145100000000     C                   MOVEL     FUV(3)        VFD003
145200000000     C                   MOVEL     FUV(4)        VFD004
145300000000     C                   MOVEL     FUV(5)        VFD005
145400000000     C                   MOVEL     FUV(6)        VFD006
145500000000     C     FAT(23)       IFEQ      'N'
145600000000     C     FAT(24)       ANDEQ     'N'
145700000000     C                   MOVEL     FUV(7)        VFD007
145800000000     C                   MOVEL     FUV(8)        VFD008
145900000000     C                   ELSE
146000000000     C     FAT(23)       IFEQ      'S'
146100000000     C     FAT(24)       ANDEQ     'S'
146200000000     C                   MOVEL     FDE(23)       VFD007
146300000000     C                   MOVEL     FDE(24)       VFD008
146400000000     C                   ELSE
146500000000     C                   MOVEL     FUV(7)        VFD007
146600000000     C     FAT(23)       IFEQ      'S'
146700000000     C                   MOVEL     FDE(23)       VFD008
146800000000     C                   ELSE
146900000000     C                   MOVEL     FDE(24)       VFD008
147000000000     C                   ENDIF
147100000000     C                   ENDIF
147200000000     C                   ENDIF
147300000000     C                   MOVEL     '1'           $TESVD
147400000000     C                   ENDSR
147500000000     C*------------------------------------------------------------------------*
147600000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
147700000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
147800000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
147900000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
148000000000     C*------------------------------------------------------------------------*
148100000000     C     CONFUN        BEGSR
148200000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
148300000000     C     1             DO        31            I
148400000000     C     KEY           IFEQ      KEYS(I)
148500000000     C* CONTROLLO SE IL TASTO E' ATTIVO
148600000000     C     FAT(I)        IFNE      'S'
148700000000     C                   MOVEL     MSG(1)        $MSG
148800000000     C                   EXSR      CARMSG
148900000000     C                   MOVEL     '1'           $ERR
149000000000     C                   ENDIF
149100000000     C                   ENDIF
149200000000     C                   ENDDO
149300000000     C                   ENDSR
149400981015** ======== SCHIERA: MSG  (MESSAGGI)                       ================
149500940803TREC76R- Tasto funzionale non valido.                                          1
149600000000TREC76R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
149700000000TREC76R- Opzione non valida.                                                   3
149800000000TREC76R- Opzione abilitata ma non gestita. Avvertire CED.                      4
149900000000TREC76R- Inizio elenco.                                                        5
150000000000TREC76R- Fine elenco.                                                          6
150100000000TREC76R- Immettere codice conto esistente e non annullato                      7
150200000000TREC76R- Immettere data formalmente valida                                     8
150300000000TREC76R- Immettere il codice competenza valido                                 9
150400000000TREC76R- La data scadenza deve essere maggiore o uguale della decorrenza      10
150500000000TREC76R- Immettere riferimento centro di costo esistente e non annullato      11
150600940719TREC76R- Immettere 'D'=Dare oppure 'A'=Avere                                  12
150700000000TREC76R- Immettere una percentuale valida                                     13
150800000000TREC76R- Immettere codice sottoconto esistente e non annullato                14
150900000000TREC76R- Il periodo immesso non e' compreso in quello della competenza        15
151000940719TREC76R- Immettere il riferimento C.D.C. oppure il centro di costo forzato    16
151100000000TREC76R- Inserimento effettuato.                                              17
151200000000TREC76R- Modifica effettuata.                                                 18
151300000000TREC76R- Cancellazione effettuata.                                            19
151400000000TREC76R- Ripristino effettuato.                                               20
151500940722TREC76R- Richiesta funzione per competenza inesistente                        21
151600000000TREC76R- Codice competenza momentaneamente allocato: riprovare più tardi      22
151700000000TREC76R- Sono gia' presenti 10 conti di contabil. per la medesima competenza  23
151800000000TREC76R- La validità della competenza si sovrappone ad una già in decorrenza  24
151900940804TREC76R- Immettere 'F' x competenze franchi, 'A' x assegnati. ' ' per tutte   25
152000940721TREC76R- La suddetta competenza non aggiorna il C/E: non inserire dettaglio   26
152100940804TREC76R- Immettere 'I' x competenze Italia, 'E' x Estere, ' ' per tutte       27
152200940727TREC76R- Centro di costo inesistente                                          28
152300951207TREC76R- Conto di competenza già esistente                                    29
152400940803TREC76R- 30
152500940803TREC76R- 31
152600940803TREC76R- 32
152700940801TREC76R- 33
152800940801TREC76R- 34
152900940801TREC76R- 35
153000940801TREC76R- 36
153100940801TREC76R- 37
153200940801TREC76R- 38
153300940801TREC76R- 39
153400940801TREC76R- 40
153500000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
153600000000F1    -                   -    NN     NN
153700000000F2    -                   -    NN     NN
153800000000F3    -F3=Fine            -    SS     SS
153900000000F4    -                   -    NN     NN
154000000000F5    -                   -    NN     NN
154100000000F6    -F6=Aggiornamento   -    SS     SS
154200000000F7    -                   -    NN     NN
154300000000F8    -                   -    NN     NN
154400000000F9    -                   -    NN     NN
154500000000F10   -                   -    NN     NN
154600000000F11   -                   -    NN     NN
154700000000F12   -F12=Ritorno        -    SS     SS
154800000000F13   -                   -    NN     NN
154900000000F14   -                   -    NN     NN
155000000000F15   -                   -    NN     NN
155100000000F16   -                   -    NN     NN
155200000000F17   -                   -    NN     NN
155300000000F18   -                   -    NN     NN
155400000000F19   -                   -    NN     NN
155500000000F20   -                   -    NN     NN
155600000000F21   -                   -    NN     NN
155700000000F22   -                   -    NN     NN
155800000000F23   -                   -    NN     NN
155900000000F24   -F24=Altri tasti    -    NN     NN
156000000000ENTER -                   -    SS     SS
156100000000ROLLUP-                   -    NN     NN
156200000000ROLLDN-                   -    NN     NN
156300000000HELP  -                   -    NN     NN
156400000000PRINT -                   -    NN     NN
156500000000BSPACE-                   -    NN     NN
156600000000CLEAR -                   -    NN     NN
156700000000** DOPZ  (OPZIONE RICHIAMATA)
156800000000* INSERIMENTO *                                                 1
156900000000*  VARIAZIONE  *                                                2
157000000000*CANCELLAZIONE*                                                 3
157100000000*  RIPRISTINO  *                                                4
157200000000VISUALIZZAZIONE                                                 5
157300981015ANNULLATO                                                       6
1574009810157
1575009810158
1576009810159
15770098101510

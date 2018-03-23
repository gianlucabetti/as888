000100000000     H*------------------------------------------------------------------------*
000200960822     H* GESTIONE BUDGET DI FILIALE - ROUTINE                                   *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800020115     FTISE96D   CF   E             WORKSTN INFDS(DEVDS1)
000900020115     F                                     SFILE(SE96DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FTABEL00F  IF   E           K DISK
001200020115     FTEBDG01L  UF A E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI E FUNZIONI
001800000000     D*-------------------
001900000000     D MSG             S             78    DIM(35) CTDATA PERRCD(1)             *MESSAGGI
002000000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002100000000     D*-------------------
002200000000     D* DECODIFICA OPZIONE RICHIAMATA
002300000000     D*-------------------
002400000000     D DOPZ            S             16    DIM(5) CTDATA PERRCD(1)              *DECODIFICA OPZIONE
002500000000     D*-------------------
002600000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002700000000     D*-------------------
002800000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002900000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZIONE
003000000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003100000000     D*-------------------
003200000000     D* TABELLA DESCRIZIONI A VIDEO
003300000000     D*-------------------
003400000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003500000000     D*-------------------
003600000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003700000000     D*-------------------
003800000000     D KEYS            S              1    DIM(40)
003900000000     D*-------------------
004000000000     D* TABELLA DECODIFICHE DEI MESI
004100000000     D*-------------------
004200000000     D MES             S              9    DIM(12) CTDATA PERRCD(1)             *DECODIFICHE MENSILITA'
004300000000     D*------------------------------------------------------------------------*
004400000000     D* INPUT
004500000000     D*------------------------------------------------------------------------*
004600000000     D*-------------------
004700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004800000000     D*-------------------
004900000000     D DSFUN         E DS
005000000000     D  FUNRIG                 1     40
005100000000     D*-------------------
005200000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005300000000     D*-------------------
005400000000     D DSMSG         E DS
005500000000     D*-------------------
005600000000     D* DS PER GESTIONE TASTI FUNZIONALI
005700000000     D*-------------------
005800000000     D DSKEYS        E DS
005900000000     D  KEYRIG                 1     40
006000000000     D*-------------------
006100000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006200000000     D*-------------------
006300000000     D                SDS
006400000000     D  USER                 254    263
006500000000     D*-------------------
006600000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006700000000     D*-------------------
006800000000     D DEVDS1          DS
006900000000     D  KEY                  369    369
007000000000     D  CURSOR               370    371
007100000000     D  FRTSFL               378    379B 0
007200000000     D*-------------------
007300000000     D* ARCHITETTURA
007400000000     D*-------------------
007500000000     D KPJBA         E DS
007600000000     D*-------------------
007700020916     D* DS REPERIMENTO DATI UTENTE
007800020916     D*-------------------
007900020916     D TIBS34DS      E DS                                                       *Profili utente
008000020916     D DDATIUTE      E DS                                                       *Dati utente
008100020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008200000000     D*---------------------
008300000000     D* DS RIFERIMENTI TARIFFA
008400000000     D*---------------------
008500000000     D DS2Q          E DS
008600000000     D  FLAGS                 31     33
008700000000     D*---------------------
008800000000     D* DS PARAMETRI IN ENTRATA
008900000000     D*---------------------
009000000000     D DS95            DS
009100000000     D  D95OP0                 1      3
009200000000     D  D95OP1                 4      6
009300000000     D  D95F03                 7      7
009400000000     D  D95ERR                 8      8
009500000000     D  D95MSG                 9     88
009600000000     D  D95FIL                89     91  0
009700000000     D  D95ANN                92     95  0
009800000000     D  D95MES                96     97  0
009900000000     D  D95IOE                98     98
010000000000     D  D95FPE                99    100
010100000000     C*------------------------------------------------------------------------*
010200000000     C* MAIN LINE
010300000000     C*------------------------------------------------------------------------*
010400000000     C*
010500000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
010600000000IF  1C     $FINE         IFEQ      '1'
010700000000     C*
010800000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
010900000000     C                   MOVEL     '1'           D95ERR
011000000000     C*
011100000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
011200000000X   1C                   ELSE
011300000000     C*
011400000000     C* CICLO FINO A CHE:
011500000000DO  2C     $FINE         DOWEQ     '0'
011600000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
011700000000     C*
011800000000     C* CONTROLLA GESTIONE VIDEO
011900000000IF  3C     $GES          IFEQ      'V1'
012000000000     C                   EXSR      GESV1                                        *MANUTENZIONE TEMPO
012100000000E   3C                   ENDIF
012200000000     C*
012300000000E   2C                   ENDDO
012400000000E   1C                   ENDIF
012500000000     C*
012600000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
012700000000     C     $F03          IFEQ      '1'
012800000000     C                   MOVEL     '1'           D95F03
012900000000E   1C                   ENDIF
013000000000     C* FINE PGM
013100000000     C                   MOVEL     DS95          KPJBU
013200000000     C                   SETON                                        LR
013300000000     C*------------------------------------------------------------------------*
013400000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
013500000000     C*------------------------------------------------------------------------*
013600000000     C     CARV1         BEGSR
013700000000     C*
013800000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013900000000     C*
014000000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014100000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014200000000     C*
014300020115     C                   WRITE     SE96DVT                                      *TESTATA
014400020115     C                   WRITE     SE96DVF                                      *FUNZIONI
014500000000     C*
014600000000     C                   ENDSR
014700000000     C*------------------------------------------------------------------------*
014800000000     C* GESV1  - GESTIONE PRIMO VIDEO
014900000000     C*------------------------------------------------------------------------*
015000000000     C     GESV1         BEGSR
015100000000     C*
015200000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
015300000000     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
015400000000E   1C                   ENDIF
015500000000     C*
015600000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGGI
015700000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
015800000000E   1C                   ENDIF
015900000000     C*
016000020115     C                   EXFMT     SE96DV1                                      *TEMPO DELIVERY
016100000000     C*
016200000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
016300000000     C*
016400000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
016500000000SL  2C                   SELECT
016600000000     C     KEY           WHENEQ    ENTER
016700000000     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
016800000000     C     KEY           WHENEQ    F6
016900000000     C                   EXSR      F06GES                                       *GEST F6 CONFERMA
017000000000     C                   OTHER
017100000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
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
018500000000IF  1C     D95OP1        IFEQ      'O05'
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
019600000000IF  1C     D95OP1        IFEQ      'F06'
019700000000     C                   EXSR      INSERI                                       *INSERIMENTO
019800000000E   1C                   ENDIF
019900000000IF  1C     D95OP1        IFEQ      'O02'
020000000000     C                   EXSR      MODIFI                                       *MODIFICA
020100000000E   1C                   ENDIF
020200000000IF  1C     D95OP1        IFEQ      'O04'
020300000000     C                   EXSR      ANNULL                                       *ANNULLAMENTO
020400000000E   1C                   ENDIF
020500000000IF  1C     D95OP1        IFEQ      'O07'
020600000000     C                   EXSR      RIPRIS                                       *RIPRISTINO
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
021800000000IF  1C     $ERR          IFEQ      '0'                                          *NON CI SONO ERRORI
021900000000     C*
022000000000     C                   Z-ADD     V1CFIL        TBGFIL                         *FILIALE
022100000000     C                   Z-ADD     V1CANN        TBGANN                         *ANNO
022200000000     C                   Z-ADD     V1CMES        TBGMES                         *MESE
022300960115     C                   MOVEL     V1CIOE        TBGIOE                         *ITA/IMP/EXP
022400000000     C                   MOVEL     V1CFPE        TBGFPE                         *FASCIA PESO
022500000000     C                   Z-ADD     V1CNSP        TBGNSP
022600000000     C                   Z-ADD     V1CNSA        TBGNSA
022700000000     C                   Z-ADD     V1CKGP        TBGKGP
022800000000     C                   Z-ADD     V1CKGA        TBGKGA
022900000000     C                   Z-ADD     V1CNCP        TBGNCP
023000000000     C                   Z-ADD     V1CNCA        TBGNCA
023100000000     C                   Z-ADD     V1CVOP        TBGVOP
023200000000     C                   Z-ADD     V1CVOA        TBGVOA
023300000000     C                   Z-ADD     V1CIRB        TBGIRB
023400000000     C                   Z-ADD     V1CINC        TBGINC
023500000000     C                   Z-ADD     V1CIRR        TBGIRR
023600000000     C*
023700000000     C                   MOVEL     *BLANKS       TBGATB                         *ANNULLAMENTO
023800000000     C*
023900020115     C                   WRITE     TEBDG
024000000000     C*
024100000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
024200000000     C                   MOVEL     '1'           $FINE
024300000000     C*
024400000000     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
024500000000     C                   MOVEL     MSG(17)       D95MSG
024600000000     C*
024700000000E   1C                   ENDIF
024800000000     C*
024900000000     C                   ENDSR
025000000000     C*------------------------------------------------------------------------*
025100000000     C* MODIFI - FUNZIONE DI MODIFICA
025200000000     C*------------------------------------------------------------------------*
025300000000     C     MODIFI        BEGSR
025400000000     C*
025500000000     C* CONTROLLA I CAMPI IMMESSI
025600000000     C                   EXSR      DECODI
025700000000     C*
025800000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
025900000000     C                   Z-ADD     D95FIL        KDGFIL                         *FILIALE
026000000000     C                   Z-ADD     D95ANN        KDGANN                         *ANNO
026100000000     C                   Z-ADD     D95MES        KDGMES                         *MESE
026200960115     C                   MOVEL     D95IOE        KDGIOE                         *ITA/IMP/EXP
026300000000     C                   MOVEL     D95FPE        KDGFPE                         *FASCIA PESO
026400020115     C     KEYBDG        CHAIN     TEBDG01L                           99
026500000000IF  1C     *IN99         IFEQ      *OFF
026600000000     C*
026700000000IF  2C     $ERR          IFEQ      '0'
026800000000     C*
026900000000     C                   Z-ADD     V1CFIL        TBGFIL                         *FILIALE
027000000000     C                   Z-ADD     V1CANN        TBGANN                         *ANNO
027100000000     C                   Z-ADD     V1CMES        TBGMES                         *MESE
027200960115     C                   MOVEL     V1CIOE        TBGIOE                         *ITA/IMP/EXP
027300000000     C                   MOVEL     V1CFPE        TBGFPE                         *FASCIA PESO
027400000000     C                   Z-ADD     V1CNSP        TBGNSP
027500000000     C                   Z-ADD     V1CNSA        TBGNSA
027600000000     C                   Z-ADD     V1CKGP        TBGKGP
027700000000     C                   Z-ADD     V1CKGA        TBGKGA
027800000000     C                   Z-ADD     V1CNCP        TBGNCP
027900000000     C                   Z-ADD     V1CNCA        TBGNCA
028000000000     C                   Z-ADD     V1CVOP        TBGVOP
028100000000     C                   Z-ADD     V1CVOA        TBGVOA
028200000000     C                   Z-ADD     V1CIRB        TBGIRB
028300000000     C                   Z-ADD     V1CINC        TBGINC
028400000000     C                   Z-ADD     V1CIRR        TBGIRR
028500000000     C*
028600000000     C                   MOVEL     *BLANKS       TBGATB                         *ANNULLAMENTO
028700000000     C*
028800020115     C                   UPDATE    TEBDG
028900000000     C*
029000000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
029100000000     C                   MOVEL     '1'           $FINE
029200000000     C*
029300000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
029400000000     C                   MOVEL     MSG(18)       D95MSG
029500000000     C*
029600000000E   2C                   ENDIF
029700000000E   1C                   ENDIF
029800000000     C*
029900000000     C                   ENDSR
030000000000     C*------------------------------------------------------------------------*
030100000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
030200000000     C*------------------------------------------------------------------------*
030300000000     C     ANNULL        BEGSR
030400000000     C*
030500000000     C* CONTROLLA I CAMPI IMMESSI
030600000000     C                   EXSR      DECODI
030700000000     C*
030800000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
030900000000     C                   Z-ADD     D95FIL        KDGFIL                         *FILIALE
031000000000     C                   Z-ADD     D95ANN        KDGANN                         *ANNO
031100960108     C                   Z-ADD     D95MES        KDGMES                         *MESE
031200960115     C                   MOVEL     D95IOE        KDGIOE                         *ITA/IMP/EXP
031300000000     C                   MOVEL     D95FPE        KDGFPE                         *FASCIA PESO
031400020115     C     KEYBDG        CHAIN     TEBDG01L                           99
031500000000IF  1C     *IN99         IFEQ      *OFF
031600000000     C*
031700000000IF  2C     $ERR          IFEQ      '0'
031800000000     C*--------------------
031900000000     C* ANNULLO
032000000000     C*--------------------
032100000000     C                   MOVEL     'A'           TBGATB                         * 'A'=ANNULLAMENTO
032200020115     C                   UPDATE    TEBDG
032300000000     C*
032400000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
032500000000     C                   MOVEL     '1'           $FINE
032600000000     C*
032700000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
032800000000     C                   MOVEL     MSG(19)       D95MSG
032900000000     C*
033000000000E   2C                   ENDIF
033100000000E   1C                   ENDIF
033200000000     C*
033300000000     C                   ENDSR
033400000000     C*------------------------------------------------------------------------*
033500000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
033600000000     C*------------------------------------------------------------------------*
033700000000     C     RIPRIS        BEGSR
033800000000     C*
033900000000     C* CONTROLLA I CAMPI IMMESSI
034000000000     C                   EXSR      DECODI
034100000000     C*
034200000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
034300000000     C                   Z-ADD     D95FIL        KDGFIL                         *FILIALE
034400000000     C                   Z-ADD     D95ANN        KDGANN                         *ANNO
034500000000     C                   Z-ADD     D95MES        KDGMES                         *MESE
034600960115     C                   MOVEL     D95IOE        KDGIOE                         *ITA/IMP/EXP
034700000000     C                   MOVEL     D95FPE        KDGFPE                         *FASCIA PESO
034800020115     C     KEYBDG        CHAIN     TEBDG01L                           99
034900000000IF  1C     *IN99         IFEQ      *OFF
035000000000     C*
035100000000IF  2C     $ERR          IFEQ      '0'
035200000000     C*--------------------
035300000000     C* RIPRISTINO
035400000000     C*--------------------
035500000000     C                   MOVEL     *BLANKS       TBGATB                         *RIPRISTINO
035600020115     C                   UPDATE    TEBDG
035700000000     C*
035800000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
035900000000     C                   MOVEL     '1'           $FINE
036000000000     C*
036100000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
036200000000     C                   MOVEL     MSG(20)       D95MSG
036300000000     C*
036400000000E   2C                   ENDIF
036500000000E   1C                   ENDIF
036600000000     C*
036700000000     C                   ENDSR
036800000000     C*------------------------------------------------------------------------*
036900000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
037000000000     C*------------------------------------------------------------------------*
037100000000     C     DECODI        BEGSR
037200000000     C*
037300000000     C* REIMPOSTO INDICATORI DI ERRORE
037400000000     C                   SETOFF                                       606162
037500960108     C                   SETOFF                                       6364
037600000000     C*--------------------
037700000000     C* CODICE FILIALE
037800000000     C*--------------------
037900000000     C     V1CFIL        CHAIN     AZORG01L                           60
038000000000     C     *IN60         IFEQ      *ON
038100000000     C                   MOVEL     '1'           $ERR
038200000000     C                   MOVEL     MSG(7)        $MSG
038300000000     C                   EXSR      CARMSG
038400000000     C                   SETON                                        60
038500000000     C                   MOVEL     *BLANKS       V1DFIL
038600000000     C                   ELSE
038700000000     C                   MOVEL     ORGDES        V1DFIL
038800000000E   1C                   ENDIF
038900960108     C*--------------------
039000960108     C* ANNO
039100960108     C*--------------------
039200960108IF  1C     $ERR          IFEQ      '0'
039300960108IF  2C     V1CANN        IFEQ      *ZEROS
039400960108     C                   MOVEL     '1'           $ERR
039500960108     C                   MOVEL     MSG(8)        $MSG
039600960108     C                   EXSR      CARMSG
039700960108     C                   SETON                                        61
039800960108E   2C                   ENDIF
039900960108E   1C                   ENDIF
040000000000     C*--------------------
040100000000     C* MESE
040200000000     C*--------------------
040300000000IF  1C     $ERR          IFEQ      '0'
040400000000     C                   MOVEL     *BLANKS       V1DMES                         *DECOD. MESE
040500000000IF  2C     V1CMES        IFLT      1
040600000000IF  2C     V1CMES        ORGT      12
040700000000     C                   MOVEL     '1'           $ERR
040800000000     C                   MOVEL     MSG(9)        $MSG
040900000000     C                   EXSR      CARMSG
041000000000     C                   SETON                                        62
041100000000     C                   ELSE
041200000000     C                   Z-ADD     V1CMES        $A                2 0
041300000000     C                   MOVEL     MES($A)       V1DMES                         *DECOD. MESE
041400000000E   2C                   ENDIF
041500000000E   1C                   ENDIF
041600960108     C*--------------------
041700960115     C* ITA/IMP/EXP
041800960108     C*--------------------
041900960108IF  1C     $ERR          IFEQ      '0'
042000960115IF  2C     V1CIOE        IFEQ      'I'
042100960115     C     V1CIOE        OREQ      'M'
042200960115     C     V1CIOE        OREQ      'X'
042300960108X   2C                   ELSE
042400960108     C                   MOVEL     '1'           $ERR
042500960108     C                   MOVEL     MSG(11)       $MSG
042600960108     C                   EXSR      CARMSG
042700960108     C                   SETON                                        64
042800960108E   2C                   ENDIF
042900960108E   1C                   ENDIF
043000000000     C*--------------------
043100000000     C* FASCIA DI PESO
043200000000     C*--------------------
043300000000IF  1C     $ERR          IFEQ      '0'
043400020916     C                   MOVEL     1             KTBKUT                         *CODICE UTENTE
043500000000     C                   MOVEL     '2L'          KTBCOD                         *CODICE TABELLA
043600990217IF  1C     V1CANN        IFLE      1998                                         *FASCE FINO AL 98
043700990217     C                   MOVEL     'SDR'         $COMOD            5            *CHIAVE COMPOSTA
043800990217X   1C                   ELSE                                                   *FASCE DOPO IL 99
043900020328     C                   MOVEL     'S9R'         $COMOD
044000020325E   1C                   ENDIF
044100000000     C                   MOVE      V1CFPE        $COMOD
044200000000     C                   MOVE      *BLANKS       KTBKEY
044300000000     C                   MOVEL     $COMOD        KTBKEY                         *CODICE TENTATIVO
044400000000     C     KEYTAB        CHAIN     TABEL00F                           99
044500000000     C     *IN99         IFEQ      '1'
044600000000     C     TBLFLG        ORNE      *BLANKS
044700000000     C                   MOVEL     '1'           $ERR
044800000000     C                   MOVEL     MSG(10)       $MSG
044900000000     C                   EXSR      CARMSG
045000000000     C                   SETON                                        63
045100000000     C                   MOVEL     *BLANKS       V1DFPE                         *DESCRIZIONE
045200000000     C                   ELSE
045300000000     C                   MOVEL     TBLUNI        V1DFPE                         *DESCRIZIONE
045400000000     C                   ENDIF
045500000000E   1C                   ENDIF
045600000000     C*
045700000000     C                   ENDSR
045800000000     C*------------------------------------------------------------------------*
045900000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
046000000000     C*------------------------------------------------------------------------*
046100000000     C     RECVID        BEGSR
046200000000     C*
046300000000     C                   Z-ADD     D95FIL        V1CFIL                         *FILIALE
046400000000     C                   Z-ADD     D95ANN        V1CANN                         *ANNO
046500000000     C                   Z-ADD     D95MES        V1CMES                         *MESE
046600960115     C                   MOVEL     D95IOE        V1CIOE                         *ITA/IMP/EXP
046700960108     C                   MOVEL     D95FPE        V1CFPE                         *FASCIA PESO
046800000000     C                   Z-ADD     TBGNSP        V1CNSP
046900000000     C                   Z-ADD     TBGNSA        V1CNSA
047000000000     C                   Z-ADD     TBGKGP        V1CKGP
047100000000     C                   Z-ADD     TBGKGA        V1CKGA
047200000000     C                   Z-ADD     TBGNCP        V1CNCP
047300000000     C                   Z-ADD     TBGNCA        V1CNCA
047400000000     C                   Z-ADD     TBGVOP        V1CVOP
047500000000     C                   Z-ADD     TBGVOA        V1CVOA
047600000000     C                   Z-ADD     TBGIRB        V1CIRB
047700000000     C                   Z-ADD     TBGINC        V1CINC
047800000000     C                   Z-ADD     TBGIRR        V1CIRR
047900000000     C*
048000000000     C                   ENDSR
048100000000     C*------------------------------------------------------------------------*
048200000000     C*  AZZVID - PULISCE TUTTI ICAMPI DEL VIDEO
048300000000     C*------------------------------------------------------------------------*
048400000000     C     AZZVID        BEGSR
048500000000     C*
048600000000     C                   Z-ADD     *ZERO         V1CFIL                         *FILIALE
048700000000     C                   Z-ADD     *ZERO         V1CANN                         *ANNO
048800000000     C                   Z-ADD     *ZERO         V1CMES                         *MESE
048900960115     C                   MOVEL     *BLANKS       V1CIOE                         *ITA/IMP/EXP
049000000000     C                   MOVEL     *BLANKS       V1CFPE                         *FASCIA PESO
049100000000     C                   Z-ADD     *ZERO         V1CNSP
049200000000     C                   Z-ADD     *ZERO         V1CNSA
049300000000     C                   Z-ADD     *ZERO         V1CKGP
049400000000     C                   Z-ADD     *ZERO         V1CKGA
049500000000     C                   Z-ADD     *ZERO         V1CNCP
049600000000     C                   Z-ADD     *ZERO         V1CNCA
049700000000     C                   Z-ADD     *ZERO         V1CVOP
049800000000     C                   Z-ADD     *ZERO         V1CVOA
049900000000     C                   Z-ADD     *ZERO         V1CIRB
050000000000     C                   Z-ADD     *ZERO         V1CINC
050100000000     C                   Z-ADD     *ZERO         V1CIRR
050200000000     C*
050300000000     C                   ENDSR
050400020916     C*--------------------------------------------------------------------------------------------*
050500020916     C* REPERISCE I DATI UTENTE
050600020916     C*--------------------------------------------------------------------------------------------*
050700020916     C     REPDATIUTE    BEGSR
050800020916     C*
050900020916     C* INIZIALIZZA VARIABILI DI WRK
051000020916     C                   CLEAR                   TIBS34DS
051100020916     C                   CLEAR                   AZUTEDS
051200020916     C                   CLEAR                   DDATIUTE
051300020916     C*
051400020916     C     *DTAARA       DEFINE    §azute        azuteds
051500020916     C     *DTAARA       DEFINE    §datiute      ddatiute
051600020916     C                   IN(E)     *DTAARA
051700020916if  1C                   IF        %Error
051800020916     c                   EVAL      I34Tla = 'L'
051900020916     C                   CALL      'TIBS34R'
052000020916     C                   PARM                    Tibs34Ds
052100020916     C                   IN        *DTAARA
052200020916e   1C                   ENDIF
052300020916     C*
052400020916     C                   ENDSR
052500000000     C*------------------------------------------------------------------------*
052600000000     C*  *INZSR - ROUTINE INIZIALE
052700000000     C*------------------------------------------------------------------------*
052800000000     C     *INZSR        BEGSR
052900000000     C*
053000000000     C* RICEVIMENTO PARAMETRI
053100000000     C     *ENTRY        PLIST
053200000000     C                   PARM                    KPJBA
053300000000     C                   MOVEL     KPJBU         DS95
053400000000     C*
053500000000     C* DEFINIZIONE VARIABILI
053600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
053700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
053800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
053900020115     C     *LIKE         DEFINE    TBGFIL        KDGFIL                         *TEBDG00F
054000000000     C     *LIKE         DEFINE    TBGANN        KDGANN
054100000000     C     *LIKE         DEFINE    TBGMES        KDGMES
054200960108     C     *LIKE         DEFINE    TBGIOE        KDGIOE
054300960108     C     *LIKE         DEFINE    TBGFPE        KDGFPE
054400000000     C*
054500020115     C* CHIAVE LETTURA TEBDG01L - COMPLETA
054600000000     C     KEYBDG        KLIST
054700000000     C                   KFLD                    KDGFIL                         *FILIALE
054800000000     C                   KFLD                    KDGANN                         *ANNO
054900000000     C                   KFLD                    KDGMES                         *MESE
055000960115     C                   KFLD                    KDGIOE                         *ITA/IMP/EXP
055100960108     C                   KFLD                    KDGFPE                         *FASCIA PESO
055200000000     C*
055300000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
055400000000     C     KEYTAB        KLIST
055500000000     C                   KFLD                    KTBKUT
055600000000     C                   KFLD                    KTBCOD
055700000000     C                   KFLD                    KTBKEY
055800000000     C*
055900000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
056000000000     C                   MOVE      D95OP0        INTGES            2
056100000000IF  1C     INTGES        IFEQ      '02'
056200000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
056300000000X   1C                   ELSE
056400000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
056500000000E   1C                   ENDIF
056600000000     C*
056700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
056800000000     C                   EXSR      INZSTD
056900000000     C*
057000000000     C* RAGIONE SOCIALE UTENTE
057100020916     C                   MOVEL     RSUT          VTDRSU
057200000000     C*
057300000000     C* DECODIFICA OPZIONE RICHIAMATA
057400000000IF  1C     D95OP1        IFEQ      'F06'
057500000000     C                   MOVEL     DOPZ(1)       VTDFUN                         *INSERIMENTO
057600000000E   1C                   ENDIF
057700000000IF  2C     D95OP1        IFEQ      'O02'
057800000000     C                   MOVEL     DOPZ(2)       VTDFUN                         *VARIAZIONE
057900000000E   1C                   ENDIF
058000000000IF  1C     D95OP1        IFEQ      'O04'
058100000000     C                   MOVEL     DOPZ(3)       VTDFUN                         *CANCELLAZIONE
058200000000E   1C                   ENDIF
058300000000IF  1C     D95OP1        IFEQ      'O07'
058400000000     C                   MOVEL     DOPZ(4)       VTDFUN                         *RIPRISTINO
058500000000E   1C                   ENDIF
058600000000IF  1C     D95OP1        IFEQ      'O05'
058700000000     C                   MOVEL     DOPZ(5)       VTDFUN                         *VISUALIZZAZIONE
058800000000E   1C                   ENDIF
058900000000     C*
059000000000     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
059100000000     C* 0=NON PROTETTO, 1=PROTETTO
059200000000     C*   90 - CAMPI CHIAVE
059300000000     C*   91 - ALTRI CAMPI
059400000000IF  1C     D95OP1        IFEQ      'F06'                                        *INSERIMENTO
059500000000     C                   SETOFF                                       9091      *NESSUNA PROTEZIONE
059600000000E   1C                   ENDIF
059700000000IF  1C     D95OP1        IFEQ      'O02'                                        *MODIFICA
059800000000     C                   SETON                                        90        *CHIAVE PROTETTA
059900000000E   1C                   ENDIF
060000000000IF  1C     D95OP1        IFEQ      'O04'                                        *CANCELLA/RIPRISTINO
060100000000     C     D95OP1        OREQ      'O07'                                        *CANCELLA/RIPRISTINO
060200000000     C     D95OP1        OREQ      'O05'
060300000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
060400000000E   1C                   ENDIF
060500000000     C*
060600000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
060700000000IF  1C     D95OP1        IFEQ      'O05'
060800000000     C                   MOVEL     FUN(6)        FUNRIG                         *AGGIORNAMENTO
060900000000     C                   MOVEL     'N'           FUNASG
061000000000     C                   MOVEL     'N'           FUNASI
061100000000     C                   MOVEL     'N'           FUNAFG
061200000000     C                   MOVEL     'N'           FUNAFG
061300000000     C                   MOVEL     FUNRIG        FUN(6)
061400000000E   1C                   ENDIF
061500000000     C*
061600000000     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
061700000000IF  1C     D95OP1        IFEQ      'O02'
061800000000     C     D95OP1        OREQ      'O04'
061900000000     C     D95OP1        OREQ      'O05'
062000000000     C     D95OP1        OREQ      'O07'
062100000000     C                   Z-ADD     D95FIL        KDGFIL                         *FILIALE
062200000000     C                   Z-ADD     D95ANN        KDGANN                         *ANNO
062300000000     C                   Z-ADD     D95MES        KDGMES                         *MESE
062400960115     C                   MOVEL     D95IOE        KDGIOE                         *ITA/IMP/EXP
062500000000     C                   MOVEL     D95FPE        KDGFPE                         *FASCIA PESO
062600000000     C*
062700000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
062800020115     C     KEYBDG        CHAIN     TEBDG01L                           9998
062900000000IF  2C     *IN99         IFEQ      '1'                                          *NON TROVATO
063000000000     C                   MOVEL     '1'           $ERR
063100000000     C                   MOVEL     '1'           $FINE
063200000000     C                   MOVEL     MSG(21)       D95MSG
063300000000X   2C                   ELSE
063400000000IF  3C     *IN98         IFEQ      '1'                                          *ALLOCAZIONE
063500000000     C                   MOVEL     '1'           $ERR
063600000000     C                   MOVEL     '1'           $FINE
063700000000     C                   MOVEL     MSG(22)       D95MSG
063800000000X   3C                   ELSE
063900000000     C*
064000000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
064100000000     C                   EXSR      CARV1                                        CARICA TESTA/FUNZIONI
064200000000E   3C                   ENDIF
064300000000E   2C                   ENDIF
064400000000E   1C                   ENDIF
064500000000     C*
064600000000     C* SE INSERIMENTO
064700000000IF  1C     D95OP1        IFEQ      'F06'
064800000000     C                   EXSR      AZZVID                                       *AZZERA CAMPI VIDEO
064900000000     C                   EXSR      CARV1                                        CARICA TESTA/FUNZIONI
065000000000     C                   ELSE
065100000000     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
065200000000E   1C                   ENDIF
065300000000     C*
065400000000     C                   ENDSR
065500000000     C*------------------------------------------------------------------------*
065600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
065700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
065800000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
065900000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
066000000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
066100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066300000000     C*------------------------------------------------------------------------*
066400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
066500000000     C*------------------------------------------------------------------------*
066600000000     C     INZSTD        BEGSR
066700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
066800000000     C                   CALL      'XKEYS'
066900000000     C                   PARM                    DSKEYS
067000000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
067100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
067200000000     C     XMSGPA        PLIST
067300000000     C                   PARM                    DSMSMS
067400000000     C                   PARM                    DSMSPG
067500000000     C                   PARM                    DSMSKE
067600000000     C                   PARM                    DSMSER
067700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
067800020115     C                   MOVEL     'TISE96R'     DSMSPG                         *NOME PGM CHIAMANTE
067900020115     C                   MOVEL     'TISE96R'     VMCQUE                         *CODA MESSAGGI DESTIN
068000000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
068100000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
068200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
068300000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
068400000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
068500000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
068600000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
068700000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
068800000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
068900000000     C* LANCIO ROUTINE PER VERIFICARE SE
069000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
069100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
069200020916     C                   EXSR      REPDATIUTE
069300000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
069400000000     C                   BITOFF    '01234567'    POSMI1            1
069500000000     C                   BITOFF    '01234567'    POSMI2            1
069600000000     C                   BITOFF    '01234567'    POSMA1            1
069700000000     C                   BITOFF    '01234567'    POSMA2            1
069800000000     C                   BITON     '34'          POSMI1            1            *18
069900000000     C                   BITON     '7'           POSMI2            1            *01
070000000000     C                   BITON     '34'          POSMA1            1            *18
070100000000     C                   BITON     '0'           POSMA2            1            *80
070200000000     C                   MOVE      POSMI2        POSMIN            2
070300000000     C                   MOVEL     POSMI1        POSMIN
070400000000     C                   MOVE      POSMA2        POSMAX            2
070500000000     C                   MOVEL     POSMA1        POSMAX
070600000000     C                   ENDSR
070700000000     C*------------------------------------------------------------------------*
070800000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
070900000000     C*------------------------------------------------------------------------*
071000000000     C     FUNSTD        BEGSR
071100000000     C                   SELECT
071200000000     C     KEY           WHENEQ    F3
071300000000     C                   EXSR      F03STD
071400000000     C     KEY           WHENEQ    F12
071500000000     C                   EXSR      F12STD
071600000000     C     KEY           WHENEQ    F24
071700000000     C                   EXSR      F24STD
071800000000     C                   OTHER
071900000000     C                   MOVEL     MSG(2)        $MSG
072000000000     C                   EXSR      CARMSG
072100000000     C                   ENDSL
072200000000     C                   ENDSR
072300000000     C*------------------------------------------------------------------------*
072400000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
072500000000     C*------------------------------------------------------------------------*
072600000000     C     F03STD        BEGSR
072700000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
072800000000     C                   MOVEL     '1'           $F03
072900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
073000000000     C                   MOVEL     '1'           $FINE
073100000000     C                   ENDSR
073200000000     C*------------------------------------------------------------------------*
073300000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
073400000000     C*------------------------------------------------------------------------*
073500000000     C     F12STD        BEGSR
073600000000     C* IMPOSTO FLAG DI FIN PGM
073700000000     C                   MOVEL     '1'           $FINE
073800000000     C                   ENDSR
073900000000     C*------------------------------------------------------------------------*
074000000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
074100000000     C*------------------------------------------------------------------------*
074200000000     C     F24STD        BEGSR
074300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
074400000000     C                   EXSR      OUTFUN
074500000000     C                   ENDSR
074600000000     C*------------------------------------------------------------------------*
074700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
074800000000     C*          CODA MESSAGGI DEL PGM
074900000000     C*------------------------------------------------------------------------*
075000000000     C     CARMSG        BEGSR
075100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
075200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
075300000000     C     MSGULT        IFEQ      0
075400000000     C                   MOVEL     '1'           *IN40
075500020115     C                   WRITE     SE96DVK
075600000000     C                   MOVEL     '0'           *IN40
075700000000     C                   ENDIF
075800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
075900000000     C                   MOVEL     $MSG          DSMSMS
076000000000     C                   CALL      'XMSG'        XMSGPA
076100000000     C     DSMSER        IFEQ      0
076200000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
076300000000     C                   ENDIF
076400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
076500000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
076600020115     C                   WRITE     SE96DVM                                      *SFL MESS
076700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
076800000000     C                   MOVEL     *BLANKS       $MSG
076900000000     C                   ENDSR
077000000000     C*------------------------------------------------------------------------*
077100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
077200000000     C*------------------------------------------------------------------------*
077300000000     C     OUTMSG        BEGSR
077400000000     C* VISUALIZZAZIONE MESSAGGIO
077500000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
077600020115     C                   WRITE     SE96DVK                                      *CONTR MESS
077700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
077800000000     C                   Z-ADD     *ZEROS        MSGULT
077900000000     C                   ENDSR
078000000000     C*------------------------------------------------------------------------*
078100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
078200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
078300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
078400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
078500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
078600000000     C*------------------------------------------------------------------------*
078700000000     C     CARFUN        BEGSR
078800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
078900000000     C     1             DO        31            I                 5 0          --- 1 -->
079000000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
079100000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
079200000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
079300020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
079400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
079500000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
079600000000     C                   ELSE                                                   *INTERROG.
079700000000     C                   MOVEL     FUNASI        FAT(I)
079800000000     C                   ENDIF
079900000000     C                   ELSE                                                   <-- 2 --> *FILIALE
080000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
080100000000     C                   MOVEL     FUNAFG        FAT(I)
080200000000     C                   ELSE                                                   *INTERROG.
080300000000     C                   MOVEL     FUNAFI        FAT(I)
080400000000     C                   ENDIF
080500000000     C                   ENDIF                                                  <-- 2 ---
080600000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
080700000000     C                   MOVEL     USER          WEDP              3            *PROFILO
080800000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
080900000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
081000000000     C                   ELSE
081100000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
081200000000     C                   ENDIF
081300000000     C                   ENDIF
081400000000     C                   ENDDO                                                  <-- 1 ---
081500000000     C                   ENDSR
081600000000     C*------------------------------------------------------------------------*
081700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
081800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
081900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
082000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
082100000000     C*------------------------------------------------------------------------*
082200000000     C     OUTFUN        BEGSR
082300000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
082400000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
082500000000     C                   ENDDO
082600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
082700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
082800000000     C     1             DO        31            I
082900000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
083000000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
083100000000     C                   ADD       1             FUNNAT
083200000000     C                   ENDIF
083300000000     C                   ENDDO
083400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
083500000000     C     FUNNAT        IFGT      8
083600000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
083700000000     C                   ENDIF
083800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
083900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
084000000000     C                   Z-ADD     8             FUNVIS            5 0
084100000000     C     FAT(23)       IFEQ      'S'
084200000000     C                   SUB       1             FUNVIS
084300000000     C                   ENDIF
084400000000     C     FAT(24)       IFEQ      'S'
084500000000     C                   SUB       1             FUNVIS
084600000000     C                   ENDIF
084700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
084800000000     C                   Z-ADD     *ZEROS        W                 5 0
084900000000     C                   ADD       1             FUNULT
085000000000     C     FUNULT        IFGT      31
085100000000     C                   Z-ADD     1             FUNULT
085200000000     C                   ENDIF
085300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
085400000000     C     FUNULT        DO        31            I                              --- 1 -->
085500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
085600000000     C     W             IFLT      FUNVIS                                       --- 2 -->
085700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
085800000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
085900000000     C     FDE(I)        ANDNE     *BLANKS
086000000000     C     I             IFEQ      23                                           *TOLGO F23
086100000000     C     I             OREQ      24                                           *E F24
086200000000     C                   ELSE
086300000000     C                   ADD       1             W
086400000000     C                   MOVEL     FDE(I)        FUV(W)
086500000000     C                   ENDIF
086600000000     C                   ENDIF                                                  <-- 3 ---
086700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
086800000000     C                   Z-ADD     I             FUNULT
086900000000     C                   ENDIF                                                  <-- 2 ---
087000000000     C                   ENDDO                                                  <-- 1 ---
087100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
087200000000     C                   MOVEL     FUV(1)        VFD001
087300000000     C                   MOVEL     FUV(2)        VFD002
087400000000     C                   MOVEL     FUV(3)        VFD003
087500000000     C                   MOVEL     FUV(4)        VFD004
087600000000     C                   MOVEL     FUV(5)        VFD005
087700000000     C                   MOVEL     FUV(6)        VFD006
087800000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
087900000000     C     FAT(24)       ANDEQ     'N'
088000000000     C                   MOVEL     FUV(7)        VFD007
088100000000     C                   MOVEL     FUV(8)        VFD008
088200000000     C                   ELSE                                                   <-- 1 -->
088300000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
088400000000     C     FAT(24)       ANDEQ     'S'
088500000000     C                   MOVEL     FDE(23)       VFD007
088600000000     C                   MOVEL     FDE(24)       VFD008
088700000000     C                   ELSE                                                   <-- 2 -->
088800000000     C                   MOVEL     FUV(7)        VFD007
088900000000     C     FAT(23)       IFEQ      'S'
089000000000     C                   MOVEL     FDE(23)       VFD008
089100000000     C                   ELSE
089200000000     C                   MOVEL     FDE(24)       VFD008
089300000000     C                   ENDIF
089400000000     C                   ENDIF                                                  <-- 2 ---
089500000000     C                   ENDIF                                                  <-- 1 ---
089600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
089700000000     C                   ENDSR
089800000000     C*------------------------------------------------------------------------*
089900000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
090000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
090100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
090200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
090300000000     C*------------------------------------------------------------------------*
090400000000     C     CONFUN        BEGSR
090500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
090600000000     C     1             DO        31            I                              --- 1 -->
090700000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
090800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
090900000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
091000000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
091100000000     C                   EXSR      CARMSG
091200000000     C                   MOVEL     '1'           $ERR
091300000000     C                   ENDIF                                                  <-- 3 ---
091400000000     C                   ENDIF                                                  <-- 2 ---
091500000000     C                   ENDDO                                                  <-- 1 ---
091600000000     C                   ENDSR
091700000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
091800020115TISE96R- Tasto funzionale non valido.                                          1
091900020115TISE96R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
092000020115TISE96R- Opzione non valida.                                                   3
092100020115TISE96R- Opzione abilitata ma non gestita. Avvertire CED.                      4
092200020115TISE96R- Inizio elenco.                                                        5
092300020115TISE96R- Fine elenco.                                                          6
092400020115TISE96R- Immettere codice filiale valido                                       7
092500020115TISE96R- Immettere l'anno                                                      8
092600020115TISE96R- Immettere il mese valido                                              9
092700020115TISE96R- Immettere fascia di peso esistente e non annullata                   10
092800020115TISE96R- Immettere uno dei valori possibili                                   11
092900020115TISE96R- 12
093000020115TISE96R- 13
093100020115TISE96R- 14
093200020115TISE96R- 15
093300020115TISE96R- 16
093400020115TISE96R- Inserimento effettuato.                                              17
093500020115TISE96R- Modifica effettuata.                                                 18
093600020115TISE96R- Cancellazione effettuata.                                            19
093700020115TISE96R- Ripristino effettuato.                                               20
093800020115TISE96R- Richiesta funzione per voce gia' esistente                           21
093900020115TISE96R- Codice voce momentaneamente allocato: riprovare più tardi            22
094000020115TISE96R- 23
094100020115TISE96R- 24
094200020115TISE96R- 25
094300020115TISE96R- 26
094400020115TISE96R- 27
094500020115TISE96R- 28
094600020115TISE96R- 29
094700020115TISE96R- 30
094800020115TISE96R- 31
094900020115TISE96R- 32
095000020115TISE96R- 33
095100020115TISE96R- 34
095200020115TISE96R- 35
095300000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
095400000000F1    -                   -    NN     NN
095500000000F2    -                   -    NN     NN
095600000000F3    -F3=Fine            -    SS     SS
095700000000F4    -                   -    NN     NN
095800000000F5    -                   -    NN     NN
095900000000F6    -F6=Aggiornamento   -    SS     SS
096000000000F7    -                   -    NN     NN
096100000000F8    -                   -    NN     NN
096200000000F9    -                   -    NN     NN
096300000000F10   -                   -    NN     NN
096400000000F11   -                   -    NN     NN
096500000000F12   -F12=Ritorno        -    SS     SS
096600000000F13   -                   -    NN     NN
096700000000F14   -                   -    NN     NN
096800000000F15   -                   -    NN     NN
096900000000F16   -                   -    NN     NN
097000000000F17   -                   -    NN     NN
097100000000F18   -                   -    NN     NN
097200000000F19   -                   -    NN     NN
097300000000F20   -                   -    NN     NN
097400000000F21   -                   -    NN     NN
097500000000F22   -                   -    NN     NN
097600000000F23   -                   -    NN     NN
097700000000F24   -F24=Altri tasti    -    NN     NN
097800000000ENTER -                   -    SS     SS
097900000000ROLLUP-                   -    NN     NN
098000000000ROLLDN-                   -    NN     NN
098100000000HELP  -                   -    NN     NN
098200000000PRINT -                   -    NN     NN
098300000000BSPACE-                   -    NN     NN
098400000000CLEAR -                   -    NN     NN
098500000000** DOPZ  (OPZIONE RICHIAMATA)
098600000000* INSERIMENTO *                                                 1
098700000000*  VARIAZIONE  *                                                2
098800000000*CANCELLAZIONE*                                                 3
098900000000*  RIPRISTINO  *                                                4
099000000000VISUALIZZAZIONE                                                 5
099100000000**
099200000000GENNAIO
099300000000FEBBRAIO
099400000000MARZO
099500000000APRILE
099600000000MAGGIO
099700000000GIUGNO
099800000000LUGLIO
099900000000AGOSTO
100000000000SETTEMBRE
100100000000OTTOBRE
100200000000NOVEMBRE
100300000000DICEMBRE

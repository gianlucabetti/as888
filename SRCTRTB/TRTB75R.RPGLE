000100000000      *------------------------------------------------------------------------*
000200941007      *  Nome programma:  TRTB75R
000300941007      *  Descrizione   :  Gesione tabella flag spedizioni/ricavi tipi bolla
000400941007      *                   Tabella 2W
000500000000      *                   Programma modifica, inserimento, visualizzazione, ...
000600941007      *  Autore        :  RAFFAELLA (RN)
000700941007      *  Data creazione:  07 OTT 1994
000800000000      *------------------------------------------------------------------------*
000900000000     H DECEDIT('0,') DATEDIT(*DMY.)
001000000000     F*------------------------------------------------------------------------*
001100000000     F* DATA BASE
001200000000     F*------------------------------------------------------------------------*
001300941007     FTRTB75D   CF   E             WORKSTN INFDS(DEVDS1)
001400941007     F                                     SFILE(TB75DVM:MSGULT)
001500000000     FTABEL00F  UF A E           K DISK
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002100000000     D*-------------------
002200000000     D* DECODIFICA OPZIONE RICHIAMATA
002300000000     D*-------------------
002400000000     D DOPZ            S             16    DIM(5) CTDATA PERRCD(1)
002500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600000000     D FFU             S              6    DIM(31)
002700000000     D FDE             S             19    DIM(31)
002800000000     D FAT             S              1    DIM(31)
002900000000     D* TABELLA DESCRIZIONI A VIDEO
003000000000     D FUV             S             19    DIM(8)
003100000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003200000000     D KEYS            S              1    DIM(40)
003300000000     D*------------------------------------------------------------------------*
003400000000     D* INPUT
003500000000     D*------------------------------------------------------------------------*
003600000000     D*-------------------
003700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003800000000     D*-------------------
003900000000     D DSFUN         E DS
004000000000     D  FUNRIG                 1     40
004100000000     D*-------------------
004200000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004300000000     D*-------------------
004400000000     D DSMSG         E DS
004500000000     D*-------------------
004600020923     D* DS REPERIMENTO DATI UTENTE
004700020923     D*-------------------
004800020923     D TIBS34DS      E DS
004900020923     D DDATIUTE      E DS
005000020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005100000000     D*-------------------
005200000000     D* DS PER GESTIONE TASTI FUNZIONALI
005300000000     D*-------------------
005400000000     D DSKEYS        E DS
005500000000     D  KEYRIG                 1     40
005600000000     D*-------------------
005700000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005800000000     D*-------------------
005900000000     D                SDS
006000000000     D  USER                 254    263
006100000000     D*-------------------
006200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006300000000     D*-------------------
006400000000     D DEVDS1          DS
006500000000     D* CHIAVE FUNZIONALE BATTUTA
006600000000     D  KEY                  369    369
006700000000     D* POSIZIONE CURSORE (IN HEX)
006800000000     D  CURSOR               370    371
006900000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
007000000000     D  FRTSFL               378    379B 0
007100000000     D*---------------------
007200000000     D* DS TABELLA
007300000000     D*---------------------
007400941007     D DS2W          E DS
007500000000     D*---------------------
007600000000     D* DS ESTERNA DI PROCEDURA
007700000000     D*---------------------
007800941007     D DS60          E DS                  EXTNAME(TRTB74DS)
007900000000     D*-------------------
008000000000     D* ARCHITETTURA
008100000000     D*-------------------
008200000000     D KPJBA         E DS
008300000000     C*------------------------------------------------------------------------*
008400000000     C* MAIN LINE
008500000000     C*------------------------------------------------------------------------*
008600000000     C*
008700000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
008800000000IF  1C     $FINE         IFEQ      '1'
008900000000     C*
009000000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
009100941007     C                   MOVEL     '1'           D74ERR
009200000000     C*
009300000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
009400000000X   1C                   ELSE
009500000000     C*
009600000000     C* CICLO FINO A CHE:
009700000000DO  2C     $FINE         DOWEQ     '0'
009800000000     C                   MOVEL     '0'           $ERR
009900000000     C*
010000000000     C* CONTROLLA GESTIONE VIDEO
010100000000IF  3C     $GES          IFEQ      'V1'
010200000000     C                   EXSR      GESV1
010300000000E   3C                   ENDIF
010400000000     C*
010500000000E   2C                   ENDDO
010600000000E   1C                   ENDIF
010700000000     C*
010800000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
010900000000     C     $F03          IFEQ      '1'
011000941007     C                   MOVEL     '1'           D74F03
011100000000E   1C                   ENDIF
011200000000     C* FINE PGM
011300000000     C                   SETON                                        LR
011400000000     C*------------------------------------------------------------------------*
011500000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
011600000000     C*------------------------------------------------------------------------*
011700000000     C     CARV1         BEGSR
011800000000     C*
011900000000     C                   MOVEL     'V1'          $GES
012000000000     C*
012100000000     C                   EXSR      CARFUN
012200000000     C                   EXSR      OUTFUN
012300000000     C*
012400941007     C                   WRITE     TB75DVT
012500941007     C                   WRITE     TB75DVF
012600000000     C*
012700000000     C                   ENDSR
012800000000     C*------------------------------------------------------------------------*
012900000000     C* GESV1  - GESTIONE VIDEO 'TEMPO DELIVERY'
013000000000     C*------------------------------------------------------------------------*
013100000000     C     GESV1         BEGSR
013200000000     C*
013300000000IF  1C     $TESVD        IFEQ      '1'
013400000000     C                   EXSR      CARV1
013500000000E   1C                   ENDIF
013600000000     C*
013700000000IF  1C     MSGULT        IFNE      *ZEROS
013800000000     C                   EXSR      OUTMSG
013900000000E   1C                   ENDIF
014000000000     C*
014100941007     C                   EXFMT     TB75DV1
014200000000     C*
014300000000     C                   EXSR      CONFUN
014400000000     C*
014500000000IF  1C     $ERR          IFEQ      '0'
014600000000SL  2C                   SELECT
014700000000     C     KEY           WHENEQ    ENTER
014800000000     C                   EXSR      ENTV1
014900000000     C     KEY           WHENEQ    F6
015000000000     C                   EXSR      F06GES
015100000000     C                   OTHER
015200000000     C                   EXSR      FUNSTD
015300000000E   2C                   ENDSL
015400000000E   1C                   ENDIF
015500000000     C*
015600000000     C                   ENDSR
015700000000     C*------------------------------------------------------------------------*
015800000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
015900000000     C*------------------------------------------------------------------------*
016000000000     C     ENTV1         BEGSR
016100000000     C*
016200000000     C* CONTROLLA I CAMPI IMMESSI
016300000000     C                   EXSR      DECODI
016400000000     C*
016500000000     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
016600941007IF  1C     D74OP1        IFEQ      'O05'
016700000000     C                   MOVEL     '1'           $FINE
016800000000E   1C                   ENDIF
016900000000     C*
017000000000     C                   ENDSR
017100000000     C*------------------------------------------------------------------------*
017200000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
017300000000     C*------------------------------------------------------------------------*
017400000000     C     F06GES        BEGSR
017500000000     C*
017600000000     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
017700941007IF  1C     D74OP1        IFEQ      'F06'
017800000000     C                   EXSR      INSERI
017900000000E   1C                   ENDIF
018000941007IF  1C     D74OP1        IFEQ      'O02'
018100000000     C                   EXSR      MODIFI
018200000000E   1C                   ENDIF
018300941007IF  1C     D74OP1        IFEQ      'O04'
018400000000     C                   EXSR      CANCEL
018500000000E   1C                   ENDIF
018600941007IF  1C     D74OP1        IFEQ      'O07'
018700000000     C                   EXSR      RIPRIS
018800000000E   1C                   ENDIF
018900000000     C*
019000000000     C                   ENDSR
019100000000     C*------------------------------------------------------------------------*
019200000000     C* INSERI - FUNZIONE DI INSERIMENTO
019300000000     C*------------------------------------------------------------------------*
019400000000     C     INSERI        BEGSR
019500000000     C*
019600000000     C* CONTROLLA I CAMPI IMMESSI
019700000000     C                   EXSR      DECODI
019800000000     C*
019900000000IF  1C     $ERR          IFEQ      '0'
020000000000     C* T A B E L
020100020923     C                   Z-ADD     1             TBLKUT
020200941007     C                   MOVEL     '2W'          TBLCOD
020300941007     C                   MOVEL     V1CRBL        TBLKEY
020400000000     C                   MOVEL     *BLANKS       TBLFLG
020500000000     C                   MOVEL     '1'           TBLFTT
020600000000     C                   MOVEL     *BLANKS       TBLFTR
020700000000     C                   Z-ADD     *ZEROS        TBLDTR
020800941007     C* D S 2 W
020900941007     C                   MOVEL     V1CFSP        §2WFSP
021000941007     C                   MOVEL     V1CFRI        §2WFRI
021100941007     C                   MOVEL     DS2W          TBLUNI
021200000000     C*
021300000000     C                   WRITE     TABEL
021400000000     C*
021500000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
021600000000     C                   MOVEL     '1'           $FINE
021700000000     C*
021800000000     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
021900000000     C                   MOVEL     MSG(17)       DSMSMS
022000000000     C*
022100000000E   1C                   ENDIF
022200000000     C*
022300000000     C                   ENDSR
022400000000     C*------------------------------------------------------------------------*
022500000000     C* MODIFI - FUNZIONE DI MODIFICA
022600000000     C*------------------------------------------------------------------------*
022700000000     C     MODIFI        BEGSR
022800000000     C*
022900000000     C* CONTROLLA I CAMPI IMMESSI
023000000000     C                   EXSR      DECODI
023100000000     C*
023200000000IF  1C     $ERR          IFEQ      '0'
023300000000     C*
023400000000     C* PRIMA DI AGGIORNARE, RICHIAMO DI NUOVO IL RECORD IN ESAME POICHE'
023500000000     C* L'HO PERSO CON ALTRE CHAIN SU TABEL00F (NO IN INSERIMENTO)
023600020923     C                   MOVEL     1             KTBKUT
023700941007     C                   MOVEL     '2W'          KTBCOD
023800000000     C                   MOVEL     *BLANKS       KTBKEY
023900941007     C                   MOVEL     D74RBL        KTBKEY
024000000000     C     KEYTAB        CHAIN     TABEL00F                           99
024100941007     C                   MOVEL     V1CFSP        §2WFSP
024200941007     C                   MOVEL     V1CFRI        §2WFRI
024300000000     C*
024400941007     C                   MOVEL     DS2W          TBLUNI
024500000000     C*
024600941012     C                   MOVEL     '1'           TBLFTT
024700941012     C                   MOVEL     *BLANKS       TBLFTR
024800000000     C                   Z-ADD     *ZEROS        TBLDTR
024900000000     C*
025000000000     C                   UPDATE    TABEL
025100000000     C*
025200000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
025300000000     C                   MOVEL     '1'           $FINE
025400000000     C*
025500000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
025600941007     C                   MOVEL     MSG(18)       D74MSG
025700000000     C*
025800000000E   1C                   ENDIF
025900000000     C*
026000000000     C                   ENDSR
026100000000     C*------------------------------------------------------------------------*
026200000000     C* CANCEL - FUNZIONE DI CANCELLAZIONE
026300000000     C*------------------------------------------------------------------------*
026400000000     C     CANCEL        BEGSR
026500000000     C*
026600000000     C* PRIMA DI AGGIORNARE, RICHIAMO DI NUOVO IL RECORD IN ESAME POICHE'
026700000000     C* L'HO PERSO CON ALTRE CHAIN SU TABEL00F (NO IN INSERIMENTO)
026800020923     C                   MOVEL     1             KTBKUT
026900941007     C                   MOVEL     '2W'          KTBCOD
027000000000     C                   MOVEL     *BLANKS       KTBKEY
027100941007     C                   MOVEL     D74RBL        KTBKEY
027200000000     C     KEYTAB        CHAIN     TABEL00F                           99
027300000000     C                   MOVEL     '*'           TBLFLG
027400941012     C                   MOVEL     '1'           TBLFTT
027500000000     C                   MOVEL     *BLANKS       TBLFTR
027600000000     C                   Z-ADD     *ZEROS        TBLDTR
027700000000     C*
027800000000     C                   UPDATE    TABEL
027900000000     C*
028000000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
028100000000     C                   MOVEL     '1'           $FINE
028200000000     C*
028300000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
028400941007     C                   MOVEL     MSG(19)       D74MSG
028500000000     C*
028600000000     C                   ENDSR
028700000000     C*------------------------------------------------------------------------*
028800000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
028900000000     C*------------------------------------------------------------------------*
029000000000     C     RIPRIS        BEGSR
029100000000     C*
029200000000     C* PRIMA DI AGGIORNARE, RICHIAMO DI NUOVO IL RECORD IN ESAME POICHE'
029300000000     C* L'HO PERSO CON ALTRE CHAIN SU TABEL00F (NO IN INSERIMENTO)
029400020923     C                   MOVEL     1             KTBKUT
029500941007     C                   MOVEL     '2W'          KTBCOD
029600000000     C                   MOVEL     *BLANKS       KTBKEY
029700941007     C                   MOVEL     D74RBL        KTBKEY
029800000000     C     KEYTAB        CHAIN     TABEL00F                           99
029900000000     C                   MOVEL     *BLANKS       TBLFLG
030000941012     C                   MOVEL     '1'           TBLFTT
030100000000     C                   MOVEL     *BLANKS       TBLFTR
030200000000     C                   Z-ADD     *ZEROS        TBLDTR
030300000000     C*
030400000000     C                   UPDATE    TABEL
030500000000     C*
030600000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
030700000000     C                   MOVEL     '1'           $FINE
030800000000     C*
030900000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
031000941007     C                   MOVEL     MSG(20)       D74MSG
031100000000     C*
031200000000     C                   ENDSR
031300000000     C*------------------------------------------------------------------------*
031400000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
031500000000     C*------------------------------------------------------------------------*
031600000000     C     DECODI        BEGSR
031700000000     C*
031800000000     C* IMPOSTO INDICATORI DI ERRORE CAMPI
031900000000     C                   SETOFF                                       606162
032000000000     C*--------------------
032100941007     C* RAGGRUPPAMENTO BOLLA (SOLO SE INSERIMENTO)
032200000000     C*--------------------
032300941007IF  1C     V1CRBL        IFEQ      *BLANKS
032400000000     C                   MOVE      '1'           $ERR
032500000000     C                   MOVEL     MSG(8)        $MSG
032600000000     C                   EXSR      CARMSG
032700000000     C                   SETON                                        60
032800000000E   1C                   ENDIF
032900000000     C*
033000000000     C* IN INSERIMENTO IL CODICE NON DEVE ESSERE GIA' ESISTENTE
033100000000IF  1C     $ERR          IFEQ      '0'
033200941007     C     D74OP1        ANDEQ     'F06'
033300020923     C                   MOVEL     1             KTBKUT
033400941007     C                   MOVEL     '2W'          KTBCOD
033500000000     C                   MOVE      *BLANKS       KTBKEY
033600941007     C                   MOVEL     V1CRBL        KTBKEY
033700000000     C     KEYTAB        SETLL     TABEL00F                               60
033800000000IF  2C     *IN60         IFEQ      *ON
033900000000     C                   MOVE      '1'           $ERR
034000000000     C                   MOVEL     MSG(7)        $MSG
034100000000     C                   EXSR      CARMSG
034200000000E   2C                   ENDIF
034300000000E   1C                   ENDIF
034400000000     C*--------------------
034500941007     C* FLAG SPEDIZIONI
034600000000     C*--------------------
034700000000IF  1C     $ERR          IFEQ      '0'
034800941007IF  2C     V1CFSP        IFEQ      'S'
034900941007     C     V1CFSP        OREQ      'N'
035000941007C   2C                   ELSE
035100941007     C                   MOVE      '1'           $ERR
035200000000     C                   MOVEL     MSG(10)       $MSG
035300000000     C                   EXSR      CARMSG
035400941007     C                   SETON                                        61
035500000000E   2C                   ENDIF
035600000000E   1C                   ENDIF
035700941007     C*--------------------
035800941007     C* FLAG RICAVI
035900941007     C*--------------------
036000941007IF  1C     $ERR          IFEQ      '0'
036100941007IF  2C     V1CFRI        IFEQ      'S'
036200941007     C     V1CFRI        OREQ      'N'
036300941007C   2C                   ELSE
036400941007     C                   MOVE      '1'           $ERR
036500941007     C                   MOVEL     MSG(10)       $MSG
036600941007     C                   EXSR      CARMSG
036700941007     C                   SETON                                        62
036800941007E   2C                   ENDIF
036900941007E   1C                   ENDIF
037000000000     C*
037100000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
037200000000     C* CURSORE SUL CAMPO DEL CONTROL
037300000000IF  1C     $ERR          IFEQ      '1'
037400000000     C                   MOVEL     '0'           *IN43
037500000000X   1C                   ELSE
037600000000     C                   MOVEL     '1'           *IN43
037700000000E   1C                   ENDIF
037800000000     C*
037900000000     C                   ENDSR
038000000000     C*------------------------------------------------------------------------*
038100000000     C*  RECVID - RIEMPE I CAMPI DAI RECORD TABELLA AL VIDEO
038200000000     C*------------------------------------------------------------------------*
038300000000     C     RECVID        BEGSR
038400000000     C*
038500941007     C                   MOVEL     TBLUNI        DS2W
038600000000     C*
038700941007     C                   MOVE      D74RBL        V1CRBL
038800941007     C                   MOVE      §2WFSP        V1CFSP
038900941007     C                   MOVE      §2WFRI        V1CFRI
039000000000     C*
039100000000     C                   ENDSR
039200000000     C*------------------------------------------------------------------------*
039300000000     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
039400000000     C*------------------------------------------------------------------------*
039500000000     C     AZZVID        BEGSR
039600000000     C*
039700941007     C                   MOVE      *BLANKS       V1CRBL
039800941007     C                   MOVEL     *BLANKS       V1CFSP
039900941007     C                   MOVE      *BLANKS       V1CFRI
040000000000     C*
040100000000     C                   ENDSR
040200020923     C*-------------------------------------------------------------------------
040300020923     C* REPERISCE I DATI UTENTE
040400020923     C*-------------------------------------------------------------------------
040500020923     C     REPDATIUTE    BEGSR
040600020923     C*
040700020923     C* INIZIALIZZA VARIABILI DI WRK
040800020923     C                   CLEAR                   TIBS34DS
040900020923     C                   CLEAR                   AZUTEDS
041000020923     C                   CLEAR                   DDATIUTE
041100020923     C*
041200020923     C     *DTAARA       DEFINE    §azute        azuteds
041300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
041400020923     C                   IN(E)     *DTAARA
041500020923if  1C                   IF        %Error
041600020923     c                   EVAL      I34Tla = 'L'
041700020923     C                   CALL      'TIBS34R'
041800020923     C                   PARM                    Tibs34Ds
041900020923     C                   IN        *DTAARA
042000020923e   1C                   ENDIF
042100020923     C*
042200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
042300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
042400020923     C*
042500020923     C                   ENDSR
042600000000     C*------------------------------------------------------------------------*
042700000000     C*  *INZSR - ROUTINE INIZIALE
042800000000     C*------------------------------------------------------------------------*
042900000000     C     *INZSR        BEGSR
043000000000     C*
043100000000     C* RICEVIMENTO PARAMETRI
043200000000     C     *ENTRY        PLIST
043300000000     C                   PARM                    DS60
043400000000     C*
043500000000     C* DEFINIZIONE VARIABILI
043600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
043700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
043800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
043900000000     C                   MOVEL     'G'           $TGES             1
044000000000     C*
044100000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
044200000000     C     KEYTAB        KLIST
044300000000     C                   KFLD                    KTBKUT
044400000000     C                   KFLD                    KTBCOD
044500000000     C                   KFLD                    KTBKEY
044600000000     C*
044700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
044800000000     C                   EXSR      INZSTD
044900000000     C*
045000000000     C* RAGIONE SOCIALE UTENTE
045100000000     C                   MOVEL     DSFIRS        VTDRSU
045200000000     C*
045300000000     C* DECODIFICA OPZIONE RICHIAMATA
045400941007IF  1C     D74OP1        IFEQ      'F06'
045500000000     C                   MOVEL     DOPZ(1)       VTDFUN
045600000000E   1C                   ENDIF
045700941007IF  2C     D74OP1        IFEQ      'O02'
045800000000     C                   MOVEL     DOPZ(2)       VTDFUN
045900000000E   1C                   ENDIF
046000941007IF  1C     D74OP1        IFEQ      'O04'
046100000000     C                   MOVEL     DOPZ(3)       VTDFUN
046200000000E   1C                   ENDIF
046300941007IF  1C     D74OP1        IFEQ      'O05'
046400000000     C                   MOVEL     DOPZ(5)       VTDFUN
046500000000E   1C                   ENDIF
046600941007IF  1C     D74OP1        IFEQ      'O07'
046700000000     C                   MOVEL     DOPZ(4)       VTDFUN
046800000000E   1C                   ENDIF
046900000000     C*
047000000000     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
047100000000     C* 0=NON PROTETTO, 1=PROTETTO
047200000000     C*   90 - CAMPI CHIAVE (CODICE ELEMENTO)
047300000000     C*   91 - ALTRI CAMPI
047400941007IF  1C     D74OP1        IFEQ      'F06'
047500000000     C                   SETOFF                                       9091
047600000000E   1C                   ENDIF
047700941007IF  1C     D74OP1        IFEQ      'O02'
047800000000     C                   SETON                                        90
047900000000E   1C                   ENDIF
048000941007IF  1C     D74OP1        IFEQ      'O04'
048100941007     C     D74OP1        OREQ      'O05'
048200941007     C     D74OP1        OREQ      'O07'
048300000000     C                   SETON                                        9091
048400000000E   1C                   ENDIF
048500000000     C*
048600000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
048700941007IF  1C     D74OP1        IFEQ      'O05'
048800000000     C                   MOVEL     FUN(6)        FUNRIG
048900000000     C                   MOVEL     'N'           FUNASG
049000000000     C                   MOVEL     'N'           FUNASI
049100000000     C                   MOVEL     'N'           FUNAFG
049200000000     C                   MOVEL     'N'           FUNAFG
049300000000     C                   MOVEL     FUNRIG        FUN(6)
049400000000E   1C                   ENDIF
049500000000     C*
049600000000     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
049700941007IF  1C     D74OP1        IFEQ      'O02'
049800941007     C     D74OP1        OREQ      'O04'
049900941007     C     D74OP1        OREQ      'O05'
050000941007     C     D74OP1        OREQ      'O07'
050100020923     C                   MOVEL     1             KTBKUT
050200941007     C                   MOVEL     '2W'          KTBCOD
050300000000     C                   MOVEL     *BLANKS       KTBKEY
050400941007     C                   MOVEL     D74RBL        KTBKEY
050500000000     C*
050600000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
050700000000     C     KEYTAB        CHAIN     TABEL00F                           9998
050800000000IF  2C     *IN99         IFEQ      '1'
050900000000     C                   MOVEL     '1'           $ERR
051000000000     C                   MOVEL     '1'           $FINE
051100941007     C                   MOVEL     MSG(14)       D74MSG
051200000000X   2C                   ELSE
051300000000IF  3C     *IN98         IFEQ      '1'
051400000000     C                   MOVEL     '1'           $ERR
051500000000     C                   MOVEL     '1'           $FINE
051600941007     C                   MOVEL     MSG(15)       D74MSG
051700000000X   3C                   ELSE
051800000000     C                   EXSR      RECVID
051900000000     C                   EXSR      CARV1
052000000000E   3C                   ENDIF
052100000000E   2C                   ENDIF
052200000000E   1C                   ENDIF
052300000000     C*
052400000000     C* SE INSERIMENTO
052500941007IF  1C     D74OP1        IFEQ      'F06'
052600000000     C                   EXSR      AZZVID
052700000000     C                   EXSR      CARV1
052800000000     C                   ELSE
052900000000     C                   EXSR      DECODI
053000000000E   1C                   ENDIF
053100000000     C*
053200000000     C                   ENDSR
053300000000     C*------------------------------------------------------------------------*
053400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053600000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
053700000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
053800000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
053900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
054000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
054100000000     C*------------------------------------------------------------------------*
054200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
054300000000     C*------------------------------------------------------------------------*
054400000000     C     INZSTD        BEGSR
054500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
054600000000     C                   CALL      'XKEYS'
054700000000     C                   PARM                    DSKEYS
054800000000     C                   MOVEA     KEYRIG        KEYS
054900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
055000000000     C     XMSGPA        PLIST
055100000000     C                   PARM                    DSMSMS
055200000000     C                   PARM                    DSMSPG
055300000000     C                   PARM                    DSMSKE
055400000000     C                   PARM                    DSMSER
055500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
055600941007     C                   MOVEL     'TRTB75R'     DSMSPG
055700941007     C                   MOVEL     'TRTB75R'     VMCQUE
055800000000     C                   MOVEL     *BLANKS       VMCKEY
055900000000     C                   MOVEL     *BLANKS       DSMSMS
056000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
056100000000     C                   MOVEL     '0'           $ERR              1
056200000000     C                   MOVEL     '0'           $F03              1
056300000000     C                   MOVEL     *BLANKS       $GES              2
056400000000     C                   MOVEL     *BLANKS       $MSG             80
056500000000     C                   MOVEL     '0'           $FINE             1
056600000000     C                   MOVEL     '0'           $TESVD            1
056700000000     C* LANCIO ROUTINE PER VERIFICARE SE
056800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
056900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
057000020923     C                   EXSR      REPDATIUTE
057100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
057200000000     C                   BITOFF    '01234567'    POSMI1            1
057300000000     C                   BITOFF    '01234567'    POSMI2            1
057400000000     C                   BITOFF    '01234567'    POSMA1            1
057500000000     C                   BITOFF    '01234567'    POSMA2            1
057600000000     C                   BITON     '34'          POSMI1            1
057700000000     C                   BITON     '7'           POSMI2            1
057800000000     C                   BITON     '34'          POSMA1            1
057900000000     C                   BITON     '0'           POSMA2            1
058000000000     C                   MOVE      POSMI2        POSMIN            2
058100000000     C                   MOVEL     POSMI1        POSMIN
058200000000     C                   MOVE      POSMA2        POSMAX            2
058300000000     C                   MOVEL     POSMA1        POSMAX
058400000000     C                   ENDSR
058500000000     C*------------------------------------------------------------------------*
058600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
058700000000     C*------------------------------------------------------------------------*
058800000000     C     FUNSTD        BEGSR
058900000000     C                   SELECT
059000000000     C     KEY           WHENEQ    F3
059100000000     C                   EXSR      F03STD
059200000000     C     KEY           WHENEQ    F12
059300000000     C                   EXSR      F12STD
059400000000     C     KEY           WHENEQ    F24
059500000000     C                   EXSR      F24STD
059600000000     C                   OTHER
059700000000     C                   MOVEL     MSG(2)        $MSG
059800000000     C                   EXSR      CARMSG
059900000000     C                   ENDSL
060000000000     C                   ENDSR
060100000000     C*------------------------------------------------------------------------*
060200000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
060300000000     C*------------------------------------------------------------------------*
060400000000     C     F03STD        BEGSR
060500000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
060600000000     C                   MOVEL     '1'           $F03
060700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
060800000000     C                   MOVEL     '1'           $FINE
060900000000     C                   ENDSR
061000000000     C*------------------------------------------------------------------------*
061100000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
061200000000     C*------------------------------------------------------------------------*
061300000000     C     F12STD        BEGSR
061400000000     C* IMPOSTO FLAG DI FIN PGM
061500000000     C                   MOVEL     '1'           $FINE
061600000000     C                   ENDSR
061700000000     C*------------------------------------------------------------------------*
061800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
061900000000     C*------------------------------------------------------------------------*
062000000000     C     F24STD        BEGSR
062100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
062200000000     C                   EXSR      OUTFUN
062300000000     C                   ENDSR
062400000000     C*------------------------------------------------------------------------*
062500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
062600000000     C*          CODA MESSAGGI DEL PGM
062700000000     C*------------------------------------------------------------------------*
062800000000     C     CARMSG        BEGSR
062900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
063000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
063100000000     C     MSGULT        IFEQ      0
063200000000     C                   MOVEL     '1'           *IN40
063300941007     C                   WRITE     TB75DVK
063400000000     C                   MOVEL     '0'           *IN40
063500000000     C                   ENDIF
063600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
063700000000     C                   MOVEL     $MSG          DSMSMS
063800000000     C                   CALL      'XMSG'        XMSGPA
063900000000     C     DSMSER        IFEQ      0
064000000000     C                   MOVEL     DSMSKE        VMCKEY
064100000000     C                   ENDIF
064200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
064300000000     C                   ADD       1             MSGULT            4 0
064400941007     C                   WRITE     TB75DVM
064500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
064600000000     C                   MOVEL     *BLANKS       $MSG
064700000000     C                   ENDSR
064800000000     C*------------------------------------------------------------------------*
064900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
065000000000     C*------------------------------------------------------------------------*
065100000000     C     OUTMSG        BEGSR
065200000000     C* VISUALIZZAZIONE MESSAGGIO
065300000000     C                   MOVEL     '1'           *IN44
065400941007     C                   WRITE     TB75DVK
065500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
065600000000     C                   Z-ADD     *ZEROS        MSGULT
065700000000     C                   ENDSR
065800000000     C*------------------------------------------------------------------------*
065900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
066000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
066100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
066200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
066300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
066400000000     C*------------------------------------------------------------------------*
066500000000     C     CARFUN        BEGSR
066600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
066700000000     C     1             DO        31            I                 5 0
066800000000     C                   MOVEL     FUN(I)        FUNRIG
066900000000     C                   MOVEL     FUNFUN        FFU(I)
067000000000     C                   MOVEL     FUNDES        FDE(I)
067100020923     C     DUTLPO        IFEQ      'S'
067200000000     C     $TGES         IFEQ      'G'
067300000000     C                   MOVEL     FUNASG        FAT(I)
067400000000     C                   ELSE
067500000000     C                   MOVEL     FUNASI        FAT(I)
067600000000     C                   ENDIF
067700000000     C                   ELSE
067800000000     C     $TGES         IFEQ      'G'
067900000000     C                   MOVEL     FUNAFG        FAT(I)
068000000000     C                   ELSE
068100000000     C                   MOVEL     FUNAFI        FAT(I)
068200000000     C                   ENDIF
068300000000     C                   ENDIF
068400000000     C     FAT(I)        IFEQ      'E'
068500000000     C                   MOVEL     USER          WEDP              3
068600000000     C     WEDP          IFEQ      'EDP'
068700000000     C                   MOVEL     'S'           FAT(I)
068800000000     C                   ELSE
068900000000     C                   MOVEL     'N'           FAT(I)
069000000000     C                   ENDIF
069100000000     C                   ENDIF
069200000000     C                   ENDDO
069300000000     C                   ENDSR
069400000000     C*------------------------------------------------------------------------*
069500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
069600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
069700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
069800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
069900000000     C*------------------------------------------------------------------------*
070000000000     C     OUTFUN        BEGSR
070100000000     C     1             DO        8             I
070200000000     C                   MOVEL     *BLANKS       FUV(I)
070300000000     C                   ENDDO
070400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
070500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
070600000000     C     1             DO        31            I
070700000000     C     FAT(I)        IFEQ      'S'
070800000000     C     FDE(I)        ANDNE     *BLANKS
070900000000     C                   ADD       1             FUNNAT
071000000000     C                   ENDIF
071100000000     C                   ENDDO
071200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
071300000000     C     FUNNAT        IFGT      8
071400000000     C                   MOVEL     'S'           FAT(24)
071500000000     C                   ENDIF
071600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
071700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
071800000000     C                   Z-ADD     8             FUNVIS            5 0
071900000000     C     FAT(23)       IFEQ      'S'
072000000000     C                   SUB       1             FUNVIS
072100000000     C                   ENDIF
072200000000     C     FAT(24)       IFEQ      'S'
072300000000     C                   SUB       1             FUNVIS
072400000000     C                   ENDIF
072500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
072600000000     C                   Z-ADD     *ZEROS        W                 5 0
072700000000     C                   ADD       1             FUNULT
072800000000     C     FUNULT        IFGT      31
072900000000     C                   Z-ADD     1             FUNULT
073000000000     C                   ENDIF
073100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
073200000000     C     FUNULT        DO        31            I
073300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
073400000000     C     W             IFLT      FUNVIS
073500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
073600000000     C     FAT(I)        IFEQ      'S'
073700000000     C     FDE(I)        ANDNE     *BLANKS
073800000000     C     I             IFEQ      23
073900000000     C     I             OREQ      24
074000000000     C                   ELSE
074100000000     C                   ADD       1             W
074200000000     C                   MOVEL     FDE(I)        FUV(W)
074300000000     C                   ENDIF
074400000000     C                   ENDIF
074500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
074600000000     C                   Z-ADD     I             FUNULT
074700000000     C                   ENDIF
074800000000     C                   ENDDO
074900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
075000000000     C                   MOVEL     FUV(1)        VFD001
075100000000     C                   MOVEL     FUV(2)        VFD002
075200000000     C                   MOVEL     FUV(3)        VFD003
075300000000     C                   MOVEL     FUV(4)        VFD004
075400000000     C                   MOVEL     FUV(5)        VFD005
075500000000     C                   MOVEL     FUV(6)        VFD006
075600000000     C     FAT(23)       IFEQ      'N'
075700000000     C     FAT(24)       ANDEQ     'N'
075800000000     C                   MOVEL     FUV(7)        VFD007
075900000000     C                   MOVEL     FUV(8)        VFD008
076000000000     C                   ELSE
076100000000     C     FAT(23)       IFEQ      'S'
076200000000     C     FAT(24)       ANDEQ     'S'
076300000000     C                   MOVEL     FDE(23)       VFD007
076400000000     C                   MOVEL     FDE(24)       VFD008
076500000000     C                   ELSE
076600000000     C                   MOVEL     FUV(7)        VFD007
076700000000     C     FAT(23)       IFEQ      'S'
076800000000     C                   MOVEL     FDE(23)       VFD008
076900000000     C                   ELSE
077000000000     C                   MOVEL     FDE(24)       VFD008
077100000000     C                   ENDIF
077200000000     C                   ENDIF
077300000000     C                   ENDIF
077400000000     C                   MOVEL     '1'           $TESVD
077500000000     C                   ENDSR
077600000000     C*------------------------------------------------------------------------*
077700000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
077800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
077900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
078000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
078100000000     C*------------------------------------------------------------------------*
078200000000     C     CONFUN        BEGSR
078300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
078400000000     C     1             DO        31            I
078500000000     C     KEY           IFEQ      KEYS(I)
078600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
078700000000     C     FAT(I)        IFNE      'S'
078800000000     C                   MOVEL     MSG(1)        $MSG
078900000000     C                   EXSR      CARMSG
079000000000     C                   MOVEL     '1'           $ERR
079100000000     C                   ENDIF
079200000000     C                   ENDIF
079300000000     C                   ENDDO
079400000000     C                   ENDSR
079500000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
079600941007TRTB75R- Tasto funzionale non valido.                                          1
079700941007TRTB75R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
079800941007TRTB75R- Opzione non valida.                                                   3
079900941007TRTB75R- Opzione abilitata ma non gestita. Avvertire CED.                      4
080000941007TRTB75R- Inizio elenco.                                                        5
080100941007TRTB75R- Fine elenco.                                                          6
080200941007TRTB75R- Immettere codice elemento non esistente                               7
080300941007TRTB75R- Immettere codice elemento                                             8
080400941007TRTB75R- 9
080500941007TRTB75R- Immettere "S" o "N"                                                  10
080600941007TRTB75R- 11
080700941007TRTB75R- 12
080800941007TRTB75R- 13
080900941007TRTB75R- Richiesta funzione per codice già esistente                          14
081000941007TRTB75R- Codice momentaneamente allocato: riprovare più tardi                 15
081100941007TRTB75R- 16
081200941007TRTB75R- Inserimento effettuato.                                              17
081300941007TRTB75R- Modifica effettuata.                                                 18
081400941007TRTB75R- Cancellazione effettuata.                                            19
081500941007TRTB75R- Ripristino effettuato.                                               20
081600000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
081700000000F1    -                   -    NN     NN
081800000000F2    -                   -    NN     NN
081900000000F3    -F3=Fine            -    SS     SS
082000000000F4    -                   -    NN     NN
082100000000F5    -                   -    NN     NN
082200000000F6    -F6=Aggiornamento   -    SS     SS
082300000000F7    -                   -    NN     NN
082400000000F8    -                   -    NN     NN
082500000000F9    -                   -    NN     NN
082600000000F10   -                   -    NN     NN
082700000000F11   -                   -    NN     NN
082800000000F12   -F12=Ritorno        -    SS     SS
082900000000F13   -                   -    NN     NN
083000000000F14   -                   -    NN     NN
083100000000F15   -                   -    NN     NN
083200000000F16   -                   -    NN     NN
083300000000F17   -                   -    NN     NN
083400000000F18   -                   -    NN     NN
083500000000F19   -                   -    NN     NN
083600000000F20   -                   -    NN     NN
083700000000F21   -                   -    NN     NN
083800000000F22   -                   -    NN     NN
083900000000F23   -                   -    NN     NN
084000000000F24   -F24=Altri tasti    -    NN     NN
084100000000ENTER -                   -    SS     SS
084200000000ROLLUP-                   -    NN     NN
084300000000ROLLDN-                   -    NN     NN
084400000000HELP  -                   -    NN     NN
084500000000PRINT -                   -    NN     NN
084600000000BSPACE-                   -    NN     NN
084700000000CLEAR -                   -    NN     NN
084800000000** DOPZ  (OPZIONE RICHIAMATA)
084900000000* INSERIMENTO *                                                 1
085000000000*  VARIAZIONE  *                                                2
085100000000*CANCELLAZIONE*                                                 3
085200000000*  RIPRISTINO  *                                                4
085300000000VISUALIZZAZIONE                                                 5

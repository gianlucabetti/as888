000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TRTB63R
000300000000      *  Descrizione   :  Gesione tabella valori di riferimento di c/e
000400000000      *                   Tabella 2R
000500000000      *                   Programma modifica, inserimento, visualizzazione, ...
000600000000      *  Autore        :  STEFANO (SC)
000700000000      *  Data creazione:  13 GIU 1994
000800000000      *------------------------------------------------------------------------*
000900000000     H DECEDIT('0,') DATEDIT(*DMY.)
001000000000     F*------------------------------------------------------------------------*
001100000000     F* DATA BASE
001200000000     F*------------------------------------------------------------------------*
001300000000     FTRTB63D   CF   E             WORKSTN INFDS(DEVDS1)
001400000000     F                                     SFILE(TB63DVM:MSGULT)
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
007400000000     D DS2R          E DS
007500000000     D*---------------------
007600000000     D* DS ESTERNA DI PROCEDURA
007700000000     D*---------------------
007800000000     D DS62          E DS                  EXTNAME(TRTB62DS)
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
009100000000     C                   MOVEL     '1'           D62ERR
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
011000000000     C                   MOVEL     '1'           D62F03
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
012400000000     C                   WRITE     TB63DVT
012500000000     C                   WRITE     TB63DVF
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
014100000000     C                   EXFMT     TB63DV1
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
016600000000IF  1C     D62OP1        IFEQ      'O05'
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
017700000000IF  1C     D62OP1        IFEQ      'F06'
017800000000     C                   EXSR      INSERI
017900000000E   1C                   ENDIF
018000000000IF  1C     D62OP1        IFEQ      'O02'
018100000000     C                   EXSR      MODIFI
018200000000E   1C                   ENDIF
018300000000IF  1C     D62OP1        IFEQ      'O04'
018400000000     C                   EXSR      CANCEL
018500000000E   1C                   ENDIF
018600000000IF  1C     D62OP1        IFEQ      'O07'
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
020200000000     C                   MOVEL     '2R'          TBLCOD
020300000000     C                   MOVEL     V1CCOD        TBLKEY
020400000000     C                   MOVEL     *BLANKS       TBLFLG
020500000000     C                   MOVEL     '1'           TBLFTT
020600000000     C                   MOVEL     *BLANKS       TBLFTR
020700000000     C                   Z-ADD     *ZEROS        TBLDTR
020800000000     C* D S 2 R
020900000000     C                   MOVE      V1CDES        §2RDES
021000000000     C                   MOVE      V1CRPV        §2RRPV
021100000000     C                   MOVE      V1CPIU        §2RPIU
021200000000     C                   MOVE      V1CTVA        §2RTVA
021300000000     C                   MOVEL     DS2R          TBLUNI
021400000000     C*
021500000000     C                   WRITE     TABEL
021600000000     C*
021700000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
021800000000     C                   MOVEL     '1'           $FINE
021900000000     C*
022000000000     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
022100000000     C                   MOVEL     MSG(17)       DSMSMS
022200000000     C*
022300000000E   1C                   ENDIF
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* MODIFI - FUNZIONE DI MODIFICA
022800000000     C*------------------------------------------------------------------------*
022900000000     C     MODIFI        BEGSR
023000000000     C*
023100000000     C* CONTROLLA I CAMPI IMMESSI
023200000000     C                   EXSR      DECODI
023300000000     C*
023400000000IF  1C     $ERR          IFEQ      '0'
023500000000     C*
023600000000     C* PRIMA DI AGGIORNARE, RICHIAMO DI NUOVO IL RECORD IN ESAME POICHE'
023700000000     C* L'HO PERSO CON ALTRE CHAIN SU TABEL00F (NO IN INSERIMENTO)
023800020923     C                   MOVEL     1             KTBKUT
023900000000     C                   MOVEL     '2R'          KTBCOD
024000000000     C                   MOVEL     *BLANKS       KTBKEY
024100000000     C                   MOVEL     D62COD        KTBKEY
024200000000     C     KEYTAB        CHAIN     TABEL00F                           99
024300000000     C                   MOVE      V1CDES        §2RDES
024400000000     C                   MOVE      V1CRPV        §2RRPV
024500000000     C                   MOVE      V1CPIU        §2RPIU
024600000000     C                   MOVE      V1CTVA        §2RTVA
024700000000     C*
024800000000     C                   MOVEL     DS2R          TBLUNI
024900000000     C*
025000000000     C                   MOVEL     *BLANKS       TBLFTR
025100000000     C                   Z-ADD     *ZEROS        TBLDTR
025200000000     C*
025300000000     C                   UPDATE    TABEL
025400000000     C*
025500000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
025600000000     C                   MOVEL     '1'           $FINE
025700000000     C*
025800000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
025900000000     C                   MOVEL     MSG(18)       D62MSG
026000000000     C*
026100000000E   1C                   ENDIF
026200000000     C*
026300000000     C                   ENDSR
026400000000     C*------------------------------------------------------------------------*
026500000000     C* CANCEL - FUNZIONE DI CANCELLAZIONE
026600000000     C*------------------------------------------------------------------------*
026700000000     C     CANCEL        BEGSR
026800000000     C*
026900000000     C* PRIMA DI AGGIORNARE, RICHIAMO DI NUOVO IL RECORD IN ESAME POICHE'
027000000000     C* L'HO PERSO CON ALTRE CHAIN SU TABEL00F (NO IN INSERIMENTO)
027100020923     C                   MOVEL     1             KTBKUT
027200000000     C                   MOVEL     '2R'          KTBCOD
027300000000     C                   MOVEL     *BLANKS       KTBKEY
027400000000     C                   MOVEL     D62COD        KTBKEY
027500000000     C     KEYTAB        CHAIN     TABEL00F                           99
027600000000     C                   MOVEL     '*'           TBLFLG
027700000000     C                   MOVEL     *BLANKS       TBLFTR
027800000000     C                   Z-ADD     *ZEROS        TBLDTR
027900000000     C*
028000000000     C                   UPDATE    TABEL
028100000000     C*
028200000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
028300000000     C                   MOVEL     '1'           $FINE
028400000000     C*
028500000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
028600000000     C                   MOVEL     MSG(19)       D62MSG
028700000000     C*
028800000000     C                   ENDSR
028900000000     C*------------------------------------------------------------------------*
029000000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
029100000000     C*------------------------------------------------------------------------*
029200000000     C     RIPRIS        BEGSR
029300000000     C*
029400000000     C* PRIMA DI AGGIORNARE, RICHIAMO DI NUOVO IL RECORD IN ESAME POICHE'
029500000000     C* L'HO PERSO CON ALTRE CHAIN SU TABEL00F (NO IN INSERIMENTO)
029600020923     C                   MOVEL     1             KTBKUT
029700000000     C                   MOVEL     '2R'          KTBCOD
029800000000     C                   MOVEL     *BLANKS       KTBKEY
029900000000     C                   MOVEL     D62COD        KTBKEY
030000000000     C     KEYTAB        CHAIN     TABEL00F                           99
030100000000     C                   MOVEL     *BLANKS       TBLFLG
030200000000     C                   MOVEL     *BLANKS       TBLFTR
030300000000     C                   Z-ADD     *ZEROS        TBLDTR
030400000000     C*
030500000000     C                   UPDATE    TABEL
030600000000     C*
030700000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
030800000000     C                   MOVEL     '1'           $FINE
030900000000     C*
031000000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
031100000000     C                   MOVEL     MSG(20)       D62MSG
031200000000     C*
031300000000     C                   ENDSR
031400000000     C*------------------------------------------------------------------------*
031500000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
031600000000     C*------------------------------------------------------------------------*
031700000000     C     DECODI        BEGSR
031800000000     C*
031900000000     C* IMPOSTO INDICATORI DI ERRORE CAMPI
032000000000     C                   SETOFF                                       606162
032100000000     C                   SETOFF                                       6364
032200000000     C*--------------------
032300000000     C* CODICE ELEMENTO
032400000000     C*--------------------
032500000000IF  1C     V1CCOD        IFEQ      *BLANKS
032600000000     C                   MOVE      '1'           $ERR
032700000000     C                   MOVEL     MSG(8)        $MSG
032800000000     C                   EXSR      CARMSG
032900000000     C                   SETON                                        60
033000000000E   1C                   ENDIF
033100000000     C*
033200000000     C* IN INSERIMENTO IL CODICE NON DEVE ESSERE GIA' ESISTENTE
033300000000IF  1C     $ERR          IFEQ      '0'
033400000000     C     D62OP1        ANDEQ     'F06'
033500020923     C                   MOVEL     1             KTBKUT
033600000000     C                   MOVEL     '2R'          KTBCOD
033700000000     C                   MOVE      *BLANKS       KTBKEY
033800000000     C                   MOVEL     V1CCOD        KTBKEY
033900000000     C     KEYTAB        SETLL     TABEL00F                               60
034000000000IF  2C     *IN60         IFEQ      *ON
034100000000     C                   MOVE      '1'           $ERR
034200000000     C                   MOVEL     MSG(7)        $MSG
034300000000     C                   EXSR      CARMSG
034400000000E   2C                   ENDIF
034500000000E   1C                   ENDIF
034600000000     C*--------------------
034700000000     C* DESCRIZIONE
034800000000     C*--------------------
034900000000     C* OBBLIGAORIA
035000000000IF  1C     $ERR          IFEQ      '0'
035100000000IF  2C     V1CDES        IFEQ      *BLANKS
035200000000     C                   MOVEL     '1'           $ERR
035300000000     C                   MOVEL     MSG(9)        $MSG
035400000000     C                   EXSR      CARMSG
035500000000     C                   SETON                                        61
035600000000E   2C                   ENDIF
035700000000E   1C                   ENDIF
035800000000     C*--------------------
035900000000     C* UTILIZZO PESO/VOLUME
036000000000     C*--------------------
036100000000IF  1C     $ERR          IFEQ      '0'
036200000000IF  2C     V1CRPV        IFNE      'S'
036300000000     C     V1CRPV        ANDNE     'N'
036400000000     C     V1CRPV        ANDNE     *BLANKS
036500000000     C                   MOVEL     MSG(10)       $MSG
036600000000     C                   EXSR      CARMSG
036700000000     C                   SETON                                        62
036800000000E   2C                   ENDIF
036900000000     C*
037000000000IF  2C     V1CRPV        IFEQ      *BLANKS
037100000000     C                   MOVE      'N'           V1CRPV
037200000000E   2C                   ENDIF
037300000000E   1C                   ENDIF
037400000000     C*--------------------
037500000000     C* CONSENTITO PIU' DI UNO SCAGLIONE
037600000000     C*--------------------
037700000000IF  1C     $ERR          IFEQ      '0'
037800000000IF  2C     V1CPIU        IFNE      'S'
037900000000     C     V1CPIU        ANDNE     'N'
038000000000     C     V1CPIU        ANDNE     *BLANKS
038100000000     C                   MOVEL     MSG(10)       $MSG
038200000000     C                   EXSR      CARMSG
038300000000     C                   SETON                                        63
038400000000E   2C                   ENDIF
038500000000     C*
038600000000IF  2C     V1CPIU        IFEQ      *BLANKS
038700000000     C                   MOVE      'N'           V1CPIU
038800000000E   2C                   ENDIF
038900000000E   1C                   ENDIF
039000000000     C*--------------------
039100000000     C* TIPO VALORIZZAZIONE
039200000000     C*--------------------
039300000000IF  1C     $ERR          IFEQ      '0'
039400000000IF  2C     V1CTVA        IFNE      'F'
039500000000     C     V1CTVA        ANDNE     'Q'
039600000000     C     V1CTVA        ANDNE     'P'
039700000000     C                   MOVEL     MSG(16)       $MSG
039800000000     C                   EXSR      CARMSG
039900000000     C                   SETON                                        64
040000000000E   2C                   ENDIF
040100000000E   1C                   ENDIF
040200000000     C*
040300000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
040400000000     C* CURSORE SUL CAMPO DEL CONTROL
040500000000IF  1C     $ERR          IFEQ      '1'
040600000000     C                   MOVEL     '0'           *IN43
040700000000X   1C                   ELSE
040800000000     C                   MOVEL     '1'           *IN43
040900000000E   1C                   ENDIF
041000000000     C*
041100000000     C                   ENDSR
041200000000     C*------------------------------------------------------------------------*
041300000000     C*  RECVID - RIEMPE I CAMPI DAI RECORD TABELLA AL VIDEO
041400000000     C*------------------------------------------------------------------------*
041500000000     C     RECVID        BEGSR
041600000000     C*
041700000000     C                   MOVEL     TBLUNI        DS2R
041800000000     C*
041900000000     C                   MOVE      D62COD        V1CCOD
042000000000     C                   MOVE      §2RDES        V1CDES
042100000000     C                   MOVE      §2RRPV        V1CRPV
042200000000     C                   MOVE      §2RPIU        V1CPIU
042300000000     C                   MOVE      §2RTVA        V1CTVA
042400000000     C*
042500000000     C                   ENDSR
042600000000     C*------------------------------------------------------------------------*
042700000000     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
042800000000     C*------------------------------------------------------------------------*
042900000000     C     AZZVID        BEGSR
043000000000     C*
043100000000     C                   MOVE      *BLANKS       V1CCOD
043200000000     C                   MOVEL     *BLANKS       V1CDES
043300000000     C                   MOVE      *BLANKS       V1CRPV
043400000000     C                   MOVE      *BLANKS       V1CPIU
043500000000     C                   MOVE      *BLANKS       V1CTVA
043600000000     C*
043700000000     C                   ENDSR
043800020923     C*-------------------------------------------------------------------------
043900020923     C* REPERISCE I DATI UTENTE
044000020923     C*-------------------------------------------------------------------------
044100020923     C     REPDATIUTE    BEGSR
044200020923     C*
044300020923     C* INIZIALIZZA VARIABILI DI WRK
044400020923     C                   CLEAR                   TIBS34DS
044500020923     C                   CLEAR                   AZUTEDS
044600020923     C                   CLEAR                   DDATIUTE
044700020923     C*
044800020923     C     *DTAARA       DEFINE    §azute        azuteds
044900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
045000020923     C                   IN(E)     *DTAARA
045100020923if  1C                   IF        %Error
045200020923     c                   EVAL      I34Tla = 'L'
045300020923     C                   CALL      'TIBS34R'
045400020923     C                   PARM                    Tibs34Ds
045500020923     C                   IN        *DTAARA
045600020923e   1C                   ENDIF
045700020923     C*
045800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
045900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
046000020923     C*
046100020923     C                   ENDSR
046200000000     C*------------------------------------------------------------------------*
046300000000     C*  *INZSR - ROUTINE INIZIALE
046400000000     C*------------------------------------------------------------------------*
046500000000     C     *INZSR        BEGSR
046600000000     C*
046700000000     C* RICEVIMENTO PARAMETRI
046800000000     C     *ENTRY        PLIST
046900000000     C                   PARM                    DS62
047000000000     C*
047100000000     C* DEFINIZIONE VARIABILI
047200000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
047300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
047400000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
047500000000     C                   MOVEL     'G'           $TGES             1
047600000000     C*
047700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
047800000000     C     KEYTAB        KLIST
047900000000     C                   KFLD                    KTBKUT
048000000000     C                   KFLD                    KTBCOD
048100000000     C                   KFLD                    KTBKEY
048200000000     C*
048300000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
048400000000     C     KEYTA2        KLIST
048500000000     C                   KFLD                    KTBKUT
048600000000     C                   KFLD                    KTBCOD
048700000000     C*
048800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
048900000000     C                   EXSR      INZSTD
049000000000     C*
049100000000     C* RAGIONE SOCIALE UTENTE
049200000000     C                   MOVEL     DSFIRS        VTDRSU
049300000000     C*
049400000000     C* DECODIFICA OPZIONE RICHIAMATA
049500000000IF  1C     D62OP1        IFEQ      'F06'
049600000000     C                   MOVEL     DOPZ(1)       VTDFUN
049700000000E   1C                   ENDIF
049800000000IF  2C     D62OP1        IFEQ      'O02'
049900000000     C                   MOVEL     DOPZ(2)       VTDFUN
050000000000E   1C                   ENDIF
050100000000IF  1C     D62OP1        IFEQ      'O04'
050200000000     C                   MOVEL     DOPZ(3)       VTDFUN
050300000000E   1C                   ENDIF
050400000000IF  1C     D62OP1        IFEQ      'O05'
050500000000     C                   MOVEL     DOPZ(5)       VTDFUN
050600000000E   1C                   ENDIF
050700000000IF  1C     D62OP1        IFEQ      'O07'
050800000000     C                   MOVEL     DOPZ(4)       VTDFUN
050900000000E   1C                   ENDIF
051000000000     C*
051100000000     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
051200000000     C* 0=NON PROTETTO, 1=PROTETTO
051300000000     C*   90 - CAMPI CHIAVE (CODICE ELEMENTO)
051400000000     C*   91 - ALTRI CAMPI
051500000000IF  1C     D62OP1        IFEQ      'F06'
051600000000     C                   SETOFF                                       9091
051700000000E   1C                   ENDIF
051800000000IF  1C     D62OP1        IFEQ      'O02'
051900000000     C                   SETON                                        90
052000000000E   1C                   ENDIF
052100000000IF  1C     D62OP1        IFEQ      'O04'
052200000000     C     D62OP1        OREQ      'O05'
052300000000     C     D62OP1        OREQ      'O07'
052400000000     C                   SETON                                        9091
052500000000E   1C                   ENDIF
052600000000     C*
052700000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
052800000000IF  1C     D62OP1        IFEQ      'O05'
052900000000     C                   MOVEL     FUN(6)        FUNRIG
053000000000     C                   MOVEL     'N'           FUNASG
053100000000     C                   MOVEL     'N'           FUNASI
053200000000     C                   MOVEL     'N'           FUNAFG
053300000000     C                   MOVEL     'N'           FUNAFG
053400000000     C                   MOVEL     FUNRIG        FUN(6)
053500000000E   1C                   ENDIF
053600000000     C*
053700000000     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
053800000000IF  1C     D62OP1        IFEQ      'O02'
053900000000     C     D62OP1        OREQ      'O04'
054000000000     C     D62OP1        OREQ      'O05'
054100000000     C     D62OP1        OREQ      'O07'
054200020923     C                   MOVEL     1             KTBKUT
054300000000     C                   MOVEL     '2R'          KTBCOD
054400000000     C                   MOVEL     *BLANKS       KTBKEY
054500000000     C                   MOVEL     D62COD        KTBKEY
054600000000     C*
054700000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
054800000000     C     KEYTAB        CHAIN     TABEL00F                           9998
054900000000IF  2C     *IN99         IFEQ      '1'
055000000000     C                   MOVEL     '1'           $ERR
055100000000     C                   MOVEL     '1'           $FINE
055200000000     C                   MOVEL     MSG(14)       D62MSG
055300000000X   2C                   ELSE
055400000000IF  3C     *IN98         IFEQ      '1'
055500000000     C                   MOVEL     '1'           $ERR
055600000000     C                   MOVEL     '1'           $FINE
055700000000     C                   MOVEL     MSG(15)       D62MSG
055800000000X   3C                   ELSE
055900000000     C                   EXSR      RECVID
056000000000     C                   EXSR      CARV1
056100000000E   3C                   ENDIF
056200000000E   2C                   ENDIF
056300000000E   1C                   ENDIF
056400000000     C*
056500000000     C* SE INSERIMENTO
056600000000IF  1C     D62OP1        IFEQ      'F06'
056700000000     C                   EXSR      AZZVID
056800000000     C                   EXSR      CARV1
056900000000E   1C                   ENDIF
057000000000     C*
057100000000     C                   ENDSR
057200000000     C*------------------------------------------------------------------------*
057300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057500000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
057600000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
057700000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
057800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
058000000000     C*------------------------------------------------------------------------*
058100000000     C* INZSTD - ROUTINE INIZIALE STANDARD
058200000000     C*------------------------------------------------------------------------*
058300000000     C     INZSTD        BEGSR
058400000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
058500000000     C                   CALL      'XKEYS'
058600000000     C                   PARM                    DSKEYS
058700000000     C                   MOVEA     KEYRIG        KEYS
058800000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
058900000000     C     XMSGPA        PLIST
059000000000     C                   PARM                    DSMSMS
059100000000     C                   PARM                    DSMSPG
059200000000     C                   PARM                    DSMSKE
059300000000     C                   PARM                    DSMSER
059400000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
059500000000     C                   MOVEL     'TRTB63R'     DSMSPG
059600000000     C                   MOVEL     'TRTB63R'     VMCQUE
059700000000     C                   MOVEL     *BLANKS       VMCKEY
059800000000     C                   MOVEL     *BLANKS       DSMSMS
059900000000     C* INIZIALIZZO FLAG CONTROLLO PGM
060000000000     C                   MOVEL     '0'           $ERR              1
060100000000     C                   MOVEL     '0'           $F03              1
060200000000     C                   MOVEL     *BLANKS       $GES              2
060300000000     C                   MOVEL     *BLANKS       $MSG             80
060400000000     C                   MOVEL     '0'           $FINE             1
060500000000     C                   MOVEL     '0'           $TESVD            1
060600000000     C* LANCIO ROUTINE PER VERIFICARE SE
060700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
060800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
060900020923     C                   EXSR      REPDATIUTE
061000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
061100000000     C                   BITOFF    '01234567'    POSMI1            1
061200000000     C                   BITOFF    '01234567'    POSMI2            1
061300000000     C                   BITOFF    '01234567'    POSMA1            1
061400000000     C                   BITOFF    '01234567'    POSMA2            1
061500000000     C                   BITON     '34'          POSMI1            1
061600000000     C                   BITON     '7'           POSMI2            1
061700000000     C                   BITON     '34'          POSMA1            1
061800000000     C                   BITON     '0'           POSMA2            1
061900000000     C                   MOVE      POSMI2        POSMIN            2
062000000000     C                   MOVEL     POSMI1        POSMIN
062100000000     C                   MOVE      POSMA2        POSMAX            2
062200000000     C                   MOVEL     POSMA1        POSMAX
062300000000     C                   ENDSR
062400000000     C*------------------------------------------------------------------------*
062500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
062600000000     C*------------------------------------------------------------------------*
062700000000     C     FUNSTD        BEGSR
062800000000     C                   SELECT
062900000000     C     KEY           WHENEQ    F3
063000000000     C                   EXSR      F03STD
063100000000     C     KEY           WHENEQ    F12
063200000000     C                   EXSR      F12STD
063300000000     C     KEY           WHENEQ    F24
063400000000     C                   EXSR      F24STD
063500000000     C                   OTHER
063600000000     C                   MOVEL     MSG(2)        $MSG
063700000000     C                   EXSR      CARMSG
063800000000     C                   ENDSL
063900000000     C                   ENDSR
064000000000     C*------------------------------------------------------------------------*
064100000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
064200000000     C*------------------------------------------------------------------------*
064300000000     C     F03STD        BEGSR
064400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
064500000000     C                   MOVEL     '1'           $F03
064600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
064700000000     C                   MOVEL     '1'           $FINE
064800000000     C                   ENDSR
064900000000     C*------------------------------------------------------------------------*
065000000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
065100000000     C*------------------------------------------------------------------------*
065200000000     C     F12STD        BEGSR
065300000000     C* IMPOSTO FLAG DI FIN PGM
065400000000     C                   MOVEL     '1'           $FINE
065500000000     C                   ENDSR
065600000000     C*------------------------------------------------------------------------*
065700000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
065800000000     C*------------------------------------------------------------------------*
065900000000     C     F24STD        BEGSR
066000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
066100000000     C                   EXSR      OUTFUN
066200000000     C                   ENDSR
066300000000     C*------------------------------------------------------------------------*
066400000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
066500000000     C*          CODA MESSAGGI DEL PGM
066600000000     C*------------------------------------------------------------------------*
066700000000     C     CARMSG        BEGSR
066800000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
066900000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
067000000000     C     MSGULT        IFEQ      0
067100000000     C                   MOVEL     '1'           *IN40
067200000000     C                   WRITE     TB63DVK
067300000000     C                   MOVEL     '0'           *IN40
067400000000     C                   ENDIF
067500000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
067600000000     C                   MOVEL     $MSG          DSMSMS
067700000000     C                   CALL      'XMSG'        XMSGPA
067800000000     C     DSMSER        IFEQ      0
067900000000     C                   MOVEL     DSMSKE        VMCKEY
068000000000     C                   ENDIF
068100000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
068200000000     C                   ADD       1             MSGULT            4 0
068300000000     C                   WRITE     TB63DVM
068400000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
068500000000     C                   MOVEL     *BLANKS       $MSG
068600000000     C                   ENDSR
068700000000     C*------------------------------------------------------------------------*
068800000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
068900000000     C*------------------------------------------------------------------------*
069000000000     C     OUTMSG        BEGSR
069100000000     C* VISUALIZZAZIONE MESSAGGIO
069200000000     C                   MOVEL     '1'           *IN44
069300000000     C                   WRITE     TB63DVK
069400000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
069500000000     C                   Z-ADD     *ZEROS        MSGULT
069600000000     C                   ENDSR
069700000000     C*------------------------------------------------------------------------*
069800000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
069900000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
070000000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
070100000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
070200000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
070300000000     C*------------------------------------------------------------------------*
070400000000     C     CARFUN        BEGSR
070500000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
070600000000     C     1             DO        31            I                 5 0
070700000000     C                   MOVEL     FUN(I)        FUNRIG
070800000000     C                   MOVEL     FUNFUN        FFU(I)
070900000000     C                   MOVEL     FUNDES        FDE(I)
071000020923     C     DUTLPO        IFEQ      'S'
071100000000     C     $TGES         IFEQ      'G'
071200000000     C                   MOVEL     FUNASG        FAT(I)
071300000000     C                   ELSE
071400000000     C                   MOVEL     FUNASI        FAT(I)
071500000000     C                   ENDIF
071600000000     C                   ELSE
071700000000     C     $TGES         IFEQ      'G'
071800000000     C                   MOVEL     FUNAFG        FAT(I)
071900000000     C                   ELSE
072000000000     C                   MOVEL     FUNAFI        FAT(I)
072100000000     C                   ENDIF
072200000000     C                   ENDIF
072300000000     C     FAT(I)        IFEQ      'E'
072400000000     C                   MOVEL     USER          WEDP              3
072500000000     C     WEDP          IFEQ      'EDP'
072600000000     C                   MOVEL     'S'           FAT(I)
072700000000     C                   ELSE
072800000000     C                   MOVEL     'N'           FAT(I)
072900000000     C                   ENDIF
073000000000     C                   ENDIF
073100000000     C                   ENDDO
073200000000     C                   ENDSR
073300000000     C*------------------------------------------------------------------------*
073400000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
073500000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
073600000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
073700000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
073800000000     C*------------------------------------------------------------------------*
073900000000     C     OUTFUN        BEGSR
074000000000     C     1             DO        8             I
074100000000     C                   MOVEL     *BLANKS       FUV(I)
074200000000     C                   ENDDO
074300000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
074400000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
074500000000     C     1             DO        31            I
074600000000     C     FAT(I)        IFEQ      'S'
074700000000     C     FDE(I)        ANDNE     *BLANKS
074800000000     C                   ADD       1             FUNNAT
074900000000     C                   ENDIF
075000000000     C                   ENDDO
075100000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
075200000000     C     FUNNAT        IFGT      8
075300000000     C                   MOVEL     'S'           FAT(24)
075400000000     C                   ENDIF
075500000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
075600000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
075700000000     C                   Z-ADD     8             FUNVIS            5 0
075800000000     C     FAT(23)       IFEQ      'S'
075900000000     C                   SUB       1             FUNVIS
076000000000     C                   ENDIF
076100000000     C     FAT(24)       IFEQ      'S'
076200000000     C                   SUB       1             FUNVIS
076300000000     C                   ENDIF
076400000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
076500000000     C                   Z-ADD     *ZEROS        W                 5 0
076600000000     C                   ADD       1             FUNULT
076700000000     C     FUNULT        IFGT      31
076800000000     C                   Z-ADD     1             FUNULT
076900000000     C                   ENDIF
077000000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
077100000000     C     FUNULT        DO        31            I
077200000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
077300000000     C     W             IFLT      FUNVIS
077400000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
077500000000     C     FAT(I)        IFEQ      'S'
077600000000     C     FDE(I)        ANDNE     *BLANKS
077700000000     C     I             IFEQ      23
077800000000     C     I             OREQ      24
077900000000     C                   ELSE
078000000000     C                   ADD       1             W
078100000000     C                   MOVEL     FDE(I)        FUV(W)
078200000000     C                   ENDIF
078300000000     C                   ENDIF
078400000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
078500000000     C                   Z-ADD     I             FUNULT
078600000000     C                   ENDIF
078700000000     C                   ENDDO
078800000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
078900000000     C                   MOVEL     FUV(1)        VFD001
079000000000     C                   MOVEL     FUV(2)        VFD002
079100000000     C                   MOVEL     FUV(3)        VFD003
079200000000     C                   MOVEL     FUV(4)        VFD004
079300000000     C                   MOVEL     FUV(5)        VFD005
079400000000     C                   MOVEL     FUV(6)        VFD006
079500000000     C     FAT(23)       IFEQ      'N'
079600000000     C     FAT(24)       ANDEQ     'N'
079700000000     C                   MOVEL     FUV(7)        VFD007
079800000000     C                   MOVEL     FUV(8)        VFD008
079900000000     C                   ELSE
080000000000     C     FAT(23)       IFEQ      'S'
080100000000     C     FAT(24)       ANDEQ     'S'
080200000000     C                   MOVEL     FDE(23)       VFD007
080300000000     C                   MOVEL     FDE(24)       VFD008
080400000000     C                   ELSE
080500000000     C                   MOVEL     FUV(7)        VFD007
080600000000     C     FAT(23)       IFEQ      'S'
080700000000     C                   MOVEL     FDE(23)       VFD008
080800000000     C                   ELSE
080900000000     C                   MOVEL     FDE(24)       VFD008
081000000000     C                   ENDIF
081100000000     C                   ENDIF
081200000000     C                   ENDIF
081300000000     C                   MOVEL     '1'           $TESVD
081400000000     C                   ENDSR
081500000000     C*------------------------------------------------------------------------*
081600000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
081700000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
081800000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
081900000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
082000000000     C*------------------------------------------------------------------------*
082100000000     C     CONFUN        BEGSR
082200000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
082300000000     C     1             DO        31            I
082400000000     C     KEY           IFEQ      KEYS(I)
082500000000     C* CONTROLLO SE IL TASTO E' ATTIVO
082600000000     C     FAT(I)        IFNE      'S'
082700000000     C                   MOVEL     MSG(1)        $MSG
082800000000     C                   EXSR      CARMSG
082900000000     C                   MOVEL     '1'           $ERR
083000000000     C                   ENDIF
083100000000     C                   ENDIF
083200000000     C                   ENDDO
083300000000     C                   ENDSR
083400000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
083500000000TRTB63R- Tasto funzionale non valido.                                          1
083600000000TRTB63R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
083700000000TRTB63R- Opzione non valida.                                                   3
083800000000TRTB63R- Opzione abilitata ma non gestita. Avvertire CED.                      4
083900000000TRTB63R- Inizio elenco.                                                        5
084000000000TRTB63R- Fine elenco.                                                          6
084100000000TRTB63R- Immettere codice elemento non esistente                               7
084200000000TRTB63R- Immettere codice elemento                                             8
084300000000TRTB63R- Immettere la descrizione                                              9
084400000000TRTB63R- Immettere S=Si oppure N=No                                           10
084500000000TRTB63R- 11
084600000000TRTB63R- 12
084700000000TRTB63R- 13
084800000000TRTB63R- Richiesta funzione per codice già esistente                          14
084900000000TRTB63R- Codice momentaneamente allocato: riprovare più tardi                 15
085000000000TRTB63R- Immettere tipo valorizzazione valido                                 16
085100000000TRTB63R- Inserimento effettuato.                                              17
085200000000TRTB63R- Modifica effettuata.                                                 18
085300000000TRTB63R- Cancellazione effettuata.                                            19
085400000000TRTB63R- Ripristino effettuato.                                               20
085500000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
085600000000F1    -                   -    NN     NN
085700000000F2    -                   -    NN     NN
085800000000F3    -F3=Fine            -    SS     SS
085900000000F4    -                   -    NN     NN
086000000000F5    -                   -    NN     NN
086100000000F6    -F6=Aggiornamento   -    SS     SS
086200000000F7    -                   -    NN     NN
086300000000F8    -                   -    NN     NN
086400000000F9    -                   -    NN     NN
086500000000F10   -                   -    NN     NN
086600000000F11   -                   -    NN     NN
086700000000F12   -F12=Ritorno        -    SS     SS
086800000000F13   -                   -    NN     NN
086900000000F14   -                   -    NN     NN
087000000000F15   -                   -    NN     NN
087100000000F16   -                   -    NN     NN
087200000000F17   -                   -    NN     NN
087300000000F18   -                   -    NN     NN
087400000000F19   -                   -    NN     NN
087500000000F20   -                   -    NN     NN
087600000000F21   -                   -    NN     NN
087700000000F22   -                   -    NN     NN
087800000000F23   -                   -    NN     NN
087900000000F24   -F24=Altri tasti    -    NN     NN
088000000000ENTER -                   -    SS     SS
088100000000ROLLUP-                   -    NN     NN
088200000000ROLLDN-                   -    NN     NN
088300000000HELP  -                   -    NN     NN
088400000000PRINT -                   -    NN     NN
088500000000BSPACE-                   -    NN     NN
088600000000CLEAR -                   -    NN     NN
088700000000** DOPZ  (OPZIONE RICHIAMATA)
088800000000* INSERIMENTO *                                                 1
088900000000*  VARIAZIONE  *                                                2
089000000000*CANCELLAZIONE*                                                 3
089100000000*  RIPRISTINO  *                                                4
089200000000VISUALIZZAZIONE                                                 5

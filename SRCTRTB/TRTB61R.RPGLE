000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TRTB61R
000300000000      *  Descrizione   :  Gesione tabella riferimenti tariffa di c/e
000400000000      *                   Tabella 2Q
000500000000      *                   Programma modifica, inserimento, visualizzazione, ...
000600000000      *  Autore        :  STEFANO (SC)
000700000000      *  Data creazione:  02 GIU 1994
000800000000      *------------------------------------------------------------------------*
000900000000     H DECEDIT('0,') DATEDIT(*DMY.)
001000000000     F*------------------------------------------------------------------------*
001100000000     F* DATA BASE
001200000000     F*------------------------------------------------------------------------*
001300000000     FTRTB61D   CF   E             WORKSTN INFDS(DEVDS1)
001400000000     F                                     SFILE(TB61DVM:MSGULT)
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
007400000000     D DS2Q          E DS
007500000000     D*---------------------
007600000000     D* DS ESTERNA DI PROCEDURA
007700000000     D*---------------------
007800000000     D DS60          E DS                  EXTNAME(TRTB60DS)
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
009100000000     C                   MOVEL     '1'           D60ERR
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
011000000000     C                   MOVEL     '1'           D60F03
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
012400000000     C                   WRITE     TB61DVT
012500000000     C                   WRITE     TB61DVF
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
014100000000     C                   EXFMT     TB61DV1
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
016600000000IF  1C     D60OP1        IFEQ      'O05'
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
017700000000IF  1C     D60OP1        IFEQ      'F06'
017800000000     C                   EXSR      INSERI
017900000000E   1C                   ENDIF
018000000000IF  1C     D60OP1        IFEQ      'O02'
018100000000     C                   EXSR      MODIFI
018200000000E   1C                   ENDIF
018300000000IF  1C     D60OP1        IFEQ      'O04'
018400000000     C                   EXSR      CANCEL
018500000000E   1C                   ENDIF
018600000000IF  1C     D60OP1        IFEQ      'O07'
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
020200000000     C                   MOVEL     '2Q'          TBLCOD
020300000000     C                   MOVEL     V1CCOD        TBLKEY
020400000000     C                   MOVEL     *BLANKS       TBLFLG
020500000000     C                   MOVEL     '1'           TBLFTT
020600000000     C                   MOVEL     *BLANKS       TBLFTR
020700000000     C                   Z-ADD     *ZEROS        TBLDTR
020800000000     C* D S 2 Q
020900000000     C                   MOVE      V1CDES        §2QDES
021000000000     C                   MOVE      V1CTTV        §2QTTV
021100000000     C                   MOVE      V1CFPA        §2QFPA
021200000000     C                   MOVE      V1CFAR        §2QFAR
021300000000     C                   MOVEL     DS2Q          TBLUNI
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
023900000000     C                   MOVEL     '2Q'          KTBCOD
024000000000     C                   MOVEL     *BLANKS       KTBKEY
024100000000     C                   MOVEL     D60COD        KTBKEY
024200000000     C     KEYTAB        CHAIN     TABEL00F                           99
024300000000     C                   MOVE      V1CDES        §2QDES
024400000000     C                   MOVE      V1CTTV        §2QTTV
024500000000     C                   MOVE      V1CFPA        §2QFPA
024600000000     C                   MOVE      V1CFAR        §2QFAR
024700000000     C*
024800000000     C                   MOVEL     DS2Q          TBLUNI
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
025900000000     C                   MOVEL     MSG(18)       D60MSG
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
027200000000     C                   MOVEL     '2Q'          KTBCOD
027300000000     C                   MOVEL     *BLANKS       KTBKEY
027400000000     C                   MOVEL     D60COD        KTBKEY
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
028600000000     C                   MOVEL     MSG(19)       D60MSG
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
029700000000     C                   MOVEL     '2Q'          KTBCOD
029800000000     C                   MOVEL     *BLANKS       KTBKEY
029900000000     C                   MOVEL     D60COD        KTBKEY
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
031100000000     C                   MOVEL     MSG(20)       D60MSG
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
032300000000     C* CODICE COMMERCIALE (SOLO SE INSERIMENTO)
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
033400000000     C     D60OP1        ANDEQ     'F06'
033500020923     C                   MOVEL     1             KTBKUT
033600000000     C                   MOVEL     '2Q'          KTBCOD
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
035900000000     C* TIPO TARIFFA
036000000000     C*--------------------
036100000000IF  1C     $ERR          IFEQ      '0'
036200000000     C                   MOVE      *BLANKS       V1CTTD
036300000000     C                   MOVEL     '2S'          §COD
036400000000     C                   MOVEL     *BLANKS       §KEY
036500000000     C                   MOVEL     V1CTTV        §KEY
036600000000     C                   EXSR      CHKTAB
036700000000     C                   MOVEL     §KEY          V1CTTV
036800000000     C                   MOVEL     §DES          V1CTTD
036900000000IF  2C     $ERR          IFEQ      '1'
037000000000     C                   MOVEL     MSG(10)       $MSG
037100000000     C                   EXSR      CARMSG
037200000000     C                   SETON                                        62
037300000000E   2C                   ENDIF
037400000000E   1C                   ENDIF
037500000000     C*--------------------
037600000000     C* FLAG 1° FILIALE
037700000000     C*--------------------
037800000000IF  1C     $ERR          IFEQ      '0'
037900000000     C*
038000000000IF  2C     V1CTTV        IFEQ      'F'
038100000000IF  3C     V1CFPA        IFNE      'L'
038200000000     C     V1CFPA        ANDNE     'T'
038300000000     C                   MOVE      '1'           $ERR
038400000000     C                   MOVEL     MSG(11)       $MSG
038500000000     C                   EXSR      CARMSG
038600000000     C                   SETON                                        63
038700000000E   3C                   ENDIF
038800000000     C*
038900000000X   2C                   ELSE
039000000000     C*
039100000000IF  3C     V1CFPA        IFNE      *BLANKS
039200000000     C                   MOVE      '1'           $ERR
039300000000     C                   MOVEL     MSG(12)       $MSG
039400000000     C                   EXSR      CARMSG
039500000000     C                   SETON                                        63
039600000000E   3C                   ENDIF
039700000000     C*
039800000000E   2C                   ENDIF
039900000000E   1C                   ENDIF
040000000000     C*--------------------
040100000000     C* FLAG 2° FILIALE
040200000000     C*--------------------
040300000000IF  1C     $ERR          IFEQ      '0'
040400000000     C*
040500000000IF  2C     V1CTTV        IFEQ      'F'
040600000000IF  3C     V1CFAR        IFNE      'L'
040700000000     C     V1CFAR        ANDNE     'T'
040800000000     C     V1CFAR        ANDNE     *BLANKS
040900000000     C                   MOVE      '1'           $ERR
041000000000     C                   MOVEL     MSG(11)       $MSG
041100000000     C                   EXSR      CARMSG
041200000000     C                   SETON                                        64
041300000000E   3C                   ENDIF
041400000000     C*
041500000000X   2C                   ELSE
041600000000     C*
041700000000IF  3C     V1CFAR        IFNE      *BLANKS
041800000000     C                   MOVE      '1'           $ERR
041900000000     C                   MOVEL     MSG(12)       $MSG
042000000000     C                   EXSR      CARMSG
042100000000     C                   SETON                                        64
042200000000E   3C                   ENDIF
042300000000     C*
042400000000E   2C                   ENDIF
042500000000E   1C                   ENDIF
042600000000     C*
042700000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
042800000000     C* CURSORE SUL CAMPO DEL CONTROL
042900000000IF  1C     $ERR          IFEQ      '1'
043000000000     C                   MOVEL     '0'           *IN43
043100000000X   1C                   ELSE
043200000000     C                   MOVEL     '1'           *IN43
043300000000E   1C                   ENDIF
043400000000     C*
043500000000     C                   ENDSR
043600000000     C*------------------------------------------------------------------------*
043700000000     C* CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
043800000000     C*          INPUT --> §COD: CODICE TABELLA
043900000000     C*                    $KEY: CODICE ELEMENTO
044000000000     C*------------------------------------------------------------------------*
044100000000     C     CHKTAB        BEGSR
044200000000     C*
044300000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
044400000000     C     '?'           SCAN      §KEY                                   99
044500000000     C     *IN99         IFEQ      *ON
044600020923     C                   MOVEL     1             §KUT
044700000000     C                   MOVE      *BLANKS       §DES
044800000000     C                   CALL      'X§TABER'
044900000000     C                   PARM                    §KUT              1 0
045000000000     C                   PARM                    §COD              2
045100000000     C                   PARM                    §KEY              8
045200000000     C                   PARM                    §DES             30
045300000000     C                   MOVEL     '1'           $PUNTO            1
045400000000     C*
045500000000     C                   ENDIF
045600000000     C*
045700000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
045800020923     C                   MOVEL     1             KTBKUT
045900000000     C                   MOVEL     §COD          KTBCOD
046000000000     C                   MOVE      *BLANKS       KTBKEY
046100000000     C                   MOVEL     §KEY          KTBKEY
046200000000     C     KEYTAB        CHAIN(N)  TABEL00F                           99
046300000000     C     *IN99         IFEQ      '1'
046400000000     C     TBLFLG        ORNE      *BLANKS
046500000000     C                   MOVEL     '1'           $ERR
046600000000     C                   MOVEL     *ALL'*'       §DES
046700000000     C                   ELSE
046800000000     C                   MOVEL     TBLUNI        §DES
046900000000     C                   ENDIF
047000000000     C*
047100000000     C                   ENDSR
047200000000     C*------------------------------------------------------------------------*
047300000000     C*  RECVID - RIEMPE I CAMPI DAI RECORD TABELLA AL VIDEO
047400000000     C*------------------------------------------------------------------------*
047500000000     C     RECVID        BEGSR
047600000000     C*
047700000000     C                   MOVEL     TBLUNI        DS2Q
047800000000     C*
047900000000     C                   MOVE      D60COD        V1CCOD
048000000000     C                   MOVE      §2QDES        V1CDES
048100000000     C                   MOVE      §2QTTV        V1CTTV
048200000000     C                   MOVE      §2QFPA        V1CFPA
048300000000     C                   MOVE      §2QFAR        V1CFAR
048400000000     C*
048500000000     C                   ENDSR
048600000000     C*------------------------------------------------------------------------*
048700000000     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
048800000000     C*------------------------------------------------------------------------*
048900000000     C     AZZVID        BEGSR
049000000000     C*
049100000000     C                   MOVE      *BLANKS       V1CCOD
049200000000     C                   MOVEL     *BLANKS       V1CDES
049300000000     C                   MOVE      *BLANKS       V1CTTV
049400000000     C                   MOVE      *BLANKS       V1CFPA
049500000000     C                   MOVE      *BLANKS       V1CFAR
049600000000     C*
049700000000     C                   ENDSR
049800020923     C*-------------------------------------------------------------------------
049900020923     C* REPERISCE I DATI UTENTE
050000020923     C*-------------------------------------------------------------------------
050100020923     C     REPDATIUTE    BEGSR
050200020923     C*
050300020923     C* INIZIALIZZA VARIABILI DI WRK
050400020923     C                   CLEAR                   TIBS34DS
050500020923     C                   CLEAR                   AZUTEDS
050600020923     C                   CLEAR                   DDATIUTE
050700020923     C*
050800020923     C     *DTAARA       DEFINE    §azute        azuteds
050900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
051000020923     C                   IN(E)     *DTAARA
051100020923if  1C                   IF        %Error
051200020923     c                   EVAL      I34Tla = 'L'
051300020923     C                   CALL      'TIBS34R'
051400020923     C                   PARM                    Tibs34Ds
051500020923     C                   IN        *DTAARA
051600020923e   1C                   ENDIF
051700020923     C*
051800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
051900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
052000020923     C*
052100020923     C                   ENDSR
052200000000     C*------------------------------------------------------------------------*
052300000000     C*  *INZSR - ROUTINE INIZIALE
052400000000     C*------------------------------------------------------------------------*
052500000000     C     *INZSR        BEGSR
052600000000     C*
052700000000     C* RICEVIMENTO PARAMETRI
052800000000     C     *ENTRY        PLIST
052900000000     C                   PARM                    DS60
053000000000     C*
053100000000     C* DEFINIZIONE VARIABILI
053200000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
053300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
053400000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
053500000000     C                   MOVEL     'G'           $TGES             1
053600000000     C*
053700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
053800000000     C     KEYTAB        KLIST
053900000000     C                   KFLD                    KTBKUT
054000000000     C                   KFLD                    KTBCOD
054100000000     C                   KFLD                    KTBKEY
054200000000     C*
054300000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
054400000000     C     KEYTA2        KLIST
054500000000     C                   KFLD                    KTBKUT
054600000000     C                   KFLD                    KTBCOD
054700000000     C*
054800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
054900000000     C                   EXSR      INZSTD
055000000000     C*
055100000000     C* RAGIONE SOCIALE UTENTE
055200000000     C                   MOVEL     DSFIRS        VTDRSU
055300000000     C*
055400000000     C* DECODIFICA OPZIONE RICHIAMATA
055500000000IF  1C     D60OP1        IFEQ      'F06'
055600000000     C                   MOVEL     DOPZ(1)       VTDFUN
055700000000E   1C                   ENDIF
055800000000IF  2C     D60OP1        IFEQ      'O02'
055900000000     C                   MOVEL     DOPZ(2)       VTDFUN
056000000000E   1C                   ENDIF
056100000000IF  1C     D60OP1        IFEQ      'O04'
056200000000     C                   MOVEL     DOPZ(3)       VTDFUN
056300000000E   1C                   ENDIF
056400000000IF  1C     D60OP1        IFEQ      'O05'
056500000000     C                   MOVEL     DOPZ(5)       VTDFUN
056600000000E   1C                   ENDIF
056700000000IF  1C     D60OP1        IFEQ      'O07'
056800000000     C                   MOVEL     DOPZ(4)       VTDFUN
056900000000E   1C                   ENDIF
057000000000     C*
057100000000     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
057200000000     C* 0=NON PROTETTO, 1=PROTETTO
057300000000     C*   90 - CAMPI CHIAVE (CODICE ELEMENTO)
057400000000     C*   91 - ALTRI CAMPI
057500000000IF  1C     D60OP1        IFEQ      'F06'
057600000000     C                   SETOFF                                       9091
057700000000E   1C                   ENDIF
057800000000IF  1C     D60OP1        IFEQ      'O02'
057900000000     C                   SETON                                        90
058000000000E   1C                   ENDIF
058100000000IF  1C     D60OP1        IFEQ      'O04'
058200000000     C     D60OP1        OREQ      'O05'
058300000000     C     D60OP1        OREQ      'O07'
058400000000     C                   SETON                                        9091
058500000000E   1C                   ENDIF
058600000000     C*
058700000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
058800000000IF  1C     D60OP1        IFEQ      'O05'
058900000000     C                   MOVEL     FUN(6)        FUNRIG
059000000000     C                   MOVEL     'N'           FUNASG
059100000000     C                   MOVEL     'N'           FUNASI
059200000000     C                   MOVEL     'N'           FUNAFG
059300000000     C                   MOVEL     'N'           FUNAFG
059400000000     C                   MOVEL     FUNRIG        FUN(6)
059500000000E   1C                   ENDIF
059600000000     C*
059700000000     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
059800000000IF  1C     D60OP1        IFEQ      'O02'
059900000000     C     D60OP1        OREQ      'O04'
060000000000     C     D60OP1        OREQ      'O05'
060100000000     C     D60OP1        OREQ      'O07'
060200020923     C                   MOVEL     1             KTBKUT
060300000000     C                   MOVEL     '2Q'          KTBCOD
060400000000     C                   MOVEL     *BLANKS       KTBKEY
060500000000     C                   MOVEL     D60COD        KTBKEY
060600000000     C*
060700000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
060800000000     C     KEYTAB        CHAIN     TABEL00F                           9998
060900000000IF  2C     *IN99         IFEQ      '1'
061000000000     C                   MOVEL     '1'           $ERR
061100000000     C                   MOVEL     '1'           $FINE
061200000000     C                   MOVEL     MSG(14)       D60MSG
061300000000X   2C                   ELSE
061400000000IF  3C     *IN98         IFEQ      '1'
061500000000     C                   MOVEL     '1'           $ERR
061600000000     C                   MOVEL     '1'           $FINE
061700000000     C                   MOVEL     MSG(15)       D60MSG
061800000000X   3C                   ELSE
061900000000     C                   EXSR      RECVID
062000000000     C                   EXSR      CARV1
062100000000E   3C                   ENDIF
062200000000E   2C                   ENDIF
062300000000E   1C                   ENDIF
062400000000     C*
062500000000     C* SE INSERIMENTO
062600000000IF  1C     D60OP1        IFEQ      'F06'
062700000000     C                   EXSR      AZZVID
062800000000     C                   EXSR      CARV1
062900000000     C                   ELSE
063000000000     C                   EXSR      DECODI
063100000000E   1C                   ENDIF
063200000000     C*
063300000000     C                   ENDSR
063400000000     C*------------------------------------------------------------------------*
063500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063700000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
063800000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
063900000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
064000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
064100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
064200000000     C*------------------------------------------------------------------------*
064300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
064400000000     C*------------------------------------------------------------------------*
064500000000     C     INZSTD        BEGSR
064600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
064700000000     C                   CALL      'XKEYS'
064800000000     C                   PARM                    DSKEYS
064900000000     C                   MOVEA     KEYRIG        KEYS
065000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
065100000000     C     XMSGPA        PLIST
065200000000     C                   PARM                    DSMSMS
065300000000     C                   PARM                    DSMSPG
065400000000     C                   PARM                    DSMSKE
065500000000     C                   PARM                    DSMSER
065600000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
065700000000     C                   MOVEL     'TRTB61R'     DSMSPG
065800000000     C                   MOVEL     'TRTB61R'     VMCQUE
065900000000     C                   MOVEL     *BLANKS       VMCKEY
066000000000     C                   MOVEL     *BLANKS       DSMSMS
066100000000     C* INIZIALIZZO FLAG CONTROLLO PGM
066200000000     C                   MOVEL     '0'           $ERR              1
066300000000     C                   MOVEL     '0'           $F03              1
066400000000     C                   MOVEL     *BLANKS       $GES              2
066500000000     C                   MOVEL     *BLANKS       $MSG             80
066600000000     C                   MOVEL     '0'           $FINE             1
066700000000     C                   MOVEL     '0'           $TESVD            1
066800000000     C* LANCIO ROUTINE PER VERIFICARE SE
066900000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
067000000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
067100020923     C                   EXSR      REPDATIUTE
067200000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
067300000000     C                   BITOFF    '01234567'    POSMI1            1
067400000000     C                   BITOFF    '01234567'    POSMI2            1
067500000000     C                   BITOFF    '01234567'    POSMA1            1
067600000000     C                   BITOFF    '01234567'    POSMA2            1
067700000000     C                   BITON     '34'          POSMI1            1
067800000000     C                   BITON     '7'           POSMI2            1
067900000000     C                   BITON     '34'          POSMA1            1
068000000000     C                   BITON     '0'           POSMA2            1
068100000000     C                   MOVE      POSMI2        POSMIN            2
068200000000     C                   MOVEL     POSMI1        POSMIN
068300000000     C                   MOVE      POSMA2        POSMAX            2
068400000000     C                   MOVEL     POSMA1        POSMAX
068500000000     C                   ENDSR
068600000000     C*------------------------------------------------------------------------*
068700000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
068800000000     C*------------------------------------------------------------------------*
068900000000     C     FUNSTD        BEGSR
069000000000     C                   SELECT
069100000000     C     KEY           WHENEQ    F3
069200000000     C                   EXSR      F03STD
069300000000     C     KEY           WHENEQ    F12
069400000000     C                   EXSR      F12STD
069500000000     C     KEY           WHENEQ    F24
069600000000     C                   EXSR      F24STD
069700000000     C                   OTHER
069800000000     C                   MOVEL     MSG(2)        $MSG
069900000000     C                   EXSR      CARMSG
070000000000     C                   ENDSL
070100000000     C                   ENDSR
070200000000     C*------------------------------------------------------------------------*
070300000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
070400000000     C*------------------------------------------------------------------------*
070500000000     C     F03STD        BEGSR
070600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
070700000000     C                   MOVEL     '1'           $F03
070800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
070900000000     C                   MOVEL     '1'           $FINE
071000000000     C                   ENDSR
071100000000     C*------------------------------------------------------------------------*
071200000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
071300000000     C*------------------------------------------------------------------------*
071400000000     C     F12STD        BEGSR
071500000000     C* IMPOSTO FLAG DI FIN PGM
071600000000     C                   MOVEL     '1'           $FINE
071700000000     C                   ENDSR
071800000000     C*------------------------------------------------------------------------*
071900000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
072000000000     C*------------------------------------------------------------------------*
072100000000     C     F24STD        BEGSR
072200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
072300000000     C                   EXSR      OUTFUN
072400000000     C                   ENDSR
072500000000     C*------------------------------------------------------------------------*
072600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
072700000000     C*          CODA MESSAGGI DEL PGM
072800000000     C*------------------------------------------------------------------------*
072900000000     C     CARMSG        BEGSR
073000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
073100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
073200000000     C     MSGULT        IFEQ      0
073300000000     C                   MOVEL     '1'           *IN40
073400000000     C                   WRITE     TB61DVK
073500000000     C                   MOVEL     '0'           *IN40
073600000000     C                   ENDIF
073700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
073800000000     C                   MOVEL     $MSG          DSMSMS
073900000000     C                   CALL      'XMSG'        XMSGPA
074000000000     C     DSMSER        IFEQ      0
074100000000     C                   MOVEL     DSMSKE        VMCKEY
074200000000     C                   ENDIF
074300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
074400000000     C                   ADD       1             MSGULT            4 0
074500000000     C                   WRITE     TB61DVM
074600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
074700000000     C                   MOVEL     *BLANKS       $MSG
074800000000     C                   ENDSR
074900000000     C*------------------------------------------------------------------------*
075000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
075100000000     C*------------------------------------------------------------------------*
075200000000     C     OUTMSG        BEGSR
075300000000     C* VISUALIZZAZIONE MESSAGGIO
075400000000     C                   MOVEL     '1'           *IN44
075500000000     C                   WRITE     TB61DVK
075600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
075700000000     C                   Z-ADD     *ZEROS        MSGULT
075800000000     C                   ENDSR
075900000000     C*------------------------------------------------------------------------*
076000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
076100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
076200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
076300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
076400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
076500000000     C*------------------------------------------------------------------------*
076600000000     C     CARFUN        BEGSR
076700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
076800000000     C     1             DO        31            I                 5 0
076900000000     C                   MOVEL     FUN(I)        FUNRIG
077000000000     C                   MOVEL     FUNFUN        FFU(I)
077100000000     C                   MOVEL     FUNDES        FDE(I)
077200020923     C     DUTLPO        IFEQ      'S'
077300000000     C     $TGES         IFEQ      'G'
077400000000     C                   MOVEL     FUNASG        FAT(I)
077500000000     C                   ELSE
077600000000     C                   MOVEL     FUNASI        FAT(I)
077700000000     C                   ENDIF
077800000000     C                   ELSE
077900000000     C     $TGES         IFEQ      'G'
078000000000     C                   MOVEL     FUNAFG        FAT(I)
078100000000     C                   ELSE
078200000000     C                   MOVEL     FUNAFI        FAT(I)
078300000000     C                   ENDIF
078400000000     C                   ENDIF
078500000000     C     FAT(I)        IFEQ      'E'
078600000000     C                   MOVEL     USER          WEDP              3
078700000000     C     WEDP          IFEQ      'EDP'
078800000000     C                   MOVEL     'S'           FAT(I)
078900000000     C                   ELSE
079000000000     C                   MOVEL     'N'           FAT(I)
079100000000     C                   ENDIF
079200000000     C                   ENDIF
079300000000     C                   ENDDO
079400000000     C                   ENDSR
079500000000     C*------------------------------------------------------------------------*
079600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
079700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
079800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
079900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
080000000000     C*------------------------------------------------------------------------*
080100000000     C     OUTFUN        BEGSR
080200000000     C     1             DO        8             I
080300000000     C                   MOVEL     *BLANKS       FUV(I)
080400000000     C                   ENDDO
080500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
080600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
080700000000     C     1             DO        31            I
080800000000     C     FAT(I)        IFEQ      'S'
080900000000     C     FDE(I)        ANDNE     *BLANKS
081000000000     C                   ADD       1             FUNNAT
081100000000     C                   ENDIF
081200000000     C                   ENDDO
081300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
081400000000     C     FUNNAT        IFGT      8
081500000000     C                   MOVEL     'S'           FAT(24)
081600000000     C                   ENDIF
081700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
081800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
081900000000     C                   Z-ADD     8             FUNVIS            5 0
082000000000     C     FAT(23)       IFEQ      'S'
082100000000     C                   SUB       1             FUNVIS
082200000000     C                   ENDIF
082300000000     C     FAT(24)       IFEQ      'S'
082400000000     C                   SUB       1             FUNVIS
082500000000     C                   ENDIF
082600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
082700000000     C                   Z-ADD     *ZEROS        W                 5 0
082800000000     C                   ADD       1             FUNULT
082900000000     C     FUNULT        IFGT      31
083000000000     C                   Z-ADD     1             FUNULT
083100000000     C                   ENDIF
083200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
083300000000     C     FUNULT        DO        31            I
083400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
083500000000     C     W             IFLT      FUNVIS
083600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
083700000000     C     FAT(I)        IFEQ      'S'
083800000000     C     FDE(I)        ANDNE     *BLANKS
083900000000     C     I             IFEQ      23
084000000000     C     I             OREQ      24
084100000000     C                   ELSE
084200000000     C                   ADD       1             W
084300000000     C                   MOVEL     FDE(I)        FUV(W)
084400000000     C                   ENDIF
084500000000     C                   ENDIF
084600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
084700000000     C                   Z-ADD     I             FUNULT
084800000000     C                   ENDIF
084900000000     C                   ENDDO
085000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
085100000000     C                   MOVEL     FUV(1)        VFD001
085200000000     C                   MOVEL     FUV(2)        VFD002
085300000000     C                   MOVEL     FUV(3)        VFD003
085400000000     C                   MOVEL     FUV(4)        VFD004
085500000000     C                   MOVEL     FUV(5)        VFD005
085600000000     C                   MOVEL     FUV(6)        VFD006
085700000000     C     FAT(23)       IFEQ      'N'
085800000000     C     FAT(24)       ANDEQ     'N'
085900000000     C                   MOVEL     FUV(7)        VFD007
086000000000     C                   MOVEL     FUV(8)        VFD008
086100000000     C                   ELSE
086200000000     C     FAT(23)       IFEQ      'S'
086300000000     C     FAT(24)       ANDEQ     'S'
086400000000     C                   MOVEL     FDE(23)       VFD007
086500000000     C                   MOVEL     FDE(24)       VFD008
086600000000     C                   ELSE
086700000000     C                   MOVEL     FUV(7)        VFD007
086800000000     C     FAT(23)       IFEQ      'S'
086900000000     C                   MOVEL     FDE(23)       VFD008
087000000000     C                   ELSE
087100000000     C                   MOVEL     FDE(24)       VFD008
087200000000     C                   ENDIF
087300000000     C                   ENDIF
087400000000     C                   ENDIF
087500000000     C                   MOVEL     '1'           $TESVD
087600000000     C                   ENDSR
087700000000     C*------------------------------------------------------------------------*
087800000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
087900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
088000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
088100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
088200000000     C*------------------------------------------------------------------------*
088300000000     C     CONFUN        BEGSR
088400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
088500000000     C     1             DO        31            I
088600000000     C     KEY           IFEQ      KEYS(I)
088700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
088800000000     C     FAT(I)        IFNE      'S'
088900000000     C                   MOVEL     MSG(1)        $MSG
089000000000     C                   EXSR      CARMSG
089100000000     C                   MOVEL     '1'           $ERR
089200000000     C                   ENDIF
089300000000     C                   ENDIF
089400000000     C                   ENDDO
089500000000     C                   ENDSR
089600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
089700000000TRTB61R- Tasto funzionale non valido.                                          1
089800000000TRTB61R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
089900000000TRTB61R- Opzione non valida.                                                   3
090000000000TRTB61R- Opzione abilitata ma non gestita. Avvertire CED.                      4
090100000000TRTB61R- Inizio elenco.                                                        5
090200000000TRTB61R- Fine elenco.                                                          6
090300000000TRTB61R- Immettere codice elemento non esistente                               7
090400000000TRTB61R- Immettere codice elemento                                             8
090500000000TRTB61R- Immettere la descrizione                                              9
090600000000TRTB61R- Immettere tipo tariffa esistente e non annullato                     10
090700000000TRTB61R- Immettere tipo filiale valido                                        11
090800000000TRTB61R- Non immettere tipo filiale se l'elemento non e' 'F'=Filiale          12
090900000000TRTB61R- 13
091000000000TRTB61R- Richiesta funzione per codice già esistente                          14
091100000000TRTB61R- Codice momentaneamente allocato: riprovare più tardi                 15
091200000000TRTB61R- 16
091300000000TRTB61R- Inserimento effettuato.                                              17
091400000000TRTB61R- Modifica effettuata.                                                 18
091500000000TRTB61R- Cancellazione effettuata.                                            19
091600000000TRTB61R- Ripristino effettuato.                                               20
091700000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
091800000000F1    -                   -    NN     NN
091900000000F2    -                   -    NN     NN
092000000000F3    -F3=Fine            -    SS     SS
092100000000F4    -                   -    NN     NN
092200000000F5    -                   -    NN     NN
092300000000F6    -F6=Aggiornamento   -    SS     SS
092400000000F7    -                   -    NN     NN
092500000000F8    -                   -    NN     NN
092600000000F9    -                   -    NN     NN
092700000000F10   -                   -    NN     NN
092800000000F11   -                   -    NN     NN
092900000000F12   -F12=Ritorno        -    SS     SS
093000000000F13   -                   -    NN     NN
093100000000F14   -                   -    NN     NN
093200000000F15   -                   -    NN     NN
093300000000F16   -                   -    NN     NN
093400000000F17   -                   -    NN     NN
093500000000F18   -                   -    NN     NN
093600000000F19   -                   -    NN     NN
093700000000F20   -                   -    NN     NN
093800000000F21   -                   -    NN     NN
093900000000F22   -                   -    NN     NN
094000000000F23   -                   -    NN     NN
094100000000F24   -F24=Altri tasti    -    NN     NN
094200000000ENTER -                   -    SS     SS
094300000000ROLLUP-                   -    NN     NN
094400000000ROLLDN-                   -    NN     NN
094500000000HELP  -                   -    NN     NN
094600000000PRINT -                   -    NN     NN
094700000000BSPACE-                   -    NN     NN
094800000000CLEAR -                   -    NN     NN
094900000000** DOPZ  (OPZIONE RICHIAMATA)
095000000000* INSERIMENTO *                                                 1
095100000000*  VARIAZIONE  *                                                2
095200000000*CANCELLAZIONE*                                                 3
095300000000*  RIPRISTINO  *                                                4
095400000000VISUALIZZAZIONE                                                 5

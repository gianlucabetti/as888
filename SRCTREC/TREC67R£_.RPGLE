000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC67R
000300000000      *  Descrizione   :  Gesione voci di maggiorazione di conto economico
000400000000      *                   Programma inserimento/modifica maggiorazione
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  18 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC67D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC67DVM:MSGULT)
001400000000     FECEVM01L  UF A E           K DISK
001500000000     FECEVT01L  IF   E           K DISK
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D MSG             S             78    DIM(25) CTDATA PERRCD(1)
002000000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002100000000     D*-------------------
002200000000     D* DECODIFICA OPZIONE RICHIAMATA
002300000000     D*-------------------
002400981015     D DOPZ            S             16    DIM(10) CTDATA PERRCD(1)
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
007200000000     D* DS ESTERNA DI PROCEDURA
007300000000     D*---------------------
007400000000     D DS60          E DS                  EXTNAME(TREC60DS)
007500000000     D*-------------------
007600000000     D* ARCHITETTURA
007700000000     D*-------------------
007800000000     D KPJBA         E DS
007900000000     D*------------------
008000000000     D* CALL XSRDA8 - CONTROLLO DATE
008100000000     D*------------------
008200000000     D WLBDAT          DS                  INZ
008300000000     D  G02DAT                 1      8  0
008400000000     D  G02INV                 9     16  0
008500000000     D  G02ERR                17     17
008600000000     D  G02TGI                18     22  0
008700000000     C*------------------------------------------------------------------------*
008800000000     C* MAIN LINE
008900000000     C*------------------------------------------------------------------------*
009000000000     C*
009100000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
009200000000IF  1C     $FINE         IFEQ      '1'
009300000000     C*
009400000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
009500000000     C                   MOVEL     '1'           D60ERR
009600000000     C*
009700000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
009800000000X   1C                   ELSE
009900000000     C*
010000000000     C* CICLO FINO A CHE:
010100000000DO  2C     $FINE         DOWEQ     '0'
010200000000     C                   MOVEL     '0'           $ERR
010300000000     C*
010400000000     C* CONTROLLA GESTIONE VIDEO
010500000000IF  3C     $GES          IFEQ      'V1'
010600000000     C                   EXSR      GESV1
010700000000E   3C                   ENDIF
010800000000     C*
010900000000E   2C                   ENDDO
011000000000E   1C                   ENDIF
011100000000     C*
011200000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
011300000000     C     $F03          IFEQ      '1'
011400000000     C                   MOVEL     '1'           D60F03
011500000000E   1C                   ENDIF
011600000000     C* FINE PGM
011700000000     C                   MOVEL     DS60          KPJBU
011800000000     C                   SETON                                        LR
011900000000     C*------------------------------------------------------------------------*
012000000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
012100000000     C*------------------------------------------------------------------------*
012200000000     C     CARV1         BEGSR
012300000000     C*
012400000000     C                   MOVEL     'V1'          $GES
012500000000     C*
012600000000     C                   EXSR      CARFUN
012700000000     C                   EXSR      OUTFUN
012800000000     C*
012900000000     C                   WRITE     EC67DVT
013000000000     C                   WRITE     EC67DVF
013100000000     C*
013200000000     C                   ENDSR
013300000000     C*------------------------------------------------------------------------*
013400000000     C* GESV1  - GESTIONE PRIMO VIDEO
013500000000     C*------------------------------------------------------------------------*
013600000000     C     GESV1         BEGSR
013700000000     C*
013800000000IF  1C     $TESVD        IFEQ      '1'
013900000000     C                   EXSR      CARV1
014000000000E   1C                   ENDIF
014100000000     C*
014200000000IF  1C     MSGULT        IFNE      *ZEROS
014300000000     C                   EXSR      OUTMSG
014400000000E   1C                   ENDIF
014500000000     C*
014600000000     C                   EXFMT     EC67DV1
014700000000     C*
014800000000     C                   EXSR      CONFUN
014900000000     C*
015000000000IF  1C     $ERR          IFEQ      '0'
015100000000SL  2C                   SELECT
015200000000     C     KEY           WHENEQ    ENTER
015300000000     C                   EXSR      ENTV1
015400000000     C     KEY           WHENEQ    F6
015500000000     C                   EXSR      F06GES
015600000000     C                   OTHER
015700000000     C                   EXSR      FUNSTD
015800000000E   2C                   ENDSL
015900000000E   1C                   ENDIF
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
016400000000     C*------------------------------------------------------------------------*
016500000000     C     ENTV1         BEGSR
016600000000     C*
016700000000     C* CONTROLLA I CAMPI IMMESSI
016800000000     C                   EXSR      DECODI
016900000000     C*
017000000000     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
017100000000IF  1C     D60OP1        IFEQ      'O05'
017200000000     C                   MOVEL     '1'           $FINE
017300000000E   1C                   ENDIF
017400000000     C*
017500000000     C                   ENDSR
017600000000     C*------------------------------------------------------------------------*
017700000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
017800000000     C*------------------------------------------------------------------------*
017900000000     C     F06GES        BEGSR
018000000000     C*
018100000000     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
018200000000IF  1C     D60OP1        IFEQ      'F06'
018300000000     C                   EXSR      INSERI
018400000000E   1C                   ENDIF
018500000000IF  1C     D60OP1        IFEQ      'O02'
018600000000     C                   EXSR      MODIFI
018700000000E   1C                   ENDIF
018800000000IF  1C     D60OP1        IFEQ      'O04'
018900000000     C                   EXSR      ANNULL
019000000000E   1C                   ENDIF
019100000000IF  1C     D60OP1        IFEQ      'O07'
019200000000     C                   EXSR      RIPRIS
019300000000E   1C                   ENDIF
019400000000     C*
019500000000     C                   ENDSR
019600000000     C*------------------------------------------------------------------------*
019700000000     C* INSERI - FUNZIONE DI INSERIMENTO
019800000000     C*------------------------------------------------------------------------*
019900000000     C     INSERI        BEGSR
020000000000     C*
020100000000     C* CONTROLLA I CAMPI IMMESSI
020200000000     C                   EXSR      DECODI
020300000000     C*
020400000000IF  1C     $ERR          IFEQ      '0'
020500000000     C*
020600000000     C                   Z-ADD     V1CVOM        EVMVOM
020700000000     C                   Z-ADD     V1CPRM        EVMPRM
020800000000     C                   Z-ADD     V1CVOC        EVMVOC
020900000000     C                   Z-ADD     V1CPRG        EVMPRG
021000000000     C                   MOVEL     *BLANKS       EVMATB
021100000000     C                   MOVEL     *BLANKS       EVMFTR
021200000000     C                   Z-ADD     *ZEROS        EVMDTR
021300000000     C*
021400000000     C                   WRITE     ECEVM
021500000000     C*
021600000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
021700000000     C                   MOVEL     '1'           $FINE
021800000000     C*
021900000000     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
022000000000     C                   MOVEL     MSG(17)       D60MSG
022100000000     C*
022200000000E   1C                   ENDIF
022300000000     C*
022400000000     C                   ENDSR
022500000000     C*------------------------------------------------------------------------*
022600000000     C* MODIFI - FUNZIONE DI MODIFICA
022700000000     C*------------------------------------------------------------------------*
022800000000     C     MODIFI        BEGSR
022900000000     C*
023000000000     C* CONTROLLA I CAMPI IMMESSI
023100000000     C                   EXSR      DECODI
023200000000     C*
023300000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
023400000000     C                   Z-ADD     D60VOM        KVDVOM
023500000000     C                   Z-ADD     D60PRM        KVDPRM
023600000000     C                   Z-ADD     D60VOC        KVDVOC
023700000000     C                   Z-ADD     D60PRG        KVDPRG
023800000000     C     KEYEVM        CHAIN     ECEVM01L                           99
023900000000IF  1C     *IN99         IFEQ      *OFF
024000000000     C*
024100000000IF  2C     $ERR          IFEQ      '0'
024200000000     C*
024300000000     C                   Z-ADD     V1CVOM        EVMVOM
024400000000     C                   Z-ADD     V1CPRM        EVMPRM
024500000000     C                   Z-ADD     V1CVOC        EVMVOC
024600000000     C                   Z-ADD     V1CPRG        EVMPRG
024700000000     C                   MOVEL     *BLANKS       EVMATB
024800000000     C                   MOVEL     *BLANKS       EVMFTR
024900970903     C*---                 Z-ADD*ZEROS    EVMDTR           *DATA TRASMISSIONE
025000000000     C*
025100000000     C                   UPDATE    ECEVM
025200000000     C*
025300000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
025400000000     C                   MOVEL     '1'           $FINE
025500000000     C*
025600000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
025700000000     C                   MOVEL     MSG(18)       D60MSG
025800000000     C*
025900000000E   2C                   ENDIF
026000000000E   1C                   ENDIF
026100000000     C*
026200000000     C                   ENDSR
026300000000     C*------------------------------------------------------------------------*
026400000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
026500000000     C*------------------------------------------------------------------------*
026600000000     C     ANNULL        BEGSR
026700000000     C*
026800000000     C* CONTROLLA I CAMPI IMMESSI
026900000000     C                   EXSR      DECODI
027000000000     C*
027100000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
027200000000     C                   Z-ADD     D60VOM        KVDVOM
027300000000     C                   Z-ADD     D60PRM        KVDPRM
027400000000     C                   Z-ADD     D60VOC        KVDVOC
027500000000     C                   Z-ADD     D60PRG        KVDPRG
027600000000     C     KEYEVM        CHAIN     ECEVM01L                           99
027700000000IF  1C     *IN99         IFEQ      *OFF
027800000000     C*
027900091112IF  2C*    $ERR          IFEQ      '0'
028000000000     C*--------------------
028100000000     C* ANNULLO VOCE
028200000000     C*--------------------
028300970903     C* SE NON E' STATO MAI TRASMESSO, CANCELLA FISICAMENTE IL RECORD
028400970903IF  3C     EVMDTR        IFGT      *ZEROS
028500000000     C                   MOVEL     'A'           EVMATB
028600000000     C                   MOVEL     *BLANKS       EVMFTR
028700970903     C*---                 Z-ADD*ZEROS    EVMDTR           *DATA TRASMISSIONE
028800000000     C                   UPDATE    ECEVM
028900970903X   3C                   ELSE
029000970903     C                   DELETE    ECEVM
029100970903E   3C                   ENDIF
029200000000     C*
029300000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
029400000000     C                   MOVEL     '1'           $FINE
029500000000     C*
029600000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
029700000000     C                   MOVEL     MSG(19)       D60MSG
029800000000     C*
029900000000E   2C                   ENDIF
030000091112E   1C*                  ENDIF
030100000000     C*
030200000000     C                   ENDSR
030300000000     C*------------------------------------------------------------------------*
030400000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
030500000000     C*------------------------------------------------------------------------*
030600000000     C     RIPRIS        BEGSR
030700000000     C*
030800000000     C* CONTROLLA I CAMPI IMMESSI
030900000000     C                   EXSR      DECODI
031000000000     C*
031100000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
031200000000     C                   Z-ADD     D60VOM        KVDVOM
031300000000     C                   Z-ADD     D60PRM        KVDPRM
031400000000     C                   Z-ADD     D60VOC        KVDVOC
031500000000     C                   Z-ADD     D60PRG        KVDPRG
031600000000     C     KEYEVM        CHAIN     ECEVM01L                           99
031700000000IF  1C     *IN99         IFEQ      *OFF
031800000000     C*
031900000000IF  2C     $ERR          IFEQ      '0'
032000000000     C*--------------------
032100000000     C* RIPRISTINO VOCE
032200000000     C*--------------------
032300000000     C                   MOVEL     *BLANKS       EVMATB
032400000000     C                   MOVEL     *BLANKS       EVMFTR
032500970903     C*---                 Z-ADD*ZEROS    EVMDTR           *DATA TRASMISSIONE
032600000000     C*
032700000000     C                   UPDATE    ECEVM
032800000000     C*
032900000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
033000000000     C                   MOVEL     '1'           $FINE
033100000000     C*
033200000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
033300000000     C                   MOVEL     MSG(20)       D60MSG
033400000000     C*
033500000000E   2C                   ENDIF
033600000000E   1C                   ENDIF
033700000000     C*
033800000000     C                   ENDSR
033900000000     C*------------------------------------------------------------------------*
034000000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
034100000000     C*------------------------------------------------------------------------*
034200000000     C     DECODI        BEGSR
034300000000     C*
034400000000     C* REIMPOSTO INDICATORI DI ERRORE
034500000000     C                   SETOFF                                       6061
034600000000     C*--------------------
034700000000     C* CODICE VOCE MAGGIORAZIONE
034800000000     C*--------------------
034900000000     C                   MOVE      *BLANKS       V1CVDM
035000000000     C                   Z-ADD     *ZERO         V1CMDV
035100000000     C                   Z-ADD     *ZERO         V1CMSV
035200000000     C                   Z-ADD     V1CVOM        KVTVOC
035300000000     C                   Z-ADD     V1CPRM        KVTPRG
035400000000     C     KEYEVT        CHAIN     ECEVT01L                           60
035500000000IF  1C     *IN60         IFEQ      *ON
035600000000     C     EVTTVO        ORNE      'M'
035700000000     C                   MOVEL     '1'           $ERR
035800000000     C                   MOVEL     MSG(7)        $MSG
035900000000     C                   EXSR      CARMSG
036000000000     C                   SETON                                        60
036100000000X   1C                   ELSE
036200000000     C                   MOVEL     EVTVOD        V1CVDM
036300000000     C                   Z-ADD     EVTDDV        G02INV
036400000000     C                   Z-ADD     *ZEROS        G02DAT
036500000000     C                   MOVEL     '3'           G02ERR
036600000000     C                   CALL      'XSRDA8'
036700000000     C                   PARM                    WLBDAT
036800000000     C                   Z-ADD     G02DAT        V1CMDV
036900000000     C                   Z-ADD     EVTDSV        G02INV
037000000000     C                   Z-ADD     *ZEROS        G02DAT
037100000000     C                   MOVEL     '3'           G02ERR
037200000000     C                   CALL      'XSRDA8'
037300000000     C                   PARM                    WLBDAT
037400000000     C                   Z-ADD     G02DAT        V1CMSV
037500000000E   1C                   ENDIF
037600000000     C*--------------------
037700000000     C* CODICE VOCE DI CONTO ECONOMICO DA APPLICARE
037800000000     C* IL PROGRESSIVO POTREBBE ANCHE ESSERE ZERO, SIGNIFICHEREBBE "TUTTI"
037900000000     C*--------------------
038000000000     C                   MOVE      *BLANKS       V1CVDC
038100000000     C                   MOVE      *BLANKS       V1CTVO
038200000000     C                   Z-ADD     *ZERO         V1CCDV
038300000000     C                   Z-ADD     *ZERO         V1CCSV
038400000000     C                   Z-ADD     V1CVOC        KVTVOC
038500000000     C                   Z-ADD     V1CPRG        KVTPRG
038600000000IF  1C     V1CPRG        IFNE      *ZERO
038700000000     C*
038800000000     C     KEYEVT        CHAIN     ECEVT01L                           61
038900000000IF  2C     *IN61         IFEQ      *ON
038901091112IF  2C     evttvo        orne      'V'
039000000000     C                   MOVEL     '1'           $ERR
039100000000     C                   MOVEL     MSG(8)        $MSG
039200000000     C                   EXSR      CARMSG
039300000000X   2C                   ELSE
039400000000     C                   MOVEL     EVTVOD        V1CVDC
039500000000     C                   MOVEL     EVTTVO        V1CTVO
039600000000     C                   Z-ADD     EVTDDV        G02INV
039700000000     C                   Z-ADD     *ZEROS        G02DAT
039800000000     C                   MOVEL     '3'           G02ERR
039900000000     C                   CALL      'XSRDA8'
040000000000     C                   PARM                    WLBDAT
040100000000     C                   Z-ADD     G02DAT        V1CCDV
040200000000     C                   Z-ADD     EVTDSV        G02INV
040300000000     C                   Z-ADD     *ZEROS        G02DAT
040400000000     C                   MOVEL     '3'           G02ERR
040500000000     C                   CALL      'XSRDA8'
040600000000     C                   PARM                    WLBDAT
040700000000     C                   Z-ADD     G02DAT        V1CCSV
040800000000E   2C                   ENDIF
040900000000     C*
041000000000X   1C                   ELSE
041100000000     C*
041200000000     C* NEL CASO V1CPRG=0 PRENDO LA DECODIFICA DELLA PRIMA VOCE
041300000000     C     KEYEVT        SETLL     ECEVT01L
041400000000     C     V1CVOC        READE     ECEVT01L                               61
041500000000IF  2C     *IN61         IFEQ      *ON
041600000000     C                   MOVEL     '1'           $ERR
041700000000     C                   MOVEL     MSG(8)        $MSG
041800000000     C                   EXSR      CARMSG
041900000000X   2C                   ELSE
042000000000     C     'Tutti i'     CAT       'progre':1    V1CVDC
042100000000     C     V1CVDC        CAT       'ssivi':0     V1CVDC
042200000000     C                   MOVEL     EVTTVO        V1CTVO
042300000000E   2C                   ENDIF
042400000000     C*
042500000000E   1C                   ENDIF
042600000000     C*
042700000000     C* NON POSSO INSERIRE DOPPIONI ALL'INTERNO DELL'ARCHIVIO
042800000000IF  1C     $ERR          IFEQ      '0'
042900000000     C     D60OP1        ANDEQ     'F06'
043000000000     C                   Z-ADD     V1CVOM        KVDVOM
043100000000     C                   Z-ADD     V1CPRM        KVDPRM
043200000000     C                   Z-ADD     V1CVOC        KVDVOC
043300000000     C                   Z-ADD     V1CPRG        KVDPRG
043400000000     C     KEYEVM        SETLL     ECEVM01L                               99
043500000000IF  2C     *IN99         IFEQ      *ON
043600000000     C                   MOVEL     '1'           $ERR
043700000000     C                   MOVEL     MSG(9)        $MSG
043800000000     C                   EXSR      CARMSG
043900000000     C                   SETON                                        6061
044000000000E   2C                   ENDIF
044100000000E   1C                   ENDIF
044200000000     C*
044300000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
044400000000     C* CURSORE SUL CAMPO DEL CONTROL
044500000000IF  1C     $ERR          IFEQ      '1'
044600000000     C                   MOVEL     '0'           *IN43
044700000000X   1C                   ELSE
044800000000     C                   MOVEL     '1'           *IN43
044900000000E   1C                   ENDIF
045000000000     C*
045100000000     C                   ENDSR
045200000000     C*------------------------------------------------------------------------*
045300000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
045400000000     C*------------------------------------------------------------------------*
045500000000     C     RECVID        BEGSR
045600000000     C*
045700000000     C                   Z-ADD     D60VOM        V1CVOM
045800000000     C                   Z-ADD     D60PRM        V1CPRM
045900000000     C                   Z-ADD     D60VOC        V1CVOC
046000000000     C                   Z-ADD     D60PRG        V1CPRG
046100981015IF  1C     EVMATB        IFNE      *BLANKS
046200981015     C                   MOVEL     DOPZ(6)       V1CATB
046300981015E   1C                   ENDIF
046400000000     C*
046500000000     C                   ENDSR
046600000000     C*------------------------------------------------------------------------*
046700000000     C*  AZZVID - PULISCE TUTTI ICAMPI DEL VIDEO
046800000000     C*------------------------------------------------------------------------*
046900000000     C     AZZVID        BEGSR
047000000000     C*
047100000000     C                   MOVE      *ZERO         V1CVOM
047200000000     C                   Z-ADD     *ZERO         V1CPRM
047300000000     C                   MOVE      *ZERO         V1CVOC
047400000000     C                   Z-ADD     *ZERO         V1CPRG
047500981015     C                   MOVEL     *BLANKS       V1CATB
047600000000     C*
047700000000     C                   ENDSR
047800020923     C*-------------------------------------------------------------------------
047900020923     C* REPERISCE I DATI UTENTE
048000020923     C*-------------------------------------------------------------------------
048100020923     C     REPDATIUTE    BEGSR
048200020923     C*
048300020923     C* INIZIALIZZA VARIABILI DI WRK
048400020923     C                   CLEAR                   TIBS34DS
048500020923     C                   CLEAR                   AZUTEDS
048600020923     C                   CLEAR                   DDATIUTE
048700020923     C*
048800020923     C     *DTAARA       DEFINE    §azute        azuteds
048900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
049000020923     C                   IN(E)     *DTAARA
049100020923if  1C                   IF        %Error
049200020923     c                   EVAL      I34Tla = 'L'
049300020923     C                   CALL      'TIBS34R'
049400020923     C                   PARM                    Tibs34Ds
049500020923     C                   IN        *DTAARA
049600020923e   1C                   ENDIF
049700020923     C*
049800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
049900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
050000020923     C*
050100020923     C                   ENDSR
050200000000     C*------------------------------------------------------------------------*
050300000000     C*  *INZSR - ROUTINE INIZIALE
050400000000     C*------------------------------------------------------------------------*
050500000000     C     *INZSR        BEGSR
050600000000     C*
050700000000     C* RICEVIMENTO PARAMETRI
050800000000     C     *ENTRY        PLIST
050900000000     C                   PARM                    KPJBA
051000000000     C                   MOVEL     KPJBU         DS60
051100000000     C*
051200000000     C* DEFINIZIONE VARIABILI
051300000000     C     *LIKE         DEFINE    EVMVOM        KVDVOM
051400000000     C     *LIKE         DEFINE    EVMPRM        KVDPRM
051500000000     C     *LIKE         DEFINE    EVMVOC        KVDVOC
051600000000     C     *LIKE         DEFINE    EVMPRG        KVDPRG
051700000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
051800000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
051900000000     C*
052000000000     C* CHIAVE LETTURA ECEVM01L - COMPLETA
052100000000     C     KEYEVM        KLIST
052200000000     C                   KFLD                    KVDVOM
052300000000     C                   KFLD                    KVDPRM
052400000000     C                   KFLD                    KVDVOC
052500000000     C                   KFLD                    KVDPRG
052600000000     C*
052700000000     C* CHIAVE LETTURA ECEVT01L - PARZIALE
052800000000     C     KEYEVT        KLIST
052900000000     C                   KFLD                    KVTVOC
053000000000     C                   KFLD                    KVTPRG
053100000000     C*
053200000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
053300000000     C                   MOVE      D60OP0        INTGES            2
053400000000IF  1C     INTGES        IFEQ      '02'
053500000000     C                   MOVEL     'G'           $TGES             1
053600000000X   1C                   ELSE
053700000000     C                   MOVEL     'I'           $TGES             1
053800000000E   1C                   ENDIF
053900000000     C*
054000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
054100000000     C                   EXSR      INZSTD
054200000000     C*
054300000000     C* RAGIONE SOCIALE UTENTE
054400000000     C                   MOVEL     DSFIRS        VTDRSU
054500000000     C*
054600000000     C* DECODIFICA OPZIONE RICHIAMATA
054700000000IF  1C     D60OP1        IFEQ      'F06'
054800000000     C                   MOVEL     DOPZ(1)       VTDFUN
054900000000E   1C                   ENDIF
055000000000IF  2C     D60OP1        IFEQ      'O02'
055100000000     C                   MOVEL     DOPZ(2)       VTDFUN
055200000000E   1C                   ENDIF
055300000000IF  1C     D60OP1        IFEQ      'O04'
055400000000     C                   MOVEL     DOPZ(3)       VTDFUN
055500000000E   1C                   ENDIF
055600000000IF  1C     D60OP1        IFEQ      'O07'
055700000000     C                   MOVEL     DOPZ(4)       VTDFUN
055800000000E   1C                   ENDIF
055900000000IF  1C     D60OP1        IFEQ      'O05'
056000000000     C                   MOVEL     DOPZ(5)       VTDFUN
056100000000E   1C                   ENDIF
056200000000     C*
056300000000     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
056400000000     C* 0=NON PROTETTO, 1=PROTETTO
056500000000     C*   90 - CAMPI CHIAVE
056600000000     C*   91 - ALTRI CAMPI
056700000000IF  1C     D60OP1        IFEQ      'F06'
056800000000     C                   SETOFF                                       9091
056900000000E   1C                   ENDIF
057000000000IF  1C     D60OP1        IFEQ      'O02'
057100000000     C                   SETON                                        90
057200000000E   1C                   ENDIF
057300000000IF  1C     D60OP1        IFEQ      'O04'
057400000000     C     D60OP1        OREQ      'O07'
057500000000     C     D60OP1        OREQ      'O05'
057600000000     C                   SETON                                        9091
057700000000E   1C                   ENDIF
057800000000     C*
057900000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
058000000000     C* E METTO F4=DECODIFICA
058100000000IF  1C     D60OP1        IFEQ      'O05'
058200000000     C                   MOVEL     FUN(6)        FUNRIG
058300000000     C                   MOVEL     'N'           FUNASG
058400000000     C                   MOVEL     'N'           FUNASI
058500000000     C                   MOVEL     'N'           FUNAFG
058600000000     C                   MOVEL     'N'           FUNAFG
058700000000     C                   MOVEL     FUNRIG        FUN(6)
058800000000E   1C                   ENDIF
058900000000     C*
059000000000     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
059100000000IF  1C     D60OP1        IFEQ      'O02'
059200000000     C     D60OP1        OREQ      'O04'
059300000000     C     D60OP1        OREQ      'O05'
059400000000     C     D60OP1        OREQ      'O07'
059500000000     C                   Z-ADD     D60VOM        KVDVOM
059600000000     C                   Z-ADD     D60PRM        KVDPRM
059700000000     C                   Z-ADD     D60VOC        KVDVOC
059800000000     C                   Z-ADD     D60PRG        KVDPRG
059900000000     C*
060000000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
060100000000     C     KEYEVM        CHAIN     ECEVM01L                           9998
060200000000IF  2C     *IN99         IFEQ      '1'
060300000000     C                   MOVEL     '1'           $ERR
060400000000     C                   MOVEL     '1'           $FINE
060500000000     C                   MOVEL     MSG(21)       D60MSG
060600000000X   2C                   ELSE
060700000000IF  3C     *IN98         IFEQ      '1'
060800000000     C                   MOVEL     '1'           $ERR
060900000000     C                   MOVEL     '1'           $FINE
061000000000     C                   MOVEL     MSG(22)       D60MSG
061100000000X   3C                   ELSE
061200000000     C*
061300000000     C                   EXSR      RECVID
061400000000     C                   EXSR      CARV1
061500000000E   3C                   ENDIF
061600000000E   2C                   ENDIF
061700000000E   1C                   ENDIF
061800000000     C*
061900000000     C* SE INSERIMENTO
062000000000IF  1C     D60OP1        IFEQ      'F06'
062100000000     C                   EXSR      AZZVID
062200000000     C                   EXSR      CARV1
062300000000     C                   ELSE
062400000000     C                   EXSR      DECODI
062500000000E   1C                   ENDIF
062600000000     C*
062700000000     C                   ENDSR
062800000000     C*------------------------------------------------------------------------*
062900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063100000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
063200000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
063300000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
063400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063600000000     C*------------------------------------------------------------------------*
063700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
063800000000     C*------------------------------------------------------------------------*
063900000000     C     INZSTD        BEGSR
064000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
064100000000     C                   CALL      'XKEYS'
064200000000     C                   PARM                    DSKEYS
064300000000     C                   MOVEA     KEYRIG        KEYS
064400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
064500000000     C     XMSGPA        PLIST
064600000000     C                   PARM                    DSMSMS
064700000000     C                   PARM                    DSMSPG
064800000000     C                   PARM                    DSMSKE
064900000000     C                   PARM                    DSMSER
065000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
065100000000     C                   MOVEL     'TREC67R'     DSMSPG
065200000000     C                   MOVEL     'TREC67R'     VMCQUE
065300000000     C                   MOVEL     *BLANKS       VMCKEY
065400000000     C                   MOVEL     *BLANKS       DSMSMS
065500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
065600000000     C                   MOVEL     '0'           $ERR              1
065700000000     C                   MOVEL     '0'           $F03              1
065800000000     C                   MOVEL     *BLANKS       $GES              2
065900000000     C                   MOVEL     *BLANKS       $MSG             80
066000000000     C                   MOVEL     '0'           $FINE             1
066100000000     C                   MOVEL     '0'           $TESVD            1
066200000000     C* LANCIO ROUTINE PER VERIFICARE SE
066300000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
066400000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
066500020923     C                   EXSR      REPDATIUTE
066600000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
066700000000     C                   BITOFF    '01234567'    POSMI1            1
066800000000     C                   BITOFF    '01234567'    POSMI2            1
066900000000     C                   BITOFF    '01234567'    POSMA1            1
067000000000     C                   BITOFF    '01234567'    POSMA2            1
067100000000     C                   BITON     '34'          POSMI1            1
067200000000     C                   BITON     '7'           POSMI2            1
067300000000     C                   BITON     '34'          POSMA1            1
067400000000     C                   BITON     '0'           POSMA2            1
067500000000     C                   MOVE      POSMI2        POSMIN            2
067600000000     C                   MOVEL     POSMI1        POSMIN
067700000000     C                   MOVE      POSMA2        POSMAX            2
067800000000     C                   MOVEL     POSMA1        POSMAX
067900000000     C                   ENDSR
068000000000     C*------------------------------------------------------------------------*
068100000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
068200000000     C*------------------------------------------------------------------------*
068300000000     C     FUNSTD        BEGSR
068400000000     C                   SELECT
068500000000     C     KEY           WHENEQ    F3
068600000000     C                   EXSR      F03STD
068700000000     C     KEY           WHENEQ    F12
068800000000     C                   EXSR      F12STD
068900000000     C     KEY           WHENEQ    F24
069000000000     C                   EXSR      F24STD
069100000000     C                   OTHER
069200000000     C                   MOVEL     MSG(2)        $MSG
069300000000     C                   EXSR      CARMSG
069400000000     C                   ENDSL
069500000000     C                   ENDSR
069600000000     C*------------------------------------------------------------------------*
069700000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
069800000000     C*------------------------------------------------------------------------*
069900000000     C     F03STD        BEGSR
070000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
070100000000     C                   MOVEL     '1'           $F03
070200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
070300000000     C                   MOVEL     '1'           $FINE
070400000000     C                   ENDSR
070500000000     C*------------------------------------------------------------------------*
070600000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
070700000000     C*------------------------------------------------------------------------*
070800000000     C     F12STD        BEGSR
070900000000     C* IMPOSTO FLAG DI FIN PGM
071000000000     C                   MOVEL     '1'           $FINE
071100000000     C                   ENDSR
071200000000     C*------------------------------------------------------------------------*
071300000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
071400000000     C*------------------------------------------------------------------------*
071500000000     C     F24STD        BEGSR
071600000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
071700000000     C                   EXSR      OUTFUN
071800000000     C                   ENDSR
071900000000     C*------------------------------------------------------------------------*
072000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
072100000000     C*          CODA MESSAGGI DEL PGM
072200000000     C*------------------------------------------------------------------------*
072300000000     C     CARMSG        BEGSR
072400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
072500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
072600000000     C     MSGULT        IFEQ      0
072700000000     C                   MOVEL     '1'           *IN40
072800000000     C                   WRITE     EC67DVK
072900000000     C                   MOVEL     '0'           *IN40
073000000000     C                   ENDIF
073100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
073200000000     C                   MOVEL     $MSG          DSMSMS
073300000000     C                   CALL      'XMSG'        XMSGPA
073400000000     C     DSMSER        IFEQ      0
073500000000     C                   MOVEL     DSMSKE        VMCKEY
073600000000     C                   ENDIF
073700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
073800000000     C                   ADD       1             MSGULT            4 0
073900000000     C                   WRITE     EC67DVM
074000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
074100000000     C                   MOVEL     *BLANKS       $MSG
074200000000     C                   ENDSR
074300000000     C*------------------------------------------------------------------------*
074400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
074500000000     C*------------------------------------------------------------------------*
074600000000     C     OUTMSG        BEGSR
074700000000     C* VISUALIZZAZIONE MESSAGGIO
074800000000     C                   MOVEL     '1'           *IN44
074900000000     C                   WRITE     EC67DVK
075000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
075100000000     C                   Z-ADD     *ZEROS        MSGULT
075200000000     C                   ENDSR
075300000000     C*------------------------------------------------------------------------*
075400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
075500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
075600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
075700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
075800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
075900000000     C*------------------------------------------------------------------------*
076000000000     C     CARFUN        BEGSR
076100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
076200000000     C     1             DO        31            I                 5 0
076300000000     C                   MOVEL     FUN(I)        FUNRIG
076400000000     C                   MOVEL     FUNFUN        FFU(I)
076500000000     C                   MOVEL     FUNDES        FDE(I)
076600020923     C     DUTLPO        IFEQ      'S'
076700000000     C     $TGES         IFEQ      'G'
076800000000     C                   MOVEL     FUNASG        FAT(I)
076900000000     C                   ELSE
077000000000     C                   MOVEL     FUNASI        FAT(I)
077100000000     C                   ENDIF
077200000000     C                   ELSE
077300000000     C     $TGES         IFEQ      'G'
077400000000     C                   MOVEL     FUNAFG        FAT(I)
077500000000     C                   ELSE
077600000000     C                   MOVEL     FUNAFI        FAT(I)
077700000000     C                   ENDIF
077800000000     C                   ENDIF
077900000000     C     FAT(I)        IFEQ      'E'
078000000000     C                   MOVEL     USER          WEDP              3
078100000000     C     WEDP          IFEQ      'EDP'
078200000000     C                   MOVEL     'S'           FAT(I)
078300000000     C                   ELSE
078400000000     C                   MOVEL     'N'           FAT(I)
078500000000     C                   ENDIF
078600000000     C                   ENDIF
078700000000     C                   ENDDO
078800000000     C                   ENDSR
078900000000     C*------------------------------------------------------------------------*
079000000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
079100000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
079200000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
079300000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
079400000000     C*------------------------------------------------------------------------*
079500000000     C     OUTFUN        BEGSR
079600000000     C     1             DO        8             I
079700000000     C                   MOVEL     *BLANKS       FUV(I)
079800000000     C                   ENDDO
079900000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
080000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
080100000000     C     1             DO        31            I
080200000000     C     FAT(I)        IFEQ      'S'
080300000000     C     FDE(I)        ANDNE     *BLANKS
080400000000     C                   ADD       1             FUNNAT
080500000000     C                   ENDIF
080600000000     C                   ENDDO
080700000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
080800000000     C     FUNNAT        IFGT      8
080900000000     C                   MOVEL     'S'           FAT(24)
081000000000     C                   ENDIF
081100000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
081200000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
081300000000     C                   Z-ADD     8             FUNVIS            5 0
081400000000     C     FAT(23)       IFEQ      'S'
081500000000     C                   SUB       1             FUNVIS
081600000000     C                   ENDIF
081700000000     C     FAT(24)       IFEQ      'S'
081800000000     C                   SUB       1             FUNVIS
081900000000     C                   ENDIF
082000000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
082100000000     C                   Z-ADD     *ZEROS        W                 5 0
082200000000     C                   ADD       1             FUNULT
082300000000     C     FUNULT        IFGT      31
082400000000     C                   Z-ADD     1             FUNULT
082500000000     C                   ENDIF
082600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
082700000000     C     FUNULT        DO        31            I
082800000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
082900000000     C     W             IFLT      FUNVIS
083000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
083100000000     C     FAT(I)        IFEQ      'S'
083200000000     C     FDE(I)        ANDNE     *BLANKS
083300000000     C     I             IFEQ      23
083400000000     C     I             OREQ      24
083500000000     C                   ELSE
083600000000     C                   ADD       1             W
083700000000     C                   MOVEL     FDE(I)        FUV(W)
083800000000     C                   ENDIF
083900000000     C                   ENDIF
084000000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
084100000000     C                   Z-ADD     I             FUNULT
084200000000     C                   ENDIF
084300000000     C                   ENDDO
084400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
084500000000     C                   MOVEL     FUV(1)        VFD001
084600000000     C                   MOVEL     FUV(2)        VFD002
084700000000     C                   MOVEL     FUV(3)        VFD003
084800000000     C                   MOVEL     FUV(4)        VFD004
084900000000     C                   MOVEL     FUV(5)        VFD005
085000000000     C                   MOVEL     FUV(6)        VFD006
085100000000     C     FAT(23)       IFEQ      'N'
085200000000     C     FAT(24)       ANDEQ     'N'
085300000000     C                   MOVEL     FUV(7)        VFD007
085400000000     C                   MOVEL     FUV(8)        VFD008
085500000000     C                   ELSE
085600000000     C     FAT(23)       IFEQ      'S'
085700000000     C     FAT(24)       ANDEQ     'S'
085800000000     C                   MOVEL     FDE(23)       VFD007
085900000000     C                   MOVEL     FDE(24)       VFD008
086000000000     C                   ELSE
086100000000     C                   MOVEL     FUV(7)        VFD007
086200000000     C     FAT(23)       IFEQ      'S'
086300000000     C                   MOVEL     FDE(23)       VFD008
086400000000     C                   ELSE
086500000000     C                   MOVEL     FDE(24)       VFD008
086600000000     C                   ENDIF
086700000000     C                   ENDIF
086800000000     C                   ENDIF
086900000000     C                   MOVEL     '1'           $TESVD
087000000000     C                   ENDSR
087100000000     C*------------------------------------------------------------------------*
087200000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
087300000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
087400000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
087500000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
087600000000     C*------------------------------------------------------------------------*
087700000000     C     CONFUN        BEGSR
087800000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
087900000000     C     1             DO        31            I
088000000000     C     KEY           IFEQ      KEYS(I)
088100000000     C* CONTROLLO SE IL TASTO E' ATTIVO
088200000000     C     FAT(I)        IFNE      'S'
088300000000     C                   MOVEL     MSG(1)        $MSG
088400000000     C                   EXSR      CARMSG
088500000000     C                   MOVEL     '1'           $ERR
088600000000     C                   ENDIF
088700000000     C                   ENDIF
088800000000     C                   ENDDO
088900000000     C                   ENDSR
089000981015** ======== SCHIERA: MSG  (MESSAGGI)                       ================
089100000000TREC67R- Tasto funzionale non valido.                                          1
089200000000TREC67R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
089300000000TREC67R- Opzione non valida.                                                   3
089400000000TREC67R- Opzione abilitata ma non gestita. Avvertire CED.                      4
089500000000TREC67R- Inizio elenco.                                                        5
089600000000TREC67R- Fine elenco.                                                          6
089700000000TREC67R- Immettere codice voce esistente e di tipo 'M'=Maggiorazione           7
089800000000TREC67R- Immettere codice voce esistente                                       8
089900000000TREC67R- Registrazione gia' presente in archivio                               9
090000000000TREC67R- 10
090100000000TREC67R- 11
090200000000TREC67R- 12
090300000000TREC67R- 13
090400000000TREC67R- 14
090500000000TREC67R- 15
090600000000TREC67R- 16
090700000000TREC67R- Inserimento effettuato.                                              17
090800000000TREC67R- Modifica effettuata.                                                 18
090900000000TREC67R- Cancellazione effettuata.                                            19
091000000000TREC67R- Ripristino effettuato.                                               20
091100000000TREC67R- Richiesta funzione per voce già esistente                            21
091200000000TREC67R- Codice voce momentaneamente allocato: riprovare più tardi            22
091300000000TREC67R- 23
091400000000TREC67R- 24
091500000000TREC67R- 25
091600000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
091700000000F1    -                   -    NN     NN
091800000000F2    -                   -    NN     NN
091900000000F3    -F3=Fine            -    SS     SS
092000000000F4    -                   -    NN     NN
092100000000F5    -                   -    NN     NN
092200000000F6    -F6=Aggiornamento   -    SS     SS
092300000000F7    -                   -    NN     NN
092400000000F8    -                   -    NN     NN
092500000000F9    -                   -    NN     NN
092600000000F10   -                   -    NN     NN
092700000000F11   -                   -    NN     NN
092800000000F12   -F12=Ritorno        -    SS     SS
092900000000F13   -                   -    NN     NN
093000000000F14   -                   -    NN     NN
093100000000F15   -                   -    NN     NN
093200000000F16   -                   -    NN     NN
093300000000F17   -                   -    NN     NN
093400000000F18   -                   -    NN     NN
093500000000F19   -                   -    NN     NN
093600000000F20   -                   -    NN     NN
093700000000F21   -                   -    NN     NN
093800000000F22   -                   -    NN     NN
093900000000F23   -                   -    NN     NN
094000000000F24   -F24=Altri tasti    -    NN     NN
094100000000ENTER -                   -    SS     SS
094200000000ROLLUP-                   -    NN     NN
094300000000ROLLDN-                   -    NN     NN
094400000000HELP  -                   -    NN     NN
094500000000PRINT -                   -    NN     NN
094600000000BSPACE-                   -    NN     NN
094700000000CLEAR -                   -    NN     NN
094800000000** DOPZ  (OPZIONE RICHIAMATA)
094900000000* INSERIMENTO *                                                 1
095000000000*  VARIAZIONE  *                                                2
095100000000*CANCELLAZIONE*                                                 3
095200000000*  RIPRISTINO  *                                                4
095300000000VISUALIZZAZIONE                                                 5
095400981015ANNULLATO                                                       6
0955009810157
0956009810158
0957009810159
09580098101510

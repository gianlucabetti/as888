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
001200100325     FTREC67D   CF   E             WORKSTN INFDS(DevDS1)
001300000000     F                                     SFILE(EC67DVM:MSGULT)
001400000000     FECEVM01L  UF A E           K DISK
001500000000     FECEVT01L  IF   E           K DISK
001600100421     Ftabel00f  IF   E           K DISK
001700140114     FECEVd06L  IF   E           K DISK
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE
002000000000     D*------------------------------------------------------------------------*
002100000000     D MSG             S             78    DIM(25) CTDATA PERRCD(1)
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
003500000000     D*------------------------------------------------------------------------*
003600000000     D* INPUT
003700000000     D*------------------------------------------------------------------------*
003800000000     D*-------------------
003900000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004000000000     D*-------------------
004100000000     D DSFUN         E DS
004200000000     D  FUNRIG                 1     40
004300000000     D*-------------------
004400000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004500000000     D*-------------------
004600000000     D DSMSG         E DS
004700000000     D*-------------------
004800020923     D* DS REPERIMENTO DATI UTENTE
004900020923     D*-------------------
005000020923     D TIBS34DS      E DS
005100020923     D DDATIUTE      E DS
005200020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005300000000     D*-------------------
005400000000     D* DS PER GESTIONE TASTI FUNZIONALI
005500000000     D*-------------------
005600000000     D DSKEYS        E DS
005700000000     D  KEYRIG                 1     40
005800000000     D*-------------------
005900000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006000000000     D*-------------------
006100000000     D                SDS
006200000000     D  USER                 254    263
006300000000     D*-------------------
006400000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006500000000     D*-------------------
006600000000     D DEVDS1          DS
006700000000     D* CHIAVE FUNZIONALE BATTUTA
006800000000     D  KEY                  369    369
006900000000     D* POSIZIONE CURSORE (IN HEX)
007000000000     D  CURSOR               370    371
007100000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
007200000000     D  FRTSFL               378    379B 0
007300000000     D*---------------------
007400000000     D* DS ESTERNA DI PROCEDURA
007500000000     D*---------------------
007600000000     D DS60          E DS                  EXTNAME(TREC60DS)
007700100421     D DS2O          E DS
007800000000     D*-------------------
007900000000     D* ARCHITETTURA
008000000000     D*-------------------
008100000000     D KPJBA         E DS
008200000000     D*------------------
008300000000     D* CALL XSRDA8 - CONTROLLO DATE
008400000000     D*------------------
008500000000     D WLBDAT          DS                  INZ
008600000000     D  G02DAT                 1      8  0
008700000000     D  G02INV                 9     16  0
008800000000     D  G02ERR                17     17
008900000000     D  G02TGI                18     22  0
009000000000     C*------------------------------------------------------------------------*
009100000000     C* MAIN LINE
009200000000     C*------------------------------------------------------------------------*
009300000000     C*
009400000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
009500000000IF  1C     $FINE         IFEQ      '1'
009600000000     C*
009700000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
009800000000     C                   MOVEL     '1'           D60ERR
009900000000     C*
010000000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
010100000000X   1C                   ELSE
010200000000     C*
010300000000     C* CICLO FINO A CHE:
010400000000DO  2C     $FINE         DOWEQ     '0'
010500000000     C                   MOVEL     '0'           $ERR
010600000000     C*
010700000000     C* CONTROLLA GESTIONE VIDEO
010800000000IF  3C     $GES          IFEQ      'V1'
010900000000     C                   EXSR      GESV1
011000000000E   3C                   ENDIF
011100000000     C*
011200000000E   2C                   ENDDO
011300000000E   1C                   ENDIF
011400000000     C*
011500000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
011600000000     C     $F03          IFEQ      '1'
011700000000     C                   MOVEL     '1'           D60F03
011800000000E   1C                   ENDIF
011900000000     C* FINE PGM
012000000000     C                   MOVEL     DS60          KPJBU
012100000000     C                   SETON                                        LR
012200000000     C*------------------------------------------------------------------------*
012300000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
012400000000     C*------------------------------------------------------------------------*
012500000000     C     CARV1         BEGSR
012600000000     C*
012700000000     C                   MOVEL     'V1'          $GES
012800000000     C*
012900000000     C                   EXSR      CARFUN
013000000000     C                   EXSR      OUTFUN
013100000000     C*
013200000000     C                   WRITE     EC67DVT
013300000000     C                   WRITE     EC67DVF
013400000000     C*
013500000000     C                   ENDSR
013600000000     C*------------------------------------------------------------------------*
013700000000     C* GESV1  - GESTIONE PRIMO VIDEO
013800000000     C*------------------------------------------------------------------------*
013900000000     C     GESV1         BEGSR
014000000000     C*
014100000000IF  1C     $TESVD        IFEQ      '1'
014200000000     C                   EXSR      CARV1
014300000000E   1C                   ENDIF
014400000000     C*
014500000000IF  1C     MSGULT        IFNE      *ZEROS
014600000000     C                   EXSR      OUTMSG
014700000000E   1C                   ENDIF
014800000000     C*
014900000000     C                   EXFMT     EC67DV1
015000000000     C*
015100000000     C                   EXSR      CONFUN
015200000000     C*
015300000000IF  1C     $ERR          IFEQ      '0'
015400000000SL  2C                   SELECT
015500000000     C     KEY           WHENEQ    ENTER
015600000000     C                   EXSR      ENTV1
015700000000     C     KEY           WHENEQ    F6
015800000000     C                   EXSR      F06GES
015900000000     C                   OTHER
016000000000     C                   EXSR      FUNSTD
016100000000E   2C                   ENDSL
016200000000E   1C                   ENDIF
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*------------------------------------------------------------------------*
016600000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
016700000000     C*------------------------------------------------------------------------*
016800000000     C     ENTV1         BEGSR
016900000000     C*
017000000000     C* CONTROLLA I CAMPI IMMESSI
017100000000     C                   EXSR      DECODI
017200000000     C*
017300000000     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
017400000000IF  1C     D60OP1        IFEQ      'O05'
017500000000     C                   MOVEL     '1'           $FINE
017600000000E   1C                   ENDIF
017700000000     C*
017800000000     C                   ENDSR
017900000000     C*------------------------------------------------------------------------*
018000000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
018100000000     C*------------------------------------------------------------------------*
018200000000     C     F06GES        BEGSR
018300000000     C*
018400000000     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
018500000000IF  1C     D60OP1        IFEQ      'F06'
018600000000     C                   EXSR      INSERI
018700000000E   1C                   ENDIF
018800000000IF  1C     D60OP1        IFEQ      'O02'
018900000000     C                   EXSR      MODIFI
019000000000E   1C                   ENDIF
019100000000IF  1C     D60OP1        IFEQ      'O04'
019200000000     C                   EXSR      ANNULL
019300000000E   1C                   ENDIF
019400000000IF  1C     D60OP1        IFEQ      'O07'
019500000000     C                   EXSR      RIPRIS
019600000000E   1C                   ENDIF
019700000000     C*
019800000000     C                   ENDSR
019900000000     C*------------------------------------------------------------------------*
020000000000     C* INSERI - FUNZIONE DI INSERIMENTO
020100000000     C*------------------------------------------------------------------------*
020200000000     C     INSERI        BEGSR
020300000000     C*
020400000000     C* CONTROLLA I CAMPI IMMESSI
020500000000     C                   EXSR      DECODI
020600000000     C*
020700000000IF  1C     $ERR          IFEQ      '0'
020800000000     C*
020900000000     C                   Z-ADD     V1CVOM        EVMVOM
021000000000     C                   Z-ADD     V1CPRM        EVMPRM
021100000000     C                   Z-ADD     V1CVOC        EVMVOC
021200100325     C                   Z-ADD     V1CPRG        EVMPrg
021300100326     C                   Z-ADD     V1CVOCsal     EVMViso
021400100325     C                   MOVEL     v1capplica    EVMApp
021500100326     C                   Z-ADD     V1CDDT        G02DAT
021600100326     C                   Z-ADD     *ZEROS        G02INV
021700100326     C                   MOVEL     '0'           G02ERR
021800100326     C                   CALL      'XSRDA8'
021900100326     C                   PARM                    WLBDAT
022000100326     C                   Z-ADD     G02INV        EVmDdT
022100100326     C                   Z-ADD     V1CDST        G02DAT
022200100326     C                   Z-ADD     *ZEROS        G02INV
022300100326     C                   MOVEL     '0'           G02ERR
022400100326     C                   CALL      'XSRDA8'
022500100326     C                   PARM                    WLBDAT
022600100326     C                   Z-ADD     G02INV        EVmDST
022700000000     C                   MOVEL     *BLANKS       EVMATB
022800000000     C                   MOVEL     *BLANKS       EVMFTR
022900000000     C                   Z-ADD     *ZEROS        EVMDTR
023000000000     C*
023100000000     C                   WRITE     ECEVM
023200000000     C*
023300000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
023400000000     C                   MOVEL     '1'           $FINE
023500000000     C*
023600000000     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
023700000000     C                   MOVEL     MSG(17)       D60MSG
023800000000     C*
023900000000E   1C                   ENDIF
024000000000     C*
024100000000     C                   ENDSR
024200000000     C*------------------------------------------------------------------------*
024300000000     C* MODIFI - FUNZIONE DI MODIFICA
024400000000     C*------------------------------------------------------------------------*
024500000000     C     MODIFI        BEGSR
024600000000     C*
024700000000     C* CONTROLLA I CAMPI IMMESSI
024800000000     C                   EXSR      DECODI
024900000000     C*
025000000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
025100000000     C                   Z-ADD     D60VOM        KVDVOM
025200000000     C                   Z-ADD     D60PRM        KVDPRM
025300000000     C                   Z-ADD     D60VOC        KVDVOC
025400000000     C                   Z-ADD     D60PRG        KVDPRG
025500000000     C     KEYEVM        CHAIN     ECEVM01L                           99
025600000000IF  1C     *IN99         IFEQ      *OFF
025700000000     C*
025800000000IF  2C     $ERR          IFEQ      '0'
025900000000     C*
026000000000     C                   Z-ADD     V1CVOM        EVMVOM
026100000000     C                   Z-ADD     V1CPRM        EVMPRM
026200000000     C                   Z-ADD     V1CVOC        EVMVOC
026300000000     C                   Z-ADD     V1CPRG        EVMPRG
026400100326     C                   Z-ADD     V1CVOCsal     EVMViso
026500100325     C                   MOVEL     v1capplica    EVMApp
026600100326     C                   Z-ADD     V1CDDT        G02DAT
026700100326     C                   Z-ADD     *ZEROS        G02INV
026800100326     C                   MOVEL     '0'           G02ERR
026900100326     C                   CALL      'XSRDA8'
027000100326     C                   PARM                    WLBDAT
027100100326     C                   Z-ADD     G02INV        EVmDdT
027200100326     C                   Z-ADD     V1CDST        G02DAT
027300100326     C                   Z-ADD     *ZEROS        G02INV
027400100326     C                   MOVEL     '0'           G02ERR
027500100326     C                   CALL      'XSRDA8'
027600100326     C                   PARM                    WLBDAT
027700100326     C                   Z-ADD     G02INV        EVmDST
027800000000     C                   MOVEL     *BLANKS       EVMATB
027900000000     C                   MOVEL     *BLANKS       EVMFTR
028000000000     C*
028100000000     C                   UPDATE    ECEVM
028200000000     C*
028300000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
028400000000     C                   MOVEL     '1'           $FINE
028500000000     C*
028600000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
028700000000     C                   MOVEL     MSG(18)       D60MSG
028800000000     C*
028900000000E   2C                   ENDIF
029000000000E   1C                   ENDIF
029100000000     C*
029200000000     C                   ENDSR
029300000000     C*------------------------------------------------------------------------*
029400000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
029500000000     C*------------------------------------------------------------------------*
029600000000     C     ANNULL        BEGSR
029700000000     C*
029800000000     C* CONTROLLA I CAMPI IMMESSI
029900000000     C                   EXSR      DECODI
030000000000     C*
030100000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
030200000000     C                   Z-ADD     D60VOM        KVDVOM
030300000000     C                   Z-ADD     D60PRM        KVDPRM
030400000000     C                   Z-ADD     D60VOC        KVDVOC
030500000000     C                   Z-ADD     D60PRG        KVDPRG
030600000000     C     KEYEVM        CHAIN     ECEVM01L                           99
030700000000IF  1C     *IN99         IFEQ      *OFF
030800000000     C*
030900091112IF  2C*    $ERR          IFEQ      '0'
031000000000     C*--------------------
031100000000     C* ANNULLO VOCE
031200000000     C*--------------------
031300970903     C* SE NON E' STATO MAI TRASMESSO, CANCELLA FISICAMENTE IL RECORD
031400970903IF  3C     EVMDTR        IFGT      *ZEROS
031500000000     C                   MOVEL     'A'           EVMATB
031600000000     C                   MOVEL     *BLANKS       EVMFTR
031700970903     C*---                 Z-ADD*ZEROS    EVMDTR           *DATA TRASMISSIONE
031800000000     C                   UPDATE    ECEVM
031900970903X   3C                   ELSE
032000970903     C                   DELETE    ECEVM
032100970903E   3C                   ENDIF
032200000000     C*
032300000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
032400000000     C                   MOVEL     '1'           $FINE
032500000000     C*
032600000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
032700000000     C                   MOVEL     MSG(19)       D60MSG
032800000000     C*
032900000000E   2C                   ENDIF
033000091112E   1C*                  ENDIF
033100000000     C*
033200000000     C                   ENDSR
033300000000     C*------------------------------------------------------------------------*
033400000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
033500000000     C*------------------------------------------------------------------------*
033600000000     C     RIPRIS        BEGSR
033700000000     C*
033800000000     C* CONTROLLA I CAMPI IMMESSI
033900000000     C                   EXSR      DECODI
034000000000     C*
034100000000     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
034200000000     C                   Z-ADD     D60VOM        KVDVOM
034300000000     C                   Z-ADD     D60PRM        KVDPRM
034400000000     C                   Z-ADD     D60VOC        KVDVOC
034500000000     C                   Z-ADD     D60PRG        KVDPRG
034600000000     C     KEYEVM        CHAIN     ECEVM01L                           99
034700000000IF  1C     *IN99         IFEQ      *OFF
034800000000     C*
034900000000IF  2C     $ERR          IFEQ      '0'
035000000000     C*--------------------
035100000000     C* RIPRISTINO VOCE
035200000000     C*--------------------
035300000000     C                   MOVEL     *BLANKS       EVMATB
035400000000     C                   MOVEL     *BLANKS       EVMFTR
035500970903     C*---                 Z-ADD*ZEROS    EVMDTR           *DATA TRASMISSIONE
035600000000     C*
035700000000     C                   UPDATE    ECEVM
035800000000     C*
035900000000     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
036000000000     C                   MOVEL     '1'           $FINE
036100000000     C*
036200000000     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI ANNULLAMENTO EFFETTUATO
036300000000     C                   MOVEL     MSG(20)       D60MSG
036400000000     C*
036500000000E   2C                   ENDIF
036600000000E   1C                   ENDIF
036700000000     C*
036800000000     C                   ENDSR
036900000000     C*------------------------------------------------------------------------*
037000000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
037100000000     C*------------------------------------------------------------------------*
037200000000     C     DECODI        BEGSR
037300000000     C*
037400000000     C* REIMPOSTO INDICATORI DI ERRORE
037500100329     C                   SETOFF                                       606162
037600100330     C                   SETOFF                                       636462
037700100419     C                   SETOFF                                       65
037800000000     C*--------------------
037900000000     C* CODICE VOCE MAGGIORAZIONE
038000000000     C*--------------------
038100000000     C                   MOVE      *BLANKS       V1CVDM
038200000000     C                   Z-ADD     *ZERO         V1CMDV
038300000000     C                   Z-ADD     *ZERO         V1CMSV
038400000000     C                   Z-ADD     V1CVOM        KVTVOC
038500000000     C                   Z-ADD     V1CPRM        KVTPRG
038600000000     C     KEYEVT        CHAIN     ECEVT01L                           60
038700000000IF  1C     *IN60         IFEQ      *ON
038800000000     C     EVTTVO        ORNE      'M'
038900000000     C                   MOVEL     '1'           $ERR
039000000000     C                   MOVEL     MSG(7)        $MSG
039100000000     C                   EXSR      CARMSG
039200000000     C                   SETON                                        60
039300000000X   1C                   ELSE
039400000000     C                   MOVEL     EVTVOD        V1CVDM
039500000000     C                   Z-ADD     EVTDDV        G02INV
039600000000     C                   Z-ADD     *ZEROS        G02DAT
039700000000     C                   MOVEL     '3'           G02ERR
039800000000     C                   CALL      'XSRDA8'
039900000000     C                   PARM                    WLBDAT
040000000000     C                   Z-ADD     G02DAT        V1CMDV
040100000000     C                   Z-ADD     EVTDSV        G02INV
040200000000     C                   Z-ADD     *ZEROS        G02DAT
040300000000     C                   MOVEL     '3'           G02ERR
040400000000     C                   CALL      'XSRDA8'
040500000000     C                   PARM                    WLBDAT
040600000000     C                   Z-ADD     G02DAT        V1CMSV
040700000000E   1C                   ENDIF
040800000000     C*--------------------
040900000000     C* CODICE VOCE DI CONTO ECONOMICO DA APPLICARE
041000000000     C* IL PROGRESSIVO POTREBBE ANCHE ESSERE ZERO, SIGNIFICHEREBBE "TUTTI"
041100000000     C*--------------------
041200000000     C                   MOVE      *BLANKS       V1CVDC
041300000000     C                   MOVE      *BLANKS       V1CTVO
041400000000     C                   Z-ADD     *ZERO         V1CCDV
041500000000     C                   Z-ADD     *ZERO         V1CCSV
041600000000     C                   Z-ADD     V1CVOC        KVTVOC
041700000000     C                   Z-ADD     V1CPRG        KVTPRG
041800000000IF  1C     V1CPRG        IFNE      *ZERO
041900000000     C*
042000000000     C     KEYEVT        CHAIN     ECEVT01L                           61
042100000000IF  2C     *IN61         IFEQ      *ON
042200091112IF  2C     evttvo        orne      'V'
042300000000     C                   MOVEL     '1'           $ERR
042400000000     C                   MOVEL     MSG(8)        $MSG
042500000000     C                   EXSR      CARMSG
042600000000X   2C                   ELSE
042700000000     C                   MOVEL     EVTVOD        V1CVDC
042800000000     C                   MOVEL     EVTTVO        V1CTVO
042900000000     C                   Z-ADD     EVTDDV        G02INV
043000000000     C                   Z-ADD     *ZEROS        G02DAT
043100000000     C                   MOVEL     '3'           G02ERR
043200000000     C                   CALL      'XSRDA8'
043300000000     C                   PARM                    WLBDAT
043400000000     C                   Z-ADD     G02DAT        V1CCDV
043500000000     C                   Z-ADD     EVTDSV        G02INV
043600000000     C                   Z-ADD     *ZEROS        G02DAT
043700000000     C                   MOVEL     '3'           G02ERR
043800000000     C                   CALL      'XSRDA8'
043900000000     C                   PARM                    WLBDAT
044000000000     C                   Z-ADD     G02DAT        V1CCSV
044100000000E   2C                   ENDIF
044200000000     C*
044300000000X   1C                   ELSE
044400000000     C*
044500000000     C* NEL CASO V1CPRG=0 PRENDO LA DECODIFICA DELLA PRIMA VOCE
044600000000     C     KEYEVT        SETLL     ECEVT01L
044700000000     C     V1CVOC        READE     ECEVT01L                               61
044800000000IF  2C     *IN61         IFEQ      *ON
044900000000     C                   MOVEL     '1'           $ERR
045000000000     C                   MOVEL     MSG(8)        $MSG
045100000000     C                   EXSR      CARMSG
045200000000X   2C                   ELSE
045300000000     C     'Tutti i'     CAT       'progre':1    V1CVDC
045400000000     C     V1CVDC        CAT       'ssivi':0     V1CVDC
045500000000     C                   MOVEL     EVTTVO        V1CTVO
045600000000E   2C                   ENDIF
045700000000E   1C                   ENDIF
045800100427     C* le voci di isolamento non possono essere maggiorate
045900100427IF  1C     $ERR          IFEQ      '0'
046000100427     c     evtvrv        andeq     '26'
046100100427     c                   seton                                        61
046200100427     C                   MOVEL     '1'           $ERR
046300100427     C                   MOVEL     MSG(8)        $MSG
046400100427     C                   EXSR      CARMSG
046500100427E   1C                   ENDIF
046600000000     C*
046700000000     C* NON POSSO INSERIRE DOPPIONI ALL'INTERNO DELL'ARCHIVIO
046800000000IF  1C     $ERR          IFEQ      '0'
046900000000     C     D60OP1        ANDEQ     'F06'
047000000000     C                   Z-ADD     V1CVOM        KVDVOM
047100000000     C                   Z-ADD     V1CPRM        KVDPRM
047200000000     C                   Z-ADD     V1CVOC        KVDVOC
047300000000     C                   Z-ADD     V1CPRG        KVDPRG
047400000000     C     KEYEVM        SETLL     ECEVM01L                               99
047500000000IF  2C     *IN99         IFEQ      *ON
047600000000     C                   MOVEL     '1'           $ERR
047700000000     C                   MOVEL     MSG(9)        $MSG
047800000000     C                   EXSR      CARMSG
047900000000     C                   SETON                                        6061
048000000000E   2C                   ENDIF
048100000000E   1C                   ENDIF
048200100325     C*--------------------
048300100325     C* CODICE VOCE DI CONTO ECONOMICO nella quale isolare l'importo della
048400100325     c* maggiorazione calcolata
048500100325     C*--------------------
048600100330IF  1C     V1Cvocsal     IFeq      *ZERO
048700100330     c                   clear                   v1cddt
048800100330     c                   clear                   v1cdst
048900100419     c                   clear                   v1capplica
049000100427     c                   clear                   V1CVDCSAL
049100100330     c                   else
049200100326     C     v1cvocsal     chain     ECEVT01L                           62
049300100325IF  2C     *IN62         IFEQ      *ON
049400100325     C                   MOVEL     '1'           $ERR
049500100325     C                   MOVEL     MSG(8)        $MSG
049600100325     C                   EXSR      CARMSG
049700100325E   1C                   ENDIF
049800100421     C* NON si possono inserire voci diverse da isolate (26)
049900100326IF  1C     $ERR          IFEQ      '0'
050000100427     c                   eval      V1CVDCSAL = evtvod
050100100421IF  2C                   IF        evtvrv <> '26'
050200100326     C                   MOVEL     '1'           $ERR
050300100326     C                   MOVEL     MSG(11)       $MSG
050400100326     C                   EXSR      CARMSG
050500100326     C                   SETON                                        62
050600100326E   2C                   ENDIF
050700100330E   2C                   ENDIF
050800100326     C*--------------------
050900100326     C* DATA DECORRENZA
051000100326     C*--------------------
051100100326IF  1C     $ERR          IFEQ      '0'
051200100326     C                   Z-ADD     V1CDDT        $DAT              8 0
051300100326     C                   EXSR      CHKDAT
051400100326     C                   Z-ADD     $DAT          V1CDDT
051500100326IF  2C     $ERR          IFEQ      '1'
051600100326     C                   EXSR      CARMSG
051700100326     C                   SETON                                        63
051800100326E   2C                   ENDIF
051900100326E   1C                   ENDIF
052000100326     C*--------------------
052100100326     C* DATA SCADENZA
052200100326     C*--------------------
052300100326IF  1C     $ERR          IFEQ      '0'
052400100326     C                   Z-ADD     V1CDST        $DAT
052500100326     C                   EXSR      CHKDAT
052600100326     C                   Z-ADD     $DAT          V1CDST
052700100326IF  2C     $ERR          IFEQ      '1'
052800100326     C                   EXSR      CARMSG
052900100326     C                   SETON                                        64
053000100419E   2C                   ENDIF
053100100330E   2C                   ENDIF
053200100326     C*
053300100326     C* MAGGIORE DELLA DECORRENZA
053400100326IF  2C     $ERR          IFEQ      '0'
053500100326     C                   Z-ADD     V1CDDT        G02DAT
053600100326     C                   Z-ADD     *ZEROS        G02INV
053700100326     C                   MOVEL     '0'           G02ERR
053800100326     C                   CALL      'XSRDA8'
053900100326     C                   PARM                    WLBDAT
054000100326     C                   Z-ADD     G02INV        DDTV1C            8 0
054100100326     C*
054200100326     C                   Z-ADD     V1CDST        G02DAT
054300100326     C                   Z-ADD     *ZEROS        G02INV
054400100326     C                   MOVEL     '0'           G02ERR
054500100326     C                   CALL      'XSRDA8'
054600100326     C                   PARM                    WLBDAT
054700100326     C                   Z-ADD     G02INV        DSTV1C            8 0
054800100326     C*
054900100326IF  3C     DSTV1C        IFLT      DDTV1C
055000100326     C                   MOVEL     '1'           $ERR
055100100329     C                   MOVEL     MSG(13)       $MSG
055200100326     C                   EXSR      CARMSG
055300100326     C                   SETON                                        6364
055400100326E   3C                   ENDIF
055500100326E   2C                   ENDIF
055600100330     C* NON si può inserire la voce isolata se esistono tariffe con segno -
055700100330IF  1C     $ERR          IFEQ      '0'
055800100330     C                   Z-ADD     V1CVOM        KVTVOC
055900100330     C                   Z-ADD     V1CPRM        KVTPRG
056000100421     c                   clear                   ok                1
056100140114     C     keyevt        SETLL     ECEVD06L
056200100330     c                   do        *hival
056300140114     C     keyevt        reade     ECEVD06L
056400140114     c                   if        %eof(ecevd06l)
056500100330     c                   leave
056600100330     c                   end
056700100421     c                   if        evdddt <= ddtv1c  and
056800100421     c                             evddst >= dstv1c
056900100420     c* questio controlli mi servono per non andare a memorizzare
057000100420     c* importi negativi nelle voci o per evitare di raddoppiare
057100100420     c* le competenze
057200100420IF  2C                   IF        evdseg = '-' and
057300100420     c                             v1capplica ='N'
057400100330     C                   MOVEL     '1'           $ERR
057500100330     C                   MOVEL     MSG(10)       $MSG
057600100330     C                   EXSR      CARMSG
057700100330     C                   SETON                                        6299
057800100421     c                   else
057900100421     c                   eval      ok = '1'
058000100330E   2C                   ENDIF
058100100420IF  2C                   IF        evdseg = '+' and
058200100420     c                             v1capplica ='S'
058300100420     C                   MOVEL     '1'           $ERR
058400100420     C                   MOVEL     MSG(15)       $MSG
058500100420     C                   EXSR      CARMSG
058600100420     C                   SETON                                        6299
058700100421     c                   else
058800100421     c                   eval      ok = '1'
058900100420E   2C                   ENDIF
059000100420E   2C                   ENDIF
059100100330E   2C                   ENDdo
059200100421     c                   if        ok = ' '
059300100421     c                   seton                                        65
059400100421     C                   MOVEL     '1'           $ERR
059500100421     C                   MOVEL     MSG(16)       $MSG
059600100421     C                   EXSR      CARMSG
059700100421     c                   end
059800100330E   1C                   ENDIF
059900100419     C* se inserita la voce isolata si deve inserire il flag da applicare
060000100419IF  1C                   IF        $err = '0'and
060100100419     c                             v1capplica = *blanks
060200100419     c                   seton                                        65
060300100419     C                   MOVEL     '1'           $ERR
060400100419     C                   MOVEL     MSG(14)       $MSG
060500100419     C                   EXSR      CARMSG
060600100419E   2C                   ENDIF
060700100419E   2C                   ENDIF
060800100421     C* verifico se il codice della maggiorazione è corretto nel range della
060900100421     c* tabella 2O
061000100421IF  1C     $ERR          IFEQ      '0'
061100100421     C                   EXSR      CHK2o
061200100421IF  3C     $ERR          IFEQ      '1'
061300100421     C                   EXSR      CARMSG
061400100421     C                   SETON                                        60
061500100421E   3C                   ENDIF
061600100421E   1C                   ENDIF
061700000000     C*
061800000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
061900000000     C* CURSORE SUL CAMPO DEL CONTROL
062000000000IF  1C     $ERR          IFEQ      '1'
062100000000     C                   MOVEL     '0'           *IN43
062200000000X   1C                   ELSE
062300000000     C                   MOVEL     '1'           *IN43
062400000000E   1C                   ENDIF
062500000000     C*
062600000000     C                   ENDSR
062700100421     C*------------------------------------------------------------------------*
062800100421     C* CHK2O - CONTROLLA IL range di validità del codice voce
062900100421     C*------------------------------------------------------------------------*
063000100421     C     CHK2o         BEGSR
063100100421     C*
063200100421     C                   CLEAR                   DS2o
063300100421     C                   Z-ADD     1             KTBKUT
063400100421     C                   MOVEL     '2O'          KTBCOD
063500100421     c                   if        v1cvocsal <> 0
063600100421     c                   eval      ktbkey ='MI'
063700100421     c                   else
063800100421     c                   eval      ktbkey ='M'
063900100421     c                   end
064000100421     C     KEYTAB        CHAIN     TABEL00F                           99
064100100421IF  2C     *IN99         IFEQ      '1'
064200100421     C     TBLFLG        ORNE      *BLANKS
064300100421     C                   MOVEL     '1'           $ERR
064400100421     C                   MOVEL     MSG(23)       $MSG
064500100421     c                   leavesr
064600100421X   3C                   End
064700100421     c                   if        %found
064800100421     C                   MOVEL     TBLUNI        DS2o
064900100421     c* verifico se il codice è nel range
065000100421     C                   if        §2ovocda <= v1cvom and
065100100421     C                             §2ovoca >= v1cvom
065200100421     c                   else
065300100421     C                   MOVEL     '1'           $ERR
065400100421     C                   MOVEL     MSG(23)       $MSG
065500100421E   3C                   ENDIF
065600100421E   2C                   ENDIF
065700100421     C                   ENDSR
065800100326     C*------------------------------------------------------------------------*
065900100326     C* CHKDAT - CONTROLLA LA DATA
066000100326     C*          INPUT --> $DAT:  DATA
066100100326     C*------------------------------------------------------------------------*
066200100326     C     CHKDAT        BEGSR
066300100326     C*
066400100326     C* OBBLIGATORIA
066500100326IF  1C     $DAT          IFEQ      *ZEROS
066600100326     C                   MOVEL     '1'           $ERR
066700100326     C                   MOVEL     MSG(12)       $MSG
066800100326X   1C                   ELSE
066900100326     C* CORRETTA
067000100326     C                   Z-ADD     $DAT          G02DAT
067100100326     C                   Z-ADD     *ZEROS        G02INV
067200100326     C                   MOVEL     '0'           G02ERR
067300100326     C                   CALL      'XSRDA8'
067400100326     C                   PARM                    WLBDAT
067500100326     C*
067600100326IF  2C     G02ERR        IFEQ      '1'
067700100326     C                   MOVEL     '1'           $ERR
067800100326     C                   MOVEL     MSG(12)       $MSG
067900100326     C                   ELSE
068000100326     C                   Z-ADD     G02DAT        $DAT
068100100326E   2C                   ENDIF
068200100326E   1C                   ENDIF
068300100326     C*
068400100326     C                   ENDSR
068500000000     C*------------------------------------------------------------------------*
068600000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
068700000000     C*------------------------------------------------------------------------*
068800000000     C     RECVID        BEGSR
068900000000     C*
069000000000     C                   Z-ADD     D60VOM        V1CVOM
069100000000     C                   Z-ADD     D60PRM        V1CPRM
069200000000     C                   Z-ADD     D60VOC        V1CVOC
069300000000     C                   Z-ADD     D60PRG        V1CPRG
069400100326     C                   z-add     evmviso       v1cvocsal
069500100325     C                   movel     evmapp        v1capplica
069600100326     C                   Z-ADD     EVmDdt        G02INV
069700100326     C                   Z-ADD     *ZEROS        G02DAT
069800100326     C                   MOVEL     '3'           G02ERR
069900100326     C                   CALL      'XSRDA8'
070000100326     C                   PARM                    WLBDAT
070100100326     C                   Z-ADD     G02DAT        V1Cddt
070200100326     C                   Z-ADD     EVmDst        G02INV
070300100326     C                   Z-ADD     *ZEROS        G02DAT
070400100326     C                   MOVEL     '3'           G02ERR
070500100326     C                   CALL      'XSRDA8'
070600100326     C                   PARM                    WLBDAT
070700100326     C                   Z-ADD     G02DAT        V1Cdst
070800100419     c                   if        v1cdst <> 0
070900100419     c     v1cdst        comp      31122039                           9292
071000100419     c                   else
071100100419     c                   setoff                                       92
071200100419     c                   end
071300981015IF  1C     EVMATB        IFNE      *BLANKS
071400981015     C                   MOVEL     DOPZ(6)       V1CATB
071500981015E   1C                   ENDIF
071600000000     C*
071700000000     C                   ENDSR
071800000000     C*------------------------------------------------------------------------*
071900000000     C*  AZZVID - PULISCE TUTTI ICAMPI DEL VIDEO
072000000000     C*------------------------------------------------------------------------*
072100000000     C     AZZVID        BEGSR
072200000000     C*
072300000000     C                   MOVE      *ZERO         V1CVOM
072400000000     C                   Z-ADD     *ZERO         V1CPRM
072500000000     C                   MOVE      *ZERO         V1CVOC
072600000000     C                   Z-ADD     *ZERO         V1CPRG
072700100325     C                   MOVE      *ZERO         V1CVOCsal
072800100326     C                   MOVE      *ZERO         V1Cddt
072900100326     C                   MOVE      *ZERO         V1Cdst
073000100419     C                   MOVEL     *BLANKS       V1Capplica
073100100419     C                   MOVEL     *BLANKS       V1CATB
073200100419     c                   setoff                                       92
073300000000     C*
073400000000     C                   ENDSR
073500020923     C*-------------------------------------------------------------------------
073600020923     C* REPERISCE I DATI UTENTE
073700020923     C*-------------------------------------------------------------------------
073800020923     C     REPDATIUTE    BEGSR
073900020923     C*
074000020923     C* INIZIALIZZA VARIABILI DI WRK
074100020923     C                   CLEAR                   TIBS34DS
074200020923     C                   CLEAR                   AZUTEDS
074300020923     C                   CLEAR                   DDATIUTE
074400020923     C*
074500020923     C     *DTAARA       DEFINE    §azute        azuteds
074600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
074700020923     C                   IN(E)     *DTAARA
074800020923if  1C                   IF        %Error
074900020923     c                   EVAL      I34Tla = 'L'
075000020923     C                   CALL      'TIBS34R'
075100020923     C                   PARM                    Tibs34Ds
075200020923     C                   IN        *DTAARA
075300020923e   1C                   ENDIF
075400020923     C*
075500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
075600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
075700020923     C*
075800020923     C                   ENDSR
075900000000     C*------------------------------------------------------------------------*
076000000000     C*  *INZSR - ROUTINE INIZIALE
076100000000     C*------------------------------------------------------------------------*
076200000000     C     *INZSR        BEGSR
076300000000     C*
076400000000     C* RICEVIMENTO PARAMETRI
076500000000     C     *ENTRY        PLIST
076600000000     C                   PARM                    KPJBA
076700000000     C                   MOVEL     KPJBU         DS60
076800000000     C*
076900000000     C* DEFINIZIONE VARIABILI
077000100421     C     *LIKE         DEFINE    TBLKUT        KTBKUT
077100100421     C     *LIKE         DEFINE    TBLCOD        KTBCOD
077200100421     C     *LIKE         DEFINE    TBLKEY        KTBKEY
077300000000     C     *LIKE         DEFINE    EVMVOM        KVDVOM
077400000000     C     *LIKE         DEFINE    EVMPRM        KVDPRM
077500000000     C     *LIKE         DEFINE    EVMVOC        KVDVOC
077600000000     C     *LIKE         DEFINE    EVMPRG        KVDPRG
077700000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
077800000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
077900000000     C*
078000000000     C* CHIAVE LETTURA ECEVM01L - COMPLETA
078100000000     C     KEYEVM        KLIST
078200000000     C                   KFLD                    KVDVOM
078300000000     C                   KFLD                    KVDPRM
078400000000     C                   KFLD                    KVDVOC
078500000000     C                   KFLD                    KVDPRG
078600100421     C* CHIAVE LETTURA TABEL00F - COMPLETA
078700100421     C     KEYTAB        KLIST
078800100421     C                   KFLD                    KTBKUT
078900100421     C                   KFLD                    KTBCOD
079000100421     C                   KFLD                    KTBKEY
079100000000     C*
079200000000     C* CHIAVE LETTURA ECEVT01L - PARZIALE
079300000000     C     KEYEVT        KLIST
079400000000     C                   KFLD                    KVTVOC
079500000000     C                   KFLD                    KVTPRG
079600000000     C*
079700000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
079800000000     C                   MOVE      D60OP0        INTGES            2
079900000000IF  1C     INTGES        IFEQ      '02'
080000000000     C                   MOVEL     'G'           $TGES             1
080100000000X   1C                   ELSE
080200000000     C                   MOVEL     'I'           $TGES             1
080300000000E   1C                   ENDIF
080400000000     C*
080500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
080600000000     C                   EXSR      INZSTD
080700000000     C*
080800000000     C* RAGIONE SOCIALE UTENTE
080900000000     C                   MOVEL     DSFIRS        VTDRSU
081000000000     C*
081100000000     C* DECODIFICA OPZIONE RICHIAMATA
081200000000IF  1C     D60OP1        IFEQ      'F06'
081300000000     C                   MOVEL     DOPZ(1)       VTDFUN
081400000000E   1C                   ENDIF
081500000000IF  2C     D60OP1        IFEQ      'O02'
081600000000     C                   MOVEL     DOPZ(2)       VTDFUN
081700000000E   1C                   ENDIF
081800000000IF  1C     D60OP1        IFEQ      'O04'
081900000000     C                   MOVEL     DOPZ(3)       VTDFUN
082000000000E   1C                   ENDIF
082100000000IF  1C     D60OP1        IFEQ      'O07'
082200000000     C                   MOVEL     DOPZ(4)       VTDFUN
082300000000E   1C                   ENDIF
082400000000IF  1C     D60OP1        IFEQ      'O05'
082500000000     C                   MOVEL     DOPZ(5)       VTDFUN
082600000000E   1C                   ENDIF
082700000000     C*
082800000000     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
082900000000     C* 0=NON PROTETTO, 1=PROTETTO
083000000000     C*   90 - CAMPI CHIAVE
083100000000     C*   91 - ALTRI CAMPI
083200000000IF  1C     D60OP1        IFEQ      'F06'
083300000000     C                   SETOFF                                       9091
083400000000E   1C                   ENDIF
083500000000IF  1C     D60OP1        IFEQ      'O02'
083600000000     C                   SETON                                        90
083700000000E   1C                   ENDIF
083800000000IF  1C     D60OP1        IFEQ      'O04'
083900000000     C     D60OP1        OREQ      'O07'
084000000000     C     D60OP1        OREQ      'O05'
084100000000     C                   SETON                                        9091
084200000000E   1C                   ENDIF
084300000000     C*
084400000000     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
084500000000     C* E METTO F4=DECODIFICA
084600000000IF  1C     D60OP1        IFEQ      'O05'
084700000000     C                   MOVEL     FUN(6)        FUNRIG
084800000000     C                   MOVEL     'N'           FUNASG
084900000000     C                   MOVEL     'N'           FUNASI
085000000000     C                   MOVEL     'N'           FUNAFG
085100000000     C                   MOVEL     'N'           FUNAFG
085200000000     C                   MOVEL     FUNRIG        FUN(6)
085300000000E   1C                   ENDIF
085400000000     C*
085500000000     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
085600000000IF  1C     D60OP1        IFEQ      'O02'
085700000000     C     D60OP1        OREQ      'O04'
085800000000     C     D60OP1        OREQ      'O05'
085900000000     C     D60OP1        OREQ      'O07'
086000000000     C                   Z-ADD     D60VOM        KVDVOM
086100000000     C                   Z-ADD     D60PRM        KVDPRM
086200000000     C                   Z-ADD     D60VOC        KVDVOC
086300000000     C                   Z-ADD     D60PRG        KVDPRG
086400000000     C*
086500000000     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
086600000000     C     KEYEVM        CHAIN     ECEVM01L                           9998
086700000000IF  2C     *IN99         IFEQ      '1'
086800000000     C                   MOVEL     '1'           $ERR
086900000000     C                   MOVEL     '1'           $FINE
087000000000     C                   MOVEL     MSG(21)       D60MSG
087100000000X   2C                   ELSE
087200000000IF  3C     *IN98         IFEQ      '1'
087300000000     C                   MOVEL     '1'           $ERR
087400000000     C                   MOVEL     '1'           $FINE
087500000000     C                   MOVEL     MSG(22)       D60MSG
087600000000X   3C                   ELSE
087700000000     C*
087800000000     C                   EXSR      RECVID
087900000000     C                   EXSR      CARV1
088000000000E   3C                   ENDIF
088100000000E   2C                   ENDIF
088200000000E   1C                   ENDIF
088300000000     C*
088400000000     C* SE INSERIMENTO
088500000000IF  1C     D60OP1        IFEQ      'F06'
088600000000     C                   EXSR      AZZVID
088700000000     C                   EXSR      CARV1
088800000000     C                   ELSE
088900000000     C                   EXSR      DECODI
089000000000E   1C                   ENDIF
089100000000     C*
089200000000     C                   ENDSR
089300000000     C*------------------------------------------------------------------------*
089400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
089500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
089600000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
089700000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
089800000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
089900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
090000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
090100000000     C*------------------------------------------------------------------------*
090200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
090300000000     C*------------------------------------------------------------------------*
090400000000     C     INZSTD        BEGSR
090500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
090600000000     C                   CALL      'XKEYS'
090700000000     C                   PARM                    DSKEYS
090800000000     C                   MOVEA     KEYRIG        KEYS
090900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
091000000000     C     XMSGPA        PLIST
091100000000     C                   PARM                    DSMSMS
091200000000     C                   PARM                    DSMSPG
091300000000     C                   PARM                    DSMSKE
091400000000     C                   PARM                    DSMSER
091500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
091600000000     C                   MOVEL     'TREC67R'     DSMSPG
091700000000     C                   MOVEL     'TREC67R'     VMCQUE
091800000000     C                   MOVEL     *BLANKS       VMCKEY
091900000000     C                   MOVEL     *BLANKS       DSMSMS
092000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
092100000000     C                   MOVEL     '0'           $ERR              1
092200000000     C                   MOVEL     '0'           $F03              1
092300000000     C                   MOVEL     *BLANKS       $GES              2
092400000000     C                   MOVEL     *BLANKS       $MSG             80
092500000000     C                   MOVEL     '0'           $FINE             1
092600000000     C                   MOVEL     '0'           $TESVD            1
092700000000     C* LANCIO ROUTINE PER VERIFICARE SE
092800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
092900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
093000020923     C                   EXSR      REPDATIUTE
093100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
093200000000     C                   BITOFF    '01234567'    POSMI1            1
093300000000     C                   BITOFF    '01234567'    POSMI2            1
093400000000     C                   BITOFF    '01234567'    POSMA1            1
093500000000     C                   BITOFF    '01234567'    POSMA2            1
093600000000     C                   BITON     '34'          POSMI1            1
093700000000     C                   BITON     '7'           POSMI2            1
093800000000     C                   BITON     '34'          POSMA1            1
093900000000     C                   BITON     '0'           POSMA2            1
094000000000     C                   MOVE      POSMI2        POSMIN            2
094100000000     C                   MOVEL     POSMI1        POSMIN
094200000000     C                   MOVE      POSMA2        POSMAX            2
094300000000     C                   MOVEL     POSMA1        POSMAX
094400000000     C                   ENDSR
094500000000     C*------------------------------------------------------------------------*
094600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
094700000000     C*------------------------------------------------------------------------*
094800000000     C     FUNSTD        BEGSR
094900000000     C                   SELECT
095000000000     C     KEY           WHENEQ    F3
095100000000     C                   EXSR      F03STD
095200000000     C     KEY           WHENEQ    F12
095300000000     C                   EXSR      F12STD
095400000000     C     KEY           WHENEQ    F24
095500000000     C                   EXSR      F24STD
095600000000     C                   OTHER
095700000000     C                   MOVEL     MSG(2)        $MSG
095800000000     C                   EXSR      CARMSG
095900000000     C                   ENDSL
096000000000     C                   ENDSR
096100000000     C*------------------------------------------------------------------------*
096200000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
096300000000     C*------------------------------------------------------------------------*
096400000000     C     F03STD        BEGSR
096500000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
096600000000     C                   MOVEL     '1'           $F03
096700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
096800000000     C                   MOVEL     '1'           $FINE
096900000000     C                   ENDSR
097000000000     C*------------------------------------------------------------------------*
097100000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
097200000000     C*------------------------------------------------------------------------*
097300000000     C     F12STD        BEGSR
097400000000     C* IMPOSTO FLAG DI FIN PGM
097500000000     C                   MOVEL     '1'           $FINE
097600000000     C                   ENDSR
097700000000     C*------------------------------------------------------------------------*
097800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
097900000000     C*------------------------------------------------------------------------*
098000000000     C     F24STD        BEGSR
098100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
098200000000     C                   EXSR      OUTFUN
098300000000     C                   ENDSR
098400000000     C*------------------------------------------------------------------------*
098500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
098600000000     C*          CODA MESSAGGI DEL PGM
098700000000     C*------------------------------------------------------------------------*
098800000000     C     CARMSG        BEGSR
098900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
099000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
099100000000     C     MSGULT        IFEQ      0
099200000000     C                   MOVEL     '1'           *IN40
099300000000     C                   WRITE     EC67DVK
099400000000     C                   MOVEL     '0'           *IN40
099500000000     C                   ENDIF
099600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
099700000000     C                   MOVEL     $MSG          DSMSMS
099800000000     C                   CALL      'XMSG'        XMSGPA
099900000000     C     DSMSER        IFEQ      0
100000000000     C                   MOVEL     DSMSKE        VMCKEY
100100000000     C                   ENDIF
100200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
100300000000     C                   ADD       1             MSGULT            4 0
100400000000     C                   WRITE     EC67DVM
100500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
100600000000     C                   MOVEL     *BLANKS       $MSG
100700000000     C                   ENDSR
100800000000     C*------------------------------------------------------------------------*
100900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
101000000000     C*------------------------------------------------------------------------*
101100000000     C     OUTMSG        BEGSR
101200000000     C* VISUALIZZAZIONE MESSAGGIO
101300000000     C                   MOVEL     '1'           *IN44
101400000000     C                   WRITE     EC67DVK
101500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
101600000000     C                   Z-ADD     *ZEROS        MSGULT
101700000000     C                   ENDSR
101800000000     C*------------------------------------------------------------------------*
101900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
102000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
102100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
102200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
102300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
102400000000     C*------------------------------------------------------------------------*
102500000000     C     CARFUN        BEGSR
102600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
102700000000     C     1             DO        31            I                 5 0
102800000000     C                   MOVEL     FUN(I)        FUNRIG
102900000000     C                   MOVEL     FUNFUN        FFU(I)
103000000000     C                   MOVEL     FUNDES        FDE(I)
103100020923     C     DUTLPO        IFEQ      'S'
103200000000     C     $TGES         IFEQ      'G'
103300000000     C                   MOVEL     FUNASG        FAT(I)
103400000000     C                   ELSE
103500000000     C                   MOVEL     FUNASI        FAT(I)
103600000000     C                   ENDIF
103700000000     C                   ELSE
103800000000     C     $TGES         IFEQ      'G'
103900000000     C                   MOVEL     FUNAFG        FAT(I)
104000000000     C                   ELSE
104100000000     C                   MOVEL     FUNAFI        FAT(I)
104200000000     C                   ENDIF
104300000000     C                   ENDIF
104400000000     C     FAT(I)        IFEQ      'E'
104500000000     C                   MOVEL     USER          WEDP              3
104600000000     C     WEDP          IFEQ      'EDP'
104700000000     C                   MOVEL     'S'           FAT(I)
104800000000     C                   ELSE
104900000000     C                   MOVEL     'N'           FAT(I)
105000000000     C                   ENDIF
105100000000     C                   ENDIF
105200000000     C                   ENDDO
105300000000     C                   ENDSR
105400000000     C*------------------------------------------------------------------------*
105500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
105600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
105700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
105800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
105900000000     C*------------------------------------------------------------------------*
106000000000     C     OUTFUN        BEGSR
106100000000     C     1             DO        8             I
106200000000     C                   MOVEL     *BLANKS       FUV(I)
106300000000     C                   ENDDO
106400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
106500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
106600000000     C     1             DO        31            I
106700000000     C     FAT(I)        IFEQ      'S'
106800000000     C     FDE(I)        ANDNE     *BLANKS
106900000000     C                   ADD       1             FUNNAT
107000000000     C                   ENDIF
107100000000     C                   ENDDO
107200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
107300000000     C     FUNNAT        IFGT      8
107400000000     C                   MOVEL     'S'           FAT(24)
107500000000     C                   ENDIF
107600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
107700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
107800000000     C                   Z-ADD     8             FUNVIS            5 0
107900000000     C     FAT(23)       IFEQ      'S'
108000000000     C                   SUB       1             FUNVIS
108100000000     C                   ENDIF
108200000000     C     FAT(24)       IFEQ      'S'
108300000000     C                   SUB       1             FUNVIS
108400000000     C                   ENDIF
108500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
108600000000     C                   Z-ADD     *ZEROS        W                 5 0
108700000000     C                   ADD       1             FUNULT
108800000000     C     FUNULT        IFGT      31
108900000000     C                   Z-ADD     1             FUNULT
109000000000     C                   ENDIF
109100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
109200000000     C     FUNULT        DO        31            I
109300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
109400000000     C     W             IFLT      FUNVIS
109500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
109600000000     C     FAT(I)        IFEQ      'S'
109700000000     C     FDE(I)        ANDNE     *BLANKS
109800000000     C     I             IFEQ      23
109900000000     C     I             OREQ      24
110000000000     C                   ELSE
110100000000     C                   ADD       1             W
110200000000     C                   MOVEL     FDE(I)        FUV(W)
110300000000     C                   ENDIF
110400000000     C                   ENDIF
110500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
110600000000     C                   Z-ADD     I             FUNULT
110700000000     C                   ENDIF
110800000000     C                   ENDDO
110900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
111000000000     C                   MOVEL     FUV(1)        VFD001
111100000000     C                   MOVEL     FUV(2)        VFD002
111200000000     C                   MOVEL     FUV(3)        VFD003
111300000000     C                   MOVEL     FUV(4)        VFD004
111400000000     C                   MOVEL     FUV(5)        VFD005
111500000000     C                   MOVEL     FUV(6)        VFD006
111600000000     C     FAT(23)       IFEQ      'N'
111700000000     C     FAT(24)       ANDEQ     'N'
111800000000     C                   MOVEL     FUV(7)        VFD007
111900000000     C                   MOVEL     FUV(8)        VFD008
112000000000     C                   ELSE
112100000000     C     FAT(23)       IFEQ      'S'
112200000000     C     FAT(24)       ANDEQ     'S'
112300000000     C                   MOVEL     FDE(23)       VFD007
112400000000     C                   MOVEL     FDE(24)       VFD008
112500000000     C                   ELSE
112600000000     C                   MOVEL     FUV(7)        VFD007
112700000000     C     FAT(23)       IFEQ      'S'
112800000000     C                   MOVEL     FDE(23)       VFD008
112900000000     C                   ELSE
113000000000     C                   MOVEL     FDE(24)       VFD008
113100000000     C                   ENDIF
113200000000     C                   ENDIF
113300000000     C                   ENDIF
113400000000     C                   MOVEL     '1'           $TESVD
113500000000     C                   ENDSR
113600000000     C*------------------------------------------------------------------------*
113700000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
113800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
113900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
114000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
114100000000     C*------------------------------------------------------------------------*
114200000000     C     CONFUN        BEGSR
114300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
114400000000     C     1             DO        31            I
114500000000     C     KEY           IFEQ      KEYS(I)
114600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
114700000000     C     FAT(I)        IFNE      'S'
114800000000     C                   MOVEL     MSG(1)        $MSG
114900000000     C                   EXSR      CARMSG
115000000000     C                   MOVEL     '1'           $ERR
115100000000     C                   ENDIF
115200000000     C                   ENDIF
115300000000     C                   ENDDO
115400000000     C                   ENDSR
115500981015** ======== SCHIERA: MSG  (MESSAGGI)                       ================
115600000000TREC67R- Tasto funzionale non valido.                                          1
115700000000TREC67R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
115800000000TREC67R- Opzione non valida.                                                   3
115900000000TREC67R- Opzione abilitata ma non gestita. Avvertire CED.                      4
116000000000TREC67R- Inizio elenco.                                                        5
116100000000TREC67R- Fine elenco.                                                          6
116200000000TREC67R- Immettere codice voce esistente e di tipo 'M'=Maggiorazione           7
116300100427TREC67R- Immettere codice voce esistente e non di isolamento                   8
116400000000TREC67R- Registrazione gia' presente in archivio                               9
116500100422TREC67R- Con applica NO la maggiorazione deve avere tariffe positive          10
116600100421TREC67R- la voce inserita non è di tipo ISOLAMENTO                            11
116700100329TREC67R- Immettere data formalmente valida                                    12
116800100329TREC67R- La scadenza voce deve essere maggiore o uguale della decorrenza      13
116900100419TREC67R- Immetere S o N
117000100422TREC67R- Con applica SI la maggiorazione deve avere tariffe negative          15
117100100421TREC67R- Per il periodo scelto mancono le tariffe della maggiorazione.
117200000000TREC67R- Inserimento effettuato.                                              17
117300000000TREC67R- Modifica effettuata.                                                 18
117400000000TREC67R- Cancellazione effettuata.                                            19
117500000000TREC67R- Ripristino effettuato.                                               20
117600000000TREC67R- Richiesta funzione per voce già esistente                            21
117700000000TREC67R- Codice voce momentaneamente allocato: riprovare più tardi            22
117800100422TREC67R-Il cod. non è presente nel range per il tipo maggiorazione (tab.2O)   23
117900100421TREC67R- 24
118000000000TREC67R- 25
118100000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
118200000000F1    -                   -    NN     NN
118300000000F2    -                   -    NN     NN
118400000000F3    -F3=Fine            -    SS     SS
118500000000F4    -                   -    NN     NN
118600000000F5    -                   -    NN     NN
118700000000F6    -F6=Aggiornamento   -    SS     SS
118800000000F7    -                   -    NN     NN
118900000000F8    -                   -    NN     NN
119000000000F9    -                   -    NN     NN
119100000000F10   -                   -    NN     NN
119200000000F11   -                   -    NN     NN
119300000000F12   -F12=Ritorno        -    SS     SS
119400000000F13   -                   -    NN     NN
119500000000F14   -                   -    NN     NN
119600000000F15   -                   -    NN     NN
119700000000F16   -                   -    NN     NN
119800000000F17   -                   -    NN     NN
119900000000F18   -                   -    NN     NN
120000000000F19   -                   -    NN     NN
120100000000F20   -                   -    NN     NN
120200000000F21   -                   -    NN     NN
120300000000F22   -                   -    NN     NN
120400000000F23   -                   -    NN     NN
120500000000F24   -F24=Altri tasti    -    NN     NN
120600000000ENTER -                   -    SS     SS
120700000000ROLLUP-                   -    NN     NN
120800000000ROLLDN-                   -    NN     NN
120900000000HELP  -                   -    NN     NN
121000000000PRINT -                   -    NN     NN
121100000000BSPACE-                   -    NN     NN
121200000000CLEAR -                   -    NN     NN
121300000000** DOPZ  (OPZIONE RICHIAMATA)
121400000000* INSERIMENTO *                                                 1
121500000000*  VARIAZIONE  *                                                2
121600000000*CANCELLAZIONE*                                                 3
121700000000*  RIPRISTINO  *                                                4
121800000000VISUALIZZAZIONE                                                 5
121900981015ANNULLATO                                                       6
1220009810157
1221009810158
1222009810159
12230098101510

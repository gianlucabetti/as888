000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC62R
000300000000      *  Descrizione   :  Gesione tariffe voci di conto economico
000400000000      *                   Programma scelta parametri di tariffa
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  09 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC62D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC62DVM:MSGULT)
001400000000     FAZORG01L  IF   E           K DISK
001500000000     FECEVT01L  IF   E           K DISK
001600000000     FTABEL00F  IF   E           K DISK
001700941128     FTNTAM01L  IF   E           K DISK
001800131119     Fazcpc01L  IF   E           K DISK
001900000000     D*------------------------------------------------------------------------*
002000000000     D* SCHIERE
002100000000     D*------------------------------------------------------------------------*
002200000000     D MSG             S             78    DIM(25) CTDATA PERRCD(1)
002300000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002400000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002500000000     D FFU             S              6    DIM(31)
002600000000     D FDE             S             19    DIM(31)
002700000000     D FAT             S              1    DIM(31)
002800000000     D* TABELLA DESCRIZIONI A VIDEO
002900000000     D FUV             S             19    DIM(8)
003000000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003100000000     D KEYS            S              1    DIM(40)
003200000000     D* TABELLA CAPICONTO
003300000000     D*------------------------------------------------------------------------*
003400000000     D* INPUT
003500000000     D*------------------------------------------------------------------------*
003600000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700000000     D DSFUN         E DS
003800000000     D  FUNRIG                 1     40
003900000000     D*------------------------------------------------------------------------*
004000000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004100000000     D DSMSG         E DS
004200000000     D*------------------------------------------------------------------------*
004300020923     D* DS REPERIMENTO DATI UTENTE
004400020923     D TIBS34DS      E DS
004500020923     D DDATIUTE      E DS
004600020923     D AZUTEDS       E DS                  extname(AZUTE00F)
004700000000     D*------------------------------------------------------------------------*
004800000000     D* DS PER GESTIONE TASTI FUNZIONALI
004900000000     D DSKEYS        E DS
005000000000     D  KEYRIG                 1     40
005100000000     D*------------------------------------------------------------------------*
005200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005300000000     D                SDS
005400000000     D* NOME UTENTE
005500000000     D  USER                 254    263
005600000000     D*------------------------------------------------------------------------*
005700000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005800000000     D DEVDS1          DS
005900000000     D* CHIAVE FUNZIONALE BATTUTA
006000000000     D  KEY                  369    369
006100000000     D* POSIZIONE CURSORE (IN HEX)
006200000000     D  CURSOR               370    371
006300970627     D*---------------------
006400970627     D* DS "TISI95R" - CONTROLLA CAP
006500970627     D*---------------------
006600970703     D DSSI95        E DS                  EXTNAME(TISI95DS)
006700000000     D*---------------------
006800000000     D* DS ESTERNA DI PROCEDURA
006900000000     D*---------------------
007000970703     D DS60          E DS                  EXTNAME(TREC60DS)
007100000000     D*---------------------
007200000000     D* ARCHITETTURA
007300000000     D*---------------------
007400000000     D KPJBA         E DS
007500970627     D*---------------------
007600970627     D* DS "XSRDA8" - CONTROLLA DATA
007700970627     D*---------------------
007800000000     D WLBDAT          DS                  INZ
007900000000     D  G02DAT                 1      8  0
008000000000     D  G02INV                 9     16  0
008100000000     D  G02ERR                17     17
008200000000     D  G02TGI                18     22  0
008300000000     D*------------------
008400950718     D* PARAMETRI CHIAMATA 'TNTA36R'
008500000000     D*------------------
008600000000     D PARAM4          DS                  INZ
008700000000     D  PARCOD                 6     12  0
008800000000     D  PARCTR                13     15
008900000000     D  PARFLG                16     16
009000000000     D  PARID                 17     17  0
009100000000     D  PARCCC                18     21  0
009200000000     D  PARDCV                22     36
009300000000     D*------------------
009400971020     D* PARAMETRI PER RICERCA C.A.P.
009500000000     D*------------------
009600000000     D PARCAP          DS
009700000000     D  PA1OPZ                 1      2
009800000000     D  PA1CAP                 3      7
009900000000     D  PA1LOC                 8     32
010000000000     D  PA1PRV                33     34
010100000000     D  PA1LNA                35     37
010200000000     D  PA1ERR                38     38
010300000000     D  PA1MSG                39    118
010400000000     D*---------------------
010500000000     D* DS ESTERNA PER TABELLA 2Q
010600000000     D*---------------------
010700000000     D DS2Q          E DS
010800131003     D*-------------------
010900131003     D* DS ESTERNA TABELLA 15 (CODICI nazioni)
011000131003     D*-------------------
011100131003     D DS15          E DS
011200000000     C*------------------------------------------------------------------------*
011300000000     C* MAIN LINE
011400000000     C*------------------------------------------------------------------------*
011500000000     C*
011600000000     C* CICLO FINO A CHE:
011700000000     C                   WRITE     EC62DVT
011800000000DO  1C     $FINE         DOUEQ     '1'
011900000000     C*
012000000000     C* CONTROLLA GESTIONE VIDEO PARAMETRI GENERALI
012100000000     C                   MOVEL     '0'           $ERR
012200000000IF  2C     $GES          IFEQ      'V1'
012300000000     C                   EXSR      GESV1
012400000000E   2C                   ENDIF
012500000000     C*
012600000000     C* CONTROLLA GESTIONE VIDEO PARAMETRI TIPO TARIFFA
012700000000     C                   MOVEL     '0'           $ERR
012800000000IF  2C     $GES          IFEQ      'V2'
012900000000     C                   EXSR      GESV2
013000000000E   2C                   ENDIF
013100000000     C*
013200000000E   1C                   ENDDO
013300000000     C*
013400000000IF  1C******     $F03      IFEQ '1'
013500000000     C******               MOVEL'1'       D60F03
013600000000E   1C******               ENDIF
013700970627     C*
013800970627     C* OPERAZIONI FINALI
013900970627     C                   EXSR      FINSR
014000970627     C*
014100970627     C                   MOVEL     DS60          KPJBU
014200000000     C                   SETON                                        LR
014300000000     C*------------------------------------------------------------------------*
014400000000     C* GESV1 - GESTIONE VIDEO RICHIESTA PARAMETRI GENERALI
014500000000     C*------------------------------------------------------------------------*
014600000000     C     GESV1         BEGSR
014700000000     C*
014800000000IF  1C     $TESVD        IFEQ      '1'
014900000000     C                   EXSR      CARV1
015000000000E   1C                   ENDIF
015100000000     C*
015200000000IF  1C     MSGULT        IFEQ      *ZEROS
015300000000     C                   MOVEL     *BLANKS       $MSG
015400000000     C                   EXSR      CARMSG
015500000000E   1C                   ENDIF
015600000000     C                   EXSR      OUTMSG
015700000000     C*
015800000000     C                   SETON                                        03
015900000000     C                   EXFMT     EC62DV1
016000000000     C*
016100000000     C                   EXSR      CONFUN
016200000000     C*
016300000000IF  1C     $ERR          IFEQ      '0'
016400000000SL  2C                   SELECT
016500000000     C     KEY           WHENEQ    ENTER
016600000000     C                   EXSR      ENTV1
016700000000     C     KEY           WHENEQ    F4
016800000000     C                   EXSR      F04V1
016900000000     C                   OTHER
017000000000     C                   EXSR      FUNSTD
017100000000E   2C                   ENDSL
017200000000E   1C                   ENDIF
017300000000     C*
017400000000     C                   ENDSR
017500000000     C*------------------------------------------------------------------------*
017600000000     C* CARV1 - CARICA VIDEO RICHIESTA PARAMETRI GENERALI
017700000000     C*------------------------------------------------------------------------*
017800000000     C     CARV1         BEGSR
017900000000     C*
018000000000     C                   MOVEL     'V1'          $GES
018100000000     C*
018200000000     C                   EXSR      CARFUN
018300000000     C                   EXSR      OUTFUN
018400000000     C*
018500000000     C********             WRITEEC62DVT                    *TESTATA
018600000000     C********             WRITEEC62DVF                    *FUNZIONI
018700000000     C*
018800000000     C                   ENDSR
018900000000     C*------------------------------------------------------------------------*
019000000000     C* ENTV1 - GESTIONE TASTO ENTER
019100000000     C*------------------------------------------------------------------------*
019200000000     C     ENTV1         BEGSR
019300000000     C*
019400000000     C* CONTROLLA I CAMPI IMMESSI
019500000000     C                   EXSR      DECOD1
019600000000     C*
019700000000IF  1C     $ERR          IFEQ      '0'
019800000000     C*
019900000000IF  2C     V1CTTV        IFEQ      'C'
020000000000     C     V1CTTV        OREQ      *BLANKS
020100000000     C                   Z-ADD     V1CVOC        D60VOC
020200000000     C                   Z-ADD     V1CPRG        D60PRG
020300000000     C                   MOVEL     V1CTTV        D60TTV
020400000000     C                   Z-ADD     *ZERO         D60FT1
020500000000     C                   Z-ADD     *ZERO         D60FT2
020600000000     C                   Z-ADD     *ZERO         D60CLI
020700000000     C                   Z-ADD     *ZERO         D60CTR
020800000000     C                   MOVE      *BLANKS       D60CTS
020900131002     C                   MOVE      *BLANKS       D60Cad
021000131002     C                   MOVE      *BLANKS       D60naz
021100000000     C                   Z-ADD     *ZERO         D60CAP
021200000000     C                   Z-ADD     V1CDDT        D60DDT
021300000000     C                   Z-ADD     *ZERO         D60KCC
021400000000     C                   MOVEL     '1'           $FINE
021500000000     C*
021600000000     C* CARICA VIDEO PARAMETRI TARIFFE CLIENTI/FILIALI
021700000000     C                   ELSE
021800000000     C                   MOVE      '1'           $TESVD
021900000000     C                   EXSR      CARV2
022000000000E   2C                   ENDIF
022100000000E   1C                   ENDIF
022200000000     C*
022300000000     C                   ENDSR
022400000000     C*------------------------------------------------------------------------*
022500000000     C* F04V1 - GESTIONE TASTO F4 = DECODIFICA
022600000000     C*------------------------------------------------------------------------*
022700000000     C     F04V1         BEGSR
022800000000     C*
022900000000     C* CARICA VIDEO PARAMETRI GENERALI
023000000000     C                   EXSR      DECOD1
023100000000     C*
023200000000     C                   ENDSR
023300000000     C*------------------------------------------------------------------------*
023400000000     C* DECOD1 - CONTROLLA CAMPI E DECODIFICA
023500000000     C*------------------------------------------------------------------------*
023600000000     C     DECOD1        BEGSR
023700000000     C*
023800000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
023900000000     C                   SETOFF                                       606162
024000000000     C                   SETOFF                                       010204
024100000000     C                   MOVE      *BLANKS       $PUNTO
024200000000     C*--------------------
024300000000     C* CODICE VOCE
024400000000     C*--------------------
024500000000     C                   MOVE      *BLANKS       V1CVOD
024600131121     C                   MOVE      *BLANKS       h1CRPI
024700131121     C                   MOVE      *BLANKS       h1CRPE
024800000000IF  1C     V1CVOC        IFNE      *ZERO
024900940726     C     D60OP1        OREQ      'F06'
025000940726     C                   Z-ADD     V1CVOC        KVTVOC
025100940726     C                   Z-ADD     V1CPRG        KVTPRG
025200940726     C     KEYEVT        CHAIN     ECEVT01L                           62
025300000000IF  2C     *IN62         IFEQ      *OFF
025400131121     C                   MOVEL     EVTVOD        V1CVOD
025500131121     C                   MOVEL     EVTRPI        h1CRPI
025600131121     C                   MOVEL     EVTRPE        h1CRPE
025700000000X   2C                   ELSE
025800000000     C                   MOVEL     '1'           $ERR
025900000000     C                   MOVEL     MSG(9)        $MSG
026000000000     C                   EXSR      CARMSG
026100000000E   2C                   ENDIF
026200940726     C*
026300940726     C* NEL CASO DI 'F'=FILIALE, DEVO VEDERE SE EMETTERE O NO LA SECONDA FILIALE
026400940726     C     V1CTTV        IFEQ      'F'
026500940729     C*
026600940726     C* AGGANCIO LA TABELLA 2Q CON IL RIFERIMENTO TARIFFA (ITALIA/ESTERO)
026700940726     C* E' SUFFICIENTE FARLO SOLO PER L'ITALIA, IN QUANTO I FLAG PER L'ITALIA E
026800940726     C* PER L'ESTERO DEVONO ESSERE UGUALI
026900020923     C                   MOVEL     1             KTBKUT
027000940726     C                   MOVEL     '2Q'          KTBCOD
027100940726     C                   MOVE      *BLANKS       KTBKEY
027200940726     C     EVTRFI        IFNE      *BLANKS
027300940726     C                   MOVEL     EVTRFI        KTBKEY
027400940726     C                   ELSE
027500940726     C                   MOVEL     EVTRFE        KTBKEY
027600940726     C                   ENDIF
027700940726     C     KEYTAB        CHAIN     TABEL00F                           99
027800940726IF   C     *IN99         IFEQ      '1'
027900940726     C     TBLFLG        ORNE      *BLANKS
028000940726     C                   CLEAR                   DS2Q
028100940726     C                   ELSE
028200940726     C                   MOVEL     TBLUNI        DS2Q
028300940726E    C                   ENDIF
028400940726     C     §2QFAR        COMP      *BLANKS                                66
028500940726     C                   ENDIF
028600940726     C*
028700000000E   1C                   ENDIF
028800000000     C*--------------------
028900000000     C* TIPO TARIFFA
029000000000     C*--------------------
029100000000     C                   MOVE      *BLANKS       V1CTTD
029200000000IF  1C     V1CTTV        IFNE      *BLANKS
029300940726     C     D60OP1        OREQ      'F06'
029400000000     C                   MOVEL     '2S'          §COD
029500000000     C                   MOVEL     *BLANKS       §KEY
029600000000     C                   MOVEL     V1CTTV        §KEY
029700000000     C                   EXSR      CHKTAB
029800000000     C                   MOVEL     §KEY          V1CTTV
029900000000     C                   MOVEL     §DES          V1CTTD
030000000000E   1C                   ENDIF
030100000000     C*
030200000000IF  1C     $ERR          IFEQ      '1'
030300000000     C                   MOVEL     MSG(7)        $MSG
030400000000     C                   EXSR      CARMSG
030500000000     C                   SETON                                        60
030600000000E   1C                   ENDIF
030700000000     C*--------------------
030800000000     C* DECORRENZA TARIFFA
030900000000     C*--------------------
031000000000IF  1C     $ERR          IFEQ      '0'
031100000000IF  2C     V1CDDT        IFGT      *ZEROS
031200000000     C* CORRETTA
031300000000     C                   Z-ADD     V1CDDT        G02DAT
031400000000     C                   Z-ADD     *ZEROS        G02INV
031500000000     C                   MOVEL     '0'           G02ERR
031600000000     C                   CALL      'XSRDA8'
031700000000     C                   PARM                    WLBDAT
031800000000     C*
031900000000IF  3C     G02ERR        IFEQ      '1'
032000000000     C                   MOVEL     '1'           $ERR
032100000000     C                   MOVEL     MSG(8)        $MSG
032200000000     C                   EXSR      CARMSG
032300000000     C                   SETON                                        61
032400000000     C                   ELSE
032500000000     C                   Z-ADD     G02DAT        V1CDDT
032600000000E   3C                   ENDIF
032700000000     C*
032800000000E   2C                   ENDIF
032900000000E   1C                   ENDIF
033000000000     C*
033100000000     C*--------------------
033200000000     C* SE ESEGUITA ROUTINE DI PUNTO INTERROGATIVO SIMULO ERRORE PER RIEMETTERE
033300000000     C*--------------------
033400000000IF  1C     $PUNTO        IFEQ      '1'
033500000000     C                   MOVE      '1'           $ERR
033600000000     C                   MOVE      '1'           $TESVD
033700000000E   1C                   ENDIF
033800000000     C*
033900000000     C*--------------------
034000000000     C* PARAMETRI PER TIPO TARIFFA RICHIESTO
034100000000     C*--------------------
034200000000IF  1C     $ERR          IFEQ      '0'
034300000000     C     V1CTTV        COMP      'K'                                    01
034400000000     C     V1CTTV        COMP      'F'                                    02
034500000000     C     V1CTTV        COMP      'P'                                    04
034600000000     C     V1CTTV        COMP      'T'                                    05
034700000000E   1C                   ENDIF
034800000000     C*
034900000000     C* Se scelto nessuno dei tre tipi non proseguo
035000000000     C  N01
035100000000     CANN02
035200000000     CANN03              MOVE      '1'           $ERR
035300000000     C*
035400000000     C                   ENDSR
035500000000     C*------------------------------------------------------------------------*
035600000000     C* CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
035700000000     C*          INPUT --> §COD: CODICE TABELLA
035800000000     C*                    $KEY: CODICE ELEMENTO
035900000000     C*------------------------------------------------------------------------*
036000000000     C     CHKTAB        BEGSR
036100000000     C*
036200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
036300000000     C     '?'           SCAN      §KEY                                   99
036400000000     C     *IN99         IFEQ      *ON
036500020923     C                   MOVEL     1             §KUT
036600000000     C                   MOVE      *BLANKS       §DES
036700000000     C                   CALL      'X§TABER'
036800000000     C                   PARM                    §KUT              1 0
036900000000     C                   PARM                    §COD              2
037000000000     C                   PARM                    §KEY              8
037100000000     C                   PARM                    §DES             30
037200000000     C                   MOVEL     '1'           $PUNTO            1
037300000000     C*
037400000000     C                   ENDIF
037500000000     C*
037600000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
037700020923     C                   MOVEL     1             KTBKUT
037800000000     C                   MOVEL     §COD          KTBCOD
037900000000     C                   MOVE      *BLANKS       KTBKEY
038000000000     C                   MOVEL     §KEY          KTBKEY
038100000000     C     KEYTAB        CHAIN     TABEL00F                           99
038200000000     C     *IN99         IFEQ      '1'
038300000000     C     TBLFLG        ORNE      *BLANKS
038400000000     C                   MOVEL     '1'           $ERR
038500000000     C                   MOVEL     *ALL'*'       §DES
038600000000     C                   ELSE
038700000000     C                   MOVEL     TBLUNI        §DES
038800000000     C                   ENDIF
038900000000     C*
039000000000     C*
039100000000     C                   ENDSR
039200000000     C*------------------------------------------------------------------------*
039300000000     C* CHKRIT - CONTROLLA UN ELEMENTO IN TABELLA
039400000000     C*          INPUT --> §TTV: TIPO TARIFFA
039500000000     C*                    $KEY: CODICE ELEMENTO
039600000000     C*------------------------------------------------------------------------*
039700000000     C     CHKRIT        BEGSR
039800000000     C*
039900000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
040000000000     C     '?'           SCAN      §KEY                                   99
040100000000     C     *IN99         IFEQ      *ON
040200020923     C                   MOVEL     1             §KUT
040300000000     C                   MOVE      V1CTTV        §TTV
040400000000     C                   MOVE      *BLANKS       §DES
040500000000     C                   CALL      'TREC65R'
040600000000     C                   PARM                    §KUT              1 0
040700000000     C                   PARM                    §KEY              8
040800000000     C                   PARM                    §DES             30
040900000000     C                   PARM                    §TTV              1
041000000000     C                   MOVEL     '1'           $PUNTO            1
041100000000     C*
041200000000     C                   ENDIF
041300000000     C*
041400000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
041500020923     C                   MOVEL     1             KTBKUT
041600000000     C                   MOVEL     '2Q'          KTBCOD
041700000000     C                   MOVE      *BLANKS       KTBKEY
041800000000     C                   MOVEL     §KEY          KTBKEY
041900000000     C     KEYTAB        CHAIN     TABEL00F                           99
042000000000     C     *IN99         IFEQ      '1'
042100000000     C     TBLFLG        ORNE      *BLANKS
042200000000     C                   MOVEL     '1'           $ERR
042300000000     C                   MOVEL     *ALL'*'       §DES
042400000000     C                   ELSE
042500000000     C                   MOVEL     TBLUNI        §DES
042600000000     C                   ENDIF
042700000000     C*
042800000000     C*
042900000000     C                   ENDSR
043000000000     C*------------------------------------------------------------------------*
043100000000     C* GESV2 - GESTIONE VIDEO RICHIESTA PARAMETRI TIPO TARIFFA
043200000000     C*------------------------------------------------------------------------*
043300000000     C     GESV2         BEGSR
043400000000     C*
043500000000IF  1C     $TESVD        IFEQ      '1'
043600000000     C                   EXSR      CARV2
043700000000E   1C                   ENDIF
043800000000     C*
043900000000IF  1C     MSGULT        IFEQ      *ZEROS
044000000000     C                   MOVEL     *BLANKS       $MSG
044100000000     C                   EXSR      CARMSG
044200000000E   1C                   ENDIF
044300000000     C                   EXSR      OUTMSG
044400000000     C*
044500000000     C                   EXFMT     EC62DV2
044600000000     C*
044700000000     C                   EXSR      CONFUN
044800000000     C*
044900000000IF  1C     $ERR          IFEQ      '0'
045000000000SL  2C                   SELECT
045100000000     C     KEY           WHENEQ    ENTER
045200000000     C                   EXSR      ENTV2
045300000000     C     KEY           WHENEQ    F4
045400000000     C                   EXSR      F04V2
045500000000     C     KEY           WHENEQ    F12
045600000000     C                   EXSR      F12V2
045700000000     C                   OTHER
045800000000     C                   EXSR      FUNSTD
045900000000E   2C                   ENDSL
046000000000E   1C                   ENDIF
046100000000     C*
046200000000     C                   ENDSR
046300000000     C*------------------------------------------------------------------------*
046400000000     C* CARV2 - CARICA VIDEO RICHIESTA PARAMETRI TIPO TARIFFA
046500000000     C*------------------------------------------------------------------------*
046600000000     C     CARV2         BEGSR
046700000000     C*
046800000000     C                   MOVEL     'V2'          $GES
046900000000     C*
047000000000     C                   EXSR      CARFUN
047100000000     C                   EXSR      OUTFUN
047200000000     C*
047300000000     C********             WRITEEC62DVT                    *TESTATA
047400000000     C********             WRITEEC62DVF                    *FUNZIONI
047500000000     C********             SETOF                     03
047600000000     C********             WRITEEC62DV1                    *PRIMI CAMPI
047700000000     C*
047800000000     C                   ENDSR
047900000000     C*------------------------------------------------------------------------*
048000000000     C* ENTV2 - GESTIONE TASTO ENTER PARAMETRI TIPO TARIFFA
048100000000     C*------------------------------------------------------------------------*
048200000000     C     ENTV2         BEGSR
048300000000     C*
048400000000     C* CONTROLLA I CAMPI IMMESSI
048500000000     C                   EXSR      DECOD2
048600000000     C*
048700000000     C* LANCIA IL SUBFILE DI SELEZIONE TARIFFE
048800000000IF  1C     $ERR          IFEQ      '0'
048900000000     C                   Z-ADD     V1CVOC        D60VOC
049000000000     C                   Z-ADD     V1CPRG        D60PRG
049100000000     C                   MOVEL     V1CTTV        D60TTV
049200000000     C                   MOVEL     V2CFT1        D60FT1
049300000000     C                   MOVEL     V2CFT2        D60FT2
049400000000     C                   MOVEL     V2CCLI        D60CLI
049500000000     C                   MOVEL     V2CCTR        D60CTR
049600000000     C                   MOVE      V2CCTS        D60CTS
049700131002     C                   MOVEl     V2CCAd        D60CAd
049800131002     C                   MOVEl     V2Cnaz        D60naz
049900000000     C                   Z-ADD     V1CDDT        D60DDT
050000020923     C                   Z-ADD     DUTKCI        D60KCC
050100000000     C                   MOVEL     '1'           $FINE
050200000000E   1C                   ENDIF
050300000000     C*
050400000000     C                   ENDSR
050500000000     C*------------------------------------------------------------------------*
050600000000     C* F04V2 - GESTIONE TASTO F4 = DECODIFICA
050700000000     C*------------------------------------------------------------------------*
050800000000     C     F04V2         BEGSR
050900000000     C*
051000000000     C* CARICA VIDEO PARAMETRI GENERALI
051100000000     C                   EXSR      DECOD2
051200000000     C*
051300000000     C                   ENDSR
051400000000     C*------------------------------------------------------------------------*
051500000000     C* F12V2 - GESTIONE TASTO ENTER PARAMETRI TIPO TARIFFA
051600000000     C*------------------------------------------------------------------------*
051700000000     C     F12V2         BEGSR
051800000000     C*
051900000000     C* CARICA VIDEO PARAMETRI GENERALI
052000000000     C                   EXSR      CARV1
052100000000     C*
052200000000     C                   ENDSR
052300000000     C*------------------------------------------------------------------------*
052400000000     C* DECOD2 - CONTROLLA CAMPI E DECODIFICA
052500000000     C*------------------------------------------------------------------------*
052600000000     C     DECOD2        BEGSR
052700000000     C*
052800000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
052900000000     C                   SETOFF                                       626364
053000000000     C                   SETOFF                                       656667
053100940722     C                   SETOFF                                       6810
053200000000     C                   MOVE      *BLANKS       $PUNTO
053300000000     C*
053400000000     C*?    V O C E    D I    C L I E N T E    ?
053500000000IF  1C     V1CTTV        IFEQ      'K'
053600000000     C*--------------------
053700000000     C* CODICE CLIENTE  (NON OBBLIGATORIO)
053800000000     C*--------------------
053900000000IF  2C     V2CCLI        IFNE      *BLANKS
054000000000     C     V2CCLI        ANDNE     *ZERO
054100950718     C                   SETOFF                                           99
054200950718     C     '?'           SCAN      V2CCLI                                 99
054300950718IF  3C     *IN99         IFEQ      *ON
054400950718     C                   MOVE      '1'           $PUNTO
054500020923     C                   MOVEL     DSFIRS        XALDUT           30
054600020923     C                   Z-ADD     1             XALKUT            1 0
054700950718     C                   MOVEL(P)  'A'           XALRAG           48
054800020923     C                   Z-ADD     DUTKCI        XALKCC            4 0
054900950718     C                   Z-ADD     9             XALSTA            1 0
055000980828     C                   MOVEL     *BLANKS       XALFLR           90
055100980828     C                   MOVEL     *BLANKS       XALDIT            3
055200000804     C                   Z-ADD     1             XALNUM
055300000804     C                   CALL      'XALFA3BR'
055400950718     C                   PARM                    XALDUT
055500950718     C                   PARM                    XALKUT
055600950718     C                   PARM                    XALRAG
055700950718     C                   PARM                    XALKCC
055800950718     C                   PARM                    XALSTA
055900980828     C                   PARM                    XALFLR
056000980828     C                   PARM                    XALDIT
056100000804     C                   PARM                    XALNUM            2 0
056200000804     C                   PARM                    XALKCM           80
056300000804     C                   PARM                    XALKSM          140
056400000804     C                   PARM                    XALKDM           60
056500000804     C     XALSTA        IFGT      -1
056600000804     C                   MOVEL(P)  XALRAG        V2CRAG
056700000804     C                   MOVEL(P)  XALKSM        V2CCLI
056800000804     C                   ELSE
056900000804     C                   CLEAR                   V2CCLI
057000000804     C                   ENDIF
057100950718E   3C                   ENDIF
057200950718E   2C                   ENDIF
057300000000     C*--------------------
057400000000     C* TARIFFA CLIENTE  (NON OBBLIGATORIO)
057500000000     C*--------------------
057600000000IF  2C     $ERR          IFEQ      '0'
057700000000     C                   MOVE      *BLANKS       V2CCTD
057800000000IF  3C     V2CCTR        IFNE      *BLANKS
057900000000     C     V2CCTR        ANDNE     *ZERO
058000000000     C*
058100000000     C                   MOVE      *BLANKS       V2CCTD
058200000000     C                   MOVEL     V2CCTR        $CTR              3
058300000000     C                   EXSR      CHKCTR
058400000000     C                   MOVEL     $CTR          V2CCTR
058500000000     C                   MOVEL     $CTD          V2CCTD
058600940722     C*
058700940722     C* ERRORE (CARATTERI ALFABETICI)
058800940722IF  4C     $ERR          IFEQ      '1'
058900940722     C                   MOVEL     MSG(13)       $MSG
059000940722     C                   EXSR      CARMSG
059100940722     C                   SETON                                        63
059200940722E   4C                   ENDIF
059300000000E   3C                   ENDIF
059400000000     C*
059500000000E   2C                   ENDIF
059600000000E   1C                   ENDIF
059700000000     C*
059800000000     C*?    V O C E    D I    F I L I A L E    ?
059900000000IF  1C     V1CTTV        IFEQ      'F'
060000000000     C*--------------------
060100000000     C* CODICE FILIALE/TERMINAL  (NON OBBLIGATORIO)
060200000000     C*--------------------
060300000000IF  2C     $ERR          IFEQ      '0'
060400000000     C                   MOVE      *BLANKS       V2CFD1
060500000000IF  3C     V2CFT1        IFNE      *BLANKS
060600000000     C     V2CFT1        ANDNE     *ZERO
060700000000     C*
060800000000     C                   MOVEL     *BLANKS       V2CFD1
060900000000     C     '?'           SCAN      V2CFT1                                 99
061000000000IF  4C     *IN99         IFEQ      *ON
061100971203     C                   CALL      'TNSD24R'
061200000000     C                   PARM                    §OFIL             3
061300000000     C                   PARM                    §OTIP             1
061400000000     C                   PARM                    §ODES            25
061500971203     C                   PARM                    §OPRA             3
061600000000     C                   MOVEL     §OFIL         V2CFT1
061700000000     C                   MOVE      '1'           $PUNTO
061800000000E   4C                   ENDIF
061900000000     C*
062000000000IF  4C     V2CFT1        IFGE      *ZERO
062100000000     C                   MOVEL     V2CFT1        KRGFIL
062200000000     C     KEYORG        CHAIN     AZORG01L                           64
062300000000IF  5C     *IN64         IFEQ      *ON
062400000000     C                   MOVEL     '1'           $ERR
062500000000     C                   MOVEL     MSG(14)       $MSG
062600000000     C                   EXSR      CARMSG
062700000000X   5C                   ELSE
062800000000     C                   MOVEL     ORGDES        V2CFD1
062900000000     C*
063000000000E   5C                   ENDIF
063100940729     C*
063200000000X   4C                   ELSE
063300000000     C                   MOVEL     '1'           $ERR
063400000000     C                   MOVEL     MSG(14)       $MSG
063500000000     C                   EXSR      CARMSG
063600000000     C                   SETON                                        64
063700000000E   4C                   ENDIF
063800000000E   3C                   ENDIF
063900000000E   2C                   ENDIF
064000000000     C*--------------------
064100000000     C* TERMINAL (SOLO PER TRANSITI)
064200000000     C*--------------------
064300000000IF  2C     $ERR          IFEQ      '0'
064400000000     C                   MOVEL     *BLANKS       V2CFD2
064500000000IF  3C     V2CFT2        IFNE      *BLANKS
064600000000     C     V2CFT2        ANDNE     *ZERO
064700000000     C*
064800000000     C     '?'           SCAN      V2CFT2                                 99
064900000000IF  4C     *IN99         IFEQ      *ON
065000971203     C                   CALL      'TNSD24R'
065100000000     C                   PARM                    §OFIL
065200000000     C                   PARM                    §OTIP
065300000000     C                   PARM                    §ODES
065400971203     C                   PARM                    §OPRA
065500000000     C                   MOVEL     §OFIL         V2CFT2
065600000000     C                   MOVE      '1'           $PUNTO
065700000000E   4C                   ENDIF
065800000000     C*
065900000000IF  4C     V2CFT2        IFGT      *ZERO
066000000000     C                   MOVEL     V2CFT2        KRGFIL
066100000000     C     KEYORG        CHAIN     AZORG01L                           65
066200000000IF  5C     *IN65         IFEQ      *ON
066300000000     C                   MOVEL     '1'           $ERR
066400000000     C                   MOVEL     MSG(14)       $MSG
066500000000     C                   EXSR      CARMSG
066600000000X   5C                   ELSE
066700000000     C                   MOVEL     ORGDES        V2CFD2
066800000000E   5C                   ENDIF
066900000000     C*
067000000000X   4C                   ELSE
067100000000     C                   MOVEL     '1'           $ERR
067200000000     C                   MOVEL     MSG(14)       $MSG
067300000000     C                   EXSR      CARMSG
067400000000     C                   SETON                                        65
067500000000E   4C                   ENDIF
067600000000     C*
067700000000E   3C                   ENDIF
067800000000E   2C                   ENDIF
067900000000     C*
068000000000E   1C                   ENDIF
068100000000     C*
068200000000     C*?    V O C E    D I    C. A. P.    ?
068300000000IF  1C     V1CTTV        IFEQ      'P'
068400000000     C*--------------------
068500000000     C* CODICE C.A.P.  (NON OBBLIGATORIO)
068600000000     C*--------------------
068700970627IF  2C     $ERR          IFEQ      '0'
068800131120     c                   clear                   v2dnaz
068900131121      **   essendo ITALIA definita sulla tab 15 come Blk
069000131121      **   deve sempre testare
069100131121     c**** v2cnaz        ifne      *blanks
069200131120     C                   MOVEL     1             KTBKUT
069300131120     C                   MOVEL     '15'          KTBCOD
069400131120     C                   MOVEl     v2cnaz        ktbkey
069500131120     C     KEYTAB        CHAIN     TABEL00F                           99
069600131120IF   C     *IN99         IFEQ      '1'
069700131120     C     TBLFLG        ORNE      *BLANKS
069800131120     C                   CLEAR                   DS15
069900131120     C                   ELSE
070000131120     C                   MOVEL     TBLUNI        DS15
070100131120     C                   MOVEL     §15DES        v2dnaz
070200131120E    C                   ENDIF
070300131121E    C********           ENDIF
070400131121      *
070500131121      ** Solo se ho la Voce posso vedere se il tipo di tariffa prevede
070600131121      ** un CAP generico oppure NO
070700131121E    C                   IF        v1cVOC > 0 and v1cPRG > 0
070800131121      * x Italia
070900131121E    C                   IF        v2cnaz = *blank and h1cRPI ='PP'
071000131121     c                               or
071100131121E    C                             v2cnaz <>*blank and h1cRPE ='PP'
071200131121     c*
071300131122     c                   if         %len(%trim(%subst(v2cCAD:§15pcf)))
071400131122     c                                  > §15ecf
071500131121     C                   MOVEL     '1'           $ERR
071600131121     C                   MOVEL     MSG(18)       $MSG
071700131121     C                   EXSR      CARMSG
071800131121     C                   SETON                                        67
071900131121     c                   end
072000131121     c*
072100131121E    C                   endIF
072200131121E    C                   ENDIF
072300131121      *
072400131125IF  3C**** V2CCAd        IFne      ' '
072500131002     C                   MOVEL(p)  v2cnaz        WNAz
072600131003     C                   MOVEL(P)  V2CCAD        WCAd
072700970627     C                   Z-ADD     *ZEROS        WDAT
072800970627     C                   EXSR      CHKCAP
072900970627IF  4C     $ERR          IFEQ      '1'
073000970627     C                   MOVEL     MSG(10)       $MSG
073100000000     C                   EXSR      CARMSG
073200000000     C                   SETON                                        67
073300000000E   4C                   ENDIF
073400000000E   3C                   ENDIF
073500131125E   2C***********        ENDIF
073600970627     C*
073700000000E   1C                   ENDIF
073800000000     C*
073900000000     C*?    V O C E    D I    T A S S A Z I O N E    ?
074000000000IF  1C     V1CTTV        IFEQ      'T'
074100000000     C*--------------------
074200000000     C* CODICE TASSAZIONE  (NON OBBLIGATORIO)
074300000000     C*--------------------
074400000000IF  2C     $ERR          IFEQ      '0'
074500000000     C                   MOVE      *BLANKS       V2CCT1
074600000000IF  3C     V2CCTS        IFNE      *BLANKS
074700000000     C*
074800000000     C                   MOVE      'CT'          §COD
074900000000     C                   MOVEL     *BLANKS       §KEY
075000000000     C                   MOVEL     V2CCTS        §KEY
075100000000     C                   EXSR      CHKTAB
075200000000     C                   MOVEL     §KEY          V2CCTS
075300000000     C                   MOVEL     §DES          V2CCT1
075400000000IF  4C     $ERR          IFEQ      '1'
075500000000     C                   MOVEL     MSG(15)       $MSG
075600000000     C                   EXSR      CARMSG
075700000000     C                   SETON                                        68
075800000000E   4C                   ENDIF
075900000000E   3C                   ENDIF
076000000000E   2C                   ENDIF
076100000000     C*
076200000000E   1C                   ENDIF
076300000000     C*
076400000000     C*--------------------
076500000000     C* SE ESEGUITA ROUTINE DI PUNTO INTERROGATIVO SIMULO ERRORE PER RIEMETTERE
076600000000     C*--------------------
076700000000IF  1C     $PUNTO        IFEQ      '1'
076800000000     C                   MOVE      '1'           $ERR
076900000000     C                   MOVE      '1'           $TESVD
077000000000E   1C                   ENDIF
077100000000     C*
077200000000     C                   ENDSR
077300970627     C*------------------------------------------------------------------------*
077400970627     C* CHKCAP - CONTROLLA IL CAP
077500970627     C*------------------------------------------------------------------------*
077600970627     C     CHKCAP        BEGSR
077700970627     C*
077800131002     C     *LIKE         DEFINE    I95NAR        WNAz
077900131002     C     *LIKE         DEFINE    I95CAP        WCAd
078000970627     C     *LIKE         DEFINE    I95DAT        WDAT
078100131002     C     *LIKE         DEFINE    O95VER        Wver
078200970627     C*
078300970627     C* CONTROLLA IL CAP
078400970627     C                   CLEAR                   DSSI95
078500970627     C                   MOVEL     *BLANKS       I95TLA
078600970627     C                   MOVEL     '3'           I95TCN
078700131002     C                   MOVEL     WNAz          I95NAR
078800131002     C                   MOVEL     WCAd          I95CAP
078900970627     C                   Z-ADD     WDAT          I95DAT
079000970627     C                   CALL      'TISI95R'
079100970627     C                   PARM                    DSSI95
079200970627IF  1C     O95ERR        IFNE      *BLANKS
079300131002     c* verifico se è un cap fittizio
079400131003     c     kazcpc        chain     azcpc01l
079500131003     c                   if        not %found(azcpc01l)
079600970627     C                   MOVEL     '1'           $ERR
079700970627E   1C                   ENDIF
079800131003E   1C                   ENDIF
079900970627     C*
080000970627     C                   ENDSR
080100000000     C*------------------------------------------------------------------------*
080200000000     C* CHKCTR - CONTROLLA CODICE TARIFFA
080300000000     C*          INPUT --> $CTR  : CODICE TARIFFA ALFABETICO
080400000000     C*          OUTPUT--> $ERR  : 1-ERRORE, 0-NO ERRORE
080500000000     C*------------------------------------------------------------------------*
080600000000     C     CHKCTR        BEGSR
080700000000     C*
080800000000     C     '?'           SCAN      $CTR                                   99
080900000000IF  2C     *IN99         IFEQ      *ON
081000000000     C                   CLEAR                   PARAM4
081100950719     C                   MOVEL     V2CCLI        PARCOD
081200950718     C                   MOVEL     '1'           PARID
081300020923     C                   Z-ADD     DUTKCI        PARCCC
081400000000     C                   MOVEL     PARAM4        KPJBU
081500941128     C                   CALL      'TNTA36R'
081600000000     C                   PARM                    KPJBA
081700000000     C                   MOVEL     KPJBU         PARAM4
081800000000     C*
081900000000     C                   MOVEL     PARCTR        $CTR
082000000000     C                   MOVEL     PARDCV        $CTD
082100000000     C                   MOVEL     '1'           $PUNTO
082200000000     C*
082300000000     C* INSERITA TARIFFA CONTROLLO ESISTENZA
082400000000X   2C                   ELSE
082500000000IF  3C     $CTR          IFGE      *ZEROS
082600950719     C                   MOVEL     V2CCLI        KTAKSC
082700000000     C                   MOVEL     $CTR          KTACTR
082800000000     C*
082900941128     C     KEYTAM        CHAIN     TNTAM01L                           99
083000940722     C*
083100940722     C* SE INESISTENTE VA BENE LO STESSO: EVIDENZIO PERO' LA COSA
083200000000IF  4C     *IN99         IFEQ      *ON
083300940722     C     $CTR          ANDNE     '999'
083400940722     C                   MOVEL     MSG(25)       $CTD
083500940722     C                   SETON                                        10
083600000000X   4C                   ELSE
083700000000     C     *LIKE         DEFINE    TAMDCV        $CTD
083800000000     C                   MOVEL     TAMDCV        $CTD
083900000000E   4C                   ENDIF
084000000000     C*
084100000000     C* INSERITI CARATTERI ALFABETICI, ERRORE
084200000000X   3C                   ELSE
084300000000     C                   MOVEL     '1'           $ERR
084400000000E   3C                   ENDIF
084500000000     C*
084600000000E   2C                   ENDIF
084700000000     C*
084800000000     C                   ENDSR
084900000000     C*------------------------------------------------------------------------*
085000000000     C* IMPVID - IMPOSTAZIOME INIZIALE RICHIESTA PARAMETRI GENERALI
085100000000     C*------------------------------------------------------------------------*
085200000000     C     IMPVID        BEGSR
085300000000     C*--------------------
085400000000     C* RAGIONE SOCIALE
085500000000     C*--------------------
085600000000     C*
085700000000     C                   ENDSR
085800970627     C*------------------------------------------------------------------------*
085900970627     C* FINSR - OPERAZIONI FINALI
086000970627     C*------------------------------------------------------------------------*
086100970627     C     FINSR         BEGSR
086200970627     C*
086300970704     C                   CLEAR                   DSSI95
086400970627     C                   MOVEL     'C'           I95TLA
086500970627     C                   CALL      'TISI95R'
086600970627     C                   PARM                    DSSI95
086700970627     C*
086800970627     C                   ENDSR
086900020923     C*-------------------------------------------------------------------------
087000020923     C* REPERISCE I DATI UTENTE
087100020923     C*-------------------------------------------------------------------------
087200020923     C     REPDATIUTE    BEGSR
087300020923     C*
087400020923     C* INIZIALIZZA VARIABILI DI WRK
087500020923     C                   CLEAR                   TIBS34DS
087600020923     C                   CLEAR                   AZUTEDS
087700020923     C                   CLEAR                   DDATIUTE
087800020923     C*
087900020923     C     *DTAARA       DEFINE    §azute        azuteds
088000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
088100020923     C                   IN(E)     *DTAARA
088200020923if  1C                   IF        %Error
088300020923     c                   EVAL      I34Tla = 'L'
088400020923     C                   CALL      'TIBS34R'
088500020923     C                   PARM                    Tibs34Ds
088600020923     C                   IN        *DTAARA
088700020923e   1C                   ENDIF
088800020923     C*
088900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
089000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
089100020923     C*
089200020923     C                   ENDSR
089300000000     C*------------------------------------------------------------------------*
089400000000     C* *INZSR  - ROUTINE INIZIALE
089500000000     C*------------------------------------------------------------------------*
089600000000     C     *INZSR        BEGSR
089700000000     C*
089800000000     C* RICEVIMENTO PARAMETRI
089900000000     C     *ENTRY        PLIST
090000000000     C                   PARM                    KPJBA
090100000000     C                   MOVEL     KPJBU         DS60
090200000000     C*
090300000000     C* DEFINIZIONE VARIABILI
090400000000     C     *LIKE         DEFINE    ORGFIL        KRGFIL
090500000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
090600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
090700000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
090800000000     C     *LIKE         DEFINE    TAMKSC        KTAKSC
090900000000     C     *LIKE         DEFINE    TAMCTR        KTACTR
091000940726     C     *LIKE         DEFINE    EVTVOC        KVTVOC
091100940726     C     *LIKE         DEFINE    EVTPRG        KVTPRG
091200940726     C*
091300940726     C* CHIAVE LETTURA ECEVT01L - COMPLETA
091400940726     C     KEYEVT        KLIST
091500940726     C                   KFLD                    KVTVOC
091600940726     C                   KFLD                    KVTPRG
091700131003     C* CHIAVE LETTURA azcpc01L - COMPLETA
091800131003     C     Kazcpc        KLIST
091900131003     C                   KFLD                    wver
092000131003     C                   KFLD                    wnaz
092100131003     C                   KFLD                    wcad
092200000000     C*
092300000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
092400000000     C     KEYORG        KLIST
092500000000     C                   KFLD                    KRGFIL
092600000000     C*
092700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
092800000000     C     KEYTAB        KLIST
092900000000     C                   KFLD                    KTBKUT
093000000000     C                   KFLD                    KTBCOD
093100000000     C                   KFLD                    KTBKEY
093200000000     C*
093300941128     C* CHIAVE LETTURA TNTAM01L - PARZIALE
093400000000     C     KEYTAM        KLIST
093500000000     C                   KFLD                    KTAKSC
093600000000     C                   KFLD                    KTACTR
093700000000     C*
093800000000     C* ?02 --> GESTIONE;  ?05 --> INTERROGAZIONE
093900000000     C                   MOVE      D60OP0        INTGES            2
094000000000IF  1C     INTGES        IFEQ      '02'
094100000000     C                   MOVEL     'G'           $TGES             1
094200000000X   1C                   ELSE
094300000000     C                   MOVEL     'I'           $TGES             1
094400000000E   1C                   ENDIF
094500000000     C*
094600000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
094700000000     C                   EXSR      INZSTD
094800000000     C*
094900000000     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
095000000000     C                   EXSR      IMPVID
095100000000     C*
095200000000     C* IMPOSTO I CAMPI COME PASSATI DALLA DS
095300000000     C                   Z-ADD     D60VOC        V1CVOC
095400000000     C                   Z-ADD     D60PRG        V1CPRG
095500000000     C                   MOVE      D60TTV        V1CTTV
095600000000     C                   MOVE      D60CLI        V2CCLI
095700000000     C                   MOVE      D60CTR        V2CCTR
095800000000     C                   MOVE      D60FT1        V2CFT1
095900000000     C                   MOVE      D60FT2        V2CFT2
096000000000     C                   MOVE      D60CTS        V2CCTS
096100131119     C                   MOVEl     D60naz        V2Cnaz
096200131119     C                   MOVEl     D60CAd        V2CCAd
096300000000     C                   Z-ADD     D60DDT        V1CDDT
096400000000     C*
096500000000     C* EMETTO PRIMO FORMATO VIDEO
096600000000     C                   EXSR      CARV1
096700131002     c* reperisco la versione del cappario
096800131002     C                   CLEAR                   wver
096900131002     C                   CLEAR                   DSSI95
097000131002     C                   MOVEL     '3'           I95TCN
097100131002     C                   MOVEL(p)  '40100'       I95CAP
097200131002     C                   Z-ADD     WDAT          I95DAT
097300131002     C                   CALL      'TISI95R'
097400131002     C                   PARM                    DSSI95
097500131002IF  1C     O95ERR        IFeq      *BLANKS
097600131002     C                   MOVEL     o95ver        wver
097700131002E   1C                   ENDIF
097800000000     C*
097900000000     C                   ENDSR
098000000000     C*------------------------------------------------------------------------*
098100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098300000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
098400000000     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
098500000000     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
098600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098800000000     C*------------------------------------------------------------------------*
098900000000     C* INZSTD - ROUTINE INIZIALE STANDARD
099000000000     C*------------------------------------------------------------------------*
099100000000     C     INZSTD        BEGSR
099200000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
099300000000     C                   CALL      'XKEYS'
099400000000     C                   PARM                    DSKEYS
099500000000     C                   MOVEA     KEYRIG        KEYS
099600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
099700000000     C     XMSGPA        PLIST
099800000000     C                   PARM                    DSMSMS
099900000000     C                   PARM                    DSMSPG
100000000000     C                   PARM                    DSMSKE
100100000000     C                   PARM                    DSMSER
100200000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
100300000000     C                   MOVEL     'TREC62R'     DSMSPG
100400000000     C                   MOVEL     'TREC62R'     VMCQUE
100500000000     C                   MOVEL     *BLANKS       VMCKEY
100600000000     C                   MOVEL     *BLANKS       DSMSMS
100700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
100800000000     C                   MOVEL     '0'           $ERR              1
100900000000     C                   MOVEL     '0'           $F03              1
101000000000     C                   MOVEL     *BLANKS       $GES              2
101100000000     C                   MOVEL     *BLANKS       $MSG             80
101200000000     C                   MOVEL     '0'           $FINE             1
101300000000     C                   MOVEL     '0'           $F03              1
101400000000     C                   MOVEL     '1'           $TESVD            1
101500000000     C* LANCIO ROUTINE PER VERIFICARE SE
101600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
101700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
101800020923     C                   EXSR      REPDATIUTE
101900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
102000000000     C                   BITOFF    '01234567'    POSMI1            1
102100000000     C                   BITOFF    '01234567'    POSMI2            1
102200000000     C                   BITOFF    '01234567'    POSMA1            1
102300000000     C                   BITOFF    '01234567'    POSMA2            1
102400000000     C                   BITON     '34'          POSMI1            1
102500000000     C                   BITON     '7'           POSMI2            1
102600000000     C                   BITON     '34'          POSMA1            1
102700000000     C                   BITON     '0'           POSMA2            1
102800000000     C                   MOVE      POSMI2        POSMIN            2
102900000000     C                   MOVEL     POSMI1        POSMIN
103000000000     C                   MOVE      POSMA2        POSMAX            2
103100000000     C                   MOVEL     POSMA1        POSMAX
103200000000     C                   ENDSR
103300000000     C*------------------------------------------------------------------------*
103400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
103500000000     C*------------------------------------------------------------------------*
103600000000     C     FUNSTD        BEGSR
103700000000     C                   SELECT
103800000000     C     KEY           WHENEQ    F3
103900000000     C                   EXSR      F03STD
104000000000     C     KEY           WHENEQ    F12
104100000000     C                   EXSR      F12STD
104200000000     C     KEY           WHENEQ    F24
104300000000     C                   EXSR      F24STD
104400000000     C                   OTHER
104500000000     C                   MOVEL     MSG(2)        $MSG
104600000000     C                   EXSR      CARMSG
104700000000     C                   ENDSL
104800000000     C                   ENDSR
104900000000     C*------------------------------------------------------------------------*
105000000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
105100000000     C*------------------------------------------------------------------------*
105200000000     C     F03STD        BEGSR
105300000000     C*
105400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
105500000000     C                   MOVEL     '1'           $FINE
105600000000     C                   MOVEL     '1'           $F03
105700000000     C*
105800000000     C                   ENDSR
105900000000     C*------------------------------------------------------------------------*
106000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
106100000000     C*------------------------------------------------------------------------*
106200000000     C     F12STD        BEGSR
106300000000     C*
106400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
106500000000     C                   MOVEL     '1'           $FINE
106600000000     C*
106700000000     C                   ENDSR
106800000000     C*------------------------------------------------------------------------*
106900000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
107000000000     C*------------------------------------------------------------------------*
107100000000     C     F24STD        BEGSR
107200000000     C*
107300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
107400000000     C                   EXSR      OUTFUN
107500000000     C*
107600000000     C                   ENDSR
107700000000     C*------------------------------------------------------------------------*
107800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
107900000000     C*          CODA MESSAGGI DEL PGM
108000000000     C*------------------------------------------------------------------------*
108100000000     C     CARMSG        BEGSR
108200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
108300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
108400000000     C     MSGULT        IFEQ      0
108500000000     C                   MOVEL     '1'           *IN40
108600000000     C                   WRITE     EC62DVK
108700000000     C                   MOVEL     '0'           *IN40
108800000000     C                   ENDIF
108900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
109000000000     C                   MOVEL     $MSG          DSMSMS
109100000000     C                   CALL      'XMSG'        XMSGPA
109200000000     C     DSMSER        IFEQ      0
109300000000     C                   MOVEL     DSMSKE        VMCKEY
109400000000     C                   ENDIF
109500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
109600000000     C                   ADD       1             MSGULT            4 0
109700000000     C                   WRITE     EC62DVM
109800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
109900000000     C                   MOVEL     *BLANKS       $MSG
110000000000     C                   ENDSR
110100000000     C*------------------------------------------------------------------------*
110200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
110300000000     C*------------------------------------------------------------------------*
110400000000     C     OUTMSG        BEGSR
110500000000     C* VISUALIZZAZIONE MESSAGGIO
110600000000     C                   MOVEL     '1'           *IN44
110700000000     C                   WRITE     EC62DVK
110800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
110900000000     C                   Z-ADD     *ZEROS        MSGULT
111000000000     C                   ENDSR
111100000000     C*------------------------------------------------------------------------*
111200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
111300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
111400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
111500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
111600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
111700000000     C*------------------------------------------------------------------------*
111800000000     C     CARFUN        BEGSR
111900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
112000000000     C     1             DO        31            I                 5 0
112100000000     C                   MOVEL     FUN(I)        FUNRIG
112200000000     C                   MOVEL     FUNFUN        FFU(I)
112300000000     C                   MOVEL     FUNDES        FDE(I)
112400020923     C     DUTLPO        IFEQ      'S'
112500000000     C     $TGES         IFEQ      'G'
112600000000     C                   MOVEL     FUNASG        FAT(I)
112700000000     C                   ELSE
112800000000     C                   MOVEL     FUNASI        FAT(I)
112900000000     C                   ENDIF
113000000000     C                   ELSE
113100000000     C     $TGES         IFEQ      'G'
113200000000     C                   MOVEL     FUNAFG        FAT(I)
113300000000     C                   ELSE
113400000000     C                   MOVEL     FUNAFI        FAT(I)
113500000000     C                   ENDIF
113600000000     C                   ENDIF
113700000000     C     FAT(I)        IFEQ      'E'
113800000000     C                   MOVEL     USER          WEDP              3
113900000000     C     WEDP          IFEQ      'EDP'
114000000000     C                   MOVEL     'S'           FAT(I)
114100000000     C                   ELSE
114200000000     C                   MOVEL     'N'           FAT(I)
114300000000     C                   ENDIF
114400000000     C                   ENDIF
114500000000     C                   ENDDO
114600000000     C                   ENDSR
114700000000     C*------------------------------------------------------------------------*
114800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
114900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
115000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
115100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
115200000000     C*------------------------------------------------------------------------*
115300000000     C     OUTFUN        BEGSR
115400000000     C     1             DO        8             I
115500000000     C                   MOVEL     *BLANKS       FUV(I)
115600000000     C                   ENDDO
115700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
115800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
115900000000     C     1             DO        31            I
116000000000     C     FAT(I)        IFEQ      'S'
116100000000     C     FDE(I)        ANDNE     *BLANKS
116200000000     C                   ADD       1             FUNNAT
116300000000     C                   ENDIF
116400000000     C                   ENDDO
116500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
116600000000     C     FUNNAT        IFGT      8
116700000000     C                   MOVEL     'S'           FAT(24)
116800000000     C                   ENDIF
116900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
117000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
117100000000     C                   Z-ADD     8             FUNVIS            5 0
117200000000     C     FAT(23)       IFEQ      'S'
117300000000     C                   SUB       1             FUNVIS
117400000000     C                   ENDIF
117500000000     C     FAT(24)       IFEQ      'S'
117600000000     C                   SUB       1             FUNVIS
117700000000     C                   ENDIF
117800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
117900000000     C                   Z-ADD     *ZEROS        W                 5 0
118000000000     C                   ADD       1             FUNULT
118100000000     C     FUNULT        IFGT      31
118200000000     C                   Z-ADD     1             FUNULT
118300000000     C                   ENDIF
118400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
118500000000     C     FUNULT        DO        31            I
118600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
118700000000     C     W             IFLT      FUNVIS
118800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
118900000000     C     FAT(I)        IFEQ      'S'
119000000000     C     FDE(I)        ANDNE     *BLANKS
119100000000     C     I             IFEQ      23
119200000000     C     I             OREQ      24
119300000000     C                   ELSE
119400000000     C                   ADD       1             W
119500000000     C                   MOVEL     FDE(I)        FUV(W)
119600000000     C                   ENDIF
119700000000     C                   ENDIF
119800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
119900000000     C                   Z-ADD     I             FUNULT
120000000000     C                   ENDIF
120100000000     C                   ENDDO
120200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
120300000000     C                   MOVEL     FUV(1)        VFD001
120400000000     C                   MOVEL     FUV(2)        VFD002
120500000000     C                   MOVEL     FUV(3)        VFD003
120600000000     C                   MOVEL     FUV(4)        VFD004
120700000000     C                   MOVEL     FUV(5)        VFD005
120800000000     C                   MOVEL     FUV(6)        VFD006
120900000000     C     FAT(23)       IFEQ      'N'
121000000000     C     FAT(24)       ANDEQ     'N'
121100000000     C                   MOVEL     FUV(7)        VFD007
121200000000     C                   MOVEL     FUV(8)        VFD008
121300000000     C                   ELSE
121400000000     C     FAT(23)       IFEQ      'S'
121500000000     C     FAT(24)       ANDEQ     'S'
121600000000     C                   MOVEL     FDE(23)       VFD007
121700000000     C                   MOVEL     FDE(24)       VFD008
121800000000     C                   ELSE
121900000000     C                   MOVEL     FUV(7)        VFD007
122000000000     C     FAT(23)       IFEQ      'S'
122100000000     C                   MOVEL     FDE(23)       VFD008
122200000000     C                   ELSE
122300000000     C                   MOVEL     FDE(24)       VFD008
122400000000     C                   ENDIF
122500000000     C                   ENDIF
122600000000     C                   ENDIF
122700000000     C                   MOVEL     '0'           $TESVD
122800000000     C                   ENDSR
122900000000     C*------------------------------------------------------------------------*
123000000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
123100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
123200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
123300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
123400000000     C*------------------------------------------------------------------------*
123500000000     C     CONFUN        BEGSR
123600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
123700000000     C     1             DO        31            I
123800000000     C     KEY           IFEQ      KEYS(I)
123900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
124000000000     C     FAT(I)        IFNE      'S'
124100000000     C                   MOVEL     MSG(1)        $MSG
124200000000     C                   EXSR      CARMSG
124300000000     C                   MOVEL     '1'           $ERR
124400000000     C                   ENDIF
124500000000     C                   ENDIF
124600000000     C                   ENDDO
124700000000     C                   ENDSR
124800000000**   SCHIERA: MSG  (MESSAGGI)
124900000000TREC62R- Tasto funzionale non valido.                                          1
125000000000TREC62R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
125100000000TREC62R- Opzione non valida.                                                   3
125200000000TREC62R- Opzione abilitata ma non gestita. Avvertire CED.                      4
125300000000TREC62R- Inizio elenco.                                                        5
125400000000TREC62R- Fine elenco.                                                          6
125500000000TREC62R- Immettere tipo tariffa esistente e non annullato                      7
125600000000TREC62R- Immettere data formalmente valida                                     8
125700000000TREC62R- Immettere codice voce esistente                                       9
125800000000TREC62R- Immettere codice C.A.P. esistente e non annullato                    10
125900000000TREC62R- Immettere riferimento tariffa esistente e non annullato              11
126000940722TREC62R- Immettere codice cliente esistente e non annullato                   12
126100940722TREC62R- Immettere codice tariffa numerico                                    13
126200000000TREC62R- Immettere codice filiale valido                                      14
126300000000TREC62R- Immettere codice tassazione esistente e non annullato                15
126400000000TREC62R- Immettere filiali non uguali                                         16
126500000000TREC62R- Se una filiale e' 1° livello l'altra deve appartenere all'area       17
126600131125TREC62R- Inserire correttamente un CAP generico                               18
126700000000TREC62R- 19
126800000000TREC62R- 20
126900000000TREC62R- 21
127000000000TREC62R- 22
127100000000TREC62R- 23
127200000000TREC62R- 24
127300940722* INESISTENTE *                                                               25
127400000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
127500000000F1    -                   -    NN     NN
127600000000F2    -                   -    NN     NN
127700940726F3    -F3=Fine            -    NN     NN
127800000000F4    -F4=Decodifica      -    SS     SS
127900000000F5    -                   -    NN     NN
128000000000F6    -                   -    NN     NN
128100000000F7    -                   -    NN     NN
128200000000F8    -                   -    NN     NN
128300000000F9    -                   -    NN     NN
128400000000F10   -                   -    NN     NN
128500000000F11   -                   -    NN     NN
128600000000F12   -F12=Ritorno        -    SS     SS
128700000000F13   -                   -    NN     NN
128800000000F14   -                   -    NN     NN
128900000000F15   -                   -    NN     NN
129000000000F16   -                   -    NN     NN
129100000000F17   -                   -    NN     NN
129200000000F18   -                   -    NN     NN
129300000000F19   -                   -    NN     NN
129400000000F20   -                   -    NN     NN
129500000000F21   -                   -    NN     NN
129600000000F22   -                   -    NN     NN
129700000000F23   -                   -    NN     NN
129800000000F24   -F24=Altri tasti    -    NN     NN
129900000000ENTER -                   -    SS     SS
130000000000ROLLUP-                   -    NN     NN
130100000000ROLLDN-                   -    NN     NN
130200000000HELP  -                   -    NN     NN
130300000000PRINT -                   -    NN     NN
130400000000BSPACE-                   -    NN     NN
130500000000CLEAR -                   -    NN     NN

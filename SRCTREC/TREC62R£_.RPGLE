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
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE
002000000000     D*------------------------------------------------------------------------*
002100000000     D MSG             S             78    DIM(25) CTDATA PERRCD(1)
002200000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002300000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002400000000     D FFU             S              6    DIM(31)
002500000000     D FDE             S             19    DIM(31)
002600000000     D FAT             S              1    DIM(31)
002700000000     D* TABELLA DESCRIZIONI A VIDEO
002800000000     D FUV             S             19    DIM(8)
002900000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003000000000     D KEYS            S              1    DIM(40)
003100000000     D* TABELLA CAPICONTO
003200000000     D*------------------------------------------------------------------------*
003300000000     D* INPUT
003400000000     D*------------------------------------------------------------------------*
003500000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003600000000     D DSFUN         E DS
003700000000     D  FUNRIG                 1     40
003800000000     D*------------------------------------------------------------------------*
003900000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004000000000     D DSMSG         E DS
004100000000     D*------------------------------------------------------------------------*
004200020923     D* DS REPERIMENTO DATI UTENTE
004300020923     D TIBS34DS      E DS
004400020923     D DDATIUTE      E DS
004500020923     D AZUTEDS       E DS                  extname(AZUTE00F)
004600000000     D*------------------------------------------------------------------------*
004700000000     D* DS PER GESTIONE TASTI FUNZIONALI
004800000000     D DSKEYS        E DS
004900000000     D  KEYRIG                 1     40
005000000000     D*------------------------------------------------------------------------*
005100000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005200000000     D                SDS
005300000000     D* NOME UTENTE
005400000000     D  USER                 254    263
005500000000     D*------------------------------------------------------------------------*
005600000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005700000000     D DEVDS1          DS
005800000000     D* CHIAVE FUNZIONALE BATTUTA
005900000000     D  KEY                  369    369
006000000000     D* POSIZIONE CURSORE (IN HEX)
006100000000     D  CURSOR               370    371
006200970627     D*---------------------
006300970627     D* DS "TISI95R" - CONTROLLA CAP
006400970627     D*---------------------
006500970703     D DSSI95        E DS                  EXTNAME(TISI95DS)
006600000000     D*---------------------
006700000000     D* DS ESTERNA DI PROCEDURA
006800000000     D*---------------------
006900970703     D DS60          E DS                  EXTNAME(TREC60DS)
007000000000     D*---------------------
007100000000     D* ARCHITETTURA
007200000000     D*---------------------
007300000000     D KPJBA         E DS
007400970627     D*---------------------
007500970627     D* DS "XSRDA8" - CONTROLLA DATA
007600970627     D*---------------------
007700000000     D WLBDAT          DS                  INZ
007800000000     D  G02DAT                 1      8  0
007900000000     D  G02INV                 9     16  0
008000000000     D  G02ERR                17     17
008100000000     D  G02TGI                18     22  0
008200000000     D*------------------
008300950718     D* PARAMETRI CHIAMATA 'TNTA36R'
008400000000     D*------------------
008500000000     D PARAM4          DS                  INZ
008600000000     D  PARCOD                 6     12  0
008700000000     D  PARCTR                13     15
008800000000     D  PARFLG                16     16
008900000000     D  PARID                 17     17  0
009000000000     D  PARCCC                18     21  0
009100000000     D  PARDCV                22     36
009200000000     D*------------------
009300971020     D* PARAMETRI PER RICERCA C.A.P.
009400000000     D*------------------
009500000000     D PARCAP          DS
009600000000     D  PA1OPZ                 1      2
009700000000     D  PA1CAP                 3      7
009800000000     D  PA1LOC                 8     32
009900000000     D  PA1PRV                33     34
010000000000     D  PA1LNA                35     37
010100000000     D  PA1ERR                38     38
010200000000     D  PA1MSG                39    118
010300000000     D*---------------------
010400000000     D* DS ESTERNA PER TABELLA 2Q
010500000000     D*---------------------
010600000000     D DS2Q          E DS
010700000000     C*------------------------------------------------------------------------*
010800000000     C* MAIN LINE
010900000000     C*------------------------------------------------------------------------*
011000000000     C*
011100000000     C* CICLO FINO A CHE:
011200000000     C                   WRITE     EC62DVT
011300000000DO  1C     $FINE         DOUEQ     '1'
011400000000     C*
011500000000     C* CONTROLLA GESTIONE VIDEO PARAMETRI GENERALI
011600000000     C                   MOVEL     '0'           $ERR
011700000000IF  2C     $GES          IFEQ      'V1'
011800000000     C                   EXSR      GESV1
011900000000E   2C                   ENDIF
012000000000     C*
012100000000     C* CONTROLLA GESTIONE VIDEO PARAMETRI TIPO TARIFFA
012200000000     C                   MOVEL     '0'           $ERR
012300000000IF  2C     $GES          IFEQ      'V2'
012400000000     C                   EXSR      GESV2
012500000000E   2C                   ENDIF
012600000000     C*
012700000000E   1C                   ENDDO
012800000000     C*
012900000000IF  1C******     $F03      IFEQ '1'
013000000000     C******               MOVEL'1'       D60F03
013100000000E   1C******               ENDIF
013200970627     C*
013300970627     C* OPERAZIONI FINALI
013400970627     C                   EXSR      FINSR
013500970627     C*
013600970627     C                   MOVEL     DS60          KPJBU
013700000000     C                   SETON                                        LR
013800000000     C*------------------------------------------------------------------------*
013900000000     C* GESV1 - GESTIONE VIDEO RICHIESTA PARAMETRI GENERALI
014000000000     C*------------------------------------------------------------------------*
014100000000     C     GESV1         BEGSR
014200000000     C*
014300000000IF  1C     $TESVD        IFEQ      '1'
014400000000     C                   EXSR      CARV1
014500000000E   1C                   ENDIF
014600000000     C*
014700000000IF  1C     MSGULT        IFEQ      *ZEROS
014800000000     C                   MOVEL     *BLANKS       $MSG
014900000000     C                   EXSR      CARMSG
015000000000E   1C                   ENDIF
015100000000     C                   EXSR      OUTMSG
015200000000     C*
015300000000     C                   SETON                                        03
015400000000     C                   EXFMT     EC62DV1
015500000000     C*
015600000000     C                   EXSR      CONFUN
015700000000     C*
015800000000IF  1C     $ERR          IFEQ      '0'
015900000000SL  2C                   SELECT
016000000000     C     KEY           WHENEQ    ENTER
016100000000     C                   EXSR      ENTV1
016200000000     C     KEY           WHENEQ    F4
016300000000     C                   EXSR      F04V1
016400000000     C                   OTHER
016500000000     C                   EXSR      FUNSTD
016600000000E   2C                   ENDSL
016700000000E   1C                   ENDIF
016800000000     C*
016900000000     C                   ENDSR
017000000000     C*------------------------------------------------------------------------*
017100000000     C* CARV1 - CARICA VIDEO RICHIESTA PARAMETRI GENERALI
017200000000     C*------------------------------------------------------------------------*
017300000000     C     CARV1         BEGSR
017400000000     C*
017500000000     C                   MOVEL     'V1'          $GES
017600000000     C*
017700000000     C                   EXSR      CARFUN
017800000000     C                   EXSR      OUTFUN
017900000000     C*
018000000000     C********             WRITEEC62DVT                    *TESTATA
018100000000     C********             WRITEEC62DVF                    *FUNZIONI
018200000000     C*
018300000000     C                   ENDSR
018400000000     C*------------------------------------------------------------------------*
018500000000     C* ENTV1 - GESTIONE TASTO ENTER
018600000000     C*------------------------------------------------------------------------*
018700000000     C     ENTV1         BEGSR
018800000000     C*
018900000000     C* CONTROLLA I CAMPI IMMESSI
019000000000     C                   EXSR      DECOD1
019100000000     C*
019200000000IF  1C     $ERR          IFEQ      '0'
019300000000     C*
019400000000IF  2C     V1CTTV        IFEQ      'C'
019500000000     C     V1CTTV        OREQ      *BLANKS
019600000000     C                   Z-ADD     V1CVOC        D60VOC
019700000000     C                   Z-ADD     V1CPRG        D60PRG
019800000000     C                   MOVEL     V1CTTV        D60TTV
019900000000     C                   Z-ADD     *ZERO         D60FT1
020000000000     C                   Z-ADD     *ZERO         D60FT2
020100000000     C                   Z-ADD     *ZERO         D60CLI
020200000000     C                   Z-ADD     *ZERO         D60CTR
020300000000     C                   MOVE      *BLANKS       D60CTS
020400000000     C                   Z-ADD     *ZERO         D60CAP
020500000000     C                   Z-ADD     V1CDDT        D60DDT
020600000000     C                   Z-ADD     *ZERO         D60KCC
020700000000     C                   MOVEL     '1'           $FINE
020800000000     C*
020900000000     C* CARICA VIDEO PARAMETRI TARIFFE CLIENTI/FILIALI
021000000000     C                   ELSE
021100000000     C                   MOVE      '1'           $TESVD
021200000000     C                   EXSR      CARV2
021300000000E   2C                   ENDIF
021400000000E   1C                   ENDIF
021500000000     C*
021600000000     C                   ENDSR
021700000000     C*------------------------------------------------------------------------*
021800000000     C* F04V1 - GESTIONE TASTO F4 = DECODIFICA
021900000000     C*------------------------------------------------------------------------*
022000000000     C     F04V1         BEGSR
022100000000     C*
022200000000     C* CARICA VIDEO PARAMETRI GENERALI
022300000000     C                   EXSR      DECOD1
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* DECOD1 - CONTROLLA CAMPI E DECODIFICA
022800000000     C*------------------------------------------------------------------------*
022900000000     C     DECOD1        BEGSR
023000000000     C*
023100000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
023200000000     C                   SETOFF                                       606162
023300000000     C                   SETOFF                                       010204
023400000000     C                   MOVE      *BLANKS       $PUNTO
023500000000     C*--------------------
023600000000     C* CODICE VOCE
023700000000     C*--------------------
023800000000     C                   MOVE      *BLANKS       V1CVOD
023900000000IF  1C     V1CVOC        IFNE      *ZERO
024000940726     C     D60OP1        OREQ      'F06'
024100940726     C                   Z-ADD     V1CVOC        KVTVOC
024200940726     C                   Z-ADD     V1CPRG        KVTPRG
024300940726     C     KEYEVT        CHAIN     ECEVT01L                           62
024400000000IF  2C     *IN62         IFEQ      *OFF
024500000000     C  N62              MOVEL     EVTVOD        V1CVOD
024600000000X   2C                   ELSE
024700000000     C                   MOVEL     '1'           $ERR
024800000000     C                   MOVEL     MSG(9)        $MSG
024900000000     C                   EXSR      CARMSG
025000000000E   2C                   ENDIF
025100940726     C*
025200940726     C* NEL CASO DI 'F'=FILIALE, DEVO VEDERE SE EMETTERE O NO LA SECONDA FILIALE
025300940726     C     V1CTTV        IFEQ      'F'
025400940729     C*
025500940726     C* AGGANCIO LA TABELLA 2Q CON IL RIFERIMENTO TARIFFA (ITALIA/ESTERO)
025600940726     C* E' SUFFICIENTE FARLO SOLO PER L'ITALIA, IN QUANTO I FLAG PER L'ITALIA E
025700940726     C* PER L'ESTERO DEVONO ESSERE UGUALI
025800020923     C                   MOVEL     1             KTBKUT
025900940726     C                   MOVEL     '2Q'          KTBCOD
026000940726     C                   MOVE      *BLANKS       KTBKEY
026100940726     C     EVTRFI        IFNE      *BLANKS
026200940726     C                   MOVEL     EVTRFI        KTBKEY
026300940726     C                   ELSE
026400940726     C                   MOVEL     EVTRFE        KTBKEY
026500940726     C                   ENDIF
026600940726     C     KEYTAB        CHAIN     TABEL00F                           99
026700940726IF   C     *IN99         IFEQ      '1'
026800940726     C     TBLFLG        ORNE      *BLANKS
026900940726     C                   CLEAR                   DS2Q
027000940726     C                   ELSE
027100940726     C                   MOVEL     TBLUNI        DS2Q
027200940726E    C                   ENDIF
027300940726     C     §2QFAR        COMP      *BLANKS                                66
027400940726     C                   ENDIF
027500940726     C*
027600000000E   1C                   ENDIF
027700000000     C*--------------------
027800000000     C* TIPO TARIFFA
027900000000     C*--------------------
028000000000     C                   MOVE      *BLANKS       V1CTTD
028100000000IF  1C     V1CTTV        IFNE      *BLANKS
028200940726     C     D60OP1        OREQ      'F06'
028300000000     C                   MOVEL     '2S'          §COD
028400000000     C                   MOVEL     *BLANKS       §KEY
028500000000     C                   MOVEL     V1CTTV        §KEY
028600000000     C                   EXSR      CHKTAB
028700000000     C                   MOVEL     §KEY          V1CTTV
028800000000     C                   MOVEL     §DES          V1CTTD
028900000000E   1C                   ENDIF
029000000000     C*
029100000000IF  1C     $ERR          IFEQ      '1'
029200000000     C                   MOVEL     MSG(7)        $MSG
029300000000     C                   EXSR      CARMSG
029400000000     C                   SETON                                        60
029500000000E   1C                   ENDIF
029600000000     C*--------------------
029700000000     C* DECORRENZA TARIFFA
029800000000     C*--------------------
029900000000IF  1C     $ERR          IFEQ      '0'
030000000000IF  2C     V1CDDT        IFGT      *ZEROS
030100000000     C* CORRETTA
030200000000     C                   Z-ADD     V1CDDT        G02DAT
030300000000     C                   Z-ADD     *ZEROS        G02INV
030400000000     C                   MOVEL     '0'           G02ERR
030500000000     C                   CALL      'XSRDA8'
030600000000     C                   PARM                    WLBDAT
030700000000     C*
030800000000IF  3C     G02ERR        IFEQ      '1'
030900000000     C                   MOVEL     '1'           $ERR
031000000000     C                   MOVEL     MSG(8)        $MSG
031100000000     C                   EXSR      CARMSG
031200000000     C                   SETON                                        61
031300000000     C                   ELSE
031400000000     C                   Z-ADD     G02DAT        V1CDDT
031500000000E   3C                   ENDIF
031600000000     C*
031700000000E   2C                   ENDIF
031800000000E   1C                   ENDIF
031900000000     C*
032000000000     C*--------------------
032100000000     C* SE ESEGUITA ROUTINE DI PUNTO INTERROGATIVO SIMULO ERRORE PER RIEMETTERE
032200000000     C*--------------------
032300000000IF  1C     $PUNTO        IFEQ      '1'
032400000000     C                   MOVE      '1'           $ERR
032500000000     C                   MOVE      '1'           $TESVD
032600000000E   1C                   ENDIF
032700000000     C*
032800000000     C*--------------------
032900000000     C* PARAMETRI PER TIPO TARIFFA RICHIESTO
033000000000     C*--------------------
033100000000IF  1C     $ERR          IFEQ      '0'
033200000000     C     V1CTTV        COMP      'K'                                    01
033300000000     C     V1CTTV        COMP      'F'                                    02
033400000000     C     V1CTTV        COMP      'P'                                    04
033500000000     C     V1CTTV        COMP      'T'                                    05
033600000000E   1C                   ENDIF
033700000000     C*
033800000000     C* Se scelto nessuno dei tre tipi non proseguo
033900000000     C  N01
034000000000     CANN02
034100000000     CANN03              MOVE      '1'           $ERR
034200000000     C*
034300000000     C                   ENDSR
034400000000     C*------------------------------------------------------------------------*
034500000000     C* CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
034600000000     C*          INPUT --> §COD: CODICE TABELLA
034700000000     C*                    $KEY: CODICE ELEMENTO
034800000000     C*------------------------------------------------------------------------*
034900000000     C     CHKTAB        BEGSR
035000000000     C*
035100000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
035200000000     C     '?'           SCAN      §KEY                                   99
035300000000     C     *IN99         IFEQ      *ON
035400020923     C                   MOVEL     1             §KUT
035500000000     C                   MOVE      *BLANKS       §DES
035600000000     C                   CALL      'X§TABER'
035700000000     C                   PARM                    §KUT              1 0
035800000000     C                   PARM                    §COD              2
035900000000     C                   PARM                    §KEY              8
036000000000     C                   PARM                    §DES             30
036100000000     C                   MOVEL     '1'           $PUNTO            1
036200000000     C*
036300000000     C                   ENDIF
036400000000     C*
036500000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
036600020923     C                   MOVEL     1             KTBKUT
036700000000     C                   MOVEL     §COD          KTBCOD
036800000000     C                   MOVE      *BLANKS       KTBKEY
036900000000     C                   MOVEL     §KEY          KTBKEY
037000000000     C     KEYTAB        CHAIN     TABEL00F                           99
037100000000     C     *IN99         IFEQ      '1'
037200000000     C     TBLFLG        ORNE      *BLANKS
037300000000     C                   MOVEL     '1'           $ERR
037400000000     C                   MOVEL     *ALL'*'       §DES
037500000000     C                   ELSE
037600000000     C                   MOVEL     TBLUNI        §DES
037700000000     C                   ENDIF
037800000000     C*
037900000000     C*
038000000000     C                   ENDSR
038100000000     C*------------------------------------------------------------------------*
038200000000     C* CHKRIT - CONTROLLA UN ELEMENTO IN TABELLA
038300000000     C*          INPUT --> §TTV: TIPO TARIFFA
038400000000     C*                    $KEY: CODICE ELEMENTO
038500000000     C*------------------------------------------------------------------------*
038600000000     C     CHKRIT        BEGSR
038700000000     C*
038800000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
038900000000     C     '?'           SCAN      §KEY                                   99
039000000000     C     *IN99         IFEQ      *ON
039100020923     C                   MOVEL     1             §KUT
039200000000     C                   MOVE      V1CTTV        §TTV
039300000000     C                   MOVE      *BLANKS       §DES
039400000000     C                   CALL      'TREC65R'
039500000000     C                   PARM                    §KUT              1 0
039600000000     C                   PARM                    §KEY              8
039700000000     C                   PARM                    §DES             30
039800000000     C                   PARM                    §TTV              1
039900000000     C                   MOVEL     '1'           $PUNTO            1
040000000000     C*
040100000000     C                   ENDIF
040200000000     C*
040300000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
040400020923     C                   MOVEL     1             KTBKUT
040500000000     C                   MOVEL     '2Q'          KTBCOD
040600000000     C                   MOVE      *BLANKS       KTBKEY
040700000000     C                   MOVEL     §KEY          KTBKEY
040800000000     C     KEYTAB        CHAIN     TABEL00F                           99
040900000000     C     *IN99         IFEQ      '1'
041000000000     C     TBLFLG        ORNE      *BLANKS
041100000000     C                   MOVEL     '1'           $ERR
041200000000     C                   MOVEL     *ALL'*'       §DES
041300000000     C                   ELSE
041400000000     C                   MOVEL     TBLUNI        §DES
041500000000     C                   ENDIF
041600000000     C*
041700000000     C*
041800000000     C                   ENDSR
041900000000     C*------------------------------------------------------------------------*
042000000000     C* GESV2 - GESTIONE VIDEO RICHIESTA PARAMETRI TIPO TARIFFA
042100000000     C*------------------------------------------------------------------------*
042200000000     C     GESV2         BEGSR
042300000000     C*
042400000000IF  1C     $TESVD        IFEQ      '1'
042500000000     C                   EXSR      CARV2
042600000000E   1C                   ENDIF
042700000000     C*
042800000000IF  1C     MSGULT        IFEQ      *ZEROS
042900000000     C                   MOVEL     *BLANKS       $MSG
043000000000     C                   EXSR      CARMSG
043100000000E   1C                   ENDIF
043200000000     C                   EXSR      OUTMSG
043300000000     C*
043400000000     C                   EXFMT     EC62DV2
043500000000     C*
043600000000     C                   EXSR      CONFUN
043700000000     C*
043800000000IF  1C     $ERR          IFEQ      '0'
043900000000SL  2C                   SELECT
044000000000     C     KEY           WHENEQ    ENTER
044100000000     C                   EXSR      ENTV2
044200000000     C     KEY           WHENEQ    F4
044300000000     C                   EXSR      F04V2
044400000000     C     KEY           WHENEQ    F12
044500000000     C                   EXSR      F12V2
044600000000     C                   OTHER
044700000000     C                   EXSR      FUNSTD
044800000000E   2C                   ENDSL
044900000000E   1C                   ENDIF
045000000000     C*
045100000000     C                   ENDSR
045200000000     C*------------------------------------------------------------------------*
045300000000     C* CARV2 - CARICA VIDEO RICHIESTA PARAMETRI TIPO TARIFFA
045400000000     C*------------------------------------------------------------------------*
045500000000     C     CARV2         BEGSR
045600000000     C*
045700000000     C                   MOVEL     'V2'          $GES
045800000000     C*
045900000000     C                   EXSR      CARFUN
046000000000     C                   EXSR      OUTFUN
046100000000     C*
046200000000     C********             WRITEEC62DVT                    *TESTATA
046300000000     C********             WRITEEC62DVF                    *FUNZIONI
046400000000     C********             SETOF                     03
046500000000     C********             WRITEEC62DV1                    *PRIMI CAMPI
046600000000     C*
046700000000     C                   ENDSR
046800000000     C*------------------------------------------------------------------------*
046900000000     C* ENTV2 - GESTIONE TASTO ENTER PARAMETRI TIPO TARIFFA
047000000000     C*------------------------------------------------------------------------*
047100000000     C     ENTV2         BEGSR
047200000000     C*
047300000000     C* CONTROLLA I CAMPI IMMESSI
047400000000     C                   EXSR      DECOD2
047500000000     C*
047600000000     C* LANCIA IL SUBFILE DI SELEZIONE TARIFFE
047700000000IF  1C     $ERR          IFEQ      '0'
047800000000     C                   Z-ADD     V1CVOC        D60VOC
047900000000     C                   Z-ADD     V1CPRG        D60PRG
048000000000     C                   MOVEL     V1CTTV        D60TTV
048100000000     C                   MOVEL     V2CFT1        D60FT1
048200000000     C                   MOVEL     V2CFT2        D60FT2
048300000000     C                   MOVEL     V2CCLI        D60CLI
048400000000     C                   MOVEL     V2CCTR        D60CTR
048500000000     C                   MOVE      V2CCTS        D60CTS
048600000000     C                   MOVE      V2CCAP        D60CAP
048700000000     C                   Z-ADD     V1CDDT        D60DDT
048800020923     C                   Z-ADD     DUTKCI        D60KCC
048900000000     C                   MOVEL     '1'           $FINE
049000000000E   1C                   ENDIF
049100000000     C*
049200000000     C                   ENDSR
049300000000     C*------------------------------------------------------------------------*
049400000000     C* F04V2 - GESTIONE TASTO F4 = DECODIFICA
049500000000     C*------------------------------------------------------------------------*
049600000000     C     F04V2         BEGSR
049700000000     C*
049800000000     C* CARICA VIDEO PARAMETRI GENERALI
049900000000     C                   EXSR      DECOD2
050000000000     C*
050100000000     C                   ENDSR
050200000000     C*------------------------------------------------------------------------*
050300000000     C* F12V2 - GESTIONE TASTO ENTER PARAMETRI TIPO TARIFFA
050400000000     C*------------------------------------------------------------------------*
050500000000     C     F12V2         BEGSR
050600000000     C*
050700000000     C* CARICA VIDEO PARAMETRI GENERALI
050800000000     C                   EXSR      CARV1
050900000000     C*
051000000000     C                   ENDSR
051100000000     C*------------------------------------------------------------------------*
051200000000     C* DECOD2 - CONTROLLA CAMPI E DECODIFICA
051300000000     C*------------------------------------------------------------------------*
051400000000     C     DECOD2        BEGSR
051500000000     C*
051600000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
051700000000     C                   SETOFF                                       626364
051800000000     C                   SETOFF                                       656667
051900940722     C                   SETOFF                                       6810
052000000000     C                   MOVE      *BLANKS       $PUNTO
052100000000     C*
052200000000     C*?    V O C E    D I    C L I E N T E    ?
052300000000IF  1C     V1CTTV        IFEQ      'K'
052400000000     C*--------------------
052500000000     C* CODICE CLIENTE  (NON OBBLIGATORIO)
052600000000     C*--------------------
052700000000IF  2C     V2CCLI        IFNE      *BLANKS
052800000000     C     V2CCLI        ANDNE     *ZERO
052900950719     C*---       V2CRAG    ORNE *BLANKS
053000950718     C                   SETOFF                                           99
053100950718     C     '?'           SCAN      V2CCLI                                 99
053200950718IF  3C     *IN99         IFEQ      *ON
053300950718     C                   MOVE      '1'           $PUNTO
053400020923     C                   MOVEL     DSFIRS        XALDUT           30
053500020923     C                   Z-ADD     1             XALKUT            1 0
053600950718     C                   MOVEL(P)  'A'           XALRAG           48
053700020923     C                   Z-ADD     DUTKCI        XALKCC            4 0
053800950718     C                   Z-ADD     9             XALSTA            1 0
053900980828     C                   MOVEL     *BLANKS       XALFLR           90
054000980828     C                   MOVEL     *BLANKS       XALDIT            3
054100000804     C                   Z-ADD     1             XALNUM
054200000804     C                   CALL      'XALFA3BR'
054300950718     C                   PARM                    XALDUT
054400950718     C                   PARM                    XALKUT
054500950718     C                   PARM                    XALRAG
054600950718     C                   PARM                    XALKCC
054700950718     C                   PARM                    XALSTA
054800980828     C                   PARM                    XALFLR
054900980828     C                   PARM                    XALDIT
055000000804     C                   PARM                    XALNUM            2 0
055100000804     C                   PARM                    XALKCM           80
055200000804     C                   PARM                    XALKSM          140
055300000804     C                   PARM                    XALKDM           60
055400000804     C     XALSTA        IFGT      -1
055500000804     C                   MOVEL(P)  XALRAG        V2CRAG
055600000804     C                   MOVEL(P)  XALKSM        V2CCLI
055700000804     C                   ELSE
055800000804     C                   CLEAR                   V2CCLI
055900000804     C                   ENDIF
056000950718E   3C                   ENDIF
056100950718E   2C                   ENDIF
056200000000     C*--------------------
056300000000     C* TARIFFA CLIENTE  (NON OBBLIGATORIO)
056400000000     C*--------------------
056500000000IF  2C     $ERR          IFEQ      '0'
056600000000     C                   MOVE      *BLANKS       V2CCTD
056700000000IF  3C     V2CCTR        IFNE      *BLANKS
056800000000     C     V2CCTR        ANDNE     *ZERO
056900000000     C*
057000000000     C                   MOVE      *BLANKS       V2CCTD
057100000000     C                   MOVEL     V2CCTR        $CTR              3
057200000000     C                   EXSR      CHKCTR
057300000000     C                   MOVEL     $CTR          V2CCTR
057400000000     C                   MOVEL     $CTD          V2CCTD
057500940722     C*
057600940722     C* ERRORE (CARATTERI ALFABETICI)
057700940722IF  4C     $ERR          IFEQ      '1'
057800940722     C                   MOVEL     MSG(13)       $MSG
057900940722     C                   EXSR      CARMSG
058000940722     C                   SETON                                        63
058100940722E   4C                   ENDIF
058200000000E   3C                   ENDIF
058300000000     C*
058400000000E   2C                   ENDIF
058500000000E   1C                   ENDIF
058600000000     C*
058700000000     C*?    V O C E    D I    F I L I A L E    ?
058800000000IF  1C     V1CTTV        IFEQ      'F'
058900000000     C*--------------------
059000000000     C* CODICE FILIALE/TERMINAL  (NON OBBLIGATORIO)
059100000000     C*--------------------
059200000000IF  2C     $ERR          IFEQ      '0'
059300000000     C                   MOVE      *BLANKS       V2CFD1
059400000000IF  3C     V2CFT1        IFNE      *BLANKS
059500000000     C     V2CFT1        ANDNE     *ZERO
059600000000     C*
059700000000     C                   MOVEL     *BLANKS       V2CFD1
059800000000     C     '?'           SCAN      V2CFT1                                 99
059900000000IF  4C     *IN99         IFEQ      *ON
060000971203     C                   CALL      'TNSD24R'
060100000000     C                   PARM                    §OFIL             3
060200000000     C                   PARM                    §OTIP             1
060300000000     C                   PARM                    §ODES            25
060400971203     C                   PARM                    §OPRA             3
060500000000     C                   MOVEL     §OFIL         V2CFT1
060600000000     C                   MOVE      '1'           $PUNTO
060700000000E   4C                   ENDIF
060800000000     C*
060900000000IF  4C     V2CFT1        IFGE      *ZERO
061000000000     C                   MOVEL     V2CFT1        KRGFIL
061100000000     C     KEYORG        CHAIN     AZORG01L                           64
061200000000IF  5C     *IN64         IFEQ      *ON
061300000000     C                   MOVEL     '1'           $ERR
061400000000     C                   MOVEL     MSG(14)       $MSG
061500000000     C                   EXSR      CARMSG
061600000000X   5C                   ELSE
061700000000     C                   MOVEL     ORGDES        V2CFD1
061800000000     C*
061900000000E   5C                   ENDIF
062000940729     C*
062100000000X   4C                   ELSE
062200000000     C                   MOVEL     '1'           $ERR
062300000000     C                   MOVEL     MSG(14)       $MSG
062400000000     C                   EXSR      CARMSG
062500000000     C                   SETON                                        64
062600000000E   4C                   ENDIF
062700000000E   3C                   ENDIF
062800000000E   2C                   ENDIF
062900000000     C*--------------------
063000000000     C* TERMINAL (SOLO PER TRANSITI)
063100000000     C*--------------------
063200000000IF  2C     $ERR          IFEQ      '0'
063300000000     C                   MOVEL     *BLANKS       V2CFD2
063400000000IF  3C     V2CFT2        IFNE      *BLANKS
063500000000     C     V2CFT2        ANDNE     *ZERO
063600000000     C*
063700000000     C     '?'           SCAN      V2CFT2                                 99
063800000000IF  4C     *IN99         IFEQ      *ON
063900971203     C                   CALL      'TNSD24R'
064000000000     C                   PARM                    §OFIL
064100000000     C                   PARM                    §OTIP
064200000000     C                   PARM                    §ODES
064300971203     C                   PARM                    §OPRA
064400000000     C                   MOVEL     §OFIL         V2CFT2
064500000000     C                   MOVE      '1'           $PUNTO
064600000000E   4C                   ENDIF
064700000000     C*
064800000000IF  4C     V2CFT2        IFGT      *ZERO
064900000000     C                   MOVEL     V2CFT2        KRGFIL
065000000000     C     KEYORG        CHAIN     AZORG01L                           65
065100000000IF  5C     *IN65         IFEQ      *ON
065200000000     C                   MOVEL     '1'           $ERR
065300000000     C                   MOVEL     MSG(14)       $MSG
065400000000     C                   EXSR      CARMSG
065500000000X   5C                   ELSE
065600000000     C                   MOVEL     ORGDES        V2CFD2
065700000000E   5C                   ENDIF
065800000000     C*
065900000000X   4C                   ELSE
066000000000     C                   MOVEL     '1'           $ERR
066100000000     C                   MOVEL     MSG(14)       $MSG
066200000000     C                   EXSR      CARMSG
066300000000     C                   SETON                                        65
066400000000E   4C                   ENDIF
066500000000     C*
066600000000E   3C                   ENDIF
066700000000E   2C                   ENDIF
066800000000     C*
066900000000E   1C                   ENDIF
067000000000     C*
067100000000     C*?    V O C E    D I    C. A. P.    ?
067200000000IF  1C     V1CTTV        IFEQ      'P'
067300000000     C*--------------------
067400000000     C* CODICE C.A.P.  (NON OBBLIGATORIO)
067500000000     C*--------------------
067600970627IF  2C     $ERR          IFEQ      '0'
067700970627IF  3C     V2CCAP        IFGT      *ZEROS
067800970627     C                   MOVEL     *BLANKS       WNAR
067900970627     C                   MOVEL(P)  V2CCAP        WCAP
068000970627     C                   Z-ADD     *ZEROS        WDAT
068100970627     C                   EXSR      CHKCAP
068200970627IF  4C     $ERR          IFEQ      '1'
068300970627     C                   MOVEL     MSG(10)       $MSG
068400000000     C                   EXSR      CARMSG
068500000000     C                   SETON                                        67
068600000000E   4C                   ENDIF
068700000000E   3C                   ENDIF
068800000000E   2C                   ENDIF
068900970627     C*
069000000000E   1C                   ENDIF
069100000000     C*
069200000000     C*?    V O C E    D I    T A S S A Z I O N E    ?
069300000000IF  1C     V1CTTV        IFEQ      'T'
069400000000     C*--------------------
069500000000     C* CODICE TASSAZIONE  (NON OBBLIGATORIO)
069600000000     C*--------------------
069700000000IF  2C     $ERR          IFEQ      '0'
069800000000     C                   MOVE      *BLANKS       V2CCT1
069900000000IF  3C     V2CCTS        IFNE      *BLANKS
070000000000     C*
070100000000     C                   MOVE      'CT'          §COD
070200000000     C                   MOVEL     *BLANKS       §KEY
070300000000     C                   MOVEL     V2CCTS        §KEY
070400000000     C                   EXSR      CHKTAB
070500000000     C                   MOVEL     §KEY          V2CCTS
070600000000     C                   MOVEL     §DES          V2CCT1
070700000000IF  4C     $ERR          IFEQ      '1'
070800000000     C                   MOVEL     MSG(15)       $MSG
070900000000     C                   EXSR      CARMSG
071000000000     C                   SETON                                        68
071100000000E   4C                   ENDIF
071200000000E   3C                   ENDIF
071300000000E   2C                   ENDIF
071400000000     C*
071500000000E   1C                   ENDIF
071600000000     C*
071700000000     C*--------------------
071800000000     C* SE ESEGUITA ROUTINE DI PUNTO INTERROGATIVO SIMULO ERRORE PER RIEMETTERE
071900000000     C*--------------------
072000000000IF  1C     $PUNTO        IFEQ      '1'
072100000000     C                   MOVE      '1'           $ERR
072200000000     C                   MOVE      '1'           $TESVD
072300000000E   1C                   ENDIF
072400000000     C*
072500000000     C                   ENDSR
072600970627     C*------------------------------------------------------------------------*
072700970627     C* CHKCAP - CONTROLLA IL CAP
072800970627     C*------------------------------------------------------------------------*
072900970627     C     CHKCAP        BEGSR
073000970627     C*
073100970627     C     *LIKE         DEFINE    I95NAR        WNAR
073200970627     C     *LIKE         DEFINE    I95CAP        WCAP
073300970627     C     *LIKE         DEFINE    I95DAT        WDAT
073400970627     C*
073500970627     C* CONTROLLA IL CAP
073600970627     C                   CLEAR                   DSSI95
073700970627     C                   MOVEL     *BLANKS       I95TLA
073800970627     C                   MOVEL     '3'           I95TCN
073900970627     C                   MOVEL     WNAR          I95NAR
074000970627     C                   MOVEL     WCAP          I95CAP
074100970627     C                   Z-ADD     WDAT          I95DAT
074200970627     C                   CALL      'TISI95R'
074300970627     C                   PARM                    DSSI95
074400970627IF  1C     O95ERR        IFNE      *BLANKS
074500970627     C                   MOVEL     '1'           $ERR
074600970627E   1C                   ENDIF
074700970627     C*
074800970627     C                   ENDSR
074900000000     C*------------------------------------------------------------------------*
075000000000     C* CHKCTR - CONTROLLA CODICE TARIFFA
075100000000     C*          INPUT --> $CTR  : CODICE TARIFFA ALFABETICO
075200000000     C*          OUTPUT--> $ERR  : 1-ERRORE, 0-NO ERRORE
075300000000     C*------------------------------------------------------------------------*
075400000000     C     CHKCTR        BEGSR
075500000000     C*
075600000000     C     '?'           SCAN      $CTR                                   99
075700000000IF  2C     *IN99         IFEQ      *ON
075800000000     C                   CLEAR                   PARAM4
075900950719     C                   MOVEL     V2CCLI        PARCOD
076000950718     C                   MOVEL     '1'           PARID
076100020923     C                   Z-ADD     DUTKCI        PARCCC
076200000000     C                   MOVEL     PARAM4        KPJBU
076300941128     C                   CALL      'TNTA36R'
076400000000     C                   PARM                    KPJBA
076500000000     C                   MOVEL     KPJBU         PARAM4
076600000000     C*
076700000000     C                   MOVEL     PARCTR        $CTR
076800000000     C                   MOVEL     PARDCV        $CTD
076900000000     C                   MOVEL     '1'           $PUNTO
077000000000     C*
077100000000     C* INSERITA TARIFFA CONTROLLO ESISTENZA
077200000000X   2C                   ELSE
077300000000IF  3C     $CTR          IFGE      *ZEROS
077400950719     C                   MOVEL     V2CCLI        KTAKSC
077500000000     C                   MOVEL     $CTR          KTACTR
077600000000     C*
077700941128     C     KEYTAM        CHAIN     TNTAM01L                           99
077800940722     C*
077900940722     C* SE INESISTENTE VA BENE LO STESSO: EVIDENZIO PERO' LA COSA
078000000000IF  4C     *IN99         IFEQ      *ON
078100940722     C     $CTR          ANDNE     '999'
078200940722     C                   MOVEL     MSG(25)       $CTD
078300940722     C                   SETON                                        10
078400000000X   4C                   ELSE
078500000000     C     *LIKE         DEFINE    TAMDCV        $CTD
078600000000     C                   MOVEL     TAMDCV        $CTD
078700000000E   4C                   ENDIF
078800000000     C*
078900000000     C* INSERITI CARATTERI ALFABETICI, ERRORE
079000000000X   3C                   ELSE
079100000000     C                   MOVEL     '1'           $ERR
079200000000E   3C                   ENDIF
079300000000     C*
079400000000E   2C                   ENDIF
079500000000     C*
079600000000     C                   ENDSR
079700000000     C*------------------------------------------------------------------------*
079800000000     C* IMPVID - IMPOSTAZIOME INIZIALE RICHIESTA PARAMETRI GENERALI
079900000000     C*------------------------------------------------------------------------*
080000000000     C     IMPVID        BEGSR
080100000000     C*--------------------
080200000000     C* RAGIONE SOCIALE
080300000000     C*--------------------
080400000000     C*
080500000000     C                   ENDSR
080600970627     C*------------------------------------------------------------------------*
080700970627     C* FINSR - OPERAZIONI FINALI
080800970627     C*------------------------------------------------------------------------*
080900970627     C     FINSR         BEGSR
081000970627     C*
081100970704     C                   CLEAR                   DSSI95
081200970627     C                   MOVEL     'C'           I95TLA
081300970627     C                   CALL      'TISI95R'
081400970627     C                   PARM                    DSSI95
081500970627     C*
081600970627     C                   ENDSR
081700020923     C*-------------------------------------------------------------------------
081800020923     C* REPERISCE I DATI UTENTE
081900020923     C*-------------------------------------------------------------------------
082000020923     C     REPDATIUTE    BEGSR
082100020923     C*
082200020923     C* INIZIALIZZA VARIABILI DI WRK
082300020923     C                   CLEAR                   TIBS34DS
082400020923     C                   CLEAR                   AZUTEDS
082500020923     C                   CLEAR                   DDATIUTE
082600020923     C*
082700020923     C     *DTAARA       DEFINE    §azute        azuteds
082800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
082900020923     C                   IN(E)     *DTAARA
083000020923if  1C                   IF        %Error
083100020923     c                   EVAL      I34Tla = 'L'
083200020923     C                   CALL      'TIBS34R'
083300020923     C                   PARM                    Tibs34Ds
083400020923     C                   IN        *DTAARA
083500020923e   1C                   ENDIF
083600020923     C*
083700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
083800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
083900020923     C*
084000020923     C                   ENDSR
084100000000     C*------------------------------------------------------------------------*
084200000000     C* *INZSR  - ROUTINE INIZIALE
084300000000     C*------------------------------------------------------------------------*
084400000000     C     *INZSR        BEGSR
084500000000     C*
084600000000     C* RICEVIMENTO PARAMETRI
084700000000     C     *ENTRY        PLIST
084800000000     C                   PARM                    KPJBA
084900000000     C                   MOVEL     KPJBU         DS60
085000000000     C*
085100000000     C* DEFINIZIONE VARIABILI
085200000000     C     *LIKE         DEFINE    ORGFIL        KRGFIL
085300000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
085400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
085500000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
085600000000     C     *LIKE         DEFINE    TAMKSC        KTAKSC
085700000000     C     *LIKE         DEFINE    TAMCTR        KTACTR
085800940726     C     *LIKE         DEFINE    EVTVOC        KVTVOC
085900940726     C     *LIKE         DEFINE    EVTPRG        KVTPRG
086000940726     C*
086100940726     C* CHIAVE LETTURA ECEVT01L - COMPLETA
086200940726     C     KEYEVT        KLIST
086300940726     C                   KFLD                    KVTVOC
086400940726     C                   KFLD                    KVTPRG
086500000000     C*
086600000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
086700000000     C     KEYORG        KLIST
086800000000     C                   KFLD                    KRGFIL
086900000000     C*
087000000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
087100000000     C     KEYTAB        KLIST
087200000000     C                   KFLD                    KTBKUT
087300000000     C                   KFLD                    KTBCOD
087400000000     C                   KFLD                    KTBKEY
087500000000     C*
087600941128     C* CHIAVE LETTURA TNTAM01L - PARZIALE
087700000000     C     KEYTAM        KLIST
087800000000     C                   KFLD                    KTAKSC
087900000000     C                   KFLD                    KTACTR
088000000000     C*
088100000000     C* ?02 --> GESTIONE;  ?05 --> INTERROGAZIONE
088200000000     C                   MOVE      D60OP0        INTGES            2
088300000000IF  1C     INTGES        IFEQ      '02'
088400000000     C                   MOVEL     'G'           $TGES             1
088500000000X   1C                   ELSE
088600000000     C                   MOVEL     'I'           $TGES             1
088700000000E   1C                   ENDIF
088800000000     C*
088900000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
089000000000     C                   EXSR      INZSTD
089100000000     C*
089200000000     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
089300000000     C                   EXSR      IMPVID
089400000000     C*
089500000000     C* IMPOSTO I CAMPI COME PASSATI DALLA DS
089600000000     C                   Z-ADD     D60VOC        V1CVOC
089700000000     C                   Z-ADD     D60PRG        V1CPRG
089800000000     C                   MOVE      D60TTV        V1CTTV
089900000000     C                   MOVE      D60CLI        V2CCLI
090000000000     C                   MOVE      D60CTR        V2CCTR
090100000000     C                   MOVE      D60FT1        V2CFT1
090200000000     C                   MOVE      D60FT2        V2CFT2
090300000000     C                   MOVE      D60CTS        V2CCTS
090400000000     C                   MOVE      D60CAP        V2CCAP
090500000000     C                   Z-ADD     D60DDT        V1CDDT
090600000000     C*
090700000000     C* EMETTO PRIMO FORMATO VIDEO
090800000000     C                   EXSR      CARV1
090900000000     C*
091000000000     C                   ENDSR
091100000000     C*------------------------------------------------------------------------*
091200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091400000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
091500000000     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
091600000000     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
091700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091900000000     C*------------------------------------------------------------------------*
092000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
092100000000     C*------------------------------------------------------------------------*
092200000000     C     INZSTD        BEGSR
092300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
092400000000     C                   CALL      'XKEYS'
092500000000     C                   PARM                    DSKEYS
092600000000     C                   MOVEA     KEYRIG        KEYS
092700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
092800000000     C     XMSGPA        PLIST
092900000000     C                   PARM                    DSMSMS
093000000000     C                   PARM                    DSMSPG
093100000000     C                   PARM                    DSMSKE
093200000000     C                   PARM                    DSMSER
093300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
093400000000     C                   MOVEL     'TREC62R'     DSMSPG
093500000000     C                   MOVEL     'TREC62R'     VMCQUE
093600000000     C                   MOVEL     *BLANKS       VMCKEY
093700000000     C                   MOVEL     *BLANKS       DSMSMS
093800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
093900000000     C                   MOVEL     '0'           $ERR              1
094000000000     C                   MOVEL     '0'           $F03              1
094100000000     C                   MOVEL     *BLANKS       $GES              2
094200000000     C                   MOVEL     *BLANKS       $MSG             80
094300000000     C                   MOVEL     '0'           $FINE             1
094400000000     C                   MOVEL     '0'           $F03              1
094500000000     C                   MOVEL     '1'           $TESVD            1
094600000000     C* LANCIO ROUTINE PER VERIFICARE SE
094700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
094800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
094900020923     C                   EXSR      REPDATIUTE
095000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
095100000000     C                   BITOFF    '01234567'    POSMI1            1
095200000000     C                   BITOFF    '01234567'    POSMI2            1
095300000000     C                   BITOFF    '01234567'    POSMA1            1
095400000000     C                   BITOFF    '01234567'    POSMA2            1
095500000000     C                   BITON     '34'          POSMI1            1
095600000000     C                   BITON     '7'           POSMI2            1
095700000000     C                   BITON     '34'          POSMA1            1
095800000000     C                   BITON     '0'           POSMA2            1
095900000000     C                   MOVE      POSMI2        POSMIN            2
096000000000     C                   MOVEL     POSMI1        POSMIN
096100000000     C                   MOVE      POSMA2        POSMAX            2
096200000000     C                   MOVEL     POSMA1        POSMAX
096300000000     C                   ENDSR
096400000000     C*------------------------------------------------------------------------*
096500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
096600000000     C*------------------------------------------------------------------------*
096700000000     C     FUNSTD        BEGSR
096800000000     C                   SELECT
096900000000     C     KEY           WHENEQ    F3
097000000000     C                   EXSR      F03STD
097100000000     C     KEY           WHENEQ    F12
097200000000     C                   EXSR      F12STD
097300000000     C     KEY           WHENEQ    F24
097400000000     C                   EXSR      F24STD
097500000000     C                   OTHER
097600000000     C                   MOVEL     MSG(2)        $MSG
097700000000     C                   EXSR      CARMSG
097800000000     C                   ENDSL
097900000000     C                   ENDSR
098000000000     C*------------------------------------------------------------------------*
098100000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
098200000000     C*------------------------------------------------------------------------*
098300000000     C     F03STD        BEGSR
098400000000     C*
098500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
098600000000     C                   MOVEL     '1'           $FINE
098700000000     C                   MOVEL     '1'           $F03
098800000000     C*
098900000000     C                   ENDSR
099000000000     C*------------------------------------------------------------------------*
099100000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
099200000000     C*------------------------------------------------------------------------*
099300000000     C     F12STD        BEGSR
099400000000     C*
099500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
099600000000     C                   MOVEL     '1'           $FINE
099700000000     C*
099800000000     C                   ENDSR
099900000000     C*------------------------------------------------------------------------*
100000000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
100100000000     C*------------------------------------------------------------------------*
100200000000     C     F24STD        BEGSR
100300000000     C*
100400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
100500000000     C                   EXSR      OUTFUN
100600000000     C*
100700000000     C                   ENDSR
100800000000     C*------------------------------------------------------------------------*
100900000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
101000000000     C*          CODA MESSAGGI DEL PGM
101100000000     C*------------------------------------------------------------------------*
101200000000     C     CARMSG        BEGSR
101300000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
101400000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
101500000000     C     MSGULT        IFEQ      0
101600000000     C                   MOVEL     '1'           *IN40
101700000000     C                   WRITE     EC62DVK
101800000000     C                   MOVEL     '0'           *IN40
101900000000     C                   ENDIF
102000000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
102100000000     C                   MOVEL     $MSG          DSMSMS
102200000000     C                   CALL      'XMSG'        XMSGPA
102300000000     C     DSMSER        IFEQ      0
102400000000     C                   MOVEL     DSMSKE        VMCKEY
102500000000     C                   ENDIF
102600000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
102700000000     C                   ADD       1             MSGULT            4 0
102800000000     C                   WRITE     EC62DVM
102900000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
103000000000     C                   MOVEL     *BLANKS       $MSG
103100000000     C                   ENDSR
103200000000     C*------------------------------------------------------------------------*
103300000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
103400000000     C*------------------------------------------------------------------------*
103500000000     C     OUTMSG        BEGSR
103600000000     C* VISUALIZZAZIONE MESSAGGIO
103700000000     C                   MOVEL     '1'           *IN44
103800000000     C                   WRITE     EC62DVK
103900000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
104000000000     C                   Z-ADD     *ZEROS        MSGULT
104100000000     C                   ENDSR
104200000000     C*------------------------------------------------------------------------*
104300000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
104400000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
104500000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
104600000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
104700000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
104800000000     C*------------------------------------------------------------------------*
104900000000     C     CARFUN        BEGSR
105000000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
105100000000     C     1             DO        31            I                 5 0
105200000000     C                   MOVEL     FUN(I)        FUNRIG
105300000000     C                   MOVEL     FUNFUN        FFU(I)
105400000000     C                   MOVEL     FUNDES        FDE(I)
105500020923     C     DUTLPO        IFEQ      'S'
105600000000     C     $TGES         IFEQ      'G'
105700000000     C                   MOVEL     FUNASG        FAT(I)
105800000000     C                   ELSE
105900000000     C                   MOVEL     FUNASI        FAT(I)
106000000000     C                   ENDIF
106100000000     C                   ELSE
106200000000     C     $TGES         IFEQ      'G'
106300000000     C                   MOVEL     FUNAFG        FAT(I)
106400000000     C                   ELSE
106500000000     C                   MOVEL     FUNAFI        FAT(I)
106600000000     C                   ENDIF
106700000000     C                   ENDIF
106800000000     C     FAT(I)        IFEQ      'E'
106900000000     C                   MOVEL     USER          WEDP              3
107000000000     C     WEDP          IFEQ      'EDP'
107100000000     C                   MOVEL     'S'           FAT(I)
107200000000     C                   ELSE
107300000000     C                   MOVEL     'N'           FAT(I)
107400000000     C                   ENDIF
107500000000     C                   ENDIF
107600000000     C                   ENDDO
107700000000     C                   ENDSR
107800000000     C*------------------------------------------------------------------------*
107900000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
108000000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
108100000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
108200000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
108300000000     C*------------------------------------------------------------------------*
108400000000     C     OUTFUN        BEGSR
108500000000     C     1             DO        8             I
108600000000     C                   MOVEL     *BLANKS       FUV(I)
108700000000     C                   ENDDO
108800000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
108900000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
109000000000     C     1             DO        31            I
109100000000     C     FAT(I)        IFEQ      'S'
109200000000     C     FDE(I)        ANDNE     *BLANKS
109300000000     C                   ADD       1             FUNNAT
109400000000     C                   ENDIF
109500000000     C                   ENDDO
109600000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
109700000000     C     FUNNAT        IFGT      8
109800000000     C                   MOVEL     'S'           FAT(24)
109900000000     C                   ENDIF
110000000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
110100000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
110200000000     C                   Z-ADD     8             FUNVIS            5 0
110300000000     C     FAT(23)       IFEQ      'S'
110400000000     C                   SUB       1             FUNVIS
110500000000     C                   ENDIF
110600000000     C     FAT(24)       IFEQ      'S'
110700000000     C                   SUB       1             FUNVIS
110800000000     C                   ENDIF
110900000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
111000000000     C                   Z-ADD     *ZEROS        W                 5 0
111100000000     C                   ADD       1             FUNULT
111200000000     C     FUNULT        IFGT      31
111300000000     C                   Z-ADD     1             FUNULT
111400000000     C                   ENDIF
111500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
111600000000     C     FUNULT        DO        31            I
111700000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
111800000000     C     W             IFLT      FUNVIS
111900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
112000000000     C     FAT(I)        IFEQ      'S'
112100000000     C     FDE(I)        ANDNE     *BLANKS
112200000000     C     I             IFEQ      23
112300000000     C     I             OREQ      24
112400000000     C                   ELSE
112500000000     C                   ADD       1             W
112600000000     C                   MOVEL     FDE(I)        FUV(W)
112700000000     C                   ENDIF
112800000000     C                   ENDIF
112900000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
113000000000     C                   Z-ADD     I             FUNULT
113100000000     C                   ENDIF
113200000000     C                   ENDDO
113300000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
113400000000     C                   MOVEL     FUV(1)        VFD001
113500000000     C                   MOVEL     FUV(2)        VFD002
113600000000     C                   MOVEL     FUV(3)        VFD003
113700000000     C                   MOVEL     FUV(4)        VFD004
113800000000     C                   MOVEL     FUV(5)        VFD005
113900000000     C                   MOVEL     FUV(6)        VFD006
114000000000     C     FAT(23)       IFEQ      'N'
114100000000     C     FAT(24)       ANDEQ     'N'
114200000000     C                   MOVEL     FUV(7)        VFD007
114300000000     C                   MOVEL     FUV(8)        VFD008
114400000000     C                   ELSE
114500000000     C     FAT(23)       IFEQ      'S'
114600000000     C     FAT(24)       ANDEQ     'S'
114700000000     C                   MOVEL     FDE(23)       VFD007
114800000000     C                   MOVEL     FDE(24)       VFD008
114900000000     C                   ELSE
115000000000     C                   MOVEL     FUV(7)        VFD007
115100000000     C     FAT(23)       IFEQ      'S'
115200000000     C                   MOVEL     FDE(23)       VFD008
115300000000     C                   ELSE
115400000000     C                   MOVEL     FDE(24)       VFD008
115500000000     C                   ENDIF
115600000000     C                   ENDIF
115700000000     C                   ENDIF
115800000000     C                   MOVEL     '0'           $TESVD
115900000000     C                   ENDSR
116000000000     C*------------------------------------------------------------------------*
116100000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
116200000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
116300000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
116400000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
116500000000     C*------------------------------------------------------------------------*
116600000000     C     CONFUN        BEGSR
116700000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
116800000000     C     1             DO        31            I
116900000000     C     KEY           IFEQ      KEYS(I)
117000000000     C* CONTROLLO SE IL TASTO E' ATTIVO
117100000000     C     FAT(I)        IFNE      'S'
117200000000     C                   MOVEL     MSG(1)        $MSG
117300000000     C                   EXSR      CARMSG
117400000000     C                   MOVEL     '1'           $ERR
117500000000     C                   ENDIF
117600000000     C                   ENDIF
117700000000     C                   ENDDO
117800000000     C                   ENDSR
117900000000**   SCHIERA: MSG  (MESSAGGI)
118000000000TREC62R- Tasto funzionale non valido.                                          1
118100000000TREC62R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
118200000000TREC62R- Opzione non valida.                                                   3
118300000000TREC62R- Opzione abilitata ma non gestita. Avvertire CED.                      4
118400000000TREC62R- Inizio elenco.                                                        5
118500000000TREC62R- Fine elenco.                                                          6
118600000000TREC62R- Immettere tipo tariffa esistente e non annullato                      7
118700000000TREC62R- Immettere data formalmente valida                                     8
118800000000TREC62R- Immettere codice voce esistente                                       9
118900000000TREC62R- Immettere codice C.A.P. esistente e non annullato                    10
119000000000TREC62R- Immettere riferimento tariffa esistente e non annullato              11
119100940722TREC62R- Immettere codice cliente esistente e non annullato                   12
119200940722TREC62R- Immettere codice tariffa numerico                                    13
119300000000TREC62R- Immettere codice filiale valido                                      14
119400000000TREC62R- Immettere codice tassazione esistente e non annullato                15
119500000000TREC62R- Immettere filiali non uguali                                         16
119600000000TREC62R- Se una filiale e' 1° livello l'altra deve appartenere all'area       17
119700940722TREC62R- 18
119800000000TREC62R- 19
119900000000TREC62R- 20
120000000000TREC62R- 21
120100000000TREC62R- 22
120200000000TREC62R- 23
120300000000TREC62R- 24
120400940722* INESISTENTE *                                                               25
120500000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
120600000000F1    -                   -    NN     NN
120700000000F2    -                   -    NN     NN
120800940726F3    -F3=Fine            -    NN     NN
120900000000F4    -F4=Decodifica      -    SS     SS
121000000000F5    -                   -    NN     NN
121100000000F6    -                   -    NN     NN
121200000000F7    -                   -    NN     NN
121300000000F8    -                   -    NN     NN
121400000000F9    -                   -    NN     NN
121500000000F10   -                   -    NN     NN
121600000000F11   -                   -    NN     NN
121700000000F12   -F12=Ritorno        -    SS     SS
121800000000F13   -                   -    NN     NN
121900000000F14   -                   -    NN     NN
122000000000F15   -                   -    NN     NN
122100000000F16   -                   -    NN     NN
122200000000F17   -                   -    NN     NN
122300000000F18   -                   -    NN     NN
122400000000F19   -                   -    NN     NN
122500000000F20   -                   -    NN     NN
122600000000F21   -                   -    NN     NN
122700000000F22   -                   -    NN     NN
122800000000F23   -                   -    NN     NN
122900000000F24   -F24=Altri tasti    -    NN     NN
123000000000ENTER -                   -    SS     SS
123100000000ROLLUP-                   -    NN     NN
123200000000ROLLDN-                   -    NN     NN
123300000000HELP  -                   -    NN     NN
123400000000PRINT -                   -    NN     NN
123500000000BSPACE-                   -    NN     NN
123600000000CLEAR -                   -    NN     NN

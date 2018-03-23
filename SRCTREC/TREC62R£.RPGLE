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
024600000000IF  1C     V1CVOC        IFNE      *ZERO
024700940726     C     D60OP1        OREQ      'F06'
024800940726     C                   Z-ADD     V1CVOC        KVTVOC
024900940726     C                   Z-ADD     V1CPRG        KVTPRG
025000940726     C     KEYEVT        CHAIN     ECEVT01L                           62
025100000000IF  2C     *IN62         IFEQ      *OFF
025200000000     C  N62              MOVEL     EVTVOD        V1CVOD
025300000000X   2C                   ELSE
025400000000     C                   MOVEL     '1'           $ERR
025500000000     C                   MOVEL     MSG(9)        $MSG
025600000000     C                   EXSR      CARMSG
025700000000E   2C                   ENDIF
025800940726     C*
025900940726     C* NEL CASO DI 'F'=FILIALE, DEVO VEDERE SE EMETTERE O NO LA SECONDA FILIALE
026000940726     C     V1CTTV        IFEQ      'F'
026100940729     C*
026200940726     C* AGGANCIO LA TABELLA 2Q CON IL RIFERIMENTO TARIFFA (ITALIA/ESTERO)
026300940726     C* E' SUFFICIENTE FARLO SOLO PER L'ITALIA, IN QUANTO I FLAG PER L'ITALIA E
026400940726     C* PER L'ESTERO DEVONO ESSERE UGUALI
026500020923     C                   MOVEL     1             KTBKUT
026600940726     C                   MOVEL     '2Q'          KTBCOD
026700940726     C                   MOVE      *BLANKS       KTBKEY
026800940726     C     EVTRFI        IFNE      *BLANKS
026900940726     C                   MOVEL     EVTRFI        KTBKEY
027000940726     C                   ELSE
027100940726     C                   MOVEL     EVTRFE        KTBKEY
027200940726     C                   ENDIF
027300940726     C     KEYTAB        CHAIN     TABEL00F                           99
027400940726IF   C     *IN99         IFEQ      '1'
027500940726     C     TBLFLG        ORNE      *BLANKS
027600940726     C                   CLEAR                   DS2Q
027700940726     C                   ELSE
027800940726     C                   MOVEL     TBLUNI        DS2Q
027900940726E    C                   ENDIF
028000940726     C     §2QFAR        COMP      *BLANKS                                66
028100940726     C                   ENDIF
028200940726     C*
028300000000E   1C                   ENDIF
028400000000     C*--------------------
028500000000     C* TIPO TARIFFA
028600000000     C*--------------------
028700000000     C                   MOVE      *BLANKS       V1CTTD
028800000000IF  1C     V1CTTV        IFNE      *BLANKS
028900940726     C     D60OP1        OREQ      'F06'
029000000000     C                   MOVEL     '2S'          §COD
029100000000     C                   MOVEL     *BLANKS       §KEY
029200000000     C                   MOVEL     V1CTTV        §KEY
029300000000     C                   EXSR      CHKTAB
029400000000     C                   MOVEL     §KEY          V1CTTV
029500000000     C                   MOVEL     §DES          V1CTTD
029600000000E   1C                   ENDIF
029700000000     C*
029800000000IF  1C     $ERR          IFEQ      '1'
029900000000     C                   MOVEL     MSG(7)        $MSG
030000000000     C                   EXSR      CARMSG
030100000000     C                   SETON                                        60
030200000000E   1C                   ENDIF
030300000000     C*--------------------
030400000000     C* DECORRENZA TARIFFA
030500000000     C*--------------------
030600000000IF  1C     $ERR          IFEQ      '0'
030700000000IF  2C     V1CDDT        IFGT      *ZEROS
030800000000     C* CORRETTA
030900000000     C                   Z-ADD     V1CDDT        G02DAT
031000000000     C                   Z-ADD     *ZEROS        G02INV
031100000000     C                   MOVEL     '0'           G02ERR
031200000000     C                   CALL      'XSRDA8'
031300000000     C                   PARM                    WLBDAT
031400000000     C*
031500000000IF  3C     G02ERR        IFEQ      '1'
031600000000     C                   MOVEL     '1'           $ERR
031700000000     C                   MOVEL     MSG(8)        $MSG
031800000000     C                   EXSR      CARMSG
031900000000     C                   SETON                                        61
032000000000     C                   ELSE
032100000000     C                   Z-ADD     G02DAT        V1CDDT
032200000000E   3C                   ENDIF
032300000000     C*
032400000000E   2C                   ENDIF
032500000000E   1C                   ENDIF
032600000000     C*
032700000000     C*--------------------
032800000000     C* SE ESEGUITA ROUTINE DI PUNTO INTERROGATIVO SIMULO ERRORE PER RIEMETTERE
032900000000     C*--------------------
033000000000IF  1C     $PUNTO        IFEQ      '1'
033100000000     C                   MOVE      '1'           $ERR
033200000000     C                   MOVE      '1'           $TESVD
033300000000E   1C                   ENDIF
033400000000     C*
033500000000     C*--------------------
033600000000     C* PARAMETRI PER TIPO TARIFFA RICHIESTO
033700000000     C*--------------------
033800000000IF  1C     $ERR          IFEQ      '0'
033900000000     C     V1CTTV        COMP      'K'                                    01
034000000000     C     V1CTTV        COMP      'F'                                    02
034100000000     C     V1CTTV        COMP      'P'                                    04
034200000000     C     V1CTTV        COMP      'T'                                    05
034300000000E   1C                   ENDIF
034400000000     C*
034500000000     C* Se scelto nessuno dei tre tipi non proseguo
034600000000     C  N01
034700000000     CANN02
034800000000     CANN03              MOVE      '1'           $ERR
034900000000     C*
035000000000     C                   ENDSR
035100000000     C*------------------------------------------------------------------------*
035200000000     C* CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
035300000000     C*          INPUT --> §COD: CODICE TABELLA
035400000000     C*                    $KEY: CODICE ELEMENTO
035500000000     C*------------------------------------------------------------------------*
035600000000     C     CHKTAB        BEGSR
035700000000     C*
035800000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
035900000000     C     '?'           SCAN      §KEY                                   99
036000000000     C     *IN99         IFEQ      *ON
036100020923     C                   MOVEL     1             §KUT
036200000000     C                   MOVE      *BLANKS       §DES
036300000000     C                   CALL      'X§TABER'
036400000000     C                   PARM                    §KUT              1 0
036500000000     C                   PARM                    §COD              2
036600000000     C                   PARM                    §KEY              8
036700000000     C                   PARM                    §DES             30
036800000000     C                   MOVEL     '1'           $PUNTO            1
036900000000     C*
037000000000     C                   ENDIF
037100000000     C*
037200000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
037300020923     C                   MOVEL     1             KTBKUT
037400000000     C                   MOVEL     §COD          KTBCOD
037500000000     C                   MOVE      *BLANKS       KTBKEY
037600000000     C                   MOVEL     §KEY          KTBKEY
037700000000     C     KEYTAB        CHAIN     TABEL00F                           99
037800000000     C     *IN99         IFEQ      '1'
037900000000     C     TBLFLG        ORNE      *BLANKS
038000000000     C                   MOVEL     '1'           $ERR
038100000000     C                   MOVEL     *ALL'*'       §DES
038200000000     C                   ELSE
038300000000     C                   MOVEL     TBLUNI        §DES
038400000000     C                   ENDIF
038500000000     C*
038600000000     C*
038700000000     C                   ENDSR
038800000000     C*------------------------------------------------------------------------*
038900000000     C* CHKRIT - CONTROLLA UN ELEMENTO IN TABELLA
039000000000     C*          INPUT --> §TTV: TIPO TARIFFA
039100000000     C*                    $KEY: CODICE ELEMENTO
039200000000     C*------------------------------------------------------------------------*
039300000000     C     CHKRIT        BEGSR
039400000000     C*
039500000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
039600000000     C     '?'           SCAN      §KEY                                   99
039700000000     C     *IN99         IFEQ      *ON
039800020923     C                   MOVEL     1             §KUT
039900000000     C                   MOVE      V1CTTV        §TTV
040000000000     C                   MOVE      *BLANKS       §DES
040100000000     C                   CALL      'TREC65R'
040200000000     C                   PARM                    §KUT              1 0
040300000000     C                   PARM                    §KEY              8
040400000000     C                   PARM                    §DES             30
040500000000     C                   PARM                    §TTV              1
040600000000     C                   MOVEL     '1'           $PUNTO            1
040700000000     C*
040800000000     C                   ENDIF
040900000000     C*
041000000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
041100020923     C                   MOVEL     1             KTBKUT
041200000000     C                   MOVEL     '2Q'          KTBCOD
041300000000     C                   MOVE      *BLANKS       KTBKEY
041400000000     C                   MOVEL     §KEY          KTBKEY
041500000000     C     KEYTAB        CHAIN     TABEL00F                           99
041600000000     C     *IN99         IFEQ      '1'
041700000000     C     TBLFLG        ORNE      *BLANKS
041800000000     C                   MOVEL     '1'           $ERR
041900000000     C                   MOVEL     *ALL'*'       §DES
042000000000     C                   ELSE
042100000000     C                   MOVEL     TBLUNI        §DES
042200000000     C                   ENDIF
042300000000     C*
042400000000     C*
042500000000     C                   ENDSR
042600000000     C*------------------------------------------------------------------------*
042700000000     C* GESV2 - GESTIONE VIDEO RICHIESTA PARAMETRI TIPO TARIFFA
042800000000     C*------------------------------------------------------------------------*
042900000000     C     GESV2         BEGSR
043000000000     C*
043100000000IF  1C     $TESVD        IFEQ      '1'
043200000000     C                   EXSR      CARV2
043300000000E   1C                   ENDIF
043400000000     C*
043500000000IF  1C     MSGULT        IFEQ      *ZEROS
043600000000     C                   MOVEL     *BLANKS       $MSG
043700000000     C                   EXSR      CARMSG
043800000000E   1C                   ENDIF
043900000000     C                   EXSR      OUTMSG
044000000000     C*
044100000000     C                   EXFMT     EC62DV2
044200000000     C*
044300000000     C                   EXSR      CONFUN
044400000000     C*
044500000000IF  1C     $ERR          IFEQ      '0'
044600000000SL  2C                   SELECT
044700000000     C     KEY           WHENEQ    ENTER
044800000000     C                   EXSR      ENTV2
044900000000     C     KEY           WHENEQ    F4
045000000000     C                   EXSR      F04V2
045100000000     C     KEY           WHENEQ    F12
045200000000     C                   EXSR      F12V2
045300000000     C                   OTHER
045400000000     C                   EXSR      FUNSTD
045500000000E   2C                   ENDSL
045600000000E   1C                   ENDIF
045700000000     C*
045800000000     C                   ENDSR
045900000000     C*------------------------------------------------------------------------*
046000000000     C* CARV2 - CARICA VIDEO RICHIESTA PARAMETRI TIPO TARIFFA
046100000000     C*------------------------------------------------------------------------*
046200000000     C     CARV2         BEGSR
046300000000     C*
046400000000     C                   MOVEL     'V2'          $GES
046500000000     C*
046600000000     C                   EXSR      CARFUN
046700000000     C                   EXSR      OUTFUN
046800000000     C*
046900000000     C********             WRITEEC62DVT                    *TESTATA
047000000000     C********             WRITEEC62DVF                    *FUNZIONI
047100000000     C********             SETOF                     03
047200000000     C********             WRITEEC62DV1                    *PRIMI CAMPI
047300000000     C*
047400000000     C                   ENDSR
047500000000     C*------------------------------------------------------------------------*
047600000000     C* ENTV2 - GESTIONE TASTO ENTER PARAMETRI TIPO TARIFFA
047700000000     C*------------------------------------------------------------------------*
047800000000     C     ENTV2         BEGSR
047900000000     C*
048000000000     C* CONTROLLA I CAMPI IMMESSI
048100000000     C                   EXSR      DECOD2
048200000000     C*
048300000000     C* LANCIA IL SUBFILE DI SELEZIONE TARIFFE
048400000000IF  1C     $ERR          IFEQ      '0'
048500000000     C                   Z-ADD     V1CVOC        D60VOC
048600000000     C                   Z-ADD     V1CPRG        D60PRG
048700000000     C                   MOVEL     V1CTTV        D60TTV
048800000000     C                   MOVEL     V2CFT1        D60FT1
048900000000     C                   MOVEL     V2CFT2        D60FT2
049000000000     C                   MOVEL     V2CCLI        D60CLI
049100000000     C                   MOVEL     V2CCTR        D60CTR
049200000000     C                   MOVE      V2CCTS        D60CTS
049300131002     C                   MOVEl     V2CCAd        D60CAd
049400131002     C                   MOVEl     V2Cnaz        D60naz
049500000000     C                   Z-ADD     V1CDDT        D60DDT
049600020923     C                   Z-ADD     DUTKCI        D60KCC
049700000000     C                   MOVEL     '1'           $FINE
049800000000E   1C                   ENDIF
049900000000     C*
050000000000     C                   ENDSR
050100000000     C*------------------------------------------------------------------------*
050200000000     C* F04V2 - GESTIONE TASTO F4 = DECODIFICA
050300000000     C*------------------------------------------------------------------------*
050400000000     C     F04V2         BEGSR
050500000000     C*
050600000000     C* CARICA VIDEO PARAMETRI GENERALI
050700000000     C                   EXSR      DECOD2
050800000000     C*
050900000000     C                   ENDSR
051000000000     C*------------------------------------------------------------------------*
051100000000     C* F12V2 - GESTIONE TASTO ENTER PARAMETRI TIPO TARIFFA
051200000000     C*------------------------------------------------------------------------*
051300000000     C     F12V2         BEGSR
051400000000     C*
051500000000     C* CARICA VIDEO PARAMETRI GENERALI
051600000000     C                   EXSR      CARV1
051700000000     C*
051800000000     C                   ENDSR
051900000000     C*------------------------------------------------------------------------*
052000000000     C* DECOD2 - CONTROLLA CAMPI E DECODIFICA
052100000000     C*------------------------------------------------------------------------*
052200000000     C     DECOD2        BEGSR
052300000000     C*
052400000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
052500000000     C                   SETOFF                                       626364
052600000000     C                   SETOFF                                       656667
052700940722     C                   SETOFF                                       6810
052800000000     C                   MOVE      *BLANKS       $PUNTO
052900000000     C*
053000000000     C*?    V O C E    D I    C L I E N T E    ?
053100000000IF  1C     V1CTTV        IFEQ      'K'
053200000000     C*--------------------
053300000000     C* CODICE CLIENTE  (NON OBBLIGATORIO)
053400000000     C*--------------------
053500000000IF  2C     V2CCLI        IFNE      *BLANKS
053600000000     C     V2CCLI        ANDNE     *ZERO
053700950718     C                   SETOFF                                           99
053800950718     C     '?'           SCAN      V2CCLI                                 99
053900950718IF  3C     *IN99         IFEQ      *ON
054000950718     C                   MOVE      '1'           $PUNTO
054100020923     C                   MOVEL     DSFIRS        XALDUT           30
054200020923     C                   Z-ADD     1             XALKUT            1 0
054300950718     C                   MOVEL(P)  'A'           XALRAG           48
054400020923     C                   Z-ADD     DUTKCI        XALKCC            4 0
054500950718     C                   Z-ADD     9             XALSTA            1 0
054600980828     C                   MOVEL     *BLANKS       XALFLR           90
054700980828     C                   MOVEL     *BLANKS       XALDIT            3
054800000804     C                   Z-ADD     1             XALNUM
054900000804     C                   CALL      'XALFA3BR'
055000950718     C                   PARM                    XALDUT
055100950718     C                   PARM                    XALKUT
055200950718     C                   PARM                    XALRAG
055300950718     C                   PARM                    XALKCC
055400950718     C                   PARM                    XALSTA
055500980828     C                   PARM                    XALFLR
055600980828     C                   PARM                    XALDIT
055700000804     C                   PARM                    XALNUM            2 0
055800000804     C                   PARM                    XALKCM           80
055900000804     C                   PARM                    XALKSM          140
056000000804     C                   PARM                    XALKDM           60
056100000804     C     XALSTA        IFGT      -1
056200000804     C                   MOVEL(P)  XALRAG        V2CRAG
056300000804     C                   MOVEL(P)  XALKSM        V2CCLI
056400000804     C                   ELSE
056500000804     C                   CLEAR                   V2CCLI
056600000804     C                   ENDIF
056700950718E   3C                   ENDIF
056800950718E   2C                   ENDIF
056900000000     C*--------------------
057000000000     C* TARIFFA CLIENTE  (NON OBBLIGATORIO)
057100000000     C*--------------------
057200000000IF  2C     $ERR          IFEQ      '0'
057300000000     C                   MOVE      *BLANKS       V2CCTD
057400000000IF  3C     V2CCTR        IFNE      *BLANKS
057500000000     C     V2CCTR        ANDNE     *ZERO
057600000000     C*
057700000000     C                   MOVE      *BLANKS       V2CCTD
057800000000     C                   MOVEL     V2CCTR        $CTR              3
057900000000     C                   EXSR      CHKCTR
058000000000     C                   MOVEL     $CTR          V2CCTR
058100000000     C                   MOVEL     $CTD          V2CCTD
058200940722     C*
058300940722     C* ERRORE (CARATTERI ALFABETICI)
058400940722IF  4C     $ERR          IFEQ      '1'
058500940722     C                   MOVEL     MSG(13)       $MSG
058600940722     C                   EXSR      CARMSG
058700940722     C                   SETON                                        63
058800940722E   4C                   ENDIF
058900000000E   3C                   ENDIF
059000000000     C*
059100000000E   2C                   ENDIF
059200000000E   1C                   ENDIF
059300000000     C*
059400000000     C*?    V O C E    D I    F I L I A L E    ?
059500000000IF  1C     V1CTTV        IFEQ      'F'
059600000000     C*--------------------
059700000000     C* CODICE FILIALE/TERMINAL  (NON OBBLIGATORIO)
059800000000     C*--------------------
059900000000IF  2C     $ERR          IFEQ      '0'
060000000000     C                   MOVE      *BLANKS       V2CFD1
060100000000IF  3C     V2CFT1        IFNE      *BLANKS
060200000000     C     V2CFT1        ANDNE     *ZERO
060300000000     C*
060400000000     C                   MOVEL     *BLANKS       V2CFD1
060500000000     C     '?'           SCAN      V2CFT1                                 99
060600000000IF  4C     *IN99         IFEQ      *ON
060700971203     C                   CALL      'TNSD24R'
060800000000     C                   PARM                    §OFIL             3
060900000000     C                   PARM                    §OTIP             1
061000000000     C                   PARM                    §ODES            25
061100971203     C                   PARM                    §OPRA             3
061200000000     C                   MOVEL     §OFIL         V2CFT1
061300000000     C                   MOVE      '1'           $PUNTO
061400000000E   4C                   ENDIF
061500000000     C*
061600000000IF  4C     V2CFT1        IFGE      *ZERO
061700000000     C                   MOVEL     V2CFT1        KRGFIL
061800000000     C     KEYORG        CHAIN     AZORG01L                           64
061900000000IF  5C     *IN64         IFEQ      *ON
062000000000     C                   MOVEL     '1'           $ERR
062100000000     C                   MOVEL     MSG(14)       $MSG
062200000000     C                   EXSR      CARMSG
062300000000X   5C                   ELSE
062400000000     C                   MOVEL     ORGDES        V2CFD1
062500000000     C*
062600000000E   5C                   ENDIF
062700940729     C*
062800000000X   4C                   ELSE
062900000000     C                   MOVEL     '1'           $ERR
063000000000     C                   MOVEL     MSG(14)       $MSG
063100000000     C                   EXSR      CARMSG
063200000000     C                   SETON                                        64
063300000000E   4C                   ENDIF
063400000000E   3C                   ENDIF
063500000000E   2C                   ENDIF
063600000000     C*--------------------
063700000000     C* TERMINAL (SOLO PER TRANSITI)
063800000000     C*--------------------
063900000000IF  2C     $ERR          IFEQ      '0'
064000000000     C                   MOVEL     *BLANKS       V2CFD2
064100000000IF  3C     V2CFT2        IFNE      *BLANKS
064200000000     C     V2CFT2        ANDNE     *ZERO
064300000000     C*
064400000000     C     '?'           SCAN      V2CFT2                                 99
064500000000IF  4C     *IN99         IFEQ      *ON
064600971203     C                   CALL      'TNSD24R'
064700000000     C                   PARM                    §OFIL
064800000000     C                   PARM                    §OTIP
064900000000     C                   PARM                    §ODES
065000971203     C                   PARM                    §OPRA
065100000000     C                   MOVEL     §OFIL         V2CFT2
065200000000     C                   MOVE      '1'           $PUNTO
065300000000E   4C                   ENDIF
065400000000     C*
065500000000IF  4C     V2CFT2        IFGT      *ZERO
065600000000     C                   MOVEL     V2CFT2        KRGFIL
065700000000     C     KEYORG        CHAIN     AZORG01L                           65
065800000000IF  5C     *IN65         IFEQ      *ON
065900000000     C                   MOVEL     '1'           $ERR
066000000000     C                   MOVEL     MSG(14)       $MSG
066100000000     C                   EXSR      CARMSG
066200000000X   5C                   ELSE
066300000000     C                   MOVEL     ORGDES        V2CFD2
066400000000E   5C                   ENDIF
066500000000     C*
066600000000X   4C                   ELSE
066700000000     C                   MOVEL     '1'           $ERR
066800000000     C                   MOVEL     MSG(14)       $MSG
066900000000     C                   EXSR      CARMSG
067000000000     C                   SETON                                        65
067100000000E   4C                   ENDIF
067200000000     C*
067300000000E   3C                   ENDIF
067400000000E   2C                   ENDIF
067500000000     C*
067600000000E   1C                   ENDIF
067700000000     C*
067800000000     C*?    V O C E    D I    C. A. P.    ?
067900000000IF  1C     V1CTTV        IFEQ      'P'
068000000000     C*--------------------
068100000000     C* CODICE C.A.P.  (NON OBBLIGATORIO)
068200000000     C*--------------------
068300970627IF  2C     $ERR          IFEQ      '0'
068400131120     c                   clear                   v2dnaz
068500131120     c     v2cnaz        ifne      *blanks
068600131120     C                   MOVEL     1             KTBKUT
068700131120     C                   MOVEL     '15'          KTBCOD
068800131120     C                   MOVEl     v2cnaz        ktbkey
068900131120     C     KEYTAB        CHAIN     TABEL00F                           99
069000131120IF   C     *IN99         IFEQ      '1'
069100131120     C     TBLFLG        ORNE      *BLANKS
069200131120     C                   CLEAR                   DS15
069300131120     C                   ELSE
069400131120     C                   MOVEL     TBLUNI        DS15
069500131120     C                   MOVEL     §15DES        v2dnaz
069600131120E    C                   ENDIF
069700131120E    C                   ENDIF
069800131120     c
069900131002IF  3C     V2CCAd        IFne      ' '
070000131002     C                   MOVEL(p)  v2cnaz        WNAz
070100131003     C                   MOVEL(P)  V2CCAD        WCAd
070200970627     C                   Z-ADD     *ZEROS        WDAT
070300970627     C                   EXSR      CHKCAP
070400970627IF  4C     $ERR          IFEQ      '1'
070500970627     C                   MOVEL     MSG(10)       $MSG
070600000000     C                   EXSR      CARMSG
070700000000     C                   SETON                                        67
070800000000E   4C                   ENDIF
070900000000E   3C                   ENDIF
071000000000E   2C                   ENDIF
071100970627     C*
071200000000E   1C                   ENDIF
071300000000     C*
071400000000     C*?    V O C E    D I    T A S S A Z I O N E    ?
071500000000IF  1C     V1CTTV        IFEQ      'T'
071600000000     C*--------------------
071700000000     C* CODICE TASSAZIONE  (NON OBBLIGATORIO)
071800000000     C*--------------------
071900000000IF  2C     $ERR          IFEQ      '0'
072000000000     C                   MOVE      *BLANKS       V2CCT1
072100000000IF  3C     V2CCTS        IFNE      *BLANKS
072200000000     C*
072300000000     C                   MOVE      'CT'          §COD
072400000000     C                   MOVEL     *BLANKS       §KEY
072500000000     C                   MOVEL     V2CCTS        §KEY
072600000000     C                   EXSR      CHKTAB
072700000000     C                   MOVEL     §KEY          V2CCTS
072800000000     C                   MOVEL     §DES          V2CCT1
072900000000IF  4C     $ERR          IFEQ      '1'
073000000000     C                   MOVEL     MSG(15)       $MSG
073100000000     C                   EXSR      CARMSG
073200000000     C                   SETON                                        68
073300000000E   4C                   ENDIF
073400000000E   3C                   ENDIF
073500000000E   2C                   ENDIF
073600000000     C*
073700000000E   1C                   ENDIF
073800000000     C*
073900000000     C*--------------------
074000000000     C* SE ESEGUITA ROUTINE DI PUNTO INTERROGATIVO SIMULO ERRORE PER RIEMETTERE
074100000000     C*--------------------
074200000000IF  1C     $PUNTO        IFEQ      '1'
074300000000     C                   MOVE      '1'           $ERR
074400000000     C                   MOVE      '1'           $TESVD
074500000000E   1C                   ENDIF
074600000000     C*
074700000000     C                   ENDSR
074800970627     C*------------------------------------------------------------------------*
074900970627     C* CHKCAP - CONTROLLA IL CAP
075000970627     C*------------------------------------------------------------------------*
075100970627     C     CHKCAP        BEGSR
075200970627     C*
075300131002     C     *LIKE         DEFINE    I95NAR        WNAz
075400131002     C     *LIKE         DEFINE    I95CAP        WCAd
075500970627     C     *LIKE         DEFINE    I95DAT        WDAT
075600131002     C     *LIKE         DEFINE    O95VER        Wver
075700970627     C*
075800970627     C* CONTROLLA IL CAP
075900970627     C                   CLEAR                   DSSI95
076000970627     C                   MOVEL     *BLANKS       I95TLA
076100970627     C                   MOVEL     '3'           I95TCN
076200131002     C                   MOVEL     WNAz          I95NAR
076300131002     C                   MOVEL     WCAd          I95CAP
076400970627     C                   Z-ADD     WDAT          I95DAT
076500970627     C                   CALL      'TISI95R'
076600970627     C                   PARM                    DSSI95
076700970627IF  1C     O95ERR        IFNE      *BLANKS
076800131002     c* verifico se è un cap fittizio
076900131003     c     kazcpc        chain     azcpc01l
077000131003     c                   if        not %found(azcpc01l)
077100970627     C                   MOVEL     '1'           $ERR
077200970627E   1C                   ENDIF
077300131003E   1C                   ENDIF
077400970627     C*
077500970627     C                   ENDSR
077600000000     C*------------------------------------------------------------------------*
077700000000     C* CHKCTR - CONTROLLA CODICE TARIFFA
077800000000     C*          INPUT --> $CTR  : CODICE TARIFFA ALFABETICO
077900000000     C*          OUTPUT--> $ERR  : 1-ERRORE, 0-NO ERRORE
078000000000     C*------------------------------------------------------------------------*
078100000000     C     CHKCTR        BEGSR
078200000000     C*
078300000000     C     '?'           SCAN      $CTR                                   99
078400000000IF  2C     *IN99         IFEQ      *ON
078500000000     C                   CLEAR                   PARAM4
078600950719     C                   MOVEL     V2CCLI        PARCOD
078700950718     C                   MOVEL     '1'           PARID
078800020923     C                   Z-ADD     DUTKCI        PARCCC
078900000000     C                   MOVEL     PARAM4        KPJBU
079000941128     C                   CALL      'TNTA36R'
079100000000     C                   PARM                    KPJBA
079200000000     C                   MOVEL     KPJBU         PARAM4
079300000000     C*
079400000000     C                   MOVEL     PARCTR        $CTR
079500000000     C                   MOVEL     PARDCV        $CTD
079600000000     C                   MOVEL     '1'           $PUNTO
079700000000     C*
079800000000     C* INSERITA TARIFFA CONTROLLO ESISTENZA
079900000000X   2C                   ELSE
080000000000IF  3C     $CTR          IFGE      *ZEROS
080100950719     C                   MOVEL     V2CCLI        KTAKSC
080200000000     C                   MOVEL     $CTR          KTACTR
080300000000     C*
080400941128     C     KEYTAM        CHAIN     TNTAM01L                           99
080500940722     C*
080600940722     C* SE INESISTENTE VA BENE LO STESSO: EVIDENZIO PERO' LA COSA
080700000000IF  4C     *IN99         IFEQ      *ON
080800940722     C     $CTR          ANDNE     '999'
080900940722     C                   MOVEL     MSG(25)       $CTD
081000940722     C                   SETON                                        10
081100000000X   4C                   ELSE
081200000000     C     *LIKE         DEFINE    TAMDCV        $CTD
081300000000     C                   MOVEL     TAMDCV        $CTD
081400000000E   4C                   ENDIF
081500000000     C*
081600000000     C* INSERITI CARATTERI ALFABETICI, ERRORE
081700000000X   3C                   ELSE
081800000000     C                   MOVEL     '1'           $ERR
081900000000E   3C                   ENDIF
082000000000     C*
082100000000E   2C                   ENDIF
082200000000     C*
082300000000     C                   ENDSR
082400000000     C*------------------------------------------------------------------------*
082500000000     C* IMPVID - IMPOSTAZIOME INIZIALE RICHIESTA PARAMETRI GENERALI
082600000000     C*------------------------------------------------------------------------*
082700000000     C     IMPVID        BEGSR
082800000000     C*--------------------
082900000000     C* RAGIONE SOCIALE
083000000000     C*--------------------
083100000000     C*
083200000000     C                   ENDSR
083300970627     C*------------------------------------------------------------------------*
083400970627     C* FINSR - OPERAZIONI FINALI
083500970627     C*------------------------------------------------------------------------*
083600970627     C     FINSR         BEGSR
083700970627     C*
083800970704     C                   CLEAR                   DSSI95
083900970627     C                   MOVEL     'C'           I95TLA
084000970627     C                   CALL      'TISI95R'
084100970627     C                   PARM                    DSSI95
084200970627     C*
084300970627     C                   ENDSR
084400020923     C*-------------------------------------------------------------------------
084500020923     C* REPERISCE I DATI UTENTE
084600020923     C*-------------------------------------------------------------------------
084700020923     C     REPDATIUTE    BEGSR
084800020923     C*
084900020923     C* INIZIALIZZA VARIABILI DI WRK
085000020923     C                   CLEAR                   TIBS34DS
085100020923     C                   CLEAR                   AZUTEDS
085200020923     C                   CLEAR                   DDATIUTE
085300020923     C*
085400020923     C     *DTAARA       DEFINE    §azute        azuteds
085500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
085600020923     C                   IN(E)     *DTAARA
085700020923if  1C                   IF        %Error
085800020923     c                   EVAL      I34Tla = 'L'
085900020923     C                   CALL      'TIBS34R'
086000020923     C                   PARM                    Tibs34Ds
086100020923     C                   IN        *DTAARA
086200020923e   1C                   ENDIF
086300020923     C*
086400020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
086500020923     C                   MOVEL(P)  RSUT          DSFIRS           20
086600020923     C*
086700020923     C                   ENDSR
086800000000     C*------------------------------------------------------------------------*
086900000000     C* *INZSR  - ROUTINE INIZIALE
087000000000     C*------------------------------------------------------------------------*
087100000000     C     *INZSR        BEGSR
087200000000     C*
087300000000     C* RICEVIMENTO PARAMETRI
087400000000     C     *ENTRY        PLIST
087500000000     C                   PARM                    KPJBA
087600000000     C                   MOVEL     KPJBU         DS60
087700000000     C*
087800000000     C* DEFINIZIONE VARIABILI
087900000000     C     *LIKE         DEFINE    ORGFIL        KRGFIL
088000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
088100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
088200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
088300000000     C     *LIKE         DEFINE    TAMKSC        KTAKSC
088400000000     C     *LIKE         DEFINE    TAMCTR        KTACTR
088500940726     C     *LIKE         DEFINE    EVTVOC        KVTVOC
088600940726     C     *LIKE         DEFINE    EVTPRG        KVTPRG
088700940726     C*
088800940726     C* CHIAVE LETTURA ECEVT01L - COMPLETA
088900940726     C     KEYEVT        KLIST
089000940726     C                   KFLD                    KVTVOC
089100940726     C                   KFLD                    KVTPRG
089200131003     C* CHIAVE LETTURA azcpc01L - COMPLETA
089300131003     C     Kazcpc        KLIST
089400131003     C                   KFLD                    wver
089500131003     C                   KFLD                    wnaz
089600131003     C                   KFLD                    wcad
089700000000     C*
089800000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
089900000000     C     KEYORG        KLIST
090000000000     C                   KFLD                    KRGFIL
090100000000     C*
090200000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
090300000000     C     KEYTAB        KLIST
090400000000     C                   KFLD                    KTBKUT
090500000000     C                   KFLD                    KTBCOD
090600000000     C                   KFLD                    KTBKEY
090700000000     C*
090800941128     C* CHIAVE LETTURA TNTAM01L - PARZIALE
090900000000     C     KEYTAM        KLIST
091000000000     C                   KFLD                    KTAKSC
091100000000     C                   KFLD                    KTACTR
091200000000     C*
091300000000     C* ?02 --> GESTIONE;  ?05 --> INTERROGAZIONE
091400000000     C                   MOVE      D60OP0        INTGES            2
091500000000IF  1C     INTGES        IFEQ      '02'
091600000000     C                   MOVEL     'G'           $TGES             1
091700000000X   1C                   ELSE
091800000000     C                   MOVEL     'I'           $TGES             1
091900000000E   1C                   ENDIF
092000000000     C*
092100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
092200000000     C                   EXSR      INZSTD
092300000000     C*
092400000000     C* IMPOSTAZIONI INIZIALI VIDEO PARAMETRI GENERALI
092500000000     C                   EXSR      IMPVID
092600000000     C*
092700000000     C* IMPOSTO I CAMPI COME PASSATI DALLA DS
092800000000     C                   Z-ADD     D60VOC        V1CVOC
092900000000     C                   Z-ADD     D60PRG        V1CPRG
093000000000     C                   MOVE      D60TTV        V1CTTV
093100000000     C                   MOVE      D60CLI        V2CCLI
093200000000     C                   MOVE      D60CTR        V2CCTR
093300000000     C                   MOVE      D60FT1        V2CFT1
093400000000     C                   MOVE      D60FT2        V2CFT2
093500000000     C                   MOVE      D60CTS        V2CCTS
093600131119     C                   MOVEl     D60naz        V2Cnaz
093700131119     C                   MOVEl     D60CAd        V2CCAd
093800000000     C                   Z-ADD     D60DDT        V1CDDT
093900000000     C*
094000000000     C* EMETTO PRIMO FORMATO VIDEO
094100000000     C                   EXSR      CARV1
094200131002     c* reperisco la versione del cappario
094300131002     C                   CLEAR                   wver
094400131002     C                   CLEAR                   DSSI95
094500131002     C                   MOVEL     '3'           I95TCN
094600131002     C                   MOVEL(p)  '40100'       I95CAP
094700131002     C                   Z-ADD     WDAT          I95DAT
094800131002     C                   CALL      'TISI95R'
094900131002     C                   PARM                    DSSI95
095000131002IF  1C     O95ERR        IFeq      *BLANKS
095100131002     C                   MOVEL     o95ver        wver
095200131002E   1C                   ENDIF
095300000000     C*
095400000000     C                   ENDSR
095500000000     C*------------------------------------------------------------------------*
095600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095800000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
095900000000     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
096000000000     C* MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
096100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
096200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
096300000000     C*------------------------------------------------------------------------*
096400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
096500000000     C*------------------------------------------------------------------------*
096600000000     C     INZSTD        BEGSR
096700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
096800000000     C                   CALL      'XKEYS'
096900000000     C                   PARM                    DSKEYS
097000000000     C                   MOVEA     KEYRIG        KEYS
097100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
097200000000     C     XMSGPA        PLIST
097300000000     C                   PARM                    DSMSMS
097400000000     C                   PARM                    DSMSPG
097500000000     C                   PARM                    DSMSKE
097600000000     C                   PARM                    DSMSER
097700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
097800000000     C                   MOVEL     'TREC62R'     DSMSPG
097900000000     C                   MOVEL     'TREC62R'     VMCQUE
098000000000     C                   MOVEL     *BLANKS       VMCKEY
098100000000     C                   MOVEL     *BLANKS       DSMSMS
098200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
098300000000     C                   MOVEL     '0'           $ERR              1
098400000000     C                   MOVEL     '0'           $F03              1
098500000000     C                   MOVEL     *BLANKS       $GES              2
098600000000     C                   MOVEL     *BLANKS       $MSG             80
098700000000     C                   MOVEL     '0'           $FINE             1
098800000000     C                   MOVEL     '0'           $F03              1
098900000000     C                   MOVEL     '1'           $TESVD            1
099000000000     C* LANCIO ROUTINE PER VERIFICARE SE
099100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
099200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
099300020923     C                   EXSR      REPDATIUTE
099400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
099500000000     C                   BITOFF    '01234567'    POSMI1            1
099600000000     C                   BITOFF    '01234567'    POSMI2            1
099700000000     C                   BITOFF    '01234567'    POSMA1            1
099800000000     C                   BITOFF    '01234567'    POSMA2            1
099900000000     C                   BITON     '34'          POSMI1            1
100000000000     C                   BITON     '7'           POSMI2            1
100100000000     C                   BITON     '34'          POSMA1            1
100200000000     C                   BITON     '0'           POSMA2            1
100300000000     C                   MOVE      POSMI2        POSMIN            2
100400000000     C                   MOVEL     POSMI1        POSMIN
100500000000     C                   MOVE      POSMA2        POSMAX            2
100600000000     C                   MOVEL     POSMA1        POSMAX
100700000000     C                   ENDSR
100800000000     C*------------------------------------------------------------------------*
100900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
101000000000     C*------------------------------------------------------------------------*
101100000000     C     FUNSTD        BEGSR
101200000000     C                   SELECT
101300000000     C     KEY           WHENEQ    F3
101400000000     C                   EXSR      F03STD
101500000000     C     KEY           WHENEQ    F12
101600000000     C                   EXSR      F12STD
101700000000     C     KEY           WHENEQ    F24
101800000000     C                   EXSR      F24STD
101900000000     C                   OTHER
102000000000     C                   MOVEL     MSG(2)        $MSG
102100000000     C                   EXSR      CARMSG
102200000000     C                   ENDSL
102300000000     C                   ENDSR
102400000000     C*------------------------------------------------------------------------*
102500000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
102600000000     C*------------------------------------------------------------------------*
102700000000     C     F03STD        BEGSR
102800000000     C*
102900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
103000000000     C                   MOVEL     '1'           $FINE
103100000000     C                   MOVEL     '1'           $F03
103200000000     C*
103300000000     C                   ENDSR
103400000000     C*------------------------------------------------------------------------*
103500000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
103600000000     C*------------------------------------------------------------------------*
103700000000     C     F12STD        BEGSR
103800000000     C*
103900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
104000000000     C                   MOVEL     '1'           $FINE
104100000000     C*
104200000000     C                   ENDSR
104300000000     C*------------------------------------------------------------------------*
104400000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
104500000000     C*------------------------------------------------------------------------*
104600000000     C     F24STD        BEGSR
104700000000     C*
104800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
104900000000     C                   EXSR      OUTFUN
105000000000     C*
105100000000     C                   ENDSR
105200000000     C*------------------------------------------------------------------------*
105300000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
105400000000     C*          CODA MESSAGGI DEL PGM
105500000000     C*------------------------------------------------------------------------*
105600000000     C     CARMSG        BEGSR
105700000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
105800000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
105900000000     C     MSGULT        IFEQ      0
106000000000     C                   MOVEL     '1'           *IN40
106100000000     C                   WRITE     EC62DVK
106200000000     C                   MOVEL     '0'           *IN40
106300000000     C                   ENDIF
106400000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
106500000000     C                   MOVEL     $MSG          DSMSMS
106600000000     C                   CALL      'XMSG'        XMSGPA
106700000000     C     DSMSER        IFEQ      0
106800000000     C                   MOVEL     DSMSKE        VMCKEY
106900000000     C                   ENDIF
107000000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
107100000000     C                   ADD       1             MSGULT            4 0
107200000000     C                   WRITE     EC62DVM
107300000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
107400000000     C                   MOVEL     *BLANKS       $MSG
107500000000     C                   ENDSR
107600000000     C*------------------------------------------------------------------------*
107700000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
107800000000     C*------------------------------------------------------------------------*
107900000000     C     OUTMSG        BEGSR
108000000000     C* VISUALIZZAZIONE MESSAGGIO
108100000000     C                   MOVEL     '1'           *IN44
108200000000     C                   WRITE     EC62DVK
108300000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
108400000000     C                   Z-ADD     *ZEROS        MSGULT
108500000000     C                   ENDSR
108600000000     C*------------------------------------------------------------------------*
108700000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
108800000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
108900000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
109000000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
109100000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
109200000000     C*------------------------------------------------------------------------*
109300000000     C     CARFUN        BEGSR
109400000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
109500000000     C     1             DO        31            I                 5 0
109600000000     C                   MOVEL     FUN(I)        FUNRIG
109700000000     C                   MOVEL     FUNFUN        FFU(I)
109800000000     C                   MOVEL     FUNDES        FDE(I)
109900020923     C     DUTLPO        IFEQ      'S'
110000000000     C     $TGES         IFEQ      'G'
110100000000     C                   MOVEL     FUNASG        FAT(I)
110200000000     C                   ELSE
110300000000     C                   MOVEL     FUNASI        FAT(I)
110400000000     C                   ENDIF
110500000000     C                   ELSE
110600000000     C     $TGES         IFEQ      'G'
110700000000     C                   MOVEL     FUNAFG        FAT(I)
110800000000     C                   ELSE
110900000000     C                   MOVEL     FUNAFI        FAT(I)
111000000000     C                   ENDIF
111100000000     C                   ENDIF
111200000000     C     FAT(I)        IFEQ      'E'
111300000000     C                   MOVEL     USER          WEDP              3
111400000000     C     WEDP          IFEQ      'EDP'
111500000000     C                   MOVEL     'S'           FAT(I)
111600000000     C                   ELSE
111700000000     C                   MOVEL     'N'           FAT(I)
111800000000     C                   ENDIF
111900000000     C                   ENDIF
112000000000     C                   ENDDO
112100000000     C                   ENDSR
112200000000     C*------------------------------------------------------------------------*
112300000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
112400000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
112500000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
112600000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
112700000000     C*------------------------------------------------------------------------*
112800000000     C     OUTFUN        BEGSR
112900000000     C     1             DO        8             I
113000000000     C                   MOVEL     *BLANKS       FUV(I)
113100000000     C                   ENDDO
113200000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
113300000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
113400000000     C     1             DO        31            I
113500000000     C     FAT(I)        IFEQ      'S'
113600000000     C     FDE(I)        ANDNE     *BLANKS
113700000000     C                   ADD       1             FUNNAT
113800000000     C                   ENDIF
113900000000     C                   ENDDO
114000000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
114100000000     C     FUNNAT        IFGT      8
114200000000     C                   MOVEL     'S'           FAT(24)
114300000000     C                   ENDIF
114400000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
114500000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
114600000000     C                   Z-ADD     8             FUNVIS            5 0
114700000000     C     FAT(23)       IFEQ      'S'
114800000000     C                   SUB       1             FUNVIS
114900000000     C                   ENDIF
115000000000     C     FAT(24)       IFEQ      'S'
115100000000     C                   SUB       1             FUNVIS
115200000000     C                   ENDIF
115300000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
115400000000     C                   Z-ADD     *ZEROS        W                 5 0
115500000000     C                   ADD       1             FUNULT
115600000000     C     FUNULT        IFGT      31
115700000000     C                   Z-ADD     1             FUNULT
115800000000     C                   ENDIF
115900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
116000000000     C     FUNULT        DO        31            I
116100000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
116200000000     C     W             IFLT      FUNVIS
116300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
116400000000     C     FAT(I)        IFEQ      'S'
116500000000     C     FDE(I)        ANDNE     *BLANKS
116600000000     C     I             IFEQ      23
116700000000     C     I             OREQ      24
116800000000     C                   ELSE
116900000000     C                   ADD       1             W
117000000000     C                   MOVEL     FDE(I)        FUV(W)
117100000000     C                   ENDIF
117200000000     C                   ENDIF
117300000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
117400000000     C                   Z-ADD     I             FUNULT
117500000000     C                   ENDIF
117600000000     C                   ENDDO
117700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
117800000000     C                   MOVEL     FUV(1)        VFD001
117900000000     C                   MOVEL     FUV(2)        VFD002
118000000000     C                   MOVEL     FUV(3)        VFD003
118100000000     C                   MOVEL     FUV(4)        VFD004
118200000000     C                   MOVEL     FUV(5)        VFD005
118300000000     C                   MOVEL     FUV(6)        VFD006
118400000000     C     FAT(23)       IFEQ      'N'
118500000000     C     FAT(24)       ANDEQ     'N'
118600000000     C                   MOVEL     FUV(7)        VFD007
118700000000     C                   MOVEL     FUV(8)        VFD008
118800000000     C                   ELSE
118900000000     C     FAT(23)       IFEQ      'S'
119000000000     C     FAT(24)       ANDEQ     'S'
119100000000     C                   MOVEL     FDE(23)       VFD007
119200000000     C                   MOVEL     FDE(24)       VFD008
119300000000     C                   ELSE
119400000000     C                   MOVEL     FUV(7)        VFD007
119500000000     C     FAT(23)       IFEQ      'S'
119600000000     C                   MOVEL     FDE(23)       VFD008
119700000000     C                   ELSE
119800000000     C                   MOVEL     FDE(24)       VFD008
119900000000     C                   ENDIF
120000000000     C                   ENDIF
120100000000     C                   ENDIF
120200000000     C                   MOVEL     '0'           $TESVD
120300000000     C                   ENDSR
120400000000     C*------------------------------------------------------------------------*
120500000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
120600000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
120700000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
120800000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
120900000000     C*------------------------------------------------------------------------*
121000000000     C     CONFUN        BEGSR
121100000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
121200000000     C     1             DO        31            I
121300000000     C     KEY           IFEQ      KEYS(I)
121400000000     C* CONTROLLO SE IL TASTO E' ATTIVO
121500000000     C     FAT(I)        IFNE      'S'
121600000000     C                   MOVEL     MSG(1)        $MSG
121700000000     C                   EXSR      CARMSG
121800000000     C                   MOVEL     '1'           $ERR
121900000000     C                   ENDIF
122000000000     C                   ENDIF
122100000000     C                   ENDDO
122200000000     C                   ENDSR
122300000000**   SCHIERA: MSG  (MESSAGGI)
122400000000TREC62R- Tasto funzionale non valido.                                          1
122500000000TREC62R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
122600000000TREC62R- Opzione non valida.                                                   3
122700000000TREC62R- Opzione abilitata ma non gestita. Avvertire CED.                      4
122800000000TREC62R- Inizio elenco.                                                        5
122900000000TREC62R- Fine elenco.                                                          6
123000000000TREC62R- Immettere tipo tariffa esistente e non annullato                      7
123100000000TREC62R- Immettere data formalmente valida                                     8
123200000000TREC62R- Immettere codice voce esistente                                       9
123300000000TREC62R- Immettere codice C.A.P. esistente e non annullato                    10
123400000000TREC62R- Immettere riferimento tariffa esistente e non annullato              11
123500940722TREC62R- Immettere codice cliente esistente e non annullato                   12
123600940722TREC62R- Immettere codice tariffa numerico                                    13
123700000000TREC62R- Immettere codice filiale valido                                      14
123800000000TREC62R- Immettere codice tassazione esistente e non annullato                15
123900000000TREC62R- Immettere filiali non uguali                                         16
124000000000TREC62R- Se una filiale e' 1° livello l'altra deve appartenere all'area       17
124100940722TREC62R- 18
124200000000TREC62R- 19
124300000000TREC62R- 20
124400000000TREC62R- 21
124500000000TREC62R- 22
124600000000TREC62R- 23
124700000000TREC62R- 24
124800940722* INESISTENTE *                                                               25
124900000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
125000000000F1    -                   -    NN     NN
125100000000F2    -                   -    NN     NN
125200940726F3    -F3=Fine            -    NN     NN
125300000000F4    -F4=Decodifica      -    SS     SS
125400000000F5    -                   -    NN     NN
125500000000F6    -                   -    NN     NN
125600000000F7    -                   -    NN     NN
125700000000F8    -                   -    NN     NN
125800000000F9    -                   -    NN     NN
125900000000F10   -                   -    NN     NN
126000000000F11   -                   -    NN     NN
126100000000F12   -F12=Ritorno        -    SS     SS
126200000000F13   -                   -    NN     NN
126300000000F14   -                   -    NN     NN
126400000000F15   -                   -    NN     NN
126500000000F16   -                   -    NN     NN
126600000000F17   -                   -    NN     NN
126700000000F18   -                   -    NN     NN
126800000000F19   -                   -    NN     NN
126900000000F20   -                   -    NN     NN
127000000000F21   -                   -    NN     NN
127100000000F22   -                   -    NN     NN
127200000000F23   -                   -    NN     NN
127300000000F24   -F24=Altri tasti    -    NN     NN
127400000000ENTER -                   -    SS     SS
127500000000ROLLUP-                   -    NN     NN
127600000000ROLLDN-                   -    NN     NN
127700000000HELP  -                   -    NN     NN
127800000000PRINT -                   -    NN     NN
127900000000BSPACE-                   -    NN     NN
128000000000CLEAR -                   -    NN     NN

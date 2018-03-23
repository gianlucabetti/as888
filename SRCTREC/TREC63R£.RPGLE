000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC63R
000300000000      *  Descrizione   :  Gesione tariffe voci di conto economico
000400000000      *                   Programma di visualizzazione subfile e scelta
000500000000      *  Autore        :  RAFFAELLA (RN), STEFANO (SC)
000600000000      *  Data creazione:  09 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC63D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC63DVS:SFLULT)
001400000000     F                                     SFILE(EC63DVM:MSGULT)
001500000000     FECEVD01L  IF   E           K DISK
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002000000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002100000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
002200000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
002300000000     D FUN             S             40    DIM(31)
002400000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002500000000     D FFU             S              6    DIM(31)
002600000000     D FDE             S             19    DIM(31)
002700000000     D FAT             S              1    DIM(31)
002800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
002900000000     D OPD             S              2    DIM(24)
003000000000     D OPS             S              2    DIM(24)
003100000000     D ODE             S             19    DIM(24)
003200000000     D OAT             S              1    DIM(24)
003300000000     D* TABELLA DESCRIZIONI A VIDEO
003400000000     D FUV             S             19    DIM(8)
003500000000     D OPV             S             19    DIM(8)
003600000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003700000000     D KEYS            S              1    DIM(40)
003800000000     D*------------------------------------------------------------------------*
003900000000     D* INPUT
004000000000     D*------------------------------------------------------------------------*
004100000000     D*---------------------
004200000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
004300000000     D*---------------------
004400000000     D DSOPZ         E DS
004500000000     D  OPZRIG                 1     40
004600000000     D*---------------------
004700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004800000000     D*---------------------
004900000000     D DSFUN         E DS
005000000000     D  FUNRIG                 1     40
005100000000     D*---------------------
005200000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005300000000     D*---------------------
005400000000     D DSMSG         E DS
005500000000     D*---------------------
005600020923     D* DS REPERIMENTO DATI UTENTE
005700020923     D*-------------------
005800020923     D TIBS34DS      E DS
005900020923     D DDATIUTE      E DS
006000020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006100000000     D*---------------------
006200000000     D* DS PER GESTIONE TASTI FUNZIONALI
006300000000     D*---------------------
006400000000     D DSKEYS        E DS
006500000000     D  KEYRIG                 1     40
006600000000     D*---------------------
006700000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006800000000     D*---------------------
006900000000     D                SDS
007000000000     D  USER                 254    263
007100000000     D*---------------------
007200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007300000000     D*---------------------
007400000000     D DEVDS1          DS
007500000000     D* CHIAVE FUNZIONALE BATTUTA
007600000000     D  KEY                  369    369
007700000000     D* POSIZIONE CURSORE (IN HEX)
007800000000     D  CURSOR               370    371
007900000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
008000000000     D  FRTSFL               378    379B 0
008100000000     D*---------------------
008200000000     D* DS ESTERNA DI PROCEDURA
008300000000     D*---------------------
008400000000     D DS60          E DS                  EXTNAME(TREC60DS)
008500000000     D*---------------------
008600000000     D* ARCHITETTURA
008700000000     D*---------------------
008800000000     D KPJBA         E DS
008900000000     D*------------------
009000000000     D* CALL XSRDA8 - CONTROLLO DATE
009100000000     D*------------------
009200000000     D WLBDAT          DS                  INZ
009300000000     D  G02DAT                 1      8  0
009400000000     D  G02INV                 9     16  0
009500000000     D  G02ERR                17     17
009600000000     D  G02TGI                18     22  0
009700000000     D*------------------
009800000000     D* SUDDIVIDO IL PRIMO CAMPO CHIAVE A VIDEO
009900000000     D*------------------
010000000000     D                 DS
010100000000     D  VCCKE1                 1      7
010200000000     D  VCCK11                 5      7
010300000000     D  VCCK21                 3      7
010400000000     D*------------------
010500000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
010600000000     D*------------------
010700000000     D DESGES          C                   CONST('** GESTIONE **')
010800000000     D DESINT          C                   CONST('INTERROGAZIONE')
010900000000     C*------------------------------------------------------------------------*
011000000000     C* MAIN LINE
011100000000     C*------------------------------------------------------------------------*
011200000000     C*
011300000000     C* CICLO FINO A CHE:
011400000000DO  1C     $FINE         DOUEQ     '1'
011500000000     C*
011600000000     C                   MOVEL     '0'           $ERR
011700000000     C*
011800000000     C* CONTROLLA GESTIONE VIDEO
011900000000IF  2C     $GES          IFEQ      'VS'
012000000000     C                   EXSR      GESVS
012100000000E   2C                   ENDIF
012200000000     C*
012300000000E   1C                   ENDDO
012400000000     C*
012500000000IF  1C     $F03          IFEQ      '1'
012600000000     C                   MOVEL     '1'           D60F03
012700000000E   1C                   ENDIF
012800000000     C* FINE PGM
012900000000     C                   MOVEL     DS60          KPJBU
013000000000     C                   SETON                                        LR
013100000000     C*------------------------------------------------------------------------*
013200000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
013300000000     C*------------------------------------------------------------------------*
013400000000     C     CARVS         BEGSR
013500000000     C*
013600000000     C                   MOVEL     'VS'          $GES
013700000000     C*
013800000000     C                   EXSR      CAROPZ
013900000000     C                   EXSR      OUTOPZ
014000000000     C                   MOVEA     FUN2          FUN
014100000000     C                   EXSR      CARFUN
014200000000     C                   EXSR      OUTFUN
014300000000     C*
014400000000     C                   WRITE     EC63DVT
014500000000     C                   WRITE     EC63DVO
014600000000     C                   WRITE     EC63DVF
014700000000     C*
014800000000     C                   EXSR      POSSFL
014900000000     C*
015000000000     C                   ENDSR
015100000000     C*------------------------------------------------------------------------*
015200000000     C* GESVS  - GESTIONE SUBFILE
015300000000     C*------------------------------------------------------------------------*
015400000000     C     GESVS         BEGSR
015500000000     C*
015600000000IF  1C     $TESVD        IFEQ      '1'
015700000000     C                   WRITE     EC63DVT
015800000000     C                   WRITE     EC63DVO
015900000000     C                   WRITE     EC63DVF
016000000000E   1C                   ENDIF
016100000000     C*
016200000000     C                   EXSR      OUTSFL
016300000000     C*
016400000000     C                   EXSR      CONFUN
016500000000     C*
016600000000IF  1C     $ERR          IFEQ      '0'
016700000000SL  2C                   SELECT
016800000000     C     KEY           WHENEQ    F6
016900000000     C                   EXSR      F06GES
017000000000     C     KEY           WHENEQ    F14
017100000000     C                   EXSR      F14GES
017200000000     C                   OTHER
017300000000     C                   EXSR      FUNSFL
017400000000E   2C                   ENDSL
017500000000E   1C                   ENDIF
017600000000     C*
017700000000     C                   ENDSR
017800000000     C*------------------------------------------------------------------------*
017900000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
018000000000     C*------------------------------------------------------------------------*
018100000000     C     F06GES        BEGSR
018200000000     C*
018300940726     C* IMPOSTO SUBITO IL CAMPO DELL'OPZIONE
018400940726     C                   MOVEL     'F06'         WOP1
018500940726     C*
018600000000     C* SE NON E' IMPOSTATO IL TIPO TARIFFA RICHIAMO PRIMA LA FINESTRA DI F14
018700940726     C* 26.7.94  LA FINESTRA DEVE APPARIRE SEMPRE
018800940726IF  1C********   VCCTTV    IFEQ *BLANKS
018900000000     C                   EXSR      F14GES
019000940726E   1C********             ENDIF
019100000000     C*
019200000000     C* LANCIO L'INSERIMENTO, SOLO SE C'E' IL TIPO TARIFFA
019300940726     C* E SE NON SONO USCITO CON F3=FINE
019400000000IF  1C     VCCTTV        IFNE      *BLANKS
019500940726     C     D60F03        ANDNE     '1'
019600000000     C                   Z-ADD     VCCVOC        WVOC
019700000000     C                   Z-ADD     VCCPRG        WPRG
019800000000     C                   Z-ADD     *ZERO         WDDV
019900000000     C                   MOVE      D60TTV        WTTV
020000000000     C                   Z-ADD     *ZEROS        WCLI
020100000000     C                   Z-ADD     *ZERO         WCTR
020200000000     C                   Z-ADD     *ZERO         WFT1
020300000000     C                   Z-ADD     *ZERO         WFT2
020400000000     C                   MOVE      *BLANKS       WCTS
020500000000     C                   Z-ADD     *ZERO         WCAP
020600000000     C                   Z-ADD     VCCDDT        WDDT
020700000000     C     D60TTV        IFEQ      'K'
020800000000     C                   MOVE      VCCKE1        WCLI
020900000000     C                   MOVE      VCCKE2        WCTR
021000000000     C                   ENDIF
021100000000     C     D60TTV        IFEQ      'F'
021200000000     C                   MOVE      VCCKE1        WFT1
021300000000     C                   MOVE      VCCKE2        WFT2
021400000000     C                   ENDIF
021500000000     C     D60TTV        IFEQ      'P'
021600000000     C                   MOVE      VCCKE1        WCAP
021700000000     C                   ENDIF
021800000000     C     D60TTV        IFEQ      'T'
021900000000     C                   MOVE      VCCKE1        WCTS
022000000000     C                   ENDIF
022100000000     C                   Z-ADD     *ZEROS        WSGI             12 3
022200000000     C*
022300000000     C                   MOVEL     D60OP0        WOP0
022400000000     C                   MOVEL     'TREC64R'     WPGM              8
022500000000     C                   EXSR      LANPGM
022600000000     C*
022700000000     C                   EXSR      POSSFL
022800000000     C*
022900000000E   1C                   ENDIF
023000000000     C*
023100000000     C                   ENDSR
023200000000     C*------------------------------------------------------------------------*
023300000000     C* F14GES -GEST.TASTO FUNZ. F14 ===> "TOM CAT"   OPZIONI ELENCO
023400000000     C* LA PRIMA ROUTINE DEDICATA AL GLORIOSO AEREO DA CACCIA AMERICANO
023500000000     C*------------------------------------------------------------------------*
023600000000     C     F14GES        BEGSR
023700000000     C*
023800000000     C* LANCIO L'INSERIMENTO
023900000000     C                   Z-ADD     VCCVOC        WVOC
024000000000     C                   Z-ADD     VCCPRG        WPRG
024100000000     C                   Z-ADD     *ZEROS        WDDV
024200000000     C                   MOVE      D60TTV        WTTV
024300000000     C                   Z-ADD     *ZEROS        WCLI
024400000000     C                   Z-ADD     *ZERO         WCTR
024500000000     C                   Z-ADD     *ZERO         WFT1
024600000000     C                   Z-ADD     *ZERO         WFT2
024700000000     C                   MOVE      *BLANKS       WCTS
024800000000IF  1C     VCCTTV        IFEQ      'K'
024900000000     C                   MOVE      VCCKE1        WCLI
025000000000     C                   MOVE      VCCKE2        WCTR
025100000000E   1C                   ENDIF
025200000000IF  1C     VCCTTV        IFEQ      'F'
025300000000     C                   MOVE      VCCKE1        WFT1
025400000000     C                   MOVE      VCCKE2        WFT2
025500000000E   1C                   ENDIF
025600000000IF  1C     VCCTTV        IFEQ      'T'
025700000000     C                   MOVE      VCCKE1        WCTS
025800000000E   1C                   ENDIF
025900000000IF  1C     VCCTTV        IFEQ      'P'
026000000000     C                   MOVE      VCCKE1        WCAP
026100000000E   1C                   ENDIF
026200000000     C                   Z-ADD     *ZERO         WCAP
026300000000     C                   Z-ADD     VCCDDT        WDDT
026400000000     C*
026500000000     C                   MOVEL     D60OP0        WOP0
026600000000     C***********          MOVEL'   '     WOP1             *LIVELLO PROGRAMMA
026700000000     C                   MOVEL     'TREC62R'     WPGM              8
026800000000     C                   EXSR      LANPGM
026900000000     C*
027000000000     C                   Z-ADD     D60VOC        VCCVOC
027100000000     C                   Z-ADD     D60PRG        VCCPRG
027200000000     C                   MOVE      D60TTV        VCCTTV
027300000000     C                   MOVE      D60DDT        VCCDDT
027400000000     C                   Z-ADD     D60DDT        G02DAT
027500000000     C                   Z-ADD     *ZEROS        G02INV
027600000000     C                   MOVEL     '0'           G02ERR
027700000000     C                   CALL      'XSRDA8'
027800000000     C                   PARM                    WLBDAT
027900000000     C                   Z-ADD     G02INV        DDTVCC
028000000000     C                   MOVE      *BLANKS       VCCKE1
028100000000     C                   MOVE      *BLANKS       VCCKE2
028200000000IF  1C     D60TTV        IFEQ      'K'
028300000000     C                   MOVE      D60CLI        VCCKE1
028400000000     C                   MOVE      D60CTR        VCCKE2
028500000000E   1C                   ENDIF
028600000000IF  1C     D60TTV        IFEQ      'F'
028700000000     C                   MOVE      D60FT1        VCCKE1
028800000000     C                   MOVE      D60FT2        VCCKE2
028900000000E   1C                   ENDIF
029000000000IF  1C     D60TTV        IFEQ      'T'
029100000000     C                   MOVE      D60CTS        VCCKE1
029200000000E   1C                   ENDIF
029300000000IF  1C     D60TTV        IFEQ      'P'
029400000000     C                   MOVE      D60CAP        VCCKE1
029500000000E   1C                   ENDIF
029600000000     C*
029700000000     C* SE SONO ENTRATO QUI PER F6, NON DEVO RIPOSIZIONARMI COME FAREI CON F14
029800000000     C     KEY           IFNE      F6
029900000000     C                   EXSR      NEWPOS
030000000000     C                   EXSR      POSSFL
030100000000     C                   ENDIF
030200000000     C*
030300000000     C                   ENDSR
030400000000     C*------------------------------------------------------------------------*
030500000000     C*  GESOPZ - GESTIONE OPZIONI
030600000000     C*------------------------------------------------------------------------*
030700000000     C     GESOPZ        BEGSR
030800000000     C*
030900000000SL  1C                   SELECT
031000000000     C     VSCOPZ        WHENEQ    ' 2'
031100000000     C                   EXSR      O02GES
031200010112     C     VSCOPZ        WHENEQ    ' 3'
031300010112     C                   EXSR      O03GES
031400000000     C     VSCOPZ        WHENEQ    ' 4'
031500000000     C                   EXSR      O04GES
031600000000     C     VSCOPZ        WHENEQ    ' 5'
031700000000     C                   EXSR      O05GES
031800000000     C     VSCOPZ        WHENEQ    ' 7'
031900000000     C                   EXSR      O07GES
032000000000     C                   OTHER
032100000000     C                   MOVEL     MSG(4)        $MSG
032200000000     C                   EXSR      CARMSG
032300000000E   1C                   ENDSL
032400000000     C*
032500000000     C                   ENDSR
032600000000     C*------------------------------------------------------------------------*
032700000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
032800000000     C*------------------------------------------------------------------------*
032900000000     C     O02GES        BEGSR
033000000000     C*
033100000000IF  1C     VSCATB        IFEQ      'A'
033200000000     C                   MOVEL     '1'           $ERR
033300000000     C                   MOVEL     MSG(12)       $MSG
033400000000     C                   EXSR      CARMSG
033500000000E   1C                   ENDIF
033600000000     C*
033700000000IF  1C     $ERR          IFEQ      '0'
033800000000     C*
033900000000     C* LANCIO LA MODIFICA
034000000000     C                   Z-ADD     VSCVOC        WVOC
034100000000     C                   Z-ADD     VSCPRG        WPRG
034200000000     C                   Z-ADD     VSCDDT        WDDT
034300000000     C                   MOVEL     VSCTTV        WTTV
034400000000     C                   Z-ADD     *ZERO         WCLI
034500000000     C                   Z-ADD     *ZERO         WCTR
034600000000     C                   Z-ADD     *ZERO         WFT1
034700000000     C                   Z-ADD     *ZERO         WFT2
034800000000     C                   Z-ADD     *ZERO         WCAP
034900000000     C                   MOVE      *BLANKS       WCTS
035000000000IF  1C     VSCTTV        IFEQ      'K'
035100000000     C                   MOVE      VSCCLI        WCLI
035200000000     C                   MOVEL     VSCCTR        WCTR
035300000000E   1C                   ENDIF
035400000000IF  1C     VSCTTV        IFEQ      'F'
035500000000     C                   MOVE      VSCCLI        WFT1
035600000000     C                   MOVEL     VSCCTR        WFT2
035700000000E   1C                   ENDIF
035800000000IF  1C     VSCTTV        IFEQ      'P'
035900000000     C                   MOVE      VSCCLI        WCAP
036000000000E   1C                   ENDIF
036100000000IF  1C     VSCTTV        IFEQ      'T'
036200000000     C                   MOVE      VSCCLI        WCTS
036300000000E   1C                   ENDIF
036400000000     C                   Z-ADD     VSCSGI        WSGI
036500000000     C*
036600000000     C                   MOVEL     D60OP0        WOP0
036700000000     C                   MOVEL     'O02'         WOP1
036800000000     C                   MOVEL     'TREC64R'     WPGM              8
036900000000     C                   EXSR      LANPGM
037000000000     C*
037100000000     C                   Z-ADD     1             SWMODI
037200000000     C*
037300000000E   1C                   ENDIF
037400000000     C*
037500000000     C                   ENDSR
037600010112     C*------------------------------------------------------------------------*
037700010112     C*  O03GES - GESTIONE OPZIONE 3=DUPLICA
037800010112     C*------------------------------------------------------------------------*
037900010112     C     O03GES        BEGSR
038000010112     C*
038100010112IF  1C     VSCATB        IFEQ      'A'
038200010112     C                   MOVEL     '1'           $ERR
038300010112     C                   MOVEL     MSG(12)       $MSG
038400010112     C                   EXSR      CARMSG
038500010112E   1C                   ENDIF
038600010112     C*
038700010112IF  1C     $ERR          IFEQ      '0'
038800010112     C*
038900010112     C* LANCIO LA MODIFICA
039000010112     C                   Z-ADD     VSCVOC        WVOC
039100010112     C                   Z-ADD     VSCPRG        WPRG
039200010112     C                   Z-ADD     VSCDDT        WDDT
039300010112     C                   MOVEL     VSCTTV        WTTV
039400010112     C                   Z-ADD     *ZERO         WCLI
039500010112     C                   Z-ADD     *ZERO         WCTR
039600010112     C                   Z-ADD     *ZERO         WFT1
039700010112     C                   Z-ADD     *ZERO         WFT2
039800010112     C                   Z-ADD     *ZERO         WCAP
039900010112     C                   MOVE      *BLANKS       WCTS
040000010112IF  1C     VSCTTV        IFEQ      'K'
040100010112     C                   MOVE      VSCCLI        WCLI
040200010112     C                   MOVEL     VSCCTR        WCTR
040300010112E   1C                   ENDIF
040400010112IF  1C     VSCTTV        IFEQ      'F'
040500010112     C                   MOVE      VSCCLI        WFT1
040600010112     C                   MOVEL     VSCCTR        WFT2
040700010112E   1C                   ENDIF
040800010112IF  1C     VSCTTV        IFEQ      'P'
040900010112     C                   MOVE      VSCCLI        WCAP
041000010112E   1C                   ENDIF
041100010112IF  1C     VSCTTV        IFEQ      'T'
041200010112     C                   MOVE      VSCCLI        WCTS
041300010112E   1C                   ENDIF
041400010112     C                   Z-ADD     VSCSGI        WSGI
041500010112     C*
041600010112     C                   MOVEL     D60OP0        WOP0
041700010112     C                   MOVEL     'O03'         WOP1
041800010112     C                   MOVEL     'TREC64R'     WPGM              8
041900010112     C                   EXSR      LANPGM
042000010112     C*
042100010112     C                   Z-ADD     1             SWMODI
042200010112     C*
042300010112E   1C                   ENDIF
042400010112     C*
042500010112     C                   ENDSR
042600000000     C*------------------------------------------------------------------------*
042700000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
042800000000     C*------------------------------------------------------------------------*
042900000000     C     O04GES        BEGSR
043000000000     C*
043100000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
043200000000IF  1C     VSCATB        IFEQ      'A'
043300000000     C                   MOVEL     '1'           $ERR
043400000000     C                   MOVEL     MSG(12)       $MSG
043500000000     C                   EXSR      CARMSG
043600000000E   1C                   ENDIF
043700000000     C*
043800000000IF  1C     $ERR          IFEQ      '0'
043900000000     C*
044000000000     C* LANCIO LA CANCELLAZIONE
044100000000     C                   Z-ADD     VSCVOC        WVOC
044200000000     C                   Z-ADD     VSCPRG        WPRG
044300000000     C                   Z-ADD     VSCDDT        WDDT
044400000000     C                   MOVEL     VSCTTV        WTTV
044500000000     C                   Z-ADD     *ZERO         WCLI
044600000000     C                   Z-ADD     *ZERO         WCTR
044700000000     C                   Z-ADD     *ZERO         WFT1
044800000000     C                   Z-ADD     *ZERO         WFT2
044900000000     C                   Z-ADD     *ZERO         WCAP
045000000000     C                   MOVE      *BLANKS       WCTS
045100000000IF  1C     VSCTTV        IFEQ      'K'
045200000000     C                   MOVE      VSCCLI        WCLI
045300000000     C                   MOVEL     VSCCTR        WCTR
045400000000E   1C                   ENDIF
045500000000IF  1C     VSCTTV        IFEQ      'F'
045600000000     C                   MOVE      VSCCLI        WFT1
045700000000     C                   MOVEL     VSCCTR        WFT2
045800000000E   1C                   ENDIF
045900000000IF  1C     VSCTTV        IFEQ      'P'
046000000000     C                   MOVE      VSCCLI        WCAP
046100000000E   1C                   ENDIF
046200000000IF  1C     VSCTTV        IFEQ      'T'
046300000000     C                   MOVE      VSCCLI        WCTS
046400000000E   1C                   ENDIF
046500000000     C                   Z-ADD     VSCSGI        WSGI
046600000000     C*
046700000000     C                   MOVEL     D60OP0        WOP0
046800000000     C                   MOVEL     'O04'         WOP1
046900000000     C                   MOVEL     'TREC64R'     WPGM              8
047000000000     C                   EXSR      LANPGM
047100000000     C*
047200000000     C                   Z-ADD     1             SWMODI
047300000000     C*
047400000000E   1C                   ENDIF
047500000000     C*
047600000000     C                   ENDSR
047700000000     C*------------------------------------------------------------------------*
047800000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
047900000000     C*------------------------------------------------------------------------*
048000000000     C     O05GES        BEGSR
048100000000     C*
048200000000     C* LANCIO LA VISUALIZZAZIONE
048300000000     C                   Z-ADD     VSCVOC        WVOC
048400000000     C                   Z-ADD     VSCPRG        WPRG
048500000000     C                   Z-ADD     VSCDDT        WDDT
048600000000     C                   MOVEL     VSCTTV        WTTV
048700000000     C                   Z-ADD     *ZERO         WCLI
048800000000     C                   Z-ADD     *ZERO         WCTR
048900000000     C                   Z-ADD     *ZERO         WFT1
049000000000     C                   Z-ADD     *ZERO         WFT2
049100000000     C                   Z-ADD     *ZERO         WCAP
049200000000     C                   MOVE      *BLANKS       WCTS
049300000000IF  1C     VSCTTV        IFEQ      'K'
049400000000     C                   MOVE      VSCCLI        WCLI
049500000000     C                   MOVEL     VSCCTR        WCTR
049600000000E   1C                   ENDIF
049700000000IF  1C     VSCTTV        IFEQ      'F'
049800000000     C                   MOVE      VSCCLI        WFT1
049900000000     C                   MOVEL     VSCCTR        WFT2
050000000000E   1C                   ENDIF
050100000000IF  1C     VSCTTV        IFEQ      'P'
050200000000     C                   MOVE      VSCCLI        WCAP
050300000000E   1C                   ENDIF
050400000000IF  1C     VSCTTV        IFEQ      'T'
050500000000     C                   MOVE      VSCCLI        WCTS
050600000000E   1C                   ENDIF
050700000000     C                   Z-ADD     VSCSGI        WSGI
050800000000     C*
050900000000     C                   MOVEL     D60OP0        WOP0
051000000000     C                   MOVEL     'O05'         WOP1
051100000000     C                   MOVEL     'TREC64R'     WPGM              8
051200000000     C                   EXSR      LANPGM
051300000000     C*
051400000000     C                   ENDSR
051500000000     C*------------------------------------------------------------------------*
051600000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
051700000000     C*------------------------------------------------------------------------*
051800000000     C     O07GES        BEGSR
051900000000     C*
052000000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
052100000000IF  1C     VSCATB        IFNE      'A'
052200000000     C                   MOVEL     '1'           $ERR
052300000000     C                   MOVEL     MSG(13)       $MSG
052400000000     C                   EXSR      CARMSG
052500000000E   1C                   ENDIF
052600000000     C*
052700000000IF  1C     $ERR          IFEQ      '0'
052800000000     C*
052900000000     C* LANCIO IL RIPRISTINO
053000000000     C                   Z-ADD     VSCVOC        WVOC
053100000000     C                   Z-ADD     VSCPRG        WPRG
053200000000     C                   Z-ADD     VSCDDT        WDDT
053300000000     C                   MOVEL     VSCTTV        WTTV
053400000000     C                   Z-ADD     *ZERO         WCLI
053500000000     C                   Z-ADD     *ZERO         WCTR
053600000000     C                   Z-ADD     *ZERO         WFT1
053700000000     C                   Z-ADD     *ZERO         WFT2
053800000000     C                   Z-ADD     *ZERO         WCAP
053900000000     C                   MOVE      *BLANKS       WCTS
054000000000IF  1C     VSCTTV        IFEQ      'K'
054100000000     C                   MOVE      VSCCLI        WCLI
054200000000     C                   MOVEL     VSCCTR        WCTR
054300000000E   1C                   ENDIF
054400000000IF  1C     VSCTTV        IFEQ      'F'
054500000000     C                   MOVE      VSCCLI        WFT1
054600000000     C                   MOVEL     VSCCTR        WFT2
054700000000E   1C                   ENDIF
054800000000IF  1C     VSCTTV        IFEQ      'P'
054900000000     C                   MOVE      VSCCLI        WCAP
055000000000E   1C                   ENDIF
055100000000IF  1C     VSCTTV        IFEQ      'T'
055200000000     C                   MOVE      VSCCLI        WCTS
055300000000E   1C                   ENDIF
055400000000     C                   Z-ADD     VSCSGI        WSGI
055500000000     C*
055600000000     C                   MOVEL     D60OP0        WOP0
055700000000     C                   MOVEL     'O07'         WOP1
055800000000     C                   MOVEL     'TREC64R'     WPGM              8
055900000000     C                   EXSR      LANPGM
056000000000     C*
056100000000     C                   Z-ADD     1             SWMODI
056200000000     C*
056300000000E   1C                   ENDIF
056400000000     C*
056500000000     C                   ENDSR
056600000000     C*------------------------------------------------------------------------*
056700000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
056800000000     C*------------------------------------------------------------------------*
056900000000     C     NEWPOS        BEGSR
057000000000     C*
057100000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
057200000000     C*        1 = RICHIESTO POSIZIONAMENTO
057300000000     C                   Z-ADD     0             SWPOSI            7 0
057400000000     C*
057500000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
057600000000     C*            NON OCCORRE RICARICARE IL SUBFILE
057700000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
057800000000     C                   Z-ADD     0             SWMODI            1 0
057900000000     C*
058000000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
058100000000IF  1C     VCCVOC        IFNE      DEPVOC
058200000000     C     VCCPRG        ORNE      DEPPRG
058300000000     C     VCCTTV        ORNE      DEPTTV
058400000000     C     VCCKE1        ORNE      DEPKE1
058500000000     C     VCCKE2        ORNE      DEPKE2
058600000000     C     VCCDDT        ORNE      DEPDDT
058700010111     C     VCCVAL        ORNE      DEPVAL
058800000000     C*
058900000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
059000000000     C                   EXSR      CHKPAR
059100000000     C*
059200000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
059300000000IF  2C     $ERR          IFEQ      '0'
059400000000     C                   Z-ADD     1             SWPOSI
059500000000     C                   Z-ADD     VCCVOC        DEPVOC
059600000000     C                   Z-ADD     VCCPRG        DEPPRG
059700000000     C                   MOVE      VCCTTV        DEPTTV
059800000000     C                   MOVE      VCCKE1        DEPKE1
059900000000     C                   MOVE      VCCKE2        DEPKE2
060000000000     C                   Z-ADD     VCCDDT        DEPDDT
060100010111     C                   Z-ADD     VCCVAL        DEPVAL
060200000000E   2C                   ENDIF
060300000000E   1C                   ENDIF
060400000000     C*
060500000000     C                   ENDSR
060600000000     C*------------------------------------------------------------------------*
060700000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
060800000000     C*------------------------------------------------------------------------*
060900000000     C     CHKPAR        BEGSR
061000000000     C*
061100000000     C* REIMPOSTO INDICATORI DI ERRORE
061200010111     C                   SETOFF                                         8183
061300000000     C*--------------------
061400000000     C* DATA DECORRENZA VOCE
061500000000     C*--------------------
061600000000     C                   Z-ADD     *ZERO         DDTVCC
061700000000IF  1C     VCCDDT        IFGT      *ZEROS
061800000000     C                   Z-ADD     VCCDDT        G02DAT
061900000000     C                   Z-ADD     *ZEROS        G02INV
062000000000     C                   MOVEL     '0'           G02ERR
062100000000     C                   CALL      'XSRDA8'
062200000000     C                   PARM                    WLBDAT
062300000000     C*
062400000000IF  2C     G02ERR        IFEQ      '1'
062500000000     C                   MOVEL     '1'           $ERR
062600000000     C                   MOVEL     MSG(9)        $MSG
062700000000     C                   EXSR      CARMSG
062800000000     C                   SETON                                        81
062900000000X   2C                   ELSE
063000000000     C                   Z-ADD     G02DAT        VCCDDT
063100000000     C                   Z-ADD     G02INV        DDTVCC            8 0
063200000000E   2C                   ENDIF
063300000000E   1C                   ENDIF
063400010111     C*--------------------
063500010111     C* DATA VALIDITA' AL
063600010111     C*--------------------
063700010111     C                   Z-ADD     *ZERO         VALVCC
063800010111IF  1C     VCCVAL        IFGT      *ZEROS
063900010111     C                   Z-ADD     VCCVAL        G02DAT
064000010111     C                   Z-ADD     *ZEROS        G02INV
064100010111     C                   MOVEL     '0'           G02ERR
064200010111     C                   CALL      'XSRDA8'
064300010111     C                   PARM                    WLBDAT
064400010111     C*
064500010111IF  2C     G02ERR        IFEQ      '1'
064600010111     C                   MOVEL     '1'           $ERR
064700010111 XXX C                   MOVEL     MSG(9)        $MSG
064800010111     C                   EXSR      CARMSG
064900010111     C                   SETON                                        83
065000010111X   2C                   ELSE
065100010111     C                   Z-ADD     G02DAT        VCCVAL
065200010111     C                   Z-ADD     G02INV        VALVCC            8 0
065300010111E   2C                   ENDIF
065400010111E   1C                   ENDIF
065500000000     C*
065600000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
065700000000     C* SUL CAMPO DEL CONTROL
065800000000IF  1C     $ERR          IFEQ      '1'
065900000000     C                   MOVEL     '0'           *IN43
066000000000X   1C                   ELSE
066100000000     C                   MOVEL     '1'           *IN43
066200000000E   1C                   ENDIF
066300000000     C*
066400000000     C                   ENDSR
066500000000     C*------------------------------------------------------------------------*
066600000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
066700000000     C*------------------------------------------------------------------------*
066800000000     C     SETFIL        BEGSR
066900000000     C*
067000000000     C                   MOVEL     '1'           *IN43
067100000000     C*
067200000000     C* POSIZIONAMENTO ARCHIVIO
067300950301     C*---                 Z-ADDVCCVOC    KVTVOC
067400950301     C*---                 Z-ADDVCCPRG    KVTPRG
067500950301     C*---                 MOVE VCCTTV    KVTTTV
067600950301     C                   Z-ADD     DEPVOC        KVTVOC
067700950301     C                   Z-ADD     DEPPRG        KVTPRG
067800950301     C                   MOVE      DEPTTV        KVTTTV
067900000000     C                   Z-ADD     *ZERO         KVTCLI
068000000000     C                   Z-ADD     *ZERO         KVTCTR
068100000000     C                   Z-ADD     *ZERO         KVTFT1
068200000000     C                   Z-ADD     *ZERO         KVTFT2
068300000000     C                   MOVE      *BLANKS       KVTCTS
068400000000     C                   Z-ADD     *ZERO         KVTCAP
068500000000IF  1C     VCCTTV        IFEQ      'K'
068600000000     C                   MOVE      VCCKE1        KVTCLI
068700000000     C                   MOVE      VCCKE2        KVTCTR
068800000000E   1C                   ENDIF
068900000000IF  1C     VCCTTV        IFEQ      'F'
069000000000     C                   MOVE      VCCKE1        KVTFT1
069100000000     C                   MOVE      VCCKE2        KVTFT2
069200000000E   1C                   ENDIF
069300000000IF  1C     VCCTTV        IFEQ      'T'
069400000000     C                   MOVE      VCCKE1        KVTCTS
069500000000E   1C                   ENDIF
069600000000IF  1C     VCCTTV        IFEQ      'P'
069700000000     C                   MOVE      VCCKE1        KVTCAP
069800000000E   1C                   ENDIF
069900000000     C                   Z-ADD     VCCDDT        KVTDDT
070000000000     C     KEYEVD        SETLL     ECEVD01L                           99
070100000000     C*
070200000000     C  N99              EXSR      LETFIL
070300000000     C*
070400000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
070500000000IF  1C     *IN99         IFEQ      '1'
070600000000     C                   Z-ADD     1             SFLEOF
070700000000E   1C                   ENDIF
070800000000     C*
070900000000     C                   ENDSR
071000000000     C*------------------------------------------------------------------------*
071100000000     C* LETFIL - LEGGE RECORD SUCESSIVO
071200000000     C*------------------------------------------------------------------------*
071300000000     C     LETFIL        BEGSR
071400000000     C*
071500000000     C* LEGGO FINO A:
071600000000     C*     - FINE FILE
071700000000     C*     - TROVATO RECORD VALIDO
071800000000     C                   MOVEL     '0'           WRECOK            1
071900000000DO  1C     *IN99         DOWEQ     *OFF
072000000000     C     WRECOK        ANDEQ     '0'
072100000000     C*
072200000000     C* LETTURA
072300000000     C                   READ      ECEVD01L                               99
072400000000     C*
072500000000     C* FINE FILE  O TABELLA DIVERSA
072600000000IF  2C     *IN99         IFEQ      '1'
072700000000     C                   Z-ADD     1             SFLEOF
072800000000X   2C                   ELSE
072900000000     C                   EXSR      CHKREC
073000000000E   2C                   ENDIF
073100000000     C*
073200000000E   1C                   ENDDO
073300000000     C*
073400000000     C                   ENDSR
073500000000     C*------------------------------------------------------------------------*
073600000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
073700000000     C*------------------------------------------------------------------------*
073800000000     C     CHKREC        BEGSR
073900000000     C*
074000000000     C                   MOVEL     '1'           WRECOK
074100000000     C*
074200000000     C* SELEZIONE PER  ====>  DATA DECORRENZA
074300000000IF  1C     DDTVCC        IFGT      *ZEROS
074400000000IF  2C     DDTVCC        IFNE      EVDDDT
074500000000     C                   MOVEL     '0'           WRECOK
074600000000E   2C                   ENDIF
074700000000E   1C                   ENDIF
074800010111     C*
074900010111     C* SELEZIONE PER  ====>  DATA VALIDITA' AL
075000010111     C     VALVCC        IFGT      *ZEROS
075100010111     C     EVDDDT        IFLE      VALVCC
075200010111     C     EVDDST        ANDGE     VALVCC
075300010111     C                   ELSE
075400010111     C                   MOVEL     '0'           WRECOK
075500010111     C                   ENDIF
075600010111     C                   ENDIF
075700000000     C*
075800000000     C* SELEZIONE PER  ====>  TIPO TARIFFA (K/F/C/T/P)
075900000000IF  1C     VCCTTV        IFNE      *BLANKS
076000000000     C     VCCTTV        ANDNE     EVDTTV
076100000000     C                   MOVEL     '0'           WRECOK
076200000000E   1C                   ENDIF
076300000000     C*
076400000000     C* INIZIO ELENCO DA  ====>  PROGRESSIVO
076500000000IF  1C     VCCPRG        IFNE      *ZERO
076600000000     C     VCCPRG        ANDGT     EVDPRG
076700000000     C                   MOVEL     '0'           WRECOK
076800000000E   1C                   ENDIF
076900000000     C* ---------------------------------------------------
077000000000IF  1C     VCCTTV        IFEQ      'K'
077100000000     C*
077200000000     C* CODICE CLIENTE
077300000000IF  2C     VCCKE1        IFNE      *BLANKS
077400000000     C     VCCKE1        ANDNE     *ZERO
077500000000     C     *LIKE         DEFINE    EVDCLI        VCCCLI
077600000000     C                   MOVE      VCCKE1        VCCCLI
077700000000IF  3C     VCCCLI        IFGT      EVDCLI
077800000000     C                   MOVEL     '0'           WRECOK
077900000000E   3C                   ENDIF
078000000000E   2C                   ENDIF
078100000000     C*
078200000000     C* TARIFFA CLIENTE
078300000000IF  2C     VCCKE2        IFNE      *BLANKS
078400000000     C     *LIKE         DEFINE    EVDCTR        VCCCTR
078500000000     C                   MOVE      VCCKE2        VCCCTR
078600000000IF  3C     VCCCTR        IFGT      EVDCTR
078700000000     C                   MOVEL     '0'           WRECOK
078800000000E   3C                   ENDIF
078900000000E   2C                   ENDIF
079000000000     C*
079100000000E   1C                   ENDIF
079200000000     C* ---------------------------------------------------
079300000000IF  1C     VCCTTV        IFEQ      'F'
079400000000     C*
079500000000     C* FILIALE 1
079600000000IF  2C     VCCK11        IFNE      *BLANKS
079700000000     C     VCCK11        ANDNE     *ZERO
079800000000     C     *LIKE         DEFINE    EVDFT1        VCCFT1
079900000000     C                   MOVE      VCCK11        VCCFT1
080000000000IF  3C     VCCFT1        IFGT      EVDFT1
080100000000     C                   MOVEL     '0'           WRECOK
080200000000E   3C                   ENDIF
080300000000E   2C                   ENDIF
080400000000     C*
080500000000     C* FILIALE 2
080600000000IF  2C     VCCKE2        IFNE      *BLANKS
080700000000     C     VCCKE2        ANDNE     *ZERO
080800000000     C     *LIKE         DEFINE    EVDFT2        VCCFT2
080900000000     C                   MOVE      VCCKE2        VCCFT2
081000000000IF  3C     VCCFT2        IFGT      EVDFT2
081100000000     C                   MOVEL     '0'           WRECOK
081200000000E   3C                   ENDIF
081300000000E   2C                   ENDIF
081400000000     C*
081500000000E   1C                   ENDIF
081600000000     C* ---------------------------------------------------
081700000000IF  1C     VCCTTV        IFEQ      'P'
081800000000     C*
081900000000     C* C.A.P.
082000000000IF  2C     VCCK21        IFNE      *BLANKS
082100000000     C     VCCK21        ANDNE     *ZERO
082200000000     C     *LIKE         DEFINE    EVDCAP        VCCCAP
082300000000     C                   MOVE      VCCK21        VCCCAP
082400000000IF  3C     VCCCAP        IFGT      EVDCAP
082500000000     C                   MOVEL     '0'           WRECOK
082600000000E   3C                   ENDIF
082700000000E   2C                   ENDIF
082800000000     C*
082900000000E   1C                   ENDIF
083000000000     C* ---------------------------------------------------
083100000000IF  1C     VCCTTV        IFEQ      'T'
083200000000     C*
083300000000     C* CODICE TASSAZIONE
083400000000IF  2C     VCCKE1        IFNE      *BLANKS
083500000000     C     *LIKE         DEFINE    EVDCTS        VCCCTS
083600000000     C                   MOVE      VCCKE1        VCCCTS
083700000000IF  3C     VCCCTS        IFGT      EVDCTS
083800000000     C                   MOVEL     '0'           WRECOK
083900000000E   3C                   ENDIF
084000000000E   2C                   ENDIF
084001091021     C*
084002091021E   1C                   ENDIF
084003091021     C* SOLO TARIFFE DELLA VOCE SCELTA
084004091021     C                   if        VCCVOC <> 0
084005091021     C                   if        evdvoc <> vccvoc
084006091021     C                   MOVEL     '0'           WRECOK
084007091021     c                   else
084008091021     c                   if        evdprg <> vccprg AND VCCPRG <> 0
084009091021     C                   MOVEL     '0'           WRECOK
084010091021     c                   end
084011091021     c                   end
084012091021     c                   end
084300000000     C*
084400000000     C                   ENDSR
084500000000     C*------------------------------------------------------------------------*
084600000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
084700000000     C*------------------------------------------------------------------------*
084800000000     C     IRESFL        BEGSR
084900000000     C*
085000000000IF  1C     EVDATB        IFNE      *BLANKS
085100000000     C                   MOVEL     'A'           VSCATB
085200000000X   1C                   ELSE
085300000000     C                   MOVEL     ' '           VSCATB
085400000000E   1C                   ENDIF
085500000000     C*
085600000000     C                   Z-ADD     EVDVOC        VSCVOC
085700000000     C                   Z-ADD     EVDPRG        VSCPRG
085800000000     C                   MOVE      EVDTTV        VSCTTV
085900000000     C                   MOVE      *BLANKS       VSCCLI
086000000000     C                   MOVE      *BLANKS       VSCCTR
086100000000IF  1C     VSCTTV        IFEQ      'K'
086200000000     C                   MOVE      EVDCLI        VSCCLI
086300000000     C                   MOVEL     EVDCTR        VSCCTR
086400000000E   1C                   ENDIF
086500000000IF  1C     VSCTTV        IFEQ      'F'
086600000000     C                   MOVE      EVDFT1        VSCCLI
086700000000     C                   MOVEL     EVDFT2        VSCCTR
086800000000E   1C                   ENDIF
086900000000IF  1C     VSCTTV        IFEQ      'P'
087000000000     C                   MOVE      EVDCAP        VSCCLI
087100000000E   1C                   ENDIF
087200000000IF  1C     VSCTTV        IFEQ      'T'
087300000000     C                   MOVE      EVDCTS        VSCCLI
087400000000E   1C                   ENDIF
087500000000     C                   Z-ADD     EVDDDT        G02INV
087600000000     C                   Z-ADD     *ZEROS        G02DAT
087700000000     C                   MOVEL     '3'           G02ERR
087800000000     C                   CALL      'XSRDA8'
087900000000     C                   PARM                    WLBDAT
088000000000     C                   Z-ADD     G02DAT        VSCDDT
088100000000     C                   Z-ADD     EVDDST        G02INV
088200000000     C                   Z-ADD     *ZEROS        G02DAT
088300000000     C                   MOVEL     '3'           G02ERR
088400000000     C                   CALL      'XSRDA8'
088500000000     C                   PARM                    WLBDAT
088600000000     C                   Z-ADD     G02DAT        VSCDST
088700940908     C*
088800940908     C                   Z-ADD     *ZEROS        VSCIOP
088900940908     C                   MOVEL     *BLANKS       VSCSEG
089000940908     C     EVDITR        IFGT      *ZEROS
089100940908     C                   Z-ADD     EVDITR        VSCIOP
089200940908     C                   ENDIF
089300940908     C     EVDPER        IFGT      *ZEROS
089400940908     C                   MOVEL     EVDSEG        VSCSEG
089500940908     C                   Z-ADD     EVDPER        VSCIOP
089600940908     C                   ENDIF
089700000000     C                   Z-ADD     EVDSGI        VSCSGI
089800000000     C*
089900000000     C                   ENDSR
090000000000     C*------------------------------------------------------------------------*
090100000000     C* LANPGM - LANCIA UN PROGRAMMA
090200000000     C*------------------------------------------------------------------------*
090300000000     C     LANPGM        BEGSR
090400000000     C*--------------------
090500000000     C* PARAMETRI INFORMATIVI
090600000000     C*--------------------
090700000000     C                   MOVEL     WOP0          D60OP0
090800000000     C                   MOVEL     WOP1          D60OP1
090900000000     C                   MOVEL     '0'           D60F03
091000000000     C                   MOVEL     '0'           D60ERR
091100000000     C                   MOVEL     *BLANKS       D60MSG
091200000000     C*--------------------
091300000000     C* PARAMETRI
091400000000     C*--------------------
091500000000     C                   Z-ADD     WVOC          D60VOC
091600000000     C                   Z-ADD     WPRG          D60PRG
091700000000     C                   Z-ADD     WDDV          D60DDV
091800000000     C                   MOVEL     WTTV          D60TTV
091900000000     C                   Z-ADD     WCLI          D60CLI
092000000000     C                   MOVEL     WCTR          D60CTR
092100000000     C                   Z-ADD     WFT1          D60FT1
092200000000     C                   Z-ADD     WFT2          D60FT2
092300000000     C                   MOVE      WCTS          D60CTS
092400000000     C                   Z-ADD     WCAP          D60CAP
092500000000     C                   Z-ADD     WDDT          D60DDT
092600000000     C                   Z-ADD     WSGI          D60SGI
092700000000     C* LANCIO
092800000000     C                   MOVEL     DS60          KPJBU
092900000000     C                   CALL      WPGM                                 99
093000000000     C                   PARM                    KPJBA
093100000000     C                   MOVEL     KPJBU         DS60
093200000000     C*
093300000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
093400000000IF  1C     D60F03        IFEQ      '1'
093500000000     C                   MOVEL     '1'           $F03
093600000000     C                   MOVEL     '1'           $ERR
093700000000     C                   MOVEL     '1'           $FINE
093800000000     C*
093900000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
094000000000X   1C                   ELSE
094100000000     C                   MOVEL     D60MSG        $MSG
094200000000     C                   EXSR      CARMSG
094300000000E   1C                   ENDIF
094400000000     C*
094500000000     C                   ENDSR
094600020923     C*-------------------------------------------------------------------------
094700020923     C* REPERISCE I DATI UTENTE
094800020923     C*-------------------------------------------------------------------------
094900020923     C     REPDATIUTE    BEGSR
095000020923     C*
095100020923     C* INIZIALIZZA VARIABILI DI WRK
095200020923     C                   CLEAR                   TIBS34DS
095300020923     C                   CLEAR                   AZUTEDS
095400020923     C                   CLEAR                   DDATIUTE
095500020923     C*
095600020923     C     *DTAARA       DEFINE    §azute        azuteds
095700020923     C     *DTAARA       DEFINE    §datiute      ddatiute
095800020923     C                   IN(E)     *DTAARA
095900020923if  1C                   IF        %Error
096000020923     c                   EVAL      I34Tla = 'L'
096100020923     C                   CALL      'TIBS34R'
096200020923     C                   PARM                    Tibs34Ds
096300020923     C                   IN        *DTAARA
096400020923e   1C                   ENDIF
096500020923     C*
096600020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
096700020923     C                   MOVEL(P)  RSUT          DSFIRS           20
096800020923     C*
096900020923     C                   ENDSR
097000000000     C*------------------------------------------------------------------------*
097100000000     C*  *INZSR - ROUTINE INIZIALE
097200000000     C*------------------------------------------------------------------------*
097300000000     C     *INZSR        BEGSR
097400000000     C*
097500000000     C* RICEVIMENTO PARAMETRI
097600000000     C     *ENTRY        PLIST
097700000000     C                   PARM                    KPJBA
097800000000     C                   MOVEL     KPJBU         DS60
097900000000     C*
098000000000     C* DEFINIZIONE KLIST
098100000000     C     *LIKE         DEFINE    EVDVOC        KVTVOC
098200000000     C     *LIKE         DEFINE    EVDPRG        KVTPRG
098300000000     C     *LIKE         DEFINE    EVDTTV        KVTTTV
098400000000     C     *LIKE         DEFINE    EVDFT1        KVTFT1
098500000000     C     *LIKE         DEFINE    EVDFT2        KVTFT2
098600000000     C     *LIKE         DEFINE    EVDCLI        KVTCLI
098700000000     C     *LIKE         DEFINE    EVDCTR        KVTCTR
098800000000     C     *LIKE         DEFINE    EVDCTS        KVTCTS
098900000000     C     *LIKE         DEFINE    EVDCAP        KVTCAP
099000000000     C     *LIKE         DEFINE    EVDDDT        KVTDDT
099100000000     C*
099200000000     C     *LIKE         DEFINE    D60OP0        WOP0
099300000000     C     *LIKE         DEFINE    D60OP1        WOP1
099400000000     C     *LIKE         DEFINE    D60VOC        WVOC
099500000000     C     *LIKE         DEFINE    D60PRG        WPRG
099600000000     C     *LIKE         DEFINE    D60DDV        WDDV
099700000000     C     *LIKE         DEFINE    D60TTV        WTTV
099800000000     C     *LIKE         DEFINE    D60CLI        WCLI
099900000000     C     *LIKE         DEFINE    D60CTR        WCTR
100000000000     C     *LIKE         DEFINE    D60FT1        WFT1
100100000000     C     *LIKE         DEFINE    D60FT2        WFT2
100200000000     C     *LIKE         DEFINE    D60CTS        WCTS
100300000000     C     *LIKE         DEFINE    D60CAP        WCAP
100400000000     C     *LIKE         DEFINE    D60DDT        WDDT
100500000000     C*
100600000000     C* DEFINIZIONE DEPOSITI
100700000000     C     *LIKE         DEFINE    VCCVOC        DEPVOC
100800000000     C     *LIKE         DEFINE    VCCPRG        DEPPRG
100900000000     C     *LIKE         DEFINE    VCCTTV        DEPTTV
101000000000     C     *LIKE         DEFINE    VCCKE1        DEPKE1
101100000000     C     *LIKE         DEFINE    VCCKE2        DEPKE2
101200000000     C     *LIKE         DEFINE    VCCDDT        DEPDDT
101300010111     C     *LIKE         DEFINE    VCCVAL        DEPVAL
101400000000     C*
101500000000     C* CHIAVE LETTURA ECEVD01L - PARZIALE
101600000000     C     KEYEVD        KLIST
101700000000     C                   KFLD                    KVTVOC
101800000000     C                   KFLD                    KVTPRG
101900000000     C                   KFLD                    KVTTTV
102000000000     C                   KFLD                    KVTFT1
102100000000     C                   KFLD                    KVTFT2
102200000000     C                   KFLD                    KVTCLI
102300000000     C                   KFLD                    KVTCTR
102400000000     C                   KFLD                    KVTCTS
102500000000     C                   KFLD                    KVTCAP
102600000000     C                   KFLD                    KVTDDT
102700000000     C*
102800000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
102900000000     C                   MOVE      D60OP0        INTGES            2
103000000000IF  1C     INTGES        IFEQ      '02'
103100000000     C                   MOVEL     'G'           $TGES             1
103200000000     C                   MOVEL     DESGES        VTDOPE
103300000000X   1C                   ELSE
103400000000     C                   MOVEL     'I'           $TGES             1
103500000000     C                   MOVEL     DESINT        VTDOPE
103600000000E   1C                   ENDIF
103700000000     C*
103800000000     C* ACCENDO UN UNDICATORE A SECONDA DEL TIPO TARIFFA DA UTILIZZARE
103900000000     C     D60TTV        COMP      'K'                                    01
104000000000     C     D60TTV        COMP      'F'                                    02
104100000000     C     D60TTV        COMP      'C'                                    03
104200000000     C     D60TTV        COMP      'P'                                    04
104300000000     C     D60TTV        COMP      'T'                                    05
104400000000     C*
104500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
104600000000     C                   EXSR      INZSTD
104700000000     C*
104800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
104900000000     C                   EXSR      INZSFL
105000000000     C*
105100000000     C* IMPOSTO CAMPI TESTATA VIDEO
105200000000     C                   MOVEL     DSFIRS        VTDRSU
105300000000     C*
105400000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
105500000000     C* SE RICHIAMATO DA ALTRO PGM IMPOSTO I CAMPI PASSATI
105600000000     C                   MOVE      *BLANKS       VCCKE1
105700000000     C                   MOVE      *BLANKS       VCCKE2
105800000000     C                   MOVEL     D60OP0        $UNO              1
105900000000     C     $UNO          IFEQ      'T'
106000000000     C                   Z-ADD     *ZEROS        VCCVOC
106100000000     C                   Z-ADD     *ZEROS        VCCPRG
106200000000     C                   MOVE      *BLANKS       VCCTTV
106300000000     C                   Z-ADD     *ZEROS        VCCDDT
106400010111     C                   Z-ADD     *ZEROS        VCCVAL
106500000000     C                   ELSE
106600000000     C                   Z-ADD     D60VOC        VCCVOC
106700000000     C                   Z-ADD     D60PRG        VCCPRG
106800000000     C                   MOVE      D60TTV        VCCTTV
106900000000IF  1C     VCCTTV        IFEQ      'K'
107000000000     C                   MOVE      D60CLI        VCCKE1
107100000000     C                   MOVE      D60CTR        VCCKE2
107200000000E   1C                   ENDIF
107300000000IF  1C     VCCTTV        IFEQ      'F'
107400000000     C                   MOVE      D60FT1        VCCKE1
107500000000     C                   MOVE      D60FT2        VCCKE2
107600000000E   1C                   ENDIF
107700000000IF  1C     VCCTTV        IFEQ      'T'
107800000000     C                   MOVE      D60CTS        VCCKE1
107900000000E   1C                   ENDIF
108000000000IF  1C     VCCTTV        IFEQ      'P'
108100000000     C                   MOVE      D60CAP        VCCKE1
108200000000E   1C                   ENDIF
108300000000     C                   Z-ADD     D60DDT        VCCDDT
108400000000     C                   Z-ADD     D60DDT        G02DAT
108500000000     C                   Z-ADD     *ZEROS        G02INV
108600000000     C                   MOVEL     '0'           G02ERR
108700000000     C                   CALL      'XSRDA8'
108800000000     C                   PARM                    WLBDAT
108900000000     C                   Z-ADD     G02INV        DDTVCC
109000000000     C                   Z-ADD     D60VOC        DEPVOC
109100000000     C                   Z-ADD     D60PRG        DEPPRG
109200000000     C                   MOVE      D60TTV        DEPTTV
109300000000     C                   MOVE      VCCKE1        DEPKE1
109400000000     C                   MOVE      VCCKE2        DEPKE2
109500000000     C                   Z-ADD     D60DDT        DEPDDT
109600000000     C                   ENDIF
109700000000     C*
109800000000     C* EMETTO VIDEATA SUBFILE
109900000000     C                   EXSR      CARVS
110000000000     C*
110100000000     C                   ENDSR
110200000000     C*------------------------------------------------------------------------*
110300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
110400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
110500000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
110600000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
110700000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
110800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
110900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
111000000000     C*------------------------------------------------------------------------*
111100000000     C* INZSTD - ROUTINE INIZIALE STANDARD
111200000000     C*------------------------------------------------------------------------*
111300000000     C     INZSTD        BEGSR
111400000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
111500000000     C                   CALL      'XKEYS'
111600000000     C                   PARM                    DSKEYS
111700000000     C                   MOVEA     KEYRIG        KEYS
111800000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
111900000000     C     XMSGPA        PLIST
112000000000     C                   PARM                    DSMSMS
112100000000     C                   PARM                    DSMSPG
112200000000     C                   PARM                    DSMSKE
112300000000     C                   PARM                    DSMSER
112400000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
112500000000     C                   MOVEL     'TREC63R'     DSMSPG
112600000000     C                   MOVEL     'TREC63R'     VMCQUE
112700000000     C                   MOVEL     *BLANKS       VMCKEY
112800000000     C                   MOVEL     *BLANKS       DSMSMS
112900000000     C* INIZIALIZZO FLAG CONTROLLO PGM
113000000000     C                   MOVEL     '0'           $ERR              1
113100000000     C                   MOVEL     '0'           $F03              1
113200000000     C                   MOVEL     *BLANKS       $GES              2
113300000000     C                   MOVEL     *BLANKS       $MSG             80
113400000000     C                   MOVEL     '0'           $FINE             1
113500000000     C                   MOVEL     '0'           $TESVD            1
113600000000     C* LANCIO ROUTINE PER VERIFICARE SE
113700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
113800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
113900020923     C                   EXSR      REPDATIUTE
114000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
114100000000     C                   BITOFF    '01234567'    POSMI1            1
114200000000     C                   BITOFF    '01234567'    POSMI2            1
114300000000     C                   BITOFF    '01234567'    POSMA1            1
114400000000     C                   BITOFF    '01234567'    POSMA2            1
114500000000     C                   BITON     '34'          POSMI1            1
114600000000     C                   BITON     '7'           POSMI2            1
114700000000     C                   BITON     '34'          POSMA1            1
114800000000     C                   BITON     '0'           POSMA2            1
114900000000     C                   MOVE      POSMI2        POSMIN            2
115000000000     C                   MOVEL     POSMI1        POSMIN
115100000000     C                   MOVE      POSMA2        POSMAX            2
115200000000     C                   MOVEL     POSMA1        POSMAX
115300000000     C                   ENDSR
115400000000     C*------------------------------------------------------------------------*
115500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
115600000000     C*------------------------------------------------------------------------*
115700000000     C     FUNSTD        BEGSR
115800000000     C                   SELECT
115900000000     C     KEY           WHENEQ    F3
116000000000     C                   EXSR      F03STD
116100000000     C     KEY           WHENEQ    F12
116200000000     C                   EXSR      F12STD
116300000000     C     KEY           WHENEQ    F24
116400000000     C                   EXSR      F24STD
116500000000     C                   OTHER
116600000000     C                   MOVEL     MSG(2)        $MSG
116700000000     C                   EXSR      CARMSG
116800000000     C                   ENDSL
116900000000     C                   ENDSR
117000000000     C*------------------------------------------------------------------------*
117100000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
117200000000     C*------------------------------------------------------------------------*
117300000000     C     F03STD        BEGSR
117400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
117500000000     C                   MOVEL     '1'           $F03
117600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
117700000000     C                   MOVEL     '1'           $FINE
117800000000     C                   ENDSR
117900000000     C*------------------------------------------------------------------------*
118000000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
118100000000     C*------------------------------------------------------------------------*
118200000000     C     F12STD        BEGSR
118300000000     C* IMPOSTO FLAG DI FIN PGM
118400000000     C                   MOVEL     '1'           $FINE
118500000000     C                   ENDSR
118600000000     C*------------------------------------------------------------------------*
118700000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
118800000000     C*------------------------------------------------------------------------*
118900000000     C     F24STD        BEGSR
119000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
119100000000     C                   EXSR      OUTFUN
119200000000     C                   ENDSR
119300000000     C*------------------------------------------------------------------------*
119400000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
119500000000     C*------------------------------------------------------------------------*
119600000000     C     INZSFL        BEGSR
119700000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
119800000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
119900000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
120000000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
120100000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
120200000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
120300000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
120400000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
120500000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
120600000000     C                   Z-ADD     12            SFLPAG
120700000000     C                   ENDSR
120800000000     C*------------------------------------------------------------------------*
120900000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
121000000000     C*------------------------------------------------------------------------*
121100000000     C     FUNSFL        BEGSR
121200000000     C                   SELECT
121300000000     C     KEY           WHENEQ    ENTER
121400000000     C                   EXSR      ENTSFL
121500000000     C     KEY           WHENEQ    F3
121600000000     C                   EXSR      F03STD
121700000000     C     KEY           WHENEQ    F5
121800000000     C                   EXSR      F05STD
121900000000     C     KEY           WHENEQ    F12
122000000000     C                   EXSR      F12STD
122100000000     C     KEY           WHENEQ    F17
122200000000     C                   EXSR      F17STD
122300000000     C     KEY           WHENEQ    F18
122400000000     C                   EXSR      F18STD
122500000000     C     KEY           WHENEQ    F23
122600000000     C                   EXSR      F23STD
122700000000     C     KEY           WHENEQ    F24
122800000000     C                   EXSR      F24STD
122900000000     C     KEY           WHENEQ    ROLLUP
123000000000     C                   EXSR      RUPSTD
123100000000     C     KEY           WHENEQ    ROLLDN
123200000000     C                   EXSR      RDNSTD
123300000000     C                   OTHER
123400000000     C                   MOVEL     MSG(2)        $MSG
123500000000     C                   EXSR      CARMSG
123600000000     C                   ENDSL
123700000000     C                   ENDSR
123800000000     C*------------------------------------------------------------------------*
123900000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
124000000000     C*------------------------------------------------------------------------*
124100000000     C     F05STD        BEGSR
124200000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
124300000000     C                   EXSR      POSSFL
124400000000     C                   ENDSR
124500000000     C*------------------------------------------------------------------------*
124600000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
124700000000     C*------------------------------------------------------------------------*
124800000000     C     F17STD        BEGSR
124900000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
125000000000     C                   Z-ADD     1             VCCNBR
125100000000     C                   ENDSR
125200000000     C*------------------------------------------------------------------------*
125300000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
125400000000     C*------------------------------------------------------------------------*
125500000000     C     F18STD        BEGSR
125600000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
125700000000     C                   Z-ADD     SFLULT        VCCNBR
125800000000     C                   ENDSR
125900000000     C*------------------------------------------------------------------------*
126000000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
126100000000     C*------------------------------------------------------------------------*
126200000000     C     F23STD        BEGSR
126300000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
126400000000     C                   EXSR      OUTOPZ
126500000000     C                   ENDSR
126600000000     C*------------------------------------------------------------------------*
126700000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
126800000000     C*------------------------------------------------------------------------*
126900000000     C     RUPSTD        BEGSR
127000000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
127100000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
127200000000     C     CURSOR        IFGE      POSMIN
127300000000     C     CURSOR        ANDLE     POSMAX
127400000000     C                   ELSE
127500000000     C                   EXSR      CARSFL
127600000000     C                   ENDIF
127700000000     C                   ENDSR
127800000000     C*------------------------------------------------------------------------*
127900000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
128000000000     C*------------------------------------------------------------------------*
128100000000     C     RDNSTD        BEGSR
128200000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
128300000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
128400000000     C     CURSOR        IFGE      POSMIN
128500000000     C     CURSOR        ANDLE     POSMAX
128600000000     C                   ELSE
128700000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
128800000000     C                   MOVEL     '1'           $ERR
128900000000     C                   MOVEL     MSG(5)        $MSG
129000000000     C                   EXSR      CARMSG
129100000000     C                   ENDIF
129200000000     C                   ENDSR
129300000000     C*------------------------------------------------------------------------*
129400000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
129500000000     C*------------------------------------------------------------------------*
129600000000     C     ENTSFL        BEGSR
129700000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
129800000000     C                   EXSR      NEWPOS
129900000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
130000000000     C     $ERR          IFEQ      '0'
130100000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
130200000000     C     SWPOSI        IFNE      0
130300000000     C                   EXSR      POSSFL
130400000000     C                   ELSE
130500000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
130600000000     C     SFLULT        IFNE      0
130700000000     C                   EXSR      OPZSFL
130800000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
130900000000     C     SWMODI        IFNE      0
131000000000     C                   EXSR      POSSFL
131100000000     C                   ENDIF
131200000000     C                   ENDIF
131300000000     C                   ENDIF
131400000000     C                   ENDIF
131500000000     C                   ENDSR
131600000000     C*------------------------------------------------------------------------*
131700000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
131800000000     C*------------------------------------------------------------------------*
131900000000     C     POSSFL        BEGSR
132000000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
132100000000     C                   EXSR      CLRSFL
132200000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
132300000000     C                   EXSR      SETFIL
132400000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
132500000000     C     SFLEOF        IFEQ      0
132600000000     C                   EXSR      CARSFL
132700000000     C                   ENDIF
132800000000     C                   ENDSR
132900000000     C*------------------------------------------------------------------------*
133000000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
133100000000     C*------------------------------------------------------------------------*
133200000000     C     OPZSFL        BEGSR
133300000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
133400000000     C                   MOVEL     'S'           OPZOK             1
133500000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
133600000000     C                   Z-ADD     0             SFLVAR            5 0
133700000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
133800000000     C                   Z-ADD     0             SFLERR
133900000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
134000000000     C                   Z-ADD     SFLULT        WFLULT
134100000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
134200000000     C                   READC     EC63DVS                                49
134300000000     C     *IN49         DOWEQ     '0'
134400941121     C     $ERR          ANDEQ     '0'
134500000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
134600000000     C                   ADD       1             SFLVAR
134700000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
134800000000     C     OPZOK         IFEQ      'S'
134900000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
135000000000     C     VSCOPZ        IFNE      *BLANKS
135100000000     C                   EXSR      CONOPZ
135200000000     C     OPZOK         IFEQ      'S'
135300941121     C     $ERR          ANDEQ     '0'
135400000000     C                   EXSR      GESOPZ
135500000000     C                   ENDIF
135600000000     C                   ENDIF
135700000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
135800000000     C*            2. PULIZIA SCELTA IN SFL
135900000000     C     OPZOK         IFEQ      'S'
136000000000     C                   MOVEL     '0'           *IN41
136100000000     C                   MOVEL     *BLANKS       VSCOPZ
136200000000     C                   ELSE
136300000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
136400000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
136500000000     C                   MOVEL     '1'           *IN41
136600000000     C                   Z-ADD     SFLULT        SFLERR
136700000000     C                   ENDIF
136800000000     C                   ELSE
136900000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
137000000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
137100000000     C                   MOVEL     '1'           *IN41
137200000000     C                   ENDIF
137300000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
137400000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
137500000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
137600000000     C     OPZOK         IFNE      'S'
137700000000     C                   Z-ADD     SFLERR        VCCNBR
137800000000     C                   ELSE
137900000000     C                   Z-ADD     SFLULT        VCCNBR
138000000000     C                   ENDIF
138100000000     C* AGGIORNO RECORD SFL
138200000000     C                   UPDATE    EC63DVS
138300941121     C     $ERR          IFEQ      '0'
138400000000     C                   READC     EC63DVS                                49
138500941121     C                   ENDIF
138600000000     C                   ENDDO
138700000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
138800000000     C                   Z-ADD     WFLULT        SFLULT
138900000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
139000000000     C     SFLVAR        IFEQ      0
139100000000     C                   MOVEL     '1'           $FINE
139200000000     C                   ENDIF
139300000000     C                   ENDSR
139400000000     C*------------------------------------------------------------------------*
139500000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
139600000000     C*------------------------------------------------------------------------*
139700000000     C     CLRSFL        BEGSR
139800000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
139900000000     C* (0=NON FINE FILE  1=FINE FILE)
140000000000     C                   Z-ADD     0             SFLEOF
140100000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
140200000000     C                   Z-ADD     0             SFLULT
140300000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
140400000000     C                   MOVEL     '1'           *IN40
140500000000     C                   WRITE     EC63DVC
140600000000     C                   MOVEL     '0'           *IN40
140700000000     C                   ENDSR
140800000000     C*------------------------------------------------------------------------*
140900000000     C* CARSFL - CARICAMENTO SFL
141000000000     C*------------------------------------------------------------------------*
141100000000     C     CARSFL        BEGSR
141200000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
141300000000     C     SFLULT        IFEQ      0
141400000000     C                   EXSR      SETFIL
141500000000     C                   ENDIF
141600000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
141700000000     C                   Z-ADD     SFLULT        WFLULT
141800000000     C* CICLO:
141900000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
142000000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
142100000000     C                   Z-ADD     1             SFLRIG
142200000000     C     SFLRIG        DOWLE     SFLPAG
142300000000     C     SFLEOF        ANDEQ     0
142400000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
142500000000     C                   MOVEL     *BLANKS       VSCOPZ
142600000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
142700000000     C                   EXSR      IRESFL
142800000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
142900000000     C                   ADD       1             SFLULT
143000000000     C* SCRITTURA RECORD SFL
143100000000     C                   WRITE     EC63DVS
143200000000     C                   ADD       1             SFLRIG
143300000000     C* LETTURA FILE DA CARICARE
143400000000     C                   EXSR      LETFIL
143500000000     C                   ENDDO
143600000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
143700000000     C     WFLULT        IFNE      SFLULT
143800000000     C                   Z-ADD     WFLULT        VCCNBR
143900000000     C                   ADD       1             VCCNBR
144000000000     C                   ENDIF
144100000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
144200000000     C     SFLEOF        IFEQ      1
144300000000     C     SFLULT        ANDGT     *ZEROS
144400000000     C                   MOVEL     '1'           $ERR
144500000000     C                   MOVEL     MSG(6)        $MSG
144600000000     C                   EXSR      CARMSG
144700000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
144800000000     C     SFLULT        CHAIN     EC63DVS                            99
144900000000     C     *IN99         IFEQ      '0'
145000000000     C                   SETON                                        45
145100000000     C                   UPDATE    EC63DVS
145200000000     C                   SETOFF                                       45
145300000000     C                   ENDIF
145400000000     C                   ENDIF
145500000000     C                   ENDSR
145600000000     C*------------------------------------------------------------------------*
145700000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
145800000000     C*------------------------------------------------------------------------*
145900000000     C     OUTSFL        BEGSR
146000000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
146100000000     C     MSGULT        IFNE      *ZEROS
146200000000     C                   EXSR      OUTMSG
146300000000     C                   ENDIF
146400000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
146500000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
146600000000     C     SFLEOF        IFEQ      0
146700000000     C                   MOVEL     '0'           *IN42
146800000000     C                   ELSE
146900000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
147000000000     C                   MOVEL     '1'           *IN42
147100000000     C                   ENDIF
147200000000     C* EMISSIONE SUBFILE
147300000000     C     SFLULT        IFLE      0
147400000000     C*  --> CASO DI SFL VUOTO
147500000000     C                   MOVEL     '0'           *IN43
147600000000     C                   WRITE     EC63DVV
147700000000     C                   WRITE     EC63DVC
147800000000     C                   READ      EC63DVC                                99
147900000000     C                   ELSE
148000000000     C*  --> CASO DI SFL CON RECORDS
148100000000     C                   WRITE     EC63DVC
148200000000     C                   READ      EC63DVC                                99
148300000000     C                   ENDIF
148400000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
148500000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
148600000000     C                   Z-ADD     FRTSFL        VCCNBR
148700000000     C                   ENDSR
148800000000     C*------------------------------------------------------------------------*
148900000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
149000000000     C*          CODA MESSAGGI DEL PGM
149100000000     C*------------------------------------------------------------------------*
149200000000     C     CARMSG        BEGSR
149300000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
149400000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
149500000000     C     MSGULT        IFEQ      0
149600000000     C                   MOVEL     '1'           *IN40
149700000000     C                   WRITE     EC63DVK
149800000000     C                   MOVEL     '0'           *IN40
149900000000     C                   ENDIF
150000000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
150100000000     C                   MOVEL     $MSG          DSMSMS
150200000000     C                   CALL      'XMSG'        XMSGPA
150300000000     C     DSMSER        IFEQ      0
150400000000     C                   MOVEL     DSMSKE        VMCKEY
150500000000     C                   ENDIF
150600000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
150700000000     C                   ADD       1             MSGULT            4 0
150800000000     C                   WRITE     EC63DVM
150900000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
151000000000     C                   MOVEL     *BLANKS       $MSG
151100000000     C                   ENDSR
151200000000     C*------------------------------------------------------------------------*
151300000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
151400000000     C*------------------------------------------------------------------------*
151500000000     C     OUTMSG        BEGSR
151600000000     C* VISUALIZZAZIONE MESSAGGIO
151700000000     C                   MOVEL     '1'           *IN44
151800000000     C                   WRITE     EC63DVK
151900000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
152000000000     C                   Z-ADD     *ZEROS        MSGULT
152100000000     C                   ENDSR
152200000000     C*------------------------------------------------------------------------*
152300000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
152400000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
152500000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
152600000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
152700000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
152800000000     C*------------------------------------------------------------------------*
152900000000     C     CARFUN        BEGSR
153000000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
153100000000     C     1             DO        31            I                 5 0
153200000000     C                   MOVEL     FUN(I)        FUNRIG
153300000000     C                   MOVEL     FUNFUN        FFU(I)
153400000000     C                   MOVEL     FUNDES        FDE(I)
153500020923     C     DUTLPO        IFEQ      'S'
153600000000     C     $TGES         IFEQ      'G'
153700000000     C                   MOVEL     FUNASG        FAT(I)
153800000000     C                   ELSE
153900000000     C                   MOVEL     FUNASI        FAT(I)
154000000000     C                   ENDIF
154100000000     C                   ELSE
154200000000     C     $TGES         IFEQ      'G'
154300000000     C                   MOVEL     FUNAFG        FAT(I)
154400000000     C                   ELSE
154500000000     C                   MOVEL     FUNAFI        FAT(I)
154600000000     C                   ENDIF
154700000000     C                   ENDIF
154800000000     C     FAT(I)        IFEQ      'E'
154900000000     C                   MOVEL     USER          WEDP              3
155000000000     C     WEDP          IFEQ      'EDP'
155100000000     C                   MOVEL     'S'           FAT(I)
155200000000     C                   ELSE
155300000000     C                   MOVEL     'N'           FAT(I)
155400000000     C                   ENDIF
155500000000     C                   ENDIF
155600000000     C                   ENDDO
155700000000     C                   ENDSR
155800000000     C*------------------------------------------------------------------------*
155900000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
156000000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
156100000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
156200000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
156300000000     C*------------------------------------------------------------------------*
156400000000     C     OUTFUN        BEGSR
156500000000     C     1             DO        8             I
156600000000     C                   MOVEL     *BLANKS       FUV(I)
156700000000     C                   ENDDO
156800000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
156900000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
157000000000     C     1             DO        31            I
157100000000     C     FAT(I)        IFEQ      'S'
157200000000     C     FDE(I)        ANDNE     *BLANKS
157300000000     C                   ADD       1             FUNNAT
157400000000     C                   ENDIF
157500000000     C                   ENDDO
157600000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
157700000000     C     FUNNAT        IFGT      8
157800000000     C                   MOVEL     'S'           FAT(24)
157900000000     C                   ENDIF
158000000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
158100000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
158200000000     C                   Z-ADD     8             FUNVIS            5 0
158300000000     C     FAT(23)       IFEQ      'S'
158400000000     C                   SUB       1             FUNVIS
158500000000     C                   ENDIF
158600000000     C     FAT(24)       IFEQ      'S'
158700000000     C                   SUB       1             FUNVIS
158800000000     C                   ENDIF
158900000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
159000000000     C                   Z-ADD     *ZEROS        W                 5 0
159100000000     C                   ADD       1             FUNULT
159200000000     C     FUNULT        IFGT      31
159300000000     C                   Z-ADD     1             FUNULT
159400000000     C                   ENDIF
159500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
159600000000     C     FUNULT        DO        31            I
159700000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
159800000000     C     W             IFLT      FUNVIS
159900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
160000000000     C     FAT(I)        IFEQ      'S'
160100000000     C     FDE(I)        ANDNE     *BLANKS
160200000000     C     I             IFEQ      23
160300000000     C     I             OREQ      24
160400000000     C                   ELSE
160500000000     C                   ADD       1             W
160600000000     C                   MOVEL     FDE(I)        FUV(W)
160700000000     C                   ENDIF
160800000000     C                   ENDIF
160900000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
161000000000     C                   Z-ADD     I             FUNULT
161100000000     C                   ENDIF
161200000000     C                   ENDDO
161300000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
161400000000     C                   MOVEL     FUV(1)        VFD001
161500000000     C                   MOVEL     FUV(2)        VFD002
161600000000     C                   MOVEL     FUV(3)        VFD003
161700000000     C                   MOVEL     FUV(4)        VFD004
161800000000     C                   MOVEL     FUV(5)        VFD005
161900000000     C                   MOVEL     FUV(6)        VFD006
162000000000     C     FAT(23)       IFEQ      'N'
162100000000     C     FAT(24)       ANDEQ     'N'
162200000000     C                   MOVEL     FUV(7)        VFD007
162300000000     C                   MOVEL     FUV(8)        VFD008
162400000000     C                   ELSE
162500000000     C     FAT(23)       IFEQ      'S'
162600000000     C     FAT(24)       ANDEQ     'S'
162700000000     C                   MOVEL     FDE(23)       VFD007
162800000000     C                   MOVEL     FDE(24)       VFD008
162900000000     C                   ELSE
163000000000     C                   MOVEL     FUV(7)        VFD007
163100000000     C     FAT(23)       IFEQ      'S'
163200000000     C                   MOVEL     FDE(23)       VFD008
163300000000     C                   ELSE
163400000000     C                   MOVEL     FDE(24)       VFD008
163500000000     C                   ENDIF
163600000000     C                   ENDIF
163700000000     C                   ENDIF
163800000000     C                   MOVEL     '1'           $TESVD
163900000000     C                   ENDSR
164000000000     C*------------------------------------------------------------------------*
164100000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
164200000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
164300000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
164400000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
164500000000     C*------------------------------------------------------------------------*
164600000000     C     CONFUN        BEGSR
164700000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
164800000000     C     1             DO        31            I
164900000000     C     KEY           IFEQ      KEYS(I)
165000000000     C* CONTROLLO SE IL TASTO E' ATTIVO
165100000000     C     FAT(I)        IFNE      'S'
165200000000     C                   MOVEL     MSG(1)        $MSG
165300000000     C                   EXSR      CARMSG
165400000000     C                   MOVEL     '1'           $ERR
165500000000     C                   ENDIF
165600000000     C                   ENDIF
165700000000     C                   ENDDO
165800000000     C                   ENDSR
165900000000     C*------------------------------------------------------------------------*
166000000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
166100000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
166200000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
166300000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
166400000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
166500000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
166600000000     C*------------------------------------------------------------------------*
166700000000     C     CAROPZ        BEGSR
166800000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
166900000000     C     1             DO        24            I
167000000000     C                   MOVEL     OPZ(I)        OPZRIG
167100000000     C                   MOVEL     OPZOPD        OPD(I)
167200000000     C                   MOVEL     OPZOPS        OPS(I)
167300000000     C                   MOVEL     OPZDES        ODE(I)
167400020923     C     DUTLPO        IFEQ      'S'
167500000000     C     $TGES         IFEQ      'G'
167600000000     C                   MOVEL     OPZASG        OAT(I)
167700000000     C                   ELSE
167800000000     C                   MOVEL     OPZASI        OAT(I)
167900000000     C                   ENDIF
168000000000     C                   ELSE
168100000000     C     $TGES         IFEQ      'G'
168200000000     C                   MOVEL     OPZAFG        OAT(I)
168300000000     C                   ELSE
168400000000     C                   MOVEL     OPZAFI        OAT(I)
168500000000     C                   ENDIF
168600000000     C                   ENDIF
168700000000     C     OAT(I)        IFEQ      'E'
168800000000     C                   MOVEL     USER          WEDP              3
168900000000     C     WEDP          IFEQ      'EDP'
169000000000     C                   MOVEL     'S'           OAT(I)
169100000000     C                   ELSE
169200000000     C                   MOVEL     'N'           OAT(I)
169300000000     C                   ENDIF
169400000000     C                   ENDIF
169500000000     C                   ENDDO
169600000000     C                   ENDSR
169700000000     C*------------------------------------------------------------------------*
169800000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
169900000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
170000000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
170100000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
170200000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
170300000000     C*------------------------------------------------------------------------*
170400000000     C     OUTOPZ        BEGSR
170500000000     C     1             DO        8             I
170600000000     C                   MOVEL     *BLANKS       OPV(I)
170700000000     C                   ENDDO
170800000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
170900000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
171000000000     C                   ADD       1             OPZULT
171100000000     C     OPZULT        IFGT      24
171200000000     C                   Z-ADD     1             OPZULT
171300000000     C                   END
171400000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
171500000000     C     1             DO        24            I
171600000000     C     OAT(I)        IFEQ      'S'
171700000000     C     ODE(I)        ANDNE     *BLANKS
171800000000     C                   ADD       1             OPZNAT
171900000000     C                   ENDIF
172000000000     C                   ENDDO
172100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
172200000000     C                   Z-ADD     0             W
172300000000     C     OPZULT        DO        24            I
172400000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
172500000000     C     W             IFLT      8
172600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
172700000000     C     OAT(I)        IFEQ      'S'
172800000000     C     ODE(I)        ANDNE     *BLANKS
172900000000     C                   ADD       1             W
173000000000     C                   MOVEL     ODE(I)        OPV(W)
173100000000     C                   ENDIF
173200000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
173300000000     C                   Z-ADD     I             OPZULT
173400000000     C                   ENDIF
173500000000     C                   ENDDO
173600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
173700000000     C                   MOVEL     OPV(1)        VOD001
173800000000     C                   MOVEL     OPV(2)        VOD002
173900000000     C                   MOVEL     OPV(3)        VOD003
174000000000     C                   MOVEL     OPV(4)        VOD004
174100000000     C                   MOVEL     OPV(5)        VOD005
174200000000     C                   MOVEL     OPV(6)        VOD006
174300000000     C                   MOVEL     OPV(7)        VOD007
174400000000     C                   MOVEL     OPV(8)        VOD008
174500000000     C                   MOVEL     '1'           $TESVD
174600000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
174700000000     C     OPZNAT        IFGT      8
174800000000     C                   MOVEL     'S'           FAT(23)
174900000000     C                   ENDIF
175000000000     C                   ENDSR
175100000000     C*------------------------------------------------------------------------*
175200000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
175300000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
175400000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
175500000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
175600000000     C*------------------------------------------------------------------------*
175700000000     C     CONOPZ        BEGSR
175800000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
175900000000     C                   MOVEL     *BLANKS       OPZOK             1
176000000000     C     1             DO        24            I                 5 0
176100000000     C     VSCOPZ        IFEQ      OPD(I)
176200000000     C     VSCOPZ        OREQ      OPS(I)
176300000000     C     OAT(I)        IFEQ      'S'
176400000000     C                   MOVEL     OPD(I)        VSCOPZ
176500000000     C                   MOVEL     'S'           OPZOK
176600000000     C                   ELSE
176700000000     C                   MOVEL     'N'           OPZOK             1
176800000000     C                   ENDIF
176900000000     C                   ENDIF
177000000000     C                   ENDDO
177100000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
177200000000     C     OPZOK         IFNE      'S'
177300000000     C                   MOVEL     MSG(3)        $MSG
177400000000     C                   EXSR      CARMSG
177500000000     C                   MOVEL     '1'           $ERR
177600000000     C                   MOVEL     'N'           OPZOK             1
177700000000     C                   ENDIF
177800000000     C                   ENDSR
177900000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
178000000000TREC63R- Tasto funzionale non valido.                                          1
178100000000TREC63R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
178200000000TREC63R- Opzione non valida.                                                   3
178300000000TREC63R- Opzione abilitata ma non gestita. Avvertire CED.                      4
178400000000TREC63R- Inizio elenco.                                                        5
178500000000TREC63R- Fine elenco.                                                          6
178600000000TREC63R- 7
178700000000TREC63R- 8
178800000000TREC63R- Immettere tipo tariffa 'K'=Cliente, 'F'=Filiale o 'C'=Cartello        9
178900000000TREC63R- 10
179000000000TREC63R- 11
179100000000TREC63R- Codice voce annullato, consentito solo il ripristino                 12
179200000000TREC63R- Codice voce non annullato, impossibile ripristinare                  13
179300000000TREC63R- 14
179400000000TREC63R- 15
179500000000TREC63R- 16
179600000000TREC63R- 17
179700000000TREC63R- 18
179800000000TREC63R- 19
179900000000TREC63R- 20
180000000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
180100000000- 1-1 -                   -    NN     NN
180200000000- 2-2 -2=Modifica         -    SN     NN
180300010112- 3-3 -3=Duplica          -    SN     NN
180400000000- 4-4 -4=Cancellazione    -    SN     NN
180500000000- 5-5 -5=Visualizzazione  -    SS     SS
180600000000- 6-6 -                   -    NN     NN
180700000000- 7-7 -7=Ripristino       -    SN     NN
180800000000- 8-8 -                   -    NN     NN
180900000000- 9-9 -                   -    NN     NN
181000000000-10-  -                   -    NN     NN
181100000000-11-  -                   -    NN     NN
181200000000-12-  -                   -    NN     NN
181300000000-13-  -                   -    NN     NN
181400000000-14-  -                   -    NN     NN
181500000000-15-  -                   -    NN     NN
181600000000-16-  -                   -    NN     NN
181700000000-17-  -                   -    NN     NN
181800000000-18-  -                   -    NN     NN
181900000000-19-  -                   -    NN     NN
182000000000-20-  -                   -    NN     NN
182100000000-21-  -                   -    NN     NN
182200000000-22-  -                   -    NN     NN
182300000000-23-  -                   -    NN     NN
182400000000-24-  -                   -    NN     NN
182500000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
182600000000F1    -                   -    NN     NN
182700000000F2    -                   -    NN     NN
182800000000F3    -F3=Fine            -    SS     SS
182900000000F4    -                   -    NN     NN
183000000000F5    -F5=Rivisualizza    -    SS     SS
183100000000F6    -F6=Inserimento     -    SN     NN
183200000000F7    -                   -    NN     NN
183300000000F8    -                   -    NN     NN
183400000000F9    -                   -    NN     NN
183500000000F10   -                   -    NN     NN
183600000000F11   -                   -    NN     NN
183700000000F12   -F12=Ritorno        -    SS     SS
183800000000F13   -                   -    NN     NN
183900000000F14   -F14=Parzializza    -    SS     SS
184000000000F15   -                   -    NN     NN
184100000000F16   -                   -    NN     NN
184200961003F17   -F17=Inizio dati    -    NN     NN
184300961003F18   -F18=Fine dati      -    NN     NN
184400000000F19   -                   -    NN     NN
184500000000F20   -                   -    NN     NN
184600000000F21   -                   -    NN     NN
184700000000F22   -                   -    NN     NN
184800000000F23   -F23=Altre opzioni  -    NN     NN
184900000000F24   -F24=Altri tasti    -    NN     NN
185000000000ENTER -                   -    SS     SS
185100000000ROLLUP-                   -    SS     SS
185200000000ROLLDN-                   -    SS     SS
185300000000HELP  -                   -    NN     NN
185400000000PRINT -                   -    NN     NN
185500000000BSPACE-                   -    NN     NN
185600000000CLEAR -                   -    NN     NN

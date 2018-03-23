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
001500131121     FECEVD06L  IF   E           K DISK
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
010100131121     D  VCCKE1                 1      9
010200131121     D  VCCK11                 7      9
010300000000     D*------------------
010400000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
010500000000     D*------------------
010600000000     D DESGES          C                   CONST('** GESTIONE **')
010700000000     D DESINT          C                   CONST('INTERROGAZIONE')
010800000000     C*------------------------------------------------------------------------*
010900000000     C* MAIN LINE
011000000000     C*------------------------------------------------------------------------*
011100000000     C*
011200000000     C* CICLO FINO A CHE:
011300000000DO  1C     $FINE         DOUEQ     '1'
011400000000     C*
011500000000     C                   MOVEL     '0'           $ERR
011600000000     C*
011700000000     C* CONTROLLA GESTIONE VIDEO
011800000000IF  2C     $GES          IFEQ      'VS'
011900000000     C                   EXSR      GESVS
012000000000E   2C                   ENDIF
012100000000     C*
012200000000E   1C                   ENDDO
012300000000     C*
012400000000IF  1C     $F03          IFEQ      '1'
012500000000     C                   MOVEL     '1'           D60F03
012600000000E   1C                   ENDIF
012700000000     C* FINE PGM
012800000000     C                   MOVEL     DS60          KPJBU
012900000000     C                   SETON                                        LR
013000000000     C*------------------------------------------------------------------------*
013100000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
013200000000     C*------------------------------------------------------------------------*
013300000000     C     CARVS         BEGSR
013400000000     C*
013500000000     C                   MOVEL     'VS'          $GES
013600000000     C*
013700000000     C                   EXSR      CAROPZ
013800000000     C                   EXSR      OUTOPZ
013900000000     C                   MOVEA     FUN2          FUN
014000000000     C                   EXSR      CARFUN
014100000000     C                   EXSR      OUTFUN
014200000000     C*
014300000000     C                   WRITE     EC63DVT
014400000000     C                   WRITE     EC63DVO
014500000000     C                   WRITE     EC63DVF
014600000000     C*
014700000000     C                   EXSR      POSSFL
014800000000     C*
014900000000     C                   ENDSR
015000000000     C*------------------------------------------------------------------------*
015100000000     C* GESVS  - GESTIONE SUBFILE
015200000000     C*------------------------------------------------------------------------*
015300000000     C     GESVS         BEGSR
015400000000     C*
015500000000IF  1C     $TESVD        IFEQ      '1'
015600000000     C                   WRITE     EC63DVT
015700000000     C                   WRITE     EC63DVO
015800000000     C                   WRITE     EC63DVF
015900000000E   1C                   ENDIF
016000000000     C*
016100000000     C                   EXSR      OUTSFL
016200000000     C*
016300000000     C                   EXSR      CONFUN
016400000000     C*
016500000000IF  1C     $ERR          IFEQ      '0'
016600000000SL  2C                   SELECT
016700000000     C     KEY           WHENEQ    F6
016800000000     C                   EXSR      F06GES
016900000000     C     KEY           WHENEQ    F14
017000000000     C                   EXSR      F14GES
017100000000     C                   OTHER
017200000000     C                   EXSR      FUNSFL
017300000000E   2C                   ENDSL
017400000000E   1C                   ENDIF
017500000000     C*
017600000000     C                   ENDSR
017700000000     C*------------------------------------------------------------------------*
017800000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
017900000000     C*------------------------------------------------------------------------*
018000000000     C     F06GES        BEGSR
018100000000     C*
018200940726     C* IMPOSTO SUBITO IL CAMPO DELL'OPZIONE
018300940726     C                   MOVEL     'F06'         WOP1
018400940726     C*
018500000000     C* SE NON E' IMPOSTATO IL TIPO TARIFFA RICHIAMO PRIMA LA FINESTRA DI F14
018600940726     C* 26.7.94  LA FINESTRA DEVE APPARIRE SEMPRE
018700940726IF  1C********   VCCTTV    IFEQ *BLANKS
018800000000     C                   EXSR      F14GES
018900940726E   1C********             ENDIF
019000000000     C*
019100000000     C* LANCIO L'INSERIMENTO, SOLO SE C'E' IL TIPO TARIFFA
019200940726     C* E SE NON SONO USCITO CON F3=FINE
019300000000IF  1C     VCCTTV        IFNE      *BLANKS
019400940726     C     D60F03        ANDNE     '1'
019500000000     C                   Z-ADD     VCCVOC        WVOC
019600000000     C                   Z-ADD     VCCPRG        WPRG
019700000000     C                   Z-ADD     *ZERO         WDDV
019800000000     C                   MOVE      D60TTV        WTTV
019900000000     C                   Z-ADD     *ZEROS        WCLI
020000000000     C                   Z-ADD     *ZERO         WCTR
020100000000     C                   Z-ADD     *ZERO         WFT1
020200000000     C                   Z-ADD     *ZERO         WFT2
020300000000     C                   MOVE      *BLANKS       WCTS
020400131121     C                   MOVE      *BLANKS       Wcad
020500131121     C                   MOVE      *BLANKS       Wnaz
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
021600131121     C                   MOVEl     VCCKE1        WCAd
021700131121     C                   MOVEl     VCCKE2        Wnaz
021800000000     C                   ENDIF
021900000000     C     D60TTV        IFEQ      'T'
022000000000     C                   MOVE      VCCKE1        WCTS
022100000000     C                   ENDIF
022200000000     C                   Z-ADD     *ZEROS        WSGI             12 3
022300000000     C*
022400000000     C                   MOVEL     D60OP0        WOP0
022500000000     C                   MOVEL     'TREC64R'     WPGM              8
022600000000     C                   EXSR      LANPGM
022700000000     C*
022800000000     C                   EXSR      POSSFL
022900000000     C*
023000000000E   1C                   ENDIF
023100000000     C*
023200000000     C                   ENDSR
023300000000     C*------------------------------------------------------------------------*
023400000000     C* F14GES -GEST.TASTO FUNZ. F14 ===> "TOM CAT"   OPZIONI ELENCO
023500000000     C* LA PRIMA ROUTINE DEDICATA AL GLORIOSO AEREO DA CACCIA AMERICANO
023600000000     C*------------------------------------------------------------------------*
023700000000     C     F14GES        BEGSR
023800000000     C*
023900000000     C* LANCIO L'INSERIMENTO
024000000000     C                   Z-ADD     VCCVOC        WVOC
024100000000     C                   Z-ADD     VCCPRG        WPRG
024200000000     C                   Z-ADD     *ZEROS        WDDV
024300000000     C                   MOVE      D60TTV        WTTV
024400000000     C                   Z-ADD     *ZEROS        WCLI
024500000000     C                   Z-ADD     *ZERO         WCTR
024600000000     C                   Z-ADD     *ZERO         WFT1
024700000000     C                   Z-ADD     *ZERO         WFT2
024800000000     C                   MOVE      *BLANKS       WCTS
024900000000IF  1C     VCCTTV        IFEQ      'K'
025000000000     C                   MOVE      VCCKE1        WCLI
025100000000     C                   MOVE      VCCKE2        WCTR
025200000000E   1C                   ENDIF
025300000000IF  1C     VCCTTV        IFEQ      'F'
025400000000     C                   MOVE      VCCKE1        WFT1
025500000000     C                   MOVE      VCCKE2        WFT2
025600000000E   1C                   ENDIF
025700000000IF  1C     VCCTTV        IFEQ      'T'
025800000000     C                   MOVE      VCCKE1        WCTS
025900000000E   1C                   ENDIF
026000000000IF  1C     VCCTTV        IFEQ      'P'
026100131121     C                   MOVEl     VCCKE1        Wcad
026200131121     C                   MOVEl     VCCKE2        Wnaz
026300131121E   1C                   ENDIF
026400131121     C                   move      *blank        Wcad
026500131121     C                   move      *blank        Wnaz
026600000000     C                   Z-ADD     VCCDDT        WDDT
026700000000     C*
026800000000     C                   MOVEL     D60OP0        WOP0
026900000000     C***********          MOVEL'   '     WOP1             *LIVELLO PROGRAMMA
027000000000     C                   MOVEL     'TREC62R'     WPGM              8
027100000000     C                   EXSR      LANPGM
027200000000     C*
027300000000     C                   Z-ADD     D60VOC        VCCVOC
027400000000     C                   Z-ADD     D60PRG        VCCPRG
027500000000     C                   MOVE      D60TTV        VCCTTV
027600000000     C                   MOVE      D60DDT        VCCDDT
027700000000     C                   Z-ADD     D60DDT        G02DAT
027800000000     C                   Z-ADD     *ZEROS        G02INV
027900000000     C                   MOVEL     '0'           G02ERR
028000000000     C                   CALL      'XSRDA8'
028100000000     C                   PARM                    WLBDAT
028200000000     C                   Z-ADD     G02INV        DDTVCC
028300000000     C                   MOVE      *BLANKS       VCCKE1
028400000000     C                   MOVE      *BLANKS       VCCKE2
028500000000IF  1C     D60TTV        IFEQ      'K'
028600000000     C                   MOVE      D60CLI        VCCKE1
028700000000     C                   MOVE      D60CTR        VCCKE2
028800000000E   1C                   ENDIF
028900000000IF  1C     D60TTV        IFEQ      'F'
029000000000     C                   MOVE      D60FT1        VCCKE1
029100000000     C                   MOVE      D60FT2        VCCKE2
029200000000E   1C                   ENDIF
029300000000IF  1C     D60TTV        IFEQ      'T'
029400000000     C                   MOVE      D60CTS        VCCKE1
029500000000E   1C                   ENDIF
029600000000IF  1C     D60TTV        IFEQ      'P'
029700131121     C                   MOVEl     D60cad        VCCKE1
029800131121     C                   MOVEl     D60naz        VCCKE2
029900000000E   1C                   ENDIF
030000000000     C*
030100000000     C* SE SONO ENTRATO QUI PER F6, NON DEVO RIPOSIZIONARMI COME FAREI CON F14
030200000000     C     KEY           IFNE      F6
030300000000     C                   EXSR      NEWPOS
030400000000     C                   EXSR      POSSFL
030500000000     C                   ENDIF
030600000000     C*
030700000000     C                   ENDSR
030800000000     C*------------------------------------------------------------------------*
030900000000     C*  GESOPZ - GESTIONE OPZIONI
031000000000     C*------------------------------------------------------------------------*
031100000000     C     GESOPZ        BEGSR
031200000000     C*
031300000000SL  1C                   SELECT
031400000000     C     VSCOPZ        WHENEQ    ' 2'
031500000000     C                   EXSR      O02GES
031600010112     C     VSCOPZ        WHENEQ    ' 3'
031700010112     C                   EXSR      O03GES
031800000000     C     VSCOPZ        WHENEQ    ' 4'
031900000000     C                   EXSR      O04GES
032000000000     C     VSCOPZ        WHENEQ    ' 5'
032100000000     C                   EXSR      O05GES
032200000000     C     VSCOPZ        WHENEQ    ' 7'
032300000000     C                   EXSR      O07GES
032400000000     C                   OTHER
032500000000     C                   MOVEL     MSG(4)        $MSG
032600000000     C                   EXSR      CARMSG
032700000000E   1C                   ENDSL
032800000000     C*
032900000000     C                   ENDSR
033000000000     C*------------------------------------------------------------------------*
033100000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
033200000000     C*------------------------------------------------------------------------*
033300000000     C     O02GES        BEGSR
033400000000     C*
033500000000IF  1C     VSCATB        IFEQ      'A'
033600000000     C                   MOVEL     '1'           $ERR
033700000000     C                   MOVEL     MSG(12)       $MSG
033800000000     C                   EXSR      CARMSG
033900000000E   1C                   ENDIF
034000000000     C*
034100000000IF  1C     $ERR          IFEQ      '0'
034200000000     C*
034300000000     C* LANCIO LA MODIFICA
034400000000     C                   Z-ADD     VSCVOC        WVOC
034500000000     C                   Z-ADD     VSCPRG        WPRG
034600131121     C                   Z-ADD     VSHDDT        WDDT
034700000000     C                   MOVEL     VSCTTV        WTTV
034800000000     C                   Z-ADD     *ZERO         WCLI
034900000000     C                   Z-ADD     *ZERO         WCTR
035000000000     C                   Z-ADD     *ZERO         WFT1
035100000000     C                   Z-ADD     *ZERO         WFT2
035200131121     C                   MOVE      *BLANKS       Wcad
035300131121     C                   MOVE      *BLANKS       Wnaz
035400000000     C                   MOVE      *BLANKS       WCTS
035500000000IF  1C     VSCTTV        IFEQ      'K'
035600000000     C                   MOVE      VSCCLI        WCLI
035700000000     C                   MOVEL     VSCCTR        WCTR
035800000000E   1C                   ENDIF
035900000000IF  1C     VSCTTV        IFEQ      'F'
036000000000     C                   MOVE      VSCCLI        WFT1
036100000000     C                   MOVEL     VSCCTR        WFT2
036200000000E   1C                   ENDIF
036300000000IF  1C     VSCTTV        IFEQ      'P'
036400131121     C                   MOVEl     VSCcli        Wcad
036500131121     C                   MOVEl     VSCctr        Wnaz
036600000000E   1C                   ENDIF
036700000000IF  1C     VSCTTV        IFEQ      'T'
036800000000     C                   MOVE      VSCCLI        WCTS
036900000000E   1C                   ENDIF
037000000000     C                   Z-ADD     VSCSGI        WSGI
037100000000     C*
037200000000     C                   MOVEL     D60OP0        WOP0
037300000000     C                   MOVEL     'O02'         WOP1
037400000000     C                   MOVEL     'TREC64R'     WPGM              8
037500000000     C                   EXSR      LANPGM
037600000000     C*
037700000000     C                   Z-ADD     1             SWMODI
037800000000     C*
037900000000E   1C                   ENDIF
038000000000     C*
038100000000     C                   ENDSR
038200010112     C*------------------------------------------------------------------------*
038300010112     C*  O03GES - GESTIONE OPZIONE 3=DUPLICA
038400010112     C*------------------------------------------------------------------------*
038500010112     C     O03GES        BEGSR
038600010112     C*
038700010112IF  1C     VSCATB        IFEQ      'A'
038800010112     C                   MOVEL     '1'           $ERR
038900010112     C                   MOVEL     MSG(12)       $MSG
039000010112     C                   EXSR      CARMSG
039100010112E   1C                   ENDIF
039200010112     C*
039300010112IF  1C     $ERR          IFEQ      '0'
039400010112     C*
039500010112     C* LANCIO LA MODIFICA
039600010112     C                   Z-ADD     VSCVOC        WVOC
039700010112     C                   Z-ADD     VSCPRG        WPRG
039800131121     C                   Z-ADD     VSHDDT        WDDT
039900010112     C                   MOVEL     VSCTTV        WTTV
040000010112     C                   Z-ADD     *ZERO         WCLI
040100010112     C                   Z-ADD     *ZERO         WCTR
040200010112     C                   Z-ADD     *ZERO         WFT1
040300010112     C                   Z-ADD     *ZERO         WFT2
040400131121     C                   MOVE      *BLANKS       Wcad
040500131121     C                   MOVE      *BLANKS       Wnaz
040600010112     C                   MOVE      *BLANKS       WCTS
040700010112IF  1C     VSCTTV        IFEQ      'K'
040800010112     C                   MOVE      VSCCLI        WCLI
040900010112     C                   MOVEL     VSCCTR        WCTR
041000010112E   1C                   ENDIF
041100010112IF  1C     VSCTTV        IFEQ      'F'
041200010112     C                   MOVE      VSCCLI        WFT1
041300010112     C                   MOVEL     VSCCTR        WFT2
041400010112E   1C                   ENDIF
041500010112IF  1C     VSCTTV        IFEQ      'P'
041600131121     C                   MOVEl     VSCcli        Wcad
041700131121     C                   MOVEl     VSCctr        Wnaz
041800010112E   1C                   ENDIF
041900010112IF  1C     VSCTTV        IFEQ      'T'
042000010112     C                   MOVE      VSCCLI        WCTS
042100010112E   1C                   ENDIF
042200010112     C                   Z-ADD     VSCSGI        WSGI
042300010112     C*
042400010112     C                   MOVEL     D60OP0        WOP0
042500010112     C                   MOVEL     'O03'         WOP1
042600010112     C                   MOVEL     'TREC64R'     WPGM              8
042700010112     C                   EXSR      LANPGM
042800010112     C*
042900010112     C                   Z-ADD     1             SWMODI
043000010112     C*
043100010112E   1C                   ENDIF
043200010112     C*
043300010112     C                   ENDSR
043400000000     C*------------------------------------------------------------------------*
043500000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
043600000000     C*------------------------------------------------------------------------*
043700000000     C     O04GES        BEGSR
043800000000     C*
043900000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
044000000000IF  1C     VSCATB        IFEQ      'A'
044100000000     C                   MOVEL     '1'           $ERR
044200000000     C                   MOVEL     MSG(12)       $MSG
044300000000     C                   EXSR      CARMSG
044400000000E   1C                   ENDIF
044500000000     C*
044600000000IF  1C     $ERR          IFEQ      '0'
044700000000     C*
044800000000     C* LANCIO LA CANCELLAZIONE
044900000000     C                   Z-ADD     VSCVOC        WVOC
045000000000     C                   Z-ADD     VSCPRG        WPRG
045100131121     C                   Z-ADD     VSHDDT        WDDT
045200000000     C                   MOVEL     VSCTTV        WTTV
045300000000     C                   Z-ADD     *ZERO         WCLI
045400000000     C                   Z-ADD     *ZERO         WCTR
045500000000     C                   Z-ADD     *ZERO         WFT1
045600000000     C                   Z-ADD     *ZERO         WFT2
045700131121     C                   MOVE      *BLANKS       Wcad
045800131121     C                   MOVE      *BLANKS       Wnaz
045900000000     C                   MOVE      *BLANKS       WCTS
046000000000IF  1C     VSCTTV        IFEQ      'K'
046100000000     C                   MOVE      VSCCLI        WCLI
046200000000     C                   MOVEL     VSCCTR        WCTR
046300000000E   1C                   ENDIF
046400000000IF  1C     VSCTTV        IFEQ      'F'
046500000000     C                   MOVE      VSCCLI        WFT1
046600000000     C                   MOVEL     VSCCTR        WFT2
046700000000E   1C                   ENDIF
046800000000IF  1C     VSCTTV        IFEQ      'P'
046900131121     C                   MOVEl     VSCcli        Wcad
047000131121     C                   MOVEl     VSCctr        Wnaz
047100000000E   1C                   ENDIF
047200000000IF  1C     VSCTTV        IFEQ      'T'
047300000000     C                   MOVE      VSCCLI        WCTS
047400000000E   1C                   ENDIF
047500000000     C                   Z-ADD     VSCSGI        WSGI
047600000000     C*
047700000000     C                   MOVEL     D60OP0        WOP0
047800000000     C                   MOVEL     'O04'         WOP1
047900000000     C                   MOVEL     'TREC64R'     WPGM              8
048000000000     C                   EXSR      LANPGM
048100000000     C*
048200000000     C                   Z-ADD     1             SWMODI
048300000000     C*
048400000000E   1C                   ENDIF
048500000000     C*
048600000000     C                   ENDSR
048700000000     C*------------------------------------------------------------------------*
048800000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
048900000000     C*------------------------------------------------------------------------*
049000000000     C     O05GES        BEGSR
049100000000     C*
049200000000     C* LANCIO LA VISUALIZZAZIONE
049300000000     C                   Z-ADD     VSCVOC        WVOC
049400000000     C                   Z-ADD     VSCPRG        WPRG
049500131121     C                   Z-ADD     VSHDDT        WDDT
049600000000     C                   MOVEL     VSCTTV        WTTV
049700000000     C                   Z-ADD     *ZERO         WCLI
049800000000     C                   Z-ADD     *ZERO         WCTR
049900000000     C                   Z-ADD     *ZERO         WFT1
050000000000     C                   Z-ADD     *ZERO         WFT2
050100131121     C                   MOVE      *BLANKS       Wcad
050200131121     C                   MOVE      *BLANKS       Wnaz
050300000000     C                   MOVE      *BLANKS       WCTS
050400000000IF  1C     VSCTTV        IFEQ      'K'
050500000000     C                   MOVE      VSCCLI        WCLI
050600000000     C                   MOVEL     VSCCTR        WCTR
050700000000E   1C                   ENDIF
050800000000IF  1C     VSCTTV        IFEQ      'F'
050900000000     C                   MOVE      VSCCLI        WFT1
051000000000     C                   MOVEL     VSCCTR        WFT2
051100000000E   1C                   ENDIF
051200000000IF  1C     VSCTTV        IFEQ      'P'
051300131121     C                   MOVEl     VSCcli        Wcad
051400131121     C                   MOVEl     VSCctr        Wnaz
051500000000E   1C                   ENDIF
051600000000IF  1C     VSCTTV        IFEQ      'T'
051700000000     C                   MOVE      VSCCLI        WCTS
051800000000E   1C                   ENDIF
051900000000     C                   Z-ADD     VSCSGI        WSGI
052000000000     C*
052100000000     C                   MOVEL     D60OP0        WOP0
052200000000     C                   MOVEL     'O05'         WOP1
052300000000     C                   MOVEL     'TREC64R'     WPGM              8
052400000000     C                   EXSR      LANPGM
052500000000     C*
052600000000     C                   ENDSR
052700000000     C*------------------------------------------------------------------------*
052800000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
052900000000     C*------------------------------------------------------------------------*
053000000000     C     O07GES        BEGSR
053100000000     C*
053200000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
053300000000IF  1C     VSCATB        IFNE      'A'
053400000000     C                   MOVEL     '1'           $ERR
053500000000     C                   MOVEL     MSG(13)       $MSG
053600000000     C                   EXSR      CARMSG
053700000000E   1C                   ENDIF
053800000000     C*
053900000000IF  1C     $ERR          IFEQ      '0'
054000000000     C*
054100000000     C* LANCIO IL RIPRISTINO
054200000000     C                   Z-ADD     VSCVOC        WVOC
054300000000     C                   Z-ADD     VSCPRG        WPRG
054400131121     C                   Z-ADD     VSHDDT        WDDT
054500000000     C                   MOVEL     VSCTTV        WTTV
054600000000     C                   Z-ADD     *ZERO         WCLI
054700000000     C                   Z-ADD     *ZERO         WCTR
054800000000     C                   Z-ADD     *ZERO         WFT1
054900000000     C                   Z-ADD     *ZERO         WFT2
055000131121     C                   MOVE      *BLANKS       Wcad
055100131121     C                   MOVE      *BLANKS       Wnaz
055200000000     C                   MOVE      *BLANKS       WCTS
055300000000IF  1C     VSCTTV        IFEQ      'K'
055400000000     C                   MOVE      VSCCLI        WCLI
055500000000     C                   MOVEL     VSCCTR        WCTR
055600000000E   1C                   ENDIF
055700000000IF  1C     VSCTTV        IFEQ      'F'
055800000000     C                   MOVE      VSCCLI        WFT1
055900000000     C                   MOVEL     VSCCTR        WFT2
056000000000E   1C                   ENDIF
056100000000IF  1C     VSCTTV        IFEQ      'P'
056200131121     C                   MOVEl     VSCcli        Wcad
056300131121     C                   MOVEl     VSCctr        Wnaz
056400000000E   1C                   ENDIF
056500000000IF  1C     VSCTTV        IFEQ      'T'
056600000000     C                   MOVE      VSCCLI        WCTS
056700000000E   1C                   ENDIF
056800000000     C                   Z-ADD     VSCSGI        WSGI
056900000000     C*
057000000000     C                   MOVEL     D60OP0        WOP0
057100000000     C                   MOVEL     'O07'         WOP1
057200000000     C                   MOVEL     'TREC64R'     WPGM              8
057300000000     C                   EXSR      LANPGM
057400000000     C*
057500000000     C                   Z-ADD     1             SWMODI
057600000000     C*
057700000000E   1C                   ENDIF
057800000000     C*
057900000000     C                   ENDSR
058000000000     C*------------------------------------------------------------------------*
058100000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
058200000000     C*------------------------------------------------------------------------*
058300000000     C     NEWPOS        BEGSR
058400000000     C*
058500000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
058600000000     C*        1 = RICHIESTO POSIZIONAMENTO
058700000000     C                   Z-ADD     0             SWPOSI            7 0
058800000000     C*
058900000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
059000000000     C*            NON OCCORRE RICARICARE IL SUBFILE
059100000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
059200000000     C                   Z-ADD     0             SWMODI            1 0
059300000000     C*
059400000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
059500000000IF  1C     VCCVOC        IFNE      DEPVOC
059600000000     C     VCCPRG        ORNE      DEPPRG
059700000000     C     VCCTTV        ORNE      DEPTTV
059800000000     C     VCCKE1        ORNE      DEPKE1
059900000000     C     VCCKE2        ORNE      DEPKE2
060000000000     C     VCCDDT        ORNE      DEPDDT
060100010111     C     VCCVAL        ORNE      DEPVAL
060200000000     C*
060300000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
060400000000     C                   EXSR      CHKPAR
060500000000     C*
060600000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
060700000000IF  2C     $ERR          IFEQ      '0'
060800000000     C                   Z-ADD     1             SWPOSI
060900000000     C                   Z-ADD     VCCVOC        DEPVOC
061000000000     C                   Z-ADD     VCCPRG        DEPPRG
061100000000     C                   MOVE      VCCTTV        DEPTTV
061200131121     C                   MOVEl     VCCKE1        DEPKE1
061300131121     C                   MOVEl     VCCKE2        DEPKE2
061400000000     C                   Z-ADD     VCCDDT        DEPDDT
061500010111     C                   Z-ADD     VCCVAL        DEPVAL
061600000000E   2C                   ENDIF
061700000000E   1C                   ENDIF
061800000000     C*
061900000000     C                   ENDSR
062000000000     C*------------------------------------------------------------------------*
062100000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
062200000000     C*------------------------------------------------------------------------*
062300000000     C     CHKPAR        BEGSR
062400000000     C*
062500000000     C* REIMPOSTO INDICATORI DI ERRORE
062600010111     C                   SETOFF                                         8183
062700000000     C*--------------------
062800000000     C* DATA DECORRENZA VOCE
062900000000     C*--------------------
063000000000     C                   Z-ADD     *ZERO         DDTVCC
063100000000IF  1C     VCCDDT        IFGT      *ZEROS
063200000000     C                   Z-ADD     VCCDDT        G02DAT
063300000000     C                   Z-ADD     *ZEROS        G02INV
063400000000     C                   MOVEL     '0'           G02ERR
063500000000     C                   CALL      'XSRDA8'
063600000000     C                   PARM                    WLBDAT
063700000000     C*
063800000000IF  2C     G02ERR        IFEQ      '1'
063900000000     C                   MOVEL     '1'           $ERR
064000000000     C                   MOVEL     MSG(9)        $MSG
064100000000     C                   EXSR      CARMSG
064200000000     C                   SETON                                        81
064300000000X   2C                   ELSE
064400000000     C                   Z-ADD     G02DAT        VCCDDT
064500000000     C                   Z-ADD     G02INV        DDTVCC            8 0
064600000000E   2C                   ENDIF
064700000000E   1C                   ENDIF
064800010111     C*--------------------
064900010111     C* DATA VALIDITA' AL
065000010111     C*--------------------
065100010111     C                   Z-ADD     *ZERO         VALVCC
065200010111IF  1C     VCCVAL        IFGT      *ZEROS
065300010111     C                   Z-ADD     VCCVAL        G02DAT
065400010111     C                   Z-ADD     *ZEROS        G02INV
065500010111     C                   MOVEL     '0'           G02ERR
065600010111     C                   CALL      'XSRDA8'
065700010111     C                   PARM                    WLBDAT
065800010111     C*
065900010111IF  2C     G02ERR        IFEQ      '1'
066000010111     C                   MOVEL     '1'           $ERR
066100010111 XXX C                   MOVEL     MSG(9)        $MSG
066200010111     C                   EXSR      CARMSG
066300010111     C                   SETON                                        83
066400010111X   2C                   ELSE
066500010111     C                   Z-ADD     G02DAT        VCCVAL
066600010111     C                   Z-ADD     G02INV        VALVCC            8 0
066700010111E   2C                   ENDIF
066800010111E   1C                   ENDIF
066900000000     C*
067000000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
067100000000     C* SUL CAMPO DEL CONTROL
067200000000IF  1C     $ERR          IFEQ      '1'
067300000000     C                   MOVEL     '0'           *IN43
067400000000X   1C                   ELSE
067500000000     C                   MOVEL     '1'           *IN43
067600000000E   1C                   ENDIF
067700000000     C*
067800000000     C                   ENDSR
067900000000     C*------------------------------------------------------------------------*
068000000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
068100000000     C*------------------------------------------------------------------------*
068200000000     C     SETFIL        BEGSR
068300000000     C*
068400000000     C                   MOVEL     '1'           *IN43
068500000000     C*
068600000000     C* POSIZIONAMENTO ARCHIVIO
068700950301     C*---                 Z-ADDVCCVOC    KVTVOC
068800950301     C*---                 Z-ADDVCCPRG    KVTPRG
068900950301     C*---                 MOVE VCCTTV    KVTTTV
069000950301     C                   Z-ADD     DEPVOC        KVTVOC
069100950301     C                   Z-ADD     DEPPRG        KVTPRG
069200950301     C                   MOVE      DEPTTV        KVTTTV
069300000000     C                   Z-ADD     *ZERO         KVTCLI
069400000000     C                   Z-ADD     *ZERO         KVTCTR
069500000000     C                   Z-ADD     *ZERO         KVTFT1
069600000000     C                   Z-ADD     *ZERO         KVTFT2
069700000000     C                   MOVE      *BLANKS       KVTCTS
069800131121     C                   MOVE      *BLANKS       KVTcad
069900131121     C                   MOVE      *BLANKS       KVTnaz
070000000000IF  1C     VCCTTV        IFEQ      'K'
070100000000     C                   MOVE      VCCKE1        KVTCLI
070200000000     C                   MOVE      VCCKE2        KVTCTR
070300000000E   1C                   ENDIF
070400000000IF  1C     VCCTTV        IFEQ      'F'
070500000000     C                   MOVE      VCCKE1        KVTFT1
070600000000     C                   MOVE      VCCKE2        KVTFT2
070700000000E   1C                   ENDIF
070800000000IF  1C     VCCTTV        IFEQ      'T'
070900000000     C                   MOVE      VCCKE1        KVTCTS
071000000000E   1C                   ENDIF
071100000000IF  1C     VCCTTV        IFEQ      'P'
071200131121     C                   MOVEl     VCCKE1        KVTcad
071300131121     C                   MOVEl     VCCKE2        KVTnaz
071400000000E   1C                   ENDIF
071500000000     C                   Z-ADD     VCCDDT        KVTDDT
071600131121     C     KEYEVD        SETLL     ECEVD06L                           99
071700000000     C*
071800000000     C  N99              EXSR      LETFIL
071900000000     C*
072000000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
072100000000IF  1C     *IN99         IFEQ      '1'
072200000000     C                   Z-ADD     1             SFLEOF
072300000000E   1C                   ENDIF
072400000000     C*
072500000000     C                   ENDSR
072600000000     C*------------------------------------------------------------------------*
072700000000     C* LETFIL - LEGGE RECORD SUCESSIVO
072800000000     C*------------------------------------------------------------------------*
072900000000     C     LETFIL        BEGSR
073000000000     C*
073100000000     C* LEGGO FINO A:
073200000000     C*     - FINE FILE
073300000000     C*     - TROVATO RECORD VALIDO
073400000000     C                   MOVEL     '0'           WRECOK            1
073500000000DO  1C     *IN99         DOWEQ     *OFF
073600000000     C     WRECOK        ANDEQ     '0'
073700000000     C*
073800000000     C* LETTURA
073900131121     C                   READ      ECEVD06L                               99
074000000000     C*
074100000000     C* FINE FILE  O TABELLA DIVERSA
074200000000IF  2C     *IN99         IFEQ      '1'
074300000000     C                   Z-ADD     1             SFLEOF
074400000000X   2C                   ELSE
074500000000     C                   EXSR      CHKREC
074600000000E   2C                   ENDIF
074700000000     C*
074800000000E   1C                   ENDDO
074900000000     C*
075000000000     C                   ENDSR
075100000000     C*------------------------------------------------------------------------*
075200000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
075300000000     C*------------------------------------------------------------------------*
075400000000     C     CHKREC        BEGSR
075500000000     C*
075600000000     C                   MOVEL     '1'           WRECOK
075700000000     C*
075800000000     C* SELEZIONE PER  ====>  DATA DECORRENZA
075900000000IF  1C     DDTVCC        IFGT      *ZEROS
076000000000IF  2C     DDTVCC        IFNE      EVDDDT
076100000000     C                   MOVEL     '0'           WRECOK
076200000000E   2C                   ENDIF
076300000000E   1C                   ENDIF
076400010111     C*
076500010111     C* SELEZIONE PER  ====>  DATA VALIDITA' AL
076600010111     C     VALVCC        IFGT      *ZEROS
076700010111     C     EVDDDT        IFLE      VALVCC
076800010111     C     EVDDST        ANDGE     VALVCC
076900010111     C                   ELSE
077000010111     C                   MOVEL     '0'           WRECOK
077100010111     C                   ENDIF
077200010111     C                   ENDIF
077300000000     C*
077400000000     C* SELEZIONE PER  ====>  TIPO TARIFFA (K/F/C/T/P)
077500000000IF  1C     VCCTTV        IFNE      *BLANKS
077600000000     C     VCCTTV        ANDNE     EVDTTV
077700000000     C                   MOVEL     '0'           WRECOK
077800000000E   1C                   ENDIF
077900000000     C*
078000000000     C* INIZIO ELENCO DA  ====>  PROGRESSIVO
078100000000IF  1C     VCCPRG        IFNE      *ZERO
078200000000     C     VCCPRG        ANDGT     EVDPRG
078300000000     C                   MOVEL     '0'           WRECOK
078400000000E   1C                   ENDIF
078500000000     C* ---------------------------------------------------
078600000000IF  1C     VCCTTV        IFEQ      'K'
078700000000     C*
078800000000     C* CODICE CLIENTE
078900000000IF  2C     VCCKE1        IFNE      *BLANKS
079000000000     C     VCCKE1        ANDNE     *ZERO
079100000000     C     *LIKE         DEFINE    EVDCLI        VCCCLI
079200000000     C                   MOVE      VCCKE1        VCCCLI
079300000000IF  3C     VCCCLI        IFGT      EVDCLI
079400000000     C                   MOVEL     '0'           WRECOK
079500000000E   3C                   ENDIF
079600000000E   2C                   ENDIF
079700000000     C*
079800000000     C* TARIFFA CLIENTE
079900000000IF  2C     VCCKE2        IFNE      *BLANKS
080000000000     C     *LIKE         DEFINE    EVDCTR        VCCCTR
080100000000     C                   MOVE      VCCKE2        VCCCTR
080200000000IF  3C     VCCCTR        IFGT      EVDCTR
080300000000     C                   MOVEL     '0'           WRECOK
080400000000E   3C                   ENDIF
080500000000E   2C                   ENDIF
080600000000     C*
080700000000E   1C                   ENDIF
080800000000     C* ---------------------------------------------------
080900000000IF  1C     VCCTTV        IFEQ      'F'
081000000000     C*
081100000000     C* FILIALE 1
081200000000IF  2C     VCCK11        IFNE      *BLANKS
081300000000     C     VCCK11        ANDNE     *ZERO
081400000000     C     *LIKE         DEFINE    EVDFT1        VCCFT1
081500000000     C                   MOVE      VCCK11        VCCFT1
081600000000IF  3C     VCCFT1        IFGT      EVDFT1
081700000000     C                   MOVEL     '0'           WRECOK
081800000000E   3C                   ENDIF
081900000000E   2C                   ENDIF
082000000000     C*
082100000000     C* FILIALE 2
082200000000IF  2C     VCCKE2        IFNE      *BLANKS
082300000000     C     VCCKE2        ANDNE     *ZERO
082400000000     C     *LIKE         DEFINE    EVDFT2        VCCFT2
082500000000     C                   MOVE      VCCKE2        VCCFT2
082600000000IF  3C     VCCFT2        IFGT      EVDFT2
082700000000     C                   MOVEL     '0'           WRECOK
082800000000E   3C                   ENDIF
082900000000E   2C                   ENDIF
083000000000     C*
083100000000E   1C                   ENDIF
083200000000     C* ---------------------------------------------------
083300000000IF  1C     VCCTTV        IFEQ      'P'
083400000000     C*
083500000000     C* C.A.P.
083600131121IF  2C     VCCKe1        IFNE      *BLANKS
083700131121     C     VCCKe1        ANDNE     *ZERO
083800131121     C     *LIKE         DEFINE    EVDCAd        VCCcad
083900131121     C                   MOVEl     VCCKe1        VCCcad
084000131121IF  3C     VCCCAd        IFGT      EVDCAd
084100000000     C                   MOVEL     '0'           WRECOK
084200000000E   3C                   ENDIF
084300000000E   2C                   ENDIF
084400131121     C*
084500131121     C* Nazione
084600131121     C     *LIKE         DEFINE    EVDnaz        VCCnaz
084700131121     C                   MOVEl     VCCKe2        VCCnaz
084800131121IF  3C     VCCnaz        IFGT      EVDnaz
084900131121     C                   MOVEL     '0'           WRECOK
085000131121E   3C                   ENDIF
085100000000     C*
085200000000E   1C                   ENDIF
085300000000     C* ---------------------------------------------------
085400000000IF  1C     VCCTTV        IFEQ      'T'
085500000000     C*
085600000000     C* CODICE TASSAZIONE
085700000000IF  2C     VCCKE1        IFNE      *BLANKS
085800000000     C     *LIKE         DEFINE    EVDCTS        VCCCTS
085900000000     C                   MOVE      VCCKE1        VCCCTS
086000000000IF  3C     VCCCTS        IFGT      EVDCTS
086100000000     C                   MOVEL     '0'           WRECOK
086200000000E   3C                   ENDIF
086300000000E   2C                   ENDIF
086400091021     C*
086500091021E   1C                   ENDIF
086600091021     C* SOLO TARIFFE DELLA VOCE SCELTA
086700091021     C                   if        VCCVOC <> 0
086800091021     C                   if        evdvoc <> vccvoc
086900091021     C                   MOVEL     '0'           WRECOK
087000091021     c                   else
087100091021     c                   if        evdprg <> vccprg AND VCCPRG <> 0
087200091021     C                   MOVEL     '0'           WRECOK
087300091021     c                   end
087400091021     c                   end
087500091021     c                   end
087600000000     C*
087700000000     C                   ENDSR
087800000000     C*------------------------------------------------------------------------*
087900000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
088000000000     C*------------------------------------------------------------------------*
088100000000     C     IRESFL        BEGSR
088200000000     C*
088300000000IF  1C     EVDATB        IFNE      *BLANKS
088400000000     C                   MOVEL     'A'           VSCATB
088500000000X   1C                   ELSE
088600000000     C                   MOVEL     ' '           VSCATB
088700000000E   1C                   ENDIF
088800000000     C*
088900000000     C                   Z-ADD     EVDVOC        VSCVOC
089000000000     C                   Z-ADD     EVDPRG        VSCPRG
089100000000     C                   MOVE      EVDTTV        VSCTTV
089200000000     C                   MOVE      *BLANKS       VSCCLI
089300000000     C                   MOVE      *BLANKS       VSCCTR
089400000000IF  1C     VSCTTV        IFEQ      'K'
089500000000     C                   MOVE      EVDCLI        VSCCLI
089600000000     C                   MOVEL     EVDCTR        VSCCTR
089700000000E   1C                   ENDIF
089800000000IF  1C     VSCTTV        IFEQ      'F'
089900000000     C                   MOVE      EVDFT1        VSCCLI
090000000000     C                   MOVEL     EVDFT2        VSCCTR
090100000000E   1C                   ENDIF
090200000000IF  1C     VSCTTV        IFEQ      'P'
090300131121     C                   MOVEl     EVDCAd        VSCCLI
090400131121     C                   MOVEl     EVDnaz        VSCctr
090500000000E   1C                   ENDIF
090600000000IF  1C     VSCTTV        IFEQ      'T'
090700000000     C                   MOVE      EVDCTS        VSCCLI
090800000000E   1C                   ENDIF
090900000000     C                   Z-ADD     EVDDDT        G02INV
091000000000     C                   Z-ADD     *ZEROS        G02DAT
091100000000     C                   MOVEL     '3'           G02ERR
091200000000     C                   CALL      'XSRDA8'
091300000000     C                   PARM                    WLBDAT
091400131121     C                   Z-ADD     G02DAT        VSHDdT
091500131121     C                   movel     G02DAT        VSCDdT
091600131121     C                   move      G02DAT        anno2             2 0
091700131121     C                   move      anno2         VSCDdT
091800000000     C                   Z-ADD     EVDDST        G02INV
091900000000     C                   Z-ADD     *ZEROS        G02DAT
092000000000     C                   MOVEL     '3'           G02ERR
092100000000     C                   CALL      'XSRDA8'
092200000000     C                   PARM                    WLBDAT
092300131121     C                   Z-ADD     G02DAT        VSHDsT
092400131121     C                   movel     G02DAT        VSCDsT
092500131121     C                   move      G02DAT        anno2             2 0
092600131121     C                   move      anno2         VSCDsT
092700940908     C*
092800940908     C                   Z-ADD     *ZEROS        VSCIOP
092900940908     C                   MOVEL     *BLANKS       VSCSEG
093000940908     C     EVDITR        IFGT      *ZEROS
093100940908     C                   Z-ADD     EVDITR        VSCIOP
093200940908     C                   ENDIF
093300940908     C     EVDPER        IFGT      *ZEROS
093400940908     C                   MOVEL     EVDSEG        VSCSEG
093500940908     C                   Z-ADD     EVDPER        VSCIOP
093600940908     C                   ENDIF
093700000000     C                   Z-ADD     EVDSGI        VSCSGI
093800000000     C*
093900000000     C                   ENDSR
094000000000     C*------------------------------------------------------------------------*
094100000000     C* LANPGM - LANCIA UN PROGRAMMA
094200000000     C*------------------------------------------------------------------------*
094300000000     C     LANPGM        BEGSR
094400000000     C*--------------------
094500000000     C* PARAMETRI INFORMATIVI
094600000000     C*--------------------
094700000000     C                   MOVEL     WOP0          D60OP0
094800000000     C                   MOVEL     WOP1          D60OP1
094900000000     C                   MOVEL     '0'           D60F03
095000000000     C                   MOVEL     '0'           D60ERR
095100000000     C                   MOVEL     *BLANKS       D60MSG
095200000000     C*--------------------
095300000000     C* PARAMETRI
095400000000     C*--------------------
095500000000     C                   Z-ADD     WVOC          D60VOC
095600000000     C                   Z-ADD     WPRG          D60PRG
095700000000     C                   Z-ADD     WDDV          D60DDV
095800000000     C                   MOVEL     WTTV          D60TTV
095900000000     C                   Z-ADD     WCLI          D60CLI
096000000000     C                   MOVEL     WCTR          D60CTR
096100000000     C                   Z-ADD     WFT1          D60FT1
096200000000     C                   Z-ADD     WFT2          D60FT2
096300000000     C                   MOVE      WCTS          D60CTS
096400131121     C                   movel     Wcad          D60cad
096500131121     C                   movel     Wnaz          D60naz
096600000000     C                   Z-ADD     WDDT          D60DDT
096700000000     C                   Z-ADD     WSGI          D60SGI
096800000000     C* LANCIO
096900000000     C                   MOVEL     DS60          KPJBU
097000000000     C                   CALL      WPGM                                 99
097100000000     C                   PARM                    KPJBA
097200000000     C                   MOVEL     KPJBU         DS60
097300000000     C*
097400000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
097500000000IF  1C     D60F03        IFEQ      '1'
097600000000     C                   MOVEL     '1'           $F03
097700000000     C                   MOVEL     '1'           $ERR
097800000000     C                   MOVEL     '1'           $FINE
097900000000     C*
098000000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
098100000000X   1C                   ELSE
098200000000     C                   MOVEL     D60MSG        $MSG
098300000000     C                   EXSR      CARMSG
098400000000E   1C                   ENDIF
098500000000     C*
098600000000     C                   ENDSR
098700020923     C*-------------------------------------------------------------------------
098800020923     C* REPERISCE I DATI UTENTE
098900020923     C*-------------------------------------------------------------------------
099000020923     C     REPDATIUTE    BEGSR
099100020923     C*
099200020923     C* INIZIALIZZA VARIABILI DI WRK
099300020923     C                   CLEAR                   TIBS34DS
099400020923     C                   CLEAR                   AZUTEDS
099500020923     C                   CLEAR                   DDATIUTE
099600020923     C*
099700020923     C     *DTAARA       DEFINE    §azute        azuteds
099800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
099900020923     C                   IN(E)     *DTAARA
100000020923if  1C                   IF        %Error
100100020923     c                   EVAL      I34Tla = 'L'
100200020923     C                   CALL      'TIBS34R'
100300020923     C                   PARM                    Tibs34Ds
100400020923     C                   IN        *DTAARA
100500020923e   1C                   ENDIF
100600020923     C*
100700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
100800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
100900020923     C*
101000020923     C                   ENDSR
101100000000     C*------------------------------------------------------------------------*
101200000000     C*  *INZSR - ROUTINE INIZIALE
101300000000     C*------------------------------------------------------------------------*
101400000000     C     *INZSR        BEGSR
101500000000     C*
101600000000     C* RICEVIMENTO PARAMETRI
101700000000     C     *ENTRY        PLIST
101800000000     C                   PARM                    KPJBA
101900000000     C                   MOVEL     KPJBU         DS60
102000000000     C*
102100000000     C* DEFINIZIONE KLIST
102200000000     C     *LIKE         DEFINE    EVDVOC        KVTVOC
102300000000     C     *LIKE         DEFINE    EVDPRG        KVTPRG
102400000000     C     *LIKE         DEFINE    EVDTTV        KVTTTV
102500000000     C     *LIKE         DEFINE    EVDFT1        KVTFT1
102600000000     C     *LIKE         DEFINE    EVDFT2        KVTFT2
102700000000     C     *LIKE         DEFINE    EVDCLI        KVTCLI
102800000000     C     *LIKE         DEFINE    EVDCTR        KVTCTR
102900000000     C     *LIKE         DEFINE    EVDCTS        KVTCTS
103000131121     C     *LIKE         DEFINE    EVDcad        KVTcad
103100131121     C     *LIKE         DEFINE    EVDnaz        KVTnaz
103200000000     C     *LIKE         DEFINE    EVDDDT        KVTDDT
103300000000     C*
103400000000     C     *LIKE         DEFINE    D60OP0        WOP0
103500000000     C     *LIKE         DEFINE    D60OP1        WOP1
103600000000     C     *LIKE         DEFINE    D60VOC        WVOC
103700000000     C     *LIKE         DEFINE    D60PRG        WPRG
103800000000     C     *LIKE         DEFINE    D60DDV        WDDV
103900000000     C     *LIKE         DEFINE    D60TTV        WTTV
104000000000     C     *LIKE         DEFINE    D60CLI        WCLI
104100000000     C     *LIKE         DEFINE    D60CTR        WCTR
104200000000     C     *LIKE         DEFINE    D60FT1        WFT1
104300000000     C     *LIKE         DEFINE    D60FT2        WFT2
104400000000     C     *LIKE         DEFINE    D60CTS        WCTS
104500131121     C     *LIKE         DEFINE    D60cad        Wcad
104600131121     C     *LIKE         DEFINE    D60naz        Wnaz
104700000000     C     *LIKE         DEFINE    D60DDT        WDDT
104800000000     C*
104900000000     C* DEFINIZIONE DEPOSITI
105000000000     C     *LIKE         DEFINE    VCCVOC        DEPVOC
105100000000     C     *LIKE         DEFINE    VCCPRG        DEPPRG
105200000000     C     *LIKE         DEFINE    VCCTTV        DEPTTV
105300000000     C     *LIKE         DEFINE    VCCKE1        DEPKE1
105400000000     C     *LIKE         DEFINE    VCCKE2        DEPKE2
105500000000     C     *LIKE         DEFINE    VCCDDT        DEPDDT
105600010111     C     *LIKE         DEFINE    VCCVAL        DEPVAL
105700000000     C*
105800131121     C* CHIAVE LETTURA ECEVD06L - PARZIALE
105900000000     C     KEYEVD        KLIST
106000000000     C                   KFLD                    KVTVOC
106100000000     C                   KFLD                    KVTPRG
106200000000     C                   KFLD                    KVTTTV
106300000000     C                   KFLD                    KVTFT1
106400000000     C                   KFLD                    KVTFT2
106500000000     C                   KFLD                    KVTCLI
106600000000     C                   KFLD                    KVTCTR
106700000000     C                   KFLD                    KVTCTS
106800131121     C                   KFLD                    KVTNAZ
106900131121     C                   KFLD                    KVTCAD
107000000000     C                   KFLD                    KVTDDT
107100000000     C*
107200000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
107300000000     C                   MOVE      D60OP0        INTGES            2
107400000000IF  1C     INTGES        IFEQ      '02'
107500000000     C                   MOVEL     'G'           $TGES             1
107600000000     C                   MOVEL     DESGES        VTDOPE
107700000000X   1C                   ELSE
107800000000     C                   MOVEL     'I'           $TGES             1
107900000000     C                   MOVEL     DESINT        VTDOPE
108000000000E   1C                   ENDIF
108100000000     C*
108200000000     C* ACCENDO UN UNDICATORE A SECONDA DEL TIPO TARIFFA DA UTILIZZARE
108300000000     C     D60TTV        COMP      'K'                                    01
108400000000     C     D60TTV        COMP      'F'                                    02
108500000000     C     D60TTV        COMP      'C'                                    03
108600000000     C     D60TTV        COMP      'P'                                    04
108700000000     C     D60TTV        COMP      'T'                                    05
108800000000     C*
108900000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
109000000000     C                   EXSR      INZSTD
109100000000     C*
109200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
109300000000     C                   EXSR      INZSFL
109400000000     C*
109500000000     C* IMPOSTO CAMPI TESTATA VIDEO
109600000000     C                   MOVEL     DSFIRS        VTDRSU
109700000000     C*
109800000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
109900000000     C* SE RICHIAMATO DA ALTRO PGM IMPOSTO I CAMPI PASSATI
110000000000     C                   MOVE      *BLANKS       VCCKE1
110100000000     C                   MOVE      *BLANKS       VCCKE2
110200000000     C                   MOVEL     D60OP0        $UNO              1
110300000000     C     $UNO          IFEQ      'T'
110400000000     C                   Z-ADD     *ZEROS        VCCVOC
110500000000     C                   Z-ADD     *ZEROS        VCCPRG
110600000000     C                   MOVE      *BLANKS       VCCTTV
110700000000     C                   Z-ADD     *ZEROS        VCCDDT
110800010111     C                   Z-ADD     *ZEROS        VCCVAL
110900000000     C                   ELSE
111000000000     C                   Z-ADD     D60VOC        VCCVOC
111100000000     C                   Z-ADD     D60PRG        VCCPRG
111200000000     C                   MOVE      D60TTV        VCCTTV
111300000000IF  1C     VCCTTV        IFEQ      'K'
111400000000     C                   MOVE      D60CLI        VCCKE1
111500000000     C                   MOVE      D60CTR        VCCKE2
111600000000E   1C                   ENDIF
111700000000IF  1C     VCCTTV        IFEQ      'F'
111800000000     C                   MOVE      D60FT1        VCCKE1
111900000000     C                   MOVE      D60FT2        VCCKE2
112000000000E   1C                   ENDIF
112100000000IF  1C     VCCTTV        IFEQ      'T'
112200000000     C                   MOVE      D60CTS        VCCKE1
112300000000E   1C                   ENDIF
112400000000IF  1C     VCCTTV        IFEQ      'P'
112500131121     C                   MOVEl     D60CAd        VCCKE1
112600131121     C                   MOVEl     D60naz        VCCKE2
112700000000E   1C                   ENDIF
112800000000     C                   Z-ADD     D60DDT        VCCDDT
112900000000     C                   Z-ADD     D60DDT        G02DAT
113000000000     C                   Z-ADD     *ZEROS        G02INV
113100000000     C                   MOVEL     '0'           G02ERR
113200000000     C                   CALL      'XSRDA8'
113300000000     C                   PARM                    WLBDAT
113400000000     C                   Z-ADD     G02INV        DDTVCC
113500000000     C                   Z-ADD     D60VOC        DEPVOC
113600000000     C                   Z-ADD     D60PRG        DEPPRG
113700000000     C                   MOVE      D60TTV        DEPTTV
113800000000     C                   MOVE      VCCKE1        DEPKE1
113900000000     C                   MOVE      VCCKE2        DEPKE2
114000000000     C                   Z-ADD     D60DDT        DEPDDT
114100000000     C                   ENDIF
114200000000     C*
114300000000     C* EMETTO VIDEATA SUBFILE
114400000000     C                   EXSR      CARVS
114500000000     C*
114600000000     C                   ENDSR
114700000000     C*------------------------------------------------------------------------*
114800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
115000000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
115100000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
115200000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
115300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
115400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
115500000000     C*------------------------------------------------------------------------*
115600000000     C* INZSTD - ROUTINE INIZIALE STANDARD
115700000000     C*------------------------------------------------------------------------*
115800000000     C     INZSTD        BEGSR
115900000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
116000000000     C                   CALL      'XKEYS'
116100000000     C                   PARM                    DSKEYS
116200000000     C                   MOVEA     KEYRIG        KEYS
116300000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
116400000000     C     XMSGPA        PLIST
116500000000     C                   PARM                    DSMSMS
116600000000     C                   PARM                    DSMSPG
116700000000     C                   PARM                    DSMSKE
116800000000     C                   PARM                    DSMSER
116900000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
117000000000     C                   MOVEL     'TREC63R'     DSMSPG
117100000000     C                   MOVEL     'TREC63R'     VMCQUE
117200000000     C                   MOVEL     *BLANKS       VMCKEY
117300000000     C                   MOVEL     *BLANKS       DSMSMS
117400000000     C* INIZIALIZZO FLAG CONTROLLO PGM
117500000000     C                   MOVEL     '0'           $ERR              1
117600000000     C                   MOVEL     '0'           $F03              1
117700000000     C                   MOVEL     *BLANKS       $GES              2
117800000000     C                   MOVEL     *BLANKS       $MSG             80
117900000000     C                   MOVEL     '0'           $FINE             1
118000000000     C                   MOVEL     '0'           $TESVD            1
118100000000     C* LANCIO ROUTINE PER VERIFICARE SE
118200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
118300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
118400020923     C                   EXSR      REPDATIUTE
118500000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
118600000000     C                   BITOFF    '01234567'    POSMI1            1
118700000000     C                   BITOFF    '01234567'    POSMI2            1
118800000000     C                   BITOFF    '01234567'    POSMA1            1
118900000000     C                   BITOFF    '01234567'    POSMA2            1
119000000000     C                   BITON     '34'          POSMI1            1
119100000000     C                   BITON     '7'           POSMI2            1
119200000000     C                   BITON     '34'          POSMA1            1
119300000000     C                   BITON     '0'           POSMA2            1
119400000000     C                   MOVE      POSMI2        POSMIN            2
119500000000     C                   MOVEL     POSMI1        POSMIN
119600000000     C                   MOVE      POSMA2        POSMAX            2
119700000000     C                   MOVEL     POSMA1        POSMAX
119800000000     C                   ENDSR
119900000000     C*------------------------------------------------------------------------*
120000000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
120100000000     C*------------------------------------------------------------------------*
120200000000     C     FUNSTD        BEGSR
120300000000     C                   SELECT
120400000000     C     KEY           WHENEQ    F3
120500000000     C                   EXSR      F03STD
120600000000     C     KEY           WHENEQ    F12
120700000000     C                   EXSR      F12STD
120800000000     C     KEY           WHENEQ    F24
120900000000     C                   EXSR      F24STD
121000000000     C                   OTHER
121100000000     C                   MOVEL     MSG(2)        $MSG
121200000000     C                   EXSR      CARMSG
121300000000     C                   ENDSL
121400000000     C                   ENDSR
121500000000     C*------------------------------------------------------------------------*
121600000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
121700000000     C*------------------------------------------------------------------------*
121800000000     C     F03STD        BEGSR
121900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
122000000000     C                   MOVEL     '1'           $F03
122100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
122200000000     C                   MOVEL     '1'           $FINE
122300000000     C                   ENDSR
122400000000     C*------------------------------------------------------------------------*
122500000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
122600000000     C*------------------------------------------------------------------------*
122700000000     C     F12STD        BEGSR
122800000000     C* IMPOSTO FLAG DI FIN PGM
122900000000     C                   MOVEL     '1'           $FINE
123000000000     C                   ENDSR
123100000000     C*------------------------------------------------------------------------*
123200000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
123300000000     C*------------------------------------------------------------------------*
123400000000     C     F24STD        BEGSR
123500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
123600000000     C                   EXSR      OUTFUN
123700000000     C                   ENDSR
123800000000     C*------------------------------------------------------------------------*
123900000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
124000000000     C*------------------------------------------------------------------------*
124100000000     C     INZSFL        BEGSR
124200000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
124300000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
124400000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
124500000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
124600000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
124700000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
124800000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
124900000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
125000000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
125100000000     C                   Z-ADD     12            SFLPAG
125200000000     C                   ENDSR
125300000000     C*------------------------------------------------------------------------*
125400000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
125500000000     C*------------------------------------------------------------------------*
125600000000     C     FUNSFL        BEGSR
125700000000     C                   SELECT
125800000000     C     KEY           WHENEQ    ENTER
125900000000     C                   EXSR      ENTSFL
126000000000     C     KEY           WHENEQ    F3
126100000000     C                   EXSR      F03STD
126200000000     C     KEY           WHENEQ    F5
126300000000     C                   EXSR      F05STD
126400000000     C     KEY           WHENEQ    F12
126500000000     C                   EXSR      F12STD
126600000000     C     KEY           WHENEQ    F17
126700000000     C                   EXSR      F17STD
126800000000     C     KEY           WHENEQ    F18
126900000000     C                   EXSR      F18STD
127000000000     C     KEY           WHENEQ    F23
127100000000     C                   EXSR      F23STD
127200000000     C     KEY           WHENEQ    F24
127300000000     C                   EXSR      F24STD
127400000000     C     KEY           WHENEQ    ROLLUP
127500000000     C                   EXSR      RUPSTD
127600000000     C     KEY           WHENEQ    ROLLDN
127700000000     C                   EXSR      RDNSTD
127800000000     C                   OTHER
127900000000     C                   MOVEL     MSG(2)        $MSG
128000000000     C                   EXSR      CARMSG
128100000000     C                   ENDSL
128200000000     C                   ENDSR
128300000000     C*------------------------------------------------------------------------*
128400000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
128500000000     C*------------------------------------------------------------------------*
128600000000     C     F05STD        BEGSR
128700000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
128800000000     C                   EXSR      POSSFL
128900000000     C                   ENDSR
129000000000     C*------------------------------------------------------------------------*
129100000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
129200000000     C*------------------------------------------------------------------------*
129300000000     C     F17STD        BEGSR
129400000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
129500000000     C                   Z-ADD     1             VCCNBR
129600000000     C                   ENDSR
129700000000     C*------------------------------------------------------------------------*
129800000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
129900000000     C*------------------------------------------------------------------------*
130000000000     C     F18STD        BEGSR
130100000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
130200000000     C                   Z-ADD     SFLULT        VCCNBR
130300000000     C                   ENDSR
130400000000     C*------------------------------------------------------------------------*
130500000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
130600000000     C*------------------------------------------------------------------------*
130700000000     C     F23STD        BEGSR
130800000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
130900000000     C                   EXSR      OUTOPZ
131000000000     C                   ENDSR
131100000000     C*------------------------------------------------------------------------*
131200000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
131300000000     C*------------------------------------------------------------------------*
131400000000     C     RUPSTD        BEGSR
131500000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
131600000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
131700000000     C     CURSOR        IFGE      POSMIN
131800000000     C     CURSOR        ANDLE     POSMAX
131900000000     C                   ELSE
132000000000     C                   EXSR      CARSFL
132100000000     C                   ENDIF
132200000000     C                   ENDSR
132300000000     C*------------------------------------------------------------------------*
132400000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
132500000000     C*------------------------------------------------------------------------*
132600000000     C     RDNSTD        BEGSR
132700000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
132800000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
132900000000     C     CURSOR        IFGE      POSMIN
133000000000     C     CURSOR        ANDLE     POSMAX
133100000000     C                   ELSE
133200000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
133300000000     C                   MOVEL     '1'           $ERR
133400000000     C                   MOVEL     MSG(5)        $MSG
133500000000     C                   EXSR      CARMSG
133600000000     C                   ENDIF
133700000000     C                   ENDSR
133800000000     C*------------------------------------------------------------------------*
133900000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
134000000000     C*------------------------------------------------------------------------*
134100000000     C     ENTSFL        BEGSR
134200000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
134300000000     C                   EXSR      NEWPOS
134400000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
134500000000     C     $ERR          IFEQ      '0'
134600000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
134700000000     C     SWPOSI        IFNE      0
134800000000     C                   EXSR      POSSFL
134900000000     C                   ELSE
135000000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
135100000000     C     SFLULT        IFNE      0
135200000000     C                   EXSR      OPZSFL
135300000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
135400000000     C     SWMODI        IFNE      0
135500000000     C                   EXSR      POSSFL
135600000000     C                   ENDIF
135700000000     C                   ENDIF
135800000000     C                   ENDIF
135900000000     C                   ENDIF
136000000000     C                   ENDSR
136100000000     C*------------------------------------------------------------------------*
136200000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
136300000000     C*------------------------------------------------------------------------*
136400000000     C     POSSFL        BEGSR
136500000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
136600000000     C                   EXSR      CLRSFL
136700000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
136800000000     C                   EXSR      SETFIL
136900000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
137000000000     C     SFLEOF        IFEQ      0
137100000000     C                   EXSR      CARSFL
137200000000     C                   ENDIF
137300000000     C                   ENDSR
137400000000     C*------------------------------------------------------------------------*
137500000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
137600000000     C*------------------------------------------------------------------------*
137700000000     C     OPZSFL        BEGSR
137800000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
137900000000     C                   MOVEL     'S'           OPZOK             1
138000000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
138100000000     C                   Z-ADD     0             SFLVAR            5 0
138200000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
138300000000     C                   Z-ADD     0             SFLERR
138400000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
138500000000     C                   Z-ADD     SFLULT        WFLULT
138600000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
138700000000     C                   READC     EC63DVS                                49
138800000000     C     *IN49         DOWEQ     '0'
138900941121     C     $ERR          ANDEQ     '0'
139000000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
139100000000     C                   ADD       1             SFLVAR
139200000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
139300000000     C     OPZOK         IFEQ      'S'
139400000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
139500000000     C     VSCOPZ        IFNE      *BLANKS
139600000000     C                   EXSR      CONOPZ
139700000000     C     OPZOK         IFEQ      'S'
139800941121     C     $ERR          ANDEQ     '0'
139900000000     C                   EXSR      GESOPZ
140000000000     C                   ENDIF
140100000000     C                   ENDIF
140200000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
140300000000     C*            2. PULIZIA SCELTA IN SFL
140400000000     C     OPZOK         IFEQ      'S'
140500000000     C                   MOVEL     '0'           *IN41
140600000000     C                   MOVEL     *BLANKS       VSCOPZ
140700000000     C                   ELSE
140800000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
140900000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
141000000000     C                   MOVEL     '1'           *IN41
141100000000     C                   Z-ADD     SFLULT        SFLERR
141200000000     C                   ENDIF
141300000000     C                   ELSE
141400000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
141500000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
141600000000     C                   MOVEL     '1'           *IN41
141700000000     C                   ENDIF
141800000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
141900000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
142000000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
142100000000     C     OPZOK         IFNE      'S'
142200000000     C                   Z-ADD     SFLERR        VCCNBR
142300000000     C                   ELSE
142400000000     C                   Z-ADD     SFLULT        VCCNBR
142500000000     C                   ENDIF
142600000000     C* AGGIORNO RECORD SFL
142700000000     C                   UPDATE    EC63DVS
142800941121     C     $ERR          IFEQ      '0'
142900000000     C                   READC     EC63DVS                                49
143000941121     C                   ENDIF
143100000000     C                   ENDDO
143200000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
143300000000     C                   Z-ADD     WFLULT        SFLULT
143400000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
143500000000     C     SFLVAR        IFEQ      0
143600000000     C                   MOVEL     '1'           $FINE
143700000000     C                   ENDIF
143800000000     C                   ENDSR
143900000000     C*------------------------------------------------------------------------*
144000000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
144100000000     C*------------------------------------------------------------------------*
144200000000     C     CLRSFL        BEGSR
144300000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
144400000000     C* (0=NON FINE FILE  1=FINE FILE)
144500000000     C                   Z-ADD     0             SFLEOF
144600000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
144700000000     C                   Z-ADD     0             SFLULT
144800000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
144900000000     C                   MOVEL     '1'           *IN40
145000000000     C                   WRITE     EC63DVC
145100000000     C                   MOVEL     '0'           *IN40
145200000000     C                   ENDSR
145300000000     C*------------------------------------------------------------------------*
145400000000     C* CARSFL - CARICAMENTO SFL
145500000000     C*------------------------------------------------------------------------*
145600000000     C     CARSFL        BEGSR
145700000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
145800000000     C     SFLULT        IFEQ      0
145900000000     C                   EXSR      SETFIL
146000000000     C                   ENDIF
146100000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
146200000000     C                   Z-ADD     SFLULT        WFLULT
146300000000     C* CICLO:
146400000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
146500000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
146600000000     C                   Z-ADD     1             SFLRIG
146700000000     C     SFLRIG        DOWLE     SFLPAG
146800000000     C     SFLEOF        ANDEQ     0
146900000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
147000000000     C                   MOVEL     *BLANKS       VSCOPZ
147100000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
147200000000     C                   EXSR      IRESFL
147300000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
147400000000     C                   ADD       1             SFLULT
147500000000     C* SCRITTURA RECORD SFL
147600000000     C                   WRITE     EC63DVS
147700000000     C                   ADD       1             SFLRIG
147800000000     C* LETTURA FILE DA CARICARE
147900000000     C                   EXSR      LETFIL
148000000000     C                   ENDDO
148100000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
148200000000     C     WFLULT        IFNE      SFLULT
148300000000     C                   Z-ADD     WFLULT        VCCNBR
148400000000     C                   ADD       1             VCCNBR
148500000000     C                   ENDIF
148600000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
148700000000     C     SFLEOF        IFEQ      1
148800000000     C     SFLULT        ANDGT     *ZEROS
148900000000     C                   MOVEL     '1'           $ERR
149000000000     C                   MOVEL     MSG(6)        $MSG
149100000000     C                   EXSR      CARMSG
149200000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
149300000000     C     SFLULT        CHAIN     EC63DVS                            99
149400000000     C     *IN99         IFEQ      '0'
149500000000     C                   SETON                                        45
149600000000     C                   UPDATE    EC63DVS
149700000000     C                   SETOFF                                       45
149800000000     C                   ENDIF
149900000000     C                   ENDIF
150000000000     C                   ENDSR
150100000000     C*------------------------------------------------------------------------*
150200000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
150300000000     C*------------------------------------------------------------------------*
150400000000     C     OUTSFL        BEGSR
150500000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
150600000000     C     MSGULT        IFNE      *ZEROS
150700000000     C                   EXSR      OUTMSG
150800000000     C                   ENDIF
150900000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
151000000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
151100000000     C     SFLEOF        IFEQ      0
151200000000     C                   MOVEL     '0'           *IN42
151300000000     C                   ELSE
151400000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
151500000000     C                   MOVEL     '1'           *IN42
151600000000     C                   ENDIF
151700000000     C* EMISSIONE SUBFILE
151800000000     C     SFLULT        IFLE      0
151900000000     C*  --> CASO DI SFL VUOTO
152000000000     C                   MOVEL     '0'           *IN43
152100000000     C                   WRITE     EC63DVV
152200000000     C                   WRITE     EC63DVC
152300000000     C                   READ      EC63DVC                                99
152400000000     C                   ELSE
152500000000     C*  --> CASO DI SFL CON RECORDS
152600000000     C                   WRITE     EC63DVC
152700000000     C                   READ      EC63DVC                                99
152800000000     C                   ENDIF
152900000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
153000000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
153100000000     C                   Z-ADD     FRTSFL        VCCNBR
153200000000     C                   ENDSR
153300000000     C*------------------------------------------------------------------------*
153400000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
153500000000     C*          CODA MESSAGGI DEL PGM
153600000000     C*------------------------------------------------------------------------*
153700000000     C     CARMSG        BEGSR
153800000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
153900000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
154000000000     C     MSGULT        IFEQ      0
154100000000     C                   MOVEL     '1'           *IN40
154200000000     C                   WRITE     EC63DVK
154300000000     C                   MOVEL     '0'           *IN40
154400000000     C                   ENDIF
154500000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
154600000000     C                   MOVEL     $MSG          DSMSMS
154700000000     C                   CALL      'XMSG'        XMSGPA
154800000000     C     DSMSER        IFEQ      0
154900000000     C                   MOVEL     DSMSKE        VMCKEY
155000000000     C                   ENDIF
155100000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
155200000000     C                   ADD       1             MSGULT            4 0
155300000000     C                   WRITE     EC63DVM
155400000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
155500000000     C                   MOVEL     *BLANKS       $MSG
155600000000     C                   ENDSR
155700000000     C*------------------------------------------------------------------------*
155800000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
155900000000     C*------------------------------------------------------------------------*
156000000000     C     OUTMSG        BEGSR
156100000000     C* VISUALIZZAZIONE MESSAGGIO
156200000000     C                   MOVEL     '1'           *IN44
156300000000     C                   WRITE     EC63DVK
156400000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
156500000000     C                   Z-ADD     *ZEROS        MSGULT
156600000000     C                   ENDSR
156700000000     C*------------------------------------------------------------------------*
156800000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
156900000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
157000000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
157100000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
157200000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
157300000000     C*------------------------------------------------------------------------*
157400000000     C     CARFUN        BEGSR
157500000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
157600000000     C     1             DO        31            I                 5 0
157700000000     C                   MOVEL     FUN(I)        FUNRIG
157800000000     C                   MOVEL     FUNFUN        FFU(I)
157900000000     C                   MOVEL     FUNDES        FDE(I)
158000020923     C     DUTLPO        IFEQ      'S'
158100000000     C     $TGES         IFEQ      'G'
158200000000     C                   MOVEL     FUNASG        FAT(I)
158300000000     C                   ELSE
158400000000     C                   MOVEL     FUNASI        FAT(I)
158500000000     C                   ENDIF
158600000000     C                   ELSE
158700000000     C     $TGES         IFEQ      'G'
158800000000     C                   MOVEL     FUNAFG        FAT(I)
158900000000     C                   ELSE
159000000000     C                   MOVEL     FUNAFI        FAT(I)
159100000000     C                   ENDIF
159200000000     C                   ENDIF
159300000000     C     FAT(I)        IFEQ      'E'
159400000000     C                   MOVEL     USER          WEDP              3
159500000000     C     WEDP          IFEQ      'EDP'
159600000000     C                   MOVEL     'S'           FAT(I)
159700000000     C                   ELSE
159800000000     C                   MOVEL     'N'           FAT(I)
159900000000     C                   ENDIF
160000000000     C                   ENDIF
160100000000     C                   ENDDO
160200000000     C                   ENDSR
160300000000     C*------------------------------------------------------------------------*
160400000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
160500000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
160600000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
160700000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
160800000000     C*------------------------------------------------------------------------*
160900000000     C     OUTFUN        BEGSR
161000000000     C     1             DO        8             I
161100000000     C                   MOVEL     *BLANKS       FUV(I)
161200000000     C                   ENDDO
161300000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
161400000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
161500000000     C     1             DO        31            I
161600000000     C     FAT(I)        IFEQ      'S'
161700000000     C     FDE(I)        ANDNE     *BLANKS
161800000000     C                   ADD       1             FUNNAT
161900000000     C                   ENDIF
162000000000     C                   ENDDO
162100000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
162200000000     C     FUNNAT        IFGT      8
162300000000     C                   MOVEL     'S'           FAT(24)
162400000000     C                   ENDIF
162500000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
162600000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
162700000000     C                   Z-ADD     8             FUNVIS            5 0
162800000000     C     FAT(23)       IFEQ      'S'
162900000000     C                   SUB       1             FUNVIS
163000000000     C                   ENDIF
163100000000     C     FAT(24)       IFEQ      'S'
163200000000     C                   SUB       1             FUNVIS
163300000000     C                   ENDIF
163400000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
163500000000     C                   Z-ADD     *ZEROS        W                 5 0
163600000000     C                   ADD       1             FUNULT
163700000000     C     FUNULT        IFGT      31
163800000000     C                   Z-ADD     1             FUNULT
163900000000     C                   ENDIF
164000000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
164100000000     C     FUNULT        DO        31            I
164200000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
164300000000     C     W             IFLT      FUNVIS
164400000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
164500000000     C     FAT(I)        IFEQ      'S'
164600000000     C     FDE(I)        ANDNE     *BLANKS
164700000000     C     I             IFEQ      23
164800000000     C     I             OREQ      24
164900000000     C                   ELSE
165000000000     C                   ADD       1             W
165100000000     C                   MOVEL     FDE(I)        FUV(W)
165200000000     C                   ENDIF
165300000000     C                   ENDIF
165400000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
165500000000     C                   Z-ADD     I             FUNULT
165600000000     C                   ENDIF
165700000000     C                   ENDDO
165800000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
165900000000     C                   MOVEL     FUV(1)        VFD001
166000000000     C                   MOVEL     FUV(2)        VFD002
166100000000     C                   MOVEL     FUV(3)        VFD003
166200000000     C                   MOVEL     FUV(4)        VFD004
166300000000     C                   MOVEL     FUV(5)        VFD005
166400000000     C                   MOVEL     FUV(6)        VFD006
166500000000     C     FAT(23)       IFEQ      'N'
166600000000     C     FAT(24)       ANDEQ     'N'
166700000000     C                   MOVEL     FUV(7)        VFD007
166800000000     C                   MOVEL     FUV(8)        VFD008
166900000000     C                   ELSE
167000000000     C     FAT(23)       IFEQ      'S'
167100000000     C     FAT(24)       ANDEQ     'S'
167200000000     C                   MOVEL     FDE(23)       VFD007
167300000000     C                   MOVEL     FDE(24)       VFD008
167400000000     C                   ELSE
167500000000     C                   MOVEL     FUV(7)        VFD007
167600000000     C     FAT(23)       IFEQ      'S'
167700000000     C                   MOVEL     FDE(23)       VFD008
167800000000     C                   ELSE
167900000000     C                   MOVEL     FDE(24)       VFD008
168000000000     C                   ENDIF
168100000000     C                   ENDIF
168200000000     C                   ENDIF
168300000000     C                   MOVEL     '1'           $TESVD
168400000000     C                   ENDSR
168500000000     C*------------------------------------------------------------------------*
168600000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
168700000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
168800000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
168900000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
169000000000     C*------------------------------------------------------------------------*
169100000000     C     CONFUN        BEGSR
169200000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
169300000000     C     1             DO        31            I
169400000000     C     KEY           IFEQ      KEYS(I)
169500000000     C* CONTROLLO SE IL TASTO E' ATTIVO
169600000000     C     FAT(I)        IFNE      'S'
169700000000     C                   MOVEL     MSG(1)        $MSG
169800000000     C                   EXSR      CARMSG
169900000000     C                   MOVEL     '1'           $ERR
170000000000     C                   ENDIF
170100000000     C                   ENDIF
170200000000     C                   ENDDO
170300000000     C                   ENDSR
170400000000     C*------------------------------------------------------------------------*
170500000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
170600000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
170700000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
170800000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
170900000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
171000000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
171100000000     C*------------------------------------------------------------------------*
171200000000     C     CAROPZ        BEGSR
171300000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
171400000000     C     1             DO        24            I
171500000000     C                   MOVEL     OPZ(I)        OPZRIG
171600000000     C                   MOVEL     OPZOPD        OPD(I)
171700000000     C                   MOVEL     OPZOPS        OPS(I)
171800000000     C                   MOVEL     OPZDES        ODE(I)
171900020923     C     DUTLPO        IFEQ      'S'
172000000000     C     $TGES         IFEQ      'G'
172100000000     C                   MOVEL     OPZASG        OAT(I)
172200000000     C                   ELSE
172300000000     C                   MOVEL     OPZASI        OAT(I)
172400000000     C                   ENDIF
172500000000     C                   ELSE
172600000000     C     $TGES         IFEQ      'G'
172700000000     C                   MOVEL     OPZAFG        OAT(I)
172800000000     C                   ELSE
172900000000     C                   MOVEL     OPZAFI        OAT(I)
173000000000     C                   ENDIF
173100000000     C                   ENDIF
173200000000     C     OAT(I)        IFEQ      'E'
173300000000     C                   MOVEL     USER          WEDP              3
173400000000     C     WEDP          IFEQ      'EDP'
173500000000     C                   MOVEL     'S'           OAT(I)
173600000000     C                   ELSE
173700000000     C                   MOVEL     'N'           OAT(I)
173800000000     C                   ENDIF
173900000000     C                   ENDIF
174000000000     C                   ENDDO
174100000000     C                   ENDSR
174200000000     C*------------------------------------------------------------------------*
174300000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
174400000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
174500000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
174600000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
174700000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
174800000000     C*------------------------------------------------------------------------*
174900000000     C     OUTOPZ        BEGSR
175000000000     C     1             DO        8             I
175100000000     C                   MOVEL     *BLANKS       OPV(I)
175200000000     C                   ENDDO
175300000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
175400000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
175500000000     C                   ADD       1             OPZULT
175600000000     C     OPZULT        IFGT      24
175700000000     C                   Z-ADD     1             OPZULT
175800000000     C                   END
175900000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
176000000000     C     1             DO        24            I
176100000000     C     OAT(I)        IFEQ      'S'
176200000000     C     ODE(I)        ANDNE     *BLANKS
176300000000     C                   ADD       1             OPZNAT
176400000000     C                   ENDIF
176500000000     C                   ENDDO
176600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
176700000000     C                   Z-ADD     0             W
176800000000     C     OPZULT        DO        24            I
176900000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
177000000000     C     W             IFLT      8
177100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
177200000000     C     OAT(I)        IFEQ      'S'
177300000000     C     ODE(I)        ANDNE     *BLANKS
177400000000     C                   ADD       1             W
177500000000     C                   MOVEL     ODE(I)        OPV(W)
177600000000     C                   ENDIF
177700000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
177800000000     C                   Z-ADD     I             OPZULT
177900000000     C                   ENDIF
178000000000     C                   ENDDO
178100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
178200000000     C                   MOVEL     OPV(1)        VOD001
178300000000     C                   MOVEL     OPV(2)        VOD002
178400000000     C                   MOVEL     OPV(3)        VOD003
178500000000     C                   MOVEL     OPV(4)        VOD004
178600000000     C                   MOVEL     OPV(5)        VOD005
178700000000     C                   MOVEL     OPV(6)        VOD006
178800000000     C                   MOVEL     OPV(7)        VOD007
178900000000     C                   MOVEL     OPV(8)        VOD008
179000000000     C                   MOVEL     '1'           $TESVD
179100000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
179200000000     C     OPZNAT        IFGT      8
179300000000     C                   MOVEL     'S'           FAT(23)
179400000000     C                   ENDIF
179500000000     C                   ENDSR
179600000000     C*------------------------------------------------------------------------*
179700000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
179800000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
179900000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
180000000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
180100000000     C*------------------------------------------------------------------------*
180200000000     C     CONOPZ        BEGSR
180300000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
180400000000     C                   MOVEL     *BLANKS       OPZOK             1
180500000000     C     1             DO        24            I                 5 0
180600000000     C     VSCOPZ        IFEQ      OPD(I)
180700000000     C     VSCOPZ        OREQ      OPS(I)
180800000000     C     OAT(I)        IFEQ      'S'
180900000000     C                   MOVEL     OPD(I)        VSCOPZ
181000000000     C                   MOVEL     'S'           OPZOK
181100000000     C                   ELSE
181200000000     C                   MOVEL     'N'           OPZOK             1
181300000000     C                   ENDIF
181400000000     C                   ENDIF
181500000000     C                   ENDDO
181600000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
181700000000     C     OPZOK         IFNE      'S'
181800000000     C                   MOVEL     MSG(3)        $MSG
181900000000     C                   EXSR      CARMSG
182000000000     C                   MOVEL     '1'           $ERR
182100000000     C                   MOVEL     'N'           OPZOK             1
182200000000     C                   ENDIF
182300000000     C                   ENDSR
182400000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
182500000000TREC63R- Tasto funzionale non valido.                                          1
182600000000TREC63R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
182700000000TREC63R- Opzione non valida.                                                   3
182800000000TREC63R- Opzione abilitata ma non gestita. Avvertire CED.                      4
182900000000TREC63R- Inizio elenco.                                                        5
183000000000TREC63R- Fine elenco.                                                          6
183100000000TREC63R- 7
183200000000TREC63R- 8
183300000000TREC63R- Immettere tipo tariffa 'K'=Cliente, 'F'=Filiale o 'C'=Cartello        9
183400000000TREC63R- 10
183500000000TREC63R- 11
183600000000TREC63R- Codice voce annullato, consentito solo il ripristino                 12
183700000000TREC63R- Codice voce non annullato, impossibile ripristinare                  13
183800000000TREC63R- 14
183900000000TREC63R- 15
184000000000TREC63R- 16
184100000000TREC63R- 17
184200000000TREC63R- 18
184300000000TREC63R- 19
184400000000TREC63R- 20
184500000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
184600000000- 1-1 -                   -    NN     NN
184700000000- 2-2 -2=Modifica         -    SN     NN
184800010112- 3-3 -3=Duplica          -    SN     NN
184900000000- 4-4 -4=Cancellazione    -    SN     NN
185000000000- 5-5 -5=Visualizzazione  -    SS     SS
185100000000- 6-6 -                   -    NN     NN
185200000000- 7-7 -7=Ripristino       -    SN     NN
185300000000- 8-8 -                   -    NN     NN
185400000000- 9-9 -                   -    NN     NN
185500000000-10-  -                   -    NN     NN
185600000000-11-  -                   -    NN     NN
185700000000-12-  -                   -    NN     NN
185800000000-13-  -                   -    NN     NN
185900000000-14-  -                   -    NN     NN
186000000000-15-  -                   -    NN     NN
186100000000-16-  -                   -    NN     NN
186200000000-17-  -                   -    NN     NN
186300000000-18-  -                   -    NN     NN
186400000000-19-  -                   -    NN     NN
186500000000-20-  -                   -    NN     NN
186600000000-21-  -                   -    NN     NN
186700000000-22-  -                   -    NN     NN
186800000000-23-  -                   -    NN     NN
186900000000-24-  -                   -    NN     NN
187000000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
187100000000F1    -                   -    NN     NN
187200000000F2    -                   -    NN     NN
187300000000F3    -F3=Fine            -    SS     SS
187400000000F4    -                   -    NN     NN
187500000000F5    -F5=Rivisualizza    -    SS     SS
187600000000F6    -F6=Inserimento     -    SN     NN
187700000000F7    -                   -    NN     NN
187800000000F8    -                   -    NN     NN
187900000000F9    -                   -    NN     NN
188000000000F10   -                   -    NN     NN
188100000000F11   -                   -    NN     NN
188200000000F12   -F12=Ritorno        -    SS     SS
188300000000F13   -                   -    NN     NN
188400000000F14   -F14=Parzializza    -    SS     SS
188500000000F15   -                   -    NN     NN
188600000000F16   -                   -    NN     NN
188700961003F17   -F17=Inizio dati    -    NN     NN
188800961003F18   -F18=Fine dati      -    NN     NN
188900000000F19   -                   -    NN     NN
189000000000F20   -                   -    NN     NN
189100000000F21   -                   -    NN     NN
189200000000F22   -                   -    NN     NN
189300000000F23   -F23=Altre opzioni  -    NN     NN
189400000000F24   -F24=Altri tasti    -    NN     NN
189500000000ENTER -                   -    SS     SS
189600000000ROLLUP-                   -    SS     SS
189700000000ROLLDN-                   -    SS     SS
189800000000HELP  -                   -    NN     NN
189900000000PRINT -                   -    NN     NN
190000000000BSPACE-                   -    NN     NN
190100000000CLEAR -                   -    NN     NN

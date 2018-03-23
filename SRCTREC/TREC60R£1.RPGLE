000100000000      *------------------------------------------------------------------------*
000200000000      *  Nome programma:  TREC60R
000300000000      *  Descrizione   :  Gestione voci di conto economico
000400000000      *                   Programma visualizzazione sfl di scelta
000500000000      *  Autore        :  STEFANO (SC)
000600000000      *  Data creazione:  05 MAG 1994
000700000000      *------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTREC60D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(EC60DVS:SFLULT)
001400000000     F                                     SFILE(EC60DVM:MSGULT)
001500000000     FECEVT01L  IF   E           K DISK
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
008500010110     D DS61          E DS                  EXTNAME(TREC61DS)
008600000000     D*---------------------
008700000000     D* ARCHITETTURA
008800000000     D*---------------------
008900000000     D KPJBA         E DS
009000000000     D*------------------
009100000000     D* CALL XSRDA8 - CONTROLLO DATE
009200000000     D*------------------
009300000000     D WLBDAT          DS                  INZ
009400000000     D  G02DAT                 1      8  0
009500000000     D  G02INV                 9     16  0
009600000000     D  G02ERR                17     17
009700000000     D  G02TGI                18     22  0
009800000000     D*------------------
009900000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
010000000000     D*------------------
010100000000     D DESGES          C                   CONST('** GESTIONE **')
010200000000     D DESINT          C                   CONST('INTERROGAZIONE')
010300000000     C*------------------------------------------------------------------------*
010400000000     C* MAIN LINE
010500000000     C*------------------------------------------------------------------------*
010600000000     C*
010700000000     C* CICLO FINO A CHE:
010800000000DO  1C     $FINE         DOUEQ     '1'
010900000000     C*
011000000000     C                   MOVEL     '0'           $ERR
011100000000     C*
011200000000     C* CONTROLLA GESTIONE VIDEO
011300000000IF  2C     $GES          IFEQ      'VS'
011400000000     C                   EXSR      GESVS
011500000000E   2C                   ENDIF
011600000000     C*
011700000000E   1C                   ENDDO
011800000000     C* FINE PGM
011900000000     C                   SETON                                        LR
012000000000     C*------------------------------------------------------------------------*
012100000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
012200000000     C*------------------------------------------------------------------------*
012300000000     C     CARVS         BEGSR
012400000000     C*
012500000000     C                   MOVEL     'VS'          $GES
012600000000     C*
012700000000     C                   EXSR      CAROPZ
012800000000     C                   EXSR      OUTOPZ
012900000000     C                   MOVEA     FUN2          FUN
013000000000     C                   EXSR      CARFUN
013100000000     C                   EXSR      OUTFUN
013200000000     C*
013300000000     C                   WRITE     EC60DVT
013400000000     C                   WRITE     EC60DVO
013500000000     C                   WRITE     EC60DVF
013600000000     C*
013700000000     C                   EXSR      POSSFL
013800000000     C*
013900000000     C                   ENDSR
014000000000     C*------------------------------------------------------------------------*
014100000000     C* GESVS  - GESTIONE SUBFILE
014200000000     C*------------------------------------------------------------------------*
014300000000     C     GESVS         BEGSR
014400000000     C*
014500000000IF  1C     $TESVD        IFEQ      '1'
014600000000     C                   WRITE     EC60DVT
014700000000     C                   WRITE     EC60DVO
014800000000     C                   WRITE     EC60DVF
014900000000E   1C                   ENDIF
015000000000     C*
015100000000     C                   EXSR      OUTSFL
015200000000     C*
015300000000     C                   EXSR      CONFUN
015400000000     C*
015500000000IF  1C     $ERR          IFEQ      '0'
015600000000SL  2C                   SELECT
015700000000     C     KEY           WHENEQ    F6
015800000000     C                   EXSR      F06GES
015900000000     C                   OTHER
016000000000     C                   EXSR      FUNSFL
016100000000E   2C                   ENDSL
016200000000E   1C                   ENDIF
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*------------------------------------------------------------------------*
016600000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
016700000000     C*------------------------------------------------------------------------*
016800000000     C     F06GES        BEGSR
016900000000     C*
017000000000     C* LANCIO L'INSERIMENTO
017100000000     C                   Z-ADD     *ZEROS        WVOC
017200000000     C                   Z-ADD     *ZEROS        WPRG
017300000000     C                   Z-ADD     *ZEROS        WDDV
017400000000     C*
017500000000     C                   MOVEL     *BLANKS       WTTV
017600000000     C                   Z-ADD     *ZEROS        WFT1
017700000000     C                   Z-ADD     *ZEROS        WFT2
017800000000     C                   Z-ADD     *ZEROS        WCLI
017900000000     C                   Z-ADD     *ZERO         WCTR
018000000000     C                   MOVE      *BLANKS       WCTS
018100000000     C                   Z-ADD     *ZERO         WCAP
018200000000     C                   Z-ADD     *ZEROS        WDDT
018300000000     C                   Z-ADD     *ZEROS        WSGI
018400000000     C                   Z-ADD     *ZEROS        WVOM
018500000000     C                   Z-ADD     *ZEROS        WPRM
018600000000     C                   Z-ADD     *ZERO         WCMP
018700000000     C                   Z-ADD     *ZERO         WNBC
018800000000     C                   Z-ADD     *ZEROS        WDDC
018900000000     C                   Z-ADD     *ZEROS        WKCC
019000000000     C                   Z-ADD     *ZEROS        WKSC
019100000000     C                   MOVEL     *BLANKS       WRIC
019200000000     C                   Z-ADD     *ZERO         WDDO
019300940719     C                   MOVEL     *BLANKS       WFRA
019400940725     C                   MOVEL     *BLANKS       WIOE
019500940719     C                   Z-ADD     *ZERO         WCCF
019600000000     C*
019700000000     C                   MOVEL     D60OP0        WOP0
019800000000     C                   MOVEL     'F06'         WOP1
019900000000     C                   MOVEL     'TREC61R'     WPGM              8
020000000000     C                   EXSR      LANPGM
020100000000     C*
020200000000     C                   EXSR      POSSFL
020300000000     C*
020400000000     C                   ENDSR
020500000000     C*------------------------------------------------------------------------*
020600000000     C*  GESOPZ - GESTIONE OPZIONI
020700000000     C*------------------------------------------------------------------------*
020800000000     C     GESOPZ        BEGSR
020900000000     C*
021000000000SL  1C                   SELECT
021100000000     C     VSCOPZ        WHENEQ    ' 2'
021200000000     C                   EXSR      O02GES
021300010110     C     VSCOPZ        WHENEQ    ' 3'
021400010110     C                   EXSR      O03GES
021500000000     C     VSCOPZ        WHENEQ    ' 4'
021600000000     C                   EXSR      O04GES
021700000000     C     VSCOPZ        WHENEQ    ' 5'
021800000000     C                   EXSR      O05GES
021900000000     C     VSCOPZ        WHENEQ    ' 7'
022000000000     C                   EXSR      O07GES
022100000000     C     VSCOPZ        WHENEQ    ' 8'
022200000000     C                   EXSR      O08GES
022300000000     C     VSCOPZ        WHENEQ    ' 9'
022400000000     C                   EXSR      O09GES
022500000000     C     VSCOPZ        WHENEQ    '10'
022600000000     C                   EXSR      O10GES
022700000000     C                   OTHER
022800000000     C                   MOVEL     MSG(4)        $MSG
022900000000     C                   EXSR      CARMSG
023000000000E   1C                   ENDSL
023100000000     C*
023200000000     C                   ENDSR
023300000000     C*------------------------------------------------------------------------*
023400000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
023500000000     C*------------------------------------------------------------------------*
023600000000     C     O02GES        BEGSR
023700000000     C*
023800000000IF  1C     VSCATB        IFEQ      'A'
023900000000     C                   MOVEL     '1'           $ERR
024000000000     C                   MOVEL     MSG(12)       $MSG
024100000000     C                   EXSR      CARMSG
024200000000E   1C                   ENDIF
024300000000     C*
024400000000IF  1C     $ERR          IFEQ      '0'
024500000000     C*
024600000000     C* LANCIO LA MODIFICA
024700000000     C                   Z-ADD     VSCVOC        WVOC
024800000000     C                   Z-ADD     VSCPRG        WPRG
024900000000     C                   Z-ADD     VSCDDV        WDDV
025000000000     C                   MOVEL     *BLANKS       WTTV
025100000000     C                   Z-ADD     *ZEROS        WFT1
025200000000     C                   Z-ADD     *ZEROS        WFT2
025300000000     C                   Z-ADD     *ZEROS        WCLI
025400000000     C                   Z-ADD     *ZERO         WCTR
025500000000     C                   MOVEL     *BLANKS       WCTS
025600000000     C                   Z-ADD     *ZERO         WCAP
025700000000     C                   Z-ADD     *ZEROS        WDDT
025800000000     C                   Z-ADD     *ZEROS        WSGI
025900000000     C                   Z-ADD     *ZEROS        WVOM
026000000000     C                   Z-ADD     *ZEROS        WPRM
026100000000     C                   Z-ADD     *ZERO         WCMP
026200000000     C                   Z-ADD     *ZERO         WNBC
026300000000     C                   Z-ADD     *ZEROS        WDDC
026400000000     C                   Z-ADD     *ZEROS        WKCC
026500000000     C                   Z-ADD     *ZEROS        WKSC
026600000000     C                   MOVEL     *BLANKS       WRIC
026700000000     C                   Z-ADD     *ZERO         WDDO
026800940725     C                   MOVEL     *BLANKS       WFRA
026900940725     C                   MOVEL     *BLANKS       WIOE
027000940719     C                   Z-ADD     *ZERO         WCCF
027100000000     C*
027200000000     C                   MOVEL     D60OP0        WOP0
027300000000     C                   MOVEL     'O02'         WOP1
027400000000     C                   MOVEL     'TREC61R'     WPGM              8
027500000000     C                   EXSR      LANPGM
027600000000     C*
027700000000     C                   Z-ADD     1             SWMODI
027800000000     C*
027900000000E   1C                   ENDIF
028000000000     C*
028100000000     C                   ENDSR
028200010110     C*------------------------------------------------------------------------*
028300010110     C*  O03GES - GESTIONE OPZIONE 3=DUPLICA
028400010110     C*------------------------------------------------------------------------*
028500010110     C     O03GES        BEGSR
028600010110     C*
028700010110IF  1C     VSCATB        IFEQ      'A'
028800010110     C                   MOVEL     '1'           $ERR
028900010110     C                   MOVEL     MSG(12)       $MSG
029000010110     C                   EXSR      CARMSG
029100010110E   1C                   ENDIF
029200010110     C*
029300010110IF  1C     $ERR          IFEQ      '0'
029400010111     C                   SETOFF                                       KKKL
029500010111     C                   MOVEL     'N'           VCCPAR
029600010111     C                   Z-ADD     WN8           VCCDRI
029700010110     C     *INKK         DOWEQ     *OFF
029800010110     C     *INKL         ANDEQ     *OFF
029900010110     C                   EXFMT     EC60DWW
030000010111     C* CONTROLLO LA DATA DI RIFERIMENTO
030100010111     C                   Z-ADD     *ZERO         INVDRI            8 0
030200010111     C                   Z-ADD     VCCDRI        G02DAT
030300010111     C                   Z-ADD     *ZEROS        G02INV
030400010111     C                   MOVEL     '0'           G02ERR
030500010111     C                   CALL      'XSRDA8'
030600010111     C                   PARM                    WLBDAT
030700010111     C*
030800010111     C     G02ERR        IFEQ      '1'
030900010111     C                   MOVEL     '1'           $ERR
031000010111     C                   MOVEL     MSG(8)        $MSG
031100010115     C                   EXSR      CARMSG
031200010111     C                   SETOFF                                       KK
031300010111     C                   SETON                                        55
031400010111     C                   ELSE
031500010111     C                   Z-ADD     G02INV        INVDRI            8 0
031600010111     C                   SETOFF                                       55
031700010111     C                   ENDIF
031800010111     C*
031900010110     C                   ENDDO
032000010110     C*
032100010110     C* LANCIO LA DUPLICA
032200010110     C     *INKK         IFEQ      *ON
032300010110     C                   Z-ADD     VSCVOC        WVOC
032400010110     C                   Z-ADD     VSCPRG        WPRG
032500010110     C                   Z-ADD     VSCDDV        WDDV
032600010110     C                   MOVEL     *BLANKS       WTTV
032700010110     C                   Z-ADD     *ZEROS        WFT1
032800010110     C                   Z-ADD     *ZEROS        WFT2
032900010110     C                   Z-ADD     *ZEROS        WCLI
033000010110     C                   Z-ADD     *ZERO         WCTR
033100010110     C                   MOVEL     *BLANKS       WCTS
033200010110     C                   Z-ADD     *ZERO         WCAP
033300010110     C                   Z-ADD     *ZEROS        WDDT
033400010110     C                   Z-ADD     *ZEROS        WSGI
033500010110     C                   Z-ADD     *ZEROS        WVOM
033600010110     C                   Z-ADD     *ZEROS        WPRM
033700010110     C                   Z-ADD     *ZERO         WCMP
033800010110     C                   Z-ADD     *ZERO         WNBC
033900010110     C                   Z-ADD     *ZEROS        WDDC
034000010110     C                   Z-ADD     *ZEROS        WKCC
034100010110     C                   Z-ADD     *ZEROS        WKSC
034200010110     C                   MOVEL     *BLANKS       WRIC
034300010110     C                   Z-ADD     *ZERO         WDDO
034400010110     C                   MOVEL     *BLANKS       WFRA
034500010110     C                   MOVEL     *BLANKS       WIOE
034600010110     C                   Z-ADD     *ZERO         WCCF
034700010110     C*
034800010110     C                   MOVEL     D60OP0        WOP0
034900010110     C                   MOVEL     'O03'         WOP1
035000010110     C                   MOVEL     'TREC61R'     WPGM              8
035100010110     C                   EXSR      LANPGM
035200010110     C*
035300010111     C* LANCIO SE RICHIESTO LA DUPLICA ANCHE DELLE TARIFFE E SE OK DUPLICA VOCE
035400010115     C     D60ERR        IFNE      '1'
035500010110     C     VCCPAR        ANDEQ     'S'
035600010115     C* PLISCO IL SUB-FILE MESSAGGI E GESTISCO DIFFERENZIAZIONE MESSAGGIO
035700010115     C     D60ERR        IFEQ      '2'
035800010115     C                   Z-ADD     *ZEROS        MSGULT
035900010115     C                   MOVEL     MSG(11)       $MSG
036000010115     C                   EXSR      CARMSG
036100010115     C                   ENDIF
036200010110     C                   Z-ADD     VSCVOC        I61VOC
036300010110     C                   Z-ADD     VSCPRG        I61PRG
036400010111     C                   Z-ADD     D60DDV        I61DAI
036500010111     C                   Z-ADD     D60DDT        I61DAF
036600010111     C                   Z-ADD     INVDRI        I61DRI
036700010110     C                   Z-ADD     D60VOC        O61VOC
036800010110     C                   Z-ADD     D60PRG        O61PRG
036900010110     C                   CALL      'TREC61R1'
037000010110     C                   PARM                    DS60
037100010110     C                   PARM                    DS61
037200010110     C                   ENDIF
037300010110     C*
037400010110     C                   Z-ADD     1             SWMODI
037500010110     C*
037600010110     C                   ENDIF
037700010110E   1C                   ENDIF
037800010110     C*
037900010110     C                   ENDSR
038000000000     C*------------------------------------------------------------------------*
038100000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
038200000000     C*------------------------------------------------------------------------*
038300000000     C     O04GES        BEGSR
038400000000     C*
038500000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
038600000000IF  1C     VSCATB        IFEQ      'A'
038700000000     C                   MOVEL     '1'           $ERR
038800000000     C                   MOVEL     MSG(12)       $MSG
038900000000     C                   EXSR      CARMSG
039000000000E   1C                   ENDIF
039100000000     C*
039200000000IF  1C     $ERR          IFEQ      '0'
039300000000     C*
039400000000     C* LANCIO LA CANCELLAZIONE
039500000000     C                   Z-ADD     VSCVOC        WVOC
039600000000     C                   Z-ADD     VSCPRG        WPRG
039700000000     C                   Z-ADD     VSCDDV        WDDV
039800000000     C                   MOVEL     *BLANKS       WTTV
039900000000     C                   Z-ADD     *ZEROS        WFT1
040000000000     C                   Z-ADD     *ZEROS        WFT2
040100000000     C                   Z-ADD     *ZEROS        WCLI
040200000000     C                   Z-ADD     *ZERO         WCTR
040300000000     C                   MOVEL     *BLANKS       WCTS
040400000000     C                   Z-ADD     *ZERO         WCAP
040500000000     C                   Z-ADD     *ZEROS        WDDT
040600000000     C                   Z-ADD     *ZEROS        WSGI
040700000000     C                   Z-ADD     *ZEROS        WVOM
040800000000     C                   Z-ADD     *ZEROS        WPRM
040900000000     C                   Z-ADD     *ZERO         WCMP
041000000000     C                   Z-ADD     *ZERO         WNBC
041100000000     C                   Z-ADD     *ZEROS        WDDC
041200000000     C                   Z-ADD     *ZEROS        WKCC
041300000000     C                   Z-ADD     *ZEROS        WKSC
041400000000     C                   MOVEL     *BLANKS       WRIC
041500000000     C                   Z-ADD     *ZERO         WDDO
041600940719     C                   MOVEL     *BLANKS       WFRA
041700940725     C                   MOVEL     *BLANKS       WIOE
041800940719     C                   Z-ADD     *ZERO         WCCF
041900000000     C*
042000000000     C                   MOVEL     D60OP0        WOP0
042100000000     C                   MOVEL     'O04'         WOP1
042200000000     C                   MOVEL     'TREC61R'     WPGM              8
042300000000     C                   EXSR      LANPGM
042400000000     C*
042500000000     C                   Z-ADD     1             SWMODI
042600000000     C*
042700000000E   1C                   ENDIF
042800000000     C*
042900000000     C                   ENDSR
043000000000     C*------------------------------------------------------------------------*
043100000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
043200000000     C*------------------------------------------------------------------------*
043300000000     C     O05GES        BEGSR
043400000000     C*
043500000000     C* LANCIO LA VISUALIZZAZIONE
043600000000     C                   Z-ADD     VSCVOC        WVOC
043700000000     C                   Z-ADD     VSCPRG        WPRG
043800000000     C                   Z-ADD     VSCDDV        WDDV
043900000000     C                   MOVEL     *BLANKS       WTTV
044000000000     C                   Z-ADD     *ZEROS        WFT1
044100000000     C                   Z-ADD     *ZEROS        WFT2
044200000000     C                   Z-ADD     *ZEROS        WCLI
044300000000     C                   Z-ADD     *ZERO         WCTR
044400000000     C                   MOVE      *BLANKS       WCTS
044500000000     C                   Z-ADD     *ZERO         WCAP
044600000000     C                   Z-ADD     *ZEROS        WDDT
044700000000     C                   Z-ADD     *ZEROS        WSGI
044800000000     C                   Z-ADD     *ZEROS        WVOM
044900000000     C                   Z-ADD     *ZEROS        WPRM
045000000000     C                   Z-ADD     *ZERO         WCMP
045100000000     C                   Z-ADD     *ZERO         WNBC
045200000000     C                   Z-ADD     *ZEROS        WDDC
045300000000     C                   Z-ADD     *ZEROS        WKCC
045400000000     C                   Z-ADD     *ZEROS        WKSC
045500000000     C                   MOVEL     *BLANKS       WRIC
045600000000     C                   Z-ADD     *ZERO         WDDO
045700940719     C                   MOVEL     *BLANKS       WFRA
045800940725     C                   MOVEL     *BLANKS       WIOE
045900940719     C                   Z-ADD     *ZERO         WCCF
046000000000     C*
046100000000     C                   MOVEL     D60OP0        WOP0
046200000000     C                   MOVEL     'O05'         WOP1
046300000000     C                   MOVEL     'TREC61R'     WPGM              8
046400000000     C                   EXSR      LANPGM
046500000000     C*
046600000000     C                   ENDSR
046700000000     C*------------------------------------------------------------------------*
046800000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
046900000000     C*------------------------------------------------------------------------*
047000000000     C     O07GES        BEGSR
047100000000     C*
047200000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
047300000000IF  1C     VSCATB        IFNE      'A'
047400000000     C                   MOVEL     '1'           $ERR
047500000000     C                   MOVEL     MSG(13)       $MSG
047600000000     C                   EXSR      CARMSG
047700000000E   1C                   ENDIF
047800000000     C*
047900000000IF  1C     $ERR          IFEQ      '0'
048000000000     C*
048100000000     C* LANCIO IL RIPRISTINO
048200000000     C                   Z-ADD     VSCVOC        WVOC
048300000000     C                   Z-ADD     VSCPRG        WPRG
048400000000     C                   Z-ADD     VSCDDV        WDDV
048500000000     C                   MOVEL     *BLANKS       WTTV
048600000000     C                   Z-ADD     *ZEROS        WFT1
048700000000     C                   Z-ADD     *ZEROS        WFT2
048800000000     C                   Z-ADD     *ZEROS        WCLI
048900000000     C                   Z-ADD     *ZERO         WCTR
049000000000     C                   MOVE      *BLANKS       WCTS
049100000000     C                   Z-ADD     *ZERO         WCAP
049200000000     C                   Z-ADD     *ZEROS        WDDT
049300000000     C                   Z-ADD     *ZEROS        WSGI
049400000000     C                   Z-ADD     *ZEROS        WVOM
049500000000     C                   Z-ADD     *ZEROS        WPRM
049600000000     C                   Z-ADD     *ZERO         WCMP
049700000000     C                   Z-ADD     *ZERO         WNBC
049800000000     C                   Z-ADD     *ZEROS        WDDC
049900000000     C                   Z-ADD     *ZEROS        WKCC
050000000000     C                   Z-ADD     *ZEROS        WKSC
050100000000     C                   MOVEL     *BLANKS       WRIC
050200000000     C                   Z-ADD     *ZEROS        WDDO
050300940719     C                   MOVEL     *BLANKS       WFRA
050400940725     C                   MOVEL     *BLANKS       WIOE
050500940719     C                   Z-ADD     *ZEROS        WCCF
050600000000     C*
050700000000     C                   MOVEL     D60OP0        WOP0
050800000000     C                   MOVEL     'O07'         WOP1
050900000000     C                   MOVEL     'TREC61R'     WPGM              8
051000000000     C                   EXSR      LANPGM
051100000000     C*
051200000000     C                   Z-ADD     1             SWMODI
051300000000     C*
051400000000E   1C                   ENDIF
051500000000     C*
051600000000     C                   ENDSR
051700000000     C*------------------------------------------------------------------------*
051800000000     C*  O08GES - GESTIONE OPZIONE 8=GESTIONE TARIFFE VOCE
051900000000     C*------------------------------------------------------------------------*
052000000000     C     O08GES        BEGSR
052100000000     C*
052200000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
052300000000     C                   Z-ADD     VSCVOC        WVOC
052400000000     C                   Z-ADD     VSCPRG        WPRG
052500000000     C                   Z-ADD     *ZEROS        WDDV
052600950529     C*---                 Z-ADDVSCDDV    WDDT             *DATA DECORRENZA TAR.
052700950529     C                   Z-ADD     *ZEROS        WDDT
052800000000     C                   MOVEL     *BLANKS       WTTV
052900000000     C                   Z-ADD     *ZEROS        WFT1
053000000000     C                   Z-ADD     *ZEROS        WFT2
053100000000     C                   Z-ADD     *ZEROS        WCLI
053200000000     C                   Z-ADD     *ZERO         WCTR
053300000000     C                   MOVE      *BLANKS       WCTS
053400000000     C                   Z-ADD     *ZERO         WCAP
053500000000     C                   Z-ADD     *ZEROS        WSGI
053600000000     C                   Z-ADD     *ZEROS        WVOM
053700000000     C                   Z-ADD     *ZEROS        WPRM
053800000000     C                   Z-ADD     *ZERO         WCMP
053900000000     C                   Z-ADD     *ZERO         WNBC
054000000000     C                   Z-ADD     *ZEROS        WDDC
054100000000     C                   Z-ADD     *ZEROS        WKCC
054200000000     C                   Z-ADD     *ZEROS        WKSC
054300000000     C                   MOVEL     *BLANKS       WRIC
054400000000     C                   Z-ADD     *ZEROS        WDDO
054500940719     C                   MOVEL     *BLANKS       WFRA
054600940725     C                   MOVEL     *BLANKS       WIOE
054700940719     C                   Z-ADD     *ZEROS        WCCF
054800000000     C*
054900000000     C                   MOVEL     D60OP0        WOP0
055000000000     C                   MOVEL     'O08'         WOP1
055100000000     C                   MOVEL     'TREC63R'     WPGM              8
055200000000     C                   EXSR      LANPGM
055300000000     C*
055400000000     C                   ENDSR
055500000000     C*------------------------------------------------------------------------*
055600000000     C*  O09GES - GESTIONE OPZIONE 9=GESTIONE COMPETENZE
055700000000     C*------------------------------------------------------------------------*
055800000000     C     O09GES        BEGSR
055900000000     C*
056000000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
056100000000     C                   Z-ADD     VSCVOC        WVOC
056200000000     C                   Z-ADD     VSCPRG        WPRG
056300000000     C                   Z-ADD     VSCDDV        WDDV
056400000000     C                   MOVEL     *BLANKS       WTTV
056500000000     C                   Z-ADD     *ZEROS        WFT1
056600000000     C                   Z-ADD     *ZEROS        WFT2
056700000000     C                   Z-ADD     *ZEROS        WCLI
056800000000     C                   Z-ADD     *ZERO         WCTR
056900000000     C                   MOVE      *BLANKS       WCTS
057000000000     C                   Z-ADD     *ZERO         WCAP
057100000000     C                   Z-ADD     *ZEROS        WDDT
057200000000     C                   Z-ADD     *ZEROS        WSGI
057300000000     C                   Z-ADD     *ZEROS        WVOM
057400000000     C                   Z-ADD     *ZEROS        WPRM
057500040209     C                   Z-ADD     vsccmp        WCMP
057600000000     C                   Z-ADD     *ZERO         WNBC
057700000000     C                   Z-ADD     *ZEROS        WDDC
057800000000     C                   Z-ADD     *ZEROS        WKCC
057900000000     C                   Z-ADD     *ZEROS        WKSC
058000000000     C                   MOVEL     *BLANKS       WRIC
058100000000     C                   Z-ADD     *ZEROS        WDDO
058200940719     C                   MOVEL     *BLANKS       WFRA
058300940725     C                   MOVEL     *BLANKS       WIOE
058400940719     C                   Z-ADD     *ZEROS        WCCF
058500000000     C*
058600000000     C                   MOVEL     D60OP0        WOP0
058700000000     C                   MOVEL     'O09'         WOP1
058800000000     C                   MOVEL     'TREC72R'     WPGM              8
058900000000     C                   EXSR      LANPGM
059000000000     C*
059100000000     C                   ENDSR
059200000000     C*------------------------------------------------------------------------*
059300000000     C*  O10GES - GESTIONE OPZIONE 10=GESTIONE MAGGIORAZIONI
059400000000     C*------------------------------------------------------------------------*
059500000000     C     O10GES        BEGSR
060400000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
060500000000IF  1C     $ERR          IFEQ      '0'
060800091021     C                   Z-ADD     *ZEROS        WVOM
060900091021     C                   Z-ADD     *ZERO         WPRM
060901091021     C                   Z-ADD     *ZEROS        WVOC
060902091021     C                   Z-ADD     *ZERO         WPRG
061000000000     C                   Z-ADD     *ZEROS        WDDV
061100000000     C                   MOVEL     *BLANKS       WTTV
061200000000     C                   Z-ADD     *ZEROS        WFT1
061300000000     C                   Z-ADD     *ZEROS        WFT2
061400000000     C                   Z-ADD     *ZEROS        WCLI
061500000000     C                   Z-ADD     *ZERO         WCTR
061600000000     C                   MOVE      *BLANKS       WCTS
061700000000     C                   Z-ADD     *ZERO         WCAP
061800000000     C                   Z-ADD     *ZEROS        WDDT
061900000000     C                   Z-ADD     *ZEROS        WSGI
062000000000     C                   Z-ADD     *ZERO         WCMP
062100000000     C                   Z-ADD     *ZERO         WNBC
062200000000     C                   Z-ADD     *ZEROS        WDDC
062300000000     C                   Z-ADD     *ZEROS        WKCC
062400000000     C                   Z-ADD     *ZEROS        WKSC
062500000000     C                   MOVEL     *BLANKS       WRIC
062600000000     C                   Z-ADD     *ZEROS        WDDO
062700940719     C                   MOVEL     *BLANKS       WFRA
062800940725     C                   MOVEL     *BLANKS       WIOE
062900940719     C                   Z-ADD     *ZEROS        WCCF
063000091021     C* maggiorazioni
063001091021     C     VSCTVO        IFEQ      'M'
063002091021     C                   Z-ADD     VSCVOC        WVOM
063003091021     C                   Z-ADD     VSCPRG        WPRM
063004091021     c                   else
063005091021     C                   Z-ADD     VSCVOC        WVOC
063006091021     C                   Z-ADD     VSCPRG        WPRG
063007091021     c                   end
063100000000     C                   MOVEL     D60OP0        WOP0
063200000000     C                   MOVEL     'O10'         WOP1
063201091021     C     VSCTVO        IFEQ      'M'
063300000000     C                   MOVEL     'TREC66R'     WPGM              8
063301091021     c                   else
063302091021     C                   MOVEL     'TREC68R'     WPGM              8
063303091021     c                   end
063400000000     C                   EXSR      LANPGM
063500000000     C                   ENDIF
063600000000     C*
063700000000     C                   ENDSR
063800000000     C*------------------------------------------------------------------------*
063900000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
064000000000     C*------------------------------------------------------------------------*
064100000000     C     NEWPOS        BEGSR
064200000000     C*
064300000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
064400000000     C*        1 = RICHIESTO POSIZIONAMENTO
064500000000     C                   Z-ADD     0             SWPOSI            7 0
064600000000     C*
064700000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
064800000000     C*            NON OCCORRE RICARICARE IL SUBFILE
064900000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
065000000000     C                   Z-ADD     0             SWMODI            1 0
065100000000     C*
065200000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
065300000000IF  1C     VCCVOC        IFNE      DEPVOC
065400000000     C     VCCPRG        ORNE      DEPPRG
065500000000     C     VCCDDV        ORNE      DEPDDV
065600010111     C     VCCVAL        ORNE      DEPVAL
065601040209     C     VCCcmp        ORNE      DEPcmp
065700000000     C*
065800000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
065900000000     C                   EXSR      CHKPAR
066000000000     C*
066100000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
066200000000IF  2C     $ERR          IFEQ      '0'
066300000000     C                   Z-ADD     1             SWPOSI
066400000000     C                   Z-ADD     VCCVOC        DEPVOC
066500000000     C                   Z-ADD     VCCPRG        DEPPRG
066600000000     C                   Z-ADD     VCCDDV        DEPDDV
066700010111     C                   Z-ADD     VCCVAL        DEPVAL
066701040209     C                   Z-ADD     VCCcmp        DEPcmp
066800000000E   2C                   ENDIF
066900000000E   1C                   ENDIF
067000000000     C*
067100000000     C                   ENDSR
067200000000     C*------------------------------------------------------------------------*
067300000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
067400000000     C*------------------------------------------------------------------------*
067500000000     C     CHKPAR        BEGSR
067600000000     C*
067700000000     C* REIMPOSTO INDICATORI DI ERRORE
067800010111     C                   SETOFF                                       808183
067900000000     C*--------------------
068000000000     C* DATA DECORRENZA VOCE
068100000000     C*--------------------
068200000000     C                   Z-ADD     *ZERO         DDVVCC
068300000000IF  1C     VCCDDV        IFGT      *ZEROS
068400000000     C                   Z-ADD     VCCDDV        G02DAT
068500000000     C                   Z-ADD     *ZEROS        G02INV
068600000000     C                   MOVEL     '0'           G02ERR
068700000000     C                   CALL      'XSRDA8'
068800000000     C                   PARM                    WLBDAT
068900000000     C*
069000000000IF  2C     G02ERR        IFEQ      '1'
069100000000     C                   MOVEL     '1'           $ERR
069200000000     C                   MOVEL     MSG(9)        $MSG
069300000000     C                   EXSR      CARMSG
069400000000     C                   SETON                                        81
069500000000X   2C                   ELSE
069600000000     C                   Z-ADD     G02DAT        VCCDDV
069700000000     C                   Z-ADD     G02INV        DDVVCC            8 0
069800000000E   2C                   ENDIF
069900000000E   1C                   ENDIF
070000010111     C*--------------------
070100010111     C* VALIDITA' AL
070200010111     C*--------------------
070300010111     C                   Z-ADD     *ZERO         VALVCC            8 0
070400010111IF  1C     VCCVAL        IFGT      *ZEROS
070500010111     C                   Z-ADD     VCCVAL        G02DAT
070600010111     C                   Z-ADD     *ZEROS        G02INV
070700010111     C                   MOVEL     '0'           G02ERR
070800010111     C                   CALL      'XSRDA8'
070900010111     C                   PARM                    WLBDAT
071000010111     C*
071100010111IF  2C     G02ERR        IFEQ      '1'
071200010111     C                   MOVEL     '1'           $ERR
071300010111     C                   MOVEL     MSG(10)       $MSG
071400010111     C                   EXSR      CARMSG
071500010111     C                   SETON                                        83
071600010111X   2C                   ELSE
071700010111     C                   Z-ADD     G02DAT        VCCVAL
071800010111     C                   Z-ADD     G02INV        VALVCC            8 0
071900010111E   2C                   ENDIF
072000010111E   1C                   ENDIF
072100000000     C*
072200000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
072300000000     C* SUL CAMPO DEL CONTROL
072400000000IF  1C     $ERR          IFEQ      '1'
072500000000     C                   MOVEL     '0'           *IN43
072600000000X   1C                   ELSE
072700000000     C                   MOVEL     '1'           *IN43
072800000000E   1C                   ENDIF
072900000000     C*
073000000000     C                   ENDSR
073100000000     C*------------------------------------------------------------------------*
073200000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD VOCE DI C/E
073300000000     C*------------------------------------------------------------------------*
073400000000     C     SETFIL        BEGSR
073500000000     C*
073600000000     C                   MOVEL     '1'           *IN43
073700000000     C*
073800000000     C* POSIZIONAMENTO ARCHIVIO VOCI DI C/E
073900000000     C                   Z-ADD     VCCVOC        KVTVOC
074000000000     C                   Z-ADD     VCCPRG        KVTPRG
074100000000     C     KEYEVT        SETLL     ECEVT01L                           99
074200000000     C*
074300000000     C  N99              EXSR      LETFIL
074400000000     C*
074500000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
074600000000IF  1C     *IN99         IFEQ      '1'
074700000000     C                   Z-ADD     1             SFLEOF
074800000000E   1C                   ENDIF
074900000000     C*
075000000000     C                   ENDSR
075100000000     C*------------------------------------------------------------------------*
075200000000     C* LETFIL - LEGGE RECORD SUCESSIVO VOCE DI C/E
075300000000     C*------------------------------------------------------------------------*
075400000000     C     LETFIL        BEGSR
075500000000     C*
075600000000     C* LEGGO FINO A:
075700000000     C*     - FINE FILE
075800000000     C*     - TROVATO RECORD VALIDO
075900000000     C                   MOVEL     '0'           WRECOK            1
076000000000DO  1C     *IN99         DOWEQ     *OFF
076100000000     C     WRECOK        ANDEQ     '0'
076200000000     C*
076300000000     C* LETTURA VOCE DI C/E
076400000000     C                   READ      ECEVT01L                               99
076500000000     C*
076600000000     C* FINE FILE  O TABELLA DIVERSA
076700000000IF  2C     *IN99         IFEQ      '1'
076800000000     C                   Z-ADD     1             SFLEOF
076900000000X   2C                   ELSE
077000000000     C                   EXSR      CHKREC
077100000000E   2C                   ENDIF
077200000000     C*
077300000000E   1C                   ENDDO
077400000000     C*
077500000000     C                   ENDSR
077600000000     C*------------------------------------------------------------------------*
077700010111     C* CHKREC - CONTROLLA VALIDITA' RECORD VOCE DI C/E
077800000000     C*------------------------------------------------------------------------*
077900000000     C     CHKREC        BEGSR
078000000000     C*
078100000000     C                   MOVEL     '1'           WRECOK
078200000000     C*
078300000000     C* SE HO CHIESTO LE VOCI IN DECORRENZA A UNA CERTA DATA, ESCLUDO QUELLE CON
078400000000     C* DATA RICHIESTA <> ALLA DATA DECORRENZA
078500000000IF  1C     DDVVCC        IFGT      *ZEROS
078600000000IF  2C     DDVVCC        IFNE      EVTDDV
078700000000     C                   MOVEL     '0'           WRECOK
078800000000E   2C                   ENDIF
078900000000E   1C                   ENDIF
079000010111     C*
079100010111     C* VERIFICO SE RICHIESTO LA DATA DI VALIDITA'
079200010111     C     VALVCC        IFGT      *ZEROS
079300010111     C     EVTDDV        IFLE      VALVCC
079400010111     C     EVTDSV        ANDGE     VALVCC
079500010111     C                   ELSE
079600010111     C                   MOVEL     '0'           WRECOK
079700010111     C                   ENDIF
079800010111     C                   ENDIF
079900000000     C*
080000040209     C* VERIFICO LA SELEZIONE PER COMPETENZA
080001040209IF  1C     vcccmp        IFGT      *ZEROS
080002040209IF  2C     vcccmp        IFNE      EVTcmp
080003040209     C                   MOVEL     '0'           WRECOK
080004040209E   2C                   ENDIF
080005040209E   1C                   ENDIF
080600000000     C*
080700000000     C                   ENDSR
080800000000     C*------------------------------------------------------------------------*
080900000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
081000000000     C*------------------------------------------------------------------------*
081100000000     C     IRESFL        BEGSR
081200000000     C*
081300000000IF  1C     EVTATB        IFNE      *BLANKS
081400000000     C                   MOVEL     'A'           VSCATB
081500000000X   1C                   ELSE
081600000000     C                   MOVEL     ' '           VSCATB
081700000000E   1C                   ENDIF
081800100326     C* se voce di isolamento lo segnalo nel sfl
081803100325     c                   clear                   vscsal
081805100422     c                   if        evtvrv = '26'
081806100326     c                   eval      vscsal = 'I'
081813100325     c                   end
081814100325     c*
081900000000     C                   Z-ADD     EVTVOC        VSCVOC
082000000000     C                   Z-ADD     EVTPRG        VSCPRG
082100000000     C                   MOVEL     EVTVOD        VSCVOD
082200000000     C                   Z-ADD     EVTDDV        G02INV
082300000000     C                   Z-ADD     *ZEROS        G02DAT
082400000000     C                   MOVEL     '3'           G02ERR
082500000000     C                   CALL      'XSRDA8'
082600000000     C                   PARM                    WLBDAT
082700000000     C                   Z-ADD     G02DAT        VSCDDV
082800000000     C                   Z-ADD     EVTDSV        G02INV
082900000000     C                   Z-ADD     *ZEROS        G02DAT
083000000000     C                   MOVEL     '3'           G02ERR
083100000000     C                   CALL      'XSRDA8'
083200000000     C                   PARM                    WLBDAT
083300000000     C                   Z-ADD     G02DAT        VSCDSV
083400000000     C                   MOVE      EVTTVO        VSCTVO
083500020614     C                   Z-ADD     EVTCMP        VSCCMP
083600021206     C                   MOVE      EVTCOP        VSCCOP
083700000000     C*
083800000000     C                   ENDSR
083900000000     C*------------------------------------------------------------------------*
084000000000     C* LANPGM - LANCIA UN PROGRAMMA
084100000000     C*------------------------------------------------------------------------*
084200000000     C     LANPGM        BEGSR
084300000000     C*--------------------
084400000000     C* PARAMETRI INFORMATIVI
084500000000     C*--------------------
084600000000     C                   MOVEL     WOP0          D60OP0
084700000000     C                   MOVEL     WOP1          D60OP1
084800000000     C                   MOVEL     '0'           D60F03
084900000000     C                   MOVEL     '0'           D60ERR
085000000000     C                   MOVEL     *BLANKS       D60MSG
085100000000     C*--------------------
085200000000     C* PARAMETRI
085300000000     C*--------------------
085400000000     C                   Z-ADD     WVOC          D60VOC
085500000000     C                   Z-ADD     WPRG          D60PRG
085600000000     C                   Z-ADD     WDDV          D60DDV
085700000000     C                   MOVEL     WTTV          D60TTV
085800000000     C                   Z-ADD     WFT1          D60FT1
085900000000     C                   Z-ADD     WFT2          D60FT2
086000000000     C                   Z-ADD     WCLI          D60CLI
086100000000     C                   Z-ADD     WCTR          D60CTR
086200000000     C                   MOVE      WCTS          D60CTS
086300000000     C                   Z-ADD     WCAP          D60CAP
086400000000     C                   Z-ADD     WDDT          D60DDT
086500000000     C                   Z-ADD     WSGI          D60SGI
086600000000     C                   Z-ADD     WVOM          D60VOM
086700000000     C                   Z-ADD     WPRM          D60PRM
086800000000     C                   Z-ADD     WCMP          D60CMP
086900000000     C                   Z-ADD     WNBC          D60NBC
087000000000     C                   Z-ADD     WDDC          D60DDC
087100000000     C                   Z-ADD     WKCC          D60KCC
087200000000     C                   Z-ADD     WKSC          D60KSC
087300000000     C                   MOVEL     WRIC          D60RIC
087400000000     C                   Z-ADD     WDDO          D60DDO
087500940719     C                   MOVEL     WRIC          D60FRA
087600940725     C                   MOVEL     WIOE          D60IOE
087700940719     C                   Z-ADD     WDDO          D60CCF
087800000000     C* LANCIO
087900000000     C                   MOVEL     DS60          KPJBU
088000000000     C                   CALL      WPGM                                 99
088100000000     C                   PARM                    KPJBA
088200000000     C                   MOVEL     KPJBU         DS60
088300000000     C*
088400000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
088500000000IF  1C     D60F03        IFEQ      '1'
088600000000     C                   MOVEL     '1'           $F03
088700000000     C                   MOVEL     '1'           $ERR
088800000000     C                   MOVEL     '1'           $FINE
088900000000     C*
089000000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
089100000000X   1C                   ELSE
089200000000     C                   MOVEL     D60MSG        $MSG
089300000000     C                   EXSR      CARMSG
089400000000E   1C                   ENDIF
089500000000     C*
089600000000     C                   ENDSR
089700020923     C*-------------------------------------------------------------------------
089800020923     C* REPERISCE I DATI UTENTE
089900020923     C*-------------------------------------------------------------------------
090000020923     C     REPDATIUTE    BEGSR
090100020923     C*
090200020923     C* INIZIALIZZA VARIABILI DI WRK
090300020923     C                   CLEAR                   TIBS34DS
090400020923     C                   CLEAR                   AZUTEDS
090500020923     C                   CLEAR                   DDATIUTE
090600020923     C*
090700020923     C     *DTAARA       DEFINE    §azute        azuteds
090800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
090900020923     C                   IN(E)     *DTAARA
091000020923if  1C                   IF        %Error
091100020923     c                   EVAL      I34Tla = 'L'
091200020923     C                   CALL      'TIBS34R'
091300020923     C                   PARM                    Tibs34Ds
091400020923     C                   IN        *DTAARA
091500020923e   1C                   ENDIF
091600020923     C*
091700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
091800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
091900020923     C*
092000020923     C                   ENDSR
092100000000     C*------------------------------------------------------------------------*
092200000000     C*  *INZSR - ROUTINE INIZIALE
092300000000     C*------------------------------------------------------------------------*
092400000000     C     *INZSR        BEGSR
092500000000     C*
092600000000     C* RICEVIMENTO PARAMETRI
092700000000     C     *ENTRY        PLIST
092800000000     C                   PARM                    KPJBA
092900000000     C                   MOVEL     KPJBU         DS60
093000010111     C*
093100010111     C* CALCOLA LA DATA CORRENTE
093200010111     C                   TIME                    WN14             14 0
093300010111     C                   MOVE      WN14          WN8               8 0
093400010111     C                   Z-ADD     WN8           G02DAT
093500010111     C                   Z-ADD     *ZEROS        G02INV
093600010111     C                   MOVEL     '0'           G02ERR
093700010111     C                   CALL      'XSRDA8'
093800010111     C                   PARM                    WLBDAT
093900010111     C                   Z-ADD     G02INV        DATCOR            8 0
094000000000     C*
094100000000     C* DEFINIZIONE KLIST
094200000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
094300000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
094301100325     C     *LIKE         DEFINE    EVTVOC        Kvocsal
094400000000     C*
094500000000     C* DEFINIZIONE DEPOSITI
094600000000     C     *LIKE         DEFINE    VCCVOC        DEPVOC
094700000000     C     *LIKE         DEFINE    VCCPRG        DEPPRG
094800000000     C     *LIKE         DEFINE    VCCDDV        DEPDDV
094900010111     C     *LIKE         DEFINE    VCCVAL        DEPVAL
094901040209     C     *LIKE         DEFINE    VCCcmp        DEPcmp
095000000000     C*
095100000000     C* DEFINIZIONE CAMPI DI COMODO LANCIO PROGRAMMA
095200000000     C     *LIKE         DEFINE    D60OP0        WOP0
095300000000     C     *LIKE         DEFINE    D60OP1        WOP1
095400000000     C     *LIKE         DEFINE    D60F03        WF03
095500000000     C     *LIKE         DEFINE    D60ERR        WERR
095600000000     C     *LIKE         DEFINE    D60MSG        WMSG
095700000000     C     *LIKE         DEFINE    D60VOC        WVOC
095800000000     C     *LIKE         DEFINE    D60PRG        WPRG
095900000000     C     *LIKE         DEFINE    D60DDV        WDDV
096000000000     C     *LIKE         DEFINE    D60TTV        WTTV
096100000000     C     *LIKE         DEFINE    D60FT1        WFT1
096200000000     C     *LIKE         DEFINE    D60FT2        WFT2
096300000000     C     *LIKE         DEFINE    D60CLI        WCLI
096400000000     C     *LIKE         DEFINE    D60CTR        WCTR
096500000000     C     *LIKE         DEFINE    D60CTS        WCTS
096600000000     C     *LIKE         DEFINE    D60CAP        WCAP
096700000000     C     *LIKE         DEFINE    D60DDT        WDDT
096800000000     C     *LIKE         DEFINE    D60SGI        WSGI
096900000000     C     *LIKE         DEFINE    D60VOM        WVOM
097000000000     C     *LIKE         DEFINE    D60PRM        WPRM
097100000000     C     *LIKE         DEFINE    D60CMP        WCMP
097200000000     C     *LIKE         DEFINE    D60NBC        WNBC
097300000000     C     *LIKE         DEFINE    D60DDC        WDDC
097400000000     C     *LIKE         DEFINE    D60KCC        WKCC
097500000000     C     *LIKE         DEFINE    D60KSC        WKSC
097600000000     C     *LIKE         DEFINE    D60RIC        WRIC
097700000000     C     *LIKE         DEFINE    D60DDO        WDDO
097800940719     C     *LIKE         DEFINE    D60FRA        WFRA
097900940725     C     *LIKE         DEFINE    D60IOE        WIOE
098000940719     C     *LIKE         DEFINE    D60CCF        WCCF
098100000000     C*
098200000000     C* CHIAVE LETTURA ECEVT01L - COMPLETA
098300000000     C     KEYEVT        KLIST
098400000000     C                   KFLD                    KVTVOC
098500000000     C                   KFLD                    KVTPRG
098600000000     C*
098700000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
098800000000     C                   MOVE      D60OP0        INTGES            2
098900000000IF  1C     INTGES        IFEQ      '02'
099000000000     C                   MOVEL     'G'           $TGES             1
099100000000     C                   MOVEL     DESGES        VTDOPE
099200000000X   1C                   ELSE
099300000000     C                   MOVEL     'I'           $TGES             1
099400000000     C                   MOVEL     DESINT        VTDOPE
099500000000E   1C                   ENDIF
099600000000     C*
099700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
099800000000     C                   EXSR      INZSTD
099900000000     C*
100000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
100100000000     C                   EXSR      INZSFL
100200000000     C*
100300000000     C* IMPOSTO CAMPI TESTATA VIDEO
100400000000     C                   MOVEL     DSFIRS        VTDRSU
100500000000     C*
100600000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
100700000000     C                   Z-ADD     *ZEROS        VCCVOC
100800000000     C                   Z-ADD     *ZEROS        VCCPRG
100900000000     C                   Z-ADD     *ZEROS        VCCDDV
101000010111     C                   Z-ADD     *ZEROS        VCCVAL
101001040209     C                   Z-ADD     *ZEROS        VCCcmp
101002040209     c* se ricevuti parametri li imposto
101003040209     c                   if        d60cmp <> 0
101004040209     c                   z-add     d60cmp        vcccmp
101009040209     c                   end
101100000000     C*
101200000000     C* EMETTO VIDEATA SUBFILE
101300000000     C                   EXSR      CARVS
101400000000     C*
101500000000     C                   ENDSR
101600000000     C*------------------------------------------------------------------------*
101700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101900000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
102000000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
102100000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
102200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102400000000     C*------------------------------------------------------------------------*
102500000000     C* INZSTD - ROUTINE INIZIALE STANDARD
102600000000     C*------------------------------------------------------------------------*
102700000000     C     INZSTD        BEGSR
102800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
102900000000     C                   CALL      'XKEYS'
103000000000     C                   PARM                    DSKEYS
103100000000     C                   MOVEA     KEYRIG        KEYS
103200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
103300000000     C     XMSGPA        PLIST
103400000000     C                   PARM                    DSMSMS
103500000000     C                   PARM                    DSMSPG
103600000000     C                   PARM                    DSMSKE
103700000000     C                   PARM                    DSMSER
103800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
103900000000     C                   MOVEL     'TREC60R'     DSMSPG
104000000000     C                   MOVEL     'TREC60R'     VMCQUE
104100000000     C                   MOVEL     *BLANKS       VMCKEY
104200000000     C                   MOVEL     *BLANKS       DSMSMS
104300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
104400000000     C                   MOVEL     '0'           $ERR              1
104500000000     C                   MOVEL     '0'           $F03              1
104600000000     C                   MOVEL     *BLANKS       $GES              2
104700000000     C                   MOVEL     *BLANKS       $MSG             80
104800000000     C                   MOVEL     '0'           $FINE             1
104900000000     C                   MOVEL     '0'           $TESVD            1
105000000000     C* LANCIO ROUTINE PER VERIFICARE SE
105100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
105200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
105300020923     C                   EXSR      REPDATIUTE
105400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
105500000000     C                   BITOFF    '01234567'    POSMI1            1
105600000000     C                   BITOFF    '01234567'    POSMI2            1
105700000000     C                   BITOFF    '01234567'    POSMA1            1
105800000000     C                   BITOFF    '01234567'    POSMA2            1
105900000000     C                   BITON     '34'          POSMI1            1
106000000000     C                   BITON     '7'           POSMI2            1
106100000000     C                   BITON     '34'          POSMA1            1
106200000000     C                   BITON     '0'           POSMA2            1
106300000000     C                   MOVE      POSMI2        POSMIN            2
106400000000     C                   MOVEL     POSMI1        POSMIN
106500000000     C                   MOVE      POSMA2        POSMAX            2
106600000000     C                   MOVEL     POSMA1        POSMAX
106700000000     C                   ENDSR
106800000000     C*------------------------------------------------------------------------*
106900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
107000000000     C*------------------------------------------------------------------------*
107100000000     C     FUNSTD        BEGSR
107200000000     C                   SELECT
107300000000     C     KEY           WHENEQ    F3
107400000000     C                   EXSR      F03STD
107500000000     C     KEY           WHENEQ    F12
107600000000     C                   EXSR      F12STD
107700000000     C     KEY           WHENEQ    F24
107800000000     C                   EXSR      F24STD
107900000000     C                   OTHER
108000000000     C                   MOVEL     MSG(2)        $MSG
108100000000     C                   EXSR      CARMSG
108200000000     C                   ENDSL
108300000000     C                   ENDSR
108400000000     C*------------------------------------------------------------------------*
108500000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
108600000000     C*------------------------------------------------------------------------*
108700000000     C     F03STD        BEGSR
108800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
108900000000     C                   MOVEL     '1'           $F03
109000000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
109100000000     C                   MOVEL     '1'           $FINE
109200000000     C                   ENDSR
109300000000     C*------------------------------------------------------------------------*
109400000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
109500000000     C*------------------------------------------------------------------------*
109600000000     C     F12STD        BEGSR
109700000000     C* IMPOSTO FLAG DI FIN PGM
109800000000     C                   MOVEL     '1'           $FINE
109900000000     C                   ENDSR
110000000000     C*------------------------------------------------------------------------*
110100000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
110200000000     C*------------------------------------------------------------------------*
110300000000     C     F24STD        BEGSR
110400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
110500000000     C                   EXSR      OUTFUN
110600000000     C                   ENDSR
110700000000     C*------------------------------------------------------------------------*
110800000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
110900000000     C*------------------------------------------------------------------------*
111000000000     C     INZSFL        BEGSR
111100000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
111200000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
111300000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
111400000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
111500000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
111600000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
111700000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
111800000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
111900000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
112000000000     C                   Z-ADD     12            SFLPAG
112100000000     C                   ENDSR
112200000000     C*------------------------------------------------------------------------*
112300000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
112400000000     C*------------------------------------------------------------------------*
112500000000     C     FUNSFL        BEGSR
112600000000     C                   SELECT
112700000000     C     KEY           WHENEQ    ENTER
112800000000     C                   EXSR      ENTSFL
112900000000     C     KEY           WHENEQ    F3
113000000000     C                   EXSR      F03STD
113100000000     C     KEY           WHENEQ    F5
113200000000     C                   EXSR      F05STD
113300000000     C     KEY           WHENEQ    F12
113400000000     C                   EXSR      F12STD
113500000000     C     KEY           WHENEQ    F17
113600000000     C                   EXSR      F17STD
113700000000     C     KEY           WHENEQ    F18
113800000000     C                   EXSR      F18STD
113900000000     C     KEY           WHENEQ    F23
114000000000     C                   EXSR      F23STD
114100000000     C     KEY           WHENEQ    F24
114200000000     C                   EXSR      F24STD
114300000000     C     KEY           WHENEQ    ROLLUP
114400000000     C                   EXSR      RUPSTD
114500000000     C     KEY           WHENEQ    ROLLDN
114600000000     C                   EXSR      RDNSTD
114700000000     C                   OTHER
114800000000     C                   MOVEL     MSG(2)        $MSG
114900000000     C                   EXSR      CARMSG
115000000000     C                   ENDSL
115100000000     C                   ENDSR
115200000000     C*------------------------------------------------------------------------*
115300000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
115400000000     C*------------------------------------------------------------------------*
115500000000     C     F05STD        BEGSR
115600000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
115700000000     C                   EXSR      POSSFL
115800000000     C                   ENDSR
115900000000     C*------------------------------------------------------------------------*
116000000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
116100000000     C*------------------------------------------------------------------------*
116200000000     C     F17STD        BEGSR
116300000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
116400000000     C                   Z-ADD     1             VCCNBR
116500000000     C                   ENDSR
116600000000     C*------------------------------------------------------------------------*
116700000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
116800000000     C*------------------------------------------------------------------------*
116900000000     C     F18STD        BEGSR
117000000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
117100000000     C                   Z-ADD     SFLULT        VCCNBR
117200000000     C                   ENDSR
117300000000     C*------------------------------------------------------------------------*
117400000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
117500000000     C*------------------------------------------------------------------------*
117600000000     C     F23STD        BEGSR
117700000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
117800000000     C                   EXSR      OUTOPZ
117900000000     C                   ENDSR
118000000000     C*------------------------------------------------------------------------*
118100000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
118200000000     C*------------------------------------------------------------------------*
118300000000     C     RUPSTD        BEGSR
118400000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
118500000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
118600000000     C     CURSOR        IFGE      POSMIN
118700000000     C     CURSOR        ANDLE     POSMAX
118800000000     C                   ELSE
118900000000     C                   EXSR      CARSFL
119000000000     C                   ENDIF
119100000000     C                   ENDSR
119200000000     C*------------------------------------------------------------------------*
119300000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
119400000000     C*------------------------------------------------------------------------*
119500000000     C     RDNSTD        BEGSR
119600000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
119700000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
119800000000     C     CURSOR        IFGE      POSMIN
119900000000     C     CURSOR        ANDLE     POSMAX
120000000000     C                   ELSE
120100000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
120200000000     C                   MOVEL     '1'           $ERR
120300000000     C                   MOVEL     MSG(5)        $MSG
120400000000     C                   EXSR      CARMSG
120500000000     C                   ENDIF
120600000000     C                   ENDSR
120700000000     C*------------------------------------------------------------------------*
120800000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
120900000000     C*------------------------------------------------------------------------*
121000000000     C     ENTSFL        BEGSR
121100000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
121200000000     C                   EXSR      NEWPOS
121300000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
121400000000     C     $ERR          IFEQ      '0'
121500000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
121600000000     C     SWPOSI        IFNE      0
121700000000     C                   EXSR      POSSFL
121800000000     C                   ELSE
121900000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
122000000000     C     SFLULT        IFNE      0
122100000000     C                   EXSR      OPZSFL
122200000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
122300000000     C     SWMODI        IFNE      0
122400000000     C                   EXSR      POSSFL
122500000000     C                   ENDIF
122600000000     C                   ENDIF
122700000000     C                   ENDIF
122800000000     C                   ENDIF
122900000000     C                   ENDSR
123000000000     C*------------------------------------------------------------------------*
123100000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
123200000000     C*------------------------------------------------------------------------*
123300000000     C     POSSFL        BEGSR
123400000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
123500000000     C                   EXSR      CLRSFL
123600000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
123700000000     C                   EXSR      SETFIL
123800000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
123900000000     C     SFLEOF        IFEQ      0
124000000000     C                   EXSR      CARSFL
124100000000     C                   ENDIF
124200000000     C                   ENDSR
124300000000     C*------------------------------------------------------------------------*
124400000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
124500000000     C*------------------------------------------------------------------------*
124600000000     C     OPZSFL        BEGSR
124700000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
124800000000     C                   MOVEL     'S'           OPZOK             1
124900000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
125000000000     C                   Z-ADD     0             SFLVAR            5 0
125100000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
125200000000     C                   Z-ADD     0             SFLERR
125300000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
125400000000     C                   Z-ADD     SFLULT        WFLULT
125500000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
125600000000     C                   READC     EC60DVS                                49
125700000000     C     *IN49         DOWEQ     '0'
125800941121     C     $ERR          ANDEQ     '0'
125900000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
126000000000     C                   ADD       1             SFLVAR
126100000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
126200000000     C     OPZOK         IFEQ      'S'
126300000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
126400000000     C     VSCOPZ        IFNE      *BLANKS
126500000000     C                   EXSR      CONOPZ
126600000000     C     OPZOK         IFEQ      'S'
126700941121     C     $ERR          ANDEQ     '0'
126800000000     C                   EXSR      GESOPZ
126900000000     C                   ENDIF
127000000000     C                   ENDIF
127100000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
127200000000     C*            2. PULIZIA SCELTA IN SFL
127300000000     C     OPZOK         IFEQ      'S'
127400000000     C                   MOVEL     '0'           *IN41
127500000000     C                   MOVEL     *BLANKS       VSCOPZ
127600000000     C                   ELSE
127700000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
127800000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
127900000000     C                   MOVEL     '1'           *IN41
128000000000     C                   Z-ADD     SFLULT        SFLERR
128100000000     C                   ENDIF
128200000000     C                   ELSE
128300000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
128400000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
128500000000     C                   MOVEL     '1'           *IN41
128600000000     C                   ENDIF
128700000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
128800000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
128900000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
129000000000     C     OPZOK         IFNE      'S'
129100000000     C                   Z-ADD     SFLERR        VCCNBR
129200000000     C                   ELSE
129300000000     C                   Z-ADD     SFLULT        VCCNBR
129400000000     C                   ENDIF
129500000000     C* AGGIORNO RECORD SFL
129600000000     C                   UPDATE    EC60DVS
129700941121     C     $ERR          IFEQ      '0'
129800000000     C                   READC     EC60DVS                                49
129900941121     C                   ENDIF
130000000000     C                   ENDDO
130100000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
130200000000     C                   Z-ADD     WFLULT        SFLULT
130300000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
130400000000     C     SFLVAR        IFEQ      0
130500000000     C                   MOVEL     '1'           $FINE
130600000000     C                   ENDIF
130700000000     C                   ENDSR
130800000000     C*------------------------------------------------------------------------*
130900000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
131000000000     C*------------------------------------------------------------------------*
131100000000     C     CLRSFL        BEGSR
131200000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
131300000000     C* (0=NON FINE FILE  1=FINE FILE)
131400000000     C                   Z-ADD     0             SFLEOF
131500000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
131600000000     C                   Z-ADD     0             SFLULT
131700000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
131800000000     C                   MOVEL     '1'           *IN40
131900000000     C                   WRITE     EC60DVC
132000000000     C                   MOVEL     '0'           *IN40
132100000000     C                   ENDSR
132200000000     C*------------------------------------------------------------------------*
132300000000     C* CARSFL - CARICAMENTO SFL
132400000000     C*------------------------------------------------------------------------*
132500000000     C     CARSFL        BEGSR
132600000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
132700000000     C     SFLULT        IFEQ      0
132800000000     C                   EXSR      SETFIL
132900000000     C                   ENDIF
133000000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
133100000000     C                   Z-ADD     SFLULT        WFLULT
133200000000     C* CICLO:
133300000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
133400000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
133500000000     C                   Z-ADD     1             SFLRIG
133600000000     C     SFLRIG        DOWLE     SFLPAG
133700000000     C     SFLEOF        ANDEQ     0
133800000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
133900000000     C                   MOVEL     *BLANKS       VSCOPZ
134000000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
134100000000     C                   EXSR      IRESFL
134200000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
134300000000     C                   ADD       1             SFLULT
134400000000     C* SCRITTURA RECORD SFL
134500000000     C                   WRITE     EC60DVS
134600000000     C                   ADD       1             SFLRIG
134700000000     C* LETTURA FILE DA CARICARE
134800000000     C                   EXSR      LETFIL
134900000000     C                   ENDDO
135000000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
135100000000     C     WFLULT        IFNE      SFLULT
135200000000     C                   Z-ADD     WFLULT        VCCNBR
135300000000     C                   ADD       1             VCCNBR
135400000000     C                   ENDIF
135500000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
135600000000     C     SFLEOF        IFEQ      1
135700000000     C     SFLULT        ANDGT     *ZEROS
135800000000     C                   MOVEL     '1'           $ERR
135900000000     C                   MOVEL     MSG(6)        $MSG
136000000000     C                   EXSR      CARMSG
136100000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
136200000000     C     SFLULT        CHAIN     EC60DVS                            99
136300000000     C     *IN99         IFEQ      '0'
136400000000     C                   SETON                                        45
136500000000     C                   UPDATE    EC60DVS
136600000000     C                   SETOFF                                       45
136700000000     C                   ENDIF
136800000000     C                   ENDIF
136900000000     C                   ENDSR
137000000000     C*------------------------------------------------------------------------*
137100000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
137200000000     C*------------------------------------------------------------------------*
137300000000     C     OUTSFL        BEGSR
137400000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
137500000000     C     MSGULT        IFNE      *ZEROS
137600000000     C                   EXSR      OUTMSG
137700000000     C                   ENDIF
137800000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
137900000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
138000000000     C     SFLEOF        IFEQ      0
138100000000     C                   MOVEL     '0'           *IN42
138200000000     C                   ELSE
138300000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
138400000000     C                   MOVEL     '1'           *IN42
138500000000     C                   ENDIF
138600000000     C* EMISSIONE SUBFILE
138700000000     C     SFLULT        IFLE      0
138800000000     C*  --> CASO DI SFL VUOTO
138900000000     C                   MOVEL     '0'           *IN43
139000000000     C                   WRITE     EC60DVV
139100000000     C                   WRITE     EC60DVC
139200000000     C                   READ      EC60DVC                                99
139300000000     C                   ELSE
139400000000     C*  --> CASO DI SFL CON RECORDS
139500000000     C                   WRITE     EC60DVC
139600000000     C                   READ      EC60DVC                                99
139700000000     C                   ENDIF
139800000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
139900000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
140000000000     C                   Z-ADD     FRTSFL        VCCNBR
140100000000     C                   ENDSR
140200000000     C*------------------------------------------------------------------------*
140300000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
140400000000     C*          CODA MESSAGGI DEL PGM
140500000000     C*------------------------------------------------------------------------*
140600000000     C     CARMSG        BEGSR
140700000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
140800000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
140900000000     C     MSGULT        IFEQ      0
141000000000     C                   MOVEL     '1'           *IN40
141100000000     C                   WRITE     EC60DVK
141200000000     C                   MOVEL     '0'           *IN40
141300000000     C                   ENDIF
141400000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
141500000000     C                   MOVEL     $MSG          DSMSMS
141600000000     C                   CALL      'XMSG'        XMSGPA
141700000000     C     DSMSER        IFEQ      0
141800000000     C                   MOVEL     DSMSKE        VMCKEY
141900000000     C                   ENDIF
142000000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
142100000000     C                   ADD       1             MSGULT            4 0
142200000000     C                   WRITE     EC60DVM
142300000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
142400000000     C                   MOVEL     *BLANKS       $MSG
142500000000     C                   ENDSR
142600000000     C*------------------------------------------------------------------------*
142700000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
142800000000     C*------------------------------------------------------------------------*
142900000000     C     OUTMSG        BEGSR
143000000000     C* VISUALIZZAZIONE MESSAGGIO
143100000000     C                   MOVEL     '1'           *IN44
143200000000     C                   WRITE     EC60DVK
143300000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
143400000000     C                   Z-ADD     *ZEROS        MSGULT
143500000000     C                   ENDSR
143600000000     C*------------------------------------------------------------------------*
143700000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
143800000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
143900000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
144000000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
144100000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
144200000000     C*------------------------------------------------------------------------*
144300000000     C     CARFUN        BEGSR
144400000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
144500000000     C     1             DO        31            I                 5 0
144600000000     C                   MOVEL     FUN(I)        FUNRIG
144700000000     C                   MOVEL     FUNFUN        FFU(I)
144800000000     C                   MOVEL     FUNDES        FDE(I)
144900020923     C     DUTLPO        IFEQ      'S'
145000000000     C     $TGES         IFEQ      'G'
145100000000     C                   MOVEL     FUNASG        FAT(I)
145200000000     C                   ELSE
145300000000     C                   MOVEL     FUNASI        FAT(I)
145400000000     C                   ENDIF
145500000000     C                   ELSE
145600000000     C     $TGES         IFEQ      'G'
145700000000     C                   MOVEL     FUNAFG        FAT(I)
145800000000     C                   ELSE
145900000000     C                   MOVEL     FUNAFI        FAT(I)
146000000000     C                   ENDIF
146100000000     C                   ENDIF
146200000000     C     FAT(I)        IFEQ      'E'
146300000000     C                   MOVEL     USER          WEDP              3
146400000000     C     WEDP          IFEQ      'EDP'
146500000000     C                   MOVEL     'S'           FAT(I)
146600000000     C                   ELSE
146700000000     C                   MOVEL     'N'           FAT(I)
146800000000     C                   ENDIF
146900000000     C                   ENDIF
147000000000     C                   ENDDO
147100000000     C                   ENDSR
147200000000     C*------------------------------------------------------------------------*
147300000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
147400000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
147500000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
147600000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
147700000000     C*------------------------------------------------------------------------*
147800000000     C     OUTFUN        BEGSR
147900000000     C     1             DO        8             I
148000000000     C                   MOVEL     *BLANKS       FUV(I)
148100000000     C                   ENDDO
148200000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
148300000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
148400000000     C     1             DO        31            I
148500000000     C     FAT(I)        IFEQ      'S'
148600000000     C     FDE(I)        ANDNE     *BLANKS
148700000000     C                   ADD       1             FUNNAT
148800000000     C                   ENDIF
148900000000     C                   ENDDO
149000000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
149100000000     C     FUNNAT        IFGT      8
149200000000     C                   MOVEL     'S'           FAT(24)
149300000000     C                   ENDIF
149400000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
149500000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
149600000000     C                   Z-ADD     8             FUNVIS            5 0
149700000000     C     FAT(23)       IFEQ      'S'
149800000000     C                   SUB       1             FUNVIS
149900000000     C                   ENDIF
150000000000     C     FAT(24)       IFEQ      'S'
150100000000     C                   SUB       1             FUNVIS
150200000000     C                   ENDIF
150300000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
150400000000     C                   Z-ADD     *ZEROS        W                 5 0
150500000000     C                   ADD       1             FUNULT
150600000000     C     FUNULT        IFGT      31
150700000000     C                   Z-ADD     1             FUNULT
150800000000     C                   ENDIF
150900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
151000000000     C     FUNULT        DO        31            I
151100000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
151200000000     C     W             IFLT      FUNVIS
151300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
151400000000     C     FAT(I)        IFEQ      'S'
151500000000     C     FDE(I)        ANDNE     *BLANKS
151600000000     C     I             IFEQ      23
151700000000     C     I             OREQ      24
151800000000     C                   ELSE
151900000000     C                   ADD       1             W
152000000000     C                   MOVEL     FDE(I)        FUV(W)
152100000000     C                   ENDIF
152200000000     C                   ENDIF
152300000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
152400000000     C                   Z-ADD     I             FUNULT
152500000000     C                   ENDIF
152600000000     C                   ENDDO
152700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
152800000000     C                   MOVEL     FUV(1)        VFD001
152900000000     C                   MOVEL     FUV(2)        VFD002
153000000000     C                   MOVEL     FUV(3)        VFD003
153100000000     C                   MOVEL     FUV(4)        VFD004
153200000000     C                   MOVEL     FUV(5)        VFD005
153300000000     C                   MOVEL     FUV(6)        VFD006
153400000000     C     FAT(23)       IFEQ      'N'
153500000000     C     FAT(24)       ANDEQ     'N'
153600000000     C                   MOVEL     FUV(7)        VFD007
153700000000     C                   MOVEL     FUV(8)        VFD008
153800000000     C                   ELSE
153900000000     C     FAT(23)       IFEQ      'S'
154000000000     C     FAT(24)       ANDEQ     'S'
154100000000     C                   MOVEL     FDE(23)       VFD007
154200000000     C                   MOVEL     FDE(24)       VFD008
154300000000     C                   ELSE
154400000000     C                   MOVEL     FUV(7)        VFD007
154500000000     C     FAT(23)       IFEQ      'S'
154600000000     C                   MOVEL     FDE(23)       VFD008
154700000000     C                   ELSE
154800000000     C                   MOVEL     FDE(24)       VFD008
154900000000     C                   ENDIF
155000000000     C                   ENDIF
155100000000     C                   ENDIF
155200000000     C                   MOVEL     '1'           $TESVD
155300000000     C                   ENDSR
155400000000     C*------------------------------------------------------------------------*
155500000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
155600000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
155700000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
155800000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
155900000000     C*------------------------------------------------------------------------*
156000000000     C     CONFUN        BEGSR
156100000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
156200000000     C     1             DO        31            I
156300000000     C     KEY           IFEQ      KEYS(I)
156400000000     C* CONTROLLO SE IL TASTO E' ATTIVO
156500000000     C     FAT(I)        IFNE      'S'
156600000000     C                   MOVEL     MSG(1)        $MSG
156700000000     C                   EXSR      CARMSG
156800000000     C                   MOVEL     '1'           $ERR
156900000000     C                   ENDIF
157000000000     C                   ENDIF
157100000000     C                   ENDDO
157200000000     C                   ENDSR
157300000000     C*------------------------------------------------------------------------*
157400000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
157500000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
157600000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
157700000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
157800000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
157900000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
158000000000     C*------------------------------------------------------------------------*
158100000000     C     CAROPZ        BEGSR
158200000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
158300000000     C     1             DO        24            I
158400000000     C                   MOVEL     OPZ(I)        OPZRIG
158500000000     C                   MOVEL     OPZOPD        OPD(I)
158600000000     C                   MOVEL     OPZOPS        OPS(I)
158700000000     C                   MOVEL     OPZDES        ODE(I)
158800020923     C     DUTLPO        IFEQ      'S'
158900000000     C     $TGES         IFEQ      'G'
159000000000     C                   MOVEL     OPZASG        OAT(I)
159100000000     C                   ELSE
159200000000     C                   MOVEL     OPZASI        OAT(I)
159300000000     C                   ENDIF
159400000000     C                   ELSE
159500000000     C     $TGES         IFEQ      'G'
159600000000     C                   MOVEL     OPZAFG        OAT(I)
159700000000     C                   ELSE
159800000000     C                   MOVEL     OPZAFI        OAT(I)
159900000000     C                   ENDIF
160000000000     C                   ENDIF
160100000000     C     OAT(I)        IFEQ      'E'
160200000000     C                   MOVEL     USER          WEDP              3
160300000000     C     WEDP          IFEQ      'EDP'
160400000000     C                   MOVEL     'S'           OAT(I)
160500000000     C                   ELSE
160600000000     C                   MOVEL     'N'           OAT(I)
160700000000     C                   ENDIF
160800000000     C                   ENDIF
160900000000     C                   ENDDO
161000000000     C                   ENDSR
161100000000     C*------------------------------------------------------------------------*
161200000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
161300000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
161400000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
161500000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
161600000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
161700000000     C*------------------------------------------------------------------------*
161800000000     C     OUTOPZ        BEGSR
161900000000     C     1             DO        8             I
162000000000     C                   MOVEL     *BLANKS       OPV(I)
162100000000     C                   ENDDO
162200000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
162300000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
162400000000     C                   ADD       1             OPZULT
162500000000     C     OPZULT        IFGT      24
162600000000     C                   Z-ADD     1             OPZULT
162700000000     C                   END
162800000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
162900000000     C     1             DO        24            I
163000000000     C     OAT(I)        IFEQ      'S'
163100000000     C     ODE(I)        ANDNE     *BLANKS
163200000000     C                   ADD       1             OPZNAT
163300000000     C                   ENDIF
163400000000     C                   ENDDO
163500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
163600000000     C                   Z-ADD     0             W
163700000000     C     OPZULT        DO        24            I
163800000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
163900000000     C     W             IFLT      8
164000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
164100000000     C     OAT(I)        IFEQ      'S'
164200000000     C     ODE(I)        ANDNE     *BLANKS
164300000000     C                   ADD       1             W
164400000000     C                   MOVEL     ODE(I)        OPV(W)
164500000000     C                   ENDIF
164600000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
164700000000     C                   Z-ADD     I             OPZULT
164800000000     C                   ENDIF
164900000000     C                   ENDDO
165000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
165100000000     C                   MOVEL     OPV(1)        VOD001
165200000000     C                   MOVEL     OPV(2)        VOD002
165300000000     C                   MOVEL     OPV(3)        VOD003
165400000000     C                   MOVEL     OPV(4)        VOD004
165500000000     C                   MOVEL     OPV(5)        VOD005
165600000000     C                   MOVEL     OPV(6)        VOD006
165700000000     C                   MOVEL     OPV(7)        VOD007
165800000000     C                   MOVEL     OPV(8)        VOD008
165900000000     C                   MOVEL     '1'           $TESVD
166000000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
166100000000     C     OPZNAT        IFGT      8
166200000000     C                   MOVEL     'S'           FAT(23)
166300000000     C                   ENDIF
166400000000     C                   ENDSR
166500000000     C*------------------------------------------------------------------------*
166600000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
166700000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
166800000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
166900000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
167000000000     C*------------------------------------------------------------------------*
167100000000     C     CONOPZ        BEGSR
167200000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
167300000000     C                   MOVEL     *BLANKS       OPZOK             1
167400000000     C     1             DO        24            I                 5 0
167500000000     C     VSCOPZ        IFEQ      OPD(I)
167600000000     C     VSCOPZ        OREQ      OPS(I)
167700000000     C     OAT(I)        IFEQ      'S'
167800000000     C                   MOVEL     OPD(I)        VSCOPZ
167900000000     C                   MOVEL     'S'           OPZOK
168000000000     C                   ELSE
168100000000     C                   MOVEL     'N'           OPZOK             1
168200000000     C                   ENDIF
168300000000     C                   ENDIF
168400000000     C                   ENDDO
168500000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
168600000000     C     OPZOK         IFNE      'S'
168700000000     C                   MOVEL     MSG(3)        $MSG
168800000000     C                   EXSR      CARMSG
168900000000     C                   MOVEL     '1'           $ERR
169000000000     C                   MOVEL     'N'           OPZOK             1
169100000000     C                   ENDIF
169200000000     C                   ENDSR
169300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
169400000000TREC60R- Tasto funzionale non valido.                                          1
169500000000TREC60R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
169600000000TREC60R- Opzione non valida.                                                   3
169700000000TREC60R- Opzione abilitata ma non gestita. Avvertire CED.                      4
169800000000TREC60R- Inizio elenco.                                                        5
169900000000TREC60R- Fine elenco.                                                          6
170000000000TREC60R- 7
170100010111TREC60R- Data di riferimento errata.                                           8
170200000000TREC60R- Data decorrenza voce errata                                           9
170300010111TREC60R- Data di validità non valida                                          10
170400010115TREC60R- Duplica effettuata ma verificare tariffe per eventuali incongruenze! 11
170500000000TREC60R- Codice voce annullato, consentito solo il ripristino                 12
170600000000TREC60R- Codice voce non annullato, impossibile ripristinare                  13
170700000000TREC60R- La gestione e' consentita solo per voci di tipo 'M'=Maggiorazione    14
170800000000TREC60R- 15
170900000000TREC60R- 16
171000000000TREC60R- 17
171100000000TREC60R- 18
171200000000TREC60R- 19
171300000000TREC60R- 20
171400000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
171500000000- 1-1 -                   -    NN     NN
171600000000- 2-2 -2=Modifica         -    SN     NN
171700010110- 3-3 -3=Duplica          -    SN     NN
171800000000- 4-4 -4=Cancellazione    -    SN     NN
171900000000- 5-5 -5=Visualizzazione  -    SS     SS
172000000000- 6-6 -                   -    NN     NN
172100000000- 7-7 -7=Ripristino       -    SN     NN
172200000000- 8-8 -8=Tariffe          -    SS     SS
172300000000- 9-9 -9=Competenze       -    SS     SS
172400000000-10-10-10=Maggiorazioni   -    SS     SS
172500000000-11-  -                   -    NN     NN
172600000000-12-  -                   -    NN     NN
172700000000-13-  -                   -    NN     NN
172800000000-14-  -                   -    NN     NN
172900000000-15-  -                   -    NN     NN
173000000000-16-  -                   -    NN     NN
173100000000-17-  -                   -    NN     NN
173200000000-18-  -                   -    NN     NN
173300000000-19-  -                   -    NN     NN
173400000000-20-  -                   -    NN     NN
173500000000-21-  -                   -    NN     NN
173600000000-22-  -                   -    NN     NN
173700000000-23-  -                   -    NN     NN
173800000000-24-  -                   -    NN     NN
173900000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
174000000000F1    -                   -    NN     NN
174100000000F2    -                   -    NN     NN
174200000000F3    -F3=Fine            -    SS     SS
174300000000F4    -                   -    NN     NN
174400000000F5    -F5=Rivisualizza    -    SS     SS
174500000000F6    -F6=Inserimento     -    SN     NN
174600000000F7    -                   -    NN     NN
174700000000F8    -                   -    NN     NN
174800000000F9    -                   -    NN     NN
174900000000F10   -                   -    NN     NN
175000000000F11   -                   -    NN     NN
175100000000F12   -F12=Ritorno        -    SS     SS
175200000000F13   -                   -    NN     NN
175300000000F14   -                   -    NN     NN
175400000000F15   -                   -    NN     NN
175500000000F16   -                   -    NN     NN
175600961003F17   -F17=Inizio dati    -    NN     NN
175700961003F18   -F18=Fine dati      -    NN     NN
175800000000F19   -                   -    NN     NN
175900000000F20   -                   -    NN     NN
176000000000F21   -                   -    NN     NN
176100000000F22   -                   -    NN     NN
176200000000F23   -F23=Altre opzioni  -    NN     NN
176300000000F24   -F24=Altri tasti    -    NN     NN
176400000000ENTER -                   -    SS     SS
176500000000ROLLUP-                   -    SS     SS
176600000000ROLLDN-                   -    SS     SS
176700000000HELP  -                   -    NN     NN
176800000000PRINT -                   -    NN     NN
176900000000BSPACE-                   -    NN     NN
177000000000CLEAR -                   -    NN     NN

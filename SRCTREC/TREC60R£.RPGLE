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
003400130919     D FUV             S             19    DIM(8)
003500130919     D OPV             S             19    DIM(8)
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
005701130919     D Trec69DS      E DS
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
008901130919     d  savkpjbu       s                   like(kpjbu)
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
022601130919     C     VSCOPZ        WHENEQ    '17'
022602130919     C                   EXSR      O17GES
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
059600000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
059700000000IF  1C     $ERR          IFEQ      '0'
059800091021     C                   Z-ADD     *ZEROS        WVOM
059900091021     C                   Z-ADD     *ZERO         WPRM
060000091021     C                   Z-ADD     *ZEROS        WVOC
060100091021     C                   Z-ADD     *ZERO         WPRG
060200000000     C                   Z-ADD     *ZEROS        WDDV
060300000000     C                   MOVEL     *BLANKS       WTTV
060400000000     C                   Z-ADD     *ZEROS        WFT1
060500000000     C                   Z-ADD     *ZEROS        WFT2
060600000000     C                   Z-ADD     *ZEROS        WCLI
060700000000     C                   Z-ADD     *ZERO         WCTR
060800000000     C                   MOVE      *BLANKS       WCTS
060900000000     C                   Z-ADD     *ZERO         WCAP
061000000000     C                   Z-ADD     *ZEROS        WDDT
061100000000     C                   Z-ADD     *ZEROS        WSGI
061200000000     C                   Z-ADD     *ZERO         WCMP
061300000000     C                   Z-ADD     *ZERO         WNBC
061400000000     C                   Z-ADD     *ZEROS        WDDC
061500000000     C                   Z-ADD     *ZEROS        WKCC
061600000000     C                   Z-ADD     *ZEROS        WKSC
061700000000     C                   MOVEL     *BLANKS       WRIC
061800000000     C                   Z-ADD     *ZEROS        WDDO
061900940719     C                   MOVEL     *BLANKS       WFRA
062000940725     C                   MOVEL     *BLANKS       WIOE
062100940719     C                   Z-ADD     *ZEROS        WCCF
062200091021     C* maggiorazioni
062300091021     C     VSCTVO        IFEQ      'M'
062400091021     C                   Z-ADD     VSCVOC        WVOM
062500091021     C                   Z-ADD     VSCPRG        WPRM
062600091021     c                   else
062700091021     C                   Z-ADD     VSCVOC        WVOC
062800091021     C                   Z-ADD     VSCPRG        WPRG
062900091021     c                   end
063000000000     C                   MOVEL     D60OP0        WOP0
063100000000     C                   MOVEL     'O10'         WOP1
063200091021     C     VSCTVO        IFEQ      'M'
063300000000     C                   MOVEL     'TREC66R'     WPGM              8
063400091021     c                   else
063500091021     C                   MOVEL     'TREC68R'     WPGM              8
063600091021     c                   end
063700000000     C                   EXSR      LANPGM
063800000000     C                   ENDIF
063900000000     C*
064000000000     C                   ENDSR
064001130919     C*------------------------------------------------------------------------*
064002130919     C*  O17GES - GESTIONE OPZIONE 17=GESTIONE note conte economico
064003130919     C*------------------------------------------------------------------------*
064004130919     C     O17GES        BEGSR
064005130919     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
064006130919     c                   movel(p)  kpjbu         savkpjbu
064007130919     C                   MOVEL     'V'           d69tpana
064033130919     C                   Z-ADD     VSCVOC        d69voc
064034130919     C                   Z-ADD     VSCPRG        d69prg
064038130919     c                   movel(p)  trec69ds      kpjbu
064042130919     C                   call      'TREC69R'
064043130919     c                   parm                    kpjba
064044130919     c                   movel(p)  savkpjbu      kpjbu
064049130919     C*
064050130919     C                   ENDSR
064100000000     C*------------------------------------------------------------------------*
064200000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
064300000000     C*------------------------------------------------------------------------*
064400000000     C     NEWPOS        BEGSR
064500000000     C*
064600000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
064700000000     C*        1 = RICHIESTO POSIZIONAMENTO
064800000000     C                   Z-ADD     0             SWPOSI            7 0
064900000000     C*
065000000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
065100000000     C*            NON OCCORRE RICARICARE IL SUBFILE
065200000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
065300000000     C                   Z-ADD     0             SWMODI            1 0
065400000000     C*
065500000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
065600000000IF  1C     VCCVOC        IFNE      DEPVOC
065700000000     C     VCCPRG        ORNE      DEPPRG
065800000000     C     VCCDDV        ORNE      DEPDDV
065900010111     C     VCCVAL        ORNE      DEPVAL
066000040209     C     VCCcmp        ORNE      DEPcmp
066100130910     C     VCCvod        ORNE      DEPvod
066200000000     C*
066300000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
066400000000     C                   EXSR      CHKPAR
066500000000     C*
066600000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
066700000000IF  2C     $ERR          IFEQ      '0'
066800000000     C                   Z-ADD     1             SWPOSI
066900000000     C                   Z-ADD     VCCVOC        DEPVOC
067000000000     C                   Z-ADD     VCCPRG        DEPPRG
067100000000     C                   Z-ADD     VCCDDV        DEPDDV
067200010111     C                   Z-ADD     VCCVAL        DEPVAL
067300040209     C                   Z-ADD     VCCcmp        DEPcmp
067400130910     C                   movel     VCCvod        DEPvod
067500000000E   2C                   ENDIF
067600000000E   1C                   ENDIF
067700000000     C*
067800000000     C                   ENDSR
067900000000     C*------------------------------------------------------------------------*
068000000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
068100000000     C*------------------------------------------------------------------------*
068200000000     C     CHKPAR        BEGSR
068300000000     C*
068400000000     C* REIMPOSTO INDICATORI DI ERRORE
068500010111     C                   SETOFF                                       808183
068600000000     C*--------------------
068700000000     C* DATA DECORRENZA VOCE
068800000000     C*--------------------
068900000000     C                   Z-ADD     *ZERO         DDVVCC
069000000000IF  1C     VCCDDV        IFGT      *ZEROS
069100000000     C                   Z-ADD     VCCDDV        G02DAT
069200000000     C                   Z-ADD     *ZEROS        G02INV
069300000000     C                   MOVEL     '0'           G02ERR
069400000000     C                   CALL      'XSRDA8'
069500000000     C                   PARM                    WLBDAT
069600000000     C*
069700000000IF  2C     G02ERR        IFEQ      '1'
069800000000     C                   MOVEL     '1'           $ERR
069900000000     C                   MOVEL     MSG(9)        $MSG
070000000000     C                   EXSR      CARMSG
070100000000     C                   SETON                                        81
070200000000X   2C                   ELSE
070300000000     C                   Z-ADD     G02DAT        VCCDDV
070400000000     C                   Z-ADD     G02INV        DDVVCC            8 0
070500000000E   2C                   ENDIF
070600000000E   1C                   ENDIF
070700010111     C*--------------------
070800010111     C* VALIDITA' AL
070900010111     C*--------------------
071000010111     C                   Z-ADD     *ZERO         VALVCC            8 0
071100010111IF  1C     VCCVAL        IFGT      *ZEROS
071200010111     C                   Z-ADD     VCCVAL        G02DAT
071300010111     C                   Z-ADD     *ZEROS        G02INV
071400010111     C                   MOVEL     '0'           G02ERR
071500010111     C                   CALL      'XSRDA8'
071600010111     C                   PARM                    WLBDAT
071700010111     C*
071800010111IF  2C     G02ERR        IFEQ      '1'
071900010111     C                   MOVEL     '1'           $ERR
072000010111     C                   MOVEL     MSG(10)       $MSG
072100010111     C                   EXSR      CARMSG
072200010111     C                   SETON                                        83
072300010111X   2C                   ELSE
072400010111     C                   Z-ADD     G02DAT        VCCVAL
072500010111     C                   Z-ADD     G02INV        VALVCC            8 0
072600010111E   2C                   ENDIF
072700010111E   1C                   ENDIF
072800000000     C*
072900000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
073000000000     C* SUL CAMPO DEL CONTROL
073100000000IF  1C     $ERR          IFEQ      '1'
073200000000     C                   MOVEL     '0'           *IN43
073300000000X   1C                   ELSE
073400000000     C                   MOVEL     '1'           *IN43
073500000000E   1C                   ENDIF
073600000000     C*
073700000000     C                   ENDSR
073800000000     C*------------------------------------------------------------------------*
073900000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD VOCE DI C/E
074000000000     C*------------------------------------------------------------------------*
074100000000     C     SETFIL        BEGSR
074200000000     C*
074300000000     C                   MOVEL     '1'           *IN43
074400000000     C*
074500000000     C* POSIZIONAMENTO ARCHIVIO VOCI DI C/E
074600000000     C                   Z-ADD     VCCVOC        KVTVOC
074700000000     C                   Z-ADD     VCCPRG        KVTPRG
074800000000     C     KEYEVT        SETLL     ECEVT01L                           99
074900000000     C*
075000000000     C  N99              EXSR      LETFIL
075100000000     C*
075200000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
075300000000IF  1C     *IN99         IFEQ      '1'
075400000000     C                   Z-ADD     1             SFLEOF
075500000000E   1C                   ENDIF
075600000000     C*
075700000000     C                   ENDSR
075800000000     C*------------------------------------------------------------------------*
075900000000     C* LETFIL - LEGGE RECORD SUCESSIVO VOCE DI C/E
076000000000     C*------------------------------------------------------------------------*
076100000000     C     LETFIL        BEGSR
076200000000     C*
076300000000     C* LEGGO FINO A:
076400000000     C*     - FINE FILE
076500000000     C*     - TROVATO RECORD VALIDO
076600000000     C                   MOVEL     '0'           WRECOK            1
076700000000DO  1C     *IN99         DOWEQ     *OFF
076800000000     C     WRECOK        ANDEQ     '0'
076900000000     C*
077000000000     C* LETTURA VOCE DI C/E
077100000000     C                   READ      ECEVT01L                               99
077200000000     C*
077300000000     C* FINE FILE  O TABELLA DIVERSA
077400000000IF  2C     *IN99         IFEQ      '1'
077500000000     C                   Z-ADD     1             SFLEOF
077600000000X   2C                   ELSE
077700000000     C                   EXSR      CHKREC
077800000000E   2C                   ENDIF
077900000000     C*
078000000000E   1C                   ENDDO
078100000000     C*
078200000000     C                   ENDSR
078300000000     C*------------------------------------------------------------------------*
078400010111     C* CHKREC - CONTROLLA VALIDITA' RECORD VOCE DI C/E
078500000000     C*------------------------------------------------------------------------*
078600000000     C     CHKREC        BEGSR
078700000000     C*
078800000000     C                   MOVEL     '1'           WRECOK
078900000000     C*
079000000000     C* SE HO CHIESTO LE VOCI IN DECORRENZA A UNA CERTA DATA, ESCLUDO QUELLE CON
079100000000     C* DATA RICHIESTA <> ALLA DATA DECORRENZA
079200000000IF  1C     DDVVCC        IFGT      *ZEROS
079300000000IF  2C     DDVVCC        IFNE      EVTDDV
079400000000     C                   MOVEL     '0'           WRECOK
079500000000E   2C                   ENDIF
079600000000E   1C                   ENDIF
079700010111     C*
079800010111     C* VERIFICO SE RICHIESTO LA DATA DI VALIDITA'
079900010111     C     VALVCC        IFGT      *ZEROS
080000010111     C     EVTDDV        IFLE      VALVCC
080100010111     C     EVTDSV        ANDGE     VALVCC
080200010111     C                   ELSE
080300010111     C                   MOVEL     '0'           WRECOK
080400010111     C                   ENDIF
080500010111     C                   ENDIF
080600130910     C* VERIFICO SE RICHIESTa LA descrizione
080700130910     c                   if        vccvod <> ' '
080800130910     c                   z-add     0             pos               2 0
080900130910     C                   eval      pos = %scan(%trim(vccvod): evtvod)
081000130910     c                   if        pos =  0
081100130910     C                   MOVEL     '0'           WRECOK
081200130910     C                   ENDIF
081300130910     C                   ENDIF
081400000000     C*
081500040209     C* VERIFICO LA SELEZIONE PER COMPETENZA
081600040209IF  1C     vcccmp        IFGT      *ZEROS
081700040209IF  2C     vcccmp        IFNE      EVTcmp
081800040209     C                   MOVEL     '0'           WRECOK
081900040209E   2C                   ENDIF
082000040209E   1C                   ENDIF
082100000000     C*
082200000000     C                   ENDSR
082300000000     C*------------------------------------------------------------------------*
082400000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
082500000000     C*------------------------------------------------------------------------*
082600000000     C     IRESFL        BEGSR
082700000000     C*
082800000000IF  1C     EVTATB        IFNE      *BLANKS
082900000000     C                   MOVEL     'A'           VSCATB
083000000000X   1C                   ELSE
083100000000     C                   MOVEL     ' '           VSCATB
083200000000E   1C                   ENDIF
083300100326     C* se voce di isolamento lo segnalo nel sfl
083400100325     c                   clear                   vscsal
083500100422     c                   if        evtvrv = '26'
083600100326     c                   eval      vscsal = 'I'
083700100325     c                   end
083800100325     c*
083900000000     C                   Z-ADD     EVTVOC        VSCVOC
084000000000     C                   Z-ADD     EVTPRG        VSCPRG
084100000000     C                   MOVEL     EVTVOD        VSCVOD
084200000000     C                   Z-ADD     EVTDDV        G02INV
084300000000     C                   Z-ADD     *ZEROS        G02DAT
084400000000     C                   MOVEL     '3'           G02ERR
084500000000     C                   CALL      'XSRDA8'
084600000000     C                   PARM                    WLBDAT
084700000000     C                   Z-ADD     G02DAT        VSCDDV
084800000000     C                   Z-ADD     EVTDSV        G02INV
084900000000     C                   Z-ADD     *ZEROS        G02DAT
085000000000     C                   MOVEL     '3'           G02ERR
085100000000     C                   CALL      'XSRDA8'
085200000000     C                   PARM                    WLBDAT
085300000000     C                   Z-ADD     G02DAT        VSCDSV
085400000000     C                   MOVE      EVTTVO        VSCTVO
085500020614     C                   Z-ADD     EVTCMP        VSCCMP
085600021206     C                   MOVE      EVTCOP        VSCCOP
085700000000     C*
085800000000     C                   ENDSR
085900000000     C*------------------------------------------------------------------------*
086000000000     C* LANPGM - LANCIA UN PROGRAMMA
086100000000     C*------------------------------------------------------------------------*
086200000000     C     LANPGM        BEGSR
086300000000     C*--------------------
086400000000     C* PARAMETRI INFORMATIVI
086500000000     C*--------------------
086600000000     C                   MOVEL     WOP0          D60OP0
086700000000     C                   MOVEL     WOP1          D60OP1
086800000000     C                   MOVEL     '0'           D60F03
086900000000     C                   MOVEL     '0'           D60ERR
087000000000     C                   MOVEL     *BLANKS       D60MSG
087100000000     C*--------------------
087200000000     C* PARAMETRI
087300000000     C*--------------------
087400000000     C                   Z-ADD     WVOC          D60VOC
087500000000     C                   Z-ADD     WPRG          D60PRG
087600000000     C                   Z-ADD     WDDV          D60DDV
087700000000     C                   MOVEL     WTTV          D60TTV
087800000000     C                   Z-ADD     WFT1          D60FT1
087900000000     C                   Z-ADD     WFT2          D60FT2
088000000000     C                   Z-ADD     WCLI          D60CLI
088100000000     C                   Z-ADD     WCTR          D60CTR
088200000000     C                   MOVE      WCTS          D60CTS
088300000000     C                   Z-ADD     WCAP          D60CAP
088400000000     C                   Z-ADD     WDDT          D60DDT
088500000000     C                   Z-ADD     WSGI          D60SGI
088600000000     C                   Z-ADD     WVOM          D60VOM
088700000000     C                   Z-ADD     WPRM          D60PRM
088800000000     C                   Z-ADD     WCMP          D60CMP
088900000000     C                   Z-ADD     WNBC          D60NBC
089000000000     C                   Z-ADD     WDDC          D60DDC
089100000000     C                   Z-ADD     WKCC          D60KCC
089200000000     C                   Z-ADD     WKSC          D60KSC
089300000000     C                   MOVEL     WRIC          D60RIC
089400000000     C                   Z-ADD     WDDO          D60DDO
089500940719     C                   MOVEL     WRIC          D60FRA
089600940725     C                   MOVEL     WIOE          D60IOE
089700940719     C                   Z-ADD     WDDO          D60CCF
089800000000     C* LANCIO
089900000000     C                   MOVEL     DS60          KPJBU
090000000000     C                   CALL      WPGM                                 99
090100000000     C                   PARM                    KPJBA
090200000000     C                   MOVEL     KPJBU         DS60
090300000000     C*
090400000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
090500000000IF  1C     D60F03        IFEQ      '1'
090600000000     C                   MOVEL     '1'           $F03
090700000000     C                   MOVEL     '1'           $ERR
090800000000     C                   MOVEL     '1'           $FINE
090900000000     C*
091000000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
091100000000X   1C                   ELSE
091200000000     C                   MOVEL     D60MSG        $MSG
091300000000     C                   EXSR      CARMSG
091400000000E   1C                   ENDIF
091500000000     C*
091600000000     C                   ENDSR
091700020923     C*-------------------------------------------------------------------------
091800020923     C* REPERISCE I DATI UTENTE
091900020923     C*-------------------------------------------------------------------------
092000020923     C     REPDATIUTE    BEGSR
092100020923     C*
092200020923     C* INIZIALIZZA VARIABILI DI WRK
092300020923     C                   CLEAR                   TIBS34DS
092400020923     C                   CLEAR                   AZUTEDS
092500020923     C                   CLEAR                   DDATIUTE
092600020923     C*
092700020923     C     *DTAARA       DEFINE    §azute        azuteds
092800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
092900020923     C                   IN(E)     *DTAARA
093000020923if  1C                   IF        %Error
093100020923     c                   EVAL      I34Tla = 'L'
093200020923     C                   CALL      'TIBS34R'
093300020923     C                   PARM                    Tibs34Ds
093400020923     C                   IN        *DTAARA
093500020923e   1C                   ENDIF
093600020923     C*
093700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
093800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
093900020923     C*
094000020923     C                   ENDSR
094100000000     C*------------------------------------------------------------------------*
094200000000     C*  *INZSR - ROUTINE INIZIALE
094300000000     C*------------------------------------------------------------------------*
094400000000     C     *INZSR        BEGSR
094500000000     C*
094600000000     C* RICEVIMENTO PARAMETRI
094700000000     C     *ENTRY        PLIST
094800000000     C                   PARM                    KPJBA
094900000000     C                   MOVEL     KPJBU         DS60
095000010111     C*
095100010111     C* CALCOLA LA DATA CORRENTE
095200010111     C                   TIME                    WN14             14 0
095300010111     C                   MOVE      WN14          WN8               8 0
095400010111     C                   Z-ADD     WN8           G02DAT
095500010111     C                   Z-ADD     *ZEROS        G02INV
095600010111     C                   MOVEL     '0'           G02ERR
095700010111     C                   CALL      'XSRDA8'
095800010111     C                   PARM                    WLBDAT
095900010111     C                   Z-ADD     G02INV        DATCOR            8 0
096000000000     C*
096100000000     C* DEFINIZIONE KLIST
096200000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
096300000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
096400100325     C     *LIKE         DEFINE    EVTVOC        Kvocsal
096500000000     C*
096600000000     C* DEFINIZIONE DEPOSITI
096700000000     C     *LIKE         DEFINE    VCCVOC        DEPVOC
096800000000     C     *LIKE         DEFINE    VCCPRG        DEPPRG
096900000000     C     *LIKE         DEFINE    VCCDDV        DEPDDV
097000010111     C     *LIKE         DEFINE    VCCVAL        DEPVAL
097100040209     C     *LIKE         DEFINE    VCCcmp        DEPcmp
097200130910     C     *LIKE         DEFINE    VCCvod        DEPvod
097300000000     C*
097400000000     C* DEFINIZIONE CAMPI DI COMODO LANCIO PROGRAMMA
097500000000     C     *LIKE         DEFINE    D60OP0        WOP0
097600000000     C     *LIKE         DEFINE    D60OP1        WOP1
097700000000     C     *LIKE         DEFINE    D60F03        WF03
097800000000     C     *LIKE         DEFINE    D60ERR        WERR
097900000000     C     *LIKE         DEFINE    D60MSG        WMSG
098000000000     C     *LIKE         DEFINE    D60VOC        WVOC
098100000000     C     *LIKE         DEFINE    D60PRG        WPRG
098200000000     C     *LIKE         DEFINE    D60DDV        WDDV
098300000000     C     *LIKE         DEFINE    D60TTV        WTTV
098400000000     C     *LIKE         DEFINE    D60FT1        WFT1
098500000000     C     *LIKE         DEFINE    D60FT2        WFT2
098600000000     C     *LIKE         DEFINE    D60CLI        WCLI
098700000000     C     *LIKE         DEFINE    D60CTR        WCTR
098800000000     C     *LIKE         DEFINE    D60CTS        WCTS
098900000000     C     *LIKE         DEFINE    D60CAP        WCAP
099000000000     C     *LIKE         DEFINE    D60DDT        WDDT
099100000000     C     *LIKE         DEFINE    D60SGI        WSGI
099200000000     C     *LIKE         DEFINE    D60VOM        WVOM
099300000000     C     *LIKE         DEFINE    D60PRM        WPRM
099400000000     C     *LIKE         DEFINE    D60CMP        WCMP
099500000000     C     *LIKE         DEFINE    D60NBC        WNBC
099600000000     C     *LIKE         DEFINE    D60DDC        WDDC
099700000000     C     *LIKE         DEFINE    D60KCC        WKCC
099800000000     C     *LIKE         DEFINE    D60KSC        WKSC
099900000000     C     *LIKE         DEFINE    D60RIC        WRIC
100000000000     C     *LIKE         DEFINE    D60DDO        WDDO
100100940719     C     *LIKE         DEFINE    D60FRA        WFRA
100200940725     C     *LIKE         DEFINE    D60IOE        WIOE
100300940719     C     *LIKE         DEFINE    D60CCF        WCCF
100400000000     C*
100500000000     C* CHIAVE LETTURA ECEVT01L - COMPLETA
100600000000     C     KEYEVT        KLIST
100700000000     C                   KFLD                    KVTVOC
100800000000     C                   KFLD                    KVTPRG
100900000000     C*
101000000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
101100000000     C                   MOVE      D60OP0        INTGES            2
101200000000IF  1C     INTGES        IFEQ      '02'
101300000000     C                   MOVEL     'G'           $TGES             1
101400000000     C                   MOVEL     DESGES        VTDOPE
101500000000X   1C                   ELSE
101600000000     C                   MOVEL     'I'           $TGES             1
101700000000     C                   MOVEL     DESINT        VTDOPE
101800000000E   1C                   ENDIF
101900000000     C*
102000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
102100000000     C                   EXSR      INZSTD
102200000000     C*
102300000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
102400000000     C                   EXSR      INZSFL
102500000000     C*
102600000000     C* IMPOSTO CAMPI TESTATA VIDEO
102700000000     C                   MOVEL     DSFIRS        VTDRSU
102800000000     C*
102900000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
103000000000     C                   Z-ADD     *ZEROS        VCCVOC
103100000000     C                   Z-ADD     *ZEROS        VCCPRG
103200000000     C                   Z-ADD     *ZEROS        VCCDDV
103300010111     C                   Z-ADD     *ZEROS        VCCVAL
103400040209     C                   Z-ADD     *ZEROS        VCCcmp
103500130910     C                   clear                   VCCvod
103600040209     c* se ricevuti parametri li imposto
103700040209     c                   if        d60cmp <> 0
103800040209     c                   z-add     d60cmp        vcccmp
103900040209     c                   end
104000000000     C*
104100000000     C* EMETTO VIDEATA SUBFILE
104200000000     C                   EXSR      CARVS
104300000000     C*
104400000000     C                   ENDSR
104500000000     C*------------------------------------------------------------------------*
104600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
104700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
104800000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
104900000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
105000000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
105100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
105200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
105300000000     C*------------------------------------------------------------------------*
105400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
105500000000     C*------------------------------------------------------------------------*
105600000000     C     INZSTD        BEGSR
105700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
105800000000     C                   CALL      'XKEYS'
105900000000     C                   PARM                    DSKEYS
106000000000     C                   MOVEA     KEYRIG        KEYS
106100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
106200000000     C     XMSGPA        PLIST
106300000000     C                   PARM                    DSMSMS
106400000000     C                   PARM                    DSMSPG
106500000000     C                   PARM                    DSMSKE
106600000000     C                   PARM                    DSMSER
106700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
106800000000     C                   MOVEL     'TREC60R'     DSMSPG
106900000000     C                   MOVEL     'TREC60R'     VMCQUE
107000000000     C                   MOVEL     *BLANKS       VMCKEY
107100000000     C                   MOVEL     *BLANKS       DSMSMS
107200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
107300000000     C                   MOVEL     '0'           $ERR              1
107400000000     C                   MOVEL     '0'           $F03              1
107500000000     C                   MOVEL     *BLANKS       $GES              2
107600000000     C                   MOVEL     *BLANKS       $MSG             80
107700000000     C                   MOVEL     '0'           $FINE             1
107800000000     C                   MOVEL     '0'           $TESVD            1
107900000000     C* LANCIO ROUTINE PER VERIFICARE SE
108000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
108100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
108200020923     C                   EXSR      REPDATIUTE
108300000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
108400000000     C                   BITOFF    '01234567'    POSMI1            1
108500000000     C                   BITOFF    '01234567'    POSMI2            1
108600000000     C                   BITOFF    '01234567'    POSMA1            1
108700000000     C                   BITOFF    '01234567'    POSMA2            1
108800000000     C                   BITON     '34'          POSMI1            1
108900000000     C                   BITON     '7'           POSMI2            1
109000000000     C                   BITON     '34'          POSMA1            1
109100000000     C                   BITON     '0'           POSMA2            1
109200000000     C                   MOVE      POSMI2        POSMIN            2
109300000000     C                   MOVEL     POSMI1        POSMIN
109400000000     C                   MOVE      POSMA2        POSMAX            2
109500000000     C                   MOVEL     POSMA1        POSMAX
109600000000     C                   ENDSR
109700000000     C*------------------------------------------------------------------------*
109800000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
109900000000     C*------------------------------------------------------------------------*
110000000000     C     FUNSTD        BEGSR
110100000000     C                   SELECT
110200000000     C     KEY           WHENEQ    F3
110300000000     C                   EXSR      F03STD
110400000000     C     KEY           WHENEQ    F12
110500000000     C                   EXSR      F12STD
110600000000     C     KEY           WHENEQ    F24
110700000000     C                   EXSR      F24STD
110800000000     C                   OTHER
110900000000     C                   MOVEL     MSG(2)        $MSG
111000000000     C                   EXSR      CARMSG
111100000000     C                   ENDSL
111200000000     C                   ENDSR
111300000000     C*------------------------------------------------------------------------*
111400000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
111500000000     C*------------------------------------------------------------------------*
111600000000     C     F03STD        BEGSR
111700000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
111800000000     C                   MOVEL     '1'           $F03
111900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
112000000000     C                   MOVEL     '1'           $FINE
112100000000     C                   ENDSR
112200000000     C*------------------------------------------------------------------------*
112300000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
112400000000     C*------------------------------------------------------------------------*
112500000000     C     F12STD        BEGSR
112600000000     C* IMPOSTO FLAG DI FIN PGM
112700000000     C                   MOVEL     '1'           $FINE
112800000000     C                   ENDSR
112900000000     C*------------------------------------------------------------------------*
113000000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
113100000000     C*------------------------------------------------------------------------*
113200000000     C     F24STD        BEGSR
113300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
113400000000     C                   EXSR      OUTFUN
113500000000     C                   ENDSR
113600000000     C*------------------------------------------------------------------------*
113700000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
113800000000     C*------------------------------------------------------------------------*
113900000000     C     INZSFL        BEGSR
114000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
114100000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
114200000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
114300000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
114400000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
114500000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
114600000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
114700000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
114800000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
114900000000     C                   Z-ADD     12            SFLPAG
115000000000     C                   ENDSR
115100000000     C*------------------------------------------------------------------------*
115200000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
115300000000     C*------------------------------------------------------------------------*
115400000000     C     FUNSFL        BEGSR
115500000000     C                   SELECT
115600000000     C     KEY           WHENEQ    ENTER
115700000000     C                   EXSR      ENTSFL
115800000000     C     KEY           WHENEQ    F3
115900000000     C                   EXSR      F03STD
116000000000     C     KEY           WHENEQ    F5
116100000000     C                   EXSR      F05STD
116200000000     C     KEY           WHENEQ    F12
116300000000     C                   EXSR      F12STD
116400000000     C     KEY           WHENEQ    F17
116500000000     C                   EXSR      F17STD
116600000000     C     KEY           WHENEQ    F18
116700000000     C                   EXSR      F18STD
116800000000     C     KEY           WHENEQ    F23
116900000000     C                   EXSR      F23STD
117000000000     C     KEY           WHENEQ    F24
117100000000     C                   EXSR      F24STD
117200000000     C     KEY           WHENEQ    ROLLUP
117300000000     C                   EXSR      RUPSTD
117400000000     C     KEY           WHENEQ    ROLLDN
117500000000     C                   EXSR      RDNSTD
117600000000     C                   OTHER
117700000000     C                   MOVEL     MSG(2)        $MSG
117800000000     C                   EXSR      CARMSG
117900000000     C                   ENDSL
118000000000     C                   ENDSR
118100000000     C*------------------------------------------------------------------------*
118200000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
118300000000     C*------------------------------------------------------------------------*
118400000000     C     F05STD        BEGSR
118500000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
118600000000     C                   EXSR      POSSFL
118700000000     C                   ENDSR
118800000000     C*------------------------------------------------------------------------*
118900000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
119000000000     C*------------------------------------------------------------------------*
119100000000     C     F17STD        BEGSR
119200000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
119300000000     C                   Z-ADD     1             VCCNBR
119400000000     C                   ENDSR
119500000000     C*------------------------------------------------------------------------*
119600000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
119700000000     C*------------------------------------------------------------------------*
119800000000     C     F18STD        BEGSR
119900000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
120000000000     C                   Z-ADD     SFLULT        VCCNBR
120100000000     C                   ENDSR
120200000000     C*------------------------------------------------------------------------*
120300000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
120400000000     C*------------------------------------------------------------------------*
120500000000     C     F23STD        BEGSR
120600000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
120700000000     C                   EXSR      OUTOPZ
120800000000     C                   ENDSR
120900000000     C*------------------------------------------------------------------------*
121000000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
121100000000     C*------------------------------------------------------------------------*
121200000000     C     RUPSTD        BEGSR
121300000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
121400000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
121500000000     C     CURSOR        IFGE      POSMIN
121600000000     C     CURSOR        ANDLE     POSMAX
121700000000     C                   ELSE
121800000000     C                   EXSR      CARSFL
121900000000     C                   ENDIF
122000000000     C                   ENDSR
122100000000     C*------------------------------------------------------------------------*
122200000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
122300000000     C*------------------------------------------------------------------------*
122400000000     C     RDNSTD        BEGSR
122500000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
122600000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
122700000000     C     CURSOR        IFGE      POSMIN
122800000000     C     CURSOR        ANDLE     POSMAX
122900000000     C                   ELSE
123000000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
123100000000     C                   MOVEL     '1'           $ERR
123200000000     C                   MOVEL     MSG(5)        $MSG
123300000000     C                   EXSR      CARMSG
123400000000     C                   ENDIF
123500000000     C                   ENDSR
123600000000     C*------------------------------------------------------------------------*
123700000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
123800000000     C*------------------------------------------------------------------------*
123900000000     C     ENTSFL        BEGSR
124000000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
124100000000     C                   EXSR      NEWPOS
124200000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
124300000000     C     $ERR          IFEQ      '0'
124400000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
124500000000     C     SWPOSI        IFNE      0
124600000000     C                   EXSR      POSSFL
124700000000     C                   ELSE
124800000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
124900000000     C     SFLULT        IFNE      0
125000000000     C                   EXSR      OPZSFL
125100000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
125200000000     C     SWMODI        IFNE      0
125300000000     C                   EXSR      POSSFL
125400000000     C                   ENDIF
125500000000     C                   ENDIF
125600000000     C                   ENDIF
125700000000     C                   ENDIF
125800000000     C                   ENDSR
125900000000     C*------------------------------------------------------------------------*
126000000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
126100000000     C*------------------------------------------------------------------------*
126200000000     C     POSSFL        BEGSR
126300000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
126400000000     C                   EXSR      CLRSFL
126500000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
126600000000     C                   EXSR      SETFIL
126700000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
126800000000     C     SFLEOF        IFEQ      0
126900000000     C                   EXSR      CARSFL
127000000000     C                   ENDIF
127100000000     C                   ENDSR
127200000000     C*------------------------------------------------------------------------*
127300000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
127400000000     C*------------------------------------------------------------------------*
127500000000     C     OPZSFL        BEGSR
127600000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
127700000000     C                   MOVEL     'S'           OPZOK             1
127800000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
127900000000     C                   Z-ADD     0             SFLVAR            5 0
128000000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
128100000000     C                   Z-ADD     0             SFLERR
128200000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
128300000000     C                   Z-ADD     SFLULT        WFLULT
128400000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
128500000000     C                   READC     EC60DVS                                49
128600000000     C     *IN49         DOWEQ     '0'
128700941121     C     $ERR          ANDEQ     '0'
128800000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
128900000000     C                   ADD       1             SFLVAR
129000000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
129100000000     C     OPZOK         IFEQ      'S'
129200000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
129300000000     C     VSCOPZ        IFNE      *BLANKS
129400000000     C                   EXSR      CONOPZ
129500000000     C     OPZOK         IFEQ      'S'
129600941121     C     $ERR          ANDEQ     '0'
129700000000     C                   EXSR      GESOPZ
129800000000     C                   ENDIF
129900000000     C                   ENDIF
130000000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
130100000000     C*            2. PULIZIA SCELTA IN SFL
130200000000     C     OPZOK         IFEQ      'S'
130300000000     C                   MOVEL     '0'           *IN41
130400000000     C                   MOVEL     *BLANKS       VSCOPZ
130500000000     C                   ELSE
130600000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
130700000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
130800000000     C                   MOVEL     '1'           *IN41
130900000000     C                   Z-ADD     SFLULT        SFLERR
131000000000     C                   ENDIF
131100000000     C                   ELSE
131200000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
131300000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
131400000000     C                   MOVEL     '1'           *IN41
131500000000     C                   ENDIF
131600000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
131700000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
131800000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
131900000000     C     OPZOK         IFNE      'S'
132000000000     C                   Z-ADD     SFLERR        VCCNBR
132100000000     C                   ELSE
132200000000     C                   Z-ADD     SFLULT        VCCNBR
132300000000     C                   ENDIF
132400000000     C* AGGIORNO RECORD SFL
132500000000     C                   UPDATE    EC60DVS
132600941121     C     $ERR          IFEQ      '0'
132700000000     C                   READC     EC60DVS                                49
132800941121     C                   ENDIF
132900000000     C                   ENDDO
133000000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
133100000000     C                   Z-ADD     WFLULT        SFLULT
133200000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
133300000000     C     SFLVAR        IFEQ      0
133400000000     C                   MOVEL     '1'           $FINE
133500000000     C                   ENDIF
133600000000     C                   ENDSR
133700000000     C*------------------------------------------------------------------------*
133800000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
133900000000     C*------------------------------------------------------------------------*
134000000000     C     CLRSFL        BEGSR
134100000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
134200000000     C* (0=NON FINE FILE  1=FINE FILE)
134300000000     C                   Z-ADD     0             SFLEOF
134400000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
134500000000     C                   Z-ADD     0             SFLULT
134600000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
134700000000     C                   MOVEL     '1'           *IN40
134800000000     C                   WRITE     EC60DVC
134900000000     C                   MOVEL     '0'           *IN40
135000000000     C                   ENDSR
135100000000     C*------------------------------------------------------------------------*
135200000000     C* CARSFL - CARICAMENTO SFL
135300000000     C*------------------------------------------------------------------------*
135400000000     C     CARSFL        BEGSR
135500000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
135600000000     C     SFLULT        IFEQ      0
135700000000     C                   EXSR      SETFIL
135800000000     C                   ENDIF
135900000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
136000000000     C                   Z-ADD     SFLULT        WFLULT
136100000000     C* CICLO:
136200000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
136300000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
136400000000     C                   Z-ADD     1             SFLRIG
136500000000     C     SFLRIG        DOWLE     SFLPAG
136600000000     C     SFLEOF        ANDEQ     0
136700000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
136800000000     C                   MOVEL     *BLANKS       VSCOPZ
136900000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
137000000000     C                   EXSR      IRESFL
137100000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
137200000000     C                   ADD       1             SFLULT
137300000000     C* SCRITTURA RECORD SFL
137400000000     C                   WRITE     EC60DVS
137500000000     C                   ADD       1             SFLRIG
137600000000     C* LETTURA FILE DA CARICARE
137700000000     C                   EXSR      LETFIL
137800000000     C                   ENDDO
137900000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
138000000000     C     WFLULT        IFNE      SFLULT
138100000000     C                   Z-ADD     WFLULT        VCCNBR
138200000000     C                   ADD       1             VCCNBR
138300000000     C                   ENDIF
138400000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
138500000000     C     SFLEOF        IFEQ      1
138600000000     C     SFLULT        ANDGT     *ZEROS
138700000000     C                   MOVEL     '1'           $ERR
138800000000     C                   MOVEL     MSG(6)        $MSG
138900000000     C                   EXSR      CARMSG
139000000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
139100000000     C     SFLULT        CHAIN     EC60DVS                            99
139200000000     C     *IN99         IFEQ      '0'
139300000000     C                   SETON                                        45
139400000000     C                   UPDATE    EC60DVS
139500000000     C                   SETOFF                                       45
139600000000     C                   ENDIF
139700000000     C                   ENDIF
139800000000     C                   ENDSR
139900000000     C*------------------------------------------------------------------------*
140000000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
140100000000     C*------------------------------------------------------------------------*
140200000000     C     OUTSFL        BEGSR
140300000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
140400000000     C     MSGULT        IFNE      *ZEROS
140500000000     C                   EXSR      OUTMSG
140600000000     C                   ENDIF
140700000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
140800000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
140900000000     C     SFLEOF        IFEQ      0
141000000000     C                   MOVEL     '0'           *IN42
141100000000     C                   ELSE
141200000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
141300000000     C                   MOVEL     '1'           *IN42
141400000000     C                   ENDIF
141500000000     C* EMISSIONE SUBFILE
141600000000     C     SFLULT        IFLE      0
141700000000     C*  --> CASO DI SFL VUOTO
141800000000     C                   MOVEL     '0'           *IN43
141900000000     C                   WRITE     EC60DVV
142000000000     C                   WRITE     EC60DVC
142100000000     C                   READ      EC60DVC                                99
142200000000     C                   ELSE
142300000000     C*  --> CASO DI SFL CON RECORDS
142400000000     C                   WRITE     EC60DVC
142500000000     C                   READ      EC60DVC                                99
142600000000     C                   ENDIF
142700000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
142800000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
142900000000     C                   Z-ADD     FRTSFL        VCCNBR
143000000000     C                   ENDSR
143100000000     C*------------------------------------------------------------------------*
143200000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
143300000000     C*          CODA MESSAGGI DEL PGM
143400000000     C*------------------------------------------------------------------------*
143500000000     C     CARMSG        BEGSR
143600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
143700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
143800000000     C     MSGULT        IFEQ      0
143900000000     C                   MOVEL     '1'           *IN40
144000000000     C                   WRITE     EC60DVK
144100000000     C                   MOVEL     '0'           *IN40
144200000000     C                   ENDIF
144300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
144400000000     C                   MOVEL     $MSG          DSMSMS
144500000000     C                   CALL      'XMSG'        XMSGPA
144600000000     C     DSMSER        IFEQ      0
144700000000     C                   MOVEL     DSMSKE        VMCKEY
144800000000     C                   ENDIF
144900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
145000000000     C                   ADD       1             MSGULT            4 0
145100000000     C                   WRITE     EC60DVM
145200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
145300000000     C                   MOVEL     *BLANKS       $MSG
145400000000     C                   ENDSR
145500000000     C*------------------------------------------------------------------------*
145600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
145700000000     C*------------------------------------------------------------------------*
145800000000     C     OUTMSG        BEGSR
145900000000     C* VISUALIZZAZIONE MESSAGGIO
146000000000     C                   MOVEL     '1'           *IN44
146100000000     C                   WRITE     EC60DVK
146200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
146300000000     C                   Z-ADD     *ZEROS        MSGULT
146400000000     C                   ENDSR
146500000000     C*------------------------------------------------------------------------*
146600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
146700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
146800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
146900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
147000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
147100000000     C*------------------------------------------------------------------------*
147200000000     C     CARFUN        BEGSR
147300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
147400000000     C     1             DO        31            I                 5 0
147500000000     C                   MOVEL     FUN(I)        FUNRIG
147600000000     C                   MOVEL     FUNFUN        FFU(I)
147700000000     C                   MOVEL     FUNDES        FDE(I)
147800020923     C     DUTLPO        IFEQ      'S'
147900000000     C     $TGES         IFEQ      'G'
148000000000     C                   MOVEL     FUNASG        FAT(I)
148100000000     C                   ELSE
148200000000     C                   MOVEL     FUNASI        FAT(I)
148300000000     C                   ENDIF
148400000000     C                   ELSE
148500000000     C     $TGES         IFEQ      'G'
148600000000     C                   MOVEL     FUNAFG        FAT(I)
148700000000     C                   ELSE
148800000000     C                   MOVEL     FUNAFI        FAT(I)
148900000000     C                   ENDIF
149000000000     C                   ENDIF
149100000000     C     FAT(I)        IFEQ      'E'
149200000000     C                   MOVEL     USER          WEDP              3
149300000000     C     WEDP          IFEQ      'EDP'
149400000000     C                   MOVEL     'S'           FAT(I)
149500000000     C                   ELSE
149600000000     C                   MOVEL     'N'           FAT(I)
149700000000     C                   ENDIF
149800000000     C                   ENDIF
149900000000     C                   ENDDO
150000000000     C                   ENDSR
150100000000     C*------------------------------------------------------------------------*
150200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
150300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
150400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
150500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
150600000000     C*------------------------------------------------------------------------*
150700000000     C     OUTFUN        BEGSR
150800000000     C     1             DO        8             I
150900000000     C                   MOVEL     *BLANKS       FUV(I)
151000000000     C                   ENDDO
151100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
151200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
151300000000     C     1             DO        31            I
151400000000     C     FAT(I)        IFEQ      'S'
151500000000     C     FDE(I)        ANDNE     *BLANKS
151600000000     C                   ADD       1             FUNNAT
151700000000     C                   ENDIF
151800000000     C                   ENDDO
151900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
152000000000     C     FUNNAT        IFGT      8
152100000000     C                   MOVEL     'S'           FAT(24)
152200000000     C                   ENDIF
152300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
152400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
152500000000     C                   Z-ADD     8             FUNVIS            5 0
152600000000     C     FAT(23)       IFEQ      'S'
152700000000     C                   SUB       1             FUNVIS
152800000000     C                   ENDIF
152900000000     C     FAT(24)       IFEQ      'S'
153000000000     C                   SUB       1             FUNVIS
153100000000     C                   ENDIF
153200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
153300000000     C                   Z-ADD     *ZEROS        W                 5 0
153400000000     C                   ADD       1             FUNULT
153500000000     C     FUNULT        IFGT      31
153600000000     C                   Z-ADD     1             FUNULT
153700000000     C                   ENDIF
153800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
153900000000     C     FUNULT        DO        31            I
154000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
154100000000     C     W             IFLT      FUNVIS
154200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
154300000000     C     FAT(I)        IFEQ      'S'
154400000000     C     FDE(I)        ANDNE     *BLANKS
154500000000     C     I             IFEQ      23
154600000000     C     I             OREQ      24
154700000000     C                   ELSE
154800000000     C                   ADD       1             W
154900000000     C                   MOVEL     FDE(I)        FUV(W)
155000000000     C                   ENDIF
155100000000     C                   ENDIF
155200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
155300000000     C                   Z-ADD     I             FUNULT
155400000000     C                   ENDIF
155500000000     C                   ENDDO
155600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
155700000000     C                   MOVEL     FUV(1)        VFD001
155800000000     C                   MOVEL     FUV(2)        VFD002
155900000000     C                   MOVEL     FUV(3)        VFD003
156000000000     C                   MOVEL     FUV(4)        VFD004
156100000000     C                   MOVEL     FUV(5)        VFD005
156200000000     C                   MOVEL     FUV(6)        VFD006
156300000000     C     FAT(23)       IFEQ      'N'
156400000000     C     FAT(24)       ANDEQ     'N'
156500000000     C                   MOVEL     FUV(7)        VFD007
156600000000     C                   MOVEL     FUV(8)        VFD008
156700000000     C                   ELSE
156800000000     C     FAT(23)       IFEQ      'S'
156900000000     C     FAT(24)       ANDEQ     'S'
157000000000     C                   MOVEL     FDE(23)       VFD007
157100000000     C                   MOVEL     FDE(24)       VFD008
157200000000     C                   ELSE
157300000000     C                   MOVEL     FUV(7)        VFD007
157400000000     C     FAT(23)       IFEQ      'S'
157500000000     C                   MOVEL     FDE(23)       VFD008
157600000000     C                   ELSE
157700000000     C                   MOVEL     FDE(24)       VFD008
157800000000     C                   ENDIF
157900000000     C                   ENDIF
158000000000     C                   ENDIF
158100000000     C                   MOVEL     '1'           $TESVD
158200000000     C                   ENDSR
158300000000     C*------------------------------------------------------------------------*
158400000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
158500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
158600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
158700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
158800000000     C*------------------------------------------------------------------------*
158900000000     C     CONFUN        BEGSR
159000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
159100000000     C     1             DO        31            I
159200000000     C     KEY           IFEQ      KEYS(I)
159300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
159400000000     C     FAT(I)        IFNE      'S'
159500000000     C                   MOVEL     MSG(1)        $MSG
159600000000     C                   EXSR      CARMSG
159700000000     C                   MOVEL     '1'           $ERR
159800000000     C                   ENDIF
159900000000     C                   ENDIF
160000000000     C                   ENDDO
160100000000     C                   ENDSR
160200000000     C*------------------------------------------------------------------------*
160300000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
160400000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
160500000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
160600000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
160700000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
160800000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
160900000000     C*------------------------------------------------------------------------*
161000000000     C     CAROPZ        BEGSR
161100000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
161200000000     C     1             DO        24            I
161300000000     C                   MOVEL     OPZ(I)        OPZRIG
161400000000     C                   MOVEL     OPZOPD        OPD(I)
161500000000     C                   MOVEL     OPZOPS        OPS(I)
161600000000     C                   MOVEL     OPZDES        ODE(I)
161700020923     C     DUTLPO        IFEQ      'S'
161800000000     C     $TGES         IFEQ      'G'
161900000000     C                   MOVEL     OPZASG        OAT(I)
162000000000     C                   ELSE
162100000000     C                   MOVEL     OPZASI        OAT(I)
162200000000     C                   ENDIF
162300000000     C                   ELSE
162400000000     C     $TGES         IFEQ      'G'
162500000000     C                   MOVEL     OPZAFG        OAT(I)
162600000000     C                   ELSE
162700000000     C                   MOVEL     OPZAFI        OAT(I)
162800000000     C                   ENDIF
162900000000     C                   ENDIF
163000000000     C     OAT(I)        IFEQ      'E'
163100000000     C                   MOVEL     USER          WEDP              3
163200000000     C     WEDP          IFEQ      'EDP'
163300000000     C                   MOVEL     'S'           OAT(I)
163400000000     C                   ELSE
163500000000     C                   MOVEL     'N'           OAT(I)
163600000000     C                   ENDIF
163700000000     C                   ENDIF
163800000000     C                   ENDDO
163900000000     C                   ENDSR
164000000000     C*------------------------------------------------------------------------*
164100000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
164200000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
164300000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
164400000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
164500000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
164600000000     C*------------------------------------------------------------------------*
164700000000     C     OUTOPZ        BEGSR
164800000000     C     1             DO        8             I
164900000000     C                   MOVEL     *BLANKS       OPV(I)
165000000000     C                   ENDDO
165100000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
165200000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
165300000000     C                   ADD       1             OPZULT
165400000000     C     OPZULT        IFGT      24
165500000000     C                   Z-ADD     1             OPZULT
165600000000     C                   END
165700000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
165800000000     C     1             DO        24            I
165900000000     C     OAT(I)        IFEQ      'S'
166000000000     C     ODE(I)        ANDNE     *BLANKS
166100000000     C                   ADD       1             OPZNAT
166200000000     C                   ENDIF
166300000000     C                   ENDDO
166400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
166500000000     C                   Z-ADD     0             W
166600000000     C     OPZULT        DO        24            I
166700000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
166800000000     C     W             IFLT      8
166900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
167000000000     C     OAT(I)        IFEQ      'S'
167100000000     C     ODE(I)        ANDNE     *BLANKS
167200000000     C                   ADD       1             W
167300000000     C                   MOVEL     ODE(I)        OPV(W)
167400000000     C                   ENDIF
167500000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
167600000000     C                   Z-ADD     I             OPZULT
167700000000     C                   ENDIF
167800000000     C                   ENDDO
167900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
168000000000     C                   MOVEL     OPV(1)        VOD001
168100000000     C                   MOVEL     OPV(2)        VOD002
168200000000     C                   MOVEL     OPV(3)        VOD003
168300000000     C                   MOVEL     OPV(4)        VOD004
168400000000     C                   MOVEL     OPV(5)        VOD005
168500000000     C                   MOVEL     OPV(6)        VOD006
168600000000     C                   MOVEL     OPV(7)        VOD007
168700000000     C                   MOVEL     OPV(8)        VOD008
168800000000     C                   MOVEL     '1'           $TESVD
168900000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
169000000000     C     OPZNAT        IFGT      8
169100000000     C                   MOVEL     'S'           FAT(23)
169200000000     C                   ENDIF
169300000000     C                   ENDSR
169400000000     C*------------------------------------------------------------------------*
169500000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
169600000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
169700000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
169800000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
169900000000     C*------------------------------------------------------------------------*
170000000000     C     CONOPZ        BEGSR
170100000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
170200000000     C                   MOVEL     *BLANKS       OPZOK             1
170300000000     C     1             DO        24            I                 5 0
170400000000     C     VSCOPZ        IFEQ      OPD(I)
170500000000     C     VSCOPZ        OREQ      OPS(I)
170600000000     C     OAT(I)        IFEQ      'S'
170700000000     C                   MOVEL     OPD(I)        VSCOPZ
170800000000     C                   MOVEL     'S'           OPZOK
170900000000     C                   ELSE
171000000000     C                   MOVEL     'N'           OPZOK             1
171100000000     C                   ENDIF
171200000000     C                   ENDIF
171300000000     C                   ENDDO
171400000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
171500000000     C     OPZOK         IFNE      'S'
171600000000     C                   MOVEL     MSG(3)        $MSG
171700000000     C                   EXSR      CARMSG
171800000000     C                   MOVEL     '1'           $ERR
171900000000     C                   MOVEL     'N'           OPZOK             1
172000000000     C                   ENDIF
172100000000     C                   ENDSR
172200000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
172300000000TREC60R- Tasto funzionale non valido.                                          1
172400000000TREC60R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
172500000000TREC60R- Opzione non valida.                                                   3
172600000000TREC60R- Opzione abilitata ma non gestita. Avvertire CED.                      4
172700000000TREC60R- Inizio elenco.                                                        5
172800000000TREC60R- Fine elenco.                                                          6
172900000000TREC60R- 7
173000010111TREC60R- Data di riferimento errata.                                           8
173100000000TREC60R- Data decorrenza voce errata                                           9
173200010111TREC60R- Data di validità non valida                                          10
173300010115TREC60R- Duplica effettuata ma verificare tariffe per eventuali incongruenze! 11
173400000000TREC60R- Codice voce annullato, consentito solo il ripristino                 12
173500000000TREC60R- Codice voce non annullato, impossibile ripristinare                  13
173600000000TREC60R- La gestione e' consentita solo per voci di tipo 'M'=Maggiorazione    14
173700000000TREC60R- 15
173800000000TREC60R- 16
173900000000TREC60R- 17
174000000000TREC60R- 18
174100000000TREC60R- 19
174200000000TREC60R- 20
174300000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
174400000000- 1-1 -                   -    NN     NN
174500000000- 2-2 -2=Modifica         -    SN     NN
174600010110- 3-3 -3=Duplica          -    SN     NN
174700000000- 4-4 -4=Cancellazione    -    SN     NN
174800000000- 5-5 -5=Visualizzazione  -    SS     SS
174900000000- 6-6 -                   -    NN     NN
175000000000- 7-7 -7=Ripristino       -    SN     NN
175100000000- 8-8 -8=Tariffe          -    SS     SS
175200000000- 9-9 -9=Competenze       -    SS     SS
175300000000-10-10-10=Maggiorazioni   -    SS     SS
175400130919-11-  -                   -    NN     NN
175500000000-12-  -                   -    NN     NN
175600000000-13-  -                   -    NN     NN
175700000000-14-  -                   -    NN     NN
175800000000-15-  -                   -    NN     NN
175900000000-16-  -                   -    NN     NN
176000130919-17-17-17=Note C.E.       -    SS     SS
176100000000-18-  -                   -    NN     NN
176200000000-19-  -                   -    NN     NN
176300000000-20-  -                   -    NN     NN
176400000000-21-  -                   -    NN     NN
176500000000-22-  -                   -    NN     NN
176600000000-23-  -                   -    NN     NN
176700000000-24-  -                   -    NN     NN
176800000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
176900000000F1    -                   -    NN     NN
177000000000F2    -                   -    NN     NN
177100000000F3    -F3=Fine            -    SS     SS
177200000000F4    -                   -    NN     NN
177300000000F5    -F5=Rivisualizza    -    SS     SS
177400000000F6    -F6=Inserimento     -    SN     NN
177500000000F7    -                   -    NN     NN
177600000000F8    -                   -    NN     NN
177700000000F9    -                   -    NN     NN
177800000000F10   -                   -    NN     NN
177900000000F11   -                   -    NN     NN
178000000000F12   -F12=Ritorno        -    SS     SS
178100000000F13   -                   -    NN     NN
178200000000F14   -                   -    NN     NN
178300000000F15   -                   -    NN     NN
178400000000F16   -                   -    NN     NN
178500961003F17   -F17=Inizio dati    -    NN     NN
178600961003F18   -F18=Fine dati      -    NN     NN
178700000000F19   -                   -    NN     NN
178800000000F20   -                   -    NN     NN
178900000000F21   -                   -    NN     NN
179000000000F22   -                   -    NN     NN
179100130919F23   -F23=Altre opzioni  -    SS     SS
179200000000F24   -F24=Altri tasti    -    NN     NN
179300000000ENTER -                   -    SS     SS
179400000000ROLLUP-                   -    SS     SS
179500000000ROLLDN-                   -    SS     SS
179600000000HELP  -                   -    NN     NN
179700000000PRINT -                   -    NN     NN
179800000000BSPACE-                   -    NN     NN
179900000000CLEAR -                   -    NN     NN

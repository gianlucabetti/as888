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
005800130919     D Trec69DS      E DS
005900020923     D TIBS34DS      E DS
006000020923     D DDATIUTE      E DS
006100020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006200000000     D*---------------------
006300000000     D* DS PER GESTIONE TASTI FUNZIONALI
006400000000     D*---------------------
006500000000     D DSKEYS        E DS
006600000000     D  KEYRIG                 1     40
006700000000     D*---------------------
006800000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006900000000     D*---------------------
007000000000     D                SDS
007100000000     D  USER                 254    263
007200000000     D*---------------------
007300000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007400000000     D*---------------------
007500000000     D DEVDS1          DS
007600000000     D* CHIAVE FUNZIONALE BATTUTA
007700000000     D  KEY                  369    369
007800000000     D* POSIZIONE CURSORE (IN HEX)
007900000000     D  CURSOR               370    371
008000000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
008100000000     D  FRTSFL               378    379B 0
008200000000     D*---------------------
008300000000     D* DS ESTERNA DI PROCEDURA
008400000000     D*---------------------
008500000000     D DS60          E DS                  EXTNAME(TREC60DS)
008600010110     D DS61          E DS                  EXTNAME(TREC61DS)
008700000000     D*---------------------
008800000000     D* ARCHITETTURA
008900000000     D*---------------------
009000000000     D KPJBA         E DS
009100130919     d  savkpjbu       s                   like(kpjbu)
009200000000     D*------------------
009300000000     D* CALL XSRDA8 - CONTROLLO DATE
009400000000     D*------------------
009500000000     D WLBDAT          DS                  INZ
009600000000     D  G02DAT                 1      8  0
009700000000     D  G02INV                 9     16  0
009800000000     D  G02ERR                17     17
009900000000     D  G02TGI                18     22  0
010000000000     D*------------------
010100000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
010200000000     D*------------------
010300000000     D DESGES          C                   CONST('** GESTIONE **')
010400000000     D DESINT          C                   CONST('INTERROGAZIONE')
010500000000     C*------------------------------------------------------------------------*
010600000000     C* MAIN LINE
010700000000     C*------------------------------------------------------------------------*
010800000000     C*
010900000000     C* CICLO FINO A CHE:
011000000000DO  1C     $FINE         DOUEQ     '1'
011100000000     C*
011200000000     C                   MOVEL     '0'           $ERR
011300000000     C*
011400000000     C* CONTROLLA GESTIONE VIDEO
011500000000IF  2C     $GES          IFEQ      'VS'
011600000000     C                   EXSR      GESVS
011700000000E   2C                   ENDIF
011800000000     C*
011900000000E   1C                   ENDDO
012000000000     C* FINE PGM
012100000000     C                   SETON                                        LR
012200000000     C*------------------------------------------------------------------------*
012300000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
012400000000     C*------------------------------------------------------------------------*
012500000000     C     CARVS         BEGSR
012600000000     C*
012700000000     C                   MOVEL     'VS'          $GES
012800000000     C*
012900000000     C                   EXSR      CAROPZ
013000000000     C                   EXSR      OUTOPZ
013100000000     C                   MOVEA     FUN2          FUN
013200000000     C                   EXSR      CARFUN
013300000000     C                   EXSR      OUTFUN
013400000000     C*
013500000000     C                   WRITE     EC60DVT
013600000000     C                   WRITE     EC60DVO
013700000000     C                   WRITE     EC60DVF
013800000000     C*
013900000000     C                   EXSR      POSSFL
014000000000     C*
014100000000     C                   ENDSR
014200000000     C*------------------------------------------------------------------------*
014300000000     C* GESVS  - GESTIONE SUBFILE
014400000000     C*------------------------------------------------------------------------*
014500000000     C     GESVS         BEGSR
014600000000     C*
014700000000IF  1C     $TESVD        IFEQ      '1'
014800000000     C                   WRITE     EC60DVT
014900000000     C                   WRITE     EC60DVO
015000000000     C                   WRITE     EC60DVF
015100000000E   1C                   ENDIF
015200000000     C*
015300000000     C                   EXSR      OUTSFL
015400000000     C*
015500000000     C                   EXSR      CONFUN
015600000000     C*
015700000000IF  1C     $ERR          IFEQ      '0'
015800000000SL  2C                   SELECT
015900000000     C     KEY           WHENEQ    F6
016000000000     C                   EXSR      F06GES
016100000000     C                   OTHER
016200000000     C                   EXSR      FUNSFL
016300000000E   2C                   ENDSL
016400000000E   1C                   ENDIF
016500000000     C*
016600000000     C                   ENDSR
016700000000     C*------------------------------------------------------------------------*
016800000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
016900000000     C*------------------------------------------------------------------------*
017000000000     C     F06GES        BEGSR
017100000000     C*
017200000000     C* LANCIO L'INSERIMENTO
017300000000     C                   Z-ADD     *ZEROS        WVOC
017400000000     C                   Z-ADD     *ZEROS        WPRG
017500000000     C                   Z-ADD     *ZEROS        WDDV
017600000000     C*
017700000000     C                   MOVEL     *BLANKS       WTTV
017800000000     C                   Z-ADD     *ZEROS        WFT1
017900000000     C                   Z-ADD     *ZEROS        WFT2
018000000000     C                   Z-ADD     *ZEROS        WCLI
018100000000     C                   Z-ADD     *ZERO         WCTR
018200000000     C                   MOVE      *BLANKS       WCTS
018300131002     C                   MOVE      *BLANKS       WCad
018400131002     C                   MOVE      *BLANKS       Wnaz
018500000000     C                   Z-ADD     *ZERO         WCAP
018600000000     C                   Z-ADD     *ZEROS        WDDT
018700000000     C                   Z-ADD     *ZEROS        WSGI
018800000000     C                   Z-ADD     *ZEROS        WVOM
018900000000     C                   Z-ADD     *ZEROS        WPRM
019000000000     C                   Z-ADD     *ZERO         WCMP
019100000000     C                   Z-ADD     *ZERO         WNBC
019200000000     C                   Z-ADD     *ZEROS        WDDC
019300000000     C                   Z-ADD     *ZEROS        WKCC
019400000000     C                   Z-ADD     *ZEROS        WKSC
019500000000     C                   MOVEL     *BLANKS       WRIC
019600000000     C                   Z-ADD     *ZERO         WDDO
019700940719     C                   MOVEL     *BLANKS       WFRA
019800940725     C                   MOVEL     *BLANKS       WIOE
019900940719     C                   Z-ADD     *ZERO         WCCF
020000000000     C*
020100000000     C                   MOVEL     D60OP0        WOP0
020200000000     C                   MOVEL     'F06'         WOP1
020300000000     C                   MOVEL     'TREC61R'     WPGM              8
020400000000     C                   EXSR      LANPGM
020500000000     C*
020600000000     C                   EXSR      POSSFL
020700000000     C*
020800000000     C                   ENDSR
020900000000     C*------------------------------------------------------------------------*
021000000000     C*  GESOPZ - GESTIONE OPZIONI
021100000000     C*------------------------------------------------------------------------*
021200000000     C     GESOPZ        BEGSR
021300000000     C*
021400000000SL  1C                   SELECT
021500000000     C     VSCOPZ        WHENEQ    ' 2'
021600000000     C                   EXSR      O02GES
021700010110     C     VSCOPZ        WHENEQ    ' 3'
021800010110     C                   EXSR      O03GES
021900000000     C     VSCOPZ        WHENEQ    ' 4'
022000000000     C                   EXSR      O04GES
022100000000     C     VSCOPZ        WHENEQ    ' 5'
022200000000     C                   EXSR      O05GES
022300000000     C     VSCOPZ        WHENEQ    ' 7'
022400000000     C                   EXSR      O07GES
022500000000     C     VSCOPZ        WHENEQ    ' 8'
022600000000     C                   EXSR      O08GES
022700000000     C     VSCOPZ        WHENEQ    ' 9'
022800000000     C                   EXSR      O09GES
022900000000     C     VSCOPZ        WHENEQ    '10'
023000000000     C                   EXSR      O10GES
023100130919     C     VSCOPZ        WHENEQ    '17'
023200130919     C                   EXSR      O17GES
023300000000     C                   OTHER
023400000000     C                   MOVEL     MSG(4)        $MSG
023500000000     C                   EXSR      CARMSG
023600000000E   1C                   ENDSL
023700000000     C*
023800000000     C                   ENDSR
023900000000     C*------------------------------------------------------------------------*
024000000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
024100000000     C*------------------------------------------------------------------------*
024200000000     C     O02GES        BEGSR
024300000000     C*
024400000000IF  1C     VSCATB        IFEQ      'A'
024500000000     C                   MOVEL     '1'           $ERR
024600000000     C                   MOVEL     MSG(12)       $MSG
024700000000     C                   EXSR      CARMSG
024800000000E   1C                   ENDIF
024900000000     C*
025000000000IF  1C     $ERR          IFEQ      '0'
025100000000     C*
025200000000     C* LANCIO LA MODIFICA
025300000000     C                   Z-ADD     VSCVOC        WVOC
025400000000     C                   Z-ADD     VSCPRG        WPRG
025500000000     C                   Z-ADD     VSCDDV        WDDV
025600000000     C                   MOVEL     *BLANKS       WTTV
025700000000     C                   Z-ADD     *ZEROS        WFT1
025800000000     C                   Z-ADD     *ZEROS        WFT2
025900000000     C                   Z-ADD     *ZEROS        WCLI
026000000000     C                   Z-ADD     *ZERO         WCTR
026100000000     C                   MOVEL     *BLANKS       WCTS
026200131002     C                   MOVE      *BLANKS       WCad
026300131002     C                   MOVE      *BLANKS       Wnaz
026400000000     C                   Z-ADD     *ZERO         WCAP
026500000000     C                   Z-ADD     *ZEROS        WDDT
026600000000     C                   Z-ADD     *ZEROS        WSGI
026700000000     C                   Z-ADD     *ZEROS        WVOM
026800000000     C                   Z-ADD     *ZEROS        WPRM
026900000000     C                   Z-ADD     *ZERO         WCMP
027000000000     C                   Z-ADD     *ZERO         WNBC
027100000000     C                   Z-ADD     *ZEROS        WDDC
027200000000     C                   Z-ADD     *ZEROS        WKCC
027300000000     C                   Z-ADD     *ZEROS        WKSC
027400000000     C                   MOVEL     *BLANKS       WRIC
027500000000     C                   Z-ADD     *ZERO         WDDO
027600940725     C                   MOVEL     *BLANKS       WFRA
027700940725     C                   MOVEL     *BLANKS       WIOE
027800940719     C                   Z-ADD     *ZERO         WCCF
027900000000     C*
028000000000     C                   MOVEL     D60OP0        WOP0
028100000000     C                   MOVEL     'O02'         WOP1
028200000000     C                   MOVEL     'TREC61R'     WPGM              8
028300000000     C                   EXSR      LANPGM
028400000000     C*
028500000000     C                   Z-ADD     1             SWMODI
028600000000     C*
028700000000E   1C                   ENDIF
028800000000     C*
028900000000     C                   ENDSR
029000010110     C*------------------------------------------------------------------------*
029100010110     C*  O03GES - GESTIONE OPZIONE 3=DUPLICA
029200010110     C*------------------------------------------------------------------------*
029300010110     C     O03GES        BEGSR
029400010110     C*
029500010110IF  1C     VSCATB        IFEQ      'A'
029600010110     C                   MOVEL     '1'           $ERR
029700010110     C                   MOVEL     MSG(12)       $MSG
029800010110     C                   EXSR      CARMSG
029900010110E   1C                   ENDIF
030000010110     C*
030100010110IF  1C     $ERR          IFEQ      '0'
030200010111     C                   SETOFF                                       KKKL
030300010111     C                   MOVEL     'N'           VCCPAR
030400010111     C                   Z-ADD     WN8           VCCDRI
030500010110     C     *INKK         DOWEQ     *OFF
030600010110     C     *INKL         ANDEQ     *OFF
030700010110     C                   EXFMT     EC60DWW
030800010111     C* CONTROLLO LA DATA DI RIFERIMENTO
030900010111     C                   Z-ADD     *ZERO         INVDRI            8 0
031000010111     C                   Z-ADD     VCCDRI        G02DAT
031100010111     C                   Z-ADD     *ZEROS        G02INV
031200010111     C                   MOVEL     '0'           G02ERR
031300010111     C                   CALL      'XSRDA8'
031400010111     C                   PARM                    WLBDAT
031500010111     C*
031600010111     C     G02ERR        IFEQ      '1'
031700010111     C                   MOVEL     '1'           $ERR
031800010111     C                   MOVEL     MSG(8)        $MSG
031900010115     C                   EXSR      CARMSG
032000010111     C                   SETOFF                                       KK
032100010111     C                   SETON                                        55
032200010111     C                   ELSE
032300010111     C                   Z-ADD     G02INV        INVDRI            8 0
032400010111     C                   SETOFF                                       55
032500010111     C                   ENDIF
032600010111     C*
032700010110     C                   ENDDO
032800010110     C*
032900010110     C* LANCIO LA DUPLICA
033000010110     C     *INKK         IFEQ      *ON
033100010110     C                   Z-ADD     VSCVOC        WVOC
033200010110     C                   Z-ADD     VSCPRG        WPRG
033300010110     C                   Z-ADD     VSCDDV        WDDV
033400010110     C                   MOVEL     *BLANKS       WTTV
033500010110     C                   Z-ADD     *ZEROS        WFT1
033600010110     C                   Z-ADD     *ZEROS        WFT2
033700010110     C                   Z-ADD     *ZEROS        WCLI
033800010110     C                   Z-ADD     *ZERO         WCTR
033900010110     C                   MOVEL     *BLANKS       WCTS
034000131002     C                   MOVE      *BLANKS       WCad
034100131002     C                   MOVE      *BLANKS       Wnaz
034200010110     C                   Z-ADD     *ZERO         WCAP
034300010110     C                   Z-ADD     *ZEROS        WDDT
034400010110     C                   Z-ADD     *ZEROS        WSGI
034500010110     C                   Z-ADD     *ZEROS        WVOM
034600010110     C                   Z-ADD     *ZEROS        WPRM
034700010110     C                   Z-ADD     *ZERO         WCMP
034800010110     C                   Z-ADD     *ZERO         WNBC
034900010110     C                   Z-ADD     *ZEROS        WDDC
035000010110     C                   Z-ADD     *ZEROS        WKCC
035100010110     C                   Z-ADD     *ZEROS        WKSC
035200010110     C                   MOVEL     *BLANKS       WRIC
035300010110     C                   Z-ADD     *ZERO         WDDO
035400010110     C                   MOVEL     *BLANKS       WFRA
035500010110     C                   MOVEL     *BLANKS       WIOE
035600010110     C                   Z-ADD     *ZERO         WCCF
035700010110     C*
035800010110     C                   MOVEL     D60OP0        WOP0
035900010110     C                   MOVEL     'O03'         WOP1
036000010110     C                   MOVEL     'TREC61R'     WPGM              8
036100010110     C                   EXSR      LANPGM
036200010110     C*
036300010111     C* LANCIO SE RICHIESTO LA DUPLICA ANCHE DELLE TARIFFE E SE OK DUPLICA VOCE
036400010115     C     D60ERR        IFNE      '1'
036500010110     C     VCCPAR        ANDEQ     'S'
036600010115     C* PLISCO IL SUB-FILE MESSAGGI E GESTISCO DIFFERENZIAZIONE MESSAGGIO
036700010115     C     D60ERR        IFEQ      '2'
036800010115     C                   Z-ADD     *ZEROS        MSGULT
036900010115     C                   MOVEL     MSG(11)       $MSG
037000010115     C                   EXSR      CARMSG
037100010115     C                   ENDIF
037200010110     C                   Z-ADD     VSCVOC        I61VOC
037300010110     C                   Z-ADD     VSCPRG        I61PRG
037400010111     C                   Z-ADD     D60DDV        I61DAI
037500010111     C                   Z-ADD     D60DDT        I61DAF
037600010111     C                   Z-ADD     INVDRI        I61DRI
037700010110     C                   Z-ADD     D60VOC        O61VOC
037800010110     C                   Z-ADD     D60PRG        O61PRG
037900010110     C                   CALL      'TREC61R1'
038000010110     C                   PARM                    DS60
038100010110     C                   PARM                    DS61
038200010110     C                   ENDIF
038300010110     C*
038400010110     C                   Z-ADD     1             SWMODI
038500010110     C*
038600010110     C                   ENDIF
038700010110E   1C                   ENDIF
038800010110     C*
038900010110     C                   ENDSR
039000000000     C*------------------------------------------------------------------------*
039100000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
039200000000     C*------------------------------------------------------------------------*
039300000000     C     O04GES        BEGSR
039400000000     C*
039500000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
039600000000IF  1C     VSCATB        IFEQ      'A'
039700000000     C                   MOVEL     '1'           $ERR
039800000000     C                   MOVEL     MSG(12)       $MSG
039900000000     C                   EXSR      CARMSG
040000000000E   1C                   ENDIF
040100000000     C*
040200000000IF  1C     $ERR          IFEQ      '0'
040300000000     C*
040400000000     C* LANCIO LA CANCELLAZIONE
040500000000     C                   Z-ADD     VSCVOC        WVOC
040600000000     C                   Z-ADD     VSCPRG        WPRG
040700000000     C                   Z-ADD     VSCDDV        WDDV
040800000000     C                   MOVEL     *BLANKS       WTTV
040900000000     C                   Z-ADD     *ZEROS        WFT1
041000000000     C                   Z-ADD     *ZEROS        WFT2
041100000000     C                   Z-ADD     *ZEROS        WCLI
041200000000     C                   Z-ADD     *ZERO         WCTR
041300000000     C                   MOVEL     *BLANKS       WCTS
041400131002     C                   MOVE      *BLANKS       WCad
041500131002     C                   MOVE      *BLANKS       Wnaz
041600000000     C                   Z-ADD     *ZERO         WCAP
041700000000     C                   Z-ADD     *ZEROS        WDDT
041800000000     C                   Z-ADD     *ZEROS        WSGI
041900000000     C                   Z-ADD     *ZEROS        WVOM
042000000000     C                   Z-ADD     *ZEROS        WPRM
042100000000     C                   Z-ADD     *ZERO         WCMP
042200000000     C                   Z-ADD     *ZERO         WNBC
042300000000     C                   Z-ADD     *ZEROS        WDDC
042400000000     C                   Z-ADD     *ZEROS        WKCC
042500000000     C                   Z-ADD     *ZEROS        WKSC
042600000000     C                   MOVEL     *BLANKS       WRIC
042700000000     C                   Z-ADD     *ZERO         WDDO
042800940719     C                   MOVEL     *BLANKS       WFRA
042900940725     C                   MOVEL     *BLANKS       WIOE
043000940719     C                   Z-ADD     *ZERO         WCCF
043100000000     C*
043200000000     C                   MOVEL     D60OP0        WOP0
043300000000     C                   MOVEL     'O04'         WOP1
043400000000     C                   MOVEL     'TREC61R'     WPGM              8
043500000000     C                   EXSR      LANPGM
043600000000     C*
043700000000     C                   Z-ADD     1             SWMODI
043800000000     C*
043900000000E   1C                   ENDIF
044000000000     C*
044100000000     C                   ENDSR
044200000000     C*------------------------------------------------------------------------*
044300000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
044400000000     C*------------------------------------------------------------------------*
044500000000     C     O05GES        BEGSR
044600000000     C*
044700000000     C* LANCIO LA VISUALIZZAZIONE
044800000000     C                   Z-ADD     VSCVOC        WVOC
044900000000     C                   Z-ADD     VSCPRG        WPRG
045000000000     C                   Z-ADD     VSCDDV        WDDV
045100000000     C                   MOVEL     *BLANKS       WTTV
045200000000     C                   Z-ADD     *ZEROS        WFT1
045300000000     C                   Z-ADD     *ZEROS        WFT2
045400000000     C                   Z-ADD     *ZEROS        WCLI
045500000000     C                   Z-ADD     *ZERO         WCTR
045600000000     C                   MOVE      *BLANKS       WCTS
045700131002     C                   MOVE      *BLANKS       WCad
045800131002     C                   MOVE      *BLANKS       Wnaz
045900000000     C                   Z-ADD     *ZERO         WCAP
046000000000     C                   Z-ADD     *ZEROS        WDDT
046100000000     C                   Z-ADD     *ZEROS        WSGI
046200000000     C                   Z-ADD     *ZEROS        WVOM
046300000000     C                   Z-ADD     *ZEROS        WPRM
046400000000     C                   Z-ADD     *ZERO         WCMP
046500000000     C                   Z-ADD     *ZERO         WNBC
046600000000     C                   Z-ADD     *ZEROS        WDDC
046700000000     C                   Z-ADD     *ZEROS        WKCC
046800000000     C                   Z-ADD     *ZEROS        WKSC
046900000000     C                   MOVEL     *BLANKS       WRIC
047000000000     C                   Z-ADD     *ZERO         WDDO
047100940719     C                   MOVEL     *BLANKS       WFRA
047200940725     C                   MOVEL     *BLANKS       WIOE
047300940719     C                   Z-ADD     *ZERO         WCCF
047400000000     C*
047500000000     C                   MOVEL     D60OP0        WOP0
047600000000     C                   MOVEL     'O05'         WOP1
047700000000     C                   MOVEL     'TREC61R'     WPGM              8
047800000000     C                   EXSR      LANPGM
047900000000     C*
048000000000     C                   ENDSR
048100000000     C*------------------------------------------------------------------------*
048200000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
048300000000     C*------------------------------------------------------------------------*
048400000000     C     O07GES        BEGSR
048500000000     C*
048600000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
048700000000IF  1C     VSCATB        IFNE      'A'
048800000000     C                   MOVEL     '1'           $ERR
048900000000     C                   MOVEL     MSG(13)       $MSG
049000000000     C                   EXSR      CARMSG
049100000000E   1C                   ENDIF
049200000000     C*
049300000000IF  1C     $ERR          IFEQ      '0'
049400000000     C*
049500000000     C* LANCIO IL RIPRISTINO
049600000000     C                   Z-ADD     VSCVOC        WVOC
049700000000     C                   Z-ADD     VSCPRG        WPRG
049800000000     C                   Z-ADD     VSCDDV        WDDV
049900000000     C                   MOVEL     *BLANKS       WTTV
050000000000     C                   Z-ADD     *ZEROS        WFT1
050100000000     C                   Z-ADD     *ZEROS        WFT2
050200000000     C                   Z-ADD     *ZEROS        WCLI
050300000000     C                   Z-ADD     *ZERO         WCTR
050400000000     C                   MOVE      *BLANKS       WCTS
050500131002     C                   MOVE      *BLANKS       WCad
050600131002     C                   MOVE      *BLANKS       Wnaz
050700000000     C                   Z-ADD     *ZERO         WCAP
050800000000     C                   Z-ADD     *ZEROS        WDDT
050900000000     C                   Z-ADD     *ZEROS        WSGI
051000000000     C                   Z-ADD     *ZEROS        WVOM
051100000000     C                   Z-ADD     *ZEROS        WPRM
051200000000     C                   Z-ADD     *ZERO         WCMP
051300000000     C                   Z-ADD     *ZERO         WNBC
051400000000     C                   Z-ADD     *ZEROS        WDDC
051500000000     C                   Z-ADD     *ZEROS        WKCC
051600000000     C                   Z-ADD     *ZEROS        WKSC
051700000000     C                   MOVEL     *BLANKS       WRIC
051800000000     C                   Z-ADD     *ZEROS        WDDO
051900940719     C                   MOVEL     *BLANKS       WFRA
052000940725     C                   MOVEL     *BLANKS       WIOE
052100940719     C                   Z-ADD     *ZEROS        WCCF
052200000000     C*
052300000000     C                   MOVEL     D60OP0        WOP0
052400000000     C                   MOVEL     'O07'         WOP1
052500000000     C                   MOVEL     'TREC61R'     WPGM              8
052600000000     C                   EXSR      LANPGM
052700000000     C*
052800000000     C                   Z-ADD     1             SWMODI
052900000000     C*
053000000000E   1C                   ENDIF
053100000000     C*
053200000000     C                   ENDSR
053300000000     C*------------------------------------------------------------------------*
053400000000     C*  O08GES - GESTIONE OPZIONE 8=GESTIONE TARIFFE VOCE
053500000000     C*------------------------------------------------------------------------*
053600000000     C     O08GES        BEGSR
053700000000     C*
053800000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
053900000000     C                   Z-ADD     VSCVOC        WVOC
054000000000     C                   Z-ADD     VSCPRG        WPRG
054100000000     C                   Z-ADD     *ZEROS        WDDV
054200950529     C*---                 Z-ADDVSCDDV    WDDT             *DATA DECORRENZA TAR.
054300950529     C                   Z-ADD     *ZEROS        WDDT
054400000000     C                   MOVEL     *BLANKS       WTTV
054500000000     C                   Z-ADD     *ZEROS        WFT1
054600000000     C                   Z-ADD     *ZEROS        WFT2
054700000000     C                   Z-ADD     *ZEROS        WCLI
054800000000     C                   Z-ADD     *ZERO         WCTR
054900000000     C                   MOVE      *BLANKS       WCTS
055000131002     C                   MOVE      *BLANKS       WCad
055100131002     C                   MOVE      *BLANKS       Wnaz
055200000000     C                   Z-ADD     *ZERO         WCAP
055300000000     C                   Z-ADD     *ZEROS        WSGI
055400000000     C                   Z-ADD     *ZEROS        WVOM
055500000000     C                   Z-ADD     *ZEROS        WPRM
055600000000     C                   Z-ADD     *ZERO         WCMP
055700000000     C                   Z-ADD     *ZERO         WNBC
055800000000     C                   Z-ADD     *ZEROS        WDDC
055900000000     C                   Z-ADD     *ZEROS        WKCC
056000000000     C                   Z-ADD     *ZEROS        WKSC
056100000000     C                   MOVEL     *BLANKS       WRIC
056200000000     C                   Z-ADD     *ZEROS        WDDO
056300940719     C                   MOVEL     *BLANKS       WFRA
056400940725     C                   MOVEL     *BLANKS       WIOE
056500940719     C                   Z-ADD     *ZEROS        WCCF
056600000000     C*
056700000000     C                   MOVEL     D60OP0        WOP0
056800000000     C                   MOVEL     'O08'         WOP1
056900000000     C                   MOVEL     'TREC63R'     WPGM              8
057000000000     C                   EXSR      LANPGM
057100000000     C*
057200000000     C                   ENDSR
057300000000     C*------------------------------------------------------------------------*
057400000000     C*  O09GES - GESTIONE OPZIONE 9=GESTIONE COMPETENZE
057500000000     C*------------------------------------------------------------------------*
057600000000     C     O09GES        BEGSR
057700000000     C*
057800000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
057900000000     C                   Z-ADD     VSCVOC        WVOC
058000000000     C                   Z-ADD     VSCPRG        WPRG
058100000000     C                   Z-ADD     VSCDDV        WDDV
058200000000     C                   MOVEL     *BLANKS       WTTV
058300000000     C                   Z-ADD     *ZEROS        WFT1
058400000000     C                   Z-ADD     *ZEROS        WFT2
058500000000     C                   Z-ADD     *ZEROS        WCLI
058600000000     C                   Z-ADD     *ZERO         WCTR
058700000000     C                   MOVE      *BLANKS       WCTS
058800131002     C                   MOVE      *BLANKS       WCad
058900131002     C                   MOVE      *BLANKS       Wnaz
059000000000     C                   Z-ADD     *ZERO         WCAP
059100000000     C                   Z-ADD     *ZEROS        WDDT
059200000000     C                   Z-ADD     *ZEROS        WSGI
059300000000     C                   Z-ADD     *ZEROS        WVOM
059400000000     C                   Z-ADD     *ZEROS        WPRM
059500040209     C                   Z-ADD     vsccmp        WCMP
059600000000     C                   Z-ADD     *ZERO         WNBC
059700000000     C                   Z-ADD     *ZEROS        WDDC
059800000000     C                   Z-ADD     *ZEROS        WKCC
059900000000     C                   Z-ADD     *ZEROS        WKSC
060000000000     C                   MOVEL     *BLANKS       WRIC
060100000000     C                   Z-ADD     *ZEROS        WDDO
060200940719     C                   MOVEL     *BLANKS       WFRA
060300940725     C                   MOVEL     *BLANKS       WIOE
060400940719     C                   Z-ADD     *ZEROS        WCCF
060500000000     C*
060600000000     C                   MOVEL     D60OP0        WOP0
060700000000     C                   MOVEL     'O09'         WOP1
060800000000     C                   MOVEL     'TREC72R'     WPGM              8
060900000000     C                   EXSR      LANPGM
061000000000     C*
061100000000     C                   ENDSR
061200000000     C*------------------------------------------------------------------------*
061300000000     C*  O10GES - GESTIONE OPZIONE 10=GESTIONE MAGGIORAZIONI
061400000000     C*------------------------------------------------------------------------*
061500000000     C     O10GES        BEGSR
061600000000     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
061700000000IF  1C     $ERR          IFEQ      '0'
061800091021     C                   Z-ADD     *ZEROS        WVOM
061900091021     C                   Z-ADD     *ZERO         WPRM
062000091021     C                   Z-ADD     *ZEROS        WVOC
062100091021     C                   Z-ADD     *ZERO         WPRG
062200000000     C                   Z-ADD     *ZEROS        WDDV
062300000000     C                   MOVEL     *BLANKS       WTTV
062400000000     C                   Z-ADD     *ZEROS        WFT1
062500000000     C                   Z-ADD     *ZEROS        WFT2
062600000000     C                   Z-ADD     *ZEROS        WCLI
062700000000     C                   Z-ADD     *ZERO         WCTR
062800000000     C                   MOVE      *BLANKS       WCTS
062900131002     C                   MOVE      *BLANKS       WCad
063000131002     C                   MOVE      *BLANKS       Wnaz
063100000000     C                   Z-ADD     *ZERO         WCAP
063200000000     C                   Z-ADD     *ZEROS        WDDT
063300000000     C                   Z-ADD     *ZEROS        WSGI
063400000000     C                   Z-ADD     *ZERO         WCMP
063500000000     C                   Z-ADD     *ZERO         WNBC
063600000000     C                   Z-ADD     *ZEROS        WDDC
063700000000     C                   Z-ADD     *ZEROS        WKCC
063800000000     C                   Z-ADD     *ZEROS        WKSC
063900000000     C                   MOVEL     *BLANKS       WRIC
064000000000     C                   Z-ADD     *ZEROS        WDDO
064100940719     C                   MOVEL     *BLANKS       WFRA
064200940725     C                   MOVEL     *BLANKS       WIOE
064300940719     C                   Z-ADD     *ZEROS        WCCF
064400091021     C* maggiorazioni
064500091021     C     VSCTVO        IFEQ      'M'
064600091021     C                   Z-ADD     VSCVOC        WVOM
064700091021     C                   Z-ADD     VSCPRG        WPRM
064800091021     c                   else
064900091021     C                   Z-ADD     VSCVOC        WVOC
065000091021     C                   Z-ADD     VSCPRG        WPRG
065100091021     c                   end
065200000000     C                   MOVEL     D60OP0        WOP0
065300000000     C                   MOVEL     'O10'         WOP1
065400091021     C     VSCTVO        IFEQ      'M'
065500000000     C                   MOVEL     'TREC66R'     WPGM              8
065600091021     c                   else
065700091021     C                   MOVEL     'TREC68R'     WPGM              8
065800091021     c                   end
065900000000     C                   EXSR      LANPGM
066000000000     C                   ENDIF
066100000000     C*
066200000000     C                   ENDSR
066300130919     C*------------------------------------------------------------------------*
066400130919     C*  O17GES - GESTIONE OPZIONE 17=GESTIONE note conte economico
066500130919     C*------------------------------------------------------------------------*
066600130919     C     O17GES        BEGSR
066700130919     C* LANCIO LA GESTIONE DELLE TARIFFE DELLA SEDE
066800130919     c                   movel(p)  kpjbu         savkpjbu
066900130919     C                   MOVEL     'V'           d69tpana
067000130919     C                   Z-ADD     VSCVOC        d69voc
067100130919     C                   Z-ADD     VSCPRG        d69prg
067200130919     c                   movel(p)  trec69ds      kpjbu
067300130919     C                   call      'TREC69R'
067400130919     c                   parm                    kpjba
067500130919     c                   movel(p)  savkpjbu      kpjbu
067600130919     C*
067700130919     C                   ENDSR
067800000000     C*------------------------------------------------------------------------*
067900000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
068000000000     C*------------------------------------------------------------------------*
068100000000     C     NEWPOS        BEGSR
068200000000     C*
068300000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
068400000000     C*        1 = RICHIESTO POSIZIONAMENTO
068500000000     C                   Z-ADD     0             SWPOSI            7 0
068600000000     C*
068700000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
068800000000     C*            NON OCCORRE RICARICARE IL SUBFILE
068900000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
069000000000     C                   Z-ADD     0             SWMODI            1 0
069100000000     C*
069200000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
069300000000IF  1C     VCCVOC        IFNE      DEPVOC
069400000000     C     VCCPRG        ORNE      DEPPRG
069500000000     C     VCCDDV        ORNE      DEPDDV
069600010111     C     VCCVAL        ORNE      DEPVAL
069700040209     C     VCCcmp        ORNE      DEPcmp
069800130910     C     VCCvod        ORNE      DEPvod
069900000000     C*
070000000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
070100000000     C                   EXSR      CHKPAR
070200000000     C*
070300000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
070400000000IF  2C     $ERR          IFEQ      '0'
070500000000     C                   Z-ADD     1             SWPOSI
070600000000     C                   Z-ADD     VCCVOC        DEPVOC
070700000000     C                   Z-ADD     VCCPRG        DEPPRG
070800000000     C                   Z-ADD     VCCDDV        DEPDDV
070900010111     C                   Z-ADD     VCCVAL        DEPVAL
071000040209     C                   Z-ADD     VCCcmp        DEPcmp
071100130910     C                   movel     VCCvod        DEPvod
071200000000E   2C                   ENDIF
071300000000E   1C                   ENDIF
071400000000     C*
071500000000     C                   ENDSR
071600000000     C*------------------------------------------------------------------------*
071700000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
071800000000     C*------------------------------------------------------------------------*
071900000000     C     CHKPAR        BEGSR
072000000000     C*
072100000000     C* REIMPOSTO INDICATORI DI ERRORE
072200010111     C                   SETOFF                                       808183
072300000000     C*--------------------
072400000000     C* DATA DECORRENZA VOCE
072500000000     C*--------------------
072600000000     C                   Z-ADD     *ZERO         DDVVCC
072700000000IF  1C     VCCDDV        IFGT      *ZEROS
072800000000     C                   Z-ADD     VCCDDV        G02DAT
072900000000     C                   Z-ADD     *ZEROS        G02INV
073000000000     C                   MOVEL     '0'           G02ERR
073100000000     C                   CALL      'XSRDA8'
073200000000     C                   PARM                    WLBDAT
073300000000     C*
073400000000IF  2C     G02ERR        IFEQ      '1'
073500000000     C                   MOVEL     '1'           $ERR
073600000000     C                   MOVEL     MSG(9)        $MSG
073700000000     C                   EXSR      CARMSG
073800000000     C                   SETON                                        81
073900000000X   2C                   ELSE
074000000000     C                   Z-ADD     G02DAT        VCCDDV
074100000000     C                   Z-ADD     G02INV        DDVVCC            8 0
074200000000E   2C                   ENDIF
074300000000E   1C                   ENDIF
074400010111     C*--------------------
074500010111     C* VALIDITA' AL
074600010111     C*--------------------
074700010111     C                   Z-ADD     *ZERO         VALVCC            8 0
074800010111IF  1C     VCCVAL        IFGT      *ZEROS
074900010111     C                   Z-ADD     VCCVAL        G02DAT
075000010111     C                   Z-ADD     *ZEROS        G02INV
075100010111     C                   MOVEL     '0'           G02ERR
075200010111     C                   CALL      'XSRDA8'
075300010111     C                   PARM                    WLBDAT
075400010111     C*
075500010111IF  2C     G02ERR        IFEQ      '1'
075600010111     C                   MOVEL     '1'           $ERR
075700010111     C                   MOVEL     MSG(10)       $MSG
075800010111     C                   EXSR      CARMSG
075900010111     C                   SETON                                        83
076000010111X   2C                   ELSE
076100010111     C                   Z-ADD     G02DAT        VCCVAL
076200010111     C                   Z-ADD     G02INV        VALVCC            8 0
076300010111E   2C                   ENDIF
076400010111E   1C                   ENDIF
076500000000     C*
076600000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
076700000000     C* SUL CAMPO DEL CONTROL
076800000000IF  1C     $ERR          IFEQ      '1'
076900000000     C                   MOVEL     '0'           *IN43
077000000000X   1C                   ELSE
077100000000     C                   MOVEL     '1'           *IN43
077200000000E   1C                   ENDIF
077300000000     C*
077400000000     C                   ENDSR
077500000000     C*------------------------------------------------------------------------*
077600000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD VOCE DI C/E
077700000000     C*------------------------------------------------------------------------*
077800000000     C     SETFIL        BEGSR
077900000000     C*
078000000000     C                   MOVEL     '1'           *IN43
078100000000     C*
078200000000     C* POSIZIONAMENTO ARCHIVIO VOCI DI C/E
078300000000     C                   Z-ADD     VCCVOC        KVTVOC
078400000000     C                   Z-ADD     VCCPRG        KVTPRG
078500000000     C     KEYEVT        SETLL     ECEVT01L                           99
078600000000     C*
078700000000     C  N99              EXSR      LETFIL
078800000000     C*
078900000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
079000000000IF  1C     *IN99         IFEQ      '1'
079100000000     C                   Z-ADD     1             SFLEOF
079200000000E   1C                   ENDIF
079300000000     C*
079400000000     C                   ENDSR
079500000000     C*------------------------------------------------------------------------*
079600000000     C* LETFIL - LEGGE RECORD SUCESSIVO VOCE DI C/E
079700000000     C*------------------------------------------------------------------------*
079800000000     C     LETFIL        BEGSR
079900000000     C*
080000000000     C* LEGGO FINO A:
080100000000     C*     - FINE FILE
080200000000     C*     - TROVATO RECORD VALIDO
080300000000     C                   MOVEL     '0'           WRECOK            1
080400000000DO  1C     *IN99         DOWEQ     *OFF
080500000000     C     WRECOK        ANDEQ     '0'
080600000000     C*
080700000000     C* LETTURA VOCE DI C/E
080800000000     C                   READ      ECEVT01L                               99
080900000000     C*
081000000000     C* FINE FILE  O TABELLA DIVERSA
081100000000IF  2C     *IN99         IFEQ      '1'
081200000000     C                   Z-ADD     1             SFLEOF
081300000000X   2C                   ELSE
081400000000     C                   EXSR      CHKREC
081500000000E   2C                   ENDIF
081600000000     C*
081700000000E   1C                   ENDDO
081800000000     C*
081900000000     C                   ENDSR
082000000000     C*------------------------------------------------------------------------*
082100010111     C* CHKREC - CONTROLLA VALIDITA' RECORD VOCE DI C/E
082200000000     C*------------------------------------------------------------------------*
082300000000     C     CHKREC        BEGSR
082400000000     C*
082500000000     C                   MOVEL     '1'           WRECOK
082600000000     C*
082700000000     C* SE HO CHIESTO LE VOCI IN DECORRENZA A UNA CERTA DATA, ESCLUDO QUELLE CON
082800000000     C* DATA RICHIESTA <> ALLA DATA DECORRENZA
082900000000IF  1C     DDVVCC        IFGT      *ZEROS
083000000000IF  2C     DDVVCC        IFNE      EVTDDV
083100000000     C                   MOVEL     '0'           WRECOK
083200000000E   2C                   ENDIF
083300000000E   1C                   ENDIF
083400010111     C*
083500010111     C* VERIFICO SE RICHIESTO LA DATA DI VALIDITA'
083600010111     C     VALVCC        IFGT      *ZEROS
083700010111     C     EVTDDV        IFLE      VALVCC
083800010111     C     EVTDSV        ANDGE     VALVCC
083900010111     C                   ELSE
084000010111     C                   MOVEL     '0'           WRECOK
084100010111     C                   ENDIF
084200010111     C                   ENDIF
084300130910     C* VERIFICO SE RICHIESTa LA descrizione
084400130910     c                   if        vccvod <> ' '
084500130910     c                   z-add     0             pos               2 0
084600130910     C                   eval      pos = %scan(%trim(vccvod): evtvod)
084700130910     c                   if        pos =  0
084800130910     C                   MOVEL     '0'           WRECOK
084900130910     C                   ENDIF
085000130910     C                   ENDIF
085100000000     C*
085200040209     C* VERIFICO LA SELEZIONE PER COMPETENZA
085300040209IF  1C     vcccmp        IFGT      *ZEROS
085400040209IF  2C     vcccmp        IFNE      EVTcmp
085500040209     C                   MOVEL     '0'           WRECOK
085600040209E   2C                   ENDIF
085700040209E   1C                   ENDIF
085800000000     C*
085900000000     C                   ENDSR
086000000000     C*------------------------------------------------------------------------*
086100000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
086200000000     C*------------------------------------------------------------------------*
086300000000     C     IRESFL        BEGSR
086400000000     C*
086500000000IF  1C     EVTATB        IFNE      *BLANKS
086600000000     C                   MOVEL     'A'           VSCATB
086700000000X   1C                   ELSE
086800000000     C                   MOVEL     ' '           VSCATB
086900000000E   1C                   ENDIF
087000100326     C* se voce di isolamento lo segnalo nel sfl
087100100325     c                   clear                   vscsal
087200100422     c                   if        evtvrv = '26'
087300100326     c                   eval      vscsal = 'I'
087400100325     c                   end
087500100325     c*
087600000000     C                   Z-ADD     EVTVOC        VSCVOC
087700000000     C                   Z-ADD     EVTPRG        VSCPRG
087800000000     C                   MOVEL     EVTVOD        VSCVOD
087900000000     C                   Z-ADD     EVTDDV        G02INV
088000000000     C                   Z-ADD     *ZEROS        G02DAT
088100000000     C                   MOVEL     '3'           G02ERR
088200000000     C                   CALL      'XSRDA8'
088300000000     C                   PARM                    WLBDAT
088400000000     C                   Z-ADD     G02DAT        VSCDDV
088500000000     C                   Z-ADD     EVTDSV        G02INV
088600000000     C                   Z-ADD     *ZEROS        G02DAT
088700000000     C                   MOVEL     '3'           G02ERR
088800000000     C                   CALL      'XSRDA8'
088900000000     C                   PARM                    WLBDAT
089000000000     C                   Z-ADD     G02DAT        VSCDSV
089100000000     C                   MOVE      EVTTVO        VSCTVO
089200020614     C                   Z-ADD     EVTCMP        VSCCMP
089300021206     C                   MOVE      EVTCOP        VSCCOP
089400000000     C*
089500000000     C                   ENDSR
089600000000     C*------------------------------------------------------------------------*
089700000000     C* LANPGM - LANCIA UN PROGRAMMA
089800000000     C*------------------------------------------------------------------------*
089900000000     C     LANPGM        BEGSR
090000000000     C*--------------------
090100000000     C* PARAMETRI INFORMATIVI
090200000000     C*--------------------
090300000000     C                   MOVEL     WOP0          D60OP0
090400000000     C                   MOVEL     WOP1          D60OP1
090500000000     C                   MOVEL     '0'           D60F03
090600000000     C                   MOVEL     '0'           D60ERR
090700000000     C                   MOVEL     *BLANKS       D60MSG
090800000000     C*--------------------
090900000000     C* PARAMETRI
091000000000     C*--------------------
091100000000     C                   Z-ADD     WVOC          D60VOC
091200000000     C                   Z-ADD     WPRG          D60PRG
091300000000     C                   Z-ADD     WDDV          D60DDV
091400000000     C                   MOVEL     WTTV          D60TTV
091500000000     C                   Z-ADD     WFT1          D60FT1
091600000000     C                   Z-ADD     WFT2          D60FT2
091700000000     C                   Z-ADD     WCLI          D60CLI
091800000000     C                   Z-ADD     WCTR          D60CTR
091900000000     C                   MOVE      WCTS          D60CTS
092000131002     C                   MOVEl     WCad          d60cad
092100131002     C                   MOVEl     Wnaz          d60naz
092200000000     C                   Z-ADD     WCAP          D60CAP
092300000000     C                   Z-ADD     WDDT          D60DDT
092400000000     C                   Z-ADD     WSGI          D60SGI
092500000000     C                   Z-ADD     WVOM          D60VOM
092600000000     C                   Z-ADD     WPRM          D60PRM
092700000000     C                   Z-ADD     WCMP          D60CMP
092800000000     C                   Z-ADD     WNBC          D60NBC
092900000000     C                   Z-ADD     WDDC          D60DDC
093000000000     C                   Z-ADD     WKCC          D60KCC
093100000000     C                   Z-ADD     WKSC          D60KSC
093200000000     C                   MOVEL     WRIC          D60RIC
093300000000     C                   Z-ADD     WDDO          D60DDO
093400940719     C                   MOVEL     WRIC          D60FRA
093500940725     C                   MOVEL     WIOE          D60IOE
093600940719     C                   Z-ADD     WDDO          D60CCF
093700000000     C* LANCIO
093800000000     C                   MOVEL     DS60          KPJBU
093900000000     C                   CALL      WPGM                                 99
094000000000     C                   PARM                    KPJBA
094100000000     C                   MOVEL     KPJBU         DS60
094200000000     C*
094300000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
094400000000IF  1C     D60F03        IFEQ      '1'
094500000000     C                   MOVEL     '1'           $F03
094600000000     C                   MOVEL     '1'           $ERR
094700000000     C                   MOVEL     '1'           $FINE
094800000000     C*
094900000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
095000000000X   1C                   ELSE
095100000000     C                   MOVEL     D60MSG        $MSG
095200000000     C                   EXSR      CARMSG
095300000000E   1C                   ENDIF
095400000000     C*
095500000000     C                   ENDSR
095600020923     C*-------------------------------------------------------------------------
095700020923     C* REPERISCE I DATI UTENTE
095800020923     C*-------------------------------------------------------------------------
095900020923     C     REPDATIUTE    BEGSR
096000020923     C*
096100020923     C* INIZIALIZZA VARIABILI DI WRK
096200020923     C                   CLEAR                   TIBS34DS
096300020923     C                   CLEAR                   AZUTEDS
096400020923     C                   CLEAR                   DDATIUTE
096500020923     C*
096600020923     C     *DTAARA       DEFINE    §azute        azuteds
096700020923     C     *DTAARA       DEFINE    §datiute      ddatiute
096800020923     C                   IN(E)     *DTAARA
096900020923if  1C                   IF        %Error
097000020923     c                   EVAL      I34Tla = 'L'
097100020923     C                   CALL      'TIBS34R'
097200020923     C                   PARM                    Tibs34Ds
097300020923     C                   IN        *DTAARA
097400020923e   1C                   ENDIF
097500020923     C*
097600020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
097700020923     C                   MOVEL(P)  RSUT          DSFIRS           20
097800020923     C*
097900020923     C                   ENDSR
098000000000     C*------------------------------------------------------------------------*
098100000000     C*  *INZSR - ROUTINE INIZIALE
098200000000     C*------------------------------------------------------------------------*
098300000000     C     *INZSR        BEGSR
098400000000     C*
098500000000     C* RICEVIMENTO PARAMETRI
098600000000     C     *ENTRY        PLIST
098700000000     C                   PARM                    KPJBA
098800000000     C                   MOVEL     KPJBU         DS60
098900010111     C*
099000010111     C* CALCOLA LA DATA CORRENTE
099100010111     C                   TIME                    WN14             14 0
099200010111     C                   MOVE      WN14          WN8               8 0
099300010111     C                   Z-ADD     WN8           G02DAT
099400010111     C                   Z-ADD     *ZEROS        G02INV
099500010111     C                   MOVEL     '0'           G02ERR
099600010111     C                   CALL      'XSRDA8'
099700010111     C                   PARM                    WLBDAT
099800010111     C                   Z-ADD     G02INV        DATCOR            8 0
099900000000     C*
100000000000     C* DEFINIZIONE KLIST
100100000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
100200000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
100300100325     C     *LIKE         DEFINE    EVTVOC        Kvocsal
100400000000     C*
100500000000     C* DEFINIZIONE DEPOSITI
100600000000     C     *LIKE         DEFINE    VCCVOC        DEPVOC
100700000000     C     *LIKE         DEFINE    VCCPRG        DEPPRG
100800000000     C     *LIKE         DEFINE    VCCDDV        DEPDDV
100900010111     C     *LIKE         DEFINE    VCCVAL        DEPVAL
101000040209     C     *LIKE         DEFINE    VCCcmp        DEPcmp
101100130910     C     *LIKE         DEFINE    VCCvod        DEPvod
101200000000     C*
101300000000     C* DEFINIZIONE CAMPI DI COMODO LANCIO PROGRAMMA
101400000000     C     *LIKE         DEFINE    D60OP0        WOP0
101500000000     C     *LIKE         DEFINE    D60OP1        WOP1
101600000000     C     *LIKE         DEFINE    D60F03        WF03
101700000000     C     *LIKE         DEFINE    D60ERR        WERR
101800000000     C     *LIKE         DEFINE    D60MSG        WMSG
101900000000     C     *LIKE         DEFINE    D60VOC        WVOC
102000000000     C     *LIKE         DEFINE    D60PRG        WPRG
102100000000     C     *LIKE         DEFINE    D60DDV        WDDV
102200000000     C     *LIKE         DEFINE    D60TTV        WTTV
102300000000     C     *LIKE         DEFINE    D60FT1        WFT1
102400000000     C     *LIKE         DEFINE    D60FT2        WFT2
102500000000     C     *LIKE         DEFINE    D60CLI        WCLI
102600000000     C     *LIKE         DEFINE    D60CTR        WCTR
102700000000     C     *LIKE         DEFINE    D60CTS        WCTS
102800000000     C     *LIKE         DEFINE    D60CAP        WCAP
102900131002     C     *LIKE         DEFINE    D60CAd        WCAd
103000131002     C     *LIKE         DEFINE    D60naz        Wnaz
103100000000     C     *LIKE         DEFINE    D60DDT        WDDT
103200000000     C     *LIKE         DEFINE    D60SGI        WSGI
103300000000     C     *LIKE         DEFINE    D60VOM        WVOM
103400000000     C     *LIKE         DEFINE    D60PRM        WPRM
103500000000     C     *LIKE         DEFINE    D60CMP        WCMP
103600000000     C     *LIKE         DEFINE    D60NBC        WNBC
103700000000     C     *LIKE         DEFINE    D60DDC        WDDC
103800000000     C     *LIKE         DEFINE    D60KCC        WKCC
103900000000     C     *LIKE         DEFINE    D60KSC        WKSC
104000000000     C     *LIKE         DEFINE    D60RIC        WRIC
104100000000     C     *LIKE         DEFINE    D60DDO        WDDO
104200940719     C     *LIKE         DEFINE    D60FRA        WFRA
104300940725     C     *LIKE         DEFINE    D60IOE        WIOE
104400940719     C     *LIKE         DEFINE    D60CCF        WCCF
104500000000     C*
104600000000     C* CHIAVE LETTURA ECEVT01L - COMPLETA
104700000000     C     KEYEVT        KLIST
104800000000     C                   KFLD                    KVTVOC
104900000000     C                   KFLD                    KVTPRG
105000000000     C*
105100000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
105200000000     C                   MOVE      D60OP0        INTGES            2
105300000000IF  1C     INTGES        IFEQ      '02'
105400000000     C                   MOVEL     'G'           $TGES             1
105500000000     C                   MOVEL     DESGES        VTDOPE
105600000000X   1C                   ELSE
105700000000     C                   MOVEL     'I'           $TGES             1
105800000000     C                   MOVEL     DESINT        VTDOPE
105900000000E   1C                   ENDIF
106000000000     C*
106100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
106200000000     C                   EXSR      INZSTD
106300000000     C*
106400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
106500000000     C                   EXSR      INZSFL
106600000000     C*
106700000000     C* IMPOSTO CAMPI TESTATA VIDEO
106800000000     C                   MOVEL     DSFIRS        VTDRSU
106900000000     C*
107000000000     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
107100000000     C                   Z-ADD     *ZEROS        VCCVOC
107200000000     C                   Z-ADD     *ZEROS        VCCPRG
107300000000     C                   Z-ADD     *ZEROS        VCCDDV
107400010111     C                   Z-ADD     *ZEROS        VCCVAL
107500040209     C                   Z-ADD     *ZEROS        VCCcmp
107600130910     C                   clear                   VCCvod
107700040209     c* se ricevuti parametri li imposto
107800040209     c                   if        d60cmp <> 0
107900040209     c                   z-add     d60cmp        vcccmp
108000040209     c                   end
108100000000     C*
108200000000     C* EMETTO VIDEATA SUBFILE
108300000000     C                   EXSR      CARVS
108400000000     C*
108500000000     C                   ENDSR
108600000000     C*------------------------------------------------------------------------*
108700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108900000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
109000000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
109100000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
109200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
109300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
109400000000     C*------------------------------------------------------------------------*
109500000000     C* INZSTD - ROUTINE INIZIALE STANDARD
109600000000     C*------------------------------------------------------------------------*
109700000000     C     INZSTD        BEGSR
109800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
109900000000     C                   CALL      'XKEYS'
110000000000     C                   PARM                    DSKEYS
110100000000     C                   MOVEA     KEYRIG        KEYS
110200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
110300000000     C     XMSGPA        PLIST
110400000000     C                   PARM                    DSMSMS
110500000000     C                   PARM                    DSMSPG
110600000000     C                   PARM                    DSMSKE
110700000000     C                   PARM                    DSMSER
110800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
110900000000     C                   MOVEL     'TREC60R'     DSMSPG
111000000000     C                   MOVEL     'TREC60R'     VMCQUE
111100000000     C                   MOVEL     *BLANKS       VMCKEY
111200000000     C                   MOVEL     *BLANKS       DSMSMS
111300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
111400000000     C                   MOVEL     '0'           $ERR              1
111500000000     C                   MOVEL     '0'           $F03              1
111600000000     C                   MOVEL     *BLANKS       $GES              2
111700000000     C                   MOVEL     *BLANKS       $MSG             80
111800000000     C                   MOVEL     '0'           $FINE             1
111900000000     C                   MOVEL     '0'           $TESVD            1
112000000000     C* LANCIO ROUTINE PER VERIFICARE SE
112100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
112200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
112300020923     C                   EXSR      REPDATIUTE
112400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
112500000000     C                   BITOFF    '01234567'    POSMI1            1
112600000000     C                   BITOFF    '01234567'    POSMI2            1
112700000000     C                   BITOFF    '01234567'    POSMA1            1
112800000000     C                   BITOFF    '01234567'    POSMA2            1
112900000000     C                   BITON     '34'          POSMI1            1
113000000000     C                   BITON     '7'           POSMI2            1
113100000000     C                   BITON     '34'          POSMA1            1
113200000000     C                   BITON     '0'           POSMA2            1
113300000000     C                   MOVE      POSMI2        POSMIN            2
113400000000     C                   MOVEL     POSMI1        POSMIN
113500000000     C                   MOVE      POSMA2        POSMAX            2
113600000000     C                   MOVEL     POSMA1        POSMAX
113700000000     C                   ENDSR
113800000000     C*------------------------------------------------------------------------*
113900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
114000000000     C*------------------------------------------------------------------------*
114100000000     C     FUNSTD        BEGSR
114200000000     C                   SELECT
114300000000     C     KEY           WHENEQ    F3
114400000000     C                   EXSR      F03STD
114500000000     C     KEY           WHENEQ    F12
114600000000     C                   EXSR      F12STD
114700000000     C     KEY           WHENEQ    F24
114800000000     C                   EXSR      F24STD
114900000000     C                   OTHER
115000000000     C                   MOVEL     MSG(2)        $MSG
115100000000     C                   EXSR      CARMSG
115200000000     C                   ENDSL
115300000000     C                   ENDSR
115400000000     C*------------------------------------------------------------------------*
115500000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
115600000000     C*------------------------------------------------------------------------*
115700000000     C     F03STD        BEGSR
115800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
115900000000     C                   MOVEL     '1'           $F03
116000000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
116100000000     C                   MOVEL     '1'           $FINE
116200000000     C                   ENDSR
116300000000     C*------------------------------------------------------------------------*
116400000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
116500000000     C*------------------------------------------------------------------------*
116600000000     C     F12STD        BEGSR
116700000000     C* IMPOSTO FLAG DI FIN PGM
116800000000     C                   MOVEL     '1'           $FINE
116900000000     C                   ENDSR
117000000000     C*------------------------------------------------------------------------*
117100000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
117200000000     C*------------------------------------------------------------------------*
117300000000     C     F24STD        BEGSR
117400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
117500000000     C                   EXSR      OUTFUN
117600000000     C                   ENDSR
117700000000     C*------------------------------------------------------------------------*
117800000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
117900000000     C*------------------------------------------------------------------------*
118000000000     C     INZSFL        BEGSR
118100000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
118200000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
118300000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
118400000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
118500000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
118600000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
118700000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
118800000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
118900000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
119000000000     C                   Z-ADD     12            SFLPAG
119100000000     C                   ENDSR
119200000000     C*------------------------------------------------------------------------*
119300000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
119400000000     C*------------------------------------------------------------------------*
119500000000     C     FUNSFL        BEGSR
119600000000     C                   SELECT
119700000000     C     KEY           WHENEQ    ENTER
119800000000     C                   EXSR      ENTSFL
119900000000     C     KEY           WHENEQ    F3
120000000000     C                   EXSR      F03STD
120100000000     C     KEY           WHENEQ    F5
120200000000     C                   EXSR      F05STD
120300000000     C     KEY           WHENEQ    F12
120400000000     C                   EXSR      F12STD
120500000000     C     KEY           WHENEQ    F17
120600000000     C                   EXSR      F17STD
120700000000     C     KEY           WHENEQ    F18
120800000000     C                   EXSR      F18STD
120900000000     C     KEY           WHENEQ    F23
121000000000     C                   EXSR      F23STD
121100000000     C     KEY           WHENEQ    F24
121200000000     C                   EXSR      F24STD
121300000000     C     KEY           WHENEQ    ROLLUP
121400000000     C                   EXSR      RUPSTD
121500000000     C     KEY           WHENEQ    ROLLDN
121600000000     C                   EXSR      RDNSTD
121700000000     C                   OTHER
121800000000     C                   MOVEL     MSG(2)        $MSG
121900000000     C                   EXSR      CARMSG
122000000000     C                   ENDSL
122100000000     C                   ENDSR
122200000000     C*------------------------------------------------------------------------*
122300000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
122400000000     C*------------------------------------------------------------------------*
122500000000     C     F05STD        BEGSR
122600000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
122700000000     C                   EXSR      POSSFL
122800000000     C                   ENDSR
122900000000     C*------------------------------------------------------------------------*
123000000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
123100000000     C*------------------------------------------------------------------------*
123200000000     C     F17STD        BEGSR
123300000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
123400000000     C                   Z-ADD     1             VCCNBR
123500000000     C                   ENDSR
123600000000     C*------------------------------------------------------------------------*
123700000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
123800000000     C*------------------------------------------------------------------------*
123900000000     C     F18STD        BEGSR
124000000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
124100000000     C                   Z-ADD     SFLULT        VCCNBR
124200000000     C                   ENDSR
124300000000     C*------------------------------------------------------------------------*
124400000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
124500000000     C*------------------------------------------------------------------------*
124600000000     C     F23STD        BEGSR
124700000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
124800000000     C                   EXSR      OUTOPZ
124900000000     C                   ENDSR
125000000000     C*------------------------------------------------------------------------*
125100000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
125200000000     C*------------------------------------------------------------------------*
125300000000     C     RUPSTD        BEGSR
125400000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
125500000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
125600000000     C     CURSOR        IFGE      POSMIN
125700000000     C     CURSOR        ANDLE     POSMAX
125800000000     C                   ELSE
125900000000     C                   EXSR      CARSFL
126000000000     C                   ENDIF
126100000000     C                   ENDSR
126200000000     C*------------------------------------------------------------------------*
126300000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
126400000000     C*------------------------------------------------------------------------*
126500000000     C     RDNSTD        BEGSR
126600000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
126700000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
126800000000     C     CURSOR        IFGE      POSMIN
126900000000     C     CURSOR        ANDLE     POSMAX
127000000000     C                   ELSE
127100000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
127200000000     C                   MOVEL     '1'           $ERR
127300000000     C                   MOVEL     MSG(5)        $MSG
127400000000     C                   EXSR      CARMSG
127500000000     C                   ENDIF
127600000000     C                   ENDSR
127700000000     C*------------------------------------------------------------------------*
127800000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
127900000000     C*------------------------------------------------------------------------*
128000000000     C     ENTSFL        BEGSR
128100000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
128200000000     C                   EXSR      NEWPOS
128300000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
128400000000     C     $ERR          IFEQ      '0'
128500000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
128600000000     C     SWPOSI        IFNE      0
128700000000     C                   EXSR      POSSFL
128800000000     C                   ELSE
128900000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
129000000000     C     SFLULT        IFNE      0
129100000000     C                   EXSR      OPZSFL
129200000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
129300000000     C     SWMODI        IFNE      0
129400000000     C                   EXSR      POSSFL
129500000000     C                   ENDIF
129600000000     C                   ENDIF
129700000000     C                   ENDIF
129800000000     C                   ENDIF
129900000000     C                   ENDSR
130000000000     C*------------------------------------------------------------------------*
130100000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
130200000000     C*------------------------------------------------------------------------*
130300000000     C     POSSFL        BEGSR
130400000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
130500000000     C                   EXSR      CLRSFL
130600000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
130700000000     C                   EXSR      SETFIL
130800000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
130900000000     C     SFLEOF        IFEQ      0
131000000000     C                   EXSR      CARSFL
131100000000     C                   ENDIF
131200000000     C                   ENDSR
131300000000     C*------------------------------------------------------------------------*
131400000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
131500000000     C*------------------------------------------------------------------------*
131600000000     C     OPZSFL        BEGSR
131700000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
131800000000     C                   MOVEL     'S'           OPZOK             1
131900000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
132000000000     C                   Z-ADD     0             SFLVAR            5 0
132100000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
132200000000     C                   Z-ADD     0             SFLERR
132300000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
132400000000     C                   Z-ADD     SFLULT        WFLULT
132500000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
132600000000     C                   READC     EC60DVS                                49
132700000000     C     *IN49         DOWEQ     '0'
132800941121     C     $ERR          ANDEQ     '0'
132900000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
133000000000     C                   ADD       1             SFLVAR
133100000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
133200000000     C     OPZOK         IFEQ      'S'
133300000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
133400000000     C     VSCOPZ        IFNE      *BLANKS
133500000000     C                   EXSR      CONOPZ
133600000000     C     OPZOK         IFEQ      'S'
133700941121     C     $ERR          ANDEQ     '0'
133800000000     C                   EXSR      GESOPZ
133900000000     C                   ENDIF
134000000000     C                   ENDIF
134100000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
134200000000     C*            2. PULIZIA SCELTA IN SFL
134300000000     C     OPZOK         IFEQ      'S'
134400000000     C                   MOVEL     '0'           *IN41
134500000000     C                   MOVEL     *BLANKS       VSCOPZ
134600000000     C                   ELSE
134700000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
134800000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
134900000000     C                   MOVEL     '1'           *IN41
135000000000     C                   Z-ADD     SFLULT        SFLERR
135100000000     C                   ENDIF
135200000000     C                   ELSE
135300000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
135400000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
135500000000     C                   MOVEL     '1'           *IN41
135600000000     C                   ENDIF
135700000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
135800000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
135900000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
136000000000     C     OPZOK         IFNE      'S'
136100000000     C                   Z-ADD     SFLERR        VCCNBR
136200000000     C                   ELSE
136300000000     C                   Z-ADD     SFLULT        VCCNBR
136400000000     C                   ENDIF
136500000000     C* AGGIORNO RECORD SFL
136600000000     C                   UPDATE    EC60DVS
136700941121     C     $ERR          IFEQ      '0'
136800000000     C                   READC     EC60DVS                                49
136900941121     C                   ENDIF
137000000000     C                   ENDDO
137100000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
137200000000     C                   Z-ADD     WFLULT        SFLULT
137300000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
137400000000     C     SFLVAR        IFEQ      0
137500000000     C                   MOVEL     '1'           $FINE
137600000000     C                   ENDIF
137700000000     C                   ENDSR
137800000000     C*------------------------------------------------------------------------*
137900000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
138000000000     C*------------------------------------------------------------------------*
138100000000     C     CLRSFL        BEGSR
138200000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
138300000000     C* (0=NON FINE FILE  1=FINE FILE)
138400000000     C                   Z-ADD     0             SFLEOF
138500000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
138600000000     C                   Z-ADD     0             SFLULT
138700000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
138800000000     C                   MOVEL     '1'           *IN40
138900000000     C                   WRITE     EC60DVC
139000000000     C                   MOVEL     '0'           *IN40
139100000000     C                   ENDSR
139200000000     C*------------------------------------------------------------------------*
139300000000     C* CARSFL - CARICAMENTO SFL
139400000000     C*------------------------------------------------------------------------*
139500000000     C     CARSFL        BEGSR
139600000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
139700000000     C     SFLULT        IFEQ      0
139800000000     C                   EXSR      SETFIL
139900000000     C                   ENDIF
140000000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
140100000000     C                   Z-ADD     SFLULT        WFLULT
140200000000     C* CICLO:
140300000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
140400000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
140500000000     C                   Z-ADD     1             SFLRIG
140600000000     C     SFLRIG        DOWLE     SFLPAG
140700000000     C     SFLEOF        ANDEQ     0
140800000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
140900000000     C                   MOVEL     *BLANKS       VSCOPZ
141000000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
141100000000     C                   EXSR      IRESFL
141200000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
141300000000     C                   ADD       1             SFLULT
141400000000     C* SCRITTURA RECORD SFL
141500000000     C                   WRITE     EC60DVS
141600000000     C                   ADD       1             SFLRIG
141700000000     C* LETTURA FILE DA CARICARE
141800000000     C                   EXSR      LETFIL
141900000000     C                   ENDDO
142000000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
142100000000     C     WFLULT        IFNE      SFLULT
142200000000     C                   Z-ADD     WFLULT        VCCNBR
142300000000     C                   ADD       1             VCCNBR
142400000000     C                   ENDIF
142500000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
142600000000     C     SFLEOF        IFEQ      1
142700000000     C     SFLULT        ANDGT     *ZEROS
142800000000     C                   MOVEL     '1'           $ERR
142900000000     C                   MOVEL     MSG(6)        $MSG
143000000000     C                   EXSR      CARMSG
143100000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
143200000000     C     SFLULT        CHAIN     EC60DVS                            99
143300000000     C     *IN99         IFEQ      '0'
143400000000     C                   SETON                                        45
143500000000     C                   UPDATE    EC60DVS
143600000000     C                   SETOFF                                       45
143700000000     C                   ENDIF
143800000000     C                   ENDIF
143900000000     C                   ENDSR
144000000000     C*------------------------------------------------------------------------*
144100000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
144200000000     C*------------------------------------------------------------------------*
144300000000     C     OUTSFL        BEGSR
144400000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
144500000000     C     MSGULT        IFNE      *ZEROS
144600000000     C                   EXSR      OUTMSG
144700000000     C                   ENDIF
144800000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
144900000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
145000000000     C     SFLEOF        IFEQ      0
145100000000     C                   MOVEL     '0'           *IN42
145200000000     C                   ELSE
145300000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
145400000000     C                   MOVEL     '1'           *IN42
145500000000     C                   ENDIF
145600000000     C* EMISSIONE SUBFILE
145700000000     C     SFLULT        IFLE      0
145800000000     C*  --> CASO DI SFL VUOTO
145900000000     C                   MOVEL     '0'           *IN43
146000000000     C                   WRITE     EC60DVV
146100000000     C                   WRITE     EC60DVC
146200000000     C                   READ      EC60DVC                                99
146300000000     C                   ELSE
146400000000     C*  --> CASO DI SFL CON RECORDS
146500000000     C                   WRITE     EC60DVC
146600000000     C                   READ      EC60DVC                                99
146700000000     C                   ENDIF
146800000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
146900000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
147000000000     C                   Z-ADD     FRTSFL        VCCNBR
147100000000     C                   ENDSR
147200000000     C*------------------------------------------------------------------------*
147300000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
147400000000     C*          CODA MESSAGGI DEL PGM
147500000000     C*------------------------------------------------------------------------*
147600000000     C     CARMSG        BEGSR
147700000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
147800000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
147900000000     C     MSGULT        IFEQ      0
148000000000     C                   MOVEL     '1'           *IN40
148100000000     C                   WRITE     EC60DVK
148200000000     C                   MOVEL     '0'           *IN40
148300000000     C                   ENDIF
148400000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
148500000000     C                   MOVEL     $MSG          DSMSMS
148600000000     C                   CALL      'XMSG'        XMSGPA
148700000000     C     DSMSER        IFEQ      0
148800000000     C                   MOVEL     DSMSKE        VMCKEY
148900000000     C                   ENDIF
149000000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
149100000000     C                   ADD       1             MSGULT            4 0
149200000000     C                   WRITE     EC60DVM
149300000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
149400000000     C                   MOVEL     *BLANKS       $MSG
149500000000     C                   ENDSR
149600000000     C*------------------------------------------------------------------------*
149700000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
149800000000     C*------------------------------------------------------------------------*
149900000000     C     OUTMSG        BEGSR
150000000000     C* VISUALIZZAZIONE MESSAGGIO
150100000000     C                   MOVEL     '1'           *IN44
150200000000     C                   WRITE     EC60DVK
150300000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
150400000000     C                   Z-ADD     *ZEROS        MSGULT
150500000000     C                   ENDSR
150600000000     C*------------------------------------------------------------------------*
150700000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
150800000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
150900000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
151000000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
151100000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
151200000000     C*------------------------------------------------------------------------*
151300000000     C     CARFUN        BEGSR
151400000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
151500000000     C     1             DO        31            I                 5 0
151600000000     C                   MOVEL     FUN(I)        FUNRIG
151700000000     C                   MOVEL     FUNFUN        FFU(I)
151800000000     C                   MOVEL     FUNDES        FDE(I)
151900020923     C     DUTLPO        IFEQ      'S'
152000000000     C     $TGES         IFEQ      'G'
152100000000     C                   MOVEL     FUNASG        FAT(I)
152200000000     C                   ELSE
152300000000     C                   MOVEL     FUNASI        FAT(I)
152400000000     C                   ENDIF
152500000000     C                   ELSE
152600000000     C     $TGES         IFEQ      'G'
152700000000     C                   MOVEL     FUNAFG        FAT(I)
152800000000     C                   ELSE
152900000000     C                   MOVEL     FUNAFI        FAT(I)
153000000000     C                   ENDIF
153100000000     C                   ENDIF
153200000000     C     FAT(I)        IFEQ      'E'
153300000000     C                   MOVEL     USER          WEDP              3
153400000000     C     WEDP          IFEQ      'EDP'
153500000000     C                   MOVEL     'S'           FAT(I)
153600000000     C                   ELSE
153700000000     C                   MOVEL     'N'           FAT(I)
153800000000     C                   ENDIF
153900000000     C                   ENDIF
154000000000     C                   ENDDO
154100000000     C                   ENDSR
154200000000     C*------------------------------------------------------------------------*
154300000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
154400000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
154500000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
154600000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
154700000000     C*------------------------------------------------------------------------*
154800000000     C     OUTFUN        BEGSR
154900000000     C     1             DO        8             I
155000000000     C                   MOVEL     *BLANKS       FUV(I)
155100000000     C                   ENDDO
155200000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
155300000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
155400000000     C     1             DO        31            I
155500000000     C     FAT(I)        IFEQ      'S'
155600000000     C     FDE(I)        ANDNE     *BLANKS
155700000000     C                   ADD       1             FUNNAT
155800000000     C                   ENDIF
155900000000     C                   ENDDO
156000000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
156100000000     C     FUNNAT        IFGT      8
156200000000     C                   MOVEL     'S'           FAT(24)
156300000000     C                   ENDIF
156400000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
156500000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
156600000000     C                   Z-ADD     8             FUNVIS            5 0
156700000000     C     FAT(23)       IFEQ      'S'
156800000000     C                   SUB       1             FUNVIS
156900000000     C                   ENDIF
157000000000     C     FAT(24)       IFEQ      'S'
157100000000     C                   SUB       1             FUNVIS
157200000000     C                   ENDIF
157300000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
157400000000     C                   Z-ADD     *ZEROS        W                 5 0
157500000000     C                   ADD       1             FUNULT
157600000000     C     FUNULT        IFGT      31
157700000000     C                   Z-ADD     1             FUNULT
157800000000     C                   ENDIF
157900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
158000000000     C     FUNULT        DO        31            I
158100000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
158200000000     C     W             IFLT      FUNVIS
158300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
158400000000     C     FAT(I)        IFEQ      'S'
158500000000     C     FDE(I)        ANDNE     *BLANKS
158600000000     C     I             IFEQ      23
158700000000     C     I             OREQ      24
158800000000     C                   ELSE
158900000000     C                   ADD       1             W
159000000000     C                   MOVEL     FDE(I)        FUV(W)
159100000000     C                   ENDIF
159200000000     C                   ENDIF
159300000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
159400000000     C                   Z-ADD     I             FUNULT
159500000000     C                   ENDIF
159600000000     C                   ENDDO
159700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
159800000000     C                   MOVEL     FUV(1)        VFD001
159900000000     C                   MOVEL     FUV(2)        VFD002
160000000000     C                   MOVEL     FUV(3)        VFD003
160100000000     C                   MOVEL     FUV(4)        VFD004
160200000000     C                   MOVEL     FUV(5)        VFD005
160300000000     C                   MOVEL     FUV(6)        VFD006
160400000000     C     FAT(23)       IFEQ      'N'
160500000000     C     FAT(24)       ANDEQ     'N'
160600000000     C                   MOVEL     FUV(7)        VFD007
160700000000     C                   MOVEL     FUV(8)        VFD008
160800000000     C                   ELSE
160900000000     C     FAT(23)       IFEQ      'S'
161000000000     C     FAT(24)       ANDEQ     'S'
161100000000     C                   MOVEL     FDE(23)       VFD007
161200000000     C                   MOVEL     FDE(24)       VFD008
161300000000     C                   ELSE
161400000000     C                   MOVEL     FUV(7)        VFD007
161500000000     C     FAT(23)       IFEQ      'S'
161600000000     C                   MOVEL     FDE(23)       VFD008
161700000000     C                   ELSE
161800000000     C                   MOVEL     FDE(24)       VFD008
161900000000     C                   ENDIF
162000000000     C                   ENDIF
162100000000     C                   ENDIF
162200000000     C                   MOVEL     '1'           $TESVD
162300000000     C                   ENDSR
162400000000     C*------------------------------------------------------------------------*
162500000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
162600000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
162700000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
162800000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
162900000000     C*------------------------------------------------------------------------*
163000000000     C     CONFUN        BEGSR
163100000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
163200000000     C     1             DO        31            I
163300000000     C     KEY           IFEQ      KEYS(I)
163400000000     C* CONTROLLO SE IL TASTO E' ATTIVO
163500000000     C     FAT(I)        IFNE      'S'
163600000000     C                   MOVEL     MSG(1)        $MSG
163700000000     C                   EXSR      CARMSG
163800000000     C                   MOVEL     '1'           $ERR
163900000000     C                   ENDIF
164000000000     C                   ENDIF
164100000000     C                   ENDDO
164200000000     C                   ENDSR
164300000000     C*------------------------------------------------------------------------*
164400000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
164500000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
164600000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
164700000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
164800000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
164900000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
165000000000     C*------------------------------------------------------------------------*
165100000000     C     CAROPZ        BEGSR
165200000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
165300000000     C     1             DO        24            I
165400000000     C                   MOVEL     OPZ(I)        OPZRIG
165500000000     C                   MOVEL     OPZOPD        OPD(I)
165600000000     C                   MOVEL     OPZOPS        OPS(I)
165700000000     C                   MOVEL     OPZDES        ODE(I)
165800020923     C     DUTLPO        IFEQ      'S'
165900000000     C     $TGES         IFEQ      'G'
166000000000     C                   MOVEL     OPZASG        OAT(I)
166100000000     C                   ELSE
166200000000     C                   MOVEL     OPZASI        OAT(I)
166300000000     C                   ENDIF
166400000000     C                   ELSE
166500000000     C     $TGES         IFEQ      'G'
166600000000     C                   MOVEL     OPZAFG        OAT(I)
166700000000     C                   ELSE
166800000000     C                   MOVEL     OPZAFI        OAT(I)
166900000000     C                   ENDIF
167000000000     C                   ENDIF
167100000000     C     OAT(I)        IFEQ      'E'
167200000000     C                   MOVEL     USER          WEDP              3
167300000000     C     WEDP          IFEQ      'EDP'
167400000000     C                   MOVEL     'S'           OAT(I)
167500000000     C                   ELSE
167600000000     C                   MOVEL     'N'           OAT(I)
167700000000     C                   ENDIF
167800000000     C                   ENDIF
167900000000     C                   ENDDO
168000000000     C                   ENDSR
168100000000     C*------------------------------------------------------------------------*
168200000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
168300000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
168400000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
168500000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
168600000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
168700000000     C*------------------------------------------------------------------------*
168800000000     C     OUTOPZ        BEGSR
168900000000     C     1             DO        8             I
169000000000     C                   MOVEL     *BLANKS       OPV(I)
169100000000     C                   ENDDO
169200000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
169300000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
169400000000     C                   ADD       1             OPZULT
169500000000     C     OPZULT        IFGT      24
169600000000     C                   Z-ADD     1             OPZULT
169700000000     C                   END
169800000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
169900000000     C     1             DO        24            I
170000000000     C     OAT(I)        IFEQ      'S'
170100000000     C     ODE(I)        ANDNE     *BLANKS
170200000000     C                   ADD       1             OPZNAT
170300000000     C                   ENDIF
170400000000     C                   ENDDO
170500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
170600000000     C                   Z-ADD     0             W
170700000000     C     OPZULT        DO        24            I
170800000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
170900000000     C     W             IFLT      8
171000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
171100000000     C     OAT(I)        IFEQ      'S'
171200000000     C     ODE(I)        ANDNE     *BLANKS
171300000000     C                   ADD       1             W
171400000000     C                   MOVEL     ODE(I)        OPV(W)
171500000000     C                   ENDIF
171600000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
171700000000     C                   Z-ADD     I             OPZULT
171800000000     C                   ENDIF
171900000000     C                   ENDDO
172000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
172100000000     C                   MOVEL     OPV(1)        VOD001
172200000000     C                   MOVEL     OPV(2)        VOD002
172300000000     C                   MOVEL     OPV(3)        VOD003
172400000000     C                   MOVEL     OPV(4)        VOD004
172500000000     C                   MOVEL     OPV(5)        VOD005
172600000000     C                   MOVEL     OPV(6)        VOD006
172700000000     C                   MOVEL     OPV(7)        VOD007
172800000000     C                   MOVEL     OPV(8)        VOD008
172900000000     C                   MOVEL     '1'           $TESVD
173000000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
173100000000     C     OPZNAT        IFGT      8
173200000000     C                   MOVEL     'S'           FAT(23)
173300000000     C                   ENDIF
173400000000     C                   ENDSR
173500000000     C*------------------------------------------------------------------------*
173600000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
173700000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
173800000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
173900000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
174000000000     C*------------------------------------------------------------------------*
174100000000     C     CONOPZ        BEGSR
174200000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
174300000000     C                   MOVEL     *BLANKS       OPZOK             1
174400000000     C     1             DO        24            I                 5 0
174500000000     C     VSCOPZ        IFEQ      OPD(I)
174600000000     C     VSCOPZ        OREQ      OPS(I)
174700000000     C     OAT(I)        IFEQ      'S'
174800000000     C                   MOVEL     OPD(I)        VSCOPZ
174900000000     C                   MOVEL     'S'           OPZOK
175000000000     C                   ELSE
175100000000     C                   MOVEL     'N'           OPZOK             1
175200000000     C                   ENDIF
175300000000     C                   ENDIF
175400000000     C                   ENDDO
175500000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
175600000000     C     OPZOK         IFNE      'S'
175700000000     C                   MOVEL     MSG(3)        $MSG
175800000000     C                   EXSR      CARMSG
175900000000     C                   MOVEL     '1'           $ERR
176000000000     C                   MOVEL     'N'           OPZOK             1
176100000000     C                   ENDIF
176200000000     C                   ENDSR
176300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
176400000000TREC60R- Tasto funzionale non valido.                                          1
176500000000TREC60R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
176600000000TREC60R- Opzione non valida.                                                   3
176700000000TREC60R- Opzione abilitata ma non gestita. Avvertire CED.                      4
176800000000TREC60R- Inizio elenco.                                                        5
176900000000TREC60R- Fine elenco.                                                          6
177000000000TREC60R- 7
177100010111TREC60R- Data di riferimento errata.                                           8
177200000000TREC60R- Data decorrenza voce errata                                           9
177300010111TREC60R- Data di validità non valida                                          10
177400010115TREC60R- Duplica effettuata ma verificare tariffe per eventuali incongruenze! 11
177500000000TREC60R- Codice voce annullato, consentito solo il ripristino                 12
177600000000TREC60R- Codice voce non annullato, impossibile ripristinare                  13
177700000000TREC60R- La gestione e' consentita solo per voci di tipo 'M'=Maggiorazione    14
177800000000TREC60R- 15
177900000000TREC60R- 16
178000000000TREC60R- 17
178100000000TREC60R- 18
178200000000TREC60R- 19
178300000000TREC60R- 20
178400000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
178500000000- 1-1 -                   -    NN     NN
178600000000- 2-2 -2=Modifica         -    SN     NN
178700010110- 3-3 -3=Duplica          -    SN     NN
178800000000- 4-4 -4=Cancellazione    -    SN     NN
178900000000- 5-5 -5=Visualizzazione  -    SS     SS
179000000000- 6-6 -                   -    NN     NN
179100000000- 7-7 -7=Ripristino       -    SN     NN
179200000000- 8-8 -8=Tariffe          -    SS     SS
179300000000- 9-9 -9=Competenze       -    SS     SS
179400000000-10-10-10=Maggiorazioni   -    SS     SS
179500130919-11-  -                   -    NN     NN
179600000000-12-  -                   -    NN     NN
179700000000-13-  -                   -    NN     NN
179800000000-14-  -                   -    NN     NN
179900000000-15-  -                   -    NN     NN
180000000000-16-  -                   -    NN     NN
180100130919-17-17-17=Note C.E.       -    SS     SS
180200000000-18-  -                   -    NN     NN
180300000000-19-  -                   -    NN     NN
180400000000-20-  -                   -    NN     NN
180500000000-21-  -                   -    NN     NN
180600000000-22-  -                   -    NN     NN
180700000000-23-  -                   -    NN     NN
180800000000-24-  -                   -    NN     NN
180900000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
181000000000F1    -                   -    NN     NN
181100000000F2    -                   -    NN     NN
181200000000F3    -F3=Fine            -    SS     SS
181300000000F4    -                   -    NN     NN
181400000000F5    -F5=Rivisualizza    -    SS     SS
181500000000F6    -F6=Inserimento     -    SN     NN
181600000000F7    -                   -    NN     NN
181700000000F8    -                   -    NN     NN
181800000000F9    -                   -    NN     NN
181900000000F10   -                   -    NN     NN
182000000000F11   -                   -    NN     NN
182100000000F12   -F12=Ritorno        -    SS     SS
182200000000F13   -                   -    NN     NN
182300000000F14   -                   -    NN     NN
182400000000F15   -                   -    NN     NN
182500000000F16   -                   -    NN     NN
182600961003F17   -F17=Inizio dati    -    NN     NN
182700961003F18   -F18=Fine dati      -    NN     NN
182800000000F19   -                   -    NN     NN
182900000000F20   -                   -    NN     NN
183000000000F21   -                   -    NN     NN
183100000000F22   -                   -    NN     NN
183200130919F23   -F23=Altre opzioni  -    SS     SS
183300000000F24   -F24=Altri tasti    -    NN     NN
183400000000ENTER -                   -    SS     SS
183500000000ROLLUP-                   -    SS     SS
183600000000ROLLDN-                   -    SS     SS
183700000000HELP  -                   -    NN     NN
183800000000PRINT -                   -    NN     NN
183900000000BSPACE-                   -    NN     NN
184000000000CLEAR -                   -    NN     NN

000100981012     H*------------------------------------------------------------------------*
000200981012     H* GESTIONE VOCI DI CONTABILITA' - SCELTA                                 *
000300981012     H*------------------------------------------------------------------------*
000400981012     H DECEDIT('0,') DATEDIT(*DMY.)
000500981012     F*------------------------------------------------------------------------*
000600981012     F* DATA BASE
000700981012     F*------------------------------------------------------------------------*
000800981012     FTREC91D   CF   E             WORKSTN INFDS(DEVDS1)
000900981012     F                                     SFILE(EC91DVS:SFLULT)
001000981012     F                                     SFILE(EC91DVM:MSGULT)
001100981012     FECECV01L  IF   E           K DISK
001200981012     D*------------------------------------------------------------------------*
001300981012     D* SCHIERE
001400981012     D*------------------------------------------------------------------------*
001500981012     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
001600981012     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700981012     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
001800981012     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
001900981012     D FUN             S             40    DIM(31)
002000981012     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002100981012     D FFU             S              6    DIM(31)
002200981012     D FDE             S             19    DIM(31)
002300981012     D FAT             S              1    DIM(31)
002400981012     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
002500981012     D OPD             S              2    DIM(24)
002600981012     D OPS             S              2    DIM(24)
002700981012     D ODE             S             19    DIM(24)
002800981012     D OAT             S              1    DIM(24)
002900981012     D* TABELLA DESCRIZIONI A VIDEO
003000981012     D FUV             S             19    DIM(8)
003100981012     D OPV             S             19    DIM(8)
003200981012     D* TABELLA TASTI FUNZIONALI PREMUTI
003300981012     D KEYS            S              1    DIM(40)
003400981012     D*------------------------------------------------------------------------*
003500981012     D* INPUT
003600981012     D*------------------------------------------------------------------------*
003700981012     D*---------------------
003800981012     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
003900981012     D*---------------------
004000981012     D DSOPZ         E DS
004100981012     D  OPZRIG                 1     40
004200981012     D*---------------------
004300981012     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004400981012     D*---------------------
004500981012     D DSFUN         E DS
004600981012     D  FUNRIG                 1     40
004700981012     D*---------------------
004800981012     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004900981012     D*---------------------
005000981012     D DSMSG         E DS
005100981012     D*---------------------
005200020923     D* DS REPERIMENTO DATI UTENTE
005300020923     D*-------------------
005400020923     D TIBS34DS      E DS
005500020923     D DDATIUTE      E DS
005600020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005700981012     D*---------------------
005800981012     D* DS PER GESTIONE TASTI FUNZIONALI
005900981012     D*---------------------
006000981012     D DSKEYS        E DS
006100981012     D  KEYRIG                 1     40
006200981012     D*---------------------
006300981012     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006400981012     D*---------------------
006500981012     D                SDS
006600981012     D  USER                 254    263
006700981012     D*---------------------
006800981012     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006900981012     D*---------------------
007000981012     D DEVDS1          DS
007100981012     D* CHIAVE FUNZIONALE BATTUTA
007200981012     D  KEY                  369    369
007300981012     D* POSIZIONE CURSORE (IN HEX)
007400981012     D  CURSOR               370    371
007500981012     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
007600981012     D  FRTSFL               378    379B 0
007700981012     D*---------------------
007800981012     D* DS ESTERNA DI PROCEDURA
007900981012     D*---------------------
008000981012     D DS60          E DS                  EXTNAME(TREC60DS)
008100981012     D*---------------------
008200981012     D* ARCHITETTURA
008300981012     D*---------------------
008400981012     D KPJBA         E DS
008500981012     D*------------------
008600981012     D* CALL XSRDA8 - CONTROLLO DATE
008700981012     D*------------------
008800981012     D WLBDA8          DS                  INZ
008900981012     D  G08DAT                 1      8  0
009000981012     D  G08INV                 9     16  0
009100981012     D  G08ERR                17     17
009200981012     D  G08TGI                18     22  0
009300981012     D*------------------
009400981012     D* DESCRIZIONE OPERAZIONE IN TESTATA
009500981012     D*------------------
009600981012     D DESGES          C                   CONST('** GESTIONE **')
009700981012     D DESINT          C                   CONST('INTERROGAZIONE')
009800981012     C*------------------------------------------------------------------------*
009900981012     C* MAIN LINE
010000981012     C*------------------------------------------------------------------------*
010100981012     C*
010200981012     C* CICLO FINO A CHE:
010300981012DO  1C     $FINE         DOUEQ     '1'
010400981012     C*
010500981012     C                   MOVEL     '0'           $ERR
010600981012     C*
010700981012     C* CONTROLLA GESTIONE VIDEO
010800981012IF  2C     $GES          IFEQ      'VS'
010900981012     C                   EXSR      GESVS
011000981012E   2C                   ENDIF
011100981012     C*
011200981012E   1C                   ENDDO
011300981012     C* FINE PGM
011400981012     C                   SETON                                        LR
011500981012     C*------------------------------------------------------------------------*
011600981012     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
011700981012     C*------------------------------------------------------------------------*
011800981012     C     CARVS         BEGSR
011900981012     C*
012000981012     C                   MOVEL     'VS'          $GES
012100981012     C*
012200981012     C                   EXSR      CAROPZ
012300981012     C                   EXSR      OUTOPZ
012400981012     C                   MOVEA     FUN2          FUN
012500981012     C                   EXSR      CARFUN
012600981012     C                   EXSR      OUTFUN
012700981012     C*
012800981012     C                   WRITE     EC91DVT
012900981012     C                   WRITE     EC91DVO
013000981012     C                   WRITE     EC91DVF
013100981012     C*
013200981012     C                   EXSR      POSSFL
013300981012     C*
013400981012     C                   ENDSR
013500981012     C*------------------------------------------------------------------------*
013600981012     C* GESVS  - GESTIONE SUBFILE
013700981012     C*------------------------------------------------------------------------*
013800981012     C     GESVS         BEGSR
013900981012     C*
014000981012IF  1C     $TESVD        IFEQ      '1'
014100981012     C                   WRITE     EC91DVT
014200981012     C                   WRITE     EC91DVO
014300981012     C                   WRITE     EC91DVF
014400981012E   1C                   ENDIF
014500981012     C*
014600981012     C                   EXSR      OUTSFL
014700981012     C*
014800981012     C                   EXSR      CONFUN
014900981012     C*
015000981012IF  1C     $ERR          IFEQ      '0'
015100981012SL  2C                   SELECT
015200981012     C     KEY           WHENEQ    F6
015300981012     C                   EXSR      F06GES
015400981012     C                   OTHER
015500981012     C                   EXSR      FUNSFL
015600981012E   2C                   ENDSL
015700981012E   1C                   ENDIF
015800981012     C*
015900981012     C                   ENDSR
016000981012     C*------------------------------------------------------------------------*
016100981012     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
016200981012     C*------------------------------------------------------------------------*
016300981012     C     F06GES        BEGSR
016400981012     C*
016500981012     C* LANCIO L'INSERIMENTO
016600981012     C                   MOVEL     *BLANKS       WVOB
016700981012     C                   Z-ADD     *ZEROS        WDDB
016800981012     C*
016900981012     C                   MOVEL     D60OP0        WOP0
017000981012     C                   MOVEL     'F06'         WOP1
017100981012     C                   MOVEL     'TREC92R'     WPGM              8
017200981012     C                   EXSR      LANPGM
017300981012     C*
017400981012     C                   EXSR      POSSFL
017500981012     C*
017600981012     C                   ENDSR
017700981012     C*------------------------------------------------------------------------*
017800981012     C*  GESOPZ - GESTIONE OPZIONI
017900981012     C*------------------------------------------------------------------------*
018000981012     C     GESOPZ        BEGSR
018100981012     C*
018200981012SL  1C                   SELECT
018300981012     C     VSCOPZ        WHENEQ    ' 2'
018400981012     C                   EXSR      O02GES
018500981012     C     VSCOPZ        WHENEQ    ' 5'
018600981012     C                   EXSR      O05GES
018700981012     C                   OTHER
018800981012     C                   MOVEL     MSG(4)        $MSG
018900981012     C                   EXSR      CARMSG
019000981012E   1C                   ENDSL
019100981012     C*
019200981012     C                   ENDSR
019300981012     C*------------------------------------------------------------------------*
019400981012     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
019500981012     C*------------------------------------------------------------------------*
019600981012     C     O02GES        BEGSR
019700981012     C*
019800981012IF  1C     DATCOR        IFGT      VSHDSC
019900981012     C                   MOVEL     '1'           $ERR
020000981012     C                   MOVEL     MSG(7)        $MSG
020100981012     C                   EXSR      CARMSG
020200981012E   1C                   ENDIF
020300981012     C*
020400981012     C* LANCIO LA MODIFICA
020500981012IF  1C     $ERR          IFEQ      '0'
020600981012     C                   MOVEL     VSCVOC        WVOB
020700981012     C                   Z-ADD     VSHDDE        WDDB
020800981012     C                   MOVEL     D60OP0        WOP0
020900981012     C                   MOVEL     'O02'         WOP1
021000981012     C                   MOVEL     'TREC92R'     WPGM              8
021100981012     C                   EXSR      LANPGM
021200981012     C                   Z-ADD     1             SWMODI
021300981012E   1C                   ENDIF
021400981012     C*
021500981012     C                   ENDSR
021600981012     C*------------------------------------------------------------------------*
021700981012     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
021800981012     C*------------------------------------------------------------------------*
021900981012     C     O05GES        BEGSR
022000981012     C*
022100981012     C* LANCIO LA VISUALIZZAZIONE
022200981012     C                   MOVEL     VSCVOC        WVOB
022300981012     C                   Z-ADD     VSHDDE        WDDB
022400981012     C                   MOVEL     D60OP0        WOP0
022500981012     C                   MOVEL     'O05'         WOP1
022600981012     C                   MOVEL     'TREC92R'     WPGM              8
022700981012     C                   EXSR      LANPGM
022800981012     C*
022900981012     C                   ENDSR
023000981012     C*------------------------------------------------------------------------*
023100981012     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
023200981012     C*------------------------------------------------------------------------*
023300981012     C     NEWPOS        BEGSR
023400981012     C*
023500981012     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
023600981012     C*        1 = RICHIESTO POSIZIONAMENTO
023700981012     C                   Z-ADD     0             SWPOSI            7 0
023800981012     C*
023900981012     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
024000981012     C*            NON OCCORRE RICARICARE IL SUBFILE
024100981012     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
024200981012     C                   Z-ADD     0             SWMODI            1 0
024300981012     C*
024400981012     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
024500981012IF  1C     VCCVOC        IFNE      DEPVOC
024600981012     C*
024700981012     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
024800981012     C                   EXSR      CHKPAR
024900981012     C*
025000981012     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
025100981012IF  2C     $ERR          IFEQ      '0'
025200981012     C                   Z-ADD     1             SWPOSI
025300981012     C                   MOVEL     VCCVOC        DEPVOC
025400981012E   2C                   ENDIF
025500981012E   1C                   ENDIF
025600981012     C*
025700981012     C                   ENDSR
025800981012     C*------------------------------------------------------------------------*
025900981012     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
026000981012     C*------------------------------------------------------------------------*
026100981012     C     CHKPAR        BEGSR
026200981012     C*
026300981012     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
026400981012     C* SUL CAMPO DEL CONTROL
026500981012IF  1C     $ERR          IFEQ      '1'
026600981012     C                   MOVEL     '0'           *IN43
026700981012X   1C                   ELSE
026800981012     C                   MOVEL     '1'           *IN43
026900981012E   1C                   ENDIF
027000981012     C*
027100981012     C                   ENDSR
027200981012     C*------------------------------------------------------------------------*
027300981012     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD VOCE DI C/E
027400981012     C*------------------------------------------------------------------------*
027500981012     C     SETFIL        BEGSR
027600981012     C*
027700981012     C                   MOVEL     '1'           *IN43
027800981012     C*
027900981012     C* POSIZIONAMENTO ARCHIVIO VOCI DI C/E
028000981012     C                   MOVEL     VCCVOC        KCVVOC
028100981012     C     KEYECV        SETLL     ECECV01L                           99
028200981012     C  N99              EXSR      LETFIL
028300981012     C*
028400981012     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
028500981012IF  1C     *IN99         IFEQ      '1'
028600981012     C                   Z-ADD     1             SFLEOF
028700981012E   1C                   ENDIF
028800981012     C*
028900981012     C                   ENDSR
029000981012     C*------------------------------------------------------------------------*
029100981012     C* LETFIL - LEGGE RECORD SUCESSIVO VOCE DI C/E
029200981012     C*------------------------------------------------------------------------*
029300981012     C     LETFIL        BEGSR
029400981012     C*
029500981012     C* LEGGO FINO A:
029600981012     C*     - FINE FILE
029700981012     C*     - TROVATO RECORD VALIDO
029800981012     C                   MOVEL     '0'           WRECOK            1
029900981012DO  1C     *IN99         DOWEQ     *OFF
030000981012     C     WRECOK        ANDEQ     '0'
030100981012     C*
030200981012     C* LETTURA VOCE DI C/E
030300981012     C                   READ      ECECV01L                               99
030400981012     C*
030500981012     C* FINE FILE  O TABELLA DIVERSA
030600981012IF  2C     *IN99         IFEQ      '1'
030700981012     C                   Z-ADD     1             SFLEOF
030800981012X   2C                   ELSE
030900981012     C                   EXSR      CHKREC
031000981012E   2C                   ENDIF
031100981012     C*
031200981012E   1C                   ENDDO
031300981012     C*
031400981012     C                   ENDSR
031500981012     C*------------------------------------------------------------------------*
031600981012     C* CHKREC - CONTROLLA VALIDITA' RECORD VOCE DI C/E
031700981012     C*------------------------------------------------------------------------*
031800981012     C     CHKREC        BEGSR
031900981012     C*
032000981012     C                   MOVEL     '1'           WRECOK
032100981012     C*
032200981012     C                   ENDSR
032300981012     C*------------------------------------------------------------------------*
032400981012     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
032500981012     C*------------------------------------------------------------------------*
032600981012     C     IRESFL        BEGSR
032700981012     C* CAMPI
032800981012     C                   Z-ADD     ECVDDE        VSHDDE
032900981012     C                   Z-ADD     ECVDSC        VSHDSC
033000981012     C                   MOVEL     ECVVOC        VSCVOC
033100981012     C                   Z-ADD     ECVDDE        G08INV
033200981012     C                   Z-ADD     *ZEROS        G08DAT
033300981012     C                   MOVEL     '3'           G08ERR
033400981012     C                   CALL      'XSRDA8'
033500981012     C                   PARM                    WLBDA8
033600981012     C                   Z-ADD     G08DAT        VSCDDE
033700981012     C                   Z-ADD     ECVDSC        G08INV
033800981012     C                   Z-ADD     *ZEROS        G08DAT
033900981012     C                   MOVEL     '3'           G08ERR
034000981012     C                   CALL      'XSRDA8'
034100981012     C                   PARM                    WLBDA8
034200981012     C                   Z-ADD     G08DAT        VSCDSC
034300981012     C                   MOVEL     ECVNCC        VSCNCC
034400981012     C                   MOVEL     ECVNCF        VSCNCF
034500981012     C                   MOVEL     ECVRFC        VSCRFC
034600981012     C                   MOVEL     ECVRFF        VSCRFF
034700981012     C* DECODIFICHE
034800981012     C                   MOVEL     *BLANKS       VSDVOC
034900981012     C                   MOVEL     '201'         YSOC              3
035000981013     C                   Z-ADD     *ZEROS        YVON              4 0
035100981013     C                   MOVEL     ECVVOC        YVOA              8
035200981012     C                   Z-ADD     ECVDDE        YDAT              8 0
035300981012     C                   MOVEL     *BLANKS       YDES             20
035400981012     C                   MOVEL     *BLANKS       YERR              1
035500981012     C                   MOVEL     *BLANKS       YMSG             80
035600981012     C                   CALL      'YCO900R'
035700981012     C                   PARM                    YSOC
035800981013     C                   PARM                    YVON
035900981013     C                   PARM                    YVOA
036000981012     C                   PARM                    YDAT
036100981012     C                   PARM                    YDES
036200981012     C                   PARM                    YERR
036300981012     C                   PARM                    YMSG
036400981012IF  1C     YERR          IFNE      'S'
036500981012     C                   MOVEL     YDES          VSDVOC
036600981012E   1C                   ENDIF
036700981012     C*
036800981012     C                   ENDSR
036900981012     C*------------------------------------------------------------------------*
037000981012     C* LANPGM - LANCIA UN PROGRAMMA
037100981012     C*------------------------------------------------------------------------*
037200981012     C     LANPGM        BEGSR
037300981012     C*--------------------
037400981012     C* PARAMETRI INFORMATIVI
037500981012     C*--------------------
037600981012     C                   MOVEL     WOP0          D60OP0
037700981012     C                   MOVEL     WOP1          D60OP1
037800981012     C                   MOVEL     '0'           D60F03
037900981012     C                   MOVEL     '0'           D60ERR
038000981012     C                   MOVEL     *BLANKS       D60MSG
038100981012     C*--------------------
038200981012     C* PARAMETRI
038300981012     C*--------------------
038400981012     C                   MOVEL     WVOB          D60VOB
038500981012     C                   Z-ADD     WDDB          D60DDB
038600981012     C* LANCIO
038700981012     C                   MOVEL     DS60          KPJBU
038800981012     C                   CALL      WPGM                                 99
038900981012     C                   PARM                    KPJBA
039000981012     C                   MOVEL     KPJBU         DS60
039100981012     C*
039200981012     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
039300981012IF  1C     D60F03        IFEQ      '1'
039400981012     C                   MOVEL     '1'           $F03
039500981012     C                   MOVEL     '1'           $ERR
039600981012     C                   MOVEL     '1'           $FINE
039700981012     C*
039800981012     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
039900981012X   1C                   ELSE
040000981012     C                   MOVEL     D60MSG        $MSG
040100981012     C                   EXSR      CARMSG
040200981012E   1C                   ENDIF
040300981012     C*
040400981012     C                   ENDSR
040500020923     C*-------------------------------------------------------------------------
040600020923     C* REPERISCE I DATI UTENTE
040700020923     C*-------------------------------------------------------------------------
040800020923     C     REPDATIUTE    BEGSR
040900020923     C*
041000020923     C* INIZIALIZZA VARIABILI DI WRK
041100020923     C                   CLEAR                   TIBS34DS
041200020923     C                   CLEAR                   AZUTEDS
041300020923     C                   CLEAR                   DDATIUTE
041400020923     C*
041500020923     C     *DTAARA       DEFINE    §azute        azuteds
041600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
041700020923     C                   IN(E)     *DTAARA
041800020923if  1C                   IF        %Error
041900020923     c                   EVAL      I34Tla = 'L'
042000020923     C                   CALL      'TIBS34R'
042100020923     C                   PARM                    Tibs34Ds
042200020923     C                   IN        *DTAARA
042300020923e   1C                   ENDIF
042400020923     C*
042500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
042600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
042700020923     C*
042800020923     C                   ENDSR
042900981012     C*------------------------------------------------------------------------*
043000981012     C*  *INZSR - ROUTINE INIZIALE
043100981012     C*------------------------------------------------------------------------*
043200981012     C     *INZSR        BEGSR
043300981012     C*
043400981012     C* RICEVIMENTO PARAMETRI
043500981012     C     *ENTRY        PLIST
043600981012     C                   PARM                    KPJBA
043700981012     C                   MOVEL     KPJBU         DS60
043800981012     C*
043900981012     C* DEFINIZIONE KLIST
044000981012     C     *LIKE         DEFINE    ECVVOC        KCVVOC
044100981012     C     *LIKE         DEFINE    VCCVOC        DEPVOC
044200981012     C*
044300981012     C* DEFINIZIONE CAMPI DI COMODO LANCIO PROGRAMMA
044400981012     C     *LIKE         DEFINE    D60OP0        WOP0
044500981012     C     *LIKE         DEFINE    D60OP1        WOP1
044600981012     C     *LIKE         DEFINE    D60F03        WF03
044700981012     C     *LIKE         DEFINE    D60ERR        WERR
044800981012     C     *LIKE         DEFINE    D60MSG        WMSG
044900981012     C     *LIKE         DEFINE    D60VOB        WVOB
045000981012     C     *LIKE         DEFINE    D60DDB        WDDB
045100981012     C*
045200981012     C* CHIAVE LETTURA ECECV01L - PARZIALE
045300981012     C     KEYECV        KLIST
045400981012     C                   KFLD                    KCVVOC
045500981012     C*
045600981012     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
045700981012     C                   MOVE      D60OP0        INTGES            2
045800981012IF  1C     INTGES        IFEQ      '02'
045900981012     C                   MOVEL     'G'           $TGES             1
046000981012     C                   MOVEL     DESGES        VTDOPE
046100981012X   1C                   ELSE
046200981012     C                   MOVEL     'I'           $TGES             1
046300981012     C                   MOVEL     DESINT        VTDOPE
046400981012E   1C                   ENDIF
046500981012     C*
046600981012     C* ESEGUE OPERAZIONI INIZIALI STANDARD
046700981012     C                   EXSR      INZSTD
046800981012     C*
046900981012     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
047000981012     C                   EXSR      INZSFL
047100981012     C*
047200981012     C* CALCOLA LA DATA CORRENTE
047300981012     C                   TIME                    N14              14 0
047400981012     C                   MOVE      N14           N8                8 0
047500981012     C                   Z-ADD     N8            G08DAT
047600981012     C                   Z-ADD     *ZEROS        G08INV
047700981012     C                   MOVEL     '0'           G08ERR
047800981012     C                   CALL      'XSRDA8'
047900981012     C                   PARM                    WLBDA8
048000981012     C                   Z-ADD     G08INV        DATCOR            8 0
048100981012     C*
048200981012     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
048300981012     C                   MOVEL     *BLANKS       VCCVOC
048400981012     C*
048500981012     C* EMETTO VIDEATA SUBFILE
048600981012     C                   EXSR      CARVS
048700981012     C*
048800981012     C                   ENDSR
048900981012     C*------------------------------------------------------------------------*
049000981012     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049100981012     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049200981012     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
049300981012     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
049400981012     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
049500981012     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049600981012     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049700981012     C*------------------------------------------------------------------------*
049800981012     C* INZSTD - ROUTINE INIZIALE STANDARD
049900981012     C*------------------------------------------------------------------------*
050000981012     C     INZSTD        BEGSR
050100981012     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
050200981012     C                   CALL      'XKEYS'
050300981012     C                   PARM                    DSKEYS
050400981012     C                   MOVEA     KEYRIG        KEYS
050500981012     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
050600981012     C     XMSGPA        PLIST
050700981012     C                   PARM                    DSMSMS
050800981012     C                   PARM                    DSMSPG
050900981012     C                   PARM                    DSMSKE
051000981012     C                   PARM                    DSMSER
051100981012     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
051200981012     C                   MOVEL     'TREC91R'     DSMSPG
051300981012     C                   MOVEL     'TREC91R'     VMCQUE
051400981012     C                   MOVEL     *BLANKS       VMCKEY
051500981012     C                   MOVEL     *BLANKS       DSMSMS
051600981012     C* INIZIALIZZO FLAG CONTROLLO PGM
051700981012     C                   MOVEL     '0'           $ERR              1
051800981012     C                   MOVEL     '0'           $F03              1
051900981012     C                   MOVEL     *BLANKS       $GES              2
052000981012     C                   MOVEL     *BLANKS       $MSG             80
052100981012     C                   MOVEL     '0'           $FINE             1
052200981012     C                   MOVEL     '0'           $TESVD            1
052300981012     C* LANCIO ROUTINE PER VERIFICARE SE
052400981012     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
052500981012     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
052600020923     C                   EXSR      REPDATIUTE
052700981012     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
052800981012     C                   BITOFF    '01234567'    POSMI1            1
052900981012     C                   BITOFF    '01234567'    POSMI2            1
053000981012     C                   BITOFF    '01234567'    POSMA1            1
053100981012     C                   BITOFF    '01234567'    POSMA2            1
053200981012     C                   BITON     '34'          POSMI1            1
053300981012     C                   BITON     '7'           POSMI2            1
053400981012     C                   BITON     '34'          POSMA1            1
053500981012     C                   BITON     '0'           POSMA2            1
053600981012     C                   MOVE      POSMI2        POSMIN            2
053700981012     C                   MOVEL     POSMI1        POSMIN
053800981012     C                   MOVE      POSMA2        POSMAX            2
053900981012     C                   MOVEL     POSMA1        POSMAX
054000981012     C                   ENDSR
054100981012     C*------------------------------------------------------------------------*
054200981012     C* FUNSTD - LANCIA SUBROUTINE STANDARD
054300981012     C*------------------------------------------------------------------------*
054400981012     C     FUNSTD        BEGSR
054500981012     C                   SELECT
054600981012     C     KEY           WHENEQ    F3
054700981012     C                   EXSR      F03STD
054800981012     C     KEY           WHENEQ    F12
054900981012     C                   EXSR      F12STD
055000981012     C     KEY           WHENEQ    F24
055100981012     C                   EXSR      F24STD
055200981012     C                   OTHER
055300981012     C                   MOVEL     MSG(2)        $MSG
055400981012     C                   EXSR      CARMSG
055500981012     C                   ENDSL
055600981012     C                   ENDSR
055700981012     C*------------------------------------------------------------------------*
055800981012     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
055900981012     C*------------------------------------------------------------------------*
056000981012     C     F03STD        BEGSR
056100981012     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
056200981012     C                   MOVEL     '1'           $F03
056300981012     C* IMPOSTO FLAG DI FINE PROGRAMMA
056400981012     C                   MOVEL     '1'           $FINE
056500981012     C                   ENDSR
056600981012     C*------------------------------------------------------------------------*
056700981012     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
056800981012     C*------------------------------------------------------------------------*
056900981012     C     F12STD        BEGSR
057000981012     C* IMPOSTO FLAG DI FIN PGM
057100981012     C                   MOVEL     '1'           $FINE
057200981012     C                   ENDSR
057300981012     C*------------------------------------------------------------------------*
057400981012     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
057500981012     C*------------------------------------------------------------------------*
057600981012     C     F24STD        BEGSR
057700981012     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
057800981012     C                   EXSR      OUTFUN
057900981012     C                   ENDSR
058000981012     C*------------------------------------------------------------------------*
058100981012     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
058200981012     C*------------------------------------------------------------------------*
058300981012     C     INZSFL        BEGSR
058400981012     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
058500981012     C     *LIKE         DEFINE    VCCNBR        SFLPAG
058600981012     C     *LIKE         DEFINE    VCCNBR        SFLEOF
058700981012     C     *LIKE         DEFINE    VCCNBR        SFLERR
058800981012     C     *LIKE         DEFINE    VCCNBR        SFLULT
058900981012     C     *LIKE         DEFINE    VCCNBR        WFLULT
059000981012     C     *LIKE         DEFINE    VCCNBR        SFLRIG
059100981012     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
059200981012     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
059300981012     C                   Z-ADD     12            SFLPAG
059400981012     C                   ENDSR
059500981012     C*------------------------------------------------------------------------*
059600981012     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
059700981012     C*------------------------------------------------------------------------*
059800981012     C     FUNSFL        BEGSR
059900981012     C                   SELECT
060000981012     C     KEY           WHENEQ    ENTER
060100981012     C                   EXSR      ENTSFL
060200981012     C     KEY           WHENEQ    F3
060300981012     C                   EXSR      F03STD
060400981012     C     KEY           WHENEQ    F5
060500981012     C                   EXSR      F05STD
060600981012     C     KEY           WHENEQ    F12
060700981012     C                   EXSR      F12STD
060800981012     C     KEY           WHENEQ    F17
060900981012     C                   EXSR      F17STD
061000981012     C     KEY           WHENEQ    F18
061100981012     C                   EXSR      F18STD
061200981012     C     KEY           WHENEQ    F23
061300981012     C                   EXSR      F23STD
061400981012     C     KEY           WHENEQ    F24
061500981012     C                   EXSR      F24STD
061600981012     C     KEY           WHENEQ    ROLLUP
061700981012     C                   EXSR      RUPSTD
061800981012     C     KEY           WHENEQ    ROLLDN
061900981012     C                   EXSR      RDNSTD
062000981012     C                   OTHER
062100981012     C                   MOVEL     MSG(2)        $MSG
062200981012     C                   EXSR      CARMSG
062300981012     C                   ENDSL
062400981012     C                   ENDSR
062500981012     C*------------------------------------------------------------------------*
062600981012     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
062700981012     C*------------------------------------------------------------------------*
062800981012     C     F05STD        BEGSR
062900981012     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
063000981012     C                   EXSR      POSSFL
063100981012     C                   ENDSR
063200981012     C*------------------------------------------------------------------------*
063300981012     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
063400981012     C*------------------------------------------------------------------------*
063500981012     C     F17STD        BEGSR
063600981012     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
063700981012     C                   Z-ADD     1             VCCNBR
063800981012     C                   ENDSR
063900981012     C*------------------------------------------------------------------------*
064000981012     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
064100981012     C*------------------------------------------------------------------------*
064200981012     C     F18STD        BEGSR
064300981012     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
064400981012     C                   Z-ADD     SFLULT        VCCNBR
064500981012     C                   ENDSR
064600981012     C*------------------------------------------------------------------------*
064700981012     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
064800981012     C*------------------------------------------------------------------------*
064900981012     C     F23STD        BEGSR
065000981012     C* GESTIONE VISUALIZZAZIONE OPZIONI
065100981012     C                   EXSR      OUTOPZ
065200981012     C                   ENDSR
065300981012     C*------------------------------------------------------------------------*
065400981012     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
065500981012     C*------------------------------------------------------------------------*
065600981012     C     RUPSTD        BEGSR
065700981012     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
065800981012     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
065900981012     C     CURSOR        IFGE      POSMIN
066000981012     C     CURSOR        ANDLE     POSMAX
066100981012     C                   ELSE
066200981012     C                   EXSR      CARSFL
066300981012     C                   ENDIF
066400981012     C                   ENDSR
066500981012     C*------------------------------------------------------------------------*
066600981012     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
066700981012     C*------------------------------------------------------------------------*
066800981012     C     RDNSTD        BEGSR
066900981012     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
067000981012     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
067100981012     C     CURSOR        IFGE      POSMIN
067200981012     C     CURSOR        ANDLE     POSMAX
067300981012     C                   ELSE
067400981012     C* INVIO MESSAGGIO DI INIZIO ELENCO
067500981012     C                   MOVEL     '1'           $ERR
067600981012     C                   MOVEL     MSG(5)        $MSG
067700981012     C                   EXSR      CARMSG
067800981012     C                   ENDIF
067900981012     C                   ENDSR
068000981012     C*------------------------------------------------------------------------*
068100981012     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
068200981012     C*------------------------------------------------------------------------*
068300981012     C     ENTSFL        BEGSR
068400981012     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
068500981012     C                   EXSR      NEWPOS
068600981012     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
068700981012     C     $ERR          IFEQ      '0'
068800981012     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
068900981012     C     SWPOSI        IFNE      0
069000981012     C                   EXSR      POSSFL
069100981012     C                   ELSE
069200981012     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
069300981012     C     SFLULT        IFNE      0
069400981012     C                   EXSR      OPZSFL
069500981012     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
069600981012     C     SWMODI        IFNE      0
069700981012     C                   EXSR      POSSFL
069800981012     C                   ENDIF
069900981012     C                   ENDIF
070000981012     C                   ENDIF
070100981012     C                   ENDIF
070200981012     C                   ENDSR
070300981012     C*------------------------------------------------------------------------*
070400981012     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
070500981012     C*------------------------------------------------------------------------*
070600981012     C     POSSFL        BEGSR
070700981012     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
070800981012     C                   EXSR      CLRSFL
070900981012     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
071000981012     C                   EXSR      SETFIL
071100981012     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
071200981012     C     SFLEOF        IFEQ      0
071300981012     C                   EXSR      CARSFL
071400981012     C                   ENDIF
071500981012     C                   ENDSR
071600981012     C*------------------------------------------------------------------------*
071700981012     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
071800981012     C*------------------------------------------------------------------------*
071900981012     C     OPZSFL        BEGSR
072000981012     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
072100981012     C                   MOVEL     'S'           OPZOK             1
072200981012     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
072300981012     C                   Z-ADD     0             SFLVAR            5 0
072400981012     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
072500981012     C                   Z-ADD     0             SFLERR
072600981012     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
072700981012     C                   Z-ADD     SFLULT        WFLULT
072800981012     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
072900981012     C                   READC     EC91DVS                                49
073000981012     C     *IN49         DOWEQ     '0'
073100981012     C     $ERR          ANDEQ     '0'
073200981012     C* ADD CONTATORE DI RIGHE SFL VARIATE
073300981012     C                   ADD       1             SFLVAR
073400981012     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
073500981012     C     OPZOK         IFEQ      'S'
073600981012     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
073700981012     C     VSCOPZ        IFNE      *BLANKS
073800981012     C                   EXSR      CONOPZ
073900981012     C     OPZOK         IFEQ      'S'
074000981012     C     $ERR          ANDEQ     '0'
074100981012     C                   EXSR      GESOPZ
074200981012     C                   ENDIF
074300981012     C                   ENDIF
074400981012     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
074500981012     C*            2. PULIZIA SCELTA IN SFL
074600981012     C     OPZOK         IFEQ      'S'
074700981012     C                   MOVEL     '0'           *IN41
074800981012     C                   MOVEL     *BLANKS       VSCOPZ
074900981012     C                   ELSE
075000981012     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
075100981012     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
075200981012     C                   MOVEL     '1'           *IN41
075300981012     C                   Z-ADD     SFLULT        SFLERR
075400981012     C                   ENDIF
075500981012     C                   ELSE
075600981012     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
075700981012     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
075800981012     C                   MOVEL     '1'           *IN41
075900981012     C                   ENDIF
076000981012     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
076100981012     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
076200981012     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
076300981012     C     OPZOK         IFNE      'S'
076400981012     C                   Z-ADD     SFLERR        VCCNBR
076500981012     C                   ELSE
076600981012     C                   Z-ADD     SFLULT        VCCNBR
076700981012     C                   ENDIF
076800981012     C* AGGIORNO RECORD SFL
076900981012     C                   UPDATE    EC91DVS
077000981012     C     $ERR          IFEQ      '0'
077100981012     C                   READC     EC91DVS                                49
077200981012     C                   ENDIF
077300981012     C                   ENDDO
077400981012     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
077500981012     C                   Z-ADD     WFLULT        SFLULT
077600981012     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
077700981012     C     SFLVAR        IFEQ      0
077800981012     C                   MOVEL     '1'           $FINE
077900981012     C                   ENDIF
078000981012     C                   ENDSR
078100981012     C*------------------------------------------------------------------------*
078200981012     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
078300981012     C*------------------------------------------------------------------------*
078400981012     C     CLRSFL        BEGSR
078500981012     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
078600981012     C* (0=NON FINE FILE  1=FINE FILE)
078700981012     C                   Z-ADD     0             SFLEOF
078800981012     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
078900981012     C                   Z-ADD     0             SFLULT
079000981012     C* ACCENSIONE INDICATORE PER CLEAR SFL
079100981012     C                   MOVEL     '1'           *IN40
079200981012     C                   WRITE     EC91DVC
079300981012     C                   MOVEL     '0'           *IN40
079400981012     C                   ENDSR
079500981012     C*------------------------------------------------------------------------*
079600981012     C* CARSFL - CARICAMENTO SFL
079700981012     C*------------------------------------------------------------------------*
079800981012     C     CARSFL        BEGSR
079900981012     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
080000981012     C     SFLULT        IFEQ      0
080100981012     C                   EXSR      SETFIL
080200981012     C                   ENDIF
080300981012     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
080400981012     C                   Z-ADD     SFLULT        WFLULT
080500981012     C* CICLO:
080600981012     C*    -CARICO NUMERO DI RIGHE=SFLPAG
080700981012     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
080800981012     C                   Z-ADD     1             SFLRIG
080900981012     C     SFLRIG        DOWLE     SFLPAG
081000981012     C     SFLEOF        ANDEQ     0
081100981012     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
081200981012     C                   MOVEL     *BLANKS       VSCOPZ
081300981012     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
081400981012     C                   EXSR      IRESFL
081500981012     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
081600981012     C                   ADD       1             SFLULT
081700981012     C* SCRITTURA RECORD SFL
081800981012     C                   WRITE     EC91DVS
081900981012     C                   ADD       1             SFLRIG
082000981012     C* LETTURA FILE DA CARICARE
082100981012     C                   EXSR      LETFIL
082200981012     C                   ENDDO
082300981012     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
082400981012     C     WFLULT        IFNE      SFLULT
082500981012     C                   Z-ADD     WFLULT        VCCNBR
082600981012     C                   ADD       1             VCCNBR
082700981012     C                   ENDIF
082800981012     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
082900981012     C     SFLEOF        IFEQ      1
083000981012     C     SFLULT        ANDGT     *ZEROS
083100981012     C                   MOVEL     '1'           $ERR
083200981012     C                   MOVEL     MSG(6)        $MSG
083300981012     C                   EXSR      CARMSG
083400981012     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
083500981012     C     SFLULT        CHAIN     EC91DVS                            99
083600981012     C     *IN99         IFEQ      '0'
083700981012     C                   SETON                                        45
083800981012     C                   UPDATE    EC91DVS
083900981012     C                   SETOFF                                       45
084000981012     C                   ENDIF
084100981012     C                   ENDIF
084200981012     C                   ENDSR
084300981012     C*------------------------------------------------------------------------*
084400981012     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
084500981012     C*------------------------------------------------------------------------*
084600981012     C     OUTSFL        BEGSR
084700981012     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
084800981012     C     MSGULT        IFNE      *ZEROS
084900981012     C                   EXSR      OUTMSG
085000981012     C                   ENDIF
085100981012     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
085200981012     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
085300981012     C     SFLEOF        IFEQ      0
085400981012     C                   MOVEL     '0'           *IN42
085500981012     C                   ELSE
085600981012     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
085700981012     C                   MOVEL     '1'           *IN42
085800981012     C                   ENDIF
085900981012     C* EMISSIONE SUBFILE
086000981012     C     SFLULT        IFLE      0
086100981012     C*  --> CASO DI SFL VUOTO
086200981012     C                   MOVEL     '0'           *IN43
086300981012     C                   WRITE     EC91DVV
086400981012     C                   WRITE     EC91DVC
086500981012     C                   READ      EC91DVC                                99
086600981012     C                   ELSE
086700981012     C*  --> CASO DI SFL CON RECORDS
086800981012     C                   WRITE     EC91DVC
086900981012     C                   READ      EC91DVC                                99
087000981012     C                   ENDIF
087100981012     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
087200981012     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
087300981012     C                   Z-ADD     FRTSFL        VCCNBR
087400981012     C                   ENDSR
087500981012     C*------------------------------------------------------------------------*
087600981012     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
087700981012     C*          CODA MESSAGGI DEL PGM
087800981012     C*------------------------------------------------------------------------*
087900981012     C     CARMSG        BEGSR
088000981012     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
088100981012     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
088200981012     C     MSGULT        IFEQ      0
088300981012     C                   MOVEL     '1'           *IN40
088400981012     C                   WRITE     EC91DVK
088500981012     C                   MOVEL     '0'           *IN40
088600981012     C                   ENDIF
088700981012     C* INVIA MESSAGGIO ALLA CODA DEL PGM
088800981012     C                   MOVEL     $MSG          DSMSMS
088900981012     C                   CALL      'XMSG'        XMSGPA
089000981012     C     DSMSER        IFEQ      0
089100981012     C                   MOVEL     DSMSKE        VMCKEY
089200981012     C                   ENDIF
089300981012     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
089400981012     C                   ADD       1             MSGULT            4 0
089500981012     C                   WRITE     EC91DVM
089600981012     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
089700981012     C                   MOVEL     *BLANKS       $MSG
089800981012     C                   ENDSR
089900981012     C*------------------------------------------------------------------------*
090000981012     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
090100981012     C*------------------------------------------------------------------------*
090200981012     C     OUTMSG        BEGSR
090300981012     C* VISUALIZZAZIONE MESSAGGIO
090400981012     C                   MOVEL     '1'           *IN44
090500981012     C                   WRITE     EC91DVK
090600981012     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
090700981012     C                   Z-ADD     *ZEROS        MSGULT
090800981012     C                   ENDSR
090900981012     C*------------------------------------------------------------------------*
091000981012     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
091100981012     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
091200981012     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
091300981012     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
091400981012     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
091500981012     C*------------------------------------------------------------------------*
091600981012     C     CARFUN        BEGSR
091700981012     C                   Z-ADD     *ZEROS        FUNULT            5 0
091800981012     C     1             DO        31            I                 5 0
091900981012     C                   MOVEL     FUN(I)        FUNRIG
092000981012     C                   MOVEL     FUNFUN        FFU(I)
092100981012     C                   MOVEL     FUNDES        FDE(I)
092200020923     C     DUTLPO        IFEQ      'S'
092300981012     C     $TGES         IFEQ      'G'
092400981012     C                   MOVEL     FUNASG        FAT(I)
092500981012     C                   ELSE
092600981012     C                   MOVEL     FUNASI        FAT(I)
092700981012     C                   ENDIF
092800981012     C                   ELSE
092900981012     C     $TGES         IFEQ      'G'
093000981012     C                   MOVEL     FUNAFG        FAT(I)
093100981012     C                   ELSE
093200981012     C                   MOVEL     FUNAFI        FAT(I)
093300981012     C                   ENDIF
093400981012     C                   ENDIF
093500981012     C     FAT(I)        IFEQ      'E'
093600981012     C                   MOVEL     USER          WEDP              3
093700981012     C     WEDP          IFEQ      'EDP'
093800981012     C                   MOVEL     'S'           FAT(I)
093900981012     C                   ELSE
094000981012     C                   MOVEL     'N'           FAT(I)
094100981012     C                   ENDIF
094200981012     C                   ENDIF
094300981012     C                   ENDDO
094400981012     C                   ENDSR
094500981012     C*------------------------------------------------------------------------*
094600981012     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
094700981012     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
094800981012     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
094900981012     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
095000981012     C*------------------------------------------------------------------------*
095100981012     C     OUTFUN        BEGSR
095200981012     C     1             DO        8             I
095300981012     C                   MOVEL     *BLANKS       FUV(I)
095400981012     C                   ENDDO
095500981012     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
095600981012     C                   Z-ADD     *ZEROS        FUNNAT            2 0
095700981012     C     1             DO        31            I
095800981012     C     FAT(I)        IFEQ      'S'
095900981012     C     FDE(I)        ANDNE     *BLANKS
096000981012     C                   ADD       1             FUNNAT
096100981012     C                   ENDIF
096200981012     C                   ENDDO
096300981012     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
096400981012     C     FUNNAT        IFGT      8
096500981012     C                   MOVEL     'S'           FAT(24)
096600981012     C                   ENDIF
096700981012     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
096800981012     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
096900981012     C                   Z-ADD     8             FUNVIS            5 0
097000981012     C     FAT(23)       IFEQ      'S'
097100981012     C                   SUB       1             FUNVIS
097200981012     C                   ENDIF
097300981012     C     FAT(24)       IFEQ      'S'
097400981012     C                   SUB       1             FUNVIS
097500981012     C                   ENDIF
097600981012     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
097700981012     C                   Z-ADD     *ZEROS        W                 5 0
097800981012     C                   ADD       1             FUNULT
097900981012     C     FUNULT        IFGT      31
098000981012     C                   Z-ADD     1             FUNULT
098100981012     C                   ENDIF
098200981012     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
098300981012     C     FUNULT        DO        31            I
098400981012     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
098500981012     C     W             IFLT      FUNVIS
098600981012     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
098700981012     C     FAT(I)        IFEQ      'S'
098800981012     C     FDE(I)        ANDNE     *BLANKS
098900981012     C     I             IFEQ      23
099000981012     C     I             OREQ      24
099100981012     C                   ELSE
099200981012     C                   ADD       1             W
099300981012     C                   MOVEL     FDE(I)        FUV(W)
099400981012     C                   ENDIF
099500981012     C                   ENDIF
099600981012     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
099700981012     C                   Z-ADD     I             FUNULT
099800981012     C                   ENDIF
099900981012     C                   ENDDO
100000981012     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
100100981012     C                   MOVEL     FUV(1)        VFD001
100200981012     C                   MOVEL     FUV(2)        VFD002
100300981012     C                   MOVEL     FUV(3)        VFD003
100400981012     C                   MOVEL     FUV(4)        VFD004
100500981012     C                   MOVEL     FUV(5)        VFD005
100600981012     C                   MOVEL     FUV(6)        VFD006
100700981012     C     FAT(23)       IFEQ      'N'
100800981012     C     FAT(24)       ANDEQ     'N'
100900981012     C                   MOVEL     FUV(7)        VFD007
101000981012     C                   MOVEL     FUV(8)        VFD008
101100981012     C                   ELSE
101200981012     C     FAT(23)       IFEQ      'S'
101300981012     C     FAT(24)       ANDEQ     'S'
101400981012     C                   MOVEL     FDE(23)       VFD007
101500981012     C                   MOVEL     FDE(24)       VFD008
101600981012     C                   ELSE
101700981012     C                   MOVEL     FUV(7)        VFD007
101800981012     C     FAT(23)       IFEQ      'S'
101900981012     C                   MOVEL     FDE(23)       VFD008
102000981012     C                   ELSE
102100981012     C                   MOVEL     FDE(24)       VFD008
102200981012     C                   ENDIF
102300981012     C                   ENDIF
102400981012     C                   ENDIF
102500981012     C                   MOVEL     '1'           $TESVD
102600981012     C                   ENDSR
102700981012     C*------------------------------------------------------------------------*
102800981012     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
102900981012     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
103000981012     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
103100981012     C*                   - ERRORE SE FUNZIONE NON ATTIVA
103200981012     C*------------------------------------------------------------------------*
103300981012     C     CONFUN        BEGSR
103400981012     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
103500981012     C     1             DO        31            I
103600981012     C     KEY           IFEQ      KEYS(I)
103700981012     C* CONTROLLO SE IL TASTO E' ATTIVO
103800981012     C     FAT(I)        IFNE      'S'
103900981012     C                   MOVEL     MSG(1)        $MSG
104000981012     C                   EXSR      CARMSG
104100981012     C                   MOVEL     '1'           $ERR
104200981012     C                   ENDIF
104300981012     C                   ENDIF
104400981012     C                   ENDDO
104500981012     C                   ENDSR
104600981012     C*------------------------------------------------------------------------*
104700981012     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
104800981012     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
104900981012     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
105000981012     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
105100981012     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
105200981012     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
105300981012     C*------------------------------------------------------------------------*
105400981012     C     CAROPZ        BEGSR
105500981012     C                   Z-ADD     *ZEROS        OPZULT            5 0
105600981012     C     1             DO        24            I
105700981012     C                   MOVEL     OPZ(I)        OPZRIG
105800981012     C                   MOVEL     OPZOPD        OPD(I)
105900981012     C                   MOVEL     OPZOPS        OPS(I)
106000981012     C                   MOVEL     OPZDES        ODE(I)
106100020923     C     DUTLPO        IFEQ      'S'
106200981012     C     $TGES         IFEQ      'G'
106300981012     C                   MOVEL     OPZASG        OAT(I)
106400981012     C                   ELSE
106500981012     C                   MOVEL     OPZASI        OAT(I)
106600981012     C                   ENDIF
106700981012     C                   ELSE
106800981012     C     $TGES         IFEQ      'G'
106900981012     C                   MOVEL     OPZAFG        OAT(I)
107000981012     C                   ELSE
107100981012     C                   MOVEL     OPZAFI        OAT(I)
107200981012     C                   ENDIF
107300981012     C                   ENDIF
107400981012     C     OAT(I)        IFEQ      'E'
107500981012     C                   MOVEL     USER          WEDP              3
107600981012     C     WEDP          IFEQ      'EDP'
107700981012     C                   MOVEL     'S'           OAT(I)
107800981012     C                   ELSE
107900981012     C                   MOVEL     'N'           OAT(I)
108000981012     C                   ENDIF
108100981012     C                   ENDIF
108200981012     C                   ENDDO
108300981012     C                   ENDSR
108400981012     C*------------------------------------------------------------------------*
108500981012     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
108600981012     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
108700981012     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
108800981012     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
108900981012     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
109000981012     C*------------------------------------------------------------------------*
109100981012     C     OUTOPZ        BEGSR
109200981012     C     1             DO        8             I
109300981012     C                   MOVEL     *BLANKS       OPV(I)
109400981012     C                   ENDDO
109500981012     C                   Z-ADD     *ZEROS        OPZNAT            2 0
109600981012     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
109700981012     C                   ADD       1             OPZULT
109800981012     C     OPZULT        IFGT      24
109900981012     C                   Z-ADD     1             OPZULT
110000981012     C                   END
110100981012     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
110200981012     C     1             DO        24            I
110300981012     C     OAT(I)        IFEQ      'S'
110400981012     C     ODE(I)        ANDNE     *BLANKS
110500981012     C                   ADD       1             OPZNAT
110600981012     C                   ENDIF
110700981012     C                   ENDDO
110800981012     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
110900981012     C                   Z-ADD     0             W
111000981012     C     OPZULT        DO        24            I
111100981012     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
111200981012     C     W             IFLT      8
111300981012     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
111400981012     C     OAT(I)        IFEQ      'S'
111500981012     C     ODE(I)        ANDNE     *BLANKS
111600981012     C                   ADD       1             W
111700981012     C                   MOVEL     ODE(I)        OPV(W)
111800981012     C                   ENDIF
111900981012     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
112000981012     C                   Z-ADD     I             OPZULT
112100981012     C                   ENDIF
112200981012     C                   ENDDO
112300981012     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
112400981012     C                   MOVEL     OPV(1)        VOD001
112500981012     C                   MOVEL     OPV(2)        VOD002
112600981012     C                   MOVEL     OPV(3)        VOD003
112700981012     C                   MOVEL     OPV(4)        VOD004
112800981012     C                   MOVEL     OPV(5)        VOD005
112900981012     C                   MOVEL     OPV(6)        VOD006
113000981012     C                   MOVEL     OPV(7)        VOD007
113100981012     C                   MOVEL     OPV(8)        VOD008
113200981012     C                   MOVEL     '1'           $TESVD
113300981012     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
113400981012     C     OPZNAT        IFGT      8
113500981012     C                   MOVEL     'S'           FAT(23)
113600981012     C                   ENDIF
113700981012     C                   ENDSR
113800981012     C*------------------------------------------------------------------------*
113900981012     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
114000981012     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
114100981012     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
114200981012     C*                   - ERRORE SE OPZIONE NON ATTIVA
114300981012     C*------------------------------------------------------------------------*
114400981012     C     CONOPZ        BEGSR
114500981012     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
114600981012     C                   MOVEL     *BLANKS       OPZOK             1
114700981012     C     1             DO        24            I                 5 0
114800981012     C     VSCOPZ        IFEQ      OPD(I)
114900981012     C     VSCOPZ        OREQ      OPS(I)
115000981012     C     OAT(I)        IFEQ      'S'
115100981012     C                   MOVEL     OPD(I)        VSCOPZ
115200981012     C                   MOVEL     'S'           OPZOK
115300981012     C                   ELSE
115400981012     C                   MOVEL     'N'           OPZOK             1
115500981012     C                   ENDIF
115600981012     C                   ENDIF
115700981012     C                   ENDDO
115800981012     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
115900981012     C     OPZOK         IFNE      'S'
116000981012     C                   MOVEL     MSG(3)        $MSG
116100981012     C                   EXSR      CARMSG
116200981012     C                   MOVEL     '1'           $ERR
116300981012     C                   MOVEL     'N'           OPZOK             1
116400981012     C                   ENDIF
116500981012     C                   ENDSR
116600981012** ======== SCHIERA: MSG  (MESSAGGI)                       ================
116700981012TREC91R- Tasto funzionale non valido.                                          1
116800981012TREC91R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
116900981012TREC91R- Opzione non valida.                                                   3
117000981012TREC91R- Opzione abilitata ma non gestita. Avvertire CED.                      4
117100981012TREC91R- Inizio elenco.                                                        5
117200981012TREC91R- Fine elenco.                                                          6
117300981012TREC91R- Codice voce scaduta, impossibile modificare                           7
117400981012TREC91R- 8
117500981012TREC91R- Data errata                                                           9
117600981012TREC91R- 10
117700981012TREC91R- 11
117800981012TREC91R- 11
117900981012TREC91R- 13
118000981012TREC91R- 14
118100981012TREC91R- 15
118200981012TREC91R- 16
118300981012TREC91R- 17
118400981012TREC91R- 18
118500981012TREC91R- 19
118600981012TREC91R- 20
118700981012** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
118800981012- 1-1 -                   -    NN     NN
118900981012- 2-2 -2=Modifica         -    SN     NN
119000981012- 3-3 -                   -    NN     NN
119100981012- 4-4 -                   -    NN     NN
119200981012- 5-5 -5=Visualizzazione  -    SS     SS
119300981012- 6-6 -                   -    NN     NN
119400981012- 7-7 -                   -    NN     NN
119500981012- 8-8 -                   -    NN     NN
119600981012- 9-9 -                   -    NN     NN
119700981012-10-10-                   -    NN     NN
119800981012-11-  -                   -    NN     NN
119900981012-12-  -                   -    NN     NN
120000981012-13-  -                   -    NN     NN
120100981012-14-  -                   -    NN     NN
120200981012-15-  -                   -    NN     NN
120300981012-16-  -                   -    NN     NN
120400981012-17-  -                   -    NN     NN
120500981012-18-  -                   -    NN     NN
120600981012-19-  -                   -    NN     NN
120700981012-20-  -                   -    NN     NN
120800981012-21-  -                   -    NN     NN
120900981012-22-  -                   -    NN     NN
121000981012-23-  -                   -    NN     NN
121100981012-24-  -                   -    NN     NN
121200981012** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
121300981012F1    -                   -    NN     NN
121400981012F2    -                   -    NN     NN
121500981012F3    -F3=Fine            -    SS     SS
121600981012F4    -                   -    NN     NN
121700981012F5    -F5=Rivisualizza    -    SS     SS
121800981012F6    -F6=Inserimento     -    SN     NN
121900981012F7    -                   -    NN     NN
122000981012F8    -                   -    NN     NN
122100981012F9    -                   -    NN     NN
122200981012F10   -                   -    NN     NN
122300981012F11   -                   -    NN     NN
122400981012F12   -F12=Ritorno        -    SS     SS
122500981012F13   -                   -    NN     NN
122600981012F14   -                   -    NN     NN
122700981012F15   -                   -    NN     NN
122800981012F16   -                   -    NN     NN
122900981012F17   -F17=Inizio dati    -    NN     NN
123000981012F18   -F18=Fine dati      -    NN     NN
123100981012F19   -                   -    NN     NN
123200981012F20   -                   -    NN     NN
123300981012F21   -                   -    NN     NN
123400981012F22   -                   -    NN     NN
123500981012F23   -F23=Altre opzioni  -    NN     NN
123600981012F24   -F24=Altri tasti    -    NN     NN
123700981012ENTER -                   -    SS     SS
123800981012ROLLUP-                   -    SS     SS
123900981012ROLLDN-                   -    SS     SS
124000981012HELP  -                   -    NN     NN
124100981012PRINT -                   -    NN     NN
124200981012BSPACE-                   -    NN     NN
124300981012CLEAR -                   -    NN     NN

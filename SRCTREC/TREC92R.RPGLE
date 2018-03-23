000100981013     H*------------------------------------------------------------------------*
000200981013     H* GESTIONE VOCI DI CONTABILITA' - ROUTINE
000300981013     H*------------------------------------------------------------------------*
000400981013     H DECEDIT('0,') DATEDIT(*DMY.)
000500981013     F*------------------------------------------------------------------------*
000600981013     F* DATA BASE
000700981013     F*------------------------------------------------------------------------*
000800981013     FTREC92D   CF   E             WORKSTN INFDS(DEVDS1)
000900981013     F                                     SFILE(EC92DVM:MSGULT)
001000981013     FECECV01L  UF A E           K DISK
001100981013     D*------------------------------------------------------------------------*
001200981013     D* SCHIERE
001300981013     D*------------------------------------------------------------------------*
001400981013     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001500981013     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
001600981013     D* DECODIFICA OPZIONE RICHIAMATA
001700981013     D DOPZ            S             16    DIM(5) CTDATA PERRCD(1)
001800981013     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001900981013     D FFU             S              6    DIM(31)
002000981013     D FDE             S             19    DIM(31)
002100981013     D FAT             S              1    DIM(31)
002200981013     D* TABELLA DESCRIZIONI A VIDEO
002300981013     D FUV             S             19    DIM(8)
002400981013     D* TABELLA TASTI FUNZIONALI PREMUTI
002500981013     D KEYS            S              1    DIM(40)
002600981013     D*------------------------------------------------------------------------*
002700981013     D* INPUT
002800981013     D*------------------------------------------------------------------------*
002900981013     D*-------------------
003000981013     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003100981013     D*-------------------
003200981013     D DSFUN         E DS
003300981013     D  FUNRIG                 1     40
003400981013     D*-------------------
003500981013     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
003600981013     D*-------------------
003700981013     D DSMSG         E DS
003800981013     D*-------------------
003900020923     D* DS REPERIMENTO DATI UTENTE
004000020923     D*-------------------
004100020923     D TIBS34DS      E DS
004200020923     D DDATIUTE      E DS
004300020923     D AZUTEDS       E DS                  extname(AZUTE00F)
004400981013     D*-------------------
004500981013     D* DS PER GESTIONE TASTI FUNZIONALI
004600981013     D*-------------------
004700981013     D DSKEYS        E DS
004800981013     D  KEYRIG                 1     40
004900981013     D*-------------------
005000981013     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100981013     D*-------------------
005200981013     D                SDS
005300981013     D  USER                 254    263
005400981013     D*-------------------
005500981013     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600981013     D*-------------------
005700981013     D DEVDS1          DS
005800981013     D* CHIAVE FUNZIONALE BATTUTA
005900981013     D  KEY                  369    369
006000981013     D* POSIZIONE CURSORE (IN HEX)
006100981013     D  CURSOR               370    371
006200981013     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
006300981013     D  FRTSFL               378    379B 0
006400981013     D*---------------------
006500981013     D* DS ESTERNA DI PROCEDURA
006600981013     D*---------------------
006700981013     D DS60          E DS                  EXTNAME(TREC60DS)
006800981013     D*-------------------
006900981013     D* ARCHITETTURA
007000981013     D*-------------------
007100981013     D KPJBA         E DS
007200981013     D*------------------
007300981013     D* CALL XSRDA8 - CONTROLLO DATE
007400981013     D*------------------
007500981013     D WLBDA8          DS                  INZ
007600981013     D  G08DAT                 1      8  0
007700981013     D  G08INV                 9     16  0
007800981013     D  G08ERR                17     17
007900981013     D  G08TGI                18     22  0
008000981013     C*------------------------------------------------------------------------*
008100981013     C* MAIN LINE
008200981013     C*------------------------------------------------------------------------*
008300981013     C*
008400981013     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
008500981013IF  1C     $FINE         IFEQ      '1'
008600981013     C*
008700981013     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
008800981013     C                   MOVEL     '1'           D60ERR
008900981013     C*
009000981013     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
009100981013X   1C                   ELSE
009200981013     C*
009300981013     C* CICLO FINO A CHE:
009400981013DO  2C     $FINE         DOWEQ     '0'
009500981013     C                   MOVEL     '0'           $ERR
009600981013     C*
009700981013     C* CONTROLLA GESTIONE VIDEO
009800981013IF  3C     $GES          IFEQ      'V1'
009900981013     C                   EXSR      GESV1
010000981013E   3C                   ENDIF
010100981013     C*
010200981013E   2C                   ENDDO
010300981013E   1C                   ENDIF
010400981013     C*
010500981013IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
010600981013     C     $F03          IFEQ      '1'
010700981013     C                   MOVEL     '1'           D60F03
010800981013E   1C                   ENDIF
010900981013     C* FINE PGM
011000981013     C                   MOVEL     DS60          KPJBU
011100981013     C                   SETON                                        LR
011200981013     C*------------------------------------------------------------------------*
011300981013     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
011400981013     C*------------------------------------------------------------------------*
011500981013     C     CARV1         BEGSR
011600981013     C*
011700981013     C                   MOVEL     'V1'          $GES
011800981013     C*
011900981013     C                   EXSR      CARFUN
012000981013     C                   EXSR      OUTFUN
012100981013     C*
012200981013     C                   WRITE     EC92DVT
012300981013     C                   WRITE     EC92DVF
012400981013     C*
012500981013     C                   ENDSR
012600981013     C*------------------------------------------------------------------------*
012700981013     C* GESV1  - GESTIONE PRIMO VIDEO
012800981013     C*------------------------------------------------------------------------*
012900981013     C     GESV1         BEGSR
013000981013     C*
013100981013IF  1C     $TESVD        IFEQ      '1'
013200981013     C                   EXSR      CARV1
013300981013E   1C                   ENDIF
013400981013     C*
013500981013IF  1C     MSGULT        IFNE      *ZEROS
013600981013     C                   EXSR      OUTMSG
013700981013E   1C                   ENDIF
013800981013     C*
013900981013     C                   EXFMT     EC92DV1
014000981013     C*
014100981013     C                   EXSR      CONFUN
014200981013     C*
014300981013IF  1C     $ERR          IFEQ      '0'
014400981013SL  2C                   SELECT
014500981013     C     KEY           WHENEQ    ENTER
014600981013     C                   EXSR      ENTV1
014700981013     C     KEY           WHENEQ    F6
014800981013     C                   EXSR      F06GES
014900981013     C                   OTHER
015000981013     C                   EXSR      FUNSTD
015100981013E   2C                   ENDSL
015200981013E   1C                   ENDIF
015300981013     C*
015400981013     C                   ENDSR
015500981013     C*------------------------------------------------------------------------*
015600981013     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
015700981013     C*------------------------------------------------------------------------*
015800981013     C     ENTV1         BEGSR
015900981013     C*
016000981013     C* CONTROLLA I CAMPI IMMESSI
016100981013     C                   EXSR      DECODI
016200981013     C*
016300981013     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
016400981013IF  1C     D60OP1        IFEQ      'O05'
016500981013     C                   MOVEL     '1'           $FINE
016600981013E   1C                   ENDIF
016700981013     C*
016800981013     C                   ENDSR
016900981013     C*------------------------------------------------------------------------*
017000981013     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
017100981013     C*------------------------------------------------------------------------*
017200981013     C     F06GES        BEGSR
017300981013     C*
017400981013     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
017500981013IF  1C     D60OP1        IFEQ      'F06'
017600981013     C                   EXSR      INSERI
017700981013E   1C                   ENDIF
017800981013IF  1C     D60OP1        IFEQ      'O02'
017900981013     C                   EXSR      MODIFI
018000981013E   1C                   ENDIF
018100981013     C*
018200981013     C                   ENDSR
018300981013     C*------------------------------------------------------------------------*
018400981013     C* INSERI - FUNZIONE DI INSERIMENTO
018500981013     C*------------------------------------------------------------------------*
018600981013     C     INSERI        BEGSR
018700981013     C*
018800981013     C* CONTROLLA I CAMPI IMMESSI
018900981013     C                   EXSR      DECODI
019000981013IF  1C     $ERR          IFEQ      '0'
019100981013     C                   MOVEL     V1CVOC        ECVVOC
019200981013     C                   Z-ADD     V1CDDE        G08DAT
019300981013     C                   Z-ADD     *ZEROS        G08INV
019400981013     C                   MOVEL     '0'           G08ERR
019500981013     C                   CALL      'XSRDA8'
019600981013     C                   PARM                    WLBDA8
019700981013     C                   Z-ADD     G08INV        ECVDDE
019800981013     C                   Z-ADD     V1CDSC        G08DAT
019900981013     C                   Z-ADD     *ZEROS        G08INV
020000981013     C                   MOVEL     '0'           G08ERR
020100981013     C                   CALL      'XSRDA8'
020200981013     C                   PARM                    WLBDA8
020300981013     C                   Z-ADD     G08INV        ECVDSC
020400981013     C                   MOVEL     V1CNCC        ECVNCC
020500981013     C                   MOVEL     V1CNCF        ECVNCF
020600981013     C                   MOVEL     V1CRFC        ECVRFC
020700981013     C                   MOVEL     V1CRFF        ECVRFF
020800981013     C                   MOVEL     *BLANKS       ECVFTR
020900981013     C                   Z-ADD     *ZEROS        ECVDTR
021000981013     C*
021100981013     C                   WRITE     ECECV000
021200981013     C*
021300981013     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
021400981013     C                   MOVEL     '1'           $FINE
021500981013     C*
021600981013     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
021700981013     C                   MOVEL     MSG(17)       D60MSG
021800981013E   1C                   ENDIF
021900981013     C*
022000981013     C                   ENDSR
022100981013     C*------------------------------------------------------------------------*
022200981013     C* MODIFI - FUNZIONE DI MODIFICA
022300981013     C*------------------------------------------------------------------------*
022400981013     C     MODIFI        BEGSR
022500981013     C*
022600981013     C* CONTROLLA I CAMPI IMMESSI
022700981013     C                   EXSR      DECODI
022800981013     C*
022900981013     C* RILEGGO LA VOCE PER SPOSIZIONAMENTO NEI CONTROLLI
023000981013     C                   MOVEL     D60VOB        KCVVOC
023100981013     C                   Z-ADD     D60DDB        KCVDDE
023200981013     C     KEYECV        CHAIN     ECECV01L                           99
023300981013IF  1C     *IN99         IFEQ      *OFF
023400981013IF  2C     $ERR          IFEQ      '0'
023500981013     C                   Z-ADD     V1CDSC        G08DAT
023600981013     C                   Z-ADD     *ZEROS        G08INV
023700981013     C                   MOVEL     '0'           G08ERR
023800981013     C                   CALL      'XSRDA8'
023900981013     C                   PARM                    WLBDA8
024000981013     C                   Z-ADD     G08INV        ECVDSC
024100981013     C                   MOVEL     V1CNCC        ECVNCC
024200981013     C                   MOVEL     V1CNCF        ECVNCF
024300981013     C                   MOVEL     V1CRFC        ECVRFC
024400981013     C                   MOVEL     V1CRFF        ECVRFF
024500981013     C                   MOVEL     *BLANKS       ECVFTR
024600981013     C                   Z-ADD     *ZEROS        ECVDTR
024700981013     C*
024800981013     C                   UPDATE    ECECV000
024900981013     C*
025000981013     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
025100981013     C                   MOVEL     '1'           $FINE
025200981013     C*
025300981013     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
025400981013     C                   MOVEL     MSG(18)       D60MSG
025500981013E   2C                   ENDIF
025600981013E   1C                   ENDIF
025700981013     C*
025800981013     C                   ENDSR
025900981013     C*------------------------------------------------------------------------*
026000981013     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
026100981013     C*------------------------------------------------------------------------*
026200981013     C     DECODI        BEGSR
026300981013     C*
026400981013     C* REIMPOSTO INDICATORI DI ERRORE
026500981013     C                   SETOFF                                       606162
026600981013     C                   SETOFF                                       636465
026700981013     C                   SETOFF                                       66
026800981013     C*--------------------
026900981013     C* DATA DECORRENZA
027000981013     C*--------------------
027100981013     C     *LIKE         DEFINE    V1CDDE        DDEV1C
027200981013IF  1C     $ERR          IFEQ      '0'
027300981013     C                   Z-ADD     V1CDDE        $DAT
027400981013     C                   Z-ADD     *ZEROS        DAT$
027500981013     C                   EXSR      CHKDAT
027600981013     C                   Z-ADD     $DAT          V1CDDE
027700981013     C                   Z-ADD     DAT$          DDEV1C
027800981013IF  2C     $ERR          IFEQ      '1'
027900981013     C                   SETON                                        61
028000981013     C                   MOVEL     MSG(10)       $MSG
028100981013     C                   EXSR      CARMSG
028200981013E   2C                   ENDIF
028300981013E   1C                   ENDIF
028400981013     C*--------------------
028500981013     C* CODICE VOCE
028600981013     C*--------------------
028700981013IF  1C     $ERR          IFEQ      '0'
028800981013     C                   MOVEL     *BLANKS       V1DVOC
028900981013     C                   MOVEL     '201'         YSOC              3
029000981013     C                   Z-ADD     *ZEROS        YVON              4 0
029100981013     C                   MOVEL     V1CVOC        YVOA              8
029200981013     C                   Z-ADD     DDEV1C        YDAT              8 0
029300981013     C                   MOVEL     *BLANKS       YDES             20
029400981013     C                   MOVEL     *BLANKS       YERR              1
029500981013     C                   MOVEL     *BLANKS       YMSG             80
029600981013     C                   CALL      'YCO900R'
029700981013     C                   PARM                    YSOC
029800981013     C                   PARM                    YVON
029900981013     C                   PARM                    YVOA
030000981013     C                   PARM                    YDAT
030100981013     C                   PARM                    YDES
030200981013     C                   PARM                    YERR
030300981013     C                   PARM                    YMSG
030400981013IF  2C     YERR          IFNE      'S'
030500981013     C                   MOVEL     YDES          V1DVOC
030600981013X   2C                   ELSE
030700981013     C                   SETON                                        60
030800981013     C                   MOVEL     '1'           $ERR
030900981013     C                   MOVEL     YMSG          $MSG
031000981013     C                   EXSR      CARMSG
031100981013E   2C                   ENDIF
031200981013E   1C                   ENDIF
031300981013     C*--------------------
031400981013     C* INSERIMENTO -> VOCE INESISTENTE
031500981013     C*--------------------
031600981013IF  1C     $ERR          IFEQ      '0'
031700981013IF  2C     D60OP1        IFEQ      'F06'
031800981013     C                   MOVEL     V1CVOC        KCVVOC
031900981013     C                   Z-ADD     DDEV1C        KCVDDE
032000981013     C     KEYECV        SETLL     ECECV01L                               99
032100981013IF  3C     *IN99         IFEQ      *ON
032200981013     C                   MOVEL     '1'           $ERR
032300981013     C                   MOVEL     MSG(14)       $MSG
032400981013     C                   EXSR      CARMSG
032500981013     C                   SETON                                        6080
032600981013E   3C                   ENDIF
032700981013E   2C                   ENDIF
032800981013E   1C                   ENDIF
032900981013     C*--------------------
033000981013     C* DATA SCADENZA
033100981013     C*--------------------
033200981013     C     *LIKE         DEFINE    V1CDSC        DSCV1C
033300981013IF  1C     $ERR          IFEQ      '0'
033400981013     C                   Z-ADD     V1CDSC        $DAT
033500981013     C                   Z-ADD     *ZEROS        DAT$
033600981013     C                   EXSR      CHKDAT
033700981013     C                   Z-ADD     $DAT          V1CDSC
033800981013     C                   Z-ADD     DAT$          DSCV1C
033900981013IF  2C     $ERR          IFEQ      '1'
034000981013     C                   SETON                                        62
034100981013     C                   MOVEL     MSG(10)       $MSG
034200981013     C                   EXSR      CARMSG
034300981013E   2C                   ENDIF
034400981013E   1C                   ENDIF
034500981013     C*
034600981013IF  1C     $ERR          IFEQ      '0'
034700981013IF  2C     DSCV1C        IFLT      DDEV1C
034800981013     C                   MOVEL     '1'           $ERR
034900981013     C                   MOVEL     MSG(11)       $MSG
035000981013     C                   EXSR      CARMSG
035100981013     C                   SETON                                        6162
035200981013E   2C                   ENDIF
035300981013E   1C                   ENDIF
035400981013     C*--------------------
035500981013     C* VALIDITA' VOCE
035600981013     C*--------------------
035700981013     C* NESSUNA VOCE CON PERIODI SOVRAPPOSTI
035800981013     C* (SE NON E' IN INSERIMENTO, DAL CONTROLLO ESCLUDE LA VOCE IN LINEA)
035900981013IF  1C     $ERR          IFEQ      '0'
036000981013     C                   MOVEL     V1CVOC        KCVVOC
036100981013     C     KCVVOC        SETLL     ECECV01L
036200981013     C     KCVVOC        READE(N)  ECECV01L                               99
036300981013DO  2C     *IN99         DOWEQ     *OFF
036400981013IF  3C     D60OP1        IFNE      'F06'
036500981013     C     DDEV1C        ANDEQ     ECVDDE
036600981013X   3C                   ELSE
036700981013IF  4C     DSCV1C        IFGE      ECVDDE
036800981013     C     DSCV1C        ANDLE     ECVDSC
036900981013     C     DDEV1C        ORGE      ECVDDE
037000981013     C     DDEV1C        ANDLE     ECVDSC
037100981013     C                   MOVEL     '1'           $ERR
037200981013     C                   MOVEL     MSG(15)       $MSG
037300981013     C                   EXSR      CARMSG
037400981013     C                   SETON                                        606162
037500981013     C                   LEAVE
037600981013E   4C                   ENDIF
037700981013E   3C                   ENDIF
037800981013     C     KCVVOC        READE(N)  ECECV01L                               99
037900981013E   2C                   ENDDO
038000981013E   1C                   ENDIF
038100981013     C*--------------------
038200981013     C* NOTE CREDITO CLIENTI
038300981013     C*--------------------
038400981013IF  1C     $ERR          IFEQ      '0'
038500981013IF  2C     V1CNCC        IFEQ      'S'
038600981013     C     V1CNCC        OREQ      'N'
038700981013X   2C                   ELSE
038800981013     C                   MOVEL     '1'           $ERR
038900981013     C                   MOVEL     MSG(13)       $MSG
039000981013     C                   EXSR      CARMSG
039100981013     C                   SETON                                        63
039200981013E   2C                   ENDIF
039300981013E   1C                   ENDIF
039400981015     C*--------------------
039500981015     C* RETTIFICHE CLIENTI
039600981015     C*--------------------
039700981015IF  1C     $ERR          IFEQ      '0'
039800981015IF  2C     V1CRFC        IFEQ      'S'
039900981015     C     V1CRFC        OREQ      'N'
040000981015X   2C                   ELSE
040100981015     C                   MOVEL     '1'           $ERR
040200981015     C                   MOVEL     MSG(13)       $MSG
040300981015     C                   EXSR      CARMSG
040400981015     C                   SETON                                        65
040500981015E   2C                   ENDIF
040600981015E   1C                   ENDIF
040700981015     C*--------------------
040800981015     C* NOTE CREDITO + RETTIFICHE CLIENTI
040900981015     C*--------------------
041000981015IF  1C     $ERR          IFEQ      '0'
041100981015IF  2C     V1CNCC        IFEQ      'S'
041200981015     C     V1CRFC        ANDEQ     'S'
041300981015     C                   MOVEL     '1'           $ERR
041400981015     C                   MOVEL     MSG(16)       $MSG
041500981015     C                   EXSR      CARMSG
041600981015     C                   SETON                                        6365
041700981015E   2C                   ENDIF
041800981015E   1C                   ENDIF
041900981013     C*--------------------
042000981013     C* NOTE CREDITO FILIALI
042100981013     C*--------------------
042200981013IF  1C     $ERR          IFEQ      '0'
042300981013IF  2C     V1CNCF        IFEQ      'S'
042400981013     C     V1CNCF        OREQ      'N'
042500981013X   2C                   ELSE
042600981013     C                   MOVEL     '1'           $ERR
042700981013     C                   MOVEL     MSG(13)       $MSG
042800981013     C                   EXSR      CARMSG
042900981013     C                   SETON                                        64
043000981013E   2C                   ENDIF
043100981013E   1C                   ENDIF
043200981013     C*--------------------
043300981013     C* RETTIFICHE FILIALI
043400981013     C*--------------------
043500981013IF  1C     $ERR          IFEQ      '0'
043600981013IF  2C     V1CRFF        IFEQ      'S'
043700981013     C     V1CRFF        OREQ      'N'
043800981013X   2C                   ELSE
043900981013     C                   MOVEL     '1'           $ERR
044000981013     C                   MOVEL     MSG(13)       $MSG
044100981013     C                   EXSR      CARMSG
044200981013     C                   SETON                                        66
044300981013E   2C                   ENDIF
044400981013E   1C                   ENDIF
044500981015     C*--------------------
044600981015     C* NOTE CREDITO + RETTIFICHE FILIALI
044700981015     C*--------------------
044800981015IF  1C     $ERR          IFEQ      '0'
044900981015IF  2C     V1CNCF        IFEQ      'S'
045000981015     C     V1CRFF        ANDEQ     'S'
045100981015     C                   MOVEL     '1'           $ERR
045200981015     C                   MOVEL     MSG(16)       $MSG
045300981015     C                   EXSR      CARMSG
045400981015     C                   SETON                                        6466
045500981015E   2C                   ENDIF
045600981015E   1C                   ENDIF
045700981013     C*
045800981013     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
045900981013     C* CURSORE SUL CAMPO DEL CONTROL
046000981013IF  1C     $ERR          IFEQ      '1'
046100981013     C                   MOVEL     '0'           *IN43
046200981013X   1C                   ELSE
046300981013     C                   MOVEL     '1'           *IN43
046400981013E   1C                   ENDIF
046500981013     C*
046600981013     C                   ENDSR
046700981013     C*------------------------------------------------------------------------*
046800981013     C* CHKDAT - CONTROLLA LA DATA
046900981013     C*          INPUT  -> $DAT:  DATA (GG/MM/AAAA)
047000981013     C*          OUTPUT -> DAT$:  DATA (AAAA/GG/MM)
047100981013     C*------------------------------------------------------------------------*
047200981013     C     CHKDAT        BEGSR
047300981013     C*
047400981013     C     *LIKE         DEFINE    G08DAT        $DAT
047500981013     C     *LIKE         DEFINE    G08INV        DAT$
047600981013     C*
047700981013     C                   Z-ADD     $DAT          G08DAT
047800981013     C                   Z-ADD     *ZEROS        G08INV
047900981013     C                   MOVEL     '0'           G08ERR
048000981013     C                   CALL      'XSRDA8'
048100981013     C                   PARM                    WLBDA8
048200981013IF  1C     G08ERR        IFEQ      '1'
048300981013     C                   MOVEL     '1'           $ERR
048400981013     C                   ELSE
048500981013     C                   Z-ADD     G08DAT        $DAT
048600981013     C                   Z-ADD     G08INV        DAT$
048700981013E   1C                   ENDIF
048800981013     C*
048900981013     C                   ENDSR
049000981013     C*------------------------------------------------------------------------*
049100981013     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
049200981013     C*------------------------------------------------------------------------*
049300981013     C     RECVID        BEGSR
049400981013     C* CAMPI
049500981013     C                   MOVEL     D60VOB        V1CVOC
049600981013     C                   Z-ADD     D60DDB        G08INV
049700981013     C                   Z-ADD     *ZEROS        G08DAT
049800981013     C                   MOVEL     '3'           G08ERR
049900981013     C                   CALL      'XSRDA8'
050000981013     C                   PARM                    WLBDA8
050100981013     C                   Z-ADD     G08DAT        V1CDDE
050200981013     C                   Z-ADD     ECVDSC        G08INV
050300981013     C                   Z-ADD     *ZEROS        G08DAT
050400981013     C                   MOVEL     '3'           G08ERR
050500981013     C                   CALL      'XSRDA8'
050600981013     C                   PARM                    WLBDA8
050700981013     C                   Z-ADD     G08DAT        V1CDSC
050800981013     C                   MOVEL     ECVNCC        V1CNCC
050900981013     C                   MOVEL     ECVNCF        V1CNCF
051000981013     C                   MOVEL     ECVRFC        V1CRFC
051100981013     C                   MOVEL     ECVRFF        V1CRFF
051200981013     C* DECODIFICHE
051300981013     C                   MOVEL     *BLANKS       V1DVOC
051400981013     C                   MOVEL     '201'         YSOC              3
051500981013     C                   Z-ADD     *ZEROS        YVON              4 0
051600981013     C                   MOVEL     D60VOB        YVOA              8
051700981013     C                   Z-ADD     D60DDB        YDAT              8 0
051800981013     C                   MOVEL     *BLANKS       YDES             20
051900981013     C                   MOVEL     *BLANKS       YERR              1
052000981013     C                   MOVEL     *BLANKS       YMSG             80
052100981013     C                   CALL      'YCO900R'
052200981013     C                   PARM                    YSOC
052300981013     C                   PARM                    YVON
052400981013     C                   PARM                    YVOA
052500981013     C                   PARM                    YDAT
052600981013     C                   PARM                    YDES
052700981013     C                   PARM                    YERR
052800981013     C                   PARM                    YMSG
052900981013IF  1C     YERR          IFNE      'S'
053000981013     C                   MOVEL     YDES          V1DVOC
053100981013E   1C                   ENDIF
053200981013     C*
053300981013     C                   ENDSR
053400981013     C*------------------------------------------------------------------------*
053500981013     C*  AZZVID - PULISCE TUTTI ICAMPI DEL VIDEO
053600981013     C*------------------------------------------------------------------------*
053700981013     C     AZZVID        BEGSR
053800981013     C* CAMPI
053900981013     C                   MOVEL     *BLANKS       V1CVOC
054000981013     C                   Z-ADD     *ZEROS        V1CDDE
054100981013     C                   Z-ADD     *ZEROS        V1CDSC
054200981013     C                   MOVEL     *BLANKS       V1CNCC
054300981013     C                   MOVEL     *BLANKS       V1CNCF
054400981013     C                   MOVEL     *BLANKS       V1CRFC
054500981013     C                   MOVEL     *BLANKS       V1CRFF
054600981013     C* DECODIFICHE
054700981013     C                   MOVEL     *BLANKS       V1DVOC
054800981013     C*
054900981013     C                   ENDSR
055000020923     C*-------------------------------------------------------------------------
055100020923     C* REPERISCE I DATI UTENTE
055200020923     C*-------------------------------------------------------------------------
055300020923     C     REPDATIUTE    BEGSR
055400020923     C*
055500020923     C* INIZIALIZZA VARIABILI DI WRK
055600020923     C                   CLEAR                   TIBS34DS
055700020923     C                   CLEAR                   AZUTEDS
055800020923     C                   CLEAR                   DDATIUTE
055900020923     C*
056000020923     C     *DTAARA       DEFINE    §azute        azuteds
056100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
056200020923     C                   IN(E)     *DTAARA
056300020923if  1C                   IF        %Error
056400020923     c                   EVAL      I34Tla = 'L'
056500020923     C                   CALL      'TIBS34R'
056600020923     C                   PARM                    Tibs34Ds
056700020923     C                   IN        *DTAARA
056800020923e   1C                   ENDIF
056900020923     C*
057000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
057100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
057200020923     C*
057300020923     C                   ENDSR
057400981013     C*------------------------------------------------------------------------*
057500981013     C*  *INZSR - ROUTINE INIZIALE
057600981013     C*------------------------------------------------------------------------*
057700981013     C     *INZSR        BEGSR
057800981013     C*
057900981013     C* RICEVIMENTO PARAMETRI
058000981013     C     *ENTRY        PLIST
058100981013     C                   PARM                    KPJBA
058200981013     C                   MOVEL     KPJBU         DS60
058300981013     C*
058400981013     C* VARIABILI RIFERITE AL DATA BASE
058500981013     C     *LIKE         DEFINE    ECVVOC        KCVVOC
058600981013     C     *LIKE         DEFINE    ECVDDE        KCVDDE
058700981013     C*
058800981013     C* CHIAVE LETTURA ECECV01L - COMPLETA
058900981013     C     KEYECV        KLIST
059000981013     C                   KFLD                    KCVVOC
059100981013     C                   KFLD                    KCVDDE
059200981013     C*
059300981013     C* B02 --> GESTIONE;  B05 --> INTERROGAZIONE
059400981013     C                   MOVE      D60OP0        INTGES            2
059500981013IF  1C     INTGES        IFEQ      '02'
059600981013     C                   MOVEL     'G'           $TGES             1
059700981013X   1C                   ELSE
059800981013     C                   MOVEL     'I'           $TGES             1
059900981013E   1C                   ENDIF
060000981013     C*
060100981013     C* ESEGUE OPERAZIONI INIZIALI STANDARD
060200981013     C                   EXSR      INZSTD
060300981013     C*
060400981013     C* CALCOLA LA DATA CORRENTE
060500981013     C                   TIME                    N14              14 0
060600981013     C                   MOVE      N14           N8                8 0
060700981013     C                   Z-ADD     N8            G08DAT
060800981013     C                   Z-ADD     *ZEROS        G08INV
060900981013     C                   MOVEL     '0'           G08ERR
061000981013     C                   CALL      'XSRDA8'
061100981013     C                   PARM                    WLBDA8
061200981013     C                   Z-ADD     G08INV        DATCOR            8 0
061300981013     C*
061400981013     C* DECODIFICA OPZIONE RICHIAMATA
061500981013IF  1C     D60OP1        IFEQ      'F06'
061600981013     C                   MOVEL     DOPZ(1)       VTDFUN
061700981013E   1C                   ENDIF
061800981013IF  2C     D60OP1        IFEQ      'O02'
061900981013     C                   MOVEL     DOPZ(2)       VTDFUN
062000981013E   1C                   ENDIF
062100981013IF  2C     D60OP1        IFEQ      'O05'
062200981013     C                   MOVEL     DOPZ(5)       VTDFUN
062300981013E   1C                   ENDIF
062400981013     C*
062500981013     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
062600981013     C* 0=NON PROTETTO, 1=PROTETTO
062700981013     C*   90 - CAMPI CHIAVE
062800981013     C*   91 - ALTRI CAMPI
062900981013IF  1C     D60OP1        IFEQ      'F06'
063000981013     C                   SETOFF                                       9091
063100981013E   1C                   ENDIF
063200981013IF  1C     D60OP1        IFEQ      'O02'
063300981013     C                   SETON                                        90
063400981013E   1C                   ENDIF
063500981013     C     D60OP1        IFEQ      'O05'
063600981013     C                   SETON                                        9091
063700981013E   1C                   ENDIF
063800981013     C*
063900981013     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
064000981013IF  1C     D60OP1        IFEQ      'O05'
064100981013     C                   MOVEL     FUN(6)        FUNRIG
064200981013     C                   MOVEL     'N'           FUNASG
064300981013     C                   MOVEL     'N'           FUNASI
064400981013     C                   MOVEL     'N'           FUNAFG
064500981013     C                   MOVEL     'N'           FUNAFG
064600981013     C                   MOVEL     FUNRIG        FUN(6)
064700981013E   1C                   ENDIF
064800981013     C*
064900981013     C* SE SONO IN MODIFICA/VISUALIZZAZIONE LEGGE IL RECORD
065000981013IF  1C     D60OP1        IFEQ      'O02'
065100981013     C     D60OP1        OREQ      'O05'
065200981013     C                   MOVEL     D60VOB        KCVVOC
065300981013     C                   Z-ADD     D60DDB        KCVDDE
065400981013     C     KEYECV        CHAIN     ECECV01L                           9998
065500981013IF  2C     *IN99         IFEQ      '1'
065600981013     C                   MOVEL     '1'           $ERR
065700981013     C                   MOVEL     '1'           $FINE
065800981013     C                   MOVEL     MSG(19)       D60MSG
065900981013X   2C                   ELSE
066000981013IF  3C     *IN98         IFEQ      '1'
066100981013     C                   MOVEL     '1'           $ERR
066200981013     C                   MOVEL     '1'           $FINE
066300981013     C                   MOVEL     MSG(20)       D60MSG
066400981013X   3C                   ELSE
066500981013     C                   EXSR      RECVID
066600981013     C                   EXSR      CARV1
066700981013E   3C                   ENDIF
066800981013E   2C                   ENDIF
066900981013E   1C                   ENDIF
067000981013     C*
067100981013     C* SE INSERIMENTO
067200981013IF  1C     D60OP1        IFEQ      'F06'
067300981013     C                   EXSR      AZZVID
067400981013     C                   EXSR      CARV1
067500981013     C                   ELSE
067600981013     C                   EXSR      DECODI
067700981013E   1C                   ENDIF
067800981013     C*
067900981013     C                   ENDSR
068000981013     C*------------------------------------------------------------------------*
068100981013     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
068200981013     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
068300981013     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
068400981013     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
068500981013     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
068600981013     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
068700981013     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
068800981013     C*------------------------------------------------------------------------*
068900981013     C* INZSTD - ROUTINE INIZIALE STANDARD
069000981013     C*------------------------------------------------------------------------*
069100981013     C     INZSTD        BEGSR
069200981013     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
069300981013     C                   CALL      'XKEYS'
069400981013     C                   PARM                    DSKEYS
069500981013     C                   MOVEA     KEYRIG        KEYS
069600981013     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
069700981013     C     XMSGPA        PLIST
069800981013     C                   PARM                    DSMSMS
069900981013     C                   PARM                    DSMSPG
070000981013     C                   PARM                    DSMSKE
070100981013     C                   PARM                    DSMSER
070200981013     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
070300981013     C                   MOVEL     'TREC92R'     DSMSPG
070400981013     C                   MOVEL     'TREC92R'     VMCQUE
070500981013     C                   MOVEL     *BLANKS       VMCKEY
070600981013     C                   MOVEL     *BLANKS       DSMSMS
070700981013     C* INIZIALIZZO FLAG CONTROLLO PGM
070800981013     C                   MOVEL     '0'           $ERR              1
070900981013     C                   MOVEL     '0'           $F03              1
071000981013     C                   MOVEL     *BLANKS       $GES              2
071100981013     C                   MOVEL     *BLANKS       $MSG             80
071200981013     C                   MOVEL     '0'           $FINE             1
071300981013     C                   MOVEL     '0'           $TESVD            1
071400981013     C* LANCIO ROUTINE PER VERIFICARE SE
071500981013     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
071600981013     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
071700020923     C                   EXSR      REPDATIUTE
071800981013     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
071900981013     C                   BITOFF    '01234567'    POSMI1            1
072000981013     C                   BITOFF    '01234567'    POSMI2            1
072100981013     C                   BITOFF    '01234567'    POSMA1            1
072200981013     C                   BITOFF    '01234567'    POSMA2            1
072300981013     C                   BITON     '34'          POSMI1            1
072400981013     C                   BITON     '7'           POSMI2            1
072500981013     C                   BITON     '34'          POSMA1            1
072600981013     C                   BITON     '0'           POSMA2            1
072700981013     C                   MOVE      POSMI2        POSMIN            2
072800981013     C                   MOVEL     POSMI1        POSMIN
072900981013     C                   MOVE      POSMA2        POSMAX            2
073000981013     C                   MOVEL     POSMA1        POSMAX
073100981013     C                   ENDSR
073200981013     C*------------------------------------------------------------------------*
073300981013     C* FUNSTD - LANCIA SUBROUTINE STANDARD
073400981013     C*------------------------------------------------------------------------*
073500981013     C     FUNSTD        BEGSR
073600981013     C                   SELECT
073700981013     C     KEY           WHENEQ    F3
073800981013     C                   EXSR      F03STD
073900981013     C     KEY           WHENEQ    F12
074000981013     C                   EXSR      F12STD
074100981013     C     KEY           WHENEQ    F24
074200981013     C                   EXSR      F24STD
074300981013     C                   OTHER
074400981013     C                   MOVEL     MSG(2)        $MSG
074500981013     C                   EXSR      CARMSG
074600981013     C                   ENDSL
074700981013     C                   ENDSR
074800981013     C*------------------------------------------------------------------------*
074900981013     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
075000981013     C*------------------------------------------------------------------------*
075100981013     C     F03STD        BEGSR
075200981013     C* IMPOSTO FLAG DI PREMUTO TASTO F03
075300981013     C                   MOVEL     '1'           $F03
075400981013     C* IMPOSTO FLAG DI FINE PROGRAMMA
075500981013     C                   MOVEL     '1'           $FINE
075600981013     C                   ENDSR
075700981013     C*------------------------------------------------------------------------*
075800981013     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
075900981013     C*------------------------------------------------------------------------*
076000981013     C     F12STD        BEGSR
076100981013     C* IMPOSTO FLAG DI FIN PGM
076200981013     C                   MOVEL     '1'           $FINE
076300981013     C                   ENDSR
076400981013     C*------------------------------------------------------------------------*
076500981013     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
076600981013     C*------------------------------------------------------------------------*
076700981013     C     F24STD        BEGSR
076800981013     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
076900981013     C                   EXSR      OUTFUN
077000981013     C                   ENDSR
077100981013     C*------------------------------------------------------------------------*
077200981013     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
077300981013     C*          CODA MESSAGGI DEL PGM
077400981013     C*------------------------------------------------------------------------*
077500981013     C     CARMSG        BEGSR
077600981013     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
077700981013     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
077800981013     C     MSGULT        IFEQ      0
077900981013     C                   MOVEL     '1'           *IN40
078000981013     C                   WRITE     EC92DVK
078100981013     C                   MOVEL     '0'           *IN40
078200981013     C                   ENDIF
078300981013     C* INVIA MESSAGGIO ALLA CODA DEL PGM
078400981013     C                   MOVEL     $MSG          DSMSMS
078500981013     C                   CALL      'XMSG'        XMSGPA
078600981013     C     DSMSER        IFEQ      0
078700981013     C                   MOVEL     DSMSKE        VMCKEY
078800981013     C                   ENDIF
078900981013     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
079000981013     C                   ADD       1             MSGULT            4 0
079100981013     C                   WRITE     EC92DVM
079200981013     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
079300981013     C                   MOVEL     *BLANKS       $MSG
079400981013     C                   ENDSR
079500981013     C*------------------------------------------------------------------------*
079600981013     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
079700981013     C*------------------------------------------------------------------------*
079800981013     C     OUTMSG        BEGSR
079900981013     C* VISUALIZZAZIONE MESSAGGIO
080000981013     C                   MOVEL     '1'           *IN44
080100981013     C                   WRITE     EC92DVK
080200981013     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
080300981013     C                   Z-ADD     *ZEROS        MSGULT
080400981013     C                   ENDSR
080500981013     C*------------------------------------------------------------------------*
080600981013     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
080700981013     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
080800981013     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
080900981013     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
081000981013     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
081100981013     C*------------------------------------------------------------------------*
081200981013     C     CARFUN        BEGSR
081300981013     C                   Z-ADD     *ZEROS        FUNULT            5 0
081400981013     C     1             DO        31            I                 5 0
081500981013     C                   MOVEL     FUN(I)        FUNRIG
081600981013     C                   MOVEL     FUNFUN        FFU(I)
081700981013     C                   MOVEL     FUNDES        FDE(I)
081800020923     C     DUTLPO        IFEQ      'S'
081900981013     C     $TGES         IFEQ      'G'
082000981013     C                   MOVEL     FUNASG        FAT(I)
082100981013     C                   ELSE
082200981013     C                   MOVEL     FUNASI        FAT(I)
082300981013     C                   ENDIF
082400981013     C                   ELSE
082500981013     C     $TGES         IFEQ      'G'
082600981013     C                   MOVEL     FUNAFG        FAT(I)
082700981013     C                   ELSE
082800981013     C                   MOVEL     FUNAFI        FAT(I)
082900981013     C                   ENDIF
083000981013     C                   ENDIF
083100981013     C     FAT(I)        IFEQ      'E'
083200981013     C                   MOVEL     USER          WEDP              3
083300981013     C     WEDP          IFEQ      'EDP'
083400981013     C                   MOVEL     'S'           FAT(I)
083500981013     C                   ELSE
083600981013     C                   MOVEL     'N'           FAT(I)
083700981013     C                   ENDIF
083800981013     C                   ENDIF
083900981013     C                   ENDDO
084000981013     C                   ENDSR
084100981013     C*------------------------------------------------------------------------*
084200981013     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
084300981013     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
084400981013     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
084500981013     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
084600981013     C*------------------------------------------------------------------------*
084700981013     C     OUTFUN        BEGSR
084800981013     C     1             DO        8             I
084900981013     C                   MOVEL     *BLANKS       FUV(I)
085000981013     C                   ENDDO
085100981013     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
085200981013     C                   Z-ADD     *ZEROS        FUNNAT            2 0
085300981013     C     1             DO        31            I
085400981013     C     FAT(I)        IFEQ      'S'
085500981013     C     FDE(I)        ANDNE     *BLANKS
085600981013     C                   ADD       1             FUNNAT
085700981013     C                   ENDIF
085800981013     C                   ENDDO
085900981013     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
086000981013     C     FUNNAT        IFGT      8
086100981013     C                   MOVEL     'S'           FAT(24)
086200981013     C                   ENDIF
086300981013     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
086400981013     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
086500981013     C                   Z-ADD     8             FUNVIS            5 0
086600981013     C     FAT(23)       IFEQ      'S'
086700981013     C                   SUB       1             FUNVIS
086800981013     C                   ENDIF
086900981013     C     FAT(24)       IFEQ      'S'
087000981013     C                   SUB       1             FUNVIS
087100981013     C                   ENDIF
087200981013     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
087300981013     C                   Z-ADD     *ZEROS        W                 5 0
087400981013     C                   ADD       1             FUNULT
087500981013     C     FUNULT        IFGT      31
087600981013     C                   Z-ADD     1             FUNULT
087700981013     C                   ENDIF
087800981013     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
087900981013     C     FUNULT        DO        31            I
088000981013     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
088100981013     C     W             IFLT      FUNVIS
088200981013     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
088300981013     C     FAT(I)        IFEQ      'S'
088400981013     C     FDE(I)        ANDNE     *BLANKS
088500981013     C     I             IFEQ      23
088600981013     C     I             OREQ      24
088700981013     C                   ELSE
088800981013     C                   ADD       1             W
088900981013     C                   MOVEL     FDE(I)        FUV(W)
089000981013     C                   ENDIF
089100981013     C                   ENDIF
089200981013     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
089300981013     C                   Z-ADD     I             FUNULT
089400981013     C                   ENDIF
089500981013     C                   ENDDO
089600981013     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
089700981013     C                   MOVEL     FUV(1)        VFD001
089800981013     C                   MOVEL     FUV(2)        VFD002
089900981013     C                   MOVEL     FUV(3)        VFD003
090000981013     C                   MOVEL     FUV(4)        VFD004
090100981013     C                   MOVEL     FUV(5)        VFD005
090200981013     C                   MOVEL     FUV(6)        VFD006
090300981013     C     FAT(23)       IFEQ      'N'
090400981013     C     FAT(24)       ANDEQ     'N'
090500981013     C                   MOVEL     FUV(7)        VFD007
090600981013     C                   MOVEL     FUV(8)        VFD008
090700981013     C                   ELSE
090800981013     C     FAT(23)       IFEQ      'S'
090900981013     C     FAT(24)       ANDEQ     'S'
091000981013     C                   MOVEL     FDE(23)       VFD007
091100981013     C                   MOVEL     FDE(24)       VFD008
091200981013     C                   ELSE
091300981013     C                   MOVEL     FUV(7)        VFD007
091400981013     C     FAT(23)       IFEQ      'S'
091500981013     C                   MOVEL     FDE(23)       VFD008
091600981013     C                   ELSE
091700981013     C                   MOVEL     FDE(24)       VFD008
091800981013     C                   ENDIF
091900981013     C                   ENDIF
092000981013     C                   ENDIF
092100981013     C                   MOVEL     '1'           $TESVD
092200981013     C                   ENDSR
092300981013     C*------------------------------------------------------------------------*
092400981013     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
092500981013     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
092600981013     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
092700981013     C*                   - ERRORE SE FUNZIONE NON ATTIVA
092800981013     C*------------------------------------------------------------------------*
092900981013     C     CONFUN        BEGSR
093000981013     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
093100981013     C     1             DO        31            I
093200981013     C     KEY           IFEQ      KEYS(I)
093300981013     C* CONTROLLO SE IL TASTO E' ATTIVO
093400981013     C     FAT(I)        IFNE      'S'
093500981013     C                   MOVEL     MSG(1)        $MSG
093600981013     C                   EXSR      CARMSG
093700981013     C                   MOVEL     '1'           $ERR
093800981013     C                   ENDIF
093900981013     C                   ENDIF
094000981013     C                   ENDDO
094100981013     C                   ENDSR
094200981013** ======== SCHIERA: MSG  (MESSAGGI)                       ================
094300981013TREC92R- Tasto funzionale non valido.                                          1
094400981013TREC92R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
094500981013TREC92R- Opzione non valida.                                                   3
094600981013TREC92R- Opzione abilitata ma non gestita. Avvertire CED.                      4
094700981013TREC92R- Inizio elenco.                                                        5
094800981013TREC92R- Fine elenco.                                                          6
094900981013TREC92R- 7
095000981013TREC92R- 8
095100981013TREC92R- 9
095200981013TREC92R- Data errata                                                          10
095300981013TREC92R- La data scadenza deve essere maggiore/uguale alla data decorrenza    11
095400981013TREC92R- Voce errata o inesistente                                            12
095500981013TREC92R- Immettere uno dei valori possibili                                   13
095600981013TREC92R- Voce già inserita                                                    14
095700981013TREC92R- La voce si sovrappone ad una già in decorrenza                       15
095800981015TREC92R- Una voce può essere o nota o rettifica                               16
095900981013TREC92R- Inserimento effettuato.                                              17
096000981013TREC92R- Modifica effettuata.                                                 18
096100981013TREC92R- Richiesta funzione per record gia' esistente                         19
096200981013TREC92R- Record momentaneamente allocato: riprovare più tardi                 20
096300981013** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
096400981013F1    -                   -    NN     NN
096500981013F2    -                   -    NN     NN
096600981013F3    -F3=Fine            -    SS     SS
096700981013F4    -                   -    NN     NN
096800981013F5    -                   -    NN     NN
096900981013F6    -F6=Aggiornamento   -    SS     SS
097000981013F7    -                   -    NN     NN
097100981013F8    -                   -    NN     NN
097200981013F9    -                   -    NN     NN
097300981013F10   -                   -    NN     NN
097400981013F11   -                   -    NN     NN
097500981013F12   -F12=Ritorno        -    SS     SS
097600981013F13   -                   -    NN     NN
097700981013F14   -                   -    NN     NN
097800981013F15   -                   -    NN     NN
097900981013F16   -                   -    NN     NN
098000981013F17   -                   -    NN     NN
098100981013F18   -                   -    NN     NN
098200981013F19   -                   -    NN     NN
098300981013F20   -                   -    NN     NN
098400981013F21   -                   -    NN     NN
098500981013F22   -                   -    NN     NN
098600981013F23   -                   -    NN     NN
098700981013F24   -F24=Altri tasti    -    NN     NN
098800981013ENTER -                   -    SS     SS
098900981013ROLLUP-                   -    NN     NN
099000981013ROLLDN-                   -    NN     NN
099100981013HELP  -                   -    NN     NN
099200981013PRINT -                   -    NN     NN
099300981013BSPACE-                   -    NN     NN
099400981013CLEAR -                   -    NN     NN
099500981013** DOPZ  (OPZIONE RICHIAMATA)
099600981013* INSERIMENTO *                                                 1
099700981013*  VARIAZIONE  *                                                2
0998009810133
0999009810134
100000981013VISUALIZZAZIONE                                                 5
